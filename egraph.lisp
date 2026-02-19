(in-package :egraph)

(declaim (inline enode-representative-p enode-canonical-p enode-eclass-info
                 make-analysis-data merge-analysis-data modify-analysis-data
                 get-analysis-data egraph-n-enodes egraph-n-eclasses dfs))

(defstruct (eclass-info (:constructor %make-eclass-info))
  "Metadata for the eclass.

Should only appear in reprensetative enode's PARENT slot.

NODES and PARENTS only store canonical enodes after `egraph-rebuild'."
  (nodes nil :type list)
  (parents nil :type list)
  (n-parents 0 :type fixnum)
  (analysis-data-vec (vector) :type simple-vector))

(declaim (type non-negative-fixnum *hash-code*))
(global-vars:define-global-var *hash-code* 0)

(defstruct (enode (:constructor %make-enode))
  "PARENT is either another enode in the same eclass, or an `eclass-info' if this
enode is the representative of its own eclass.

Set CANONICAL-FLAG to NIL to mark the node as non-canonical. `egraph-rebuild'
trusts this information to avoid testing all term arguments for
representativeness."
  (term) (parent)
  (hash-code
   (setf *hash-code*
         (logand (1+ *hash-code*) most-positive-fixnum))
   :type fixnum)
  (canonical-flag t :type boolean))

;; Be aware that representative enode might be non-canonical!
(defun enode-representative-p (enode)
  (eclass-info-p (enode-parent enode)))

(defun enode-canonical-p (enode)
  (every #'enode-representative-p (cdr (enode-term enode))))

(defun enode-eclass-info (enode)
  (enode-parent (enode-find enode)))

(defmethod print-object ((self enode) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~:[~;REP ~]~a" (enode-representative-p self) (enode-term self))))

(defun term-equal (x y)
  (unless (eql (car x) (car y))
    (return-from term-equal nil))
  (setq x (cdr x) y (cdr y))
  (loop
    (unless (or x y) (return))
    (unless (eq (car x) (car y))
      (return-from term-equal nil))
    (setq x (cdr x) y (cdr y)))
  t)

(defun term-hash (x)
  (let ((hash (sxhash (car x))))
    (declare (type non-negative-fixnum hash))
    (dolist (i (cdr x))
      ;; Copied sb-c::mix
      (let* ((mul (logand 3622009729038463111 most-positive-fixnum))
             (xor (logand 608948948376289905 most-positive-fixnum))
             (xy (logand (+ (* hash mul) (enode-hash-code i)) most-positive-fixnum)))
        (setq hash (logand (logxor xor xy (ash xy -5)) most-positive-fixnum))))
    hash))

(cl-custom-hash-table:define-custom-hash-table-constructor make-hash-cons
  :test term-equal :hash-function term-hash)

(defstruct fsym-info
  "Index data for specific function symbol.

NODES store all enodes with this function symbol.

NODE-TABLE maps eclasses (i.e. representative enodes) to member enodes with this
function symbol."
  (nodes nil :type list)
  (node-table (make-hash-table :test 'eq) :type hash-table))

(defvar *analysis-info-registry* (trivial-garbage:make-weak-hash-table))

(defstruct analysis-info
  "Data for e-analysis."
  (name (required-argument :name) :type symbol)
  (make (required-argument :make) :type (function (enode) t))
  (merge (required-argument :merge) :type function)
  (modify (required-argument :modify) :type function))

(defmacro define-analysis (name &key make merge (modify '(constantly nil)))
  `(progn
     (setf (gethash ',name *analysis-info-registry*)
           (make-analysis-info :name ',name :make ,make :merge ,merge :modify ,modify))
     (declaim (inline name))
     (defun ,name (enode) (get-analysis-data enode ',name))))

(defstruct (egraph (:constructor make-egraph (&key analyses enode-limit)))
  "HASH-CONS stores all canonical enodes. CLASSES stores all
eclass (i.e. representative enodes). FSYM-TABLE stores a `fsym-info' entry for
every encountered function symbol.

CLASSES and FSYM-TABLE are only up-to-date after `egraph-rebuild'."
  (hash-cons (make-hash-cons) :type hash-table)
  (classes (make-hash-table :test 'eq) :type hash-table)
  (fsym-table (make-hash-table) :type hash-table)
  (work-list nil :type list)
  (analysis-info-list
   (mapcar (lambda (name) (or (gethash name *analysis-info-registry*)
                              (error "No analysis named ~a." name)))
           (ensure-list analyses))
   :type list)
  (analysis-work-list nil :type list)
  (enode-limit array-total-size-limit :type positive-fixnum))

(defvar *egraph*)
(setf (documentation '*egraph* 'variable) "Current egraph under operation.")

(-> enode-find (enode) enode)
(defun enode-find (enode)
  (let ((parent (enode-parent enode)))
    (if (eclass-info-p parent)
        enode
        (loop
          (let ((grandparent (enode-parent parent)))
            (when (eclass-info-p grandparent)
              (return parent))
            (psetf (enode-parent enode) grandparent
                   parent grandparent
                   enode parent))))))

(defun make-analysis-data (out enode)
  (map-into out (lambda (analysis-info)
                  (funcall (analysis-info-make analysis-info) enode))
            (egraph-analysis-info-list *egraph*)))

(defun merge-analysis-data (eclass data-vec)
  (let ((data-changed nil)
        (class-info (enode-parent eclass)))
    (loop for analysis-info in (egraph-analysis-info-list *egraph*)
          for i from 0 do
            (multiple-value-bind (new-data changed)
                (funcall (analysis-info-merge analysis-info)
                         (svref (eclass-info-analysis-data-vec class-info) i)
                         (svref data-vec i))
              (setf (svref (eclass-info-analysis-data-vec class-info) i) new-data
                    data-changed (or data-changed changed))))
    (when data-changed
      (push eclass (egraph-analysis-work-list *egraph*)))))

(defun modify-analysis-data (eclass)
  (loop for analysis-info in (egraph-analysis-info-list *egraph*)
        for i from 0 do
          (progn
            (funcall (analysis-info-modify analysis-info) eclass
                     (svref (eclass-info-analysis-data-vec (enode-parent eclass)) i))
            ;; modify hook might make ECLASS no longer representative
            (setf eclass (enode-find eclass)))))

(-> get-analysis-data (enode symbol) t)
(defun get-analysis-data (enode name)
  (svref (eclass-info-analysis-data-vec (enode-eclass-info enode))
         (or (position name (egraph-analysis-info-list *egraph*) :key #'analysis-info-name)
             (error "Analysis ~a missing from egraph." name))))

(-> make-enode (list) enode)
(defun make-enode (term)
  (let ((term (cons (car term) (mapcar #'enode-find (cdr term)))))
    (or (gethash term (egraph-hash-cons *egraph*))
        (lret ((data-vec (make-array (length (egraph-analysis-info-list *egraph*))
                                     :initial-element 'unbound))
               (enode (%make-enode :term term)))
          (unless (< (hash-table-count (egraph-hash-cons *egraph*))
                     (egraph-enode-limit *egraph*))
            (throw 'stop :max-enodes))
          (setf (enode-parent enode)
                (%make-eclass-info :nodes (list enode) :analysis-data-vec data-vec))
          (dolist (arg (cdr term))
            (push enode (eclass-info-parents (enode-parent arg)))
            (incf (eclass-info-n-parents (enode-parent arg))))
          (setf (gethash term (egraph-hash-cons *egraph*)) enode)
          (make-analysis-data data-vec enode)
          (modify-analysis-data enode)))))

(-> enode-merge (enode enode) null)
(defun enode-merge (x y)
  (let ((x (enode-find x))
        (y (enode-find y)))
    (unless (eq x y)
      (let ((px (enode-parent x))
            (py (enode-parent y)))
        (when (< (eclass-info-n-parents px)
                 (eclass-info-n-parents py))
          (rotatef x y)
          (rotatef px py))
        (dolist (parent (eclass-info-parents py))
          (setf (enode-canonical-flag parent) nil)
          (push parent (egraph-work-list *egraph*)))
        (setf (eclass-info-nodes px)
              (nreconc (eclass-info-nodes py) (eclass-info-nodes px))
              (enode-parent y) x)
        (merge-analysis-data x (eclass-info-analysis-data-vec py))
        (modify-analysis-data x)
        nil))))

(defun egraph-rebuild ()
  ;; Upward propagation

  ;; Note: we allow duplicates in `egraph-work-list'. Currently we don't bother
  ;; `remove-duplicate' beforehand because doing such seem to actually slow
  ;; things down a bit.
  (loop
    (let ((enode (pop (egraph-work-list *egraph*))))
      (unless enode (return))
      (remhash (enode-term enode) (egraph-hash-cons *egraph*))
      (enode-merge (make-enode (enode-term enode)) enode)))
  ;; Update analysis
  (loop
    (let ((enode (pop (egraph-analysis-work-list *egraph*))))
      (unless enode (return))
      (let ((info (enode-parent enode)))
        (when (eclass-info-p info)
          (dolist (parent (eclass-info-parents info))
            (when (enode-canonical-flag parent)
              (let ((data-vec (make-array (length (egraph-analysis-info-list *egraph*))))
                    (eclass (enode-find parent)))
                (declare (dynamic-extent data-vec))
                (make-analysis-data data-vec parent)
                (merge-analysis-data eclass data-vec)
                (modify-analysis-data eclass))))))))
  ;; Build eclass index by collecting all representative enodes of canonical
  ;; enodes in `egraph-hash-cons'. Note we really need to `enode-find' here,
  ;; because canon-enodes might be non-rep, while rep-enodes might not be canon
  ;; thus not appear in `egraph-hash-cons' either so we can't simply test for
  ;; `enode-representative-p'.
  (clrhash (egraph-classes *egraph*))
  (maphash-values (lambda (node)
                    (setf (gethash (enode-find node) (egraph-classes *egraph*)) t))
                  (egraph-hash-cons *egraph*))
  ;; Build various node index
  (clrhash (egraph-fsym-table *egraph*))
  (maphash-keys (lambda (class)
                  (let ((info (enode-parent class)))
                    (setf (eclass-info-nodes info)
                          (delete-if-not #'enode-canonical-flag (eclass-info-nodes info))
                          (eclass-info-parents info)
                          (delete-if-not #'enode-canonical-flag (eclass-info-parents info))
                          (eclass-info-n-parents info)
                          (length (eclass-info-parents info)))
                    (dolist (node (eclass-info-nodes info))
                      (let ((fsym-info (ensure-gethash (car (enode-term node)) (egraph-fsym-table *egraph*)
                                                       (make-fsym-info))))
                        (push node (gethash class (fsym-info-node-table fsym-info)))
                        (push node (fsym-info-nodes fsym-info))))))
                (egraph-classes *egraph*)))

;;; Utils

(defun hash-table-keys-difference (table-1 table-2)
  (let ((results nil))
    (maphash-keys (lambda (class)
                    (unless (gethash class table-2)
                      (push class results)))
                  table-1)
    results))

(defun check-egraph ()
  "Various sanity check."
  (declare (optimize (debug 3)))
  (let ((classes (make-hash-table))
        (n-parent-list 0)
        (n-parent-list-distinct 0))
    (dolist (enode (hash-table-values (egraph-hash-cons *egraph*)))
      (setf (gethash (enode-find enode) classes) t))
    (format t "~&There're ~a eclasses.~%" (hash-table-count classes))
    (when-let (diff (hash-table-keys-difference classes (egraph-classes *egraph*)))
      (error "Missing eclasses:~% ~a" diff))
    (when-let (diff (hash-table-keys-difference (egraph-classes *egraph*) classes))
      (error "Extra eclasses:~% ~a" diff))
    (dolist (class (hash-table-keys classes))
      (let ((nodes (list-enodes class)))
        (dolist (node nodes)
          (if (enode-canonical-p node)
              (dolist (arg (cdr (enode-term node)))
                (unless (member node (eclass-info-parents (enode-parent arg)))
                  (error "Missing parent link from ~a to ~a" arg node)))
              (error "Non canonical node ~a on ~a's node list" node class)))
        (unless (= (length nodes)
                   (length (remove-duplicates nodes :test 'equal :key #'enode-term)))
          (warn "Duplicates in ~a's enodes:~% ~a" class nodes)))
      (let ((parents (eclass-info-parents (enode-parent class))))
        (dolist (node parents)
          (unless (enode-canonical-p node)
            (error "Non canonical ~a on ~a's parent list" node class))
          (unless (member class (cdr (enode-term node)))
            (error "Extra parent link from ~a to ~a" class node)))
        ;; Currently we allow duplicates in parent list
        (incf n-parent-list (length parents))
        (incf n-parent-list-distinct (length (remove-duplicates parents)))))
    (let ((n-dup (- n-parent-list n-parent-list-distinct)))
      (unless (zerop n-dup)
        (format t "~a/~a (~,2$%) elements in parent list are duplicates.~%"
                n-dup n-parent-list (* 100 (/ n-dup n-parent-list)))))))

(defun list-enodes (enode)
  "List of enodes equivalent to ENODE.

Only contains canonical enodes after `egraph-rebuild'."
  (eclass-info-nodes (enode-eclass-info enode)))

(defun make-term (term)
  (typecase term
    (cons (make-enode (cons (car term) (mapcar #'make-term (cdr term)))))
    (enode term)
    (t (make-enode (list term)))))

(defun egraph-n-enodes (egraph)
  (hash-table-count (egraph-hash-cons egraph)))

(defun egraph-n-eclasses (egraph)
  (hash-table-count (egraph-classes egraph)))
