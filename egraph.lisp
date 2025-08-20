(uiop:define-package :egraph
    (:use :cl :alexandria)
  (:import-from :serapeum #:lret #:lret* #:-> #:string-prefix-p)
  (:import-from :bind #:bind)
  (:export #:make-enode #:make-egraph #:list-enodes
           #:*egraph* #:enode-find #:enode-merge #:egraph-rebuild
           #:enode-representative-p #:enode-canonical-p #:check-egraph
           #:egraph-n-enodes #:egraph-n-eclasses
           #:do-matches #:defrw #:make-term #:run-rewrites
           #:make-analysis-info #:get-analysis-data
           #:greedy-extract))

(in-package :egraph)

;;; E-graph data structure

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

HASH-CODE-AND-FLAGS store 1 in the lowest bit if the node is marked as
non-canonical. `egraph-rebuild' trusts this information to avoid testing all
term arguments for representativeness. Higher bits store a unique hash code."
  (term) (parent)
  (hash-code-and-flags
   (prog1 (ash *hash-code* 1)
     (setf *hash-code*
           (logand (1+ *hash-code*) most-positive-fixnum)))
   :type (unsigned-byte 64)))

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
             (xy (logand (+ (* hash mul) (ash (enode-hash-code-and-flags i) -1))
                         most-positive-fixnum)))
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

(defstruct analysis-info
  "Data for e-analysis."
  (name (required-argument :name) :type symbol)
  (make (required-argument :make) :type function)
  (merge (required-argument :merge) :type function)
  (modify (constantly nil) :type function)
  (work-list nil :type list)
  (data-index 0 :type fixnum))

(defstruct (egraph (:constructor %make-egraph))
  "HASH-CONS stores all canonical enodes. CLASSES stores all
eclass (i.e. representative enodes). FSYM-TABLE stores a `fsym-info' entry for
every encountered function symbol.

CLASSES and FSYM-TABLE are only up-to-date after `egraph-rebuild'."
  (hash-cons (make-hash-cons) :type hash-table)
  (classes (make-hash-table :test 'eq) :type hash-table)
  (fsym-table (make-hash-table) :type hash-table)
  (work-list nil :type list)
  (analysis-info-list nil :type list))

(defvar *egraph*)
(setf (documentation '*egraph* 'variable) "Current egraph under operation.")

(defun make-egraph (&key analyses)
  (let ((analyses (ensure-list analyses)))
    (mapc (lambda (a i)
            (setf (analysis-info-data-index a) i))
          analyses (iota (length analyses)))
    (%make-egraph :analysis-info-list analyses)))

(-> enode-find (enode) enode)
(defun enode-find (enode)
  (let ((enode enode)
        (parent (enode-parent enode)))
    (if (eclass-info-p parent)
        enode
        (loop
          (let ((grandparent (enode-parent parent)))
            (when (eclass-info-p grandparent)
              (return parent))
            (psetf (enode-parent enode) grandparent
                   parent grandparent
                   enode parent))))))

(defun list-enodes (enode)
  "List of enodes equivalent to ENODE.

Only contains canonical enodes after `egraph-rebuild'."
  (eclass-info-nodes (enode-parent (enode-find enode))))

(declaim (inline make-analysis-data modify-analysis-data merge-analysis-data))

(defun make-analysis-data (enode analysis-info)
  (let ((term (enode-term enode))
        (i (analysis-info-data-index analysis-info)))
    (funcall (analysis-info-make analysis-info) (car term)
             (mapcar (lambda (arg)
                       (svref (eclass-info-analysis-data-vec (enode-parent arg)) i))
                     (cdr term)))))

(defun modify-analysis-data (eclass analysis-info)
  (funcall (analysis-info-modify analysis-info) eclass
           (svref (eclass-info-analysis-data-vec (enode-parent eclass))
                  (analysis-info-data-index analysis-info))))

(defun merge-analysis-data (eclass analysis-info data)
  (let ((data-changed nil)
        (class-info (enode-parent eclass))
        (i (analysis-info-data-index analysis-info)))
    (setf (values (svref (eclass-info-analysis-data-vec class-info) i) data-changed)
          (funcall (analysis-info-merge analysis-info)
                   (svref (eclass-info-analysis-data-vec class-info) i)
                   data))
    (when data-changed
      (push eclass (analysis-info-work-list analysis-info)))
    (modify-analysis-data eclass analysis-info)))

(-> get-analysis-data (enode symbol) t)
(defun get-analysis-data (enode name)
  (svref (eclass-info-analysis-data-vec (enode-parent (enode-find enode)))
         (analysis-info-data-index
          (find name (egraph-analysis-info-list *egraph*) :key #'analysis-info-name))))

(defun make-enode (term)
  (let ((term (cons (car term) (mapcar #'enode-find (cdr term)))))
    (or (gethash term (egraph-hash-cons *egraph*))
        (lret ((enode (%make-enode :term term)))
          (setf (enode-parent enode)
                (%make-eclass-info :nodes (list enode)
                                   :analysis-data-vec
                                   (map 'vector (lambda (info)
                                                  (make-analysis-data enode info))
                                        (egraph-analysis-info-list *egraph*))))
          (dolist (arg (cdr term))
            (push enode (eclass-info-parents (enode-parent arg)))
            (incf (eclass-info-n-parents (enode-parent arg))))
          (setf (gethash term (egraph-hash-cons *egraph*)) enode)
          (map nil (lambda (info)
                     (modify-analysis-data (enode-find enode) info))
               (egraph-analysis-info-list *egraph*))))))


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
          (setf (enode-hash-code-and-flags parent)
                (logior (enode-hash-code-and-flags parent) 1))
          (push parent (egraph-work-list *egraph*)))
        (setf (eclass-info-nodes px)
              (nreconc (eclass-info-nodes py) (eclass-info-nodes px))
              (enode-parent y) x)
        (dolist (info (egraph-analysis-info-list *egraph*))
          (merge-analysis-data x info
                               (svref (eclass-info-analysis-data-vec py)
                                      (analysis-info-data-index info))))
        nil))))

(declaim (inline enode-representative-p enode-canonical-p))

;; Be aware that representative enode might be non-canonical!
(defun enode-representative-p (enode)
  (eclass-info-p (enode-parent enode)))

(defun enode-canonical-p (enode)
  (every #'enode-representative-p (cdr (enode-term enode))))

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
  (flet ((enode-canonical-p (enode)
           (zerop (logand (enode-hash-code-and-flags enode) 1))))
    ;; Update analysis
    (map nil (lambda (analysis-info)
               (loop
                 (let ((enode (pop (analysis-info-work-list analysis-info))))
                   (unless enode (return))
                   (let ((info (enode-parent enode)))
                     (when (eclass-info-p info)
                       (dolist (parent (eclass-info-parents info))
                         (when (enode-canonical-p parent)
                           (merge-analysis-data (enode-find parent) analysis-info
                                                (make-analysis-data parent analysis-info)))))))))
         (egraph-analysis-info-list *egraph*))
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
                            (delete-if-not #'enode-canonical-p (eclass-info-nodes info))
                            (eclass-info-parents info)
                            (delete-if-not #'enode-canonical-p (eclass-info-parents info))
                            (eclass-info-n-parents info)
                            (length (eclass-info-parents info)))
                      (dolist (node (eclass-info-nodes info))
                        (let ((fsym-info (ensure-gethash (car (enode-term node)) (egraph-fsym-table *egraph*)
                                                         (make-fsym-info))))
                          (push node (gethash class (fsym-info-node-table fsym-info)))
                          (push node (fsym-info-nodes fsym-info))))))
                  (egraph-classes *egraph*))))

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
              (error "Non canonical node ~a on ~a's circular node list" node class)))
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

;;; E-match/rewrite compiler

(defun var-p (object)
  (typecase object
    (symbol (string-prefix-p "?" (symbol-name object)))))

(defvar *fsym-info-var-alist*)

(defun gensym-1 (thing)
  (make-gensym (prin1-to-string thing)))

(defun parse-pattern (pat eclass-var)
  "Convert PAT into a list of the form ((eclass-var fsym arg-var...) ...)."
  (cond ((consp pat)
         (let* ((fsym (car pat))
                (arg-vars (mapcar (lambda (arg) (if (var-p arg) arg (gensym-1 fsym))) (cdr pat))))
           (cons (list* eclass-var fsym arg-vars)
                 (mapcan (lambda (arg var)
                           (unless (var-p arg)
                             (parse-pattern arg var)))
                         (cdr pat) arg-vars))))
        ((var-p pat) (error "Single variable pattern should not be handled here."))
        (t ;; Non-variable atoms are short hand for 0-arity function symbol
         (parse-pattern (list pat) eclass-var))))

(defun expand-match (bound-vars subst-alist cont-expr)
  "Generate code that solves for SUBST-ALIST (as returned by `parse-pattern') then
evaluate CONT-EXPR."
  (if subst-alist
      (bind ((((var fsym . arg-vars) . rest) subst-alist)
             ((:flet lisp-var (var))
              (if (member var bound-vars)
                  (gensym-1 fsym)
                  (progn (push var bound-vars) var)))
             (lisp-arg-vars (mapcar #'lisp-var arg-vars))
             (fsym-info-var (serapeum:ensure (assoc-value *fsym-info-var-alist* fsym)
                              (gensym-1 fsym)))
             (lhs-bound-p (member var bound-vars))
             (node-var (lisp-var var)))
        ;; Currently we use indexes (in `fsym-info') as single source of truth
        ;; for matching, thus no-need to `enode-find' representative of VAR
        ;; (even if VAR is non-rep, it once was when we built the index in
        ;; `egraph-rebuild'
        `(dolist (,node-var ,(if lhs-bound-p
                                 `(gethash ,var (fsym-info-node-table ,fsym-info-var))
                                 `(fsym-info-nodes ,fsym-info-var)))
           (destructuring-bind ,lisp-arg-vars (cdr (enode-term ,node-var))
             (declare (ignorable ,@lisp-arg-vars))
             (when (and ,@ (mapcan (lambda (lisp-var var)
                                     (when (and (var-p var) (not (var-p lisp-var)))
                                       `((eq ,lisp-var ,var))))
                                   lisp-arg-vars arg-vars))
               ,(expand-match bound-vars rest cont-expr)))))
      cont-expr))

(defun expand-template (tmpl)
  "Generate code that creates an enode according to TMPL (rhs of rewrite rule)."
  (labels ((process (tmpl)
             (cond ((consp tmpl)
                    `(let ((list (list ',(car tmpl) ,@ (mapcar #'process (cdr tmpl)))))
                       (declare (dynamic-extent list))
                       (make-enode list)))
                   ((var-p tmpl) tmpl)
                   (t (process (list tmpl))))))
    (process tmpl)))

(defmacro do-matches ((top-node-var pat) &body body)
  "Evaluate BODY for every PAT match in EGRAPH.

BODY is evaluated with variables in PAT bound to matched eclasses and
TOP-NODE-VAR bound to the enode matching PAT."
  (if (var-p pat) ; Special case for single variable PAT that scans all enodes
      `(maphash-values
        (lambda (fsym-info)
          (dolist (,pat (fsym-info-nodes fsym-info))
            (let ((,top-node-var ,pat)) ,@body)))
        (egraph-fsym-table *egraph*))
      (let* ((*fsym-info-var-alist* nil)
             (match-body
               (expand-match nil (parse-pattern pat top-node-var)
                             `(locally ,@body))))
        `(let ,(mapcar (lambda (kv) `(,(cdr kv)
                                      (ensure-gethash ',(car kv) (egraph-fsym-table *egraph*)
                                                      (make-fsym-info))))
                       *fsym-info-var-alist*)
           ,match-body))))

(defvar *max-enodes* nil)

(defmacro defrw (name lhs rhs &key (guard t))
  "Define a rule that rewrites LHS to RHS when GUARD is evaluated to true."
  `(defun ,name ()
     (do-matches (top-node ,lhs)
       (when (and *max-enodes* (>= (egraph-n-enodes *egraph*) *max-enodes*))
         (throw 'stop :max-enodes))
       (when ,guard
         (enode-merge top-node ,(expand-template rhs))))))

;;; Utils

(defun make-term (term)
  (if (consp term)
      (make-enode (cons (car term) (mapcar #'make-term (cdr term))))
      (make-enode (list term))))

(declaim (inline egraph-n-enodes egraph-n-eclasses))

(defun egraph-n-enodes (egraph)
  (hash-table-count (egraph-hash-cons egraph)))

(defun egraph-n-eclasses (egraph)
  (hash-table-count (egraph-classes egraph)))

(defun run-rewrites (rules &key max-iter check ((:max-enodes *max-enodes*)))
  "Run RULES repeatly on `*egraph*' until some stop criterion.

Returns the reason for termination: one of :max-iter, :max-enodes, :saturate.

Note: this function does not call `egraph-rebuild' upfront. Particularly, if you
have added some terms to EGRAPH, you MUST call `egraph-rebuild' before calling
this function."
  (let ((n-enodes (egraph-n-enodes *egraph*))
        (n-eclasses (egraph-n-eclasses *egraph*))
        (n-iter 0))
    (catch 'stop
      (loop
        (when (and max-iter (>= n-iter max-iter))
          (throw 'stop :max-iter))
        (when (and *max-enodes* (>= n-enodes *max-enodes*))
          (throw 'stop :max-enodes))
        (unwind-protect
             (dolist (rule (ensure-list rules))
               (funcall rule))
          (egraph-rebuild))
        (when check (check-egraph))
        (incf n-iter)
        (let ((n-enodes-1 (egraph-n-enodes *egraph*))
              (n-eclasses-1 (egraph-n-eclasses *egraph*)))
          (if (and (= n-enodes n-enodes-1) (= n-eclasses n-eclasses-1))
              (return :saturate)
              (setq n-enodes n-enodes-1 n-eclasses n-eclasses-1)))))))

;;; Extract

(defun greedy-extract (enode cost-fn)
  "Greedy extract a term for ENODE from `*egraph*' using COST-FN.

COST-FN should accept 2 arguments: a function symbol and a list of costs for
each argument eclass. It should return a number.

ENODE can also be a list of enodes, and a list of terms will be returned."
  (let ((selections (make-hash-table))) ;; map eclass to (cost . enode)
    (loop
      (let (dirty)
        (maphash-keys (lambda (class)
                        (let ((selection (gethash class selections)))
                          (dolist (enode (list-enodes class))
                            (let* ((term (enode-term enode))
                                   (new-cost
                                     (funcall cost-fn (car term)
                                              (mapcar (compose #'car (rcurry #'gethash selections))
                                                      (cdr term)))))
                              (when (if selection (and new-cost (< new-cost (car selection)))
                                        new-cost)
                                (setq selection (cons new-cost enode)
                                      dirty t))))
                          (setf (gethash class selections) selection)))
                      (egraph-classes *egraph*))
        (unless dirty (return))))
    (labels ((build-term (class)
               (let ((term (enode-term (cdr (gethash class selections)))))
                 (if (cdr term)
                     (cons (car term) (mapcar #'build-term (cdr term)))
                     (car term)))))
      (if (listp enode)
          (mapcar (compose #'build-term #'enode-find) enode)
          (build-term (enode-find enode))))))
