(uiop:define-package :egraph
    (:use :cl :alexandria)
  (:import-from :serapeum #:lret #:lret* #:-> #:string-prefix-p)
  (:export #:make-enode #:make-egraph #:enode-find #:list-enodes #:map-enodes
           #:egraph-merge #:egraph-rebuild
           #:enode-representative-p #:enode-canonical-p #:check-egraph
           #:define-rewrite #:defrw #:make-term
           #:greedy-extract))

(in-package :egraph)

;;; E-graph

(defstruct (eclass-info (:constructor %make-eclass-info))
  "Metadata for the eclass.

Should only appear in reprensetative enode's PARENT slot.

NODES and PARENTS only store canonical enodes after `egraph-rebuild'."
  (nodes nil :type list)
  (node-table (make-hash-table) :type hash-table)
  (parents nil :type list)
  (n-parents 0 :type fixnum))

(declaim (type non-negative-fixnum *hash-code*))
(global-vars:define-global-var *hash-code* 0)

(defstruct (enode (:constructor %make-enode))
  "PARENT is either another enode in the same eclass, or an `eclass-info' if this
enode is the representative of its own eclass."
  (term) (parent)
  (hash-code (prog1 *hash-code*
               (setf *hash-code*
                     (logand (1+ *hash-code*) most-positive-fixnum)))
   :type non-negative-fixnum))

(defmethod print-object ((self enode) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~:[~;REP ~]~a" (enode-representative-p self) (enode-term self))))

(defun term-equal (x y)
  (loop
    (unless (eq (car x) (car y))
      (return-from term-equal nil))
    (setq x (cdr x) y (cdr y))
    (unless (or x y) (return)))
  t)

(defun term-hash (x)
  (let ((hash (sxhash (car x))))
    (declare (type non-negative-fixnum hash))
    (dolist (i (cdr x))
      ;; Copied sb-c::mix
      (let* ((mul (logand 3622009729038463111 most-positive-fixnum))
             (xor (logand 608948948376289905 most-positive-fixnum))
             (xy (logand (+ (* hash mul) (enode-hash-code i))
                         most-positive-fixnum)))
        (setq hash (logand (logxor xor xy (ash xy -5)) most-positive-fixnum))))
    hash))

(defstruct egraph
  (hash-cons (make-hash-table :test 'term-equal :hash-function #'term-hash)
   :type hash-table)
  (classes (make-hash-table :test 'eq) :type hash-table)
  (work-list))

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

(declaim (inline map-enodes))
(defun map-enodes (fn enode)
  (maphash fn (eclass-info-node-table (enode-parent (enode-find enode)))))

(defun make-enode (egraph term)
  (let ((term (cons (car term) (mapcar #'enode-find (cdr term)))))
    (ensure-gethash
     term
     (egraph-hash-cons egraph)
     (lret* ((eclass-info (%make-eclass-info))
             (enode (%make-enode :parent eclass-info :term term)))
       (setf (gethash enode (egraph-classes egraph)) t)
       (setf (eclass-info-nodes eclass-info) (list enode))
       (dolist (arg (cdr term))
         (push enode (eclass-info-parents (enode-parent arg)))
         (incf (eclass-info-n-parents (enode-parent arg))))))))

(-> egraph-merge (egraph enode enode) null)
(defun egraph-merge (egraph x y)
  (let ((x (enode-find x))
        (y (enode-find y)))
    (unless (eq x y)
      (let ((px (enode-parent x))
            (py (enode-parent y)))
        (when (< (eclass-info-n-parents px)
                 (eclass-info-n-parents py))
          (rotatef x y)
          (rotatef px py))
        (setf (egraph-work-list egraph)
              (nreconc (eclass-info-parents py) (egraph-work-list egraph))
              (eclass-info-nodes px)
              (nreconc (eclass-info-nodes py) (eclass-info-nodes px)))
        (remhash y (egraph-classes egraph))
        (setf (enode-parent y) x)
        nil))))

(declaim (inline enode-representative-p enode-canonical-p))

;; Be aware that representative enode might be non-canonical!
(defun enode-representative-p (enode)
  (eclass-info-p (enode-parent enode)))

(defun enode-canonical-p (enode)
  (every #'enode-representative-p (cdr (enode-term enode))))

(defun egraph-rebuild (egraph)
  (loop
    (let ((enode (pop (egraph-work-list egraph))))
      (unless enode (return))
      (remhash (enode-term enode) (egraph-hash-cons egraph))
      (egraph-merge egraph (make-enode egraph (enode-term enode)) enode)))
  (maphash
   (lambda (class ignore)
     (declare (ignore ignore))
     (let ((info (enode-parent class)))
       (setf (eclass-info-nodes info)
             (delete-if-not #'enode-canonical-p (eclass-info-nodes info))
             (eclass-info-parents info)
             (delete-if-not #'enode-canonical-p (eclass-info-parents info))
             (eclass-info-n-parents info)
             (length (eclass-info-parents info)))
       (clrhash (eclass-info-node-table info))
       (dolist (node (eclass-info-nodes info))
         (push node (gethash (car (enode-term node))
                             (eclass-info-node-table info))))))
   (egraph-classes egraph)))

(defun check-egraph (egraph)
  "Various sanity check."
  (declare (optimize (debug 3)))
  (let ((classes (make-hash-table)))
    (dolist (enode (hash-table-values (egraph-hash-cons egraph)))
      (setf (gethash (enode-find enode) classes) t))
    (format t "~&There're ~a eclasses.~%" (hash-table-count classes))
    (when-let (diff (set-exclusive-or (hash-table-keys classes)
                                      (hash-table-keys (egraph-classes egraph))))
      (error "Difference between classes slot and actual eclasses:~% ~a" diff))
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
        (unless (= (length parents)
                   (length (remove-duplicates parents :test 'equal :key #'enode-term)))
          (warn "Duplicates in ~a's parent list:~% ~a" class parents))
        (dolist (node parents)
          (unless (enode-canonical-p node)
            (error "Non canonical ~a on ~a's parent list" node class))
          (unless (member class (cdr (enode-term node)))
            (error "Extra parent link from ~a to ~a" class node)))))))

;;; E-match/rewrite compiler

(defun var-p (object)
  (typecase object
    (symbol (string-prefix-p "?" (symbol-name object)))))

(defun expand-match (pat eclass-var pat-var-alist cont)
  (cond ((consp pat)
         (let* ((fsym (car pat))
                (arg-vars (make-gensym-list (length (cdr pat)) fsym)))
           (labels ((process-args (args arg-vars pat-var-alist cont)
                      (if args
                          (expand-match
                           (car args) (car arg-vars)
                           pat-var-alist
                           (lambda (pat-var-alist)
                             (process-args (cdr args) (cdr arg-vars) pat-var-alist cont)))
                          (funcall cont pat-var-alist))))
             ;; If we run this not right after rebuilding, eclass-var might be
             ;; bound to non-representative enode, thus need the `enode-find'
             `(dolist (node (gethash ',fsym (eclass-info-node-table (enode-parent (enode-find ,eclass-var)))))
                (destructuring-bind ,arg-vars (cdr (enode-term node))
                  (declare (ignorable ,@arg-vars))
                  ,(process-args (cdr pat) arg-vars pat-var-alist cont))))))
        ((var-p pat)
         (if-let (pat-var (assoc-value pat-var-alist pat))
           `(when (eql ,eclass-var ,pat-var)
              ,(funcall cont pat-var-alist))
           (funcall cont (cons (cons pat eclass-var) pat-var-alist))))
        (t ;; Non-variable atoms are short hand for 0-arity function symbol
         (expand-match (list pat) eclass-var pat-var-alist cont))))

(defun expand-template (tmpl egraph-var)
  (labels ((process (tmpl)
             (cond ((consp tmpl)
                    `(make-enode ,egraph-var
                                 (list ',(car tmpl) ,@ (mapcar #'process (cdr tmpl)))))
                   ((var-p tmpl) tmpl)
                   (t (process (list tmpl))))))
    (process tmpl)))

(defmacro define-rewrite (name egraph-var pat &body body)
  `(defun ,name (egraph)
     (dolist (eclass (hash-table-keys (egraph-classes ,egraph-var)))
       ,(expand-match pat 'eclass nil
                      (lambda (pat-var-alist)
                        (let ((pat-vars (mapcar #'car pat-var-alist))
                              (match-vars (mapcar #'cdr pat-var-alist)))
                          `(egraph-merge ,egraph-var
                                         eclass
                                         (let ,(mapcar #'list pat-vars match-vars)
                                           (declare (ignorable ,@pat-vars))
                                           ,@body))))))))

(defmacro defrw (name lhs rhs &key (guard t))
  `(define-rewrite ,name egraph ,lhs
     (when ,guard ,(expand-template rhs 'egraph))))

;;; Utils

(defun make-term (egraph term)
  (labels ((process (term)
             (if (consp term)
                 (make-enode egraph (cons (car term) (mapcar #'process (cdr term))))
                 (make-enode egraph (list term)))))
    (process term)))

;;; Extract

(defun greedy-extract (egraph enode cost-fn)
  (let ((selections (make-hash-table))) ;; map eclass to (cost . enode)
    (loop
      (let (dirty)
        (maphash
         (lambda (class ignore)
           (declare (ignore ignore))
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
         (egraph-classes egraph))
        (unless dirty (return))))
    (labels ((build-term (class)
               (let ((term (enode-term (cdr (gethash class selections)))))
                 (if (cdr term)
                     (cons (car term) (mapcar #'build-term (cdr term)))
                     (car term)))))
      (build-term (enode-find enode)))))
