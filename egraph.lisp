(uiop:define-package :egraph
    (:use :cl :alexandria)
  (:import-from :serapeum #:lret #:lret* #:-> #:string-prefix-p)
  (:export #:make-enode #:make-egraph #:enode-find #:do-enodes #:list-enodes
           #:egraph-merge #:egraph-rebuild
           #:enode-representative-p #:enode-canonical-p #:check-egraph
           #:define-rewrite #:make-term))

(in-package :egraph)

;;; E-graph

(defstruct (eclass-info (:constructor %make-eclass-info))
  "Metadata for the eclass.

Should only appear in reprensetative enode's PARENT slot."
  (nodes)
  (parents)
  (n-parents 0 :type fixnum))

(defstruct (enode (:constructor %make-enode))
  "NEXT pointer forms a circular linked list of all enodes in the same eclass.

PARENT is either another enode in the same eclass, or an `eclass-info' if this
enode is the representative of its own eclass."
  (term) (next) (parent))

(defmethod print-object ((self enode) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~:[~;REP ~]~a" (enode-representative-p self) (enode-term self))))

(defstruct egraph
  (hash-cons (make-hash-table :test 'equal) :type hash-table)
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

(defun make-enode (egraph term)
  (enode-find
   (ensure-gethash
    term
    (egraph-hash-cons egraph)
    (lret* ((eclass-info (%make-eclass-info))
            (enode (%make-enode :parent eclass-info :term term)))
      (setf (gethash enode (egraph-classes egraph)) t)
      (setf (eclass-info-nodes eclass-info) enode)
      (setf (enode-next enode) enode)
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
              (nreconc (eclass-info-parents py) (egraph-work-list egraph)))
        (remhash y (egraph-classes egraph))
        (setf (enode-parent y) x)
        (rotatef (enode-next x) (enode-next y))
        nil))))

(defmacro do-enodes-for-eclass-info ((var eclass-info) &body body)
  (with-gensyms (init)
    `(let* ((,init (eclass-info-nodes ,eclass-info))
            (,var ,init))
       (loop
         ,@body
           (setf ,var (enode-next ,var))
           (when (eq ,var ,init)
             (return))))))

(defmacro do-enodes ((var enode) &body body)
  "Evaluate BODY with VAR bound to each enode equivalanet to ENODE."
  `(do-enodes-for-eclass-info (,var (enode-parent (enode-find ,enode))) ,@body))

(defun list-enodes (enode)
  (let (results)
    (do-enodes (node enode)
      (push node results))
    results))

(defun egraph-rebuild (egraph)
  (loop
    (let ((enode (pop (egraph-work-list egraph))))
      (unless enode (return))
      (let* ((term (enode-term enode))
             (canon-args (mapcar #'enode-find (cdr term))))
        (remhash (enode-term enode) (egraph-hash-cons egraph))
        (egraph-merge egraph (make-enode egraph (cons (car (enode-term enode)) canon-args)) enode))))
  (flet ((enode-canonical-p (enode)
           (eq (gethash (enode-term enode) (egraph-hash-cons egraph))
               enode)))
    (maphash
     (lambda (class ignore)
       (declare (ignore ignore))
       (let ((start (eclass-info-nodes (enode-parent class))))
         (loop
           (when (enode-canonical-p start)
             (return))
           (setf start (enode-next start)))
         (setf (eclass-info-nodes (enode-parent class)) start)
         (let* ((prev start)
                (cur (enode-next start)))
           (loop
             (when (enode-canonical-p cur)
               (setf (enode-next prev) cur
                     prev cur))
             (setf cur (enode-next cur))
             (when (eq cur start)
               (setf (enode-next prev) start)
               (return)))))
       (setf (eclass-info-parents (enode-parent class))
             (delete-if-not #'enode-canonical-p (eclass-info-parents (enode-parent class)))
             (eclass-info-n-parents (enode-parent class))
             (length (eclass-info-parents (enode-parent class)))))
     (egraph-classes egraph))))

(defun enode-representative-p (enode)
  (eclass-info-p (enode-parent enode)))

(defun enode-canonical-p (enode)
  (every #'enode-representative-p (cdr (enode-term enode))))

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
             `(do-enodes-for-eclass-info (node (enode-parent ,eclass-var))
                (let ((term (enode-term node)))
                  (when (eql (car term) ',fsym)
                    (destructuring-bind ,arg-vars (cdr term)
                      (declare (ignorable ,@arg-vars))
                      ,(process-args (cdr pat) arg-vars pat-var-alist cont))))))))
        ((var-p pat)
         (if-let (pat-var (assoc-value pat-var-alist pat))
           `(progn
              (unless (eql ,eclass-var ,pat-var))
              ,(funcall cont pat-var-alist))
           (funcall cont (cons (cons pat eclass-var) pat-var-alist))))
        (t ;; Non-variable atoms are short hand for 0-arity function symbol
         (expand-match (list pat) eclass-var pat-var-alist cont))))

(defun expand-template (tmpl egraph-var pat-var-alist)
  (labels ((process (tmpl)
             (cond ((consp tmpl)
                    `(make-enode ,egraph-var
                                 (list ',(car tmpl) ,@ (mapcar #'process (cdr tmpl)))))
                   ((var-p tmpl)
                    (if-let (pat-var (assoc-value pat-var-alist tmpl))
                      `(enode-find ,pat-var)
                      (error "Variable ~a not present in match pattern" tmpl)))
                   (t (process (list tmpl))))))
    (process tmpl)))

(defun expand-rewrite (lhs rhs egraph-var)
  `(maphash
    (lambda (eclass ignore)
      (declare (ignore ignore))
      ,(expand-match lhs 'eclass nil
                     (lambda (pat-var-alist)
                       `(egraph-merge ,egraph-var
                                      eclass
                                      ,(expand-template rhs egraph-var pat-var-alist)))))
    (egraph-classes ,egraph-var)))

(defmacro define-rewrite (name lhs rhs)
  `(defun ,name (egraph)
     ,(expand-rewrite lhs rhs 'egraph)))

;;; Utils

(defun make-term (egraph term)
  (labels ((process (term)
             (if (consp term)
                 (make-enode egraph (cons (car term) (mapcar #'process (cdr term))))
                 (make-enode egraph (list term)))))
    (process term)))
