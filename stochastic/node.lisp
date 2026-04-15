(in-package :egraph)

(defun make-weak-hash-cons ()
  (make-hash-table :test #'term-equal :hash-function #'term-hash :weakness :value))

(defvar *term*)

(declaim (type hash-table *hash-cons*))
(defvar *hash-cons*)

(declaim (inline make-node make-hash-node))

(defun make-node (fsym &rest args)
  (%make-node :fsym fsym :args args))

(defun make-hash-node (fsym &rest args)
  (let ((node (%make-node :fsym fsym :args args)))
    (declare (dynamic-extent node))
    (or (gethash node *hash-cons*)
        (let ((node (%make-hash-node :fsym fsym :args args)))
          (setf (gethash node *hash-cons*) node)))))

(declaim (type function *term-normalizer*))
(defvar *term-normalizer* #'make-node)

(defun make-term-1 (term)
  (if (consp term)
      (apply *term-normalizer* (car term) (mapcar #'make-term-1 (cdr term)))
      term))

(defun demake-term-1 (node)
  (if (node-p node)
      (cons (node-fsym node) (mapcar #'demake-term-1 (node-args node)))
      node))

(defun node-equal (x y)
  (cond
    ((and (not (node-p x)) (not (node-p y))) (eql x y))
    ((and (node-p x) (node-p y))
     (unless (eql (node-fsym x) (node-fsym y))
       (return-from node-equal nil))
     (let ((x (node-args x))
           (y (node-args y)))
       (loop
         (unless (or x y) (return))
         (unless (node-equal (car x) (car y))
           (return-from node-equal nil))
         (setq x (cdr x) y (cdr y))))
     t)))

(defun copy-node-tree (x)
  (if (node-p x)
      (sb-sys:without-gcing
        (let ((new (copy-structure x)))
          (setf (node-args new)
                (mapcar #'copy-node-tree (node-args new)))
          new))
      x))

(defstruct (rose-node (:include node) (:constructor %make-rose-node))
  (weight 0.0 :type single-float)
  (n-rewrites 0 :type fixnum))

(declaim (inline search-rose))

(defun search-rose (node value accessor)
  (labels ((process (node value)
             (dolist (arg (node-args node))
               (let ((a-value (funcall accessor arg)))
                 (if (< a-value value)
                     (decf value a-value)
                     (return-from process (process arg value)))))
             (return-from process (values node value))))
    (process node value)))

(defun rose-normalizer (normalizer rules cost-fn e^beta/2)
  (lambda (fsym &rest args)
    (let* ((node (apply normalizer fsym args))
           (cost (funcall cost-fn node))
           (*term-normalizer* normalizer))
      (dolist (arg args)
        (incf (rose-node-weight node) (rose-node-weight arg)))
      (dolist (rule rules)
        (funcall (get rule 'term-rewrite-1) node
                 (lambda (candidate)
                   (incf (rose-node-n-rewrites node))
                   (incf (rose-node-weight node)
                         (expt e^beta/2 (- cost
                                           (funcall cost-fn candidate)))))))
      node)))
