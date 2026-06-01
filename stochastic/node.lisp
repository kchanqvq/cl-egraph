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

(defun demake-term-1 (term)
  (if (node-p term)
      (cons (node-fsym term) (mapcar #'demake-term-1 (node-args term)))
      term))

(defstruct (rose-node (:include node) (:constructor %make-rose-node))
  "N-REWRITES = -1 means the rose tree data hasn't been computed for
 this node."
  (weight 0.0 :type single-float)
  (n-rewrites -1 :type fixnum))

(defmacro do-args ((arg-var with-args) args &body body)
  "Bind ARG-VAR to successive elements of ARGS and evaluate BODY.

When evaluating BODY, WITH-ARGS is locally bound to a macro with syntax
(WITH-ARGS (ARGS-VAR) NEW-ARG BODY-1...). WITH-ARGS binds ARGS-VAR to a new list
that is the same as ARGS, except that the current element (bound to ARG-VAR) is
replaced with NEW-ARG. ARGS-VAR can only be used during the dynamic extent of BODY-1."
  `(do* ((tail ,args (cdr tail))
         (,arg-var (car tail) (car tail))
         (revtail))
        ((null tail))
     (macrolet ((,with-args ((args-var) new-arg &body body)
                  `(let ((,args-var (cons ,new-arg (cdr tail))))
                     (declare (dynamic-extent ,args-var))
                     (dolist (arg revtail)
                       (push arg ,args-var))
                     ,@body)))
       ,@body)
     (push ,arg-var revtail)))

(defmacro def-search-rose (name accessor type)
  `(progn
     (declaim (ftype (function (rose-node ,type) (values rose-node function ,type)) ,name))
     (defun ,name (node value)
       (labels ((process (node context value)
                  (declare (optimize speed)
                           (rose-node node)
                           (function context)
                           (,type value))
                  (do-args (arg with-args) (node-args node)
                    (when (rose-node-p arg)
                      (let ((a-value (,accessor arg)))
                        (if (<= a-value value)
                            (decf value a-value)
                            (return-from process
                              (process arg
                                       (lambda (node-1)
                                         (with-args (args) node-1
                                           (funcall context
                                                    (apply *term-normalizer* (node-fsym node) args))))
                                       value))))))
                  (return-from process (values node context value))))
         (process node #'identity value)))))

(def-search-rose search-rose-n-rewrites rose-node-n-rewrites fixnum)
(def-search-rose search-rose-weight rose-node-weight single-float)
