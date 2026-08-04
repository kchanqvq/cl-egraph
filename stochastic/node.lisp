(in-package :egraph)

(define-variadic-structure rose-node
  "N-REWRITES = -1 means the rose tree data hasn't been computed for
 this node."
  (weight 0.0 :type single-float)
  (n-rewrites -1 :type fixnum)
  (cost 1 :type fixnum)
  (fsym)
  (arg))

(defvar *term*)

(declaim (inline make-node))

(declaim (type function *term-normalizer*))
(defvar *term-normalizer* #'identity)

(defun make-term-1 (term)
  (if (consp term)
      (funcall *term-normalizer*
               (apply #'vector 0.0 -1 1 (car term) (mapcar #'make-term-1 (cdr term))))
      term))

(defun demake-term-1 (term)
  (if (vectorp term)
      (cons (rose-node-fsym term)
            (map-rose-node-args #'demake-term-1 term))
      term))

(defmacro def-search-rose (name accessor type)
  `(progn
     (declaim (ftype (function (rose-node ,type) (values rose-node function ,type)) ,name))
     (defun ,name (node value)
       (labels ((process (node context value)
                  (declare (optimize speed)
                           (rose-node node)
                           (function context)
                           (,type value))
                  (do-rose-node-args ((arg i) node)
                    (when (vectorp arg)
                      (let ((a-value (,accessor arg)))
                        (if (<= a-value value)
                            (decf value a-value)
                            (return-from process
                              (process arg
                                       (lambda (node-1)
                                         (let ((new-node (copy-seq node)))
                                           (setf (rose-node-weight new-node) 0.0
                                                 (rose-node-n-rewrites new-node) -1
                                                 (rose-node-cost new-node) 1
                                                 (rose-node-arg i new-node) node-1)
                                           (funcall context (funcall *term-normalizer* new-node))))
                                       value))))))
                  (return-from process (values node context value))))
         (process node #'identity value)))))

(def-search-rose search-rose-n-rewrites rose-node-n-rewrites fixnum)
(def-search-rose search-rose-weight rose-node-weight single-float)
