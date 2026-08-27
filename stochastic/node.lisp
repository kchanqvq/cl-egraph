(in-package :ggs/stochastic)

(define-variadic-structure rose-node
  "N-REWRITES = -1 means the rose tree data hasn't been computed for
 this node."
  (weight 0.0 :type single-float)
  (n-rewrites -1 :type fixnum)
  (cost 0 :type non-negative-fixnum)
  (fsym)
  (arg))

(defvar *node*)

(defun term-node (term cost-fn)
  (labels ((process (term)
             (if (consp term)
                 (let ((new-node (apply #'vector 0.0 -1 0.0 (car term) (mapcar #'process (cdr term)))))
                   (setf (rose-node-cost new-node) (funcall cost-fn new-node))
                   new-node)
                 term)))
    (process term)))

(defun node-term (term)
  (if (rose-node-p term)
      (cons (rose-node-fsym term)
            (map-rose-node-args #'node-term term))
      term))

(declaim (inline node-replace-arg))
(defun node-replace-arg (node i new-arg cost-fn)
  (declare (optimize (speed 3) (safety 0))
           (rose-node node)
           ((function (t) fixnum) cost-fn)
           (fixnum i))
  (let* ((n (length node))
         (new-node (make-array n))
         (i-1 (+ i +rose-node-args-offset+)))
    (setf (rose-node-weight new-node) 0.0
          (rose-node-n-rewrites new-node) -1
          (rose-node-fsym new-node) (rose-node-fsym node)
          (rose-node-cost new-node) 0)
    (loop for j of-type fixnum from +rose-node-args-offset+ below n
          for old-arg = (svref node j)
          do (setf (svref new-node j) (if (= i-1 j) new-arg old-arg)))
    (setf (rose-node-cost new-node) (funcall cost-fn new-node))
    new-node))

(defmacro def-search-rose (name accessor type)
  `(progn
     (declaim (ftype (function (rose-node ,type (function (t) fixnum))
                               (values rose-node function ,type))
                     ,name))
     (defun ,name (node value cost-fn)
       (labels ((process (node context value)
                  (declare (optimize speed)
                           (rose-node node)
                           (function context)
                           (,type value))
                  (do-rose-node-args ((arg i) node)
                    (when (rose-node-p arg)
                      (let ((a-value (,accessor arg)))
                        (if (<= a-value value)
                            (decf value a-value)
                            (return-from process
                              (process arg
                                       (lambda (node-1)
                                         (funcall context (node-replace-arg node i node-1 cost-fn)))
                                       value))))))
                  (return-from process (values node context value))))
         (process node #'identity value)))))

(def-search-rose search-rose-n-rewrites rose-node-n-rewrites fixnum)
(def-search-rose search-rose-weight rose-node-weight single-float)
