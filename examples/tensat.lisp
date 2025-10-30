(uiop:define-package :egraph/examples/tensat
    (:use #:cl #:egraph)
  (:import-from #:alexandria #:compose #:map-iota)
  (:import-from #:bind #:bind)
  (:import-from #:serapeum #:lret #:lastcar))

(in-package :egraph/examples/tensat)

(defun enode-value (enode)
  (assert (null (cdr (enode-term enode))))
  (car (enode-term enode)))

(defmacro defrw2 (name lhs rhs)
  `(progn
     (defrw ,name ,lhs ,rhs)
     (defrw ,(alexandria:symbolicate "-" name) ,rhs ,lhs)))

(defrw2 ewadd-is-associative (ewadd ?x (ewadd ?y ?z)) (ewadd (ewadd ?x ?y) ?z))
(defrw2 ewadd-is-communitative (ewadd ?x ?y) (ewadd ?y ?x))
(defrw2 ewmul-is-associative (ewmul ?x (ewmul ?y ?z)) (ewmul (ewmul ?x ?y) ?z))
(defrw2 ewmul-is-communitative (ewmul ?x ?y) (ewmul ?y ?x))
(defrw2 distributive-0 (ewmul (ewadd ?x ?y) ?z) (ewadd (ewmul ?x ?z) (ewmul ?y ?z)))
(defrw2 smul-is-associative (smul ?x (smul ?y ?z)) (smul (smul ?x ?y) ?z))
(defrw2 distributive-1 (smul (sadd ?x ?y) ?z) (sadd (smul ?x ?z) (smul ?y ?z)))
(defrw2 operator-commutativity-0 (smul (ewmul ?x ?y) ?w) (ewmul ?x (smul ?y ?w)))
(defrw2 transpose-is-its-own-inverse (transpose (transpose ?x)) ?x)
(defrw2 operator-commutativity-1 (transpose (ewadd ?x ?y)) (ewadd (transpose ?x) (transpose ?y)))
(defrw2 operator-commutativity-2 (transpose (ewmul ?x ?y)) (ewmul (transpose ?x) (transpose ?y)))
(defrw2 operator-commutativity-3 (smul (transpose ?x) ?w) (transpose (smul ?x ?w)))
(defrw2 matmul-is-associative (matmul ?x (matmul ?y ?z)) (matmul (matmul ?x ?y) ?z))
(defrw2 matmul-is-linear-0 (smul (matmul ?x ?y) ?w) (matmul ?x (smul ?y ?w)))
(defrw2 matmul-is-linear-1 (matmul ?x (ewadd ?y ?z)) (ewadd (matmul ?x ?y) (matmul ?x ?z)))
(defrw2 matmul-and-transpose (transpose (matmul ?x ?y)) (matmul (transpose ?y) (transpose ?x)))
(defrw2 conv-is-bilinear-0 (conv2d ?sx ?sy ?p ?c (smul ?x ?w) ?y) (conv2d ?sx ?sy ?p ?c ?x (smul ?y ?w)))
(defrw2 conv-is-bilinear-1 (smul (conv2d ?sx ?sy ?p actnone ?x ?y) ?w) (conv2d ?sx ?sy ?p actnone (smul ?x ?w) ?y))
(defrw2 conv-is-bilinear-2 (conv2d ?sx ?sy ?p actnone ?x (ewadd ?y ?z)) (ewadd (conv2d ?sx ?sy ?p actnone ?x ?y) (conv2d ?sx ?sy ?p 0 ?x ?z)))
(defrw2 conv-is-bilinear-3 (conv2d ?sx ?sy ?p actnone (ewadd ?x ?y) ?z) (ewadd (conv2d ?sx ?sy ?p actnone ?x ?z) (conv2d ?sx ?sy ?p 0 ?y ?z)))
(defrw -enlarge-convolution-kernel (conv2d ?sx ?sy actnone ?c ?x (enlarge ?kx ?ky ?y)) (conv2d ?sx ?sy actnone ?c ?x ?y))
(defrw2 operator-commutativity-4 (conv2d ?sx ?sy ?p actrelu ?x ?y) (relu (conv2d ?sx ?sy ?p actnone ?x ?y)))
(defrw2 conv-with-2-applies-actrelu (relu (transpose ?x)) (transpose (relu ?x)))
(defrw -pooling-by-conv.-with-cpool (poolavg ?kx ?ky ?sx ?sy ?p ?x) (conv2d ?sx ?sy ?p actnone ?x (cpool ?kx ?ky)))
(defrw2 const-iconv-and-const-pool (poolavg ?kx ?ky 1 1 psame (iconv ?kx ?ky)) (cpool ?kx ?ky))
(defrw identity-kernel (conv2d 1 1 psame actnone ?x (iconv ?kx ?ky)) ?x)
(defrw2 identity-matrix (matmul ?x imatmul) ?x)
(defrw2 ewmul-identity (ewmul ?x iewmul) ?x)
(defrw split-definition-0 (split-0 ?a ?n (concat ?a ?x ?y)) ?x)
(defrw split-definition-1 (split-1 ?a ?n (concat ?a ?x ?y)) ?y)
(defrw2 geometry-of-concatenation (concat 0 (concat 1 ?x ?y) (concat 1 ?z ?w)) (concat 1 (concat 0 ?x ?z) (concat 0 ?y ?w)))
(defrw2 operator-commutativity-5 (concat ?a (smul ?x ?w) (smul ?y ?w)) (smul (concat ?a ?x ?y) ?w))
(defrw2 operator-commutativity-6 (concat ?a (ewadd ?x ?y) (ewadd ?z ?w)) (ewadd (concat ?a ?x ?z) (concat ?a ?y ?w)))
(defrw2 operator-commutativity-7 (concat ?a (ewmul ?x ?y) (ewmul ?z ?w)) (ewmul (concat ?a ?x ?z) (concat ?a ?y ?w)))
(defrw2 operator-commutativity-8 (concat ?a (relu ?x) (relu ?y)) (relu (concat ?a ?x ?y)))
(defrw2 concatenation-and-transpose (concat 1 (transpose ?x) (transpose ?y)) (transpose (concat 0 ?x ?y)))
(defrw2 concatenation-and-matrix-mul.-0 (concat 1 (matmul ?x ?y) (matmul ?x ?z)) (matmul ?x (concat 1 ?y ?z)))
(defrw2 concatenation-and-matrix-mul.-1 (matmul (concat 1 ?x ?z) (concat 0 ?y ?w)) (ewadd (matmul ?x ?y) (matmul ?z ?w)))
(defrw2 concatenation-and-conv.-0 (concat 0 (conv2d ?sx ?sy ?p ?c ?x ?z) (conv2d ?sx ?sy ?p ?c ?y ?z)) (conv2d ?sx ?sy ?p ?c (concat 0 ?x ?y) ?z))
(defrw2 concatenation-and-conv.-1 (concat 1 (conv2d ?sx ?sy ?p ?c ?x ?y) (conv2d ?sx ?sy ?p ?c ?x ?z)) (conv2d ?sx ?sy ?p ?c ?x (concat 0 ?y ?z)))
(defrw2 concatenation-and-conv.-2 (conv2d ?sx ?sy ?p actnone (concat 1 ?x ?z) (concat 1 ?y ?w)) (ewadd (conv2d ?sx ?sy ?p actnone ?x ?y) (conv2d ?sx ?sy ?p actnone ?z ?w)))
(defrw2 concatenation-and-pooling-0 (concat 1 (poolavg ?kx ?ky ?sx ?sy ?p ?x) (poolavg ?kx ?ky ?sx ?sy ?p ?y)) (poolavg ?kx ?ky ?sx ?sy ?p (concat 1 ?x ?y)))
(defrw2 concatenation-and-pooling-1 (concat 0 (poolmax ?kx ?ky ?sx ?sy ?p ?x) (poolmax ?kx ?ky ?sx ?sy ?p ?y)) (poolmax ?kx ?ky ?sx ?sy ?p (concat 0 ?x ?y)))
(defrw2 concatenation-and-pooling-2 (concat 1 (poolmax ?kx ?ky ?sx ?sy ?p ?x) (poolmax ?kx ?ky ?sx ?sy ?p ?y)) (poolmax ?kx ?ky ?sx ?sy ?p (concat 1 ?x ?y)))

(defun gen-split ()
  (do-matches (x ?x)
    (do-matches (y ?y)
      (let ((xs (shape x))
            (ys (shape y)))
        (flet ((gen-concat (axis)
                 (let* ((concat (make-enode (list 'concat (make-enode (list axis)) x y)))
                        (n (make-enode (list (nth axis xs))))
                        (axis (make-enode (list axis))))
                   (enode-merge x (make-enode (list 'split-0 axis n concat)))
                   (enode-merge y (make-enode (list 'split-1 axis n concat))))))
          (when (= (length xs) (length ys))
            (let* ((matchps (mapcar #'= xs ys))
                   (n-unmatch (count nil matchps)))
              (case n-unmatch
                (0 (map-iota #'gen-concat (length xs)))
                (1 (gen-concat (position nil matchps)))))))))))

(defvar *tensat-rules*
  '(EWADD-IS-ASSOCIATIVE -EWADD-IS-ASSOCIATIVE EWADD-IS-COMMUNITATIVE -EWADD-IS-COMMUNITATIVE EWMUL-IS-ASSOCIATIVE
    -EWMUL-IS-ASSOCIATIVE EWMUL-IS-COMMUNITATIVE -EWMUL-IS-COMMUNITATIVE DISTRIBUTIVE-0
    -DISTRIBUTIVE-0 SMUL-IS-ASSOCIATIVE -SMUL-IS-ASSOCIATIVE DISTRIBUTIVE-1 -DISTRIBUTIVE-1
    OPERATOR-COMMUTATIVITY-0 -OPERATOR-COMMUTATIVITY-0 #+nil TRANSPOSE-IS-ITS-OWN-INVERSE
    #+nil -TRANSPOSE-IS-ITS-OWN-INVERSE OPERATOR-COMMUTATIVITY-1 -OPERATOR-COMMUTATIVITY-1
    OPERATOR-COMMUTATIVITY-2 -OPERATOR-COMMUTATIVITY-2 OPERATOR-COMMUTATIVITY-3
    -OPERATOR-COMMUTATIVITY-3 MATMUL-IS-ASSOCIATIVE -MATMUL-IS-ASSOCIATIVE MATMUL-IS-LINEAR-0
    -MATMUL-IS-LINEAR-0 MATMUL-IS-LINEAR-1 -MATMUL-IS-LINEAR-1 MATMUL-AND-TRANSPOSE
    -MATMUL-AND-TRANSPOSE CONV-IS-BILINEAR-0 -CONV-IS-BILINEAR-0 CONV-IS-BILINEAR-1
    -CONV-IS-BILINEAR-1 CONV-IS-BILINEAR-2 -CONV-IS-BILINEAR-2 CONV-IS-BILINEAR-3
    -CONV-IS-BILINEAR-3 -ENLARGE-CONVOLUTION-KERNEL OPERATOR-COMMUTATIVITY-4
    -OPERATOR-COMMUTATIVITY-4 CONV-WITH-2-APPLIES-ACTRELU -CONV-WITH-2-APPLIES-ACTRELU
    -POOLING-BY-CONV.-WITH-CPOOL CONST-ICONV-AND-CONST-POOL -CONST-ICONV-AND-CONST-POOL
    IDENTITY-KERNEL IDENTITY-MATRIX -IDENTITY-MATRIX #+nil EWMUL-IDENTITY #+nil -EWMUL-IDENTITY
    SPLIT-DEFINITION-0 SPLIT-DEFINITION-1 GEOMETRY-OF-CONCATENATION -GEOMETRY-OF-CONCATENATION
    OPERATOR-COMMUTATIVITY-5 -OPERATOR-COMMUTATIVITY-5 OPERATOR-COMMUTATIVITY-6
    -OPERATOR-COMMUTATIVITY-6 OPERATOR-COMMUTATIVITY-7 -OPERATOR-COMMUTATIVITY-7
    OPERATOR-COMMUTATIVITY-8 -OPERATOR-COMMUTATIVITY-8 CONCATENATION-AND-TRANSPOSE
    -CONCATENATION-AND-TRANSPOSE CONCATENATION-AND-MATRIX-MUL.-0 -CONCATENATION-AND-MATRIX-MUL.-0
    CONCATENATION-AND-MATRIX-MUL.-1 -CONCATENATION-AND-MATRIX-MUL.-1 CONCATENATION-AND-CONV.-0
    -CONCATENATION-AND-CONV.-0 CONCATENATION-AND-CONV.-1 -CONCATENATION-AND-CONV.-1
    CONCATENATION-AND-CONV.-2 -CONCATENATION-AND-CONV.-2 CONCATENATION-AND-POOLING-0
    -CONCATENATION-AND-POOLING-0 CONCATENATION-AND-POOLING-1 -CONCATENATION-AND-POOLING-1
    CONCATENATION-AND-POOLING-2 -CONCATENATION-AND-POOLING-2))

(defun compute-pad-dims (pmode input-h input-w stride-h stride-w kernel-h kernel-w)
  (ecase pmode
    (psame (list (floor (1- (+ input-h stride-h)) stride-h)
                 (floor (1- (+ input-w stride-w)) stride-w)))
    (pvalid (list (1+ (floor (- input-h kernel-h) stride-h))
                  (1+ (floor (- input-w kernel-w) stride-w))))))

(define-analysis shape
  :make (lambda (enode)
          (trivia:match (enode-term enode)
            ((list* (or 'input 'weight) args) (mapcar #'enode-value args))
            ((list 'conv2d stride-h stride-w pmode _ input kernel)
             (bind (((input-0 _ input-h input-w) (shape input))
                    ((kernel-0 _ kernel-h kernel-w) (shape kernel)))
               (list* input-0 kernel-0
                      (compute-pad-dims (enode-value pmode) input-h input-w
                                        (enode-value stride-h) (enode-value stride-w)
                                        kernel-h kernel-w))))
            ((list 'poolmax kernel-h kernel-w stride-h stride-w pmode input)
             (bind (((input-0 input-1 input-h input-w) (shape input)))
               (list* input-0 input-1
                      (compute-pad-dims (enode-value pmode) input-h input-w
                                        (enode-value stride-h) (enode-value stride-w)
                                        (enode-value kernel-h) (enode-value kernel-w)))))
            ((list 'concat axis x y)
             (let ((axis (enode-value axis))
                   (xdims (shape x)) (ydims (shape y)))
               (assert (= (length xdims) (length ydims)))
               (loop for i from 0
                     for xdim in xdims
                     for ydim in ydims
                     collect (if (= i axis) (+ xdim ydim)
                                 (progn (assert (= xdim ydim)) xdim)))))
            ((list* (or 'relu 'ewadd 'ewmul) args)
             (lret ((shape (shape (car args))))
               (dolist (arg (cdr args))
                 (assert (equal shape (shape arg))))))))
  :merge (lambda (x y)
           (if (and (not x) y)
               (values y t)
               (progn
                 (when (and x y) (assert (equal x y)))
                 (values x nil)))))

(define-analysis cost
  :make (lambda (enode)
          (trivia:match (enode-term enode)
            ((list* 'ewadd _) (reduce #'* (shape enode)))
            ;; FIXME: Somehow this does not affect resnext-50...
            ((list* 'conv2d children)
             (let* ((output (shape enode))
                    (kernel (shape (lastcar children))))
               (reduce #'* (append output (nthcdr 2 kernel)))))
            ((list* args) (reduce #'+ (cdr args) :key #'cost))))
  :merge  (lambda (x y) (if (< y x) (values y t) (values x nil)))
  :depends-on 'shape)

(defun resnext-block (input stride-h stride-w out-channels input-dim groups)
  (let* ((w1 (make-term (list 'weight out-channels input-dim 1 1)))
         (tmp (make-term (list 'conv2d 1 1 'psame 'actrelu input w1)))
         (w2 (make-term (list 'weight out-channels (floor out-channels groups) 3 3)))
         (tmp (make-term (list 'conv2d stride-h stride-w 'psame 'actnone tmp w2)))
         (w3 (make-term (list 'weight (* out-channels 2) out-channels 1 1)))
         (tmp (make-term (list 'conv2d 1 1 'psame 'actnone tmp w3))))
    (when (or (> stride-h 1) (not (= input-dim (* out-channels 2))))
      (let ((w4 (make-term (list 'weight (* out-channels 2) input-dim 1 1))))
        (setq input (make-term (list 'conv2d stride-h stride-w 'psame 'actnone input w4)))))
    (make-term (list 'relu (list 'ewadd input tmp)))))

(defun resnext-50 ()
  (let* ((input (make-term (list 'input 1 3 224 224)))
         (weight (make-term (list 'weight 64 3 7 7)))
         (tmp (make-term (list 'conv2d 2 2 'psame 'actrelu input weight)))
         (tmp (make-term (list 'poolmax 3 3 2 2 'psame tmp)))
         (groups 32)
         (input-dim 64))
    (flet ((cfg-blocks (n stride-h stride-w out-channels)
             (setq tmp (resnext-block tmp stride-h stride-w out-channels input-dim groups)
                   input-dim (* out-channels 2))
             (loop repeat (1- n) do
               (setq tmp (resnext-block tmp 1 1 out-channels input-dim groups)))))
      (cfg-blocks 3 1 1 128)
      (cfg-blocks 4 2 2 256)
      (cfg-blocks 6 2 2 512)
      (cfg-blocks 3 2 2 1024))
    tmp))

#+nil (let* ((*egraph* (make-egraph :analyses 'cost))
       (a (resnext-50)))
  (egraph-rebuild)
  (cost a))

#+nil (let* ((*egraph* (make-egraph :analyses 'cost))
      (input (make-term (list 'input 1 3 224 224)))
      (weight (make-term (list 'weight 64 3 7 7)))
      (add (make-term (list 'ewadd input input)))
      (conv (make-term (list 'conv2d 1 1 'psame 'actrelu input weight))))
  (egraph-rebuild)
  (cost conv))
