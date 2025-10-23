(uiop:define-package :egraph/examples/tensat
    (:use #:cl #:egraph))

(in-package :egraph/examples/tensat)

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
(defrw2 conv-is-bilinear-1 (smul (conv2d ?sx ?sy ?p 0 ?x ?y) ?w) (conv2d ?sx ?sy ?p 0 (smul ?x ?w) ?y))
(defrw2 conv-is-bilinear-2 (conv2d ?sx ?sy ?p 0 ?x (ewadd ?y ?z)) (ewadd (conv2d ?sx ?sy ?p 0 ?x ?y) (conv2d ?sx ?sy ?p 0 ?x ?z)))
(defrw2 conv-is-bilinear-3 (conv2d ?sx ?sy ?p 0 (ewadd ?x ?y) ?z) (ewadd (conv2d ?sx ?sy ?p 0 ?x ?z) (conv2d ?sx ?sy ?p 0 ?y ?z)))
(defrw -enlarge-convolution-kernel (conv2d ?sx ?sy 0 ?c ?x (enlarge ?kx ?ky ?y)) (conv2d ?sx ?sy 0 ?c ?x ?y))
(defrw2 operator-commutativity-4 (conv2d ?sx ?sy ?p 2 ?x ?y) (relu (conv2d ?sx ?sy ?p 0 ?x ?y)))
(defrw2 conv-with-2-applies-relu (relu (transpose ?x)) (transpose (relu ?x)))
(defrw -pooling-by-conv.-with-cpool (poolavg ?kx ?ky ?sx ?sy ?p ?x) (conv2d ?sx ?sy ?p 0 ?x (cpool ?kx ?ky)))
(defrw2 const-iconv-and-const-pool (poolavg ?kx ?ky 1 1 0 (iconv ?kx ?ky)) (cpool ?kx ?ky))
(defrw identity-kernel (conv2d 1 1 0 0 ?x (iconv ?kx ?ky)) ?x)
(defrw2 identity-matrix (matmul ?x imatmul) ?x)
(defrw2 ewmul-identity (ewmul ?x iewmul) ?x)
(defrw split-definition-0 (split_0 ?a (concat ?a ?x ?y)) ?x)
(defrw split-definition-1 (split_1 ?a (concat ?a ?x ?y)) ?y)
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
(defrw2 concatenation-and-conv.-2 (conv2d ?sx ?sy ?p 0 (concat 1 ?x ?z) (concat 1 ?y ?w)) (ewadd (conv2d ?sx ?sy ?p 0 ?x ?y) (conv2d ?sx ?sy ?p 0 ?z ?w)))
(defrw2 concatenation-and-pooling-0 (concat 1 (poolavg ?kx ?ky ?sx ?sy ?p ?x) (poolavg ?kx ?ky ?sx ?sy ?p ?y)) (poolavg ?kx ?ky ?sx ?sy ?p (concat 1 ?x ?y)))
(defrw2 concatenation-and-pooling-1 (concat 0 (poolmax ?kx ?ky ?sx ?sy ?p ?x) (poolmax ?kx ?ky ?sx ?sy ?p ?y)) (poolmax ?kx ?ky ?sx ?sy ?p (concat 0 ?x ?y)))
(defrw2 concatenation-and-pooling-2 (concat 1 (poolmax ?kx ?ky ?sx ?sy ?p ?x) (poolmax ?kx ?ky ?sx ?sy ?p ?y)) (poolmax ?kx ?ky ?sx ?sy ?p (concat 1 ?x ?y)))

(defvar *tensat-rules*
  '(EWADD-IS-ASSOCIATIVE -EWADD-IS-ASSOCIATIVE EWADD-IS-COMMUNITATIVE -EWADD-IS-COMMUNITATIVE EWMUL-IS-ASSOCIATIVE
    -EWMUL-IS-ASSOCIATIVE EWMUL-IS-COMMUNITATIVE -EWMUL-IS-COMMUNITATIVE DISTRIBUTIVE-0
    -DISTRIBUTIVE-0 SMUL-IS-ASSOCIATIVE -SMUL-IS-ASSOCIATIVE DISTRIBUTIVE-1 -DISTRIBUTIVE-1
    OPERATOR-COMMUTATIVITY-0 -OPERATOR-COMMUTATIVITY-0 TRANSPOSE-IS-ITS-OWN-INVERSE
    -TRANSPOSE-IS-ITS-OWN-INVERSE OPERATOR-COMMUTATIVITY-1 -OPERATOR-COMMUTATIVITY-1
    OPERATOR-COMMUTATIVITY-2 -OPERATOR-COMMUTATIVITY-2 OPERATOR-COMMUTATIVITY-3
    -OPERATOR-COMMUTATIVITY-3 MATMUL-IS-ASSOCIATIVE -MATMUL-IS-ASSOCIATIVE MATMUL-IS-LINEAR-0
    -MATMUL-IS-LINEAR-0 MATMUL-IS-LINEAR-1 -MATMUL-IS-LINEAR-1 MATMUL-AND-TRANSPOSE
    -MATMUL-AND-TRANSPOSE CONV-IS-BILINEAR-0 -CONV-IS-BILINEAR-0 CONV-IS-BILINEAR-1
    -CONV-IS-BILINEAR-1 CONV-IS-BILINEAR-2 -CONV-IS-BILINEAR-2 CONV-IS-BILINEAR-3
    -CONV-IS-BILINEAR-3 -ENLARGE-CONVOLUTION-KERNEL OPERATOR-COMMUTATIVITY-4
    -OPERATOR-COMMUTATIVITY-4 CONV-WITH-2-APPLIES-RELU -CONV-WITH-2-APPLIES-RELU
    -POOLING-BY-CONV.-WITH-CPOOL CONST-ICONV-AND-CONST-POOL -CONST-ICONV-AND-CONST-POOL
    IDENTITY-KERNEL IDENTITY-MATRIX -IDENTITY-MATRIX EWMUL-IDENTITY -EWMUL-IDENTITY
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
