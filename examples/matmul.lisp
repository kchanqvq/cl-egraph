(uiop:define-package :egraph/examples/matmul
    (:use #:cl #:egraph)
  (:import-from #:fiveam #:def-suite* #:def-test #:is #:in-suite))

(in-package :egraph/examples/matmul)

(def-suite* matmul :in :egraph)

(defrw assoc-matmul (matmul ?x (matmul ?y ?z)) (matmul (matmul ?x ?y) ?z))
(defrw -assoc-matmul (matmul (matmul ?x ?y) ?z) (matmul ?x (matmul ?y ?z)))

(defstruct (matrix-var (:constructor matrix-var (&rest shape))) (shape))

(define-analysis shape
  :make (lambda (fsym &rest args)
          (if args
              (list (car (get-analysis-data (car args) 'shape))
                    (cadr (get-analysis-data (cadr args) 'shape)))
              (matrix-var-shape fsym)))
  :merge (lambda (x y)
           (if (and (not x) y)
               (values y t)
               (values x nil))))

(define-analysis cost
  :make (lambda (fsym &rest args)
          (if (eq fsym 'matmul)
              (+ (get-analysis-data (car args) 'cost)
                 (get-analysis-data (cadr args) 'cost)
                 (let ((mn (get-analysis-data (car args) 'shape))
                       (nk (get-analysis-data (cadr args) 'shape)))
                   (* (car mn) (cadr mn) (cadr nk))))
              0))
  :merge (lambda (x y) (if (< y x) (values y t) (values x nil))))

(defun make-matmul-term (dims)
  (labels ((process (dims)
             (if (cdddr dims)
                 `(matmul (,(matrix-var (car dims) (cadr dims)))
                          ,(process (cdr dims)))
                 `(matmul (,(matrix-var (car dims) (cadr dims)))
                          (,(matrix-var (cadr dims) (caddr dims)))))))
    (process dims)))

(def-test matmul.1 ()
  (let* ((*egraph* (make-egraph :analyses '(shape cost)))
         (a (make-term
             (make-matmul-term '(10 14 12 9 13 16 1 6 13 14 7 7 3 14 15 15 19 12 19 8)))))
    (egraph-rebuild)
    (is (eq :saturate (run-rewrites 'assoc-matmul)))
    (is (= 2619 (get-analysis-data a 'cost)))))
