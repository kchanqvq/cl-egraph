(uiop:define-package :egraph/examples/matmul
    (:use #:cl #:egraph #:alexandria)
  (:import-from #:fiveam #:def-suite* #:def-test #:is #:in-suite))

(in-package :egraph/examples/matmul)

(def-suite* matmul :in :egraph)

(defrw assoc-matmul (matmul ?x (matmul ?y ?z)) (matmul (matmul ?x ?y) ?z))
(defrw -assoc-matmul (matmul (matmul ?x ?y) ?z) (matmul ?x (matmul ?y ?z)))

(define-analysis shape
  :make (lambda (fsym &rest args)
          (case fsym
            (matmul (list (car (shape (car args)))
                          (cadr (shape (cadr args)))))
            (mat (mapcar (compose #'car #'enode-term) args))))
  :merge (lambda (x y)
           (if (and (not x) y)
               (values y t)
               (values x nil))))

(define-analysis cost
  :make (lambda (fsym &rest args)
          (case fsym
            (matmul (+ (cost (car args))
                       (cost (cadr args))
                       (let ((mn (shape (car args)))
                             (nk (shape (cadr args))))
                         (* (car mn) (cadr mn) (cadr nk)))))
            (mat 0)))
  :merge (lambda (x y) (if (< y x) (values y t) (values x nil))))

(defun make-matmul-term (dims)
  (labels ((process (dims)
             (if (cdddr dims)
                 `(matmul (mat ,(car dims) ,(cadr dims))
                          ,(process (cdr dims)))
                 `(matmul (mat ,(car dims) ,(cadr dims))
                          (mat ,(cadr dims) ,(caddr dims))))))
    (process dims)))

(def-test matmul.1 ()
  (let* ((*egraph* (make-egraph :analyses '(shape cost)))
         (a (make-term
             (make-matmul-term '(10 14 12 9 13 16 1 6 13 14 7 7 3 14 15 15 19 12 19 8)))))
    (egraph-rebuild)
    (is (eq :saturate (run-rewrites 'assoc-matmul)))
    (is (= 2619 (cost a)))))
