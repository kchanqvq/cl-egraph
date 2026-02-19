(uiop:define-package :egraph/examples/matmul
    (:use #:cl #:egraph #:alexandria)
  (:import-from #:fiveam #:def-suite* #:def-test #:is #:in-suite))

(in-package :egraph/examples/matmul)

(def-suite* matmul :in :egraph)

(defrw assoc-matmul (matmul ?x (matmul ?y ?z)) (matmul (matmul ?x ?y) ?z))
(defrw -assoc-matmul (matmul (matmul ?x ?y) ?z) (matmul ?x (matmul ?y ?z)))

(define-analysis shape
  :make (lambda (enode)
          (trivia:match (enode-term enode)
            ((list 'matmul x y) (list (car (shape x)) (cadr (shape y))))
            ((list* 'mat args) (mapcar (compose #'car #'enode-term) args))))
  :merge (make-orp #'equal))

(define-analysis cost
  :make (lambda (enode)
          (trivia:match (enode-term enode)
            ((list 'matmul x y)
             (+ (cost x) (cost y)
                (let ((mn (shape x)) (nk (shape y)))
                  (* (car mn) (cadr mn) (cadr nk)))))
            ((list* 'mat _) 0)))
  :merge #'min)

(defun make-matmul-term (dims)
  (labels ((process (dims)
             (if (cdddr dims)
                 `(matmul (mat ,(car dims) ,(cadr dims))
                          ,(process (cdr dims)))
                 `(matmul (mat ,(car dims) ,(cadr dims))
                          (mat ,(cadr dims) ,(caddr dims))))))
    (process dims)))

(def-test matmul.1 ()
  (let* ((*egraph* (make-egraph :analyses '(cost shape)))
         (a (make-term
             (make-matmul-term '(10 14 12 9 13 16 1 6 13 14 7 7 3 14 15 15 19 12 19 8)))))
    (egraph-rebuild)
    (is (eq :saturate (run-rewrites 'assoc-matmul)))
    (is (= 2619 (cost a)))))
