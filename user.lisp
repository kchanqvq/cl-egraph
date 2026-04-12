(in-package :egraph)

(defun make-term (term)
  (typecase term
    (cons (apply #'make-enode (car term) (mapcar #'make-term (cdr term))))
    (enode term)
    (t (make-enode term))))

(declaim (inline orp make-orp))

(defun orp (x y) (or x y))

(defun make-orp (test)
  (lambda (x y)
    (if x (if y (if (funcall test x y) x (error "Conflicting values: ~a vs ~a" x y)) x) y)))
