(in-package :egraph)

(defun make-term (term)
  (typecase term
    (cons (make-enode (cons (car term) (mapcar #'make-term (cdr term)))))
    (enode term)
    (t (make-enode (list term)))))

(declaim (inline orp make-orp))

(defun orp (x y) (or x y))

(defun make-orp (test)
  (lambda (x y)
    (if x (if y (if (funcall test x y) x (error "Conflicting values: ~a vs ~a" x y)) x) y)))
