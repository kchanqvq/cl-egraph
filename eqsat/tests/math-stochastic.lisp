(in-package :ggs/eqsat/tests/math)

(defun ast-size-no-d-or-i-cost (term)
  (if (consp term)
      (reduce #'+ (cdr term) :key #'ast-size-no-d-or-i-cost
                             :initial-value (if (member (car term) '(d i)) 4.0 1.0))
      1))

(defun const-normalizer (fsym &rest args)
  (if (and args (every #'numberp args))
      ;; Guard against things like division by zero
      (or
       (ignore-errors
        (let* ((result (apply fsym args)))
          ;; Coerce integral float into integer
          (if (floatp result)
              (multiple-value-bind (int frac) (truncate result (float 1.0 result))
                (if (zerop frac) int result))
              result)))
       (cons fsym args))
      (cons fsym args)))
