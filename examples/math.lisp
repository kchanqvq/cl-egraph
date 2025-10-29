(uiop:define-package :egraph/examples/math
    (:use #:cl #:egraph)
  (:import-from #:fiveam #:def-suite* #:def-test #:is #:in-suite))

(in-package :egraph/examples/math)

(def-suite* math :in :egraph)

(define-analysis const
  :make (lambda (fsym &rest args)
          (if args
              (block nil
                (let ((args (mapcar (lambda (enode)
                                      (or (const enode)
                                          (return)))
                                    args)))
                  ;; Guard against things like division by zero
                  (ignore-errors
                   (let* ((result (apply fsym args)))
                     ;; Coerce integral float into integer
                     (if (floatp result)
                         (multiple-value-bind (int frac) (truncate result (float 1.0 result))
                           (if (zerop frac) int result))
                         result)))))
              (when (numberp fsym)
                fsym)))
  :merge (lambda (x y)
           (if (and (not x) y)
               (values y t)
               (values x nil)))
  :modify (lambda (node data)
            (when data
              (let* ((term (list data))
                     (const (make-enode term)))
                (declare (dynamic-extent term))
                (enode-merge node const)
                (setf (egraph::eclass-info-nodes (enode-eclass-info node))
                      (list const))))))

(define-analysis var
  :make (lambda (fsym &rest args)
          (when (and (not args) (symbolp fsym))
            fsym))
  :merge (lambda (x y)
           (if (and (not x) y)
               (values y t)
               (values x nil))))

(defrw commute-add (+ ?a ?b) (+ ?b ?a))
(defrw commute-mul (* ?a ?b) (* ?b ?a))
(defrw assoc-add (+ ?a (+ ?b ?c)) (+ (+ ?a ?b) ?c))
(defrw assoc-mul (* ?a (* ?b ?c)) (* (* ?a ?b) ?c))

(defrw sub-canon (- ?a ?b) (+ ?a (* -1 ?b)))
(defrw div-canon (/ ?a ?b) (* ?a (pow ?b -1)) :guard (not (eql 0 (const ?b))))

(defrw add-0 (+ ?a 0) ?a)
(defrw mul-0 (* ?a 0) 0)
(defrw mul-1 (* ?a 1) ?a)

(defrw -add-0 ?a (+ ?a 0))
(defrw -mul-1 ?a (* ?a 1))

(defrw sub-cancel (- ?a ?a) 0)
(defrw div-cancel (/ ?a ?a) 1 :guard (not (eql 0 (const ?a))))

(defrw distribute (* ?a (+ ?b ?c)) (+ (* ?a ?b) (* ?a ?c)))
(defrw factor (+ (* ?a ?b) (* ?a ?c)) (* ?a (+ ?b ?c)))

(defrw pow-mul (* (pow ?a ?b) (pow ?a ?c)) (pow ?a (+ ?b ?c)))
(defrw pow-0 (pow ?a 0) 1 :guard (not (eql 0 (const ?a))))
(defrw pow-1 (pow ?a 1) ?a)
(defrw pow-2 (pow ?a 2) (* ?a ?a))
(defrw pow-recip (pow ?a -1) (/ 1 ?a) :guard (not (eql 0 (const ?a))))
(defrw recip-mul-div (* ?x (/ 1 ?x)) 1 :guard (not (eql 0 (const ?x))))

(declaim (inline pow))
(defun pow (x y) (expt x y))

(defvar *math-base-rules*
  '(COMMUTE-ADD COMMUTE-MUL ASSOC-ADD ASSOC-MUL SUB-CANON DIV-CANON ADD-0 MUL-0 MUL-1 -ADD-0 -MUL-1 SUB-CANCEL
    DIV-CANCEL DISTRIBUTE FACTOR POW-MUL POW-0 POW-1 POW-2 POW-RECIP RECIP-MUL-DIV))

(defrw d-var (d ?x ?x) 1 :guard (var ?x))
(defrw d-const (d ?x ?c) 0 :guard (or (const ?c)
                                      (alexandria:when-let* ((vx (var ?x))
                                                             (vc (var ?c)))
                                        (not (eq vx vc)))))
(defrw d-add (d ?x (+ ?a ?b)) (+ (d ?x ?a) (d ?x ?b)))
(defrw d-mul (d ?x (* ?a ?b)) (+ (* ?a (d ?x ?b)) (* ?b (d ?x ?a))))
(defrw d-sin (d ?x (sin ?x)) (cos ?x))
(defrw d-cos (d ?x (cos ?x)) (* -1 (sin ?x)))
(defrw d-ln (d ?x (ln ?x)) (/ 1 ?x) :guard (not (eql 0 (const ?x))))
(defrw d-pow (d ?x (pow ?f ?g)) (* (pow ?f ?g) (+ (* (d ?x ?f) (/ ?g ?f)) (* (d ?x ?g) (ln ?f))))
  :guard (and (not (eql 0 (const ?f)))
              (not (eql 0 (const ?g)))))

(defvar *math-diff-rules* '(D-VAR D-CONST D-ADD D-MUL D-SIN D-COS D-LN D-POW))

(defrw i-one (i 1 ?x) ?x)
(defrw i-pow-const (i (pow ?x ?c) ?x)
  (/ (pow ?x (+ ?c 1)) (+ ?c 1)) :guard (const ?c))
(defrw i-cos (i (cos ?x) ?x) (sin ?x))
(defrw i-sin (i (sin ?x) ?x) (* -1 (cos ?x)))
(defrw i-sum (i (+ ?f ?g) ?x) (+ (i ?f ?x) (i ?g ?x)))
(defrw i-dif (i (- ?f ?g) ?x) (- (i ?f ?x) (i ?g ?x)))
(defrw i-parts (i (* ?a ?b) ?x) (- (* ?a (i ?b ?x)) (i (* (d ?x ?a) (i ?b ?x)) ?x)))

(defvar *math-integral-rules* '(I-ONE I-POW-CONST I-COS I-SIN I-SUM I-DIF I-PARTS))

(defvar *math-rules* (append *math-base-rules* *math-diff-rules* *math-integral-rules*))

(defun ast-size-no-d-or-i (fsym arg-costs)
  (when (every #'identity arg-costs)
    (+ (if (member fsym '(d i)) 100 1) (reduce #'+ arg-costs))))

(defmacro def-math-test (name () lhs rhs)
  `(def-test ,name ()
     (let* ((*egraph* (make-egraph :analyses '(var const)))
            (lhs (make-term ',lhs)))
       (egraph-rebuild)
       (run-rewrites *math-rules* :max-enodes 5000)
       (is (eq (enode-find (make-term ',rhs)) (enode-find lhs))))))

(def-math-test math.simplify-root ()
  (/ 1 (- (/ (+ 1 (sqrt five)) 2)
          (/ (- 1 (sqrt five)) 2)))
  (/ 1 (sqrt five)))

(def-math-test math.simplify-factor ()
  (* (+ x 3) (+ x 1)) (+ (+ (* x x) (* 4 x)) 3))

(def-math-test math.diff-power-simple ()
  (d x (pow x 3)) (* 3 (pow x 2)))

(def-test math.diff-power-harder ()
  (let* ((*egraph* (make-egraph :analyses '(var const)))
         (a (make-term '(d x (- (pow x 3) (* 7 (pow x 2))))))
         (b (make-term '(* x (- (* 3 x) 14)))))
    (egraph-rebuild)
    (run-rewrites *math-rules* :max-enodes 5000)
    (is (eq (enode-find b) (enode-find a)))))

(def-test bench.math.diff ()
  (let ((timer (benchmark:make-timer)))
    (loop for i from 1 to 3 do
      (let ((*egraph* (make-egraph :analyses '(var const))))
        (format t "~&Benchmark run ~a." i)
        (trivial-garbage:gc :full t)
        (make-term '(d x (- (pow x 3) (* 7 (pow x 2)))))
        (egraph-rebuild)
        (benchmark:with-sampling (timer)
          (run-rewrites *math-rules* :max-enodes 50000))))
    (benchmark:report timer)))

(def-math-test math.integral-part.1 ()
  (i (* x (cos x)) x) (+ (* x (sin x)) (cos x)))

(def-math-test math.integral-part.2 ()
  (i (* (cos x) x) x) (+ (* x (sin x)) (cos x)))

(def-math-test math.integral-part.3 ()
  (i (ln x) x) (- (* x (ln x)) x))

(def-test math.lp-extract ()
  (let* ((*egraph* (make-egraph :analyses '(var const)))
         (a (make-term '(pow (+ x (+ x x)) (+ x x)))))
    (egraph-rebuild)
    (run-rewrites *math-rules* :max-enodes 5000)
    (is (equal '(pow (* 3 x) (+ x x))
               (greedy-extract a #'ast-size-no-d-or-i)))
    (is (equal '(pow (+ x (+ x x)) (+ x x))
               (lp-extract a (constantly 1))))))
