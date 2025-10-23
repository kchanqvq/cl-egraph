(uiop:define-package :egraph/tests
    (:use #:cl #:egraph)
  (:import-from #:fiveam #:def-suite* #:def-test #:is #:in-suite))

(in-package :egraph/tests)

(def-suite* :egraph)

;;; E-graph data structure and rebuild

(def-test union-find ()
  (let* ((*egraph* (make-egraph))
         (x (make-enode (list 1)))
         (y (make-enode (list 2))))
    (is (eq (enode-find x) x))
    (is (eq (enode-find y) y))
    (enode-merge x y)
    (is (eq (enode-find x) (enode-find y)))

    (let ((results (list-enodes x)))
      (is (= 2 (length results)))
      (is (member x results))
      (is (member y results)))

    (let ((results (list-enodes y)))
      (is (= 2 (length results)))
      (is (member x results))
      (is (member y results)))

    (egraph-rebuild)
    (is (eq (enode-find x) (enode-find y)))
    (check-egraph)))

(def-test rebuild ()
  (let* ((*egraph* (make-egraph))
         (a (make-enode (list 'a)))
         (b (make-enode (list 'b)))
         (f-a (make-enode (list 'f a)))
         (f-b (make-enode (list 'f b)))
         (g-a (make-enode (list 'g a)))
         (g-b (make-enode (list 'g b)))
         (f-f-a (make-enode (list 'f f-a)))
         (f-f-b (make-enode (list 'f f-b))))
    (enode-merge a b)
    (is (eq (enode-find a) (enode-find b)))
    (is (every #'enode-canonical-p (list-enodes a)))
    (egraph-rebuild)
    (is (eq (enode-find f-a) (enode-find f-b)))
    (is (eq (enode-find f-f-a) (enode-find f-f-b)))
    (is (eq (enode-find g-a) (enode-find g-b)))
    (is (not (eq (enode-find a) (enode-find f-a))))
    (is (not (eq (enode-find f-a) (enode-find g-a))))
    (is (not (eq (enode-find f-a) (enode-find f-f-a))))
    (is (every #'enode-canonical-p (list-enodes a)))
    (is (every #'enode-canonical-p (list-enodes f-a)))
    (is (every #'enode-canonical-p (list-enodes g-a)))
    (is (every #'enode-canonical-p (list-enodes f-f-a)))
    (check-egraph)
    (enode-merge f-a g-a)
    (egraph-rebuild)
    (is (eq (enode-find f-a) (enode-find g-a)))
    (is (eq (enode-find f-b) (enode-find g-b)))
    (check-egraph)))

(def-test rebuild-cyclic ()
  (let* ((*egraph* (make-egraph))
         (a (make-enode (list 'a)))
         (f-a (make-enode (list 'f a)))
         (f-f-a (make-enode (list 'f f-a)))
         (f-f-f-a (make-enode (list 'f f-f-a))))
    (enode-merge a f-a)
    (egraph-rebuild)
    (is (eq (enode-find a) (enode-find f-a)))
    (is (eq (enode-find a) (enode-find f-f-a)))
    (is (eq (enode-find a) (enode-find f-f-f-a)))
    (check-egraph)))

;;; Pattern compiler

(defrw commute-add (+ ?a ?b) (+ ?b ?a))
(defrw commute-mul (* ?a ?b) (* ?b ?a))
(defrw add-0 (+ ?a 0) ?a)
(defrw mul-0 (* ?a 0) 0)
(defrw mul-1 (* ?a 1) ?a)

(defun ast-size (enode arg-costs)
  (declare (ignore enode))
  (when (every #'identity arg-costs)
    (1+ (reduce #'+ arg-costs))))

(def-test simple.1 ()
  (let* ((*egraph* (make-egraph))
         (a (intern-term '(* 0 42)))
         (b (intern-term 0)))
    (egraph-rebuild)
    (is (eq :saturate
            (run-rewrites '(commute-add commute-mul add-0 mul-0 mul-1)
                          :check t :max-iter 10)))
    (is (eq (enode-find a) (enode-find b)))
    (is (equal 0 (greedy-extract a #'ast-size)))))

(def-test simple.2 ()
  (let* ((*egraph* (make-egraph))
         (a (intern-term '(+ 0 (* 1 foo))))
         (b (intern-term 'foo)))
    (egraph-rebuild)
    (is (eq :saturate
            (run-rewrites '(commute-add commute-mul add-0 mul-0 mul-1)
                          :check t :max-iter 10)))
    (is (eq (enode-find a) (enode-find b)))
    (is (equal 'foo (greedy-extract a #'ast-size)))))

(defrw assoc-add (+ ?a (+ ?b ?c)) (+ (+ ?a ?b) ?c))
(defrw assoc-mul (* ?a (* ?b ?c)) (* (* ?a ?b) ?c))

(def-test ac ()
  (let ((*egraph* (make-egraph)))
    (intern-term '(+ 0 (+ 1 (+ 2 (+ 3 (+ 4 (+ 5 (+ 6 7))))))))
    (egraph-rebuild)
    (is (eq :saturate
            (run-rewrites '(commute-add assoc-add)
                          :check t :max-iter 10)))
    (is (= 6058 (egraph-n-enodes *egraph*)))
    (is (= 255 (egraph-n-eclasses *egraph*)))))

(defrw sub-cancel (- ?a ?a) 0)
(defrw add-2 (+ ?a ?a) (* 2 ?a))

(def-test nonlinear ()
  (let* ((*egraph* (make-egraph))
         (a (intern-term '(- (+ a b) (+ b a))))
         (b (intern-term '(- (+ a a) (* 2 a))))
         (c (intern-term '(+ a (- b (* 1 b))))))
    (egraph-rebuild)
    (is (eq :saturate
            (run-rewrites '(sub-cancel add-2
                                   commute-add commute-mul add-0 mul-0 mul-1)
                          :check t :max-iter 10)))
    (is (equal 0 (greedy-extract a #'ast-size)))
    (is (equal 0 (greedy-extract b #'ast-size)))
    (is (equal 'a (greedy-extract c #'ast-size)))))

(defrw d-sin (d ?x (sin ?x)) (cos ?x))
(defrw d-cos (d ?x (cos ?x)) (* -1 (sin ?x)))

(def-test ground ()
  (let* ((*egraph* (make-egraph))
         (a (intern-term '(d a (sin a))))
         (b (intern-term '(d (+ a b) (sin (+ b a)))))
         (b-1 (intern-term '(cos (+ a b))))
         (c (intern-term '(d a (d a (sin a)))))
         (c-1 (intern-term '(* -1 (sin a)))))
    (egraph-rebuild)
    (is (eq :saturate
            (run-rewrites '(d-sin d-cos
                                   commute-add commute-mul add-0 mul-0 mul-1)
                          :check t :max-iter 10)))
    (is (equal '(cos a) (greedy-extract a #'ast-size)))
    (is (eq (enode-find b-1) (enode-find b)))
    (is (eq (enode-find c-1) (enode-find c)))))

(defrw -add-0 ?a (+ ?a 0))

(def-test single-var-pat ()
  (let* ((*egraph* (make-egraph))
         (a (intern-term 'a))
         (b (intern-term '(+ (+ (+ a 0) 0) 0))))
    (egraph-rebuild)
    (is (eq :saturate
            (run-rewrites '-add-0 :check t :max-iter 10)))
    (is (eq (enode-find a) (enode-find b)))))

;;; E-analysis

(define-analysis const
  :make (lambda (fsym &rest args)
          (if args
              (block nil
                (let ((args (mapcar (lambda (enode)
                                      (or (get-analysis-data enode 'const)
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
                     (const (intern-enode (make-enode term))))
                (declare (dynamic-extent term))
                (enode-merge node const)
                (setf (egraph::eclass-info-nodes (enode-eclass-info node))
                      (list const))))))

(def-test analysis.const ()
  (let* ((*egraph* (make-egraph :analyses 'const))
         (a (intern-term '(+ 3 (+ 2 a))))
         (b (intern-term '(+ a 5)))
         (c (intern-term '(+ 2 (* 3 5)))))
    (egraph-rebuild)
    (is (eq :saturate
            (run-rewrites '(commute-add commute-mul assoc-add assoc-mul) :check t :max-iter 10)))
    (is (eq (enode-find a) (enode-find b)))
    (is (equal 17 (greedy-extract c #'ast-size)))))

(define-analysis var
  :make (lambda (fsym &rest args)
          (when (and (not args) (symbolp fsym))
            fsym))
  :merge (lambda (x y)
           (if (and (not x) y)
               (values y t)
               (values x nil))))

(defrw d-var (d ?x ?x) 1 :guard (get-analysis-data ?x 'var))
(defrw d-const (d ?x ?c) 0 :guard (or (get-analysis-data ?c 'const)
                                      (alexandria:when-let* ((vx (get-analysis-data ?x 'var))
                                                             (vc (get-analysis-data ?c 'var)))
                                        (not (eq vx vc)))))

(def-test analysis.multiple.1 ()
  (let* ((*egraph* (make-egraph :analyses '(var const)))
         (a (intern-term '(+ (d x (+ 1 2)) (d y y)))))
    (egraph-rebuild)
    (run-rewrites '(commute-add assoc-add d-var d-const) :max-iter 10)
    (is (eq (enode-find (intern-term 1)) (enode-find a)))))

(def-test analysis.multiple.2 ()
  (let* ((*egraph* (make-egraph :analyses '(const var)))
         (a (intern-term '(+ (d x (+ 1 2)) (d y y)))))
    (egraph-rebuild)
    (run-rewrites '(commute-add assoc-add d-var d-const) :max-iter 10)
    (is (eq (enode-find (intern-term 1)) (enode-find a)))))

;;; Micro benchmark

(defrw sub-canon (- ?a ?b) (+ ?a (* -1 ?b)))

(defrw distribute (* ?a (+ ?b ?c)) (+ (* ?a ?b) (* ?a ?c)))
(defrw factor (+ (* ?a ?b) (* ?a ?c)) (* ?a (+ ?b ?c)))

(defrw d-add (d ?x (+ ?a ?b)) (+ (d ?x ?a) (d ?x ?b)))
(defrw d-mul (d ?x (* ?a ?b)) (+ (* ?a (d ?x ?b)) (* ?b (d ?x ?a))))

(defrw i-const (i ?x 1) ?x)
(defrw i-cos (i ?x (cos ?x)) (sin ?x))
(defrw i-sin (i ?x (sin ?x)) (* -1 (cos ?x)))
(defrw i-add (i ?x (+ ?f ?g)) (+ (i ?x ?f) (i ?x ?g)))
(defrw i-sub (i ?x (- ?f ?g)) (- (i ?x ?f) (i ?x ?g)))
(defrw i-part (i ?x (* ?a ?b)) (- (* ?a (i ?x ?b)) (i ?x (* (d ?x ?a) (i ?x ?b)))))

(def-test bench.math ()
  (let ((timer (benchmark:make-timer)))
    (loop for i from 1 to 3 do
      (let ((*egraph* (make-egraph)))
        (format t "~&Benchmark run ~a." i)
        (trivial-garbage:gc :full t)
        (intern-term '(i x (ln x)))
        (intern-term '(i x (+ x (cos x))))
        (intern-term '(i x (* (cos x) x)))
        (intern-term '(d x (+ 1 (* 2 x))))
        (intern-term '(d x (- (pow x 3) (* 7 (pow x 2)))))
        (intern-term '(+ (* y (+ x y)) (- (+ x 2) (+ x x))))
        (intern-term '(/ 1 (- (/ (+ 1 (sqrt five)) 2) (/ (- 1 (sqrt five)) 2))))
        (egraph-rebuild)
        (benchmark:with-sampling (timer)
          (run-rewrites '(commute-add commute-mul add-0 mul-0 mul-1
                          assoc-add assoc-mul sub-canon sub-cancel distribute factor
                          d-add d-mul d-sin d-cos
                          i-const i-cos i-sin i-add i-sub i-part)
                        :max-iter 11))
        (is (= 443792 (egraph-n-eclasses *egraph*)))
        (is (= 1047556 (egraph-n-enodes *egraph*)))))
    (benchmark:report timer)))

(def-test bench.analysis-ac ()
  (let ((timer (benchmark:make-timer)))
    (loop for i from 1 to 3 do
      (let ((*egraph* (make-egraph :analyses 'const)))
        (format t "~&Benchmark run ~a." i)
        (trivial-garbage:gc :full t)
        (intern-term '(+ 0 (+ 1 (+ 2 (+ 3 (+ 4 (+ 5 (+ 6 (+ 7 (+ x (+ y (+ z w))))))))))))
        (egraph-rebuild)
        (benchmark:with-sampling (timer)
          (run-rewrites '(commute-add assoc-add)))
        (is (= 479 (egraph-n-eclasses *egraph*)))
        (is (= 39088 (egraph-n-enodes *egraph*)))))
    (benchmark:report timer)))
