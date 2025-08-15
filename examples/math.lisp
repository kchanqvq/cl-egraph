(in-package :egraph/tests)

(in-suite :egraph)

(defrw sub-canon (- ?a ?b) (+ ?a (* -1 ?b)))
(defrw div-canon (/ ?a ?b) (* ?a (pow ?b -1)) :guard (not (eql 0 (get-analysis-data ?b 'const))))

(defrw -add-0 ?a (+ ?a 0))
(defrw -mul-1 ?a (* ?a 1))

(defrw div-cancel (/ ?a ?a) 1 :guard (not (eql 0 (get-analysis-data ?a 'const))))

(defrw distribute (* ?a (+ ?b ?c)) (+ (* ?a ?b) (* ?a ?c)))
(defrw factor (+ (* ?a ?b) (* ?a ?c)) (* ?a (+ ?b ?c)))

(defrw pow-mul (* (pow ?a ?b) (pow ?a ?c)) (pow ?a (+ ?b ?c)))
(defrw pow-0 (pow ?a 0) 1 :guard (not (eql 0 (get-analysis-data ?a 'const))))
(defrw pow-1 (pow ?a 1) ?a)
(defrw pow-2 (pow ?a 2) (* ?a ?a))
(defrw pow-recip (pow ?a -1) (/ 1 ?a) :guard (not (eql 0 (get-analysis-data ?a 'const))))
(defrw recip-mul-div (* ?x (/ 1 ?x)) 1 :guard (not (eql 0 (get-analysis-data ?x 'const))))

(declaim (inline pow))
(defun pow (x y) (expt x y))

(def-test math.simplify-root ()
  (let* ((*egraph* (make-egraph :analyses (list (make-const-analysis))))
         (a (make-term '(/ 1 (- (/ (+ 1 (sqrt five)) 2)
                                     (/ (- 1 (sqrt five)) 2))))))
    (egraph-rebuild)
    (run-rewrites '(commute-add commute-mul assoc-add assoc-mul
                    sub-canon div-canon add-0 mul-0 mul-1 -add-0 -mul-1
                    sub-cancel div-cancel
                    distribute factor
                    pow-mul pow-0 pow-1 pow-2 pow-recip
                    recip-mul-div)
                  :max-enodes 75000)
    (is (equal '(/ 1 (sqrt five)) (greedy-extract a #'ast-size)))))

(def-test math.simplify-factor ()
  (let* ((*egraph* (make-egraph :analyses (list (make-const-analysis))))
         (a (make-term '(* (+ x 3) (+ x 1)))))
    (egraph-rebuild)
    (run-rewrites '(commute-add commute-mul assoc-add assoc-mul
                    sub-canon div-canon add-0 mul-0 mul-1 -add-0 -mul-1
                    sub-cancel div-cancel
                    distribute factor
                    pow-mul pow-0 pow-1 pow-2 pow-recip
                    recip-mul-div)
                  :max-enodes 75000)
    (is (eq (enode-find (make-term '(+ (+ (* x x) (* 4 x)) 3)))
            (enode-find a)))))
