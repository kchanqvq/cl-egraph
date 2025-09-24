(uiop:define-package :egraph/ac-math
    (:use :cl :egraph)
  (:shadow #:* #:+))

(in-package #:egraph/ac-math)

(serapeum:eval-always
  (setf (gethash '+ egraph::*ac-symbols*) t)
  (setf (gethash '* egraph::*ac-symbols*) t)
  (setf (fdefinition '+) #'cl:+)
  (setf (fdefinition '*) #'cl:*))

(defun make-const-analysis ()
  (make-analysis-info
   :name 'const
   :make (lambda (fsym args)
           (if args
               (when (every #'identity args)
                 ;; Guard against things like division by zero
                 (ignore-errors
                  (let* ((result (apply fsym args)))
                    ;; Coerce integral float into integer
                    (if (floatp result)
                        (multiple-value-bind (int frac) (truncate result (float 1.0 result))
                          (if (zerop frac) int result))
                        result))))
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
                 (setf (egraph::eclass-info-nodes (egraph::enode-parent (enode-find node)))
                       (list const)))))))

(defun make-cost-analysis ()
  (make-analysis-info
   :name 'cost
   :make #'ast-size
   :merge #'min))

(defun ast-size (fsym arg-costs)
  (declare (ignore fsym))
  (when (every #'identity arg-costs)
    (1+ (reduce #'cl:+ arg-costs))))

(defun cost (term cost-fn)
  (if (consp term)
      (let* ((fsym (car term)))
        (funcall cost-fn fsym (mapcar (alexandria:rcurry #'cost cost-fn) (cdr term))))
      (cost (list term) cost-fn)))

(defrw add-0 (+ ?a 0 ??rest) (+ ?a ??rest))
(defrw add-0% (+ 0 ?a ??rest) (+ ?a ??rest))
(defrw mul-0 (* ?a 0 ??rest) 0)
(defrw mul-0% (* 0 ?a ??rest) 0)
(defrw mul-1 (* ?a 1 ??rest) (* ?a ??rest))
(defrw mul-1% (* 1 ?a ??rest) (* ?a ??rest))

(defrw sub-cancel (- ?a ?a) 0)

(defrw add-2 (+ ?a ?a ??rest) (+ (* 2 ?a) ??rest))

(defrw sub-canon (- ?a ?b) (+ ?a (* -1 ?b)))
(defrw div-canon (/ ?a ?b) (* ?a (pow ?b -1)) :guard (not (eql 0 (get-analysis-data ?b 'const))))

(defrw -add-0 ?a (+ ?a 0))
(defrw -add-0% ?a (+ 0 ?a))
(defrw -mul-1 ?a (* ?a 1))
(defrw -mul-1% ?a (* 1 ?a))

(defrw div-cancel (/ ?a ?a) 1 :guard (not (eql 0 (get-analysis-data ?a 'const))))

(defrw distribute (* ?a (+ ?b ?c ??d) ??e) (+ (* ?a ?b ??e) (* ?a (+ ?c ??d) ??e)))
(defrw factor (+ (* ?a ?b ??d) (* ?a ?c ??e) ??f) (+ (* ?a (+ (* ?b ??d) (* ?c ??e))) ??f))
(defrw factor% (+ (* ?b ?a ??d) (* ?a ?c ??e) ??f) (+ (* ?a (+ (* ?b ??d) (* ?c ??e))) ??f))
(defrw factor%% (+ (* ?b ?a ??d) (* ?c ?a ??e) ??f) (+ (* ?a (+ (* ?b ??d) (* ?c ??e))) ??f))

(defrw pow-mul (* (pow ?a ?b) (pow ?a ?c) ??rest) (* (pow ?a (+ ?b ?c)) ??rest))
(defrw pow-0 (pow ?a 0) 1 :guard (not (eql 0 (get-analysis-data ?a 'const))))
(defrw pow-1 (pow ?a 1) ?a)
(defrw pow-2 (pow ?a 2) (* ?a ?a))
(defrw pow-recip (pow ?a -1) (/ 1 ?a) :guard (not (eql 0 (get-analysis-data ?a 'const))))
(defrw recip-mul-div (* ?x (/ 1 ?x) ??rest) (* ??rest) :guard (not (eql 0 (get-analysis-data ?x 'const))))

(defun add-unit ()
  (do-matches (top-node (+ ??rest))
    (cond ((not ??rest)
           (enode-merge top-node (make-enode '(0))))
          ((not (cdr ??rest))
           (enode-merge top-node (car ??rest))))))

(defun mul-unit ()
  (do-matches (top-node (* ??rest))
    (cond ((not ??rest)
           (enode-merge top-node (make-enode '(1))))
          ((not (cdr ??rest))
           (enode-merge top-node (car ??rest))))))

(defrw assoc-add (+ (+ ?a ?b ??d) ?c ??e) (+ ?a ?b ?c ??d ??e))
(defrw assoc-mul (* (* ?a ?b ??d) ?c ??e) (* ?a ?b ?c ??d ??e))

#+nil (defrw assoc-add (+ (+ ??a) ??b) (+ ??a ??b))
#+nil (defrw assoc-mul (* (* ??a) ??b) (* ??a ??b))

(defvar *math-base-rules*
  '(add-unit mul-unit ASSOC-ADD ASSOC-MUL SUB-CANON DIV-CANON
    ADD-0 #+nil add-0% MUL-0 #+nil mul-0% MUL-1 #+nil mul-1% -ADD-0 #+nil -add-0% -MUL-1 #+nil -mul-1%
    SUB-CANCEL
    DIV-CANCEL DISTRIBUTE FACTOR POW-MUL POW-0 POW-1 POW-2 POW-RECIP RECIP-MUL-DIV))

#+nil (let* ((*egraph* (make-egraph :analyses (list (make-cost-analysis) (make-const-analysis))))
       (lhs (make-term '(* (+ x 3) (+ x 1))))
       (rhs (make-term '(+ (+ (* x x) (* 4 x)) 3)))
       (egraph::*cost-analysis* 'cost)
       (egraph::*cost-fn* #'ast-size)
       (egraph::*max-cost* 6))
  (egraph-rebuild)
  (print (run-rewrites *math-base-rules* :max-enodes 1000))
  (print *egraph*)
  (eq (enode-find lhs) (enode-find rhs)))
