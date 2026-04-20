(in-package :egraph)

(declaim (inline fastlog2))
(serapeum:eval-always
  (defun fastlog2 (p)
    "Compute log2(P) approximately for *positive* integer P."
    (declare (optimize speed) (fixnum p))
    (let* ((exponent (1- (integer-length p)))
           (x (scale-float (coerce p 'single-float) (- exponent))))
      (declare (type single-float x))
      (+ exponent (- (* -0.4326728 x (- x 5.261706)) 1.8439242)))))

(defun stochastic-search-1
    (term rules cost-fn
     &key (finish-flag (list nil)) (seed 0) (stride 1)
       (beta 2.0)
       (inf-temp-period 100) (inf-temp-iters 3)
       (max-stall 16000) (max-restart 64)
       (target-cost 0) verbose
       (normalizer *term-normalizer*)
       max-time (inf-cost 100000000))
  (declare ((or null fixnum) inf-temp-period)
           ((or null fixnum) inf-temp-iters)
           (single-float beta))
  (let* ((end-time (and max-time
                        (+ (get-internal-real-time)
                           (* max-time internal-time-units-per-second))))
         (rules (mapcar (alexandria:rcurry #'get 'term-rewrite) rules))
         (cost-fn (ensure-function cost-fn))
         (*term-normalizer* (ensure-function normalizer))
         (*hash-cons* (make-weak-hash-cons))
         (init-term (make-term-1 term))
         (init-cost (funcall cost-fn init-term))
         (best-term term)
         (best-cost init-cost)
         (e^beta/2 (exp (/ beta 2)))
         (n-proposal 0)
         (n-accepted 0)
         (n-restart 0))
    (float-features:with-float-traps-masked t
      ;; Outer loop: restart with different seeds
      (block solve
        (loop for seed from seed below (+ seed max-restart) by stride do
          ;; Inner loop: one run of stochastic search
          (let* ((*random-state* (sb-ext:seed-random-state seed))
                 (*term* init-term)
                 (cost init-cost)
                 (best-cost-1 cost)
                 (n-stall 0))
            (declare (fixnum cost n-proposal n-accepted n-restart))
            (incf n-restart)
            (loop for i of-type fixnum from 0 do
              ;; select a rewrite via reservoir sampling
              (let ((selected-nonce (- #.(fastlog2 most-positive-fixnum)
                                       (fastlog2 (random most-positive-fixnum))))
                    (selected-term *term*)
                    (selected-cost cost))
                (when (or (car finish-flag)
                          (and end-time (>= (get-internal-real-time) end-time)))
                  (return-from solve))
                (dolist (rule rules)
                  (funcall rule
                           (lambda (result)
                             (declare (optimize speed (safety 0)))
                             (let* ((new-cost (funcall cost-fn result))
                                    (1/weight (if (and inf-temp-period
                                                       inf-temp-iters
                                                       (< (mod i inf-temp-period)
                                                          inf-temp-iters))
                                                  1.0
                                                  (expt e^beta/2 (- new-cost cost))))
                                    (nonce (* (- #.(fastlog2 most-positive-fixnum)
                                                 (fastlog2 (random most-positive-fixnum)))
                                              1/weight)))
                               (declare (fixnum new-cost))
                               (incf n-proposal)
                               (when (< nonce selected-nonce)
                                 (setq selected-nonce nonce
                                       selected-term result
                                       selected-cost new-cost))))))
                (unless (eq *term* selected-term)
                  (incf n-accepted))
                (setq *term* selected-term
                      cost selected-cost)
                ;; Check for cost function decrease
                (if (< cost best-cost-1)
                    (progn
                      (when verbose
                        (format t "~&Iteration ~a/~a found ~a ~a~%"
                                seed i cost (demake-term-1 *term*)))
                      (setq best-cost-1 cost
                            n-stall 0)
                      (when (< cost best-cost)
                        (setq best-cost cost
                              best-term (demake-term-1 *term*))
                        (when (<= cost target-cost)
                          (setf (car finish-flag) t)
                          (return-from solve))))
                    (incf n-stall))
                ;; Check for restart
                (unless (and (< n-stall max-stall)
                             (< cost inf-cost))
                  (when verbose
                    (format t "~&Iteration ~a/~a restart ~a ~a~%"
                            seed i cost (demake-term-1 *term*)))
                  (return))))))))
    (values best-cost best-term n-proposal n-accepted n-restart)))

(defun reduce-stochastic-result (results-1 results-2)
  (destructuring-bind (bc1 bt1 np1 na1 nr1) results-1
    (destructuring-bind (bc2 bt2 np2 na2 nr2) results-2
      (append (if (< bc1 bc2)
                  (list bc1 bt1)
                  (list bc2 bt2))
              (list (+ np1 np2) (+ na1 na2) (+ nr1 nr2))))))

(defun worker-loop ()
  (with-standard-io-syntax
    (loop
      (handler-case
          (prin1 (cons :result (multiple-value-list (eval (read)))))
        (serious-condition (c)
          (prin1 (cons :error (princ-to-string c)))))
      (terpri)
      (finish-output))))

(defun stochastic-search (term rules cost-fn &rest args
                          &key (beta 2.0) (seed 0) (stride 1)
                            (inf-temp-period 100) (inf-temp-iters 3)
                            (max-stall 16000) (max-restart 64)
                            (target-cost 0) verbose
                            (normalizer *term-normalizer*)
                            max-time (inf-cost 100000000)
                            (nproc 1) workers)
  (declare (ignore beta inf-temp-period inf-temp-iters
                   max-stall max-restart
                   target-cost verbose
                   normalizer
                   max-time))
  (cond (workers
         (let ((n-workers (length workers)))
           (multiple-value-bind (nproc rem) (floor nproc n-workers)
             (loop for proc in workers
                   for i from 0
                   for nproc-1 = (if (< i rem) (1+ nproc) nproc)
                   do (with-standard-io-syntax
                        (write `(apply #'stochastic-search
                                       ',term ',rules ',cost-fn
                                       :seed ',(+ seed (* i stride))
                                       :stride ',(* n-workers stride)
                                       :nproc ',nproc-1
                                       ',(remove-from-plist args :workers :nproc :seed :stride))
                               :stream (uiop:process-info-input proc))
                        (terpri (uiop:process-info-input proc))
                        (finish-output (uiop:process-info-input proc)))))
           (values-list
            (reduce #'reduce-stochastic-result workers :key
                    (lambda (proc)
                      (let ((result (with-standard-io-syntax
                                      (read (uiop:process-info-output proc)))))
                        (ecase (car result)
                          (:result (cdr result))
                          (:error (error "Error in worker: ~a" (cadr result))))))))))
        ((> nproc 1)
         (let* ((finish-flag (list nil))
                (threads (mapcar (lambda (i)
                                   (bt:make-thread
                                    (lambda ()
                                      (multiple-value-list
                                       (apply #'stochastic-search-1 term rules cost-fn
                                              :seed (+ seed (* i stride))
                                              :stride (* nproc stride)
                                              :finish-flag finish-flag
                                              (remove-from-plist args :nproc :seed :stride))))
                                    :name (format nil "search worker ~a" i)))
                                 (iota nproc))))
           (unwind-protect
                (values-list (reduce #'reduce-stochastic-result threads :key #'bt:join-thread))
             (setf (car finish-flag) t))))
        ((= nproc 0) (values (1+ inf-cost) term 0 0 0))
        (t (apply #'stochastic-search-1
                  term rules cost-fn
                  (remove-from-plist args :nproc)))))
