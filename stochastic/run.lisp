(in-package :egraph)

(defun stochastic-search (term rules cost-fn
                          &key (beta (constantly 2.0)) (seed 0)
                            (max-stall 16000) (max-restart 64)
                            (target-cost 0.0) verbose
                            (normalizer *term-normalizer*)
                            (nproc 1) max-time)
  (let (finish
        (init-term term)
        (init-cost (funcall cost-fn term))
        (start-time (get-internal-real-time)))
    (flet ((search-1 (seed stride)
             (let* ((rules (mapcar (alexandria:rcurry #'get 'term-rewrite) rules))
                    (*term-normalizer* normalizer)
                    (best-term init-term)
                    (best-cost init-cost)
                    (n-proposal 0)
                    (n-accepted 0))
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
                       (loop for i from 0 do
                         ;; select a rewrite via reservoir sampling
                         (let ((e^beta/2 (exp (/ (funcall beta i) 2)))
                               (selected-nonce (- (log (random 1.0f0) 2)))
                               (selected-term *term*)
                               (selected-cost cost))
                           (when (or finish
                                     (and max-time
                                          (> (/ (- (get-internal-real-time) start-time)
                                                internal-time-units-per-second)
                                             max-time)))
                             (return-from solve))
                           (dolist (rule rules)
                             (funcall rule
                                      (lambda (result)
                                        (let* ((new-cost (funcall cost-fn result))
                                               (1/weight (expt e^beta/2 (- new-cost cost)))
                                               (nonce (* (- (log (random 1.0f0) 2)) 1/weight)))
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
                                           seed i cost *term*))
                                 (setq best-cost-1 cost
                                       n-stall 0)
                                 (when (< cost best-cost)
                                   (setq best-cost cost
                                         best-term *term*)
                                   (when (<= cost target-cost)
                                     (return-from solve))))
                               (incf n-stall))
                           ;; Check for restart
                           (unless (< n-stall max-stall)
                             (when verbose
                               (format t "~&Iteration ~a/~a restart ~a ~a~%"
                                       seed i cost *term*))
                             (return))))))))
               (values best-cost best-term n-proposal n-accepted))))
      (if (> nproc 1)
          (let ((threads (mapcar (lambda (i)
                                   (bt:make-thread
                                    (lambda ()
                                      (multiple-value-prog1 (search-1 (+ seed i) nproc)
                                        (setq finish t)))
                                    :name (format nil "search worker ~a" i)))
                                 (iota nproc)))
                (best-cost init-cost)
                (best-term init-term)
                (n-proposal 0)
                (n-accepted 0))
            (unwind-protect
                 (dolist (thread threads)
                   (multiple-value-bind (bc1 bt1 np1 na1) (bt:join-thread thread)
                     (when (< bc1 best-cost)
                       (setq best-cost bc1 best-term bt1))
                     (incf n-proposal np1)
                     (incf n-accepted na1)))
              (setq finish t))
            (values best-cost best-term n-proposal n-accepted))
          (search-1 seed 1)))))
