(in-package :egraph)

(defun stochastic-search
    (term rules cost-fn
     &key (beta (constantly 2.0)) (seed 0)
       (max-stall 16000) (max-restart 64)
       (target-cost 0.0) verbose
       (normalizer *term-normalizer*))
  (let* ((init-term term)
         (init-cost (funcall cost-fn term))
         (rules (mapcar (alexandria:rcurry #'get 'term-rewrite) rules))
         (*term-normalizer* normalizer)
         (best-cost init-cost)
         (best-term init-term)
         (n-proposal 0)
         (n-accepted 0))
    (float-features:with-float-traps-masked t
      ;; Outer loop: restart with different seeds
      (block solve
        (loop for seed from seed below (+ seed max-restart) do
          ;; Inner loop: one run of stochastic search
          (let* ((*random-state* (sb-ext:seed-random-state seed))
                 (*term* init-term)
                 (cost init-cost)
                 (best-cost-1 cost)
                 (n-stall 0))
            (loop for i from 0 do
              ;; select a rewrite via reservoir sampling
              (let ((e^beta (exp (funcall beta i)))
                    (selected-nonce (- (log (random 1.0f0) 2)))
                    (selected-term *term*)
                    (selected-cost cost))
                (dolist (rule rules)
                  (funcall rule
                           (lambda (result)
                             (let* ((new-cost (funcall cost-fn result))
                                    (1/weight (if (<= new-cost cost) 1.0f0
                                                  (expt e^beta (- new-cost cost))))
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
    (values best-cost best-term n-proposal n-accepted)))
