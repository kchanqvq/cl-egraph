(in-package :egraph)

(defun run-rewrites (rules &key max-iter check verbose
                             initial-match-limit
                             (initial-ban-length 5))
  "Run RULES repeatly on `*egraph*' until some stop criterion.

Returns the reason for termination: one of :max-iter, :saturate.

If INITIAL-MATCH-LIMIT is non-nil, schedule rules in the style of
egg's BackoffScheduler.

Note: this function does not call `egraph-rebuild' upfront. Particularly, if you
have added some terms to EGRAPH, you MUST call `egraph-rebuild' before calling
this function."
  (let ((n-enodes (egraph-n-enodes *egraph*))
        (n-eclasses (egraph-n-eclasses *egraph*))
        (n-iter 0)
        (ban-until-table (make-hash-table))
        (ban-times-table (make-hash-table)))
    (catch 'stop
      (loop
        (when (and max-iter (>= n-iter max-iter))
          (return :max-iter))
        (when verbose (format t "Iteration ~d: " n-iter))
        (when verbose (format t "Applying rules... "))
        (unwind-protect
             (dolist (rule (ensure-list rules))
               (let* ((ban-until (gethash rule ban-until-table))
                      (ban-times (gethash rule ban-times-table 0))
                      (match-limit (and initial-match-limit
                                        (ash initial-match-limit ban-times))))
                 (unless (and ban-until (< n-iter ban-until))
                   (remhash rule ban-until-table)
                   (handler-case (funcall rule :match-limit match-limit)
                     (match-limit-exceeded (c)
                       (when verbose (format t "~&~a~%" c))
                       (setf (gethash rule ban-until-table)
                             (+ n-iter (ash initial-ban-length ban-times)))
                       (incf (gethash rule ban-times-table 0)))))))
          (when verbose (format t "Rebuilding... "))
          (egraph-rebuild))
        (when check (check-egraph))
        (incf n-iter)
        (let ((n-enodes-1 (egraph-n-enodes *egraph*))
              (n-eclasses-1 (egraph-n-eclasses *egraph*)))
          (when verbose
            (format t "Done. ~a enodes, ~a eclasses~%" n-enodes-1 n-eclasses-1))
          (cond ((not (and (= n-enodes n-enodes-1) (= n-eclasses n-eclasses-1)))
                 (setq n-enodes n-enodes-1 n-eclasses n-eclasses-1))
                ;; Some rules are still banned, skip till they reactivate
                ((plusp (hash-table-count ban-until-table)))
                (t (return :saturate))))))))

(defun random-search (term rules cost-fn
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
