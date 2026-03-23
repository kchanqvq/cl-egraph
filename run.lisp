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

(defun random-search (term rules cost-fn &key (beta 5.0) (seed 0) (max-iter 10000))
  (let* ((*random-state* (sb-ext:seed-random-state seed))
         (rules (mapcar (alexandria:rcurry #'get 'egraph::term-rewrite) rules))
         (egraph::*term* term)
         (e^beta (exp beta))
         (cost (funcall cost-fn term))
         (best-cost cost)
         (all-pc 0)
         (accept-pc 0)
         #+nil cost-hist)
    (loop for i below max-iter do
      (let ((selected-nonce (- (log (random 1.0f0) 2)))
            (selected-term egraph::*term*)
            (selected-cost cost))
        (dolist (rule rules)
          (funcall rule
                   (lambda (result)
                     (let* ((new-cost (funcall cost-fn result))
                            (1/weight (if (<= new-cost cost) 1.0f0
                                          (expt e^beta (- new-cost cost))))
                            (nonce (* (- (log (random 1.0f0) 2)) 1/weight)))
                       (incf all-pc)
                       (when (< nonce selected-nonce)
                         (incf accept-pc)
                         (setq selected-nonce nonce
                               selected-term result
                               selected-cost new-cost))))))
        (setq egraph::*term* selected-term
              cost selected-cost)
        #+nil (push cost cost-hist)
        (when (< cost best-cost)
          (print (list i cost egraph::*term*))
          (setq best-cost cost))))
    (values cost egraph::*term* all-pc accept-pc #+nil (nreverse cost-hist))))
