(in-package :egraph)

(defun run-rewrites (rules &key max-iter check verbose
                             (max-enodes array-total-size-limit))
  "Run RULES repeatly on `*egraph*' until some stop criterion.

Returns the reason for termination: one of :max-iter, :max-enodes, :saturate.

Note: this function does not call `egraph-rebuild' upfront. Particularly, if you
have added some terms to EGRAPH, you MUST call `egraph-rebuild' before calling
this function."
  (let ((n-enodes (egraph-n-enodes *egraph*))
        (n-eclasses (egraph-n-eclasses *egraph*))
        (n-iter 0))
    (setf (egraph-enode-limit *egraph*) max-enodes)
    (catch 'stop
      (loop
        (when (and max-iter (>= n-iter max-iter))
          (return :max-iter))
        (when verbose (format t "Iteration ~d: " n-iter))
        (when verbose (format t "Applying rules... "))
        (unwind-protect
             (dolist (rule (ensure-list rules))
               (funcall rule))
          (when verbose (format t "Rebuilding... "))
          (egraph-rebuild))
        (when check (check-egraph))
        (incf n-iter)
        (let ((n-enodes-1 (egraph-n-enodes *egraph*))
              (n-eclasses-1 (egraph-n-eclasses *egraph*)))
          (when verbose
            (format t "Done. ~a enodes, ~a eclasses~%" n-enodes-1 n-eclasses-1))
          (if (and (= n-enodes n-enodes-1) (= n-eclasses n-eclasses-1))
              (return :saturate)
              (setq n-enodes n-enodes-1 n-eclasses n-eclasses-1)))))))
