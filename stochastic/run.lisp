(in-package :ggs/stochastic)

(declaim (inline fastlog2 fastexp2))
(serapeum:eval-always
  (defun fastlog2 (p)
    "Compute log2(P) approximately for *positive* integer P."
    (declare (optimize speed) (fixnum p))
    (let* ((exponent (1- (integer-length p)))
           (x (scale-float (coerce p 'single-float) (- exponent))))
      (declare (type single-float x))
      (+ exponent (- (* -0.4326728 x (- x 5.261706)) 1.8439242)))))

(declaim (inline fastexp2))
(defun constant-for-fastexp2 (base)
  (values (floor (* (expt 2 23) (log base 2)))))
(defun fastexp2 (p base-constant)
  (declare (optimize speed (safety 0))
           (fixnum p base-constant))
  (min (float-features:bits-single-float
        (max 0 (min #x7F800000 (+ (the fixnum (* p base-constant))
                                  (- (ash 127 23) 366393)))))
       1e20))

(defun recompute-rose (node rules cost-fn beta-constant)
  (declare (optimize speed (safety 0))
           ((function (t) fixnum) cost-fn))
  (labels ((process (node)
             (declare (rose-node node))
             (flet ((consider-rewrites (subject)
                      (let ((cost (funcall cost-fn subject)))
                        (klet ((cont (candidate)
                                 (incf (rose-node-n-rewrites node))
                                 (incf (rose-node-weight node)
                                       (fastexp2 (- cost (funcall cost-fn candidate))
                                                 beta-constant))))
                          (dolist (rule rules)
                            (declare (function rule))
                            (funcall rule subject #'cont))))))
               (setf (rose-node-n-rewrites node) 0)
               (do-rose-node-args (arg node)
                 (if (vectorp arg)
                     (progn
                       (when (minusp (rose-node-n-rewrites arg))
                         (process arg))
                       (incf (rose-node-n-rewrites node) (rose-node-n-rewrites arg))
                       (incf (rose-node-weight node) (rose-node-weight arg)))
                     ;; Probability weight of constant symbol children
                     ;; are counted together
                     (consider-rewrites arg)))
               (consider-rewrites node))))
    (when (vectorp node)
      (when (minusp (rose-node-n-rewrites node))
        (process node)))))

(defun stochastic-search-1
    (term rules cost-fn
     &key (finish-flag (list nil)) (seed 0) (stride 1)
       (beta 2.0) (inf-temp-period 100) (inf-temp-iters 3)
       (max-stall 16000) (max-restart 64)
       (target-cost 0) max-time (inf-cost 100000000)
       (normalizer *term-normalizer*) (proxy-cost-fn cost-fn)
       verbose)
  (declare ((or null fixnum) inf-temp-period)
           ((or null fixnum) inf-temp-iters)
           (single-float beta))
  (let* ((end-time (and max-time
                        (+ (get-internal-real-time)
                           (* max-time internal-time-units-per-second))))
         (rules (mapcar (alexandria:rcurry #'get 'term-rewrite) rules))
         (cost-fn (ensure-function cost-fn))
         (proxy-cost-fn (ensure-function proxy-cost-fn))
         (beta-constant (constant-for-fastexp2 (exp (/ beta 2))))
         (*term-normalizer* (ensure-function normalizer))
         (init-term (make-term-1 term))
         (init-cost (funcall cost-fn init-term))
         (best-term (demake-term-1 init-term))
         (best-cost init-cost)
         (n-accepted 0)
         (n-restart 0))
    (declare ((function (t) fixnum) cost-fn proxy-cost-fn))
    (assert (every #'functionp rules))
    (float-features:with-float-traps-masked t
      ;; Outer loop: restart with different seeds
      (block solve
        (loop for seed from seed below (+ seed max-restart) by stride do
          ;; Inner loop: one run of stochastic search
          (let* ((*random-state* (sb-ext:seed-random-state seed))
                 (*term* init-term)
                 (best-cost-1 init-cost)
                 (n-stall 0))
            (declare (fixnum n-accepted n-restart))
            (incf n-restart)
            (loop for i of-type fixnum from 0 do
              (progn
                (when (or (car finish-flag)
                          (and end-time (>= (get-internal-real-time) end-time)))
                  (return-from solve))
                (recompute-rose *term* rules proxy-cost-fn beta-constant)

                ;; FIXME: a constant top-level *term* might still be rewritable,
                ;; although this probably is not usually useful.
                (when (or (not (vectorp *term*))
                          (zerop (rose-node-n-rewrites *term*)))
                  (return))

                (macrolet ((consider-rewrites (subject weight-var weight-form context-form)
                             `(klet ((cont (candidate)
                                       (decf ,weight-var ,weight-form)
                                       (when (minusp ,weight-var)
                                         (setq *term* ,context-form)
                                         (return))))
                                (dolist (rule rules)
                                  (declare (function rule))
                                  (funcall rule ,subject #'cont)))))
                  (declare (optimize speed))
                  (block nil
                    (if (and inf-temp-period inf-temp-iters
                             (< (mod i inf-temp-period)
                                inf-temp-iters))
                        ;; Inf temperature
                        (multiple-value-bind (subject context n-rewrites)
                            (search-rose-n-rewrites *term* (random (rose-node-n-rewrites *term*)))
                          (declare (fixnum n-rewrites))
                          ;; rewrites for this rose node
                          (consider-rewrites subject n-rewrites 1 (funcall context candidate))
                          ;; rewrites for constant symbol children
                          (do-rose-node-args ((arg i) subject)
                            (unless (vectorp arg)
                              (consider-rewrites arg n-rewrites 1
                                                 (let ((new-node (copy-seq subject)))
                                                   (setf (rose-node-weight new-node) 0.0
                                                         (rose-node-n-rewrites new-node) -1
                                                         (rose-node-cost new-node) 1
                                                         (rose-node-arg i new-node) candidate)
                                                   (funcall context (funcall *term-normalizer* new-node)))))))
                        ;; Finite temperature
                        (multiple-value-bind (subject context weight)
                            (search-rose-weight *term* (random (rose-node-weight *term*)))
                          (declare (single-float weight))
                          ;; rewrites for this rose node
                          (let ((cost-1 (funcall proxy-cost-fn subject)))
                            (consider-rewrites subject weight
                                               (fastexp2 (- cost-1 (funcall proxy-cost-fn candidate))
                                                         beta-constant)
                                               (funcall context candidate)))
                          ;; rewrites for constant symbol children
                          (do-rose-node-args ((arg i) subject)
                            (unless (vectorp arg)
                              (let ((cost-1 (funcall proxy-cost-fn arg)))
                                (consider-rewrites arg weight
                                                   (fastexp2 (- cost-1 (funcall proxy-cost-fn candidate))
                                                             beta-constant)
                                                   (let ((new-node (copy-seq subject)))
                                                     (setf (rose-node-weight new-node) 0.0
                                                           (rose-node-n-rewrites new-node) -1
                                                           (rose-node-cost new-node) 1
                                                           (rose-node-arg i new-node) candidate)
                                                     (funcall context (funcall *term-normalizer* new-node)))))))))))
                (incf n-accepted)
                (let ((cost (funcall cost-fn *term*)))
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
                    (return)))))))))
    (values best-cost best-term n-accepted n-restart)))

(defun reduce-stochastic-result (results-1 results-2)
  (destructuring-bind (bc1 bt1 na1 nr1) results-1
    (destructuring-bind (bc2 bt2 na2 nr2) results-2
      (append (if (< bc1 bc2)
                  (list bc1 bt1)
                  (list bc2 bt2))
              (list (+ na1 na2) (+ nr1 nr2))))))

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
                          &key (seed 0) (stride 1)
                            (beta 2.0) (inf-temp-period 100) (inf-temp-iters 3)
                            (max-stall 16000) (max-restart 64)
                            (target-cost 0) max-time (inf-cost 100000000)
                            (normalizer *term-normalizer*) (proxy-cost-fn cost-fn)
                            verbose
                            (nproc 1) workers)
  (declare (ignore beta inf-temp-period inf-temp-iters
                   max-stall max-restart
                   target-cost max-time
                   normalizer proxy-cost-fn
                   verbose))
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
                                              (remove-from-plist args :workers :nproc :seed :stride))))
                                    :name (format nil "search worker ~a" i)))
                                 (iota nproc))))
           (unwind-protect
                (values-list (reduce #'reduce-stochastic-result threads :key #'bt:join-thread))
             (setf (car finish-flag) t))))
        ((= nproc 0) (values (1+ inf-cost) term 0 0 0))
        (t (apply #'stochastic-search-1
                  term rules cost-fn
                  (remove-from-plist args :nproc :workers)))))
