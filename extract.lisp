(in-package :egraph)

(defun build-term (enode selections)
  (let ((memo (make-hash-table)))       ; used to preserve sharing
    (labels ((process (class)
               (ensure-gethash
                class memo
                (let ((term (enode-term (gethash class selections))))
                  (if (cdr term)
                      (cons (car term) (mapcar #'process (cdr term)))
                      (car term))))))
      (process (enode-find enode)))))

(defun graph-cost (enode selections cost-fn)
  (let ((memo (make-hash-table))
        (cost 0))
    (labels ((process (class)
               (ensure-gethash
                class memo
                (let ((enode (gethash class selections)))
                  (incf cost (funcall cost-fn enode))
                  (mapc #'process (cdr (enode-term enode)))
                  t))))
      (process (enode-find enode))
      cost)))

(defun tree-cost (enode selections cost-fn)
  (let ((memo (make-hash-table)))
    (labels ((process (class)
               (ensure-gethash
                class memo
                (let ((enode (gethash class selections)))
                  (reduce #'+ (cdr (enode-term enode))
                          :key #'process :initial-value (funcall cost-fn enode))))))
      (process (enode-find enode)))))

(defun greedy-select (cost-fn)
  (lret ((costs (make-hash-table))       ; map eclass to cost
         (selections (make-hash-table))) ; map eclass to enode
    (loop
      (let (dirty)
        (maphash-keys (lambda (class)
                        (let ((selection (gethash class selections))
                              (cost (gethash class costs)))
                          (dolist (enode (list-enodes class))
                            (let* ((term (enode-term enode))
                                   (new-cost
                                     (funcall cost-fn enode
                                              (mapcar (rcurry #'gethash costs) (cdr term)))))
                              (when (if cost (and new-cost (< new-cost cost))
                                        new-cost)
                                (setf selection enode
                                      cost new-cost
                                      dirty t))))
                          (setf (gethash class selections) selection
                                (gethash class costs) cost)))
                      (egraph-classes *egraph*))
        (unless dirty (return))))))

(defun greedy-extract (enode cost-fn)
  "Greedy extract a term for ENODE from `*egraph*' using COST-FN.

COST-FN should accept 2 arguments: the enode and a list of costs for each
argument eclass. It should return a number. The cost of an extraction is the
cost of its root node."
  (build-term enode (greedy-select cost-fn)))

(defun lp-select (enode cost-fn)
  (let ((class-vars (make-hash-table))  ; map eclass to lp var or 'visiting
        (enode-vars (make-hash-table))
        (objective-terms nil)
        (constraints nil))
    (labels ((visit-class (class)
               (ensure-gethash
                class class-vars
                (progn
                  ;; Mark the eclass as 'visiting to detect back edge from
                  ;; enode, so that we can ensure acyclicity
                  (setf (gethash class class-vars) 'visiting)
                  (lret ((var (gensym-1 'class)))
                    (push `(lp:<=
                            ,var
                            (lp:+ ,@ (remove-if #'not (mapcar #'visit-enode (list-enodes class)))))
                          constraints)))))
             (visit-enode (enode)
               ;; Return a lp var or NIL. NIL is returned if there's back edge
               ;; to a visiting eclass, therefore this enode is not processed
               (unless (some (lambda (class) (eq (gethash class class-vars) 'visiting))
                             (cdr (enode-term enode)))
                 (lret ((cost (funcall cost-fn enode))
                        (var (gensym-1 (car (enode-term enode)))))
                   (setf (gethash enode enode-vars) var)
                   (push `(lp:* ,cost ,var) objective-terms)
                   (dolist (class (cdr (enode-term enode)))
                     (push `(lp:<= ,var ,(visit-class class)) constraints))))))
      (dolist (enode (ensure-list enode))
        (push `(lp:<= 1 ,(visit-class (enode-find enode))) constraints)))
    (lret ((solution
            (lp:solve-problem
             (lp:parse-linear-problem
              `(lp:min (lp:+ ,@objective-terms))
              `(,@constraints
                (lp:binary ,@ (hash-table-values class-vars))
                (lp:binary ,@ (hash-table-values enode-vars))))))
           (selections (make-hash-table)))
      (maphash
       (lambda (enode var)
         (when (= 1 (lp:solution-variable solution var))
           (setf (gethash (enode-find enode) selections) enode)))
       enode-vars))))

(defun lp-extract (enode cost-fn)
  "Extract a term for ENODE from `*egraph*' using ILP.

COST-FN should accept 1 argument: the enode. It should return a number. The cost
of an extraction is the sum of costs of the enodes it contains. Note that
different from `greedy-extract', the cost model is implicitly additive."
  (build-term enode (lp-select enode cost-fn)))
