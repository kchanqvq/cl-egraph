(in-package :egraph)

(defmacro klet (((name (&rest args) &body kbody)) &body body)
  `(flet ((,name ,args ,@kbody))
     (declare (dynamic-extent #',name))
     ,@body))

#+nil
(defmacro do-subterm ((subterm-var cont-var term cont) &body body)
  (with-gensyms (process tail revtail result cont-1 arg args)
    `(labels ((,process (,subterm-var ,cont-var)
                ,@body
                (when (node-p ,subterm-var)
                  (do ((,tail (node-args ,subterm-var) (cdr ,tail))
                       (,revtail))
                      ((null ,tail))
                    (declare (optimize speed)
                             (dynamic-extent ,revtail))
                    (klet ((,cont-1 (,result)
                             (let ((,args (cons ,result (cdr ,tail))))
                               (declare (dynamic-extent ,args))
                               (dolist (,arg ,revtail)
                                 (push ,arg ,args))
                               (funcall ,cont-var
                                        (apply *term-normalizer* (node-fsym ,subterm-var) ,args)))))
                      (,process (car ,tail) #',cont-1))
                    (push (car ,tail) ,revtail)))))
       (,process ,term ,cont))))

(defun subst-row (new old pat-row)
  (maplist (lambda (tail)
             (if (cdr tail)
                 (subst new old (car tail))
                 `(let ((,old ,new))
                    (declare (ignorable ,old))
                    ,(car tail))))
           pat-row))

(defun expand-term-match (var-list pat-mat)
  (unless var-list
    (return-from expand-term-match
      (mapcar #'serapeum:only-elt pat-mat)))
  ;; pattern column selection heuristics
  (when pat-mat
    (setq pat-mat (copy-tree pat-mat)
          var-list (copy-list var-list))
    (let* ((columns (butlast (apply #'mapcar #'list pat-mat)))
           (column-n-tested (mapcar (lambda (c)
                                      (count-if-not #'var-p (mapcar #'ensure-car c)))
                                    columns))
           (selected (position (reduce #'max column-n-tested)
                               column-n-tested)))
      (rotatef (car var-list) (nth selected var-list))
      (mapc (lambda (row)
              (rotatef (car row) (nth selected row)))
            pat-mat)))
  (let ((groups (make-hash-table))
        (var (car var-list))
        bind-rows
        node-clauses
        atom-clauses)
    (dolist (pat-row pat-mat)
      (let ((pat (car pat-row)))
        (cond ((consp pat)
               (push pat-row (gethash (car pat) groups)))
              ((var-p pat)
               (push pat-row bind-rows))
              (t
               (push pat-row (gethash pat groups))))))
    (maphash-values
     (lambda (pat-rows)
       (let* ((sample (ensure-list (caar pat-rows)))
              (arg-vars (make-gensym-list (length (cdr sample))
                                          (prin1-to-string (car sample)))))
         (if arg-vars
             (push `((,(car sample))
                     (let ,(mapcar (lambda (i arg-var)
                                     `(,arg-var (rose-node-arg ,i ,var)))
                                   (iota (length arg-vars)) arg-vars)
                       ,@(expand-term-match
                          (append arg-vars (cdr var-list))
                          (mapcar (lambda (pat-row)
                                    (append (cdr (ensure-list (car pat-row)))
                                            (cdr pat-row)))
                                  pat-rows))))
                   node-clauses)
             (push `((,(car sample))
                     ,@(expand-term-match
                        (cdr var-list)
                        (mapcar #'cdr pat-rows)))
                   atom-clauses))))
     groups)
    (append
     (expand-term-match
      (cdr var-list)
      (mapcar (lambda (pat-row)
                (subst-row var (car pat-row) (cdr pat-row)))
              bind-rows))
     (when (or node-clauses atom-clauses)
       `((if (vectorp ,var)
             (case (rose-node-fsym ,var) ,@node-clauses)
             (case ,var ,@atom-clauses)))))))

(defun expand-term-template (tmpl)
  (labels ((process (tmpl)
             (cond ((consp tmpl)
                    `(funcall *term-normalizer*
                              (vector 0.0 -1 1 ',(car tmpl)
                                      ,@(mapcar #'process (cdr tmpl)))))
                   ((var-p tmpl) tmpl)
                   (t `',tmpl))))
    (process tmpl)))

(defun node-equal (x y)
  (cond
    ((and (not (vectorp x)) (not (vectorp y))) (eql x y))
    ((and (vectorp x) (vectorp y))
     (unless (= (length x) (length y))
       (return-from node-equal nil))
     (loop for i from (1- +rose-node-args-offset+) below (length x)
           always (node-equal (svref x i) (svref y i))))))

(defun decompose-occur-check (pat cont-expr)
  (let (vars checks)
    (labels ((process (pat)
               (cond ((consp pat)
                      (cons (car pat)
                            (mapcar #'process (cdr pat))))
                     ((var-p pat)
                      (if (member pat vars)
                          (let ((new-var (gensym-1 pat)))
                            (push `(node-equal ,pat ,new-var) checks)
                            new-var)
                          (progn
                            (push pat vars)
                            pat)))
                     (t pat))))
      (values (process pat)
              `(when (and ,@checks)
                 ,cont-expr)))))

(defmacro do-term-matches* (top-term-var &rest clauses)
  (let ((pat-rows (mapcar (lambda (clause)
                            (multiple-value-list
                             (decompose-occur-check
                              (car clause)
                              `(locally ,@(cdr clause)))))
                          clauses)))
    `(progn ,@(expand-term-match (list top-term-var) pat-rows))))
