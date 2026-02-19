(in-package :egraph)

(defun var-p (object)
  (typecase object
    (symbol (string-prefix-p "?" (symbol-name object)))))

(defvar *fsym-info-var-alist*)

(defun gensym-1 (thing)
  (make-gensym (prin1-to-string thing)))

(defun parse-pattern (pat eclass-var)
  "Convert PAT into a list of the form ((eclass-var fsym arg-var...) ...)."
  (cond ((consp pat)
         (let* ((fsym (car pat))
                (arg-vars (mapcar (lambda (arg) (if (var-p arg) arg (gensym-1 fsym))) (cdr pat))))
           (cons (list* eclass-var fsym arg-vars)
                 (mapcan (lambda (arg var)
                           (unless (var-p arg)
                             (parse-pattern arg var)))
                         (cdr pat) arg-vars))))
        ((var-p pat) (error "Single variable pattern should not be handled here."))
        (t ;; Non-variable atoms are short hand for 0-arity function symbol
         (parse-pattern (list pat) eclass-var))))

(defun expand-match (bound-vars subst-alist cont-expr)
  "Generate code that solves for SUBST-ALIST (as returned by `parse-pattern') then
evaluate CONT-EXPR."
  (if subst-alist
      (bind ((((var fsym . arg-vars) . rest) subst-alist)
             ((:flet lisp-var (var))
              (if (member var bound-vars)
                  (gensym-1 fsym)
                  (progn (push var bound-vars) var)))
             (lisp-arg-vars (mapcar #'lisp-var arg-vars))
             (fsym-info-var (serapeum:ensure (assoc-value *fsym-info-var-alist* fsym)
                              (gensym-1 fsym)))
             (lhs-bound-p (member var bound-vars))
             (node-var (lisp-var var)))
        ;; Currently we use indexes (in `fsym-info') as single source of truth
        ;; for matching, thus no-need to `enode-find' representative of VAR
        ;; (even if VAR is non-rep, it once was when we built the index in
        ;; `egraph-rebuild'
        `(dolist (,node-var ,(if lhs-bound-p
                                 `(gethash ,var (fsym-info-node-table ,fsym-info-var))
                                 `(fsym-info-nodes ,fsym-info-var)))
           (destructuring-bind ,lisp-arg-vars (cdr (enode-term ,node-var))
             (declare (ignorable ,@lisp-arg-vars))
             (when (and ,@ (mapcan (lambda (lisp-var var)
                                     (when (and (var-p var) (not (var-p lisp-var)))
                                       `((eq ,lisp-var ,var))))
                                   lisp-arg-vars arg-vars))
               ,(expand-match bound-vars rest cont-expr)))))
      cont-expr))

(defun expand-template (tmpl)
  "Generate code that creates an enode according to TMPL (rhs of rewrite rule)."
  (labels ((process (tmpl)
             (cond ((consp tmpl)
                    `(let ((list (list ',(car tmpl) ,@ (mapcar #'process (cdr tmpl)))))
                       (declare (dynamic-extent list))
                       (make-enode list)))
                   ((var-p tmpl) tmpl)
                   (t (process (list tmpl))))))
    (process tmpl)))

(defmacro do-matches ((top-node-var pat) &body body)
  "Evaluate BODY for every PAT match in EGRAPH.

BODY is evaluated with variables in PAT bound to matched eclasses and
TOP-NODE-VAR bound to the enode matching PAT."
  (if (var-p pat) ; Special case for single variable PAT that scans all enodes
      `(maphash-keys
        (lambda (,pat) (let ((,top-node-var ,pat)) ,@body))
        (egraph-classes *egraph*))
      (let* ((*fsym-info-var-alist* nil)
             (match-body
               (expand-match nil (parse-pattern pat top-node-var)
                             `(locally ,@body))))
        `(let ,(mapcar (lambda (kv) `(,(cdr kv)
                                      (ensure-gethash ',(car kv) (egraph-fsym-table *egraph*)
                                                      (make-fsym-info))))
                       *fsym-info-var-alist*)
           ,match-body))))

(defmacro defrw (name lhs rhs &key (guard t))
  "Define a rule that rewrites LHS to RHS when GUARD is evaluated to true."
  `(defun ,name ()
     (do-matches (top-node ,lhs)
       (when ,guard
         (enode-merge top-node ,(expand-template rhs))))))
