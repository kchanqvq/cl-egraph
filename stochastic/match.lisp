(in-package :egraph)

(defmacro do-term ((subterm-var term) &body body)
  (with-gensyms (process)
    `(labels ((,process (,subterm-var)
                ,@body
                (when (consp ,subterm-var)
                  (mapc #',process (cdr ,subterm-var)))))
       (,process ,term))))

(defvar *term*)

(declaim (type function *term-normalizer*))
(defvar *term-normalizer* #'list)

(defun expand-term-match (bound-vars subst-alist cont-expr)
  (if subst-alist
      (bind ((((var fsym . arg-vars) . rest) subst-alist)
             ((:flet lisp-var (var))
              (if (member var bound-vars)
                  (gensym-1 fsym)
                  (progn (push var bound-vars) var)))
             (lisp-arg-vars (mapcar #'lisp-var arg-vars))
             (lhs-bound-p (member var bound-vars))
             (term-var (lisp-var var)))
        `(,@(if lhs-bound-p
                `(let ((,term-var ,var)))
                `(do-term (,term-var *term*)))
          ,(if arg-vars
               `(when (and (consp ,term-var)
                           (eq (car ,term-var) ',fsym))
                  (destructuring-bind ,lisp-arg-vars (cdr ,term-var)
                    (declare (ignorable ,@lisp-arg-vars))
                    (when (and ,@ (mapcan (lambda (lisp-var var)
                                            (when (and (var-p var) (not (var-p lisp-var)))
                                              `((equal ,lisp-var ,var))))
                                          lisp-arg-vars arg-vars))
                      ,(expand-term-match bound-vars rest cont-expr))))
               `(when (eq ,term-var ',fsym)
                  ,(expand-term-match bound-vars rest cont-expr)))))
      cont-expr))

(defun expand-term-template (tmpl)
  (labels ((process (tmpl)
             (cond ((consp tmpl)
                    `(funcall *term-normalizer* ',(car tmpl)
                              ,@ (mapcar #'process (cdr tmpl))))
                   ((var-p tmpl) tmpl)
                   (t `',tmpl))))
    (process tmpl)))

(defmacro do-term-matches ((top-term-var pat) &body body)
  (if (var-p pat) ; Special case for single variable PAT that scans all enodes
      `(do-term (,pat *term*)
         (let ((,top-term-var ,pat))
           (declare (ignorable ,top-term-var))
           ,@body))
      (expand-term-match nil (parse-pattern pat top-term-var)
                         `(locally ,@body))))


