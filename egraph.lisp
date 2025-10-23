(uiop:define-package :egraph
    (:use #:cl #:alexandria)
  (:import-from #:serapeum #:lret #:lret* #:-> #:string-prefix-p)
  (:import-from #:bind #:bind)
  (:export #:make-enode #:intern-enode #:enode-term #:make-egraph #:egraph #:list-enodes
           #:*egraph* #:enode-find #:enode-merge #:egraph-rebuild
           #:enode-representative-p #:enode-canonical-p #:check-egraph
           #:egraph-n-enodes #:egraph-n-eclasses
           #:do-matches #:defrw #:intern-term #:run-rewrites
           #:make-analysis-info #:get-analysis-data
           #:greedy-extract #:cost))

(in-package :egraph)

;;; E-graph data structure

(defclass eclass-info ()
  ((nodes :initform nil :type list :accessor eclass-info-nodes)
   (parents :initform nil :type list :accessor eclass-info-parents)
   (n-parents :initform 0 :type fixnum :accessor eclass-info-n-parents))
  (:documentation
   "Metadata for the eclass.

Should only appear in reprensetative enode's PARENT slot.

NODES and PARENTS only store canonical enodes after `egraph-rebuild'."))

(defmethod initialize-instance :after ((instance eclass-info) &key node)
  (setf (eclass-info-nodes instance) (list node)))

(declaim (type non-negative-fixnum *hash-code*))
(global-vars:define-global-var *hash-code* 0)

(defstruct (enode (:constructor %make-enode))
  "PARENT is either another enode in the same eclass, or an `eclass-info' if this
enode is the representative of its own eclass.

Set CANONICAL-FLAG to NIL to mark the node as non-canonical. `egraph-rebuild'
trusts this information to avoid testing all term arguments for
representativeness."
  (term) (parent)
  (hash-code
   (setf *hash-code*
         (logand (1+ *hash-code*) most-positive-fixnum))
   :type fixnum)
  (canonical-flag t :type boolean))

(defmethod print-object ((self enode) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~:[~;REP ~]~a" (enode-representative-p self) (enode-term self))))

(defun term-equal (x y)
  (unless (eql (car x) (car y))
    (return-from term-equal nil))
  (setq x (cdr x) y (cdr y))
  (loop
    (unless (or x y) (return))
    (unless (eq (car x) (car y))
      (return-from term-equal nil))
    (setq x (cdr x) y (cdr y)))
  t)

(defun term-hash (x)
  (let ((hash (sxhash (car x))))
    (declare (type non-negative-fixnum hash))
    (dolist (i (cdr x))
      ;; Copied sb-c::mix
      (let* ((mul (logand 3622009729038463111 most-positive-fixnum))
             (xor (logand 608948948376289905 most-positive-fixnum))
             (xy (logand (+ (* hash mul) (enode-hash-code i)) most-positive-fixnum)))
        (setq hash (logand (logxor xor xy (ash xy -5)) most-positive-fixnum))))
    hash))

(cl-custom-hash-table:define-custom-hash-table-constructor make-hash-cons
  :test term-equal :hash-function term-hash)

(defstruct fsym-info
  "Index data for specific function symbol.

NODES store all enodes with this function symbol.

NODE-TABLE maps eclasses (i.e. representative enodes) to member enodes with this
function symbol."
  (nodes nil :type list)
  (node-table (make-hash-table :test 'eq) :type hash-table))

(defclass egraph ()
  ((hash-cons :initform (make-hash-cons) :type hash-table :accessor egraph-hash-cons)
   (classes :initform (make-hash-table :test 'eq) :type egraph-hash-table :accessor egraph-classes)
   (fsym-table :initform (make-hash-table) :type hash-table :accessor egraph-fsym-table)
   (work-list :initform nil :type list :accessor egraph-work-list)
   (analysis-work-list :initform nil :type list :accessor egraph-analysis-work-list))
  (:documentation
   "HASH-CONS stores all canonical enodes. CLASSES stores all
   eclass (i.e. representative enodes). FSYM-TABLE stores a `fsym-info' entry for
   every encountered function symbol.

   CLASSES and FSYM-TABLE are only up-to-date after `egraph-rebuild'."))

(defmethod make-eclass-info ((egraph egraph) &key node)
  (make-instance 'eclass-info :node node))

(defun make-egraph ()
  (make-instance 'egraph))

(defvar *egraph*)
(setf (documentation '*egraph* 'variable) "Current egraph under operation.")

(-> enode-find (enode) enode)
(defun enode-find (enode)
  (let ((enode enode)
        (parent (enode-parent enode)))
    (if (enode-p parent)
        (loop
          (let ((grandparent (enode-parent parent)))
            (unless (enode-p grandparent)
              (return parent))
            (psetf (enode-parent enode) grandparent
                   parent grandparent
                   enode parent)))
        enode)))

(defun list-enodes (enode)
  "List of enodes equivalent to ENODE.

Only contains canonical enodes after `egraph-rebuild'."
  (eclass-info-nodes (enode-parent (enode-find enode))))

(defgeneric make-eclass-info (egraph &key node))

(-> make-enode (list) enode)
(defun make-enode (term)
  (let ((term (cons (car term) (mapcar #'enode-find (cdr term)))))
    (or (gethash term (egraph-hash-cons *egraph*))
        (lret ((enode (%make-enode :term term)))
          (setf (enode-parent enode)
                (make-eclass-info *egraph* :node enode))
          (dolist (arg (cdr term))
            (push enode (eclass-info-parents (enode-parent arg)))
            (incf (eclass-info-n-parents (enode-parent arg))))
          (setf (gethash term (egraph-hash-cons *egraph*)) enode)
          (update-eclass-info (enode-parent enode) enode)))))

(-> intern-enode (enode) enode)
(defun intern-enode (enode)
  (labels ((process (enode)
             (let ((term (enode-term enode)))
               (unless (gethash term (egraph-hash-cons *egraph*))
                 (setf (gethash term (egraph-hash-cons *egraph*)) enode)
                 (mapc #'process (cdr term))))))
    (process enode)
    enode))

(defmacro sor (&rest clauses)
  `(let (flag)
     ,@ (mapcar (lambda (clause) `(setq flag (or flag ,clause))) clauses)
     flag))

(define-method-combination sor :operator sor)

(defgeneric merge-eclass-info (dst src)
  (:method-combination sor)
  (:method sor ((dst eclass-info) src)
    (setf (eclass-info-nodes dst)
          (nreconc (eclass-info-nodes src) (eclass-info-nodes dst)))
    nil))

(defgeneric update-eclass-info (eclass-info enode)
  (:method-combination sor)
  (:method sor ((self eclass-info) (enode t))
    nil))

(-> enode-merge (enode enode) null)
(defun enode-merge (x y)
  (let ((x (enode-find x))
        (y (enode-find y)))
    (unless (eq x y)
      (let ((px (enode-parent x))
            (py (enode-parent y)))
        (when (< (eclass-info-n-parents px)
                 (eclass-info-n-parents py))
          (rotatef x y)
          (rotatef px py))
        (dolist (parent (eclass-info-parents py))
          (setf (enode-canonical-flag parent) nil)
          (push parent (egraph-work-list *egraph*)))
        (setf (enode-parent y) x)
        (when (merge-eclass-info px py)
          (push px (egraph-analysis-work-list *egraph*)))
        nil))))

(declaim (inline enode-representative-p enode-canonical-p))

;; Be aware that representative enode might be non-canonical!
(defun enode-representative-p (enode)
  (not (enode-p (enode-parent enode))))

(defun enode-canonical-p (enode)
  (every #'enode-representative-p (cdr (enode-term enode))))

(defun egraph-rebuild ()
  ;; Upward propagation

  ;; Note: we allow duplicates in `egraph-work-list'. Currently we don't bother
  ;; `remove-duplicate' beforehand because doing such seem to actually slow
  ;; things down a bit.
  (loop
    (let ((enode (pop (egraph-work-list *egraph*))))
      (unless enode (return))
      (remhash (enode-term enode) (egraph-hash-cons *egraph*))
      (enode-merge (intern-enode (make-enode (enode-term enode))) enode)))
  ;; Update analysis
  (loop
    (let ((info (pop (egraph-analysis-work-list *egraph*))))
      (unless info (return))
      (unless (enode-p info)
        (dolist (parent (eclass-info-parents info))
          (when (enode-canonical-flag parent)
            (let* ((eclass (enode-find parent))
                   (info (enode-parent eclass)))
              (when (update-eclass-info info parent)
                (push eclass (egraph-analysis-work-list *egraph*)))))))))
  ;; Build eclass index by collecting all representative enodes of canonical
  ;; enodes in `egraph-hash-cons'. Note we really need to `enode-find' here,
  ;; because canon-enodes might be non-rep, while rep-enodes might not be canon
  ;; thus not appear in `egraph-hash-cons' either so we can't simply test for
  ;; `enode-representative-p'.
  (clrhash (egraph-classes *egraph*))
  (maphash-values (lambda (node)
                    (setf (gethash (enode-find node) (egraph-classes *egraph*)) t))
                  (egraph-hash-cons *egraph*))
  ;; Build various node index
  (clrhash (egraph-fsym-table *egraph*))
  (maphash-keys (lambda (class)
                  (let ((info (enode-parent class)))
                    (setf (eclass-info-nodes info)
                          (delete-if-not #'enode-canonical-flag (eclass-info-nodes info))
                          (eclass-info-parents info)
                          (delete-if-not #'enode-canonical-flag (eclass-info-parents info))
                          (eclass-info-n-parents info)
                          (length (eclass-info-parents info)))
                    (dolist (node (eclass-info-nodes info))
                      (let ((fsym-info (ensure-gethash (car (enode-term node)) (egraph-fsym-table *egraph*)
                                                       (make-fsym-info))))
                        (push node (gethash class (fsym-info-node-table fsym-info)))
                        (push node (fsym-info-nodes fsym-info))))))
                (egraph-classes *egraph*)))

(defun hash-table-keys-difference (table-1 table-2)
  (let ((results nil))
    (maphash-keys (lambda (class)
                    (unless (gethash class table-2)
                      (push class results)))
                  table-1)
    results))

(defun check-egraph ()
  "Various sanity check."
  (declare (optimize (debug 3)))
  (let ((classes (make-hash-table))
        (n-parent-list 0)
        (n-parent-list-distinct 0))
    (dolist (enode (hash-table-values (egraph-hash-cons *egraph*)))
      (setf (gethash (enode-find enode) classes) t))
    (format t "~&There're ~a eclasses.~%" (hash-table-count classes))
    (when-let (diff (hash-table-keys-difference classes (egraph-classes *egraph*)))
      (error "Missing eclasses:~% ~a" diff))
    (when-let (diff (hash-table-keys-difference (egraph-classes *egraph*) classes))
      (error "Extra eclasses:~% ~a" diff))
    (dolist (class (hash-table-keys classes))
      (let ((nodes (list-enodes class)))
        (dolist (node nodes)
          (if (enode-canonical-p node)
              (dolist (arg (cdr (enode-term node)))
                (unless (member node (eclass-info-parents (enode-parent arg)))
                  (error "Missing parent link from ~a to ~a" arg node)))
              (error "Non canonical node ~a on ~a's circular node list" node class)))
        (unless (= (length nodes)
                   (length (remove-duplicates nodes :test 'equal :key #'enode-term)))
          (warn "Duplicates in ~a's enodes:~% ~a" class nodes)))
      (let ((parents (eclass-info-parents (enode-parent class))))
        (dolist (node parents)
          (unless (enode-canonical-p node)
            (error "Non canonical ~a on ~a's parent list" node class))
          (unless (member class (cdr (enode-term node)))
            (error "Extra parent link from ~a to ~a" class node)))
        ;; Currently we allow duplicates in parent list
        (incf n-parent-list (length parents))
        (incf n-parent-list-distinct (length (remove-duplicates parents)))))
    (let ((n-dup (- n-parent-list n-parent-list-distinct)))
      (unless (zerop n-dup)
        (format t "~a/~a (~,2$%) elements in parent list are duplicates.~%"
                n-dup n-parent-list (* 100 (/ n-dup n-parent-list)))))))

;;; E-match/rewrite compiler

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
      `(maphash-values
        (lambda (fsym-info)
          (dolist (,pat (fsym-info-nodes fsym-info))
            (let ((,top-node-var ,pat)) ,@body)))
        (egraph-fsym-table *egraph*))
      (let* ((*fsym-info-var-alist* nil)
             (match-body
               (expand-match nil (parse-pattern pat top-node-var)
                             `(locally ,@body))))
        `(let ,(mapcar (lambda (kv) `(,(cdr kv)
                                      (ensure-gethash ',(car kv) (egraph-fsym-table *egraph*)
                                                      (make-fsym-info))))
                       *fsym-info-var-alist*)
           ,match-body))))

(declaim (type function *apply-hook*))
(defvar *apply-hook* #'enode-merge
  "Function to apply a rewrite, receives two argument: LHS enode and (not yet
interned) RHS enode.")

(defmacro defrw (name lhs rhs &key (guard t))
  "Define a rule that rewrites LHS to RHS when GUARD is evaluated to true."
  `(defun ,name ()
     (do-matches (top-node ,lhs)
       (when ,guard
         (funcall *apply-hook* top-node (intern-enode ,(expand-template rhs)))))))

;;; Utils

(defun intern-term (term)
  (if (consp term)
      (intern-enode (make-enode (cons (car term) (mapcar #'intern-term (cdr term)))))
      (intern-enode (make-enode (list term)))))

(declaim (inline egraph-n-enodes egraph-n-eclasses))

(defun egraph-n-enodes (egraph)
  (hash-table-count (egraph-hash-cons egraph)))

(defun egraph-n-eclasses (egraph)
  (hash-table-count (egraph-classes egraph)))

(defun run-rewrites (rules &key max-iter check max-enodes)
  "Run RULES repeatly on `*egraph*' until some stop criterion.

Returns the reason for termination: one of :max-iter, :max-enodes, :saturate.

Note: this function does not call `egraph-rebuild' upfront. Particularly, if you
have added some terms to EGRAPH, you MUST call `egraph-rebuild' before calling
this function."
  (let ((n-enodes (egraph-n-enodes *egraph*))
        (n-eclasses (egraph-n-eclasses *egraph*))
        (n-iter 0)
        (*apply-hook* *apply-hook*))
    (when max-enodes
      (setq *apply-hook*
            (let ((old-hook *apply-hook*))
              (lambda (lhs rhs)
                (if (>= (egraph-n-enodes *egraph*) max-enodes)
                    (throw 'stop :max-enodes)
                    (funcall old-hook lhs rhs))))))
    (catch 'stop
      (loop
        (when (and max-iter (>= n-iter max-iter))
          (throw 'stop :max-iter))
        (when (and max-enodes (>= n-enodes max-enodes))
          (throw 'stop :max-enodes))
        (unwind-protect
             (dolist (rule (ensure-list rules))
               (funcall rule))
          (egraph-rebuild))
        (when check (check-egraph))
        (incf n-iter)
        (let ((n-enodes-1 (egraph-n-enodes *egraph*))
              (n-eclasses-1 (egraph-n-eclasses *egraph*)))
          (if (and (= n-enodes n-enodes-1) (= n-eclasses n-eclasses-1))
              (return :saturate)
              (setq n-enodes n-enodes-1 n-eclasses n-eclasses-1)))))))

;;; Extract

(defun greedy-extract (enode cost-fn)
  "Greedy extract a term for ENODE from `*egraph*' using COST-FN.

COST-FN should accept 2 arguments: the enode and a list of costs for each
argument eclass. It should return a number.

ENODE can also be a list of enodes, and a list of terms will be returned."
  (let ((selections (make-hash-table))) ;; map eclass to (cost . enode)
    (loop
      (let (dirty)
        (maphash-keys (lambda (class)
                        (let ((selection (gethash class selections)))
                          (dolist (enode (list-enodes class))
                            (let* ((term (enode-term enode))
                                   (new-cost
                                     (funcall cost-fn enode
                                              (mapcar (compose #'car (rcurry #'gethash selections))
                                                      (cdr term)))))
                              (when (if selection (and new-cost (< new-cost (car selection)))
                                        new-cost)
                                (setq selection (cons new-cost enode)
                                      dirty t))))
                          (setf (gethash class selections) selection)))
                      (egraph-classes *egraph*))
        (unless dirty (return))))
    (labels ((build-term (class)
               (let ((term (enode-term (cdr (gethash class selections)))))
                 (if (cdr term)
                     (cons (car term) (mapcar #'build-term (cdr term)))
                     (car term)))))
      (if (listp enode)
          (mapcar (compose #'build-term #'enode-find) enode)
          (build-term (enode-find enode))))))
