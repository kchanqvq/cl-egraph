(uiop:define-package :egraph
    (:use :cl :alexandria)
  (:import-from :serapeum #:lret #:lret* #:-> #:string-prefix-p)
  (:import-from :bind #:bind)
  (:export #:make-enode #:make-egraph #:enode-find #:list-enodes
           #:egraph-merge #:egraph-rebuild
           #:enode-representative-p #:enode-canonical-p #:check-egraph
           #:define-rewrite #:defrw #:make-term
           #:greedy-extract))

(in-package :egraph)

;;; E-graph data structure

(defstruct (eclass-info (:constructor %make-eclass-info))
  "Metadata for the eclass.

Should only appear in reprensetative enode's PARENT slot.

NODES and PARENTS only store canonical enodes after `egraph-rebuild'."
  (nodes nil :type list)
  (parents nil :type list)
  (n-parents 0 :type fixnum))

(declaim (type non-negative-fixnum *hash-code*))
(global-vars:define-global-var *hash-code* 0)

(defstruct (enode (:constructor %make-enode))
  "PARENT is either another enode in the same eclass, or an `eclass-info' if this
enode is the representative of its own eclass.

HASH-CODE-AND-FLAGS store 1 in the lowest bit if the node is marked as
non-canonical. `egraph-rebuild' trusts this information to avoid testing all
term arguments for representativeness. Higher bits store a unique hash code."
  (term) (parent)
  (hash-code-and-flags
   (prog1 (ash *hash-code* 1)
     (setf *hash-code*
           (logand (1+ *hash-code*) most-positive-fixnum)))
   :type (unsigned-byte 64)))

(defmethod print-object ((self enode) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~:[~;REP ~]~a" (enode-representative-p self) (enode-term self))))

(defun term-equal (x y)
  (loop
    (unless (eq (car x) (car y))
      (return-from term-equal nil))
    (setq x (cdr x) y (cdr y))
    (unless (or x y) (return)))
  t)

(defun term-hash (x)
  (let ((hash (sxhash (car x))))
    (declare (type non-negative-fixnum hash))
    (dolist (i (cdr x))
      ;; Copied sb-c::mix
      (let* ((mul (logand 3622009729038463111 most-positive-fixnum))
             (xor (logand 608948948376289905 most-positive-fixnum))
             (xy (logand (+ (* hash mul) (ash (enode-hash-code-and-flags i) -1))
                         most-positive-fixnum)))
        (setq hash (logand (logxor xor xy (ash xy -5)) most-positive-fixnum))))
    hash))

(defstruct fsym-info
  "Index data for specific function symbol.

NODES store all enodes with this function symbol.

NODE-TABLE maps eclasses (i.e. representative enodes) to member enodes with this
function symbol."
  (nodes nil :type list)
  (node-table (make-hash-table :test 'eq) :type hash-table))

(defstruct egraph
  "HASH-CONS stores all canonical enodes. CLASSES stores all
eclass (i.e. representative enodes). FSYM-TABLE stores a `fsym-info' entry for
every encountered function symbol.

CLASSES and FSYM-TABLE are only up-to-date after `egraph-rebuild'."
  (hash-cons (make-hash-table :test 'term-equal :hash-function #'term-hash)
   :type hash-table)
  (classes (make-hash-table :test 'eq) :type hash-table)
  (fsym-table (make-hash-table) :type hash-table)
  (work-list nil :type list))

(-> enode-find (enode) enode)
(defun enode-find (enode)
  (let ((enode enode)
        (parent (enode-parent enode)))
    (if (eclass-info-p parent)
        enode
        (loop
          (let ((grandparent (enode-parent parent)))
            (when (eclass-info-p grandparent)
              (return parent))
            (psetf (enode-parent enode) grandparent
                   parent grandparent
                   enode parent))))))

(defun list-enodes (enode)
  "List of enodes equivalent to ENODE.

Only contains canonical enodes after `egraph-rebuild'."
  (eclass-info-nodes (enode-parent (enode-find enode))))

(defun make-enode (egraph term)
  (let ((term (cons (car term) (mapcar #'enode-find (cdr term)))))
    (ensure-gethash
     term
     (egraph-hash-cons egraph)
     (lret* ((eclass-info (%make-eclass-info))
             (enode (%make-enode :parent eclass-info :term term)))
       (setf (eclass-info-nodes eclass-info) (list enode))
       (dolist (arg (cdr term))
         (push enode (eclass-info-parents (enode-parent arg)))
         (incf (eclass-info-n-parents (enode-parent arg))))))))

(-> egraph-merge (egraph enode enode) null)
(defun egraph-merge (egraph x y)
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
          (setf (enode-hash-code-and-flags parent)
                (logior (enode-hash-code-and-flags parent) 1))
          (push parent (egraph-work-list egraph)))
        (setf (eclass-info-nodes px)
              (nreconc (eclass-info-nodes py) (eclass-info-nodes px)))
        (remhash y (egraph-classes egraph))
        (setf (enode-parent y) x)
        nil))))

(declaim (inline enode-representative-p enode-canonical-p))

;; Be aware that representative enode might be non-canonical!
(defun enode-representative-p (enode)
  (eclass-info-p (enode-parent enode)))

(defun enode-canonical-p (enode)
  (every #'enode-representative-p (cdr (enode-term enode))))

(defun egraph-rebuild (egraph)
  ;; Upward propagation
  (loop
    (let ((enode (pop (egraph-work-list egraph))))
      (unless enode (return))
      (remhash (enode-term enode) (egraph-hash-cons egraph))
      (egraph-merge egraph (make-enode egraph (enode-term enode)) enode)))
  ;; Build eclass index by collecting all representative enodes of canonical
  ;; enodes in `egraph-hash-cons'. Note we really need to `enode-find' here,
  ;; because canon-enodes might be non-rep, while rep-enodes might not be canon
  ;; thus not appear in `egraph-hash-cons' either so we can't simply test for
  ;; `enode-representative-p'.
  (clrhash (egraph-classes egraph))
  (maphash
   (lambda (ignore node)
     (declare (ignore ignore))
     (setf (gethash (enode-find node) (egraph-classes egraph)) egraph))
   (egraph-hash-cons egraph))
  ;; Build various node index
  (clrhash (egraph-fsym-table egraph))
  (flet ((enode-canonical-p (enode)
           (zerop (logand (enode-hash-code-and-flags enode) 1))))
    (maphash
     (lambda (class ignore)
       (declare (ignore ignore))
       (let ((info (enode-parent class)))
         (setf (eclass-info-nodes info)
               (delete-if-not #'enode-canonical-p (eclass-info-nodes info))
               (eclass-info-parents info)
               (delete-if-not #'enode-canonical-p (eclass-info-parents info))
               (eclass-info-n-parents info)
               (length (eclass-info-parents info)))
         (dolist (node (eclass-info-nodes info))
           (let ((fsym-info (ensure-gethash (car (enode-term node)) (egraph-fsym-table egraph)
                                            (make-fsym-info))))
             (push node (gethash class (fsym-info-node-table fsym-info)))
             (push node (fsym-info-nodes fsym-info))))))
     (egraph-classes egraph))))

(defun check-egraph (egraph)
  "Various sanity check."
  (declare (optimize (debug 3)))
  (let ((classes (make-hash-table)))
    (dolist (enode (hash-table-values (egraph-hash-cons egraph)))
      (setf (gethash (enode-find enode) classes) t))
    (format t "~&There're ~a eclasses.~%" (hash-table-count classes))
    (when-let (diff (set-exclusive-or (hash-table-keys classes)
                                      (hash-table-keys (egraph-classes egraph))))
      (error "Difference between classes slot and actual eclasses:~% ~a" diff))
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
        (unless (= (length parents)
                   (length (remove-duplicates parents :test 'equal :key #'enode-term)))
          (warn "Duplicates in ~a's parent list:~% ~a" class parents))
        (dolist (node parents)
          (unless (enode-canonical-p node)
            (error "Non canonical ~a on ~a's parent list" node class))
          (unless (member class (cdr (enode-term node)))
            (error "Extra parent link from ~a to ~a" class node)))))))

;;; E-match/rewrite compiler

(defun var-p (object)
  (typecase object
    (symbol (string-prefix-p "?" (symbol-name object)))))

(defvar *fsym-info-var-alist*)

(defun parse-pattern (pat eclass-var)
  "Convert PAT into a list of the form ((eclass-var fsym arg-var...) ...)."
  (cond ((consp pat)
         (let* ((fsym (car pat))
                (arg-vars (mapcar (lambda (arg) (if (var-p arg) arg (make-gensym fsym))) (cdr pat))))
           (cons (list* eclass-var fsym arg-vars)
                 (mapcan (lambda (arg var)
                           (unless (var-p arg)
                             (parse-pattern arg var)))
                         (cdr pat) arg-vars))))
        ((var-p pat) (error "Single variable pattern not implemented."))
        (t ;; Non-variable atoms are short hand for 0-arity function symbol
         (parse-pattern (list pat) eclass-var))))

(defun expand-match (bound-vars subst-alist cont-expr)
  "Generate code that solves for SUBST-ALIST (as returned by `parse-pattern' then
evaluate CONT-EXPR."
  (if subst-alist
      (bind ((((var fsym . arg-vars) . rest) subst-alist))
        (let ((lisp-arg-vars (mapcar (lambda (var)
                                       (if (member var bound-vars)
                                           (make-gensym fsym)
                                           (progn (push var bound-vars) var)))
                                     arg-vars))
              (fsym-info-var (serapeum:ensure (assoc-value *fsym-info-var-alist* fsym)
                               (make-gensym fsym)))
              (node-var (if (member var bound-vars) (make-gensym 'node) var)))
          ;; Currently we use indexes (in `fsym-info') as single source of truth
          ;; for matching, thus no-need to `enode-find' representative of VAR
          ;; (even if VAR is non-rep, it once was when we built the index in
          ;; `egraph-rebuild'
          `(dolist (,node-var ,(if (member var bound-vars)
                                   `(gethash ,var (fsym-info-node-table ,fsym-info-var))
                                   `(fsym-info-nodes ,fsym-info-var)))
             (destructuring-bind ,lisp-arg-vars (cdr (enode-term ,node-var))
               (declare (ignorable ,@lisp-arg-vars))
               (when (and ,@ (mapcan (lambda (lisp-var var)
                                       (when (and (var-p var) (not (var-p lisp-var)))
                                         `((eql ,lisp-var ,var))))
                                     lisp-arg-vars arg-vars))
                 ,(expand-match (union arg-vars bound-vars) rest cont-expr))))))
      cont-expr))

(defun expand-template (tmpl egraph-var)
  "Generate code that creates an enode according to TMPL (rhs of rewrite rule) in
EGRAPH-VAR."
  (labels ((process (tmpl)
             (cond ((consp tmpl)
                    `(let ((list (list ',(car tmpl) ,@ (mapcar #'process (cdr tmpl)))))
                       (declare (dynamic-extent list))
                       (make-enode ,egraph-var list)))
                   ((var-p tmpl) tmpl)
                   (t (process (list tmpl))))))
    (process tmpl)))

(defmacro define-rewrite (name egraph-var pat &body body)
  "Define a rewrite rule function with NAME.

The function accepts an argument and bind to EGRAPH-VAR, then match for PAT, and
for each match evaluate BODY under the variable substitution. BODY should
evaluate to a enode created in EGRAPH-VAR, which is merged with the matched
enode."
  (bind ((*fsym-info-var-alist* nil)
         (match-body
          (expand-match nil (parse-pattern pat 'top-node)
                        `(egraph-merge ,egraph-var top-node (locally ,@body)))))
    `(defun ,name (,egraph-var)
       (let ,(mapcar (lambda (kv) `(,(cdr kv)
                                    (ensure-gethash ',(car kv) (egraph-fsym-table ,egraph-var)
                                                    (make-fsym-info))))
                     *fsym-info-var-alist*)
         ,match-body))))

(defmacro defrw (name lhs rhs &key (guard t))
  "Define a rule that rewrites LHS to RHS when GUARD is evaluated to true."
  `(define-rewrite ,name egraph ,lhs
     (when ,guard ,(expand-template rhs 'egraph))))

;;; Utils

(defun make-term (egraph term)
  (labels ((process (term)
             (if (consp term)
                 (make-enode egraph (cons (car term) (mapcar #'process (cdr term))))
                 (make-enode egraph (list term)))))
    (process term)))

;;; Extract

(defun greedy-extract (egraph enode cost-fn)
  (let ((selections (make-hash-table))) ;; map eclass to (cost . enode)
    (loop
      (let (dirty)
        (maphash
         (lambda (class ignore)
           (declare (ignore ignore))
           (let ((selection (gethash class selections)))
             (dolist (enode (list-enodes class))
               (let* ((term (enode-term enode))
                      (new-cost
                        (funcall cost-fn (car term)
                                 (mapcar (compose #'car (rcurry #'gethash selections))
                                         (cdr term)))))
                 (when (if selection (and new-cost (< new-cost (car selection)))
                           new-cost)
                   (setq selection (cons new-cost enode)
                         dirty t))))
             (setf (gethash class selections) selection)))
         (egraph-classes egraph))
        (unless dirty (return))))
    (labels ((build-term (class)
               (let ((term (enode-term (cdr (gethash class selections)))))
                 (if (cdr term)
                     (cons (car term) (mapcar #'build-term (cdr term)))
                     (car term)))))
      (build-term (enode-find enode)))))
