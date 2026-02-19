(uiop:define-package :egraph
    (:use #:cl #:alexandria)
  (:import-from #:serapeum #:lret #:lret* #:-> #:string-prefix-p)
  (:import-from #:bind #:bind)
  (:export #:make-enode #:enode-term #:make-egraph #:list-enodes
           #:enode-representative-p #:enode-canonical-p #:enode-eclass-info
           #:*egraph* #:enode-find #:enode-merge #:egraph-rebuild #:check-egraph
           #:egraph-n-enodes #:egraph-n-eclasses #:orp #:make-orp
           #:do-matches #:defrw #:make-term #:run-rewrites
           #:define-analysis #:get-analysis-data
           #:build-term #:graph-cost #:tree-cost
           #:greedy-select #:greedy-extract #:lp-select #:lp-extract))

(serapeum:eval-always
  (trivial-package-local-nicknames:add-package-local-nickname
   '#:lp '#:linear-programming '#:egraph))
