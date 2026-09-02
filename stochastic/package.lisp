(uiop:define-package :ggs/stochastic
    (:use #:cl #:alexandria #:ggs/common)
  (:import-from #:serapeum #:eval-always #:collecting #:partition)
  (:export #:stochastic-search #:define-tree-sum-cost #:defrw #:defrw* #:precompile-rule-set))
