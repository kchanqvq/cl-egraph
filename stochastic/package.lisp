(uiop:define-package :ggs/stochastic
    (:use #:cl #:alexandria #:ggs/common)
  (:import-from #:serapeum #:eval-always)
  (:export #:stochastic-search #:define-tree-sum-cost #:define-problem))
