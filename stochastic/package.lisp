(uiop:define-package :ggs/stochastic
    (:use #:cl #:alexandria #:ggs/common)
  (:export #:rose-node #:*term-normalizer* #:do-term-matches #:stochastic-search
           #:defrw*))
