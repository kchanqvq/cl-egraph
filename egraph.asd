(asdf:defsystem #:egraph
  :description "E-graph engine"
  :author "Qiantan Hong <qthong@stanford.edu>"
  :license "MIT License"
  :serial t
  :depends-on (:alexandria
               :serapeum
               :cl-custom-hash-table
               :global-vars)
  :components ((:file "egraph"))
  :in-order-to ((test-op (test-op "egraph/tests"))))

(asdf:defsystem #:egraph/tests
  :serial t
  :depends-on (:egraph
               :fiveam)
  :components ((:file "egraph-tests"))
  :perform (test-op (o c) (symbol-call :fiveam '#:run! :egraph)))
