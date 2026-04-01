(asdf:defsystem #:egraph
  :description "E-graph engine"
  :author "Qiantan Hong <qthong@stanford.edu>"
  :license "MIT License"
  :serial t
  :depends-on (:alexandria
               :serapeum
               :metabang-bind
               :trivia
               :cl-custom-hash-table
               :global-vars
               :float-features
               :trivial-garbage
               :trivial-package-local-nicknames
               :linear-programming)
  :components ((:file "package")
               (:file "egraph")
               (:file "match")
               (:file "run")
               (:file "extract")
               (:file "user"))
  :in-order-to ((test-op (test-op "egraph/tests"))))

(asdf:defsystem #:egraph/tests
  :serial t
  :depends-on (:egraph
               :fiveam
               :trivial-benchmark)
  :components ((:module "tests"
                :components ((:file "simple")
                             (:file "math")
                             (:file "matmul"))))
  :perform (test-op (o c)
                    (symbol-call :fiveam '#:run! :egraph)
                    (symbol-call :fiveam '#:run! :egraph/bench)))
