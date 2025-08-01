(asdf:defsystem #:egraph
  :description "E-graph engine"
  :author "Qiantan Hong <qthong@stanford.edu>"
  :license "MIT License"
  :serial t
  :depends-on (:alexandria
               :serapeum
               :iterate)
  :components ((:file "egraph")))

(asdf:defsystem #:egraph/tests
  :serial t
  :depends-on (:egraph
               :fiveam)
  :components ((:file "egraph-tests")))
