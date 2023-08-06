(defsystem "nx-router"
  :description "nx-router is a declarative URL routing extension for Nyxt"
  :author "Miguel Ángel Moreno"
  :license "BSD 3-Clause"
  :version "0.3.0"
  :serial t
  :depends-on ("nyxt")
  :components ((:file "package")
               (:file "instances")
               (:file "router"))
  :in-order-to ((test-op (test-op "nx-router/tests"))))

(defsystem "nx-router/tests"
  :depends-on ("nx-router" "lisp-unit2")
  :serial t
  :pathname "tests/"
  :components ((:file "package")
               (:file "router"))
  :perform (test-op (o s)
                    (uiop:symbol-call :lisp-unit2 :run-tests
                                      :package :nx-router/tests
                                      :run-contexts (find-symbol "WITH-SUMMARY-CONTEXT" :lisp-unit2))))
