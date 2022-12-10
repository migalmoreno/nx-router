(defsystem #:nx-router
  :description "nx-router allows you to define composable and flexible routes for Nyxt."
  :author "conses"
  :license "BSD 3-Clause"
  :version "0.0.1"
  :serial t
  :depends-on (nyxt)
  :components ((:file "package")
               (:file "router"))
  :in-order-to ((test-op (test-op "nx-router/tests"))))

(defsystem #:nx-router/tests
  :depends-on (nx-router lisp-unit2)
  :components ((:file "tests/router")))
