(defsystem #:nx-router
  :description "nx-router allows you to define composable and flexible routes for Nyxt."
  :author "efimerspan"
  :license "BSD 3-Clause"
  :version "0.0.1"
  :serial t
  :depends-on (#:nyxt)
  :components ((:file "package")
               (:file "router")))
