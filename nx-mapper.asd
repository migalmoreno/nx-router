(asdf:defsystem #:nx-mapper
  :description "nx-mapper provides easy-to-define mappings for the Nyxt browser."
  :author "efimerspan"
  :license  "BSD 3-Clause"
  :version "0.0.1"
  :serial t
  :depends-on (#:nyxt)
  :pathname "src/"
  :components ((:file "package")
               (:file "stylor")
               (:file "rural")
               (:file "settings")
               (:file "mapper")))
