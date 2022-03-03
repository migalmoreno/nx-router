(asdf:defsystem #:nx-mapper
  :description "nx-mapper provides easy-to-define mappings for the Nyxt browser."
  :author "franesio"
  :license  "GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (#:nyxt)
  :pathname "src/"
  :components ((:file "package")
               (:file "stylor")
               (:file "rural")
               (:file "settings")
               (:file "mapper")))
