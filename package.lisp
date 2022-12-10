(in-package #:cl-user)

(uiop:define-package #:nx-router
  (:use #:cl)
  (:nicknames #:router)
  (:import-from #:nyxt
                #:define-class
                #:user-class
                #:define-mode
                #:define-command-global
                #:current-buffer
                #:url
                #:buffer
                #:render-url)
  (:import-from #:serapeum
                #:->
                #:export-always)
  (:documentation "nx-router allows you to define composable and flexible routes for Nyxt."))
