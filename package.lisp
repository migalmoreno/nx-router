(uiop:define-package #:nx-router
  (:nicknames #:router)
  (:use #:cl)
  (:import-from #:nyxt
                #:define-class
                #:user-class
                #:define-mode
                #:define-command-global
                #:current-buffer
                #:url
                #:buffer)
  (:documentation "nx-router allows you to define composable and flexible routes for Nyxt."))