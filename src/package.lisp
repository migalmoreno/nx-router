(uiop:define-package #:nx-mapper
  (:nicknames #:mapper)
  (:use #:cl)
  (:import-from #:nyxt
                #:define-mode
                #:define-class
                #:define-user-class
                #:define-command-global)
  (:documentation "nx-mapper provides a general mode to map entities into triggers which will invoke them.
Currently, supported entities include themes and URL associations."))

(in-package #:nx-mapper)
(nyxt:use-nyxt-package-nicknames)

(define-class mapping ()
  ((name
    ""
    :type string
    :documentation "The name of the mapping."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

;; WIP: Add a global settings class that smaller extensions can inherit from to define
;; their own settings or the other way round, i.e. a global settings class which inherits from all other classes?

(sera:export-always '*user-settings*)
(defparameter *user-settings* nil
  "The global user `nx-mapper' user settings.")

(defun list-of-lists-p (object)
  "Returns non-nil of OBJECT consists of a list of lists."
  (and (listp object)
       (every #'listp object)))
