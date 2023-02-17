(in-package #:nx-router)
(nyxt:use-nyxt-package-nicknames)

(define-class instances-builder ()
  ((source
    nil
    :type (or quri:uri string null))
   (builder
    nil
    :type (or function symbol null)))
  (:export-class-name-p t)
  (:export-slot-names-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "An instances builder for an alternative front-end."))

(export-always 'libredirect-instances-source)
(defvar libredirect-instances-source
  "https://raw.githubusercontent.com/libredirect/instances/main/data.json")

(export-always 'libredirect-instances-builder)
(defun libredirect-instances-builder (service)
  "Return an instances builder for SERVICE in Libredirect's generated list
of instances."
  (lambda (instances)
    (rest (alex:assoc-value
           (alex:assoc-value
            (json:with-decoder-simple-list-semantics
              (json:decode-json-from-string instances))
            service)
           :clearnet))))

(export-always 'proxitok-instances-builder)
(defvar proxitok-instances-builder
  (make-instance
   'instances-builder
   :source libredirect-instances-source
   :builder (libredirect-instances-builder :proxi-tok)))

(export-always 'whoogle-instances-builder)
(defvar whoogle-instances-builder
  (make-instance
   'instances-builder
   :source libredirect-instances-source
   :builder (libredirect-instances-builder :whoogle)))

(export-always 'breezewiki-instances-builder)
(defvar breezewiki-instances-builder
  (make-instance
   'instances-builder
   :source libredirect-instances-source
   :builder (libredirect-instances-builder :breeze-wiki)))

(export-always 'invidious-instances-builder)
(defvar invidious-instances-builder
  (make-instance
   'instances-builder
   :source "https://api.invidious.io/instances.json"
   :builder
   (lambda (instances)
     (mapcar 'first
             (json:with-decoder-simple-list-semantics
               (json:decode-json-from-string instances))))))

(export-always 'scribe-instances-builder)
(defvar scribe-instances-builder
  (make-instance
   'instances-builder
   :source
   "https://git.sr.ht/~edwardloveall/scribe/blob/main/docs/instances.json"
   :builder
   (lambda (instances)
     (json:decode-json-from-string instances))))

(export-always 'teddit-instances-builder)
(defvar teddit-instances-builder
  (make-instance
   'instances-builder
   :source
   "https://codeberg.org/teddit/teddit/raw/branch/main/instances.json"
   :builder
   (lambda (instances)
     (mapcar (lambda (instance)
               (unless (str:emptyp (alex:assoc-value instance :url))
                 (alex:assoc-value instance :url)))
             (json:with-decoder-simple-list-semantics
               (json:decode-json-from-string instances))))))

(export-always 'libreddit-instances-builder)
(defvar libreddit-instances-builder
  (make-instance
   'instances-builder
   :source
   "https://raw.githubusercontent.com/libreddit/libreddit-instances/master/instances.json"
   :builder
   (lambda (instances)
     (mapcar (lambda (instance)
               (unless (str:emptyp (alex:assoc-value instance :url))
                 (alex:assoc-value instance :url)))
             (alex:assoc-value
              (json:with-decoder-simple-list-semantics
                (json:decode-json-from-string instances))
              :instances)))))

(defun fetch-instances (url)
  (handler-case (dex:get url)
    (usocket:ns-host-not-found-error ()
      (nyxt:echo-warning
       "There's no Internet connection to retrieve instances")
      nil)))

(defgeneric build-instances (instances-builder)
  (:documentation "Build a list of instances from INSTANCES-BUILDER."))

(defmethod build-instances ((instances-builder instances-builder))
  (alex:when-let ((instances (fetch-instances (source instances-builder))))
    (delete nil (funcall (builder instances-builder) instances))))
