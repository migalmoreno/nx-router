(uiop:define-package #:nx-mapper/rural-mode
  (:use #:cl)
  (:import-from #:nyxt
                #:define-class
                #:define-user-class
                #:define-mode
                #:define-command-global
                #:current-buffer
                #:url)
  (:documentation "Provides composable, easy-to-define, and flexible URL mappings for Nyxt."))

(in-package #:nx-mapper/rural-mode)
(nyxt:use-nyxt-package-nicknames)

(define-class settings ()
  ((active-url-mapping
    nil
    :type (or null url-mapping)
    :documentation "`url-mapping' currently active in the browser.")
   (url-mappings
    '()
    :type list
    :documentation "List of URL mappings that predicates are to be matched against the user's buffers."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))
(define-user-class settings)


;; If instances-fn is provided as a function, it can be too computing expensive and it won't get performed
;; at the right time to be added to the URL mapping sources, needing for the mode to be re-invoked
(define-class url-mapping (nx-mapper:mapping)
  ((source
    '()
    :type (list function)
    :documentation "Source where this mapping should be applied to.")
   (redirect
    nil
    :type (or list quri:uri string null)
    :documentation "Main redirect URL to be used or, when :path is given, a list of lists
of the form REPLACEMENT-PATH ORIGINAL-PATHS where ORIGINAL-PATHS is a list of paths in the original URL
which should be redirected to REDIRECT-PATH. To redirect all paths except ORIGINAL-PATHS to REDIRECT-PATH,
prefix this list with `not'.")
   (blocklist
    '()
    :type (or null list)
    :documentation "Property list of block listed resources in the form of TYPE VALUE where TYPE
is one of :path or :host, and VALUE is another plist of the form TYPE and PATHNAMES where TYPE is either
 :start, :end, or :contain and PATHNAMES is a list of URL pathnames to draw the comparison against. If PATHNAMES
is prefixed with `not', all sites will be blocked except for the specified list.")
   (external
    nil
    :type (or null function string)
    :documentation "This tells the resource is to be opened externally. If function form, it takes
a single parameter REQUEST-DATA and can invoke arbitrary Lisp forms within it. If provided as a string form, it runs
the specified command via `uiop:run-program' with the current URL as argument, can be given in a `format'-like syntax.")
   (instances
    nil
    :type (or null function list)
    :documentation "This provides a list of instances to add to the default sources, useful if a services provides an
 official endpoint where these are stored. It can be provided as either a list or a function which computes the instances."))
  (:export-class-name-p t)
  (:export-slot-names-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "A `url-mapping' is that of a service which often times needs to be
redirected to a privacy-friendly alternative. Additionally, it can be used to enforce allow lists and
 block lists to provide one with fine-grained access to a URL."))

(defmethod initialize-instance :after ((mapping url-mapping) &key)
  (with-slots (instances source) mapping
    (flet ((construct-predicates ()
             (mapcar (lambda (instance)
                       (str:emptyp instance)
                       (if (quri:uri-http-p (quri:uri instance))
                           `(nyxt:match-url ,instance)
                           `(nyxt:match-host ,instance)))
                     (delete nil instances))))
      (alex:when-let ((instances (if (typep instances 'function)
                                     (funcall instances)
                                     instances)))
        (if (nx-mapper::list-of-lists-p source)
            (setf (source mapping) (append source (construct-predicates)))
            (setf (source mapping) (cons source (construct-predicates))))))))

(defun perform-redirect (mapping url)
  "Performs the redirect of URL as provided by `redirect' in MAPPING."
  (if (typep (redirect mapping) 'list)
      (progn
        (loop for rule in (cdr (redirect mapping))
              do (handle-redirect-rule rule url))
        (setf (quri:uri-host url) (first (redirect mapping))))
      (setf (quri:uri-host url) (redirect mapping))))

;; For same page requests, it doesn't do the redirect so I believe
;; I should use the `buffer-loaded-hook' with this handler
(defun redirect-handler (request-data mapping)
  "Redirects REQUEST-DATA to the redirect of MAPPING."
  (let ((url (url request-data)))
    (perform-redirect mapping url)
    (setf (url request-data) url))
  request-data)

(defmethod block-handler (request-data mapping)
  "Specifies rules for which to block REQUEST-DATA from loading in MAPPING."
  (let ((url (url request-data))
        block-p)
    (loop for rule in (blocklist mapping)
          do (when (handle-block-rule rule url mapping)
               (setf block-p t)))
    (if block-p
        (progn
          ;; TODO: see if I can invoke buffer-load on the internal block page
          (when (banner-p (nyxt:current-mode 'rural))
            (nyxt:delete-current-buffer)
            (display-blocked-page :url (nyxt:render-url url)))
          nil)
        request-data)))

(nyxt::define-internal-page-command-global display-blocked-page (&key (url nil))
    (buffer "*Blocked Site*" 'nyxt:base-mode)
  "Shows blocked warning for URL."
  (spinneret:with-html-string
    (let ((mapping (nx-mapper/rural-mode:active-url-mapping nx-mapper:*user-settings*)))
      (:style (nyxt:style buffer))
      (:div :style "display: flex; width: 100%; justify-content:center; align-items: center; flex-direction: column; height: 100%;"
            (:img :src "https://nyxt.atlas.engineer/image/nyxt_128x128.png")
            (:h1 "The page you're trying to access has been blocked by nx-mapper.")
            (when url
              (:a :style "text-decoration: underline;" url))))))

(defun external-handler (request-data mapping)
  "Runs the MAPPING's specified external command with REQUEST-DATA."
  (let ((rule (eval (external mapping)))
        (url (url request-data)))
    (etypecase rule
      (function
       (when (redirect mapping)
         (perform-redirect mapping url))
       (funcall rule request-data))
      (string
       (uiop:run-program (format rule (quri:render-uri url)))))
    nil))

(defun handle-redirect-rule (url-rules url)
  "Transform URL based on the provided URL-RULES."
  (alex:if-let ((path-rules (getf url-rules :path)))
    (loop for (replacement original-paths) in path-rules
          do (if (equal (first original-paths) 'not)
                 (unless (url-compare url (rest original-paths))
                   (setf (quri:uri-path url) (str:concat replacement (quri:uri-path url))))
                 (alex:if-let ((old-prefix (url-compare url original-paths :return-value t)))
                   (setf (quri:uri-path url) (str:replace-first old-prefix replacement (quri:uri-path url))))))
    url))

(defun handle-block-rule (url-rules url mapping)
  "Evaluates if resource blocking should take place in URL according to URL-RULES and
uses MAPPING to block redirects accordingly."
  (let ((path-rules (getf url-rules :path))
        (host-rules (getf url-rules :host))
        block-p)
    (flet ((assess-rules (type test rules)
             (if (equal (first rules) 'not)
                 (unless (url-compare url (rest rules) :eq-fn test :type type)
                   (setf block-p t))
                 (when (url-compare url rules :eq-fn test :type type)
                   (setf block-p t)))))
      (loop for (predicate paths) in path-rules
            do (case predicate
                 (:contains (assess-rules :path :contains paths))
                 (:starts (assess-rules :path :starts paths))
                 (:ends (assess-rules :path :ends paths))))
      (loop for (predicate hostnames) in host-rules
            do (case predicate
                 (:contains (assess-rules :host :contains hostnames))
                 (:starts (assess-rules :host :starts hostnames))
                 (:ends (assess-rules :host :ends hostnames))))
      block-p)))

(defun url-compare (url url-parts &key (type :path) (eq-fn :starts) (return-value nil))
  "Returns true or return-value when this is non-nil if at least one of URL-PARTS
matches the provided URL TYPE with EQ-FN.
TYPE can be one of :host, :path or :domain, while EQ-FN can be one of :starts, :contains, or :ends."
  (let ((uri-part (case type
                    (:host
                     (quri:uri-host url))
                    (:domain
                     (quri:uri-domain url))
                    (otherwise
                     (quri:uri-path url))))
        (predicate (case eq-fn
                     (:contains #'str:containsp)
                     (:ends #'str:ends-with-p)
                     (otherwise #'str:starts-with-p))))
    (if return-value
        (find-if (lambda (prefix)
                (if (string= prefix "/")
                    (string= (quri:uri-path url) "/")
                    (funcall predicate prefix uri-part)))
                 url-parts)
        (some (lambda (prefix)
                (if (string= prefix "/")
                    (string= (quri:uri-path url) "/")
                    (funcall predicate prefix uri-part)))
              url-parts))))

(define-mode rural-mode ()
  "Apply a set of rules on a site."
  ((nyxt:glyph "🖇")
   (nyxt:constructor #'initialize)
   (nyxt:destructor #'cleanup)
   (banner-p t :type boolean
             :documentation "Whether to show a popup when the current site is blocked.")))

;; TODO: third party requests, such as embedded frames, are still being acted upon
;; as per https://github.com/atlas-engineer/nyxt/issues/980
(defun url-mapping-handler (request-data)
  "Handles buffer and the `rural-mode' URL mapping associations to dispatch the corresponding request-data."
  (alex:if-let ((mapping (find-if (lambda (mapping)
                                    (let ((source (source mapping)))
                                      (if (nx-mapper::list-of-lists-p source)
                                          (some (lambda (predicate)
                                                     (funcall (eval predicate) (nyxt:url request-data)))
                                                source)
                                          (funcall (eval source) (nyxt:url request-data)))))
                                    (url-mappings nx-mapper:*user-settings*))))
    ;; TODO: external handler for certain URL parts?
    ;; TODO: block or redirect per page title?
    (progn
      (setf (active-url-mapping nx-mapper:*user-settings*) mapping)
      (if (nyxt:request-resource-hook (current-buffer))
          (cond
            ((external mapping)
             (external-handler request-data mapping))
            ((and (redirect mapping)
                  (blocklist mapping))
             (progn
               (redirect-handler request-data mapping)
               (block-handler request-data mapping)))
            ((and (redirect mapping)
                  (null (external mapping)))
             (redirect-handler request-data mapping))
            ((blocklist mapping)
             (block-handler request-data mapping))
            (t request-data))
          request-data))
    (progn
      ;; (setf (active-url-mapping nx-mapper:*user-settings*) nil)
      request-data)))

(defmethod initialize ((mode rural-mode))
  "Initializes `url-mapping-mode' to enable URL associations."
  (when (nyxt:web-buffer-p (nyxt:buffer mode))
    (hooks:add-hook (nyxt:request-resource-hook (nyxt:buffer mode)) #'url-mapping-handler)))

(defmethod cleanup ((mode rural-mode))
  "Cleans up `url-mapping-mode'."
  (when (nyxt:web-buffer-p (nyxt:buffer mode))
    (hooks:remove-hook (nyxt:request-resource-hook (nyxt:buffer mode))
                       #'url-mapping-handler)))
