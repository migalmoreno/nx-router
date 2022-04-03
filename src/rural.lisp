(uiop:define-package #:nx-mapper/rural-mode
  (:use #:cl)
  (:import-from #:nyxt
                #:define-class
                #:define-user-class
                #:define-mode
                #:define-command-global
                #:current-buffer
                #:url
                #:buffer)
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
    :documentation "List of URL mappings that predicates are to be matched against the user's buffers.")
   (media-enabled-p
    t
    :type boolean
    :documentation "Whether to allow media in sites. This can be overridden per `url-mapping'."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))
(define-user-class settings)


;; TODO: If instances is provided as a function, it can be too computing expensive
;; on initial Nyxt launch and it sometimes might not even get run at the right time
;; to be added to the URL mapping sources, needing for the mode to be re-invoked.
;; Look into using threads.
(define-class url-mapping (nx-mapper:mapping)
  ((source
    '()
    :type (list function)
    :documentation "Source where this mapping should be applied to.")
   (redirect
    nil
    :type (or list quri:uri string null)
    :documentation "Main redirect URL to be used or, when :path is given, a single-pair
of the form REPLACEMENT-PATH ORIGINAL-PATHS where ORIGINAL-PATHS is a list of paths of the original URL
which will be redirected to REPLACEMENT-PATH. To redirect all paths except ORIGINAL-PATHS to REPLACEMENT-PATH,
prefix this list with `not'.")
   (blocklist
    '()
    :type (or null list)
    :documentation "Property list of blocking conditions in the form of TYPE VALUE where TYPE
is one of :path or :host, and VALUE is another plist of the form TYPE PATHNAMES where TYPE is either
 :start, :end, or :contain and PATHNAMES is a list of URL pathnames to draw the comparison against. If PATHNAMES
is prefixed with `not', all sites will be blocked except for the specified list. Also, if this is `t', it
will block the whole URL for the defined sources.")
   (external
    nil
    :type (or null function string)
    :documentation "This tells the resource is to be opened externally. If function form, it takes
a single parameter REQUEST-DATA and can invoke arbitrary Lisp forms within it. If provided as a string form,
it runs the specified command via `uiop:run-program' with the current URL as argument, can be given in
 a `format'-like syntax.")
   (media-p nil :type boolean
                :documentation "Whether to show media in the site or not.")
   (instances
    nil
    :type (or null function list)
    :documentation "This provides a list of instances to add to the default sources, useful if a service
provides an official endpoint where these are stored. It can be provided as either a list or a function
which computes the instances."))
  (:export-class-name-p t)
  (:export-slot-names-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "A `url-mapping' is that of a service which often times needs to be
redirected to a privacy-friendly alternative. Additionally, it can be used to enforce good habits by setting
 block lists to mold you the way you access sites."))

(defmethod initialize-instance :after ((mapping url-mapping) &key)
  (with-slots (instances source) mapping
    (flet ((construct-predicates ()
             (mapcar (lambda (instance)
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
        (loop for (original rules) on (redirect mapping)
              by #'cddr while rules
              do (handle-redirect-rule rules url))
        (setf (quri:uri-host url) (first (redirect mapping))))
      (setf (quri:uri-host url) (redirect mapping))))

;; TODO: For same page requests, it sometimes won't perform the redirect so look
;; into using `buffer-loaded-hook' or `buffer-load-hook'
(defun redirect-handler (request-data mapping)
  "Redirects REQUEST-DATA to the redirect of MAPPING."
  (let ((url (url request-data)))
    (perform-redirect mapping url)
    (setf (url request-data) url))
  request-data)

(defmethod block-handler (request-data mapping)
  "Specifies rules for which to block REQUEST-DATA from loading in MAPPING."
  (let ((url (url request-data))
        (blocklist (blocklist mapping))
        block-p)
    (typecase blocklist
      (list (loop for (type rules) on blocklist
                    by #'cddr while rules
                  do (setf block-p (handle-block-rules rules url type))))
      (t (setf block-p t)))
    (if block-p
        (progn
          ;; TODO: see if I can invoke `buffer-load' on the internal block page
          ;; to avoid having to delete the current buffer
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
      (:div :style (cl-css:inline-css
                    '(:display "flex" :width "100%"
                      :justify-content "center"
                      :align-items "center"
                      :flex-direction "column"
                      :height "100%"))
            (:img :src "https://nyxt.atlas.engineer/image/nyxt_128x128.png")
            (:h1 "The page you're trying to access has been blocked by nx-mapper.")
            (when url
              (:a :style (cl-css:inline-css '(:text-decoration "underline")) url))))))

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

(defun handle-redirect-rule (rules url)
  "Transform URL based on the provided RULES."
  (loop for (type redirect) on rules
        by #'cddr while redirect
        do (case type
             (:path (loop for (replacement original-paths) on redirect
                            by #'cddr while original-paths
                          do (if (equal (first original-paths) 'not)
                                 (unless (url-compare url (rest original-paths))
                                   (setf (quri:uri-path url)
                                         (str:concat replacement (quri:uri-path url))))
                                 (alex:if-let ((old-prefix
                                                (url-compare url original-paths :return-value t)))
                                   (setf (quri:uri-path url)
                                         (str:replace-first old-prefix replacement
                                                            (quri:uri-path url)))))))))

  url)

(defun handle-block-rules (rules url type)
  "Evaluates if resource blocking should take place in URL according to RULES and TYPE."
  (let (block-p)
    (flet ((assess-rules (type test rules)
             (setf block-p
                   (if (equal (first rules) 'not)
                       (unless (url-compare url (rest rules) :eq-fn test :type type)
                           t)
                       (when (url-compare url rules :eq-fn test :type type)
                         t)))))
      (case type
        (:path
         (etypecase rules
           (list (if (equal (first rules) 'or)
                     (loop for clause in (rest rules)
                           collect (progn
                                     (etypecase clause
                                       (list
                                        (loop for (predicate paths) on clause
                                              by #'cddr while paths
                                              collect (case predicate
                                                        (:contains (assess-rules :path :contains paths))
                                                        (:starts (assess-rules :path :starts paths))
                                                        (:ends (assess-rules :path :ends paths)))))
                                       (integer (when (= (length (str:split-omit-nulls "/" (quri:uri-path url)))
                                                         clause)
                                                  (setf block-p t))))
                                     block-p)
                             into clauses
                           finally (setf block-p (not (some #'null clauses))))
                     (loop for (predicate paths) on rules
                           by #'cddr while paths
                           do (case predicate
                                (:contains (assess-rules :path :contains paths))
                                (:starts (assess-rules :path :starts paths))
                                (:ends (assess-rules :path :ends paths))))))
           ;; TODO: allow to block user-provided predicate that takes the URL mapping path
           (integer (when (= (length (str:split-omit-nulls "/" (quri:uri-path url)))
                             rules)
                      (setf block-p t)))))
        ;; TODO: Allow rule combinations for host, but need to abstract away logic to avoid
        ;; code duplication
        (:host
         (loop for (predicate hostnames) on rules
               by #'cddr while hostnames
               do (case predicate
                    (:contains (assess-rules :host :contains hostnames))
                    (:starts (assess-rules :host :starts hostnames))
                    (:ends (assess-rules :host :ends hostnames))))))
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
               :documentation "Whether to show a block banner when the current site is blocked.")
   (enforce-p nil :type boolean
                  :documentation "Set this to non-nil to prevent you from disabling the mode.")))

(defun set-media-state (state request-data)
  "Sets the value of `url-media-p' to STATE for the current REQUEST-DATA."
  (nyxt:ffi-buffer-auto-load-image (buffer request-data) state)
  (nyxt:ffi-buffer-enable-media (buffer request-data) state))

;; TODO: third party requests, such as embedded frames, are still being triggered
;; as per https://github.com/atlas-engineer/nyxt/issues/980
(defun url-mapping-handler (request-data)
  "Handles buffer and the `rural-mode' URL mapping associations
to dispatch the corresponding request-data."
  (alex:if-let ((mapping (find-if (lambda (mapping)
                                    (let ((source (source mapping)))
                                      (if (nx-mapper::list-of-lists-p source)
                                          (some (lambda (predicate)
                                                  (funcall (eval predicate) (url request-data)))
                                                source)
                                          (funcall (eval source) (url request-data)))))
                                  (url-mappings nx-mapper:*user-settings*))))
    ;; TODO: external handler for certain URL parts
    ;; TODO: block or redirect per page title
    (progn
      (if (media-p mapping)
          (set-media-state (not (media-enabled-p nx-mapper:*user-settings*)) request-data)
          (set-media-state (media-enabled-p nx-mapper:*user-settings*) request-data))
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
      (setf (active-url-mapping nx-mapper:*user-settings*) nil)
      (set-media-state (media-enabled-p nx-mapper:*user-settings*) request-data)
      request-data)))

(defmethod initialize ((mode rural-mode))
  "Initializes `url-mapping-mode' to enable URL associations."
  (when (nyxt:web-buffer-p (buffer mode))
    (hooks:add-hook (nyxt:request-resource-hook (buffer mode)) #'url-mapping-handler)))

(defmethod cleanup ((mode rural-mode))
  "Cleans up `url-mapping-mode'."
  (when (and (nyxt:web-buffer-p (buffer mode))
             (not (enforce-p mode)))
    (hooks:remove-hook (nyxt:request-resource-hook (buffer mode))
                       #'url-mapping-handler)))
