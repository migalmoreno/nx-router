(in-package #:nx-router)
(nyxt:use-nyxt-package-nicknames)

(sera:export-always 'make-route)
(defun make-route (trigger &rest extra-slots &key &allow-other-keys)
  "Constructs a route. TRIGGER is required and EXTRA-SLOTS can vary
depending on the complexity of the route."
  (apply #'make-instance 'route :trigger trigger extra-slots))

(defun list-of-lists-p (object)
  "Returns non-nil of OBJECT consists of a list of lists."
  (and (listp object)
       (every #'listp object)))

(define-class route ()
  ((trigger
    '()
    :type (or list function)
    :documentation "Trigger(s) for this route to be followed.")
   (redirect
    nil
    :type (or list quri:uri string null)
    :documentation "Main redirect URL to be used for this route or, when :path is given, a single-pair
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
will block the whole URL for the defined triggers.")
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
    :type (or null function)
    :documentation "This provides a function to compute a list of instances to add to the default triggers,
useful if a service provides an official endpoint where these are stored."))
  (:export-class-name-p t)
  (:export-slot-names-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "A `route' is that of a service which often times needs to be
redirected to a privacy-friendly alternative. Additionally, it can be used to enforce good habits by setting
 block lists to mold you the way you access sites."))

(define-mode router-mode ()
  "Applies a set of routes on the current browsing session."
  ((banner-p
    :type (or null boolean)
    :documentation "Whether to show a block banner when the route is blocked.")
   (enforce-p
    :type (or null boolean)
    :documentation "Set this to non-nil to prevent you from disabling the mode.")
   (current-route
    nil
    :type (or null route)
    :documentation "Currently active `route'.")
   (routes
    '()
    :type list
    :documentation "List of provided routes to be matched against current buffer.")
   (media-enabled-p
    t
    :type boolean
    :documentation "Whether to allow media in routes. This can be overridden per `route'.")))

(defmethod nyxt:enable ((mode router-mode) &key)
  "Initializes `router-mode' to enable routes."
  (hooks:add-hook (nyxt:request-resource-hook (buffer mode))
                  (make-instance
                   'hooks:handler
                   :fn (lambda (request-data)
                         (route-handler request-data mode))
                   :name 'handle-routing)))

(defmethod nyxt:disable ((mode router-mode) &key)
  "Cleans up `router-mode', removing the existing routes."
  (when (not (enforce-p mode))
    (hooks:remove-hook (nyxt:request-resource-hook (buffer mode)) 'handle-routing)))

(define-class source-type (prompter:source)
  ((prompter:name "Source type")
   (prompter:constructor (list "Domain" "Host" "Regex" "URL"))))

(defmethod initialize-instance :after ((route route) &key)
  (nyxt:run-thread "Builds list of instances"
    (with-slots (instances trigger) route
      (flet ((construct-predicates (sources)
               (mapcar (lambda (instance)
                         (if (quri:uri-http-p (quri:uri instance))
                             `(nyxt:match-url ,instance)
                             `(nyxt:match-host ,instance)))
                       sources)))
        (alex:when-let ((sources (and instances (delete nil (funcall instances)))))
          (cond
            ((list-of-lists-p trigger)
             (setf (trigger route) (append trigger (construct-predicates sources))))
            (t
             (setf (trigger route) (cons trigger (construct-predicates sources))))))))))

(defun perform-redirect (route url)
  "Performs the redirect of URL as provided by `redirect' in ROUTE."
  (if (typep (redirect route) 'list)
      (progn
        (loop for (original rules) on (redirect route)
              by #'cddr while rules
              do (handle-redirect-rule rules url))
        (setf (quri:uri-host url) (first (redirect route))))
      (setf (quri:uri-host url) (redirect route))))

(defun redirect-handler (request-data route)
  "Redirects REQUEST-DATA to the redirect of ROUTE."
  (when (nyxt:toplevel-p request-data)
    (let ((url (url request-data)))
      (perform-redirect route url)
      (setf (url request-data) url))
    request-data))

(defun block-handler (request-data route)
  "Specifies rules for which to block REQUEST-DATA from loading in ROUTE."
  (let ((url (url request-data))
        (blocklist (blocklist route))
        block-p)
    (typecase blocklist
      (list (loop for (type rules) on blocklist
                    by #'cddr while rules
                  do (setf block-p (handle-block-rules rules url type))))
      (t (setf block-p t)))
    (if block-p
        (progn
          (when (banner-p (current-router-mode))
            (nyxt:buffer-load (nyxt:nyxt-url 'display-blocked-page :url (nyxt:render-url url))
                              :buffer (buffer request-data)))
          nil)
        request-data)))

(defun external-handler (request-data route)
  "Runs the ROUTE's specified external command with REQUEST-DATA."
  (let ((external-rule (external route))
        (url (url request-data)))
    (nyxt:run-thread "Open external resource"
      (etypecase external-rule
        (function
         (when (redirect route)
           (perform-redirect route url))
         (funcall external-rule request-data))
        (string
         (uiop:run-program (format external-rule (quri:render-uri url)))))
      (when (nyxt:toplevel-p request-data)
        (nyxt::buffer-delete (buffer request-data))))
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
  "Evaluates if resource blocking should take place in URL according to blocking
RULES and TYPE."
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
           ;; TODO: allow to block user-provided predicate that takes the URL rule path
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

(defun set-media-state (state request-data)
  "Sets the value of `media-p' to STATE for the current REQUEST-DATA."
  (setf (nyxt:ffi-buffer-auto-load-image-enabled-p (buffer request-data)) state)
  (setf (nyxt:ffi-buffer-media-enabled-p (buffer request-data)) state))

(defun current-router-mode ()
  "Returns `router-mode' if it's active in the current buffer."
  (nyxt:find-submode
   (nyxt:resolve-symbol :router-mode :mode '(:nx-router))))

(defun route-handler (request-data mode)
  "Handles routes to dispatch with REQUEST-DATA from MODE's buffer."
  (alex:if-let ((route (find-if (lambda (route)
                                  (let ((source (trigger route)))
                                    (cond
                                      ((list-of-lists-p source)
                                       (some (lambda (predicate)
                                               (etypecase predicate
                                                 (list
                                                  (funcall (eval predicate) (url request-data)))
                                                 (function
                                                  (funcall predicate (url request-data)))))
                                             source))
                                      ((listp source)
                                       (funcall (eval source) (url request-data)))
                                      ((functionp source)
                                       (funcall source (url request-data))))))
                                (routes mode))))
    (progn
      (setf (current-route mode) route)
      (if (media-p route)
          (set-media-state (not (media-enabled-p mode)) request-data)
          (set-media-state (media-enabled-p mode) request-data))
      (if (nyxt:request-resource-hook (buffer mode))
          (cond
            ((external route)
             (external-handler request-data route))
            ((and (redirect route)
                  (blocklist route))
             (progn
               (redirect-handler request-data route)
               (block-handler request-data route)))
            ((redirect route)
             (redirect-handler request-data route))
            ((blocklist route)
             (block-handler request-data route))
            (t request-data))
          request-data))
    (progn
      (setf (current-route mode) nil)
      (set-media-state (media-enabled-p mode) request-data)
      request-data)))

(nyxt::define-internal-page-command-global display-blocked-page (&key (url nil))
    (buffer "*Blocked Site*" 'nyxt:base-mode)
  "Shows blocked warning for URL."
  (spinneret:with-html-string
    (:style (nyxt:style buffer))
    (:div :style (cl-css:inline-css
                  '(:display "flex" :width "100%"
                    :justify-content "center"
                    :align-items "center"
                    :flex-direction "column"
                    :height "100%"))
          (:img :src "https://nyxt.atlas.engineer/image/nyxt_128x128.png")
          (:h1 "The page you're trying to access has been blocked by nx-router.")
          (when url
            (:a :style (cl-css:inline-css '(:text-decoration "underline")) url)))))
