(in-package #:nx-router)
(nyxt:use-nyxt-package-nicknames)

(-> list-of-lists-p ((or function list)) boolean)
(defun list-of-lists-p (object)
  "Return non-nil of OBJECT is a list of lists."
  (and (listp object)
       (every #'listp object)))

(define-class route ()
  ((trigger
    '()
    :type (or string list function)
    :documentation "Trigger(s) to determine if this `route' is to be activated.")
   (instances
    nil
    :type (or null function)
    :documentation "A function to compute a list of instances to add to `:trigger',
useful if a service provides an official endpoint where these are stored.")
   (toplevel-p
    t
    :type boolean
    :documentation "Whether `route' is meant to process only top-level requests."))
  (:export-class-name-p t)
  (:export-slot-names-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "Extensible handler with predefined slots that allow end-users to customize
its behavior."))

(defun maybe-list-of-routes-p (list)
  "Return t if LIST is null or a list of `route' objects."
  (or (null list)
      (and (consp list)
           (every #'route-p list))))

(deftype maybe-list-of-routes ()
  `(satisfies maybe-list-of-routes-p))

(define-class blocker (route)
  ((block-banner-p
    t
    :type boolean
    :documentation "Whether to display a block banner upon blocking the `route'.")
   (blocklist
    nil
    :type (or boolean string list)
    :documentation "A PCRE to match against the current URL, `t' to block the entire route, or a property
list of blocking conditions in the form of TYPE VALUE, where TYPE is one of :path or :host.  VALUE is
another plist of the form PRED RULES, where PRED is either :starts, :ends, or :contains and RULES is a
list of strings to draw the comparison against according to the current TYPE.  If RULES is prefixed with
`not', the entire route will be blocked except for the specified RULES.  You can also pass an integer as
VALUE to indicate the number of URL sections (e.g. https://example.com/<section1>/<section2>) to block in
case the blocking condition value is not known.

Combined RULES (specified via `:or') allow you to specify two or more predicates that you wish to draw the
path comparison against, useful if you want to specify a more general block rule first and bypass it for
 certain scenarios."))
  (:export-class-name-p t)
  (:export-slot-names-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:metaclass user-class)
  (:documentation "General-purpose `route' to determine what to block."))

(define-class redirector (route)
  ((redirect-rule
    nil
    :type (or null string list)
    :documentation "A PCRE to match against the current URL or an alist of redirection rules for paths.
Each entry is a cons of the form REDIRECT-PATH . TRIGGER-PATHS, where TRIGGER-PATHS is a list
of paths of the trigger URL that will be redirected to REDIRECT-PATH.  To redirect all paths except
TRIGGER-PATHS to REDIRECT-PATH, prefix this list with `not'.")
   (redirect-url
    nil
    :type (or null string quri:uri function symbol)
    :documentation "The URL to redirect to.")
   (original-url
    nil
    :type (or null string quri:uri)
    :documentation "Original URL of the redirect.  Useful for storage purposes (bookmarks, history, etc.)
so that the original URL is recorded instead of the redirect."))
  (:export-class-name-p t)
  (:export-slot-names-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:metaclass user-class)
  (:documentation "General-purpose redirect `route'."))

(define-class opener (route)
  ((resource
    nil
    :type (or null string function symbol)
    :documentation "A resource can be either a function form, in which case it takes
a single parameter URL and can invoke arbitrary Lisp forms with it.  If it's a string form,
it runs the specified command via `uiop:run-program' with the current URL as argument, and can be
given in a `format'-like syntax."))
  (:export-class-name-p t)
  (:export-slot-names-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:metaclass user-class)
  (:documentation "`route' that instructs resources to be opened externally."))

(define-class media-toggler (route)
  ((media-p
    t
    :type boolean
    :documentation "Whether to display media in the route."))
  (:export-class-name-p t)
  (:export-slot-names-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:metaclass user-class)
  (:documentation "`route' that toggles the state of media display."))

(define-class web-route (redirector blocker opener media-toggler)
  ()
  (:export-class-name-p t)
  (:export-slot-names-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:metaclass user-class)
  (:documentation "`route' that combines many routes for behavior you might want to modify
in a web buffer."))

(define-mode router-mode ()
  "Apply a set of routes on the current browsing session."
  ((routes
    '()
    :type list
    :documentation "List of provided routes to be matched against the current buffer.")
   (nyxt:glyph "⚑")))

(defmethod nyxt:enable ((mode router-mode) &key)
  "Initialize `router-mode' to enable routes."
  (hooks:add-hook (nyxt:request-resource-hook (buffer mode))
                  (make-instance
                   'hooks:handler
                   :fn (lambda (request-data)
                         (route-handler request-data mode))
                   :name 'handle-routing)))

(defmethod nyxt:disable ((mode router-mode) &key)
  "Clean up `router-mode', removing the existing routes."
  (hooks:remove-hook (nyxt:request-resource-hook (buffer mode)) 'handle-routing))

(defmethod initialize-instance :after ((route route) &key)
  (with-slots (instances trigger) route
    (nyxt:run-thread "Set up trigger"
      (flet ((construct-predicates (sources)
               (mapcar (lambda (instance)
                         `(nyxt:match-host
                           ,(if (quri:uri-http-p (quri:uri instance))
                                (str:join "" (str:split-omit-nulls
                                              "/" (nyxt::schemeless-url (quri:uri instance))))
                                instance)))
                       sources)))
        (alex:when-let ((sources (and instances (delete nil (funcall instances)))))
          (cond
            ((list-of-lists-p trigger)
             (setf (trigger route) (append trigger (construct-predicates sources))))
            (t (setf (trigger route) (cons trigger (construct-predicates sources))))))))))

(-> match-by-redirect (quri:uri router-mode) maybe-list-of-routes)
(defun match-by-redirect (url mode)
  "Match MODE routes by route redirect against URL."
  (remove-if-not
   (lambda (route)
     (when (and (redirector-p route)
                (with-slots (redirect-url) route
                  (and redirect-url
                       (string= (quri:uri-host url)
                                (etypecase redirect-url
                                  (string redirect-url)
                                  (quri:uri (quri:uri-host redirect-url))
                                  ((or function symbol) (funcall redirect-url)))))))
       route))
   (routes mode)))

(-> match-by-trigger (quri:uri router-mode) maybe-list-of-routes)
(defun match-by-trigger (url mode)
  "Match MODE routes by route trigger against URL."
  (flet ((triggers-match-p (triggers)
           (some (lambda (pred)
                   (typecase pred
                     (string
                      (funcall (nyxt:match-regex pred) url))
                     (list
                      (funcall (eval pred) url))
                     (function
                      (funcall pred url))))
                 triggers)))
    (remove-if-not
     (lambda (route)
       (with-slots (trigger) route
         (cond
           ((stringp trigger)
            (funcall (nyxt:match-regex trigger) url))
           ((list-of-lists-p trigger)
            (triggers-match-p trigger))
           ((listp trigger)
            (if (instances route)
                (triggers-match-p trigger)
                (funcall (eval trigger) url)))
           ((functionp trigger)
            (funcall trigger url)))))
     (routes mode))))

(export-always 'trace-url)
(-> trace-url (quri:uri) t)
(defun trace-url (url)
  (alex:if-let ((route (find-if (lambda (r)
                                  (redirector-p r))
                                (match-by-redirect
                                 url
                                 (nyxt:find-submode
                                  (nyxt:resolve-symbol :router-mode :mode '(:nx-router)))))))
    (with-slots (redirect-url original-url) route
      (cond
        ((and route
              (string= (etypecase redirect-url
                         (string redirect-url)
                         (quri:uri (quri:uri-host redirect-url))
                         ((or function symbol) (funcall redirect-url)))
                       (quri:uri-host url)))
         (compute-route route url :reverse t))
        ((and route original-url) (quri:copy-uri url :host original-url))
        (t url)))
    url))

(-> url-compare (quri:uri list &key (:type keyword) (:eq-fn keyword) (:value boolean)) t)
(defun url-compare (url url-parts &key (type :path) (eq-fn :starts) value)
  "Return true or VALUE if at least one of URL-PARTS matches the
 provided URL TYPE with EQ-FN. TYPE can be one of :host, :path or :domain,
while EQ-FN can be one of :starts, :contains, or :ends."
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
    (funcall (if value #'find-if #'some)
             (lambda (prefix)
               (funcall predicate prefix uri-part))
             url-parts)))

(-> redirect-paths (list quri:uri &key (:reverse boolean)) (or string null))
(defun redirect-paths (rules url &key reverse)
  "Redirect path RULES for URL.
If REVERSE, reverse the redirect logic."
  (loop for (replacement . original-rules) in rules
        collect
        (if reverse
            (alex:when-let ((prefix (url-compare url (list replacement) :value t)))
              (str:replace-first
               prefix
               (cond
                 ((and (consp original-rules) (equal (first original-rules) 'not)) "")
                 ((consp original-rules) (car original-rules))
                 (t original-rules))
               (quri:uri-path url)))
            (if (and (consp original-rules) (equal (first original-rules) 'not))
                (unless (or (url-compare url (remove-if (lambda (rule)
                                                          (string= rule "/"))
                                                        (rest original-rules)))
                            (find-if (lambda (prefix)
                                       (and (str:starts-with? "/" prefix) (string= (quri:uri-path url) "/")))
                                     (rest original-rules)))
                  (str:concat replacement (str:join "/" (str:split-omit-nulls "/" (quri:uri-path url)))))
                (alex:when-let ((old-prefix (url-compare url (if (consp original-rules)
                                                                 original-rules
                                                                 (list original-rules))
                                                         :value t)))
                  (str:replace-first old-prefix replacement (quri:uri-path url)))))
          into paths
        finally (return (car (delete nil paths)))))

(-> block-rules-p (list quri:uri keyword) boolean)
(defun block-rules-p (rules url type)
  "Determine whether RULES should be blocked in URL according to TYPE."
  (flet ((assess-block-rules (url type test rules)
           (if (and (consp rules)
                    (equal (first rules) 'not))
               (not (url-compare url (rest rules) :eq-fn test :type type))
               (url-compare url (if (consp rules)
                                    rules
                                    (list rules))
                            :eq-fn test :type type))))
    (loop for (predicate elements) on rules
            by #'cddr while elements
          collect (case predicate
                    (:contains (assess-block-rules url type :contains elements))
                    (:starts (assess-block-rules url type :starts elements))
                    (:ends (assess-block-rules url type :ends elements)))
            into blocked-results
          finally (return (not (some #'null blocked-results))))))

(-> block-paths-p ((or list integer) quri:uri) boolean)
(defun block-paths-p (paths url)
  "Determine whether PATHS should be blocked for URL's path."
  (etypecase paths
    (list (if (equal (first paths) :or)
              (loop for clause in (rest paths)
                    collect
                    (etypecase clause
                      (list (block-rules-p clause url :path))
                      (integer (= (length (str:split-omit-nulls "/" (quri:uri-path url)))
                                  clause)))
                      into clauses
                    finally (return (notevery #'null clauses)))
              (block-rules-p paths url :path)))
    (integer (= (length (str:split-omit-nulls "/" (quri:uri-path url))) paths))))

(-> block-hosts-p (list quri:uri) boolean)
(defun block-hosts-p (hosts url)
  "Determine whether HOSTS should be blocked for URL's host."
  (etypecase hosts
    (list (if (equal (first hosts) :or)
              (loop for clause in (rest hosts)
                    collect (block-rules-p clause url :host)
                      into clauses
                    finally (return (notevery #'null clauses)))
              (block-rules-p hosts url :host)))))

(defgeneric compute-route (route url &key &allow-other-keys)
  (:documentation "Compute ROUTE with URL."))

(defmethod compute-route ((route redirector) url &key reverse)
  "Redirect URL based on the provided ROUTE.
If REVERSE, reverse the redirect logic."
  (flet ((build-uri (uri)
           (let ((uri (quri:uri uri)))
             (apply #'quri:make-uri
                    :scheme (or (quri:uri-scheme uri) (quri:uri-scheme url))
                    :host (or (quri:uri-host uri) (quri:uri-host url))
                    :path (or (quri:uri-path uri) (quri:uri-path url))
                    :query (quri:uri-query url)
                    :fragment (quri:uri-fragment url)
                    :userinfo (quri:uri-userinfo url)
                    (alex:if-let ((port (quri:uri-port uri)))
                      (list :port port)
                      '())))))
    (with-slots (original-url redirect-url redirect-rule trigger) route
      (let ((redirect-url
              (let ((redirect (etypecase redirect-url
                                (quri:uri redirect-url)
                                (string (quri:make-uri :host redirect-url))
                                ((or function symbol) (quri:uri (funcall redirect-url))))))
                (if (stringp trigger)
                    (if (ppcre:scan trigger (render-url url))
                        (ppcre:regex-replace trigger (render-url url) (render-url redirect-url))
                        url)
                    (if redirect-rule
                        (typecase redirect-rule
                          (string
                           (if (ppcre:scan redirect-rule (render-url url))
                               (ppcre:regex-replace redirect-rule (render-url url) (render-url redirect-url))
                               url)
                           (ppcre:regex-replace redirect-rule (render-url url) (render-url redirect-url)))
                          (list
                           (quri:copy-uri url :host (quri:uri-host redirect)
                                              :path (redirect-paths redirect-rule url :reverse reverse)))
                          (otherwise redirect))
                        redirect)))))
        (build-uri
         (if (and reverse original-url)
             original-url
             redirect-url))))))

(defmethod compute-route ((route blocker) url &key)
  "Determine whether URL should be blocked according to ROUTE."
  (with-slots (blocklist) route
    (typecase blocklist
      (string
       (not (null (ppcre:scan blocklist (render-url url)))))
      (list
       (if (equal (first blocklist) :or)
           (loop for blocklist-type in (rest blocklist)
                 collect (loop for (type rules) on blocklist-type by #'cddr while rules
                               collect (case type
                                         (:path (block-paths-p rules url))
                                         (:host (block-hosts-p rules url)))
                                 into clauses
                               finally (return (not (some #'null clauses))))
                   into clauses
                 finally (return (notevery #'null clauses)))
           (loop for (type rules) on blocklist by #'cddr while rules
                 collect (case type
                           (:path (block-paths-p rules url))
                           (:host (block-hosts-p rules url)))
                   into clauses
                 finally (return (not (some #'null clauses))))))
      (otherwise t))))

(defmethod compute-route ((route opener) url &key)
  (with-slots (resource) route
    (typecase resource
      (string
       (uiop:run-program (format nil resource (quri:render-uri url))))
      ((or function symbol)
       (nyxt:run-thread "Spawn external rules"
         (funcall resource url)))))
  nil)

(nyxt::define-internal-page-command-global display-blocked-page (&key url)
    (buffer "*Blocked Site*" 'nyxt:base-mode)
  "Show blocked internal page for URL."
  (let ((blocked-style (theme:themed-css (nyxt:theme nyxt:*browser*)
                         `(body
                           :padding 0
                           :margin 0)
                         `(.container
                           :display "flex"
                           :height "100vh"
                           :justify-content "center"
                           :align-items "center"
                           :flex-direction "column")
                         `("#banner"
                           :display "flex"
                           :justify-content "center"
                           :flex-direction "column"
                           :width "70vw")
                         `("#url"
                           :text-decoration "underline"))))
    (spinneret:with-html-string
      (:style blocked-style)
      (:div :class "container"
            (:img :src "https://nyxt.atlas.engineer/image/nyxt_128x128.png")
            (:div :id "banner"
                  (:h1 "The page you're trying to access has been blocked by nx-router.")
                  (when url
                    (:a :id "url" :href url url)))))))

(defgeneric dispatch-route (request-data route)
  (:documentation "Dispatch ROUTE with REQUEST-DATA."))

(defmethod dispatch-route (request-data (route redirector))
  (let ((url (and request-data (url request-data))))
    (when (and url (or (nyxt:toplevel-p request-data) (not (toplevel-p route))))
      (let ((redirect-url (compute-route route url)))
        (setf (url request-data) redirect-url))))
  request-data)

(defmethod dispatch-route (request-data (route blocker))
  (let ((url (and request-data (url request-data))))
    (if (and url (or (nyxt:toplevel-p request-data) (not (toplevel-p route))))
        (if (compute-route route url)
            (progn
              (and (block-banner-p route)
                   (nyxt:buffer-load (nyxt:nyxt-url 'display-blocked-page :url (render-url url))
                                     :buffer (buffer request-data)))
              nil)
            request-data)
        request-data)))

(defmethod dispatch-route (request-data (route opener))
  (let ((url (and request-data (url request-data))))
    (when (and url (or (nyxt:toplevel-p request-data) (not (toplevel-p route))))
      (compute-route route url))
    (when (nyxt:toplevel-p request-data)
      (nyxt::buffer-delete (buffer request-data)))))

(defmethod dispatch-route (request-data (route media-toggler))
  (when request-data
    (flet ((set-media-state (state req)
             (setf (nyxt:ffi-buffer-auto-load-image-enabled-p (buffer req)) state)
             (setf (nyxt:ffi-buffer-media-enabled-p (buffer req)) state)))
      (set-media-state (media-p route) request-data))
    request-data))

(defmethod dispatch-route (request-data (route web-route))
  (with-slots (redirect-url original-url redirect-rule blocklist resource media-p) route
    (when (or original-url redirect-url redirect-rule)
      (dispatch-route request-data (make-instance 'redirector
                                                  :original-url original-url
                                                  :redirect-url redirect-url
                                                  :redirect-rule redirect-rule)))
    (when blocklist
      (dispatch-route request-data (make-instance 'blocker :blocklist blocklist)))
    (when media-p
      (dispatch-route request-data (make-instance 'media-toggler :media-p media-p)))
    (when resource
      (dispatch-route request-data (make-instance 'opener :resource resource)))))

(defmethod route-handler (request-data (mode router-mode))
  "Handle routes from MODE to dispatch with REQUEST-DATA."
  (when request-data
    (alex:if-let ((routes (match-by-trigger (url request-data) mode)))
      (progn
        (when (nyxt:request-resource-hook (buffer mode))
          (dolist (route routes)
            (setf request-data (dispatch-route request-data route))))
        request-data)
      request-data)))
