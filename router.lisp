(in-package #:nx-router)
(nyxt:use-nyxt-package-nicknames)

(export-always 'make-route)
(-> make-route ((or function list) &rest t &key &allow-other-keys) t)
(defun make-route (trigger &rest extra-slots &key &allow-other-keys)
  "Construct a `route'. TRIGGER is required and EXTRA-SLOTS can vary
depending on the complexity of the route."
  (apply #'make-instance 'route :trigger trigger extra-slots))

(-> list-of-lists-p ((or function list)) boolean)
(defun list-of-lists-p (object)
  "Return non-nil of OBJECT is a list of lists."
  (and (listp object)
       (every #'listp object)))

(define-class route ()
  ((trigger
    '()
    :type (or list function)
    :documentation "Trigger(s) for this route to be followed.")
   (original
    nil
    :type (or null string quri:uri)
    :documentation "Original host of the route. Useful for storage purposes (bookmarks, history, etc.) so that
the original URL is recorded.")
   (redirect
    nil
    :type (or redirect list string quri:uri function symbol null)
    :documentation "Main redirect to be used for this route. It can be given as a simple string to
redirect to a hostname, as a cons pair of REDIRECT-URL . REDIRECT-RULES, where REDIRECT-URLS is a plist of
TYPE RULES where RULES is an alist of cons pairs of the form REPLACEMENT-PATH . ORIGINAL-PATHS where ORIGINAL-PATHS
is a list of paths of the original URL which will be redirected to REPLACEMENT-PATH. To redirect all
paths except ORIGINAL-PATHS to REPLACEMENT-PATH, prefix this list with `not'. Alternatively, it can be given as a
`redirect' object with the appropriate slots or as a function to compute an arbitrary redirect URL.")
   (blocklist
    '()
    :type (or blocklist null list)
    :documentation "Property list of blocking conditions in the form of TYPE VALUE where TYPE
is one of :path or :host, and VALUE is another plist of the form TYPE PATHNAMES where TYPE is either
 :start, :end, or :contain and PATHNAMES is a list of URL pathnames to draw the comparison against. If PATHNAMES
is prefixed with `not', all sites will be blocked except for the specified list. Also, if this is `t', it
will block the whole URL for the defined triggers.")
   (external
    nil
    :type (or null function string)
    :documentation "Instruct the resource is to be opened externally. If a function form, it takes
a single parameter REQUEST-DATA and can invoke arbitrary Lisp forms within it. If a string form,
it runs the specified command via `uiop:run-program' with the current URL as argument, and can be given in
 a `format'-like syntax.")
   (media-p nil :type boolean
                :documentation "Whether to show media in the site or not.")
   (instances
    nil
    :type (or null function)
    :documentation "A function to compute a list of instances to add to the default triggers,
useful if a service provides an official endpoint where these are stored."))
  (:export-class-name-p t)
  (:export-slot-names-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "A `route' is a series of modifications to apply to a url
to mold the way you interact with it."))

(define-class blocklist ()
  ((block-type
    ':path
    :type keyword
    :documentation "The type of block that will be test on the URL. Currently, only
:path and :host are supported.")
   (rules
    '()
    :type list
    :documentation "A property list of the form TYPE PATHNAMES where TYPE is either
:start, :end, or :contain and PATHNAMES is a list of URL pathnames to draw the comparison
against. If PATHNAMES is prefixed with `not', all sites will be blocked except for the
specified list. Also, if this is `t', it will block the whole URL for the defined triggers."))
  (:export-class-name-p t)
  (:export-slot-names-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(define-class redirect ()
  ((redir-type
    ':path
    :type keyword
    :documentation "The type of redirection that will be performed on the URL. Currently,
only :path is supported.")
   (rules
    '()
    :type list
    :documentation "An alist of redirection rules, where each entry is a cons of the form
REPLACEMENT-PATH . ORIGINAL-PATHS, where ORIGINAL-PATHS is a list of paths of the original URL
which will be redirected to REPLACEMENT-PATH. To redirect all paths except ORIGINAL-PATHS to
REPLACEMENT-PATH, prefix this list with `not'.")
   (to
    ""
    :type string
    :documentation "The hostname to redirect to."))
  (:export-class-name-p t)
  (:export-slot-names-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(define-mode router-mode ()
  "Apply a set of routes on the current browsing session."
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
    :documentation "List of provided routes to be matched against the current buffer.")
   (media-enabled-p
    t
    :type boolean
    :documentation "Whether to allow media in routes. This can be overridden per `route'.")))

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
  (when (not (enforce-p mode))
    (hooks:remove-hook (nyxt:request-resource-hook (buffer mode)) 'handle-routing)))

(export-always 'trace-url)
(-> trace-url (quri:uri) quri:uri)
(defun trace-url (url)
  (let* ((route (find-matching-route url (current-router-mode) :redirect t))
         (original-host (and route (original route)))
         (redir (and route (redirect route))))
    (cond
      ((and redir
            (string= (quri:uri-host url)
                     (typecase redir
                       (redirect (to redir))
                       (cons (car redir))
                       (string redir)
                       (quri:uri (quri:uri-host redir)))))
       (perform-redirect route url :reverse t))
      (route (quri:copy-uri url :host original-host))
      (t url))))

(defmethod initialize-instance :after ((route route) &key)
  (nyxt:run-thread "Build list of instances"
    (with-slots (instances trigger redirect) route
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
            (t
             (setf (trigger route) (cons trigger (construct-predicates sources))))))))))

(defmethod perform-redirect ((route route) url &key reverse)
  "Perform the redirect of URL as provided by the `redirect' slot in ROUTE.
If REVERSE, reverse the redirect logic."
  (flet ((build-uri (uri)
           (let ((uri (quri:uri uri)))
             (apply #'quri:make-uri
                    :scheme (alex:if-let ((scheme (quri:uri-scheme uri)))
                              scheme
                              (quri:uri-scheme url))
                    :host (alex:if-let ((host (quri:uri-host uri)))
                            host
                            uri)
                    :path (quri:uri-path url)
                    :query (quri:uri-query url)
                    :fragment (quri:uri-fragment url)
                    :userinfo (quri:uri-userinfo url)
                    (alex:if-let ((port (quri:uri-port uri)))
                      (list :port port)
                      '())))))
    (with-slots (redirect original) route
      (build-uri
       (etypecase redirect
         ((or function symbol)
          (if reverse
              original
              (funcall redirect)))
         (redirect
          (handle-redirect-rule redirect url
                                :reverse reverse)
          (if reverse
              original
              (quri:copy-uri url :host (to redirect))))
         (cons
          (loop for (original . rules) in redirect
                do (handle-redirect-rule rules url
                                         :reverse reverse))
          (if reverse
              original
              (quri:copy-uri url :host (first redirect))))
         ((or quri:uri string)
          (if reverse
              original
              redirect)))))))

(-> handle-path-redirect (list quri:uri &key (:reverse boolean)) (or string null))
(defun handle-path-redirect (redirect url &key reverse)
  "Handle REDIRECT rules targeted at the URL's path.
If REVERSE, reverse the redirect logic."
  (car
   (delete
    nil
    (loop for (replacement . original-paths) in redirect
          collect
          (if reverse
              (alex:when-let ((prefix (url-compare url (list replacement) :return-value t)))
                (str:replace-first
                 prefix
                 (cond
                   ((and (consp original-paths)
                         (equal (first original-paths) 'not))
                    "")
                   ((consp original-paths)
                    (car original-paths))
                   (t
                    original-paths))
                 (quri:uri-path url)))
              (if (and (consp original-paths)
                       (equal (first original-paths) 'not))
                  (unless (or (url-compare url (remove-if (lambda (path)
                                                            (string= path "/"))
                                                          (rest original-paths)))
                              (find-if (lambda (prefix)
                                         (if (str:starts-with? "/" prefix)
                                             (string= (quri:uri-path url) "/")))
                                       (rest original-paths)))
                    (str:concat replacement (str:join "/" (str:split-omit-nulls "/" (quri:uri-path url)))))
                  (alex:when-let ((old-prefix
                                   (url-compare url
                                                (if (consp original-paths)
                                                    original-paths
                                                    (list original-paths))
                                                :return-value t)))
                    (str:replace-first old-prefix replacement
                                       (quri:uri-path url)))))))))

(-> handle-redirect-rule ((or redirect list) quri:uri &key (:reverse boolean)) quri:uri)
(defun handle-redirect-rule (redirect url &key reverse)
  "Transform URL based on the provided REDIRECT.
If REVERSE, reverse the redirect logic."
  (etypecase redirect
    (redirect
     (case (redir-type redirect)
       (:path (alex:when-let ((path (handle-path-redirect (rules redirect)
                                                          url :reverse reverse)))
                (setf (quri:uri-path url) path)))))
    (list
     (loop for (type redirect) on redirect
             by #'cddr while redirect
           return (case type
                    (:path
                     (alex:when-let ((path (handle-path-redirect redirect
                                                                 url :reverse reverse)))
                       (setf (quri:uri-path url) path)))))))
  url)

(defmethod redirect-handler (request-data (route route))
  "Redirect REQUEST-DATA to the redirect of ROUTE."
  (when (and request-data (nyxt:toplevel-p request-data))
    (let ((url (url request-data)))
      (setf (url request-data) (perform-redirect route url))))
  request-data)

(-> handle-block-rules (list quri:uri keyword) boolean)
(defun handle-block-rules (rules url type)
  "Evaluate if resource blocking should take place in URL according to blocking
RULES and TYPE."
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

(-> handle-path-block ((or list integer) quri:uri) boolean)
(defun handle-path-block (rules url)
  "Handle blocklist RULES targeted at the URL's path."
  (etypecase rules
    (list (if (equal (first rules) 'or)
              (loop for clause in (rest rules)
                    collect
                    (etypecase clause
                      (list
                       (handle-block-rules clause url :path))
                      (integer (= (length (str:split-omit-nulls "/" (quri:uri-path url)))
                                  clause)))
                      into clauses
                    finally (return (not (some #'null clauses))))
              (handle-block-rules rules url :path)))
    (integer (= (length (str:split-omit-nulls "/" (quri:uri-path url))) rules))))

(-> handle-host-block (list quri:uri) boolean)
(defun handle-host-block (rules url)
  "Handle blocklist RULES targeted at the URL's hostname."
  (etypecase rules
    (list (if (equal (first rules) 'or)
              (loop for clause in (rest rules)
                    collect (handle-block-rules clause url :host)
                      into clauses
                    finally (return (not (some #'null clauses))))
              (handle-block-rules rules url :host)))))

(defmethod block-handler (request-data (route route))
  "Specify rules for which to block REQUEST-DATA from loading in ROUTE."
  (if (and request-data (nyxt:toplevel-p request-data))
      (let* ((url (url request-data))
             (blocklist (blocklist route))
             (block-p
               (typecase blocklist
                 (blocklist (case (block-type blocklist)
                              (:path (handle-path-block (rules blocklist) url))
                              (:host (handle-host-block (rules blocklist) url))))
                 (list (loop for (type rules) on blocklist
                               by #'cddr while rules
                             return (case type
                                      (:path (handle-path-block rules url))
                                      (:host (handle-host-block rules url)))))
                 (otherwise t))))
        (if block-p
            (progn
              (when (banner-p (current-router-mode))
                (nyxt:buffer-load (nyxt:nyxt-url 'display-blocked-page :url (nyxt:render-url url))
                                  :buffer (buffer request-data)))
              nil)
            request-data))
    request-data))

(defmethod external-handler (request-data (route route))
  "Run the ROUTE's specified external command with REQUEST-DATA."
  (when request-data
    (let ((external-rule (external route))
          (url (url request-data)))
      (typecase external-rule
        (function
         (when (redirect route)
           (perform-redirect route url))
         (funcall external-rule request-data))
        (string
         (uiop:run-program (format external-rule (quri:render-uri url)))))
      (when (nyxt:toplevel-p request-data)
        (nyxt::buffer-delete (buffer request-data)))
      nil)))

(-> url-compare (quri:uri list &key (:type keyword) (:eq-fn keyword) (:return-value boolean)) (or string boolean))
(defun url-compare (url url-parts &key (type :path) (eq-fn :starts) (return-value nil))
  "Return true or RETURN-VALUE if at least one of URL-PARTS matches the
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
    (if return-value
        (find-if (lambda (prefix)
                   (funcall predicate prefix uri-part))
                 url-parts)
        (some (lambda (prefix)
                (funcall predicate prefix uri-part))
              url-parts))))

(-> set-media-state (boolean nyxt:request-data) boolean)
(defun set-media-state (state request-data)
  "Set the value of `media-p' to STATE for the current REQUEST-DATA."
  (setf (nyxt:ffi-buffer-auto-load-image-enabled-p (buffer request-data)) state)
  (setf (nyxt:ffi-buffer-media-enabled-p (buffer request-data)) state))

(export-always 'current-router-mode)
(-> current-router-mode () router-mode)
(defun current-router-mode ()
  "Return `router-mode' if it's active in the current buffer."
  (nyxt:find-submode
   (nyxt:resolve-symbol :router-mode :mode '(:nx-router))))

(export-always 'find-matching-route)
(-> find-matching-route (quri:uri router-mode &key (:redirect boolean)) (or route null))
(defun find-matching-route (url mode &key redirect)
  "Find the matching route from MODE's triggers for URL.
Optionally, match against the route's REDIRECT."
  (flet ((triggers-match-p (triggers)
           (some (lambda (predicate)
                   (typecase predicate
                     (list
                      (funcall (eval predicate) url))
                     (function
                      (funcall predicate url))))
                 triggers)))
    (find-if (lambda (route)
               (let ((source (trigger route))
                     (redir (and redirect (redirect route))))
                 (if (and redir
                          (string= (quri:uri-host url)
                                   (typecase redir
                                     (redirect (to redir))
                                     (cons (car redir))
                                     (string redir)
                                     (quri:uri (quri:uri-host redir)))))
                     route
                     (cond
                       ((list-of-lists-p source)
                        (triggers-match-p source))
                       ((listp source)
                        (if (instances route)
                            (triggers-match-p source)
                            (funcall (eval source) url)))
                       ((functionp source)
                        (funcall source url))))))
             (routes mode))))

(defmethod route-handler (request-data (mode router-mode))
  "Handle routes to dispatch with REQUEST-DATA from MODE's buffer."
  (when request-data
    (alex:if-let ((route (find-matching-route (url request-data) mode)))
      (with-slots (redirect external blocklist) route
        (setf (current-route mode) route)
        (if (media-p route)
            (set-media-state (not (media-enabled-p mode)) request-data)
            (set-media-state (media-enabled-p mode) request-data))
        (if (nyxt:request-resource-hook (buffer mode))
            (cond
              (external
               (external-handler request-data route))
              ((and redirect blocklist)
               (redirect-handler request-data route)
               (block-handler request-data route))
              (redirect
               (redirect-handler request-data route))
              (blocklist
               (block-handler request-data route))
              (t request-data))
            request-data))
      (progn
        (setf (current-route mode) nil)
        (set-media-state (media-enabled-p mode) request-data)
        request-data))))

(nyxt::define-internal-page-command-global display-blocked-page (&key (url nil))
    (buffer "*Blocked Site*" 'nyxt:base-mode)
  "Show blocked internal page for URL."
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
