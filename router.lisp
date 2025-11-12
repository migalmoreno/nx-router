(in-package #:nx-router)
(nyxt:use-nyxt-package-nicknames)

(-> list-of-lists-p (t) boolean)
(defun list-of-lists-p (object)
  "Return non-nil of OBJECT is a list of lists."
  (and (listp object)
       (every #'listp object)))

(define-class routes-builder ()
  ((source
    nil
    :type (or quri:uri string null))
   (builder
    nil
    :type (or function symbol null)))
  (:export-class-name-p t)
  (:export-slot-names-p t)
  (:export-accessor-names-p t)
  (:documentation "A routes builder for a router."))

(defun fetch-routes (url)
  (handler-case (dex:get url)
    (usocket:ns-host-not-found-error ()
      (nyxt:echo-warning
       "There's no Internet connection to retrieve routes")
      nil)))

(defgeneric build-routes (routes-builder)
  (:documentation "Build a list of routes from ROUTES-BUILDER."))

(defmethod build-routes ((routes-builder routes-builder))
  (alex:when-let ((routes (fetch-routes (source routes-builder))))
    (delete nil (funcall (builder routes-builder) routes))))

(defun maybe-list-of-routes-builder-p (list)
  "Return t if LIST is null or a list of `routes-builder' objects."
  (or (null list)
      (and (consp list)
           (every #'routes-builder-p list))))

(deftype maybe-list-of-routes-builder ()
  `(satisfies maybe-list-of-routes-builder-p))

(define-class router ()
  ((name
    nil
    :type (or null symbol))
   (route
    nil
    :type (or null string list function)
    :documentation "Route(s) to determine if `router' is to be activated.")
   (routes-builder
    nil
    :type maybe-list-of-routes-builder
    :documentation "A `routes-builder' object that holds the necessary setup
to build a list of routes for a `router'.  These will be appended to
the router's `route'.")
   (toplevel-p
    t
    :type boolean
    :documentation "Whether `router' should process only top-level requests."))
  (:export-class-name-p t)
  (:export-slot-names-p t)
  (:export-accessor-names-p t)
  (:documentation "Customizable request resource handler for routing."))

(defun maybe-list-of-routers-p (list)
  "Return t if LIST is null or a list of `router' objects."
  (or (null list)
      (and (consp list)
           (every #'router-p list))))

(deftype maybe-list-of-routers ()
  `(satisfies maybe-list-of-routers-p))

(define-class blocker (router)
  ((block-banner-p
    t
    :type boolean
    :documentation "Whether to display a block banner upon route blocking.")
   (blocklist
    nil
    :type (or boolean string list)
    :documentation "A PCRE to match against the current route, `t' to block the
entire route, or a list of regexps to draw the comparison against.  If any
single list is prefixed with `not', the entire route will be blocked except for
the specified regexps.  If all of the lists are prefixed with `or', this follows
an exception-based blocking where you can specify a more general block regexp
first and bypass it for more specific routes."))
  (:export-class-name-p t)
  (:export-slot-names-p t)
  (:export-accessor-names-p t)
  (:metaclass user-class)
  (:documentation "General-purpose `router' to determine what to block."))

(define-class redirector (router)
  ((redirect
    nil
    :type (or null string list quri:uri function symbol)
    :documentation "A string for the hostname of the URL to redirect to, a PCRE
or an alist of redirection rules.  These have the form REDIRECT . ROUTES, where
ROUTES is a list of regexps that will be matched against and redirected to
REDIRECT.  To redirect all routes except ROUTES to REDIRECT, prefix this list
with `not'.")
   (reverse
    nil
    :type (or null string quri:uri)
    :documentation "Original URL of the redirect.  Useful for storage purposes
 (bookmarks, history, etc.) so this is recorded instead of the redirect."))
  (:export-class-name-p t)
  (:export-slot-names-p t)
  (:export-accessor-names-p t)
  (:metaclass user-class)
  (:documentation "General-purpose redirect `router'."))

(define-class opener (router)
  ((resource
    nil
    :type (or null string function symbol)
    :documentation "A resource can be either a function form, in which case it
takes a single parameter URL and can invoke arbitrary Lisp forms with it.
If it's a string form, it runs the specified command via `uiop:run-program' with
the current URL as argument, and can be given in a `format'-like syntax."))
  (:export-class-name-p t)
  (:export-slot-names-p t)
  (:export-accessor-names-p t)
  (:metaclass user-class)
  (:documentation "`router' that instructs resources to be opened externally."))

(define-mode router-mode ()
  "Apply a set of routers on the current browsing session."
  ((routers
    '()
    :type list
    :documentation "List of `router's to be matched against the current buffer.")
   (nyxt:glyph "⚑")))

(defmethod nyxt:enable ((mode router-mode) &key)
  (with-slots (routers buffer) mode
    (setf routers
          (reverse
           (reduce
            (lambda (acc router)
              (when (name router)
                (let ((base (find (name router) acc :key #'name)))
                  (dolist (slot (set-difference
                                 (mopu:slot-names (class-of base))
                                 (mopu:direct-slot-names (class-of base))))
                    (setf (slot-value router slot) (slot-value base slot)))))
              (cons router acc))
            routers
            :initial-value '())))
    (hooks:add-hook (nyxt:request-resource-hook buffer)
                    (make-instance 'hooks:handler
                                   :fn (lambda (request-data)
                                         (router-handler request-data mode))
                                   :name 'handle-routing))))

(defmethod nyxt:disable ((mode router-mode) &key)
  (hooks:remove-hook (nyxt:request-resource-hook (buffer mode))
                     'handle-routing))

(defmethod initialize-instance :after ((router router) &key)
  (with-slots (routes-builder route) router
    (nyxt:run-thread "nx-router build routes"
      (flet ((construct-predicates (routes)
               (mapcar (lambda (r)
                         `(nyxt:match-host
                           ,(if (quri:uri-http-p (quri:uri r))
                                (str:join
                                 ""
                                 (str:split-omit-nulls
                                  "/"
                                  (nyxt::schemeless-url (quri:uri r))))
                                r)))
                       routes)))
        (alex:when-let ((routes (and routes-builder
                                     (build-routes routes-builder))))
          (cond
            ((list-of-lists-p route)
             (setf (route router)
                   (append route (construct-predicates routes))))
            (t (setf (route router)
                     (cons route (construct-predicates routes))))))))))

(-> match-by-redirect (quri:uri router-mode) maybe-list-of-routers)
(defun match-by-redirect (url mode)
  "Match MODE routers by route redirect against URL."
  (remove-if-not
   (lambda (router)
     (when (and (redirector-p router)
                (with-slots (redirect) router
                  (and redirect
                       (string= (quri:uri-host url)
                                (etypecase redirect
                                  (string redirect)
                                  (quri:uri (quri:uri-host redirect))
                                  (list (get-redirect redirect url))
                                  ((or function symbol)
                                   (funcall redirect)))))))
       router))
   (routers mode)))

(-> match-by-route (quri:uri router-mode) maybe-list-of-routers)
(defun match-by-route (url mode)
  "Match MODE routers by route against URL."
  (flet ((routes-match-p (routes)
           (some (lambda (pred)
                   (typecase pred
                     (string
                      (funcall (nyxt:match-regex pred) url))
                     (list
                      (funcall (eval pred) url))
                     (function
                      (funcall pred url))))
                 routes)))
    (remove-if-not
     (lambda (router)
       (with-slots (route) router
         (cond
           ((stringp route)
            (funcall (nyxt:match-regex route) url))
           ((list-of-lists-p route)
            (routes-match-p route))
           ((listp route)
            (if (routes-builder router)
                (routes-match-p route)
                (funcall (eval route) url)))
           ((functionp route)
            (funcall route url)))))
     (routers mode))))

(export-always 'trace-url)
(-> trace-url (quri:uri) t)
(defun trace-url (url)
  (alex:if-let ((router (find-if (lambda (r)
                                   (redirector-p r))
                                 (match-by-redirect
                                  url
                                  (nyxt:find-submode
                                   (sym:resolve-symbol :router-mode :mode
                                                       '(:nx-router)))))))
    (with-slots (redirect reverse) router
      (cond
        ((and router
              (string= (etypecase redirect
                         (string redirect)
                         (quri:uri (quri:uri-host redirect))
                         (list "")
                         ((or function symbol) (funcall redirect)))
                       (quri:uri-host url)))
         (compute-router router url :reversed t))
        ((and router reverse) (quri:copy-uri url :host reverse))
        (t url)))
    url))

(-> find-url (quri:uri list &key (:key function) (:test function) (:pred function)) t)
(defun find-url (url url-parts &key (key #'quri:render-uri) (test #'ppcre:scan) (pred #'find-if))
  "Test URL-PARTS with PRED against URL by KEY with TEST."
  (funcall pred (lambda (prefix)
                  (funcall test prefix (funcall key url)))
           url-parts))

(-> get-redirect (list quri:uri &key (:reversed boolean)) (or string null))
(defun get-redirect (rules url &key reversed)
  "Compute redirect TARGETS for URL and return the first matching
redirect.  If REVERSED, reverse the redirection."
  (loop for (replacement . targets) in rules
        collect
        (cond
          (reversed
           (alex:when-let ((prefix (find-url url (list replacement))))
             (ppcre:regex-replace
              (cond
                ((and (consp targets)
                      (equal (first targets) 'not))
                 "")
                ((consp targets) (car targets))
                (t targets))
              prefix (quri:render-uri url))))
          ((and (consp targets) (equal (first targets) 'not))
           (unless (find-url url (rest targets))
             (str:concat replacement
                         (str:join "/" (str:split-omit-nulls
                                        "/" (quri:uri-path url))))))
          (t (alex:when-let ((prefix
                              (find-url
                               url
                               (if (consp targets)
                                   targets
                                   (list targets)))))
               (ppcre:regex-replace prefix (quri:render-uri url)
                                    replacement))))
          into paths
        finally (return (car (delete nil paths)))))

(-> get-blocklist (list quri:uri) boolean)
(defun get-blocklist (targets url)
  "Determine whether TARGETS should be blocked according to URL."
  (loop for target in targets
        collect
        (if (and (consp targets) (equal (first targets) 'not))
            (not (find-url url (rest targets) :pred #'every))
            (find-url url (if (consp targets) targets (list targets)) :pred #'every))
          into blocked-results
        finally (return (not (some #'null blocked-results)))))

(defgeneric compute-router (router url &key &allow-other-keys))

(defmethod compute-router ((router redirector) url &key reversed)
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
    (with-slots (reverse redirect route) router
      (cond
        ((stringp route)
         (quri:uri
          (if (ppcre:scan route (render-url url))
              (ppcre:regex-replace
               route (render-url url) (etypecase redirect
                                        (string redirect)
                                        (quri:uri (render-url redirect))))
              url)))
        ((consp redirect)
         (alex:if-let ((redirect-url
                        (get-redirect redirect url
                                      :reversed reversed)))
           (quri:uri redirect-url)
           url))
        (t
         (build-uri
          (if reversed
              (typecase reverse
                (string (quri:make-uri :host reverse))
                (quri:uri reverse))
              (typecase redirect
                (string (quri:make-uri :host redirect))
                (quri:uri redirect)
                ((or function symbol)
                 (quri:uri (funcall redirect)))))))))))

(defmethod compute-router ((router blocker) url &key)
  (with-slots (blocklist) router
    (typecase blocklist
      (string
       (not (null (ppcre:scan blocklist (render-url url)))))
      (list
       (if (equal (first blocklist) 'or)
           (loop for rules in (rest blocklist)
                 collect
                 (get-blocklist (if (consp rules) rules (list rules)) url)
                   into clauses
                 finally (return (not (some #'null clauses))))
           (get-blocklist blocklist url)))
      (otherwise t))))

(defmethod compute-router ((router opener) url &key)
  (with-slots (resource) router
    (let ((url (quri:url-decode (quri:render-uri url))))
      (typecase resource
        (string
         (uiop:run-program (format nil resource url)))
        ((or function symbol)
         (nyxt:run-thread "Spawn external rules"
           (funcall resource url))))))
  nil)

(nyxt::define-internal-page-command-global display-blocked-page (&key url)
    (buffer "*Blocked Site*" 'nyxt:base-mode)
  "Show blocked internal page for URL."
  (let ((style (theme:themed-css (nyxt:theme nyxt:*browser*)
                 `(body
                   :padding 0
                   :margin 0)
                 `(.container
                   :display flex
                   :height 100vh
                   :justify-content center
                   :align-items center
                   :flex-direction column
                   :text-align center)
                 `(|#banner|
                   :display flex
                   :justify-content center
                   :flex-direction column
                   :width 70vw)
                 `(|#url|
                   :text-decoration none
                   :font-weight bold
                   :color ,theme:accent
                   :pointer-events none))))
    (spinneret:with-html-string
      (:style style)
      (:div :class "container"
            (:img :src "https://nyxt.atlas.engineer/image/nyxt_128x128.png")
            (:div :id "banner"
                  (:h1 "The page you're trying to access has been blocked.")
                  (when url
                    (:a :id "url" url)))))))

(defgeneric dispatch-router (request-data router))

(defmethod dispatch-router (request-data (router redirector))
  (let ((url (and request-data (url request-data))))
    (when (and url (or (nyxt:toplevel-p request-data)
                       (not (toplevel-p router))))
      (setf (url request-data) (compute-router router url))))
  request-data)

(defmethod dispatch-router (request-data (router blocker))
  (let ((url (and request-data (url request-data))))
    (if (and url (or (nyxt:toplevel-p request-data) (not (toplevel-p router))))
        (if (compute-router router url)
            (progn
              (and (block-banner-p router)
                   (nyxt:buffer-load
                    (nyxt:nyxt-url 'display-blocked-page
                                   :url (render-url url))
                    :buffer (buffer request-data)))
              nil)
            request-data)
        request-data)))

(defmethod dispatch-router (request-data (router opener))
  (let ((url (and request-data (url request-data))))
    (when (and url (or (nyxt:toplevel-p request-data)
                       (not (toplevel-p router))))
      (compute-router router url))
    (when (nyxt:toplevel-p request-data)
      (nyxt::buffer-delete (buffer request-data)))))

(defmethod router-handler (request-data (mode router-mode))
  (when request-data
    (alex:if-let ((routers (match-by-route (url request-data) mode)))
      (progn
        (when (nyxt:request-resource-hook (buffer mode))
          (dolist (router routers)
            (setf request-data (dispatch-router request-data router))))
        request-data)
      request-data)))
