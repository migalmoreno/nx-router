(uiop:define-package #:nx-mapper/stylor-mode
  (:use #:cl)
  (:import-from #:nyxt
                #:define-class
                #:define-user-class
                #:define-mode
                #:define-command
                #:define-command-global
                #:*browser*
                #:theme
                #:current-buffer
                #:current-window
                #:current-mode
                #:buffer
                #:url)
  (:documentation "An interface to manage user custom style-sheets and scripts in Nyxt."))

(in-package #:nx-mapper/stylor-mode)
(nyxt:use-nyxt-package-nicknames)

(define-class settings ()
  ((external-themes
    '()
    :type list
    :documentation "`external-theme' objects for which to apply styles mapped to sources.")
   (internal-themes
    '()
    :type list
    :documentation "`internal-theme' objects among which to select the main internal interface theme.")
   (active-internal-theme
    nil
    :type (or null internal-theme)
    :documentation "`internal-theme' currently active in the browser.")
   (active-external-theme
    nil
    :type (or null external-theme)
    :documentation "`external-theme' currently active in the browser.")
   (scripts
    '()
    :type list
    :documentation "`user-script' objects for which to map sources to pieces of JavaScript to run in them."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "User style and script configurations for both internal and external use to be leveraged
by `stylor-mode'."))
(define-user-class settings)

(define-class external-theme (theme:theme nx-mapper:mapping)
  ((source
    :documentation "Source where this theme is to be applied to.")
   (style nil
    :type (or null quri:uri string function)
    :documentation "Style provided either as a CSS string, a function which takes a theme and returns a
CSS string (useful to share the same look between internal and external themes, can also be specified via :internal), a URL pointing to a CSS file, or a local pathname to a CSS file to apply to the specified SOURCE."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(define-class stylist ()
  ((name
    nil
    :type (or null string)
    :documentation "Stylist name.")
   (buffer-style nil
    :type (or null function)
    :documentation "A function which takes an `internal-theme' and styles the `buffer' appearance.")
   (prompt-style nil
    :type (or null function)
    :documentation "A function which takes an `internal-theme' and styles the `prompt-buffer' appearance.")
   (status-style nil
    :type (or null function)
    :documentation "A function which takes an `internal-theme' and styles the `status-buffer' appearance.")
   (message-style nil
    :type (or null function)
    :documentation "A function which takes an `internal-theme' and styles the `message-buffer' appearance.")
   (hint-style nil
    :type (or null function)
    :documentation "A function which takes an `internal-theme' and styles the `box-style' appearance."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-slot-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "A stylist is a custom style crafter filled with functions to style specific elements of the
internal interface of the browser."))
(define-user-class stylist)

(define-class internal-theme (theme:theme nx-mapper:mapping)
  ((stylist
    nil
    :type (or null stylist)
    :documentation "`stylist' object to allow for dynamic theme change."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(define-class user-script (nx-mapper:mapping)
  ((source
    '()
    :type list
    :documentation "Source where this script is to be applied to.")
   (script
    nil
    :type (or function null)
    :documentation "Script provided as a JavaScript string to apply to the specified SOURCE."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(define-class theme-source (prompter:source)
  ((prompter:name "User themes")
   (prompter:constructor (internal-themes nx-mapper:*user-settings*))
   (prompter:active-attributes-keys '("Name"))))

(define-class source-type (prompter:source)
  ((prompter:name "Source type")
   (prompter:constructor (list "Domain" "Host" "Regex" "URL"))))

(defun get-original-interface-style (element &optional style-slot parent-class)
  "Finds the original STYLE-SLOT slot value of ELEMENT. If PARENT-CLASS,
looks through all the children class slots."
  (sb-mop:slot-definition-initform
   (find (or style-slot 'nyxt:style) (if parent-class
                                         (sb-mop:class-slots
                                          (find-class element))
                                         (sb-mop:class-direct-slots
                                          (find-class element)))
         :key (lambda (el) (slot-value el 'sb-pcl::name)))))

;; TODO: take value to update various fields in a mapping
;; TODO: can this be abstracted away to `update-mapping'?
(defun update-style (name &key (external-p nil))
  "Updates style NAME. If EXTERNAL-P, it updates the external theme NAME."
  (let ((mapping (find name (if external-p
                                (external-themes nx-mapper:*user-settings*)
                                (internal-themes nx-mapper:*user-settings*))
                       :key #'nx-mapper:name :test #'string=)))
    (setf (style mapping)
          (etypecase (style mapping)
            (function
             (nyxt::%edit-with-external-editor
              (funcall
               (style mapping)
               (nx-mapper/stylor-mode:active-internal-theme
                nx-mapper:*user-settings*))))
            (string
             (nyxt::%edit-with-external-editor
              (style mapping)))))))

(define-mode stylor-mode (nyxt/style-mode:style-mode)
  "Mode that applies custom external themes."
  ((nyxt:glyph "🖌")
   (auto-p
    nil
    :type boolean
    :documentation "Whether to automatically apply an `internal-theme'
variant based on the system environment.")
   (nyxt:destructor #'cleanup)
   (nyxt:constructor #'initialize)))

(defun external-style-handler (buffer)
  "Handles setting external styles if the user-defined rules match BUFFER, and applies
the styles depending on the type of mapping provided."
  (alex:when-let* ((mapping (find-if (lambda (mapping)
                                       (let ((source (source mapping)))
                                         (if (nx-mapper::list-of-lists-p source)
                                             (some (lambda (predicate)
                                                     (funcall (eval predicate) buffer))
                                                   source)
                                             (funcall (eval source) buffer))))
                                     (external-themes nx-mapper:*user-settings*)))
                   (style (style mapping)))
    (setf (active-external-theme nx-mapper:*user-settings*) mapping)
    (nyxt::html-set-style
     (etypecase style
       (pathname (ignore-errors (uiop:read-file-string
                                 (style mapping))))
       (quri:uri (nyxt/style-mode::open-or-cache-url (current-mode 'stylor) (style mapping)))
       (string
        (if (quri:uri-http-p (quri:uri style))
            (nyxt/style-mode::open-or-cache-url (current-mode 'stylor) (style mapping))
            (style mapping)))
       (function (funcall style
                          (active-internal-theme nx-mapper:*user-settings*))))
     buffer))
  (alex:when-let* ((mapping (find-if (lambda (mapping)
                                       (funcall (eval (source mapping)) buffer))
                                            (scripts nx-mapper:*user-settings*)))
                   (script (script mapping)))
    (nyxt:ffi-buffer-evaluate-javascript-async
     buffer
     script)))

(defmethod initialize ((mode stylor-mode))
  (with-slots (auto-p) mode
    (unless (or (find (theme *browser*)
                      (internal-themes nx-mapper:*user-settings*) :test #'equal)
                (active-internal-theme nx-mapper:*user-settings*))
      (or (and auto-p
               (if (string= (uiop:getenv "GTK_THEME") ":light")
                   (select-internal-theme (nx-mapper:name (find-internal-variant)) mode)
                   (setf (nyxt::style (buffer mode))
                         (compute-buffer-style
                          (select-internal-theme (nx-mapper:name (find-internal-variant :dark t)) mode)))))
          (select-internal-theme
           (nx-mapper:name (car (internal-themes nx-mapper:*user-settings*))) mode)))
    (hooks:add-hook (nyxt:buffer-before-make-hook *browser*) #'internal-style-handler)
    (hooks:add-hook (nyxt:buffer-loaded-hook (buffer mode)) #'external-style-handler)))

(defmethod cleanup ((mode stylor-mode))
  (hooks:remove-hook (nyxt:buffer-loaded-hook (buffer mode)) #'external-style-handler)
  (hooks:remove-hook (nyxt:buffer-before-make-hook *browser*) #'internal-style-handler)
  (hooks:remove-hook (nyxt:prompt-buffer-make-hook *browser*) 'style-prompt-buffer))

(defmethod find-internal-variant (&key dark)
  "Finds the first light theme variant from MODE. If DARK, it finds the first dark theme."
  (if dark
      (find-if #'theme:dark-p (internal-themes nx-mapper:*user-settings*))
      (find-if-not #'theme:dark-p (internal-themes nx-mapper:*user-settings*))))

(defun internal-style-handler (buffer)
  "Handler function to re-calculate some styles in every new buffer."
  (setf (nyxt::style buffer) (compute-buffer-style
                              (active-internal-theme nx-mapper:*user-settings*)))
  (when (nyxt:find-submode buffer 'web-mode)
    (setf (nyxt/web-mode:box-style (nyxt:find-submode buffer 'web-mode))
          (compute-hint-style
           (active-internal-theme nx-mapper:*user-settings*)))))

(defun compute-buffer-style (theme)
  (str:concat
   (eval (get-original-interface-style 'nyxt:user-buffer nil t))
   (funcall (buffer-style (stylist theme)) theme)))

(defun compute-hint-style (theme)
  (str:concat
   (eval (get-original-interface-style 'nyxt/web-mode:web-mode 'nyxt/web-mode:box-style))
   (funcall (hint-style (stylist theme)) theme)))

(define-command-global select-internal-theme (&optional name (mode (current-mode 'stylor)))
  "Selects an `internal-theme' with NAME from MODE and applies it."
  (flet ((compute-prompt-style (theme)
           (str:concat
            (eval (get-original-interface-style
                   'nyxt:prompt-buffer))
            (funcall (prompt-style (stylist theme)) theme))))
    (let* ((theme (or (and name
                           (find name (internal-themes nx-mapper:*user-settings*)
                                 :key #'nx-mapper:name :test #'string=))
                      (nyxt:prompt1
                        :prompt "Select theme"
                        :sources (make-instance 'theme-source))))
           (status-style (str:concat
                          (eval (get-original-interface-style 'nyxt:status-buffer))
                          (funcall (status-style (stylist theme)) theme)))
           (message-style (str:concat
                           (eval (get-original-interface-style
                                  'nyxt:window 'nyxt:message-buffer-style))
                           (funcall (message-style (stylist theme)) theme))))
      (setf (active-internal-theme nx-mapper:*user-settings*) theme
            (theme *browser*) theme)
      (hooks:add-hook (nyxt:prompt-buffer-make-hook *browser*)
                      (make-instance
                       'hooks:handler
                       :fn (lambda (prompt)
                             (setf (nyxt:style prompt) (compute-prompt-style
                                                        (active-internal-theme nx-mapper:*user-settings*))))
                       :name 'style-prompt-buffer))
      (if (not (current-window))
          (hooks:add-hook (nyxt:window-make-hook *browser*)
                          (make-instance
                           'hooks:handler
                           :fn (lambda (window)
                                 (setf (nyxt::style (nyxt::status-buffer window)) status-style
                                       (nyxt:message-buffer-style window) message-style)
                                 (hooks:remove-hook (nyxt:window-make-hook *browser*)
                                                    'style-window-on-startup))
                           :name 'style-window-on-startup))
          (setf (nyxt::style (nyxt::status-buffer (current-window))) status-style
                (nyxt:message-buffer-style (current-window)) message-style))
      (loop for buffer in (nyxt:buffer-list)
            do (progn
                 (setf (nyxt::style buffer) (compute-buffer-style
                                             (active-internal-theme nx-mapper:*user-settings*)))
                 ;; (unless name
                 ;;   (nyxt:buffer-load (nyxt:url buffer) :buffer buffer))
                 (nyxt:buffer-load (nyxt:url buffer) :buffer buffer)))
      (when (nyxt:find-submode (buffer mode) 'web-mode)
        (setf (nyxt/web-mode:box-style (nyxt:find-submode (buffer mode) 'web-mode))
              (compute-hint-style
               (active-internal-theme nx-mapper:*user-settings*))))
      (nyxt::print-status)
      (nyxt::echo "")
      theme)))
