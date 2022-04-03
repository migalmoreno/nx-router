(in-package #:nx-mapper)

(define-class settings (nx-mapper/stylor-mode:user-settings
                        nx-mapper/rural-mode:user-settings)
  ()
  (:export-class-name-p t))
(define-user-class settings)

(sera:export-always 'make-mapping)
(defun make-mapping (name source &rest extra-slots &key &allow-other-keys)
  "Constructs a `nx-mapper' mapping. NAME and SOURCE are required and EXTRA-SLOTS
can vary depending on how modular one wants their mapping to be."
  (cond
    ((some (lambda (slot)
             (member slot (list :redirect :blocklist :external :media-p)))
           extra-slots)
     (apply #'make-instance 'nx-mapper/rural-mode:url-mapping :name name :source source extra-slots))
    ((member :style extra-slots)
     (apply #'make-instance 'nx-mapper/stylor-mode:external-theme :name name :source source extra-slots))
    ((member :script extra-slots)
     (apply #'make-instance 'nx-mapper/stylor-mode:user-script :name name :source source extra-slots))
    (t
     (apply #'make-instance 'nx-mapper/stylor-mode:internal-theme :name name extra-slots))))

(sera:export-always 'add-mapping)
(defun add-mapping (type name &key (filter-fn nil) (direct-p nil))
  "Interactively adds a new mapping of class TYPE to the extension's user settings slot NAME.
Optionally, specify how to  filter out each of the class slots with FILTER-FN, and specify
DIRECT-P to only get prompted for the direct slots."
  (let* ((computed-slots
           (if filter-fn
               (remove-if-not filter-fn
                              (if direct-p
                                  (sb-mop:class-direct-slots (find-class type))
                                  (sb-mop:class-slots (find-class type))))
               (if direct-p
                   (sb-mop:class-direct-slots (find-class type))
                   (sb-mop:class-slots (find-class type)))))
         (slots (mapcar (lambda (slot)
                          (let* ((slot-name (sb-mop:slot-definition-name slot))
                                 (formatted-name (intern (symbol-name slot-name))))
                            ;; When adding a source slot, prompt the user for the type of predicate, and then
                            ;; construct the source with it and the url, use data-path-source
                            ;; When the slot has type boolean or ends in -p, prompt for yes or no sources
                            (etypecase (sb-mop:slot-definition-type slot)
                              (boolean
                               (cons (intern (symbol-name slot-name) "KEYWORD")
                                     (nyxt:prompt1 :prompt formatted-name
                                                   :sources (make-instance 'prompter:yes-no-source))))
                              (t
                               (cons (intern (symbol-name slot-name) "KEYWORD")
                                     (nyxt:prompt1 :prompt formatted-name
                                                   :sources (make-instance 'prompter:raw-source)))))))
                        computed-slots)))
    (nconc
     (funcall
      (sb-mop:slot-definition-name
       (find name (sb-mop:class-slots (find-class 'nx-mapper:settings))
            :key #'sb-mop:slot-definition-name))
      nx-mapper:*user-settings*)
     (list
      (eval
       `(make-instance ',type
                       ,@(alex:flatten slots)))))))

(defun retrieve-instance-slots (class instance &key (direct-p nil))
  "Returns the slot information as it was provided to create the INSTANCE from CLASS.
If DIRECT-P, only return the direct slots from CLASS."
  (let ((slots
          (mapcar
           (lambda (slot)
             (let ((slot-initarg
                     (intern
                      (symbol-name (sb-mop:slot-definition-name slot)) "KEYWORD"))
                   (computed-slot (funcall (sb-mop:slot-definition-name slot) instance)))
               (if (typep (class-of computed-slot) 'standard-class)
                   (cons slot-initarg
                         (intern
                          (format nil "(make-instance '~(~s~))"
                                  (type-of computed-slot))))
                   (when computed-slot
                     (typecase computed-slot
                       (list
                        (list slot-initarg (intern (format nil "'~s" computed-slot))))
                       (function
                        (cons slot-initarg (funcall computed-slot instance)))
                       (quri:uri
                        (cons slot-initarg (nyxt:render-url computed-slot)))
                       (t
                        (cons slot-initarg computed-slot)))))))
           (sb-mop:class-slots (find-class class)))))
    (loop for (head . tail) in (delete nil slots)
          nconc (list head (if (listp tail)
                               (car tail)
                               tail)))))

(defun print-configuration (slots)
  (when slots
      (format
       nil "  (~(~a~) ~&    (list~&~{~a~^~&~}))"
       (format nil "~ss"(type-of (car slots)))
       (loop for slot in slots
             collect
             (format nil "      (make-instance '~(~s~) ~&        ~{~(~s~)~* ~:*~s~^~&        ~})"
                     (type-of slot)
                     (retrieve-instance-slots (type-of slot) slot))))))

(defun print-init-snippet ()
  "Prints the resulting code snippet to be placed in the user's
Nyxt initialization file."
  (let* ((internal-themes (nx-mapper/stylor-mode:internal-themes nx-mapper:*user-settings*))
         (external-themes (nx-mapper/stylor-mode:external-themes nx-mapper:*user-settings*))
         (url-assocs (nx-mapper/rural-mode:url-mappings nx-mapper:*user-settings*))
         (generated-code (format nil "(define-configuration nx-mapper:settings ~&~@{~a~^~&~})"
                                 (print-configuration internal-themes)
                                 (print-configuration external-themes)
                                 (print-configuration url-assocs))))
    (spinneret:with-html-string
      (:aside
       :style (cl-css:inline-css
               '(:width "50%" :padding "0 2 rem"
                 :overflow-y "scroll"))
       (:h1 (:code "nx-mapper") " initialization snippet")
       (:p
        "After customizing, one is to put this code snippet into their Nyxt init file at "
        (:code (format nil "~a" (nfiles:expand nyxt:*init-file*))) ".")
       (:div (:button :class "button"
                      :onclick (ps:ps (nyxt/ps:lisp-eval
                                       `(progn
                                          (nyxt:copy-to-clipboard ,generated-code)
                                          (nyxt:echo "Copied to clipboard!"))))
                      "+ Copy ")
             (:button :class "button"
                         :onclick (ps:ps (nyxt/ps:lisp-eval
                                          `(reset-configuration)))
                         "🔁 Reset configuration"))
       (:pre (:code generated-code))))))

(defun show-internal-themes-configuration ()
  "Shows the section about Internal Themes."
  (spinneret:with-html-string
    (:h2 "Internal themes")
    (:p "The following themes tweak the appearance of the browser interface.")
    ;; TODO: Add sun or moon icon depending if the theme is dark or not
    (:div
     (:button :class "button"
              :onclick (ps:ps (nyxt/ps:lisp-eval '(nx-mapper:add-mapping
                                                   'nx-mapper/stylor-mode:internal-theme
                                                   'nx-mapper/stylor-mode:internal-themes)))
              "+ Add theme"))
    (alex:if-let ((internal-themes (nx-mapper/stylor-mode:internal-themes
                                    nx-mapper:*user-settings*)))
      (dolist (theme internal-themes)
        (let ((theme-name (nx-mapper:name theme))
              (global-theme (nyxt:theme nyxt:*browser*))
              (color-slots (remove-if-not
                            (lambda (slot)
                              (str:containsp "color" (str:downcase slot)))
                            (mapcar (lambda (slot)
                                      (sb-mop:slot-definition-name slot))
                                    (sb-mop:class-direct-slots (find-class 'theme:theme))))))
          (:div
           (:h2 (str:concat theme-name
                            (when (eq global-theme theme)
                              " *")
                            (if (theme:dark-p theme)
                                " 🌚" " 🌞")))
           (:table :style (format
                           nil
                           "table-layout: fixed; width: 100%; margin: 10px 0; border: 1px solid ~a"
                           (if (theme:dark-p global-theme)
                               (theme:tertiary-color global-theme)
                               (theme:text-color global-theme)))
                   (loop for slot in color-slots
                         collect (:tr
                                  (:td :style (cl-css:inline-css '(:padding "10px"))
                                       (car (str:split "-" (str:capitalize (string slot)))))
                                  ;; Once the color picker is chosen, add a button which lets us
                                  ;; edit the color from this table data
                                  (:td :style (cl-css:inline-css
                                               `(:background ,(funcall slot theme)
                                                 :padding "25px" :width "50%"))))))
           (:button :class "button"
                    :onclick (ps:ps (nyxt/ps:lisp-eval
                                     `(nx-mapper/stylor-mode:select-internal-theme ,theme-name)))
                    "💾 Load theme")
           (:button :class "button"
                    :onclick (ps:ps (nyxt/ps:lisp-eval
                                     `(delete-mapping
                                       'nx-mapper/stylor-mode:internal-theme
                                       'nx-mapper/stylor-mode:internal-themes
                                       ,theme-name)))
                    "- Delete theme")))))
    (:p "No browser themes specified.")))

(defun show-external-themes-configuration (buffer)
  "Displays the section to do with external themes in BUFFER."
  (spinneret:with-html-string
    (:h2 "External Themes")
    (:p "These allow you to set rules for which to apply CSS styles to arbitrary sources.")
    (:div
     (:button :class "button"
              :onclick (ps:ps (nyxt/ps:lisp-eval '(nx-mapper:add-mapping
                                                   'nx-mapper/stylor-mode:external-theme
                                                   'nx-mapper/stylor-mode:external-themes)))
              "+ Add theme"))
    (alex:if-let ((external-themes (nx-mapper/stylor-mode:external-themes
                                    nx-mapper:*user-settings*)))
      (dolist (theme external-themes)
        (let ((name (nx-mapper:name theme))
              (style-mapping (nx-mapper/stylor-mode:style theme))
              (sources (nx-mapper/stylor-mode:source theme)))
          (:div
           (:h3 name)
           (:raw (nx-mapper::show-mapping-sources sources))
           (typecase style-mapping
             (quri:uri
              (:pre (:code (nyxt/style-mode::open-or-cache-url (nyxt:find-mode buffer 'stylor-mode)
                                                               (nyxt:render-url style-mapping)))))
             (pathname
              (:pre (:code (uiop:read-file-string style-mapping))))
             (function (:pre
                        (:code
                         (if (nx-mapper/stylor-mode:active-internal-theme
                               nx-mapper:*user-settings*)
                             (funcall style-mapping (nx-mapper/stylor-mode:active-internal-theme
                                                     nx-mapper:*user-settings*))
                             (funcall style-mapping (nyxt:theme nyxt:*browser*))))))
             (t (:pre (:code style-mapping))))
           (:div
            (:button
             :class "button"
             :onclick (ps:ps (nyxt/ps:lisp-eval
                              '(progn
                                (setf sources
                                 (nyxt:prompt1
                                   :prompt "Source type:"
                                   :source (make-instance
                                            nx-mapper/stylor-mode::source-type))))))
             "Change sources")
            (typecase style-mapping
              (quri:uri
               (:button
                :class "button"
                :onclick (ps:ps (nyxt/ps:lisp-eval
                                 ;; Provide the value to be updated (i.e. the URL)
                                 `(nx-mapper/stylor-mode::update-style
                                   ,name
                                   :external-p t)))
                "Change CSS URI"))
              (string (:button :class "button"
                               :onclick (ps:ps (nyxt/ps:lisp-eval
                                                `(nyxt::%edit-with-external-editor ,style-mapping)))
                               "Edit CSS"))
              (function (:button :class "button"
                                 :onclick (ps:ps (nyxt/ps:lisp-eval
                                                  `(nx-mapper/stylor-mode::update-style
                                                    ,name
                                                    :external-p t)))
                                 "Edit CSS"))
              (t (:button :class "button"
                          :onclick (ps:ps (nyxt/ps:lisp-eval
                                           `(nyxt::%edit-with-external-editor ,style-mapping)))
                          "Edit CSS")))))))
      (:p "No external themes specified."))))

(defun show-scripts-configuration ()
  "Shows the section relevant to user scripts."
  (spinneret:with-html-string
    (:h2 "User Scripts")
    (:p "The following allow one to tweak the behavior of a site via JavaScript.")
    (:div
     (:button :class "button"
              :onclick (ps:ps (nyxt/ps:lisp-eval '(nx-mapper:add-mapping
                                                   'nx-mapper/stylor-mode:script
                                                   'nx-mapper/stylor-mode:scripts)))
              "+ Add script"))
    (alex:if-let ((scripts (nx-mapper/stylor-mode:scripts nx-mapper:*user-settings*)))
      (dolist (script scripts)
        (let ((name (nx-mapper:name script))
              (script-mapping (nx-mapper/stylor-mode:script script))
              (sources (delete nil (nx-mapper/stylor-mode:source script))))
          (:div
           (:h3 name)
           (:raw (nx-mapper::show-mapping-sources sources))
           (typecase script-mapping
             ;; (quri:uri
             ;;   (:pre (:code (nyxt/style-mode::open-or-cache-url (nyxt:find-mode buffer 'stylor-mode)
             ;;                                                   (nyxt:render-url style-mapping))))
             ;;  ;; (:p "The style is located at " (:a :href (nyxt:render-url style-mapping)
             ;;  ;;                                    (nyxt:render-url style-mapping)))
             ;;  )
             ;; (pathname
             ;;  (:pre (:code (uiop:read-file-string style-mapping))))
             ;; (function (:pre
             ;;             (:code
             ;;              (funcall style-mapping (nx-mapper/stylor-mode:active-internal-theme
             ;;                                      nx-mapper:*user-settings*)))))
             (t (:pre (:code script-mapping)))))))
      (:p "No user scripts specified."))))

;; TODO: add buttons to edit all fields of a mapping and send these per value
(defun show-url-assocs-configuration ()
  "Shows the section to do with URL mappings."
  (spinneret:with-html-string
    (:h2 "URL Associations")
    (:p "The following allow you to provide behavior for certain sources, such as redirect or block.")
        (:div
         (:button :class "button"
                  :onclick (ps:ps (nyxt/ps:lisp-eval '(nx-mapper:add-mapping
                                                       'nx-mapper/rural-mode:url-mapping
                                                       'nx-mapper/rural-mode:url-mappings)))
                  "+ Add URL mapping"))
    (alex:if-let ((url-assocs (nx-mapper/rural-mode:url-mappings nx-mapper:*user-settings*)))
      (dolist (url-mapping url-assocs)
        (let ((name (nx-mapper:name url-mapping))
              (sources (delete nil (nx-mapper/rural-mode:source url-mapping))))
          (:div
           (:h3 name)
           (:p "Invoked with one of these triggers, and type of matching.")
           (:raw (nx-mapper::show-mapping-sources sources)))))
      (:p "No URL mappings specified."))))

(defun show-mapping-sources (sources)
  "Styles SOURCES to be displayed on the customization page."
  (spinneret:with-html-string
    (if (nx-mapper::list-of-lists-p sources)
        (loop for (predicate . urls) in sources
              collect
              (:div
               (:ul
                (loop for url in urls
                      collect
                      (:li
                       (:p (:a :style "text-decoration: underline" url)
                           (:span (format nil " (~a)"
                                          (cadr (str:split "-" (symbol-name predicate)))))))))))
        (:ul
         (:li
          (:p (:a :style "text-decoration:underline" (cadr sources))
              (:span (format nil " (~a)"
                             (cadr (str:split "-" (symbol-name (car sources))))))))))))

(nyxt::define-internal-page-command-global customize-mappings ()
  (buffer "*nx-mapper settings*" 'nyxt:base-mode)
  "Displays an extension customization page."
  (spinneret:with-html-string
    (:style (str:concat
             (nyxt:style buffer)
             (cl-css:css
              '(("#settings-container"
                 :display "flex"
                 :width "100%"
                 :height "100%"
                 :overflow "hidden"
                 :flex-direction "row"
                 :justify-content "center")
                ("#settings-pane"
                 :width "50%"
                 :overflow-y "scroll"
                 :padding "0 2rem")))))
    (:div :id "settings-container"
     (:raw (nx-mapper::print-init-snippet))
     (:div :id "settings-pane"
           (:h1 "Settings")
           (:p (format nil "Welcome back, ~:(~a~), these are your nx-mapper settings."
                       (uiop:getenv "USER")))
           (:raw (nx-mapper::show-internal-themes-configuration))
           (:hr)
           (:raw (nx-mapper::show-external-themes-configuration buffer))
           (:hr)
           (:raw (nx-mapper::show-scripts-configuration))
           (:hr)
           (:raw (nx-mapper::show-url-assocs-configuration))))))
