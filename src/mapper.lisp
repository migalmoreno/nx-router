(in-package #:nx-mapper)

(hooks:add-hook nyxt:*after-init-hook*
                (make-instance
                 'hooks:handler
                 :fn (lambda ()
                       (setf nx-mapper:*user-settings*
                             (make-instance 'nx-mapper:user-settings)))
                 :name 'apply-user-settings))

(sera:export-always 'delete-mapping)
(define-command-global delete-mapping (type name mapping-name)
  "Deletes the mapping of class TYPE with MAPPING-NAME from the
 global extension settings' slot NAME."
  (let* ((settings-slot (funcall
                         (sb-mop:slot-definition-name
                          (find name (sb-mop:class-slots (find-class 'nx-mapper:user-settings))
                                :key #'sb-mop:slot-definition-name))
                         nx-mapper:*user-settings*))
         (mapping (find mapping-name
                        (nx-mapper/stylor-mode:internal-themes nx-mapper:*user-settings*)
                        :key #'nx-mapper:name :test #'string=)))
    (when (nyxt:prompt1 :prompt (format nil "Are you sure you want to delete ~s?"
                                        (nx-mapper:name mapping))
                        :sources (make-instance 'prompter:yes-no-source))
      (setf settings-slot (delete mapping settings-slot))
      (nyxt:reload-current-buffer))))

(define-command-global reset-configuration ()
  "Resets user settings to the default values. This is useful to re-evaluate
your user settings in your init file and then reload them in the current Nyxt session."
  (let ((active-theme (nx-mapper/stylor-mode:active-internal-theme
                       nx-mapper:*user-settings*)))
    (setf nx-mapper:*user-settings*
          (make-instance 'nx-mapper:user-settings))
    (setf (nx-mapper/stylor-mode:active-internal-theme
           nx-mapper:*user-settings*)
          active-theme)
    (nyxt:reload-current-buffer)
    (nyxt:echo "Settings reset.")))
