;;; core/autoload/help.el -*- lexical-binding: t; -*-

;;;###autoload
(defun njord/describe-setting (setting)
  "Open the documentation of SETTING (a keyword defined with `def-setting!')."
  (interactive
   ;; TODO try to read setting from whole line
   (list (completing-read "Describe setting%s: "
                          (mapcar #'car njord-settings)
                          nil t nil nil)))
  (let ((fn (cdr (assq (intern setting) njord-settings))))
    (unless fn
      (error "'%s' is not a valid NJORD setting" setting))
    (describe-function fn)))

;;;###autoload
(defun njord/describe-module (module)
  "Open the documentation of MODULE (a string that represents the category and
submodule in the format, e.g. ':feature evil')."
  (interactive
   ;; TODO try to read module from whole line
   (list (completing-read "Describe module: "
                          (cl-loop for (module . sub) in (reverse (hash-table-keys njord-modules))
                                   collect (format "%s %s" module sub))
                          nil t)))
  (cl-destructuring-bind (category submodule)
      (mapcar #'intern (split-string module " "))
    (unless (member (cons category submodule) (njord--module-pairs))
      (error "'%s' isn't a valid module" module))
    (let ((doc-path (expand-file-name "README.org" (njord-module-path category submodule))))
      (unless (file-exists-p doc-path)
        (error "There is no documentation for this module"))
      (find-file doc-path))))
