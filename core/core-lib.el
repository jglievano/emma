;;; core/core-lib.el --- Functions, macros and other utilities. -*- lexical-binding: t; -*-

;;; Commentary:
;; Utility functions.

;;; Code:

(defconst IS-MAC   (eq system-type 'darwin))
(defconst IS-LINUX (eq system-type 'gnu/linux))

(defmacro emma! (&rest modules)
  (dolist (m modules)
    (message "  emma! %s" m)
    (let ((module-path (concat emma-modules-dir
                               "mod-"
                               (symbol-name m)
                               ".el")))
      (load module-path))))

(provide 'core-lib)
;;; core-lib.el ends here
