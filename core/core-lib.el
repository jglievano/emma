;;; core/core-lib.el -*- lexical-binding: t; -*-

(defconst IS-MAC   (eq system-type 'darwin))
(defconst IS-LINUX (eq system-type 'gnu/linux))

(defmacro emma! (&rest modules)
  (dolist (m modules)
    (message "emma! %s" m)
    `(use-package ,m)))

(provide 'core-lib)
