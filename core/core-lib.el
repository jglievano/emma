;;; core/core-lib.el -*- lexical-binding: t; -*-

(defconst IS-MAC   (eq system-type 'darwin))
(defconst IS-LINUX (eq system-type 'gnu/linux))

(defmacro emma! (&rest modules)
  (dolist (m modules)
    (message "  emma! %s" m)
    (let ((module-path (concat emma-modules-dir (symbol-name m) ".el")))
      (setq inhibit-message t)
      (load module-path)
      (setq inhibit-message nil))))

(provide 'core-lib)
