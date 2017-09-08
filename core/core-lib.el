;;; core/core-lib.el -*- lexical-biding: t; -*-

(defconst IS-MAC   (eq system-type 'darwin))
(defconst IS-LINUX (eq system-type 'gnu/linux))

(defmacro emma! (&rest modules))

(provide 'core-lib)
