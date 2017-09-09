;;; mod-projectile.el --- Projectile mostly. -*- lexical-binding: t; -*-

;;; Commentary:
;; projectile loading and configuration.

;;; Code:

(use-package projectile
  :demand t
  :diminish projectile-mode
  :config (projectile-global-mode))

(provide 'mod-projectile)
;;; mod-projectile.el ends here
