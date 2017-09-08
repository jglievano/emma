(use-package projectile
  :demand t
  :init (add-hook 'after-init-hook #'projectile-mode)
  :config
  (setq projectile-cache-file (concat emma-local-dir "projectile.cache")))

(provide 'core-projects)
