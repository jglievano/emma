(defun load-vendor-dep (dep)
  (add-to-list 'load-path (expand-file-name dep emma-vendor)))

(load-vendor-dep "use-package")

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; flx.
(load-vendor-dep "flx")
(require 'flx-ido)

;; evil-mode.
(load-vendor-dep "evil")
(require 'evil)
(evil-mode 1)
(load-vendor-dep "evil-leader")
(require 'evil-leader)
(global-evil-leader-mode)

;; which-key-mode.
(load-vendor-dep "which-key")
(require 'which-key)
(which-key-mode)

;; projectile.
(load-vendor-dep "projectile")
(require 'projectile)
(projectile-mode)

(provide 'emma-packages)
