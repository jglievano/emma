(defun load-dep (dep)
  (add-to-list 'load-path (expand-file-name dep emma-deps)))

;; evil-mode.
(load-dep "evil")
(require 'evil)
(evil-mode 1)
(load-dep "evil-leader")
(require 'evil-leader)
(global-evil-leader-mode)

;; which-key-mode.
(load-dep "which-key")
(require 'which-key)
(which-key-mode)

;; projectile.
(load-dep "projectile")
(require 'projectile)
(projectile-mode)

(provide 'emma-dependencies)
