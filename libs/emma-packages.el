(defun load-vendor-dep (dep)
  (add-to-list 'load-path (expand-file-name dep emma-vendor)))

(add-to-list 'load-path (expand-file-name "vendor" user-emacs-directory))
(load-vendor-dep "dash")
(load-vendor-dep "diminish")
(load-vendor-dep "use-package")
(load-vendor-dep "with-editor")

(eval-when-compile
  (require 'use-package))
(require 'dash)
(require 'delight)
(require 'diminish)
(require 'bind-key)
(require 'with-editor)

;; flx.
(use-package flx
  :load-path "vendor/flx"
  :init
  (require 'flx-ido))

;; evil-mode.
(use-package evil
  :load-path "vendor/evil"
  :init
  (require 'evil)
  (evil-mode 1))

(use-package evil-mode
  :load-path "vendor/evil-leader"
  :after evil
  :init
  (require 'evil-leader)
  (global-evil-leader-mode))

;; which-key-mode.
(use-package which-key
  :load-path "vendor/which-key"
  :init
  (require 'which-key)
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.2))

;; projectile.
(use-package projectile
  :load-path "vendor/projectile"
  :after evil-leader
  :delight '(:eval (concat " " (projectile-project-name)))
  :commands projectile-find-file
  :init
  (require 'projectile)
  (projectile-mode)
  (which-key-add-key-based-replacements "<SPC> p" "projectile")
  (evil-leader/set-key
    "pf" 'projectile-find-file))

;; magit.
(use-package magit
  :load-path "vendor/magit/lisp"
  :init
  (require 'magit)
  (with-eval-after-load 'info
    (info-initialize)
    (add-to-list 'Info-directory-list
		 "~/.emacs.d/vendor/magit/Documentation/"))
  (which-key-add-key-based-replacements "<SPC> g" "git")
  (evil-leader/set-key
    "gs" 'magit-status))

;; rust.
(use-package rust-mode
  :load-path "vendor/rust-mode"
  :mode "\\.rs\\'")

(provide 'emma-packages)
