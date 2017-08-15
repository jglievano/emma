(defun load-vendor-dep (dep)
  (add-to-list 'load-path (expand-file-name dep "~/.emacs.d/vendor")))

(add-to-list 'load-path (expand-file-name "vendor" user-emacs-directory))
(load-vendor-dep "dash")
(load-vendor-dep "diminish")
(load-vendor-dep "use-package")
(load-vendor-dep "rich-minority")
(load-vendor-dep "with-editor")

(eval-when-compile
  (require 'use-package))
(require 'dash)
(require 'delight)
(require 'diminish)
(require 'bind-key)
(require 'with-editor)

(use-package autothemer
  :load-path "vendor/autothemer")

(use-package anzu
  :load-path "vendor/emacs-anzu"
  :commands global-anzu-mode
  :config
  (global-anzu-mode +1))

(use-package flx
  :load-path "vendor/flx"
  :init
  (require 'flx-ido))

(use-package which-key
  :load-path "vendor/which-key"
  :init
  (require 'which-key)
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.2))

(use-package projectile
  :load-path "vendor/projectile"
  :diminish projectile-mode
  :commands projectile-global-mode
  :defer 5
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-global-mode))

(use-package rainbow-delimiters
  :load-path "vendor/rainbow-delimiters"
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package magit
  :load-path "vendor/magit/lisp"
  :init
  (require 'magit)
  (with-eval-after-load 'info
    (info-initialize)
    (add-to-list 'Info-directory-list
		 "~/.emacs.d/vendor/magit/Documentation/")))

(use-package php-mode
  :load-path "vendor/php-mode"
  :mode "\\.php\\'"
  :init
  (defun emma-php-setup ()
    (setq tab-width 2
          indent-tabs-mode nil)
    (set (make-local-variable 'show-trailing-whitespace) t)
    (add-hook 'before-saving-hook 'delete-trailing-whitespace nil t)
    (c-set-style "drupal"))
  (add-hook 'php-mode-hook #'emma-php-setup))

(use-package rust-mode
  :load-path "vendor/rust-mode"
  :mode "\\.rs\\'"
  :init
  (defun emma-rust-setup ()
    (setq-local rust-indent-offset 2))
  (add-hook 'rust-mode-hook #'emma-rust-setup))

(use-package smart-mode-line
  :load-path "vendor/smart-mode-line"
  :commands sml/setup
  :init
  (progn
    (setq sml/no-confirm-load-theme t)
    (sml/setup)))

(use-package toml-mode
  :load-path "vendor/toml-mode.el"
  :mode "\\.toml\\'")

(provide 'emma-packages)
