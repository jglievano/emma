(eval-when-compile
  (require 'use-package))
(require 'dash)
(require 'delight)
(require 'diminish)
(require 'bind-key)
(require 'with-editor)

(use-package ace-window
  :load-path "vendor/ace-window"
  :after avy
  :init
  (global-set-key (kbd "C-c .") 'ace-window))

(use-package anzu
  :load-path "vendor/emacs-anzu"
  :commands global-anzu-mode
  :config
  (global-anzu-mode +1))

(use-package avy
  :load-path "vendor/avy"
  :init
  (global-set-key (kbd "C-c ,") 'avy-goto-char-2))

(use-package company
  :load-path "vendor/company-mode"
  :commands global-company-mode
  :config
  (global-company-mode))

(use-package counsel
  :load-path "vendor/swiper"
  :bind (("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
	 ("C-c g" . counsel-git)
	 ("C-c j" . counsel-git-grep)
	 ("C-c k" . counsel-ag)
	 ("C-x l" . counsel-locate)
	 ("C-S-o" . counsel-rhythmbox)))

(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :load-path "vendor/exec-path-from-shell"
    :commands exec-path-from-shell-initialize
    :config
    (exec-path-from-shell-initialize)))

(use-package flycheck
  :load-path "vendor/flycheck"
  :commands global-flycheck-mode
  :config
  (global-flycheck-mode))

(use-package flx
  :load-path "vendor/flx"
  :init
  (require 'flx-ido))

(use-package go-mode
  :load-path "vendor/go-mode.el"
  :mode "\\.go\\'"
  :interpreter ("go" . go-mode))

(use-package ivy
  :load-path "vendor/swiper"
  :after counsel
  :diminish ivy-mode
  :bind (("C-c C-r" . ivy-resume)
	 ("C-x b" . ivy-switch-buffer))
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

(use-package json-mode
  :load-path "vendor/json-mode"
  :mode "\\.json\\'")

(use-package js2-mode
  :load-path "vendor/js-mode"
  :mode "\\.js\\'"
  :interpreter ("node" . js2-mode)
  :config
  (add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2))))

(use-package magit
  :load-path "vendor/magit/lisp"
  :commands magit-status
  :init
  (require 'magit)
  (with-eval-after-load 'info
    (info-initialize)
    (add-to-list 'Info-directory-list
		 "~/.emacs.d/vendor/magit/Documentation/"))
  :config
  (global-set-key (kbd "C-c g") 'magit-status))

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

(use-package rust-mode
  :load-path "vendor/rust-mode"
  :mode "\\.rs\\'"
  :init
  (defun emma-rust-setup ()
    (setq-local rust-indent-offset 2))
  (add-hook 'rust-mode-hook #'emma-rust-setup))

(use-package scss-mode
  :load-path "vendor/scss-mode"
  :mode "\\.scss\\'")

(use-package swiper
  :load-path "vendor/swiper"
  :after ivy
  :bind (("C-s" . swiper)
	 ("C-r" . swiper)))

(use-package toml-mode
  :load-path "vendor/toml-mode.el"
  :mode "\\.toml\\'")

(use-package web-mode
  :load-path "vendor/web-mode"
  :mode (("\\.phtml\\'" . web-mode)
	 ("\\.tpl\\.php\\'" . web-mode)
	 ("\\.[agj]sp\\'" . web-mode)
	 ("\\.as[cp]x\\'" . web-mode)
	 ("\\.erb\\'" . web-mode)
	 ("\\.mustache\\'" . web-mode)
	 ("\\.djhtml\\'" . web-mode)
	 ("\\.html?\\'" . web-mode))
  :config
  (defun my-web-mode-hook ()
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2))
  (add-hook 'web-mode-hook 'my-web-mode-hook))

(use-package which-key
  :load-path "vendor/which-key"
  :init
  (require 'which-key)
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.2))

(provide 'emma-packages)
