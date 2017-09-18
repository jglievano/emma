;;; mod-coding.el --- Programming languages. -*- lexical-binding: t; -*-

;;; Commentary:
;; Set up custom programming language conguration.

;;; Code:

(use-package conf-mode
  :mode (("offlineimaprc\\'" . conf-mode)
         ("muttrc\\'" . conf-mode)
         ("msmtprc\\'" . conf-mode)
         ("\\.conf\\'" . conf-mode)))

(use-package fish-mode
  :mode "\\.fish\\'")

(use-package go-mode
  :mode "\\.go\\'"
  :interpreter ("go" . go-mode))

(use-package json-reformat :defer t)
(use-package json-snatcher :defer t)
(use-package json-mode
  :mode "\\.json\\'"
  :config
  (add-hook 'json-mode-hook
            (lambda ()
              (make-local-variable 'js-indent-level)
              (setq js-indent-level 2))))
(use-package js2-mode
  :mode "\\.js\\'"
  :interpreter ("node" . js2-mode)
  :config
  (add-hook 'js2-mode-hook
            (lambda ()
              (setq js2-basic-offset 2))))

(use-package paredit
  :diminish paredit-mode)
(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing on Lisp mode." t)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook #'enable-paredit-mode)

(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package php-mode
  :mode "\\.php\\'"
  :init
  (defun emma-php-setup ()
    (setq tab-width 2
          indent-tabs-mode nil)
    (set (make-local-variable 'show-trailing-whitespace) t)
    (add-hookk 'before-saving-hook 'delete-trailing-whitespace nil t)
    (c-set-style "drupal"))
  (add-hook 'php-mode-hook #'emma-php-setup))

(use-package python-mode
  :mode (("\\.py\\'" . python-mode)
         ("BUILD\\'" . python-mode)
         ("WORKSPACE\\'" . python-mode)))

(use-package rust-mode
  :mode "\\.rs\\'"
  :init
  (defun emma-rust-setup ()
    (setq-local rust-indent-offset 2))
  (add-hook 'rust-mode-hook #'emma-rust-setup))

(use-package scss-mode :mode "\\.scss\\'")

(use-package toml-mode :mode "\\.toml\\'")

(use-package web-mode
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("\\.njk\\'" . web-mode)
         ("\\.hbs\\'" . web-mode))
  :config
  (defun emma-web-mode-hook ()
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2))
  (add-hook 'web-mode-hook 'emma-web-mode-hook))

(provide 'mod-coding)
;;; mod-coding.el ends here
