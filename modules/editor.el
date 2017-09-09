;; editor.el --- Editing settings. -*- lexical-binding: t; -*-

;;; Commentary:
;; Settings related to editing text and code with Emma.

;;; Code:

(setq-default
 indent-tabs-mode nil
 tab-width 2
 c-basic-offset 2
 css-indent-offset 2
 sh-basic-offset 2
 sh-indentation 2)

(use-package company
  :config (global-company-mode))

(use-package flycheck
  :config (global-flycheck-mode))

(use-package counsel :disabled)
(use-package ivy :after counsel :disabled :diminish ivy-mode)
(use-package swiper :after ivy :demand t
  :bind (("C-s" . swiper)
	 ("C-r" . swiper)))

(provide 'editor)
;;; editor.el ends here
