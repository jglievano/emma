;; navigation.el --- Navigation settings. -*- lexical-binding: t; -*-

;;; Commentary:
;; These are settings related to how you navigate with Emma.

;;; Code:

(use-package ido)
(use-package ido-completing-read+ :after ido)
(use-package flx-ido :after ido
  :config
  (setq ido-enable-prefix nil
	ido-enable-flex-matching t
	ido-create-new-buffer 'always
	ido-use-filename-at-point 'guess
	ido-use-faces nil
	ido-max-prospects 10
	ido-max-directory-size 100000
	ido-save-directory-list-file (concat emma-local-dir "ido.hist")
	ido-default-file-method 'selected-window
	ido-default-buffer-method 'selected-window
	ido-auto-merge-work-directories-length -1)
  (ido-mode 1)
  (ido-everywhere 1)
  (ido-ubiquitous-mode 1)

  (flx-ido-mode 1))
(use-package smex
  :demand t
  :config
  (setq smex-save-file (concat emma-local-dir ".smex-items"))
  (smex-initialize))

(use-package which-key
  :init
  (require 'which-key)
  (which-key-mode)
  :config (setq which-key-idle-delay 0.5))

(provide 'navigation)
;;; navigation.el ends here
