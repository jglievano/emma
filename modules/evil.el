(use-package evil
  :demand t
  :init
  (setq evil-want-C-u-scroll t
	evil-want-visual-char-semil-exclusive t
	evil-want-Y-yank-to-eat t
	evil-magic t
	evil-echo-state t
	evil-indent-convert-tabs t
	evil-ex-search-vim-style-regexp t
	evil-ex-substitute-global t
	evil-ex-visual-char-range t
	evil-insert-skip-empty-lines t
	evil-mode-line-format 'nil
	evil-symbol-word-search t
	shift-select-mode nil)
  :config
  (add-hook 'after-init-hook #'evil-mode)
  (evil-select-search-module 'evil-search-module 'evil-search)
  (define-key evil-motion-state-map "\\" nil)

  (dolist (mode '(tabulated-list-mode view-mode comint-mode term-mode
				      calendar-mode Man-mode grep-mode))
    (evil-set-initial-state mode 'emacs))
  (dolist (mode '(help-mode debugger-mode))
    (evil-set-initial-state mode 'normal)))
