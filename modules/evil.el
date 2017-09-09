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
  :config (evil-mode 1))

(use-package evil-leader
  :after evil
  :demand t
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>"))
