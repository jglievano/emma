;;; core/core-ui.el -*- lexical-binding: t; -*-

;; UI.
(setq-default
 bidi-display-reordering nil
 blink-matching-paren nil
 cursor-in-non-selected-windows nil
 display-line-numbers-width 3
 frame-inhibit-implied-resize t
 fringe-indicator-alist (delq (assq 'continuation fringe-indicator-alist)
			      fringe-indicator-alist)
 highlight-nonselected-windows nil
 image-animate-loop t
 indicate-buffer-boundaries nil
 indicate-empty-lines nil
 max-mini-window-height 0.3
 mode-line-default-help-echo nil
 mouse-yank-at-point t
 resize-mini-windows 'grow-only
 show-help-function nil
 split-width-threshold 160
 uniquify-buffer-name-style 'forward
 use-dialog-box nil
 visible-cursor nil
 x-stretch-cursor nil
 jit-lock-defer-time nil
 jit-lock-stealth-nice 0.1
 jit-lock-stealth-time 0.2
 jit-lock-stealth-verbose nil
 pos-tip-internal-border-width 6
 pos-tip-border-width 1
 ring-bell-function #'ignore
 visible-bell nil)

(fset #'yes-or-no-p #'y-or-n-p)

(defun emma|enable-ui-keystrokes () (setq echo-keystrokes 0.02))
(defun emma|disable-ui-keystrokes () (setq echo-keystrokes 0))
(emma|enable-ui-keystrokes)
(add-hook 'isearch-mode-hook #'emma|disable-ui-keystrokes)
(add-hook 'isearch-mode-end-hook #'emma|enable-ui-keystrokes)

;; TODO: init-ui-hook
(setq show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t)
(add-hook 'after-init-hook #'show-paren-mode)

(setq-default window-divider-default-places t
	      window-divider-default-bottom-width 0
	      window-divider-default-right-width 1)
(add-hook 'after-init-hook #'window-divider-mode)

(global-eldoc-mode -1)
(add-hook 'after-init-hook #'blink-cursor-mode)
(if (fboundp 'fringe-mode) (fringe-mode '4))
(tooltip-mode -1)
(menu-bar-mode -1)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(defun emma|no-fringes-in-minibuffer ()
  (set-window-fringes (minibuffer-window) 0 0 nil))
(add-hook 'after-init-hook #'emma|no-fringes-in-minibuffer)
(add-hook 'minibuffer-setup-hook #'emma|no-fringes-in-minibuffer)

(use-package all-the-icons
  :commands (all-the-icons-octicon all-the-icons-faicon all-the-icons-fileicon
             all-the-icons-wicon all-the-icons-material all-the-icons-alltheicon
             all-the-icons-install-fonts)
  :init
  (defun emma*disable-all-the-icons-in-tty (orig-fn &rest args)
    (when (display-graphic-p)
      (apply orig-fn args)))
  (advice-add #'all-the-icons-octicon :around #'emma*disable-all-the-icons-in-tty
  )
  (advice-add #'all-the-icons-material :around #'emma*disable-all-the-icons-in-tty)
  (advice-add #'all-the-icons-faicon :around #'emma*disable-all-the-icons-in-tty)
  (advice-add #'all-the-icons-fileicon :around #'emma*disable-all-the-icons-in-tty)
  (advice-add #'all-the-icons-wicon :around #'emma*disable-all-the-icons-in-tty)
  (advice-add #'all-the-icons-alltheicon :around #'emma*disable-all-the-icons-in-tty))

(use-package fringe-helper
  :commands (fringe-helper-define fringe-helper-convert)
  :init
  (unless (fboundp 'define-fringe-bitmap)
    (defun define-fringe-bitmap (&rest _))))

(use-package hideshow
  :commands (hs-minor-mode hs-toggle-hiding hs-already-hidden-p)
  :config
  (setq hs-hide-comments-when-hiding-all nil))

(use-package highlight-indentation
  :commands (highlight-indentation-mode highlight-indentation-current-column-mode))

(use-package highlight-numbers
  :commands highlight-numbers-mode)

(use-package hl-line
  :init
  (add-hook 'prog-mode #'hl-line-mode)
  (add-hook 'text-mode #'hl-line-mode)
  (add-hook 'conf-mode #'hl-line-mode)
  :config
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil)
  (with-eval-after-load "evil"
    (defun emma|disable-hl-line () (hl-line-mode -1))
    (add-hook 'evil-visual-state-entry-hook #'emma|disable-hl-line)
    (add-hook 'evil-visual-state-exit-hook #'hl-line-mode)))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package vi-tilde-fringe
  :commands (global-vi-tilde-fringe-mode vi-tilde-fringe-mode)
  :init
  (add-hook 'after-init-hook #'global-vi-tilde-fringe-mode))

(use-package visual-fill-column
  :commands visual-fill-column-mode
  :config
  (setq-default visual-fill-column-center-text nil
                visual-fill-column-width fill-column))

(load-theme 'emma t)

(set-face-attribute 'default nil :height 150 :family "Operator Mono" :weight 'light)
(set-face-attribute 'comint-highlight-prompt nil :inherit nil)

(provide 'core-ui)
