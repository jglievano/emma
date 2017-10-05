;;; mod-ui.el --- UI settings. -*- lexical-binding: t; -*-

;;; Commentary:
;; This is were all UI settings are defined for Emma.

;;; Code:

(setq-default
 column-number-mode t
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

(if (fboundp 'fringe-mode) (fringe-mode '4))
(tooltip-mode -1)
(menu-bar-mode -1)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package all-the-icons :commands all-the-icons-install-fonts)

(use-package neotree
  :demand t
  :after all-the-icons
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(use-package all-the-icons-dired
  :after all-the-icons
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package powerline :defer t)

(use-package all-the-icons-spaceline :after spaceline)

(use-package spaceline
  :after powerline
  :demand t
  :config
  (spaceline-emacs-theme)
  (if (display-graphic-p)
      (setq-default mode-line-format
                    '("%e" (:eval (spaceline-ml-ati))))))

(provide 'mod-ui)
;;; mod-ui.el ends here
