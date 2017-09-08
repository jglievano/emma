;;; core.el --- The key to her heart. -*- lexical-binding: t; -*-

;;; Naming conventions:
;;
;;   emma-...  public variables or non-interactive functions.
;;   emma--... private anything (non-interactive), unsafe for direct use.
;;   emma/...  an interactive function. Safe for M-x or keybinding.
;;   emma:...  an evil operator, motion or command.
;;   emma|...  hook function.
;;   emma*...  advising functions.
;;   ...!      a macro or function that configures Emma.
;;   %...      functions used for in-snippet logic.
;;   +...      any of the above but part of a module, e.g. `+emacs-lisp|init-hook'
;;

(add-to-list 'custom-theme-load-path emma-themes-dir)

(eval-when-compile (require 'core-lib))

;; Remove lisp/org package because we use our own.
(dolist (path load-path)
  (if (string-match-p (regexp-quote "lisp/org") path)
      (setq load-path (remove path load-path))))

(let ((default-directory emma-vendor-dir))
  (normal-top-level-add-subdirs-to-load-path))
(eval-when-compile (require 'org))
(eval-when-compile (require 'use-package))

(use-package async :defer t)
(use-package autothemer :defer t)
(use-package dash :defer t)
(use-package f :defer t)
(use-package rich-minority :defer t)
(use-package s :defer t)
(use-package with-editor :defer t)
(if IS-MAC (use-package exec-path-from-shell
	     :commands exec-path-from-shell-initialize
	     :config (exec-path-from-shell-initialize)))

(unless (boundp 'display-line-numbers)
  (use-package nlinum :defer t)
  (use-package nlinum-hl :defer t)
  (use-package nlinum-relative :defer t))
(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))
(use-package vi-tilde-fringe :defer t)
(use-package visual-fill-column :defer t)

;; UTF-8 as the default coding system.
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

(setq-default
 package-user-dir emma-vendor-dir
 ad-redefinition-action 'accept ;; silence advised function warnings.
 apropos-do-all t               ;; make `apropos' more useful.
 compilation-always-kill t      ;; kill compile process before starting another.
 compilation-ask-about-save nil ;; save all buffers on `compile'.
 compilation-scroll-output t
 confirm-nonexistent-file-or-buffer t
 enable-recursive-minibuffers nil
 debug-on-error (and (not noninteractive) emma-debug-mode)
 idle-update-delay 2            ;; update UI less often.
 ;; keep the point out of the minibuffer.
 minibuffer-prompt-properties '(read-only t
                                          point-entered minibuffer-avoid-prompt
                                          face minibuffer-prompt)
 auto-save-default nil
 create-lockfiles nil
 history-length 500
 make-backup-files nil
 abbrev-file-name (concat emma-local-dir "abbrev.el")
 auto-save-list-file-name (concat emma-local-dir "autosave")
 backup-directory-alist (list (cons "." (concat emma-local-dir "backup/")))
 pcache-directory (concat emma-local-dir "pcache/")
 server-auth-dir (concat emma-local-dir "server/")
 shared-game-score-directory (concat emma-local-dir "shared-game-score/")
 tramp-auto-save-directory (concat emma-local-dir "tramp-auto-save/")
 tramp-backup-directory-alist backup-directory-alist
 tramp-persistency-file-name (concat emma-local-dir "tramp-persistency.el")
 url-cache-directory (concat emma-local-dir "url/")
 url-configuration-directory (concat emma-local-dir "url/"))

;; be quiet at startup.
(advice-add #'display-startup-echo-area-message :override #'ignore)
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'org-mode
      initial-scratch-message nil
      mode-line-format nil)

(setq-default
 vc-follow-symlinks t
 save-interprogram-paste-before-kill t
 bookmark-default-file (concat emma-local-dir "bookmarks")
 bookmark-save-flag t
 delete-trailing-lines nil
 fill-column 80
 sentence-end-double-space nil
 word-wrap t
 hscroll-margin 1
 hscroll-step 1
 scroll-conservatively 1001
 scroll-margin 0
 scroll-preserve-screen-position t
 indent-tabs-mode nil
 require-final-newline t
 tab-always-indent t
 tab-width 2
 c-basic-offset 2
 css-indent-offset 2
 sh-basic-offset 2
 sh-indentation 2
 whitespace-line-column fill-column
 whitespace-style
 '(face indentation tabs tab-mark spaces space-mark newline newline-mark
	trailing lines-tail)
 whitespace-display-mappings
 '((tab-mark ?\t [?› ?\t])
   (newline-mark ?\n [?¬ ?\n])
   (space-mark ?\  [?·] [?.])))

(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)
;;(electric-indent-mode -1)

(setq savehist-file (concat emma-local-dir "savehist")
      savehist-save-minibuffer-history t
      savehist-autosave-interval nil
      savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
      save-place-file (concat emma-local-dir "saveplace"))
(add-hook 'after-init-hook #'savehist-mode)
(add-hook 'after-init-hook #'save-place-mode)

(use-package recentf
  :init (add-hook 'after-init-hook #'recentf-mode)
  :config
  (setq recentf-save-file (concat emma-local-dir "recentf")
	recentf-max-menu-items 0
	recentf-max-saved-items 300
	recentf-filename-handlers '(abbreviate-file-name)
	recentf-exclude
	(list "^/tmp/" "^/ssh:" "\\.?ido\\.last$" "\\.revive$" "/TAGS$"
	      "^/var/folders/.+$")))

(use-package editorconfig
  :disabled ;; TODO: demand t after remove emacs-lisp-mode and lisp-mode.
  :mode ("\\.?editorconfig$" . editorconfig-conf-mode)
  :config
  (add-hook 'after-init-hook #'editorconfig-mode))

(use-package smartparens
  :demand t
  :config
  (setq sp-autowrap-region nil
	sp-highlight-pair-overlay nil
	sp-cancel-autoskip-on-backward-movement nil
	sp-show-pair-delay 0
	sp-max-pair-length 3)

  (add-hook 'after-init-hook #'smartparens-global-mode)
  (require 'smartparens-config)

  (add-hook 'evil-replace-state-entry-hook #'turn-off-smartparens-mode)
  (add-hook 'evil-replace-state-exit-hook #'turn-on-smartparens-mode)

  (sp-local-pair '(xml-mode nxml-mode php-mode) "<!--" "-->"
		 :post-handlers '(("| " "SPC"))))

(use-package ace-link
  :commands (ace-link-help ace-link-org))

(use-package ace-window
  :commands (ace-window ace-swap-window ace-delete-window
			ace-select-window ace-delete-other-windows)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
	aw-scope 'frame
	aw-background t))

(use-package avy
  :commands (avy-goto-char-2 avy-goto-line)
  :config
  (setq avy-all-windows nil
	avy-background t))

(use-package command-log-mode
  :commands (command-log-mode global-command-log-mode)
  :config
  (setq command-log-mode-auto-show t
	command-log-mode-open-log-turns-on-mode t))

(use-package expand-region
  :commands (er/expand-region er/contract-region er/mark-symbol er/mark-word))

(use-package help-fns+
  :commands (describe-buffer describe-command describe-file
			     describe-keymap describe-option describe-option-of-type))

(use-package imenu-anywhere
  :commands (ido-imenu-anywhere ivy-imenu-anywhere))

(use-package imenu-list
  :commands imenu-list-minor-mode)

(use-package pcre2el
  :commands rxt-quote-pcre)

(use-package smart-forward
  :commands (smart-up smart-down smart-backward smart-forward))

(use-package wgrep
  :commands (wgrep-setup wgrep-change-to-wgrep-mode)
  :config
  (setq wgrep-auto-save-buffer t))

(eval-when-compile (require 'core-ui))
(eval-when-compile (require 'core-keys))
(eval-when-compile (require 'core-projects))

(provide 'core)
