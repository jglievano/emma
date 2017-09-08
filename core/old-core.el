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
(setq load-path (append load-path (directory-files emma-vendor-dir t "^[^.]" t)))
(eval-when-compile (require 'use-package))

(use-package autothemer :defer t)
(use-package dash :defer t)
(use-package rich-minority :defer t)
(use-package s :defer t)
(use-package with-editor :defer t)

(defvar emma-init-ui-hook nil
  "List of hooks to run when the theme and font is initialized.")

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

(defvar winner-dont-bind-my-keys t)
(autoload 'winner-mode "winner" nil t)
(add-hook 'emma-init-ui-hook #'winner-mode)

(setq show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t)
(add-hook 'emma-init-ui-hook #'show-paren-mode)

(setq-default window-divider-default-places t
	      window-divider-default-bottom-width 0
	      window-divider-default-right-width 1)
(add-hook 'emma-init-ui-hook #'window-divider-mode)

(defun emma|init-ui (&optional frame)
  (if emma-debug-mode (message "emma|init-ui"))
  (load-theme 'emma t)
  (run-hooks 'emma-init-ui-hook))

(defun emma|reload-ui-in-daemon (frame)
  (when (or (daemonp) (not (display-graphic-p)))
    (with-selected-frame frame
      (run-with-timer 0.1 nil #'emma|init-ui))))

(add-hook 'after-init-hook #'emma|init-ui)
(add-hook 'after-make-frame-functions 'doom|init-ui)
(add-hook 'after-make-frame-functions 'doom|reload-ui-in-daemon)

(provide 'core)
