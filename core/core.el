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

(defun emma-try-run-hook (fn hook)
  "Runs a hook wrapped in a `condition-case-unless-debug' block."
  (condition-case-unless-debug ex
      (if noninteractive
          (quiet! (funcall fn))
        (funcall fn))
    ('error
     (lwarn hook :error
            "%s in '%s' -> %s"
            (car ex) fn (error-message-string ex))))
  nil)

(defun emma|finalize ()
  (unless (or emma-init-p noninteractive)
    (dolist (hook '(emma-init-hook emma-post-init-hook))
      (run-hook-wrapped hook #'emma-try-run-hook hook))
    (setq emma-init-p t))

  (setq gc-cons-threshold 16777216
        gc-cons-percentage 0.1
        file-name-handler-alist emma--file-name-handler-alist))

(defun emma-initialize ()
  ;; Ensure core folders exist.
  (unless (file-directory-p emma-local-dir)
    (make-directory dir t))

  (package-initialize t)

  ;; Add load-paths for each vendor package.
  (setq emma--package-load-path (directory-files emma-vendor-dir t "^[^.]" t)
        load-path (append load-path emma--package-load-path)))


(defun emma|init ()
  (message "emma|init")
  (require 'use-package)
  (require 'evil)
  (evil-mode 1)
  (unless (string-match "^9" (org-version))
    (warn "org-mode is out of date. org-mode >= 9 expected, got %s instead"
          (org-version))))

;;; Initialize.
(eval-and-compile
  (defvar emma--file-name-handler-alist file-name-handler-alist)
  (setq gc-cons-threshold 402653184
        gc-cons-percentage 0.6
        file-name-handler-alist nil)

  (emma-initialize)
  
  (add-hook 'after-init-hook 'emma|init)
  
  (setq load-path (eval-when-compile load-path)
        emma--package-load-path (eval-when-compile emma--package-load-path)))

(provide 'core)
