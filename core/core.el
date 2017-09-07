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

(defvar emma-version "1.1.0"
  "Current Emma version.")

(defvar emma-debug-mode (or (getenv "DEBUG") init-file-debug)
  "If non-nil, all emma functions will be verbose. Set DEBUG=1 or use
--debug-init to enable.")

(defvar emma-emacs-dir (expand-file-name user-emacs-directory)
  "The path to this emacs directory.")

(defvar emma-core-dir (concat emma-emacs-dir "core/")
  "Core files.")

(defvar emma-modules-dir (concat emma-emacs-dir "modules/")
  "Module configuration files.")

(defvar emma-local-dir (concat emma-emacs-dir ".local/")
  "Root directory for local Emacs files.")

(defvar emma-host-dir (concat emma-local-dir "@" (system-name))
  "Directory for hostname-specific file storage.")

(defvar emma-etc-dir (concat emma-host-dir "/etc/")
  "Host-namespaced directory for non-volatile storage. Use this for dependnecies
like servers or config files that are stable.")

(defvar emma-cache-dir (concat emma-host-dir "/cache/")
  "Host-namespaced directory for volatile storage. Deleted when `emma/reset' is
called. Use for transient files.")

(defvar emma-packages-dir (concat emma-local-dir "packages/")
  "Where packages.el and quelpa plugins are stored.")

(defvar emma-vendor-dir (concat emma-emacs-dir "vendor/")
  "Where vendor packages are stored.")

(defvar emma-autoload-file (concat emma-local-dir "autoloads.el")
  "Location of autoloads file generated by `emma/reload-autoloads'.")

(defgroup emma nil
  "Emma Emacs."
  :group 'emacs)

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
 auto-save-list-file-name (concat emma-cache-dir "autosave")
 backup-directory-alist (list (cons "." (concat emma-cache-dir "backup/")))
 pcache-directory (concat emma-cache-dir "pcache/")
 server-auth-dir (concat emma-cache-dir "server/")
 shared-game-score-directory (concat emma-etc-dir "shared-game-score/")
 tramp-auto-save-directory (concat emma-cache-dir "tramp-auto-save/")
 tramp-backup-directory-alist backup-directory-alist
 tramp-persistency-file-name (concat emma-cache-dir "tramp-persistency.el")
 url-cache-directory (concat emma-cache-dir "url/")
 url-configuration-directory (concat emma-etc-dir "url/"))

(setq custom-file (concat emma-etc-dir "custom.el"))
(load custom-file t t)

;; be quiet at startup.
(advice-add #'display-startup-echo-area-message :override #'ignore)
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'org-mode
      initial-scratch-message nil
      mode-line-format nil)

;; Custom init hooks.
(defvar emma-init-hook nil
  "A list of hooks run when Emma is initialized, before `emma-startup-hook'.")

(defvar emma-post-init-hook nil
  "A list of hooks run after `emma-init-hook'.")

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
        file-name-handler-alist emma--file-handler-alist))

;;; Initialize.
(eval-and-compile
  (defvar emma--file-name-handler-alist file-name-handler-alist)
  (setq gc-cons-threshold 402653184
        gc-cons-percentage 0.6
        file-name-handler-alist nil)

  (require 'cl-lib)
  (require 'core-packages (concat emma-core-dir "core-packages"))

  (eval-when-compile (emma-initialize))
  (setq load-path (eval-when-compile load-path)
        emma--package-load-path (eval-when-compile emma--package-load-path))

  (load! core-lib)
  (load! core-os)
  (condition-case-unless-debug ex
      (require 'autoloads emma-autoload-file t)
    ('error
     (lwarn 'emma-autoloads :warning
            "%s in autoloads.el -> %s"
            (car ex) (error-message-string ex))))

  (unless noninteractive
    (load! core-ui)
    (load! core-popups)
    (load! core-editor)
    (load! core-projects)
    (load! core-keybinds)))

(add-hook! '(emacs-startup-hook emma-reload-hook)
           #'emma|finalize)

(provide 'core)
