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
(use-package font-lock+ :defer t)
(use-package rich-minority :defer t)
(use-package s :defer t)
(use-package with-editor :defer t)
(if IS-MAC (use-package exec-path-from-shell
	     :commands exec-path-from-shell-initialize
	     :config (exec-path-from-shell-initialize)))

(setq-default
 ad-redefinition-action 'accept
 compilation-always-kill t
 compilation-ask-about-save nil
 minibuffer-prompt-properties '(readonly t
					 point-entered minibuffer-avoid-prompt
					 face minibuffer-prompt)
 auto-save-default nil
 create-lockfiles nil
 make-backup-files nil
 abbrev-file-name (concat emma-local-dir "abbrev.el")
 auto-save-list-file-name (concat emma-local-dir "autosave")
 backup-directory-alist (list (cons "." (concat emma-local-dir "backup/")))
 pcache-directory (concat emma-local-dir "pcache/")
 server-auth-dir (concat emma-local-dir "server/"))

(setq inhibit-startup-message t
      initial-scratch-message nil)

(load-theme 'nord t)

(set-face-attribute 'default nil
		    :height 150
		    :family "Operator Mono"
		    :weight 'light)
(set-face-attribute 'comint-highlight-prompt nil
		    :inherit nil)

(provide 'core)
