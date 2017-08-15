(package-initialize)

;; Disable window-y UI
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No bell.
(setq visible-bell 1)

;; No startup or splash gimmicks
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; 2-space indentation
(setq indent-tabs-mode nil)
(setq tab-width 2)
(setq-default c-basic-offset 2)

;; display column number
(setq column-number-mode t)

;; better backup
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t
      delete-old-versions 6
      kept-old-versions 2
      version-control t)

;; packages
(add-to-list 'package-archives
	     '("org" . "https://orgmode.org/elpa/"))
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'org-plus-contrib)
  (package-install 'org-plus-contrib))

(mapc #'(lambda (path)
          (add-to-list 'load-path
                       (expand-file-name path user-emacs-directory)))
      '("lisp" "vendor/use-package" "themes/emacs-theme-gruvbox"))
(mapc #'(lambda (path)
          (add-to-list 'custom-theme-load-path
                       (expand-file-name path user-emacs-directory)))
      '("themes/emacs-theme-gruvbox"))
(load-theme 'gruvbox t)

(require 'emma-packages)

;; org-mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(add-hook 'org-mode-hook 'turn-on-font-lock)

;; Start server.
(require 'server)
(if (and (fboundp 'server-running-p)
         (not (server-running-p)))
  (server-start))

;; <EOF>
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3f1262dbd56ee3609693b8c6ec31d9ed005eb816699fe9bdc266c952858ed265" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
