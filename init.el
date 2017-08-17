;; Added by package.el. This must come before configurations of
;; installed packages. Don't delete this line. If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'package-archives
	     '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(unless package-archive-contents
  (package-refresh-contents))

;; Make sure we're using the latest org-mode.
(unless (package-installed-p 'org-plus-contrib)
  (package-install 'org-plus-contrib))

(require 'org)
(unless (string-match "^9" (org-version))
  (warn "org-mode is out of date. org-mode >= 9 expected got %s instead"
	(org-version)))

(mapc #'(lambda (path)
          (add-to-list 'load-path
                       (expand-file-name path user-emacs-directory)))
      '("lisp" "vendor/use-package"))

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

(org-babel-load-file "~/.emacs.d/emma.org")

;; <EOF>
