;;; init.el
;;
;; Copyright (c) 2017 Gabriel Lievano
;;
;; Author: J. Gabriel Lievano <gabe@jglievano.com>

(when (version< emacs-version "24.4")
  (error "Emma requires at least GNU Emacs 24.4. You're running %s"
	 emacs-version))

(add-to-list 'load-path "~/.emacs.d/libs")

;; Global variables.
(require 'emma-global-vars)

;; UI settings.
(require 'emma-ui)

;; Start evil-mode.
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)
(add-to-list 'load-path "~/.emacs.d/evil-leader")
(require 'evil-leader)
(global-evil-leader-mode)

;; Start which-key-mode.
(add-to-list 'load-path "~/.emacs.d/which-key")
(require 'which-key)
(which-key-mode)

;; TODO: install .emma files.

;; Start server.
(require 'server)
(if (and (fboundp 'server-running-p)
	 (not (server-running-p)))
    (server-start))
  