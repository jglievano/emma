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

;; Better defaults...
;; TODO: move to a library.
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t
      delete-old-versions 6
      kept-old-versions 2
      version-control t)

;; Load packages.
(require 'emma-packages)

;; UI settings.
(require 'emma-ui)

;; Keymap.
(require 'emma-keymap)

;; TODO: install .emma files.

;; Start server.
(require 'server)
(if (and (fboundp 'server-running-p)
	 (not (server-running-p)))
    (server-start))

