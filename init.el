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

