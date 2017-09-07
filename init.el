;;; init.el -*- lexical-binding: t; -*-
;;
;; Author: Gabriel Lievano <gabe@jglievano.com>
;; URL: https://github.com/jglievano/emma.git
;;
;; Emacs configuration based on hlissner/doom-emacs.
;;
;; ▓█████  ███▄ ▄███▓ ███▄ ▄███▓ ▄▄▄      
;; ▓█   ▀ ▓██▒▀█▀ ██▒▓██▒▀█▀ ██▒▒████▄    
;; ▒███   ▓██    ▓██░▓██    ▓██░▒██  ▀█▄  
;; ▒▓█  ▄ ▒██    ▒██ ▒██    ▒██ ░██▄▄▄▄██ 
;; ░▒████▒▒██▒   ░██▒▒██▒   ░██▒ ▓█   ▓██▒
;; ░░ ▒░ ░░ ▒░   ░  ░░ ▒░   ░  ░ ▒▒   ▓▒█░
;;  ░ ░  ░░  ░      ░░  ░      ░  ▒   ▒▒ ░
;;    ░   ░      ░   ░      ░     ░   ▒   
;;    ░  ░       ░          ░         ░  ░


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defvar emma-version "2.0.1"
  "Current Emma version.")

(defvar emma-debug-mode (or (getenv "DEBUG") init-file-debug)
  "If non-nil, all emma functions will be verbose. Set DEBUG=1 or use
--debug-init to enable.")

(defvar emma-emacs-dir (expand-file-name user-emacs-directory)
  "The path to this emacs directory.")

(defvar emma-core-dir (concat emma-emacs-dir "core/")
  "Core files.")

(defvar emma-modules-dir (concat emma-emacs-dir "modules/")
  "Path to module directories.")

(defvar emma-vendor-dir (concat emma-emacs-dir "vendor/")
  "Vendor packages.")

(defvar emma-local-dir (concat emma-emacs-dir ".local/")
  "Local files.")

;; Launch
(require 'core (concat emma-core-dir "core"))
