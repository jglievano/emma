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

(defvar emma-version "1.1.0"
  "Current Emma version.")

(defvar emma-debug-mode (or (getenv "DEBUG") init-file-debug)
  "If non-nil, all emma functions will be verbose. Set DEBUG=1 or use
--debug-init to enable.")
                                       
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(load (expand-file-name "~/.emacs.d/old.el"))
