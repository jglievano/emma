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
                                       
;(require 'core (concat user-emacs-directory "core/core"))

;(emma! evil
 ;      jump)


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(load (expand-file-name "~/.emacs.d/old.el"))
