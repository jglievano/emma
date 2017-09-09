;;; mod-git.el --- Git tool. -*- lexical-binding: t; -*-

;;; Commentary:
;; Emma uses magit.

;;; Code:
(use-package magit
  :commands magit-status
  :init
  (require 'magit)
  (with-eval-after-load 'info
    (info-initialize)
    (add-to-list 'Info-directory-list
                 "~/.emacs.d/vendor/magit/Documentation/")))

(provide 'mod-git)
;;; mod-git.el ends here
