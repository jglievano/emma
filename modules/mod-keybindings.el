;;; mod-keybindings.el --- Set all Emma keybindings. -*- lexical-binding: t; -*-

;;; Commentary:
;; All evil-leader keys and others settings.

;;; Code:

(defun emma|set-keybindings ()
  (which-key-add-key-based-replacements
    "SPC b" "buffer"
    "SPC e" "edit"
    "SPC f" "file"
    "SPC g" "git"
    "SPC p" "project"
    "SPC w" "window")
  (evil-leader/set-key
    ":"  'smex
    "$"  'shell
    "bs" 'save-buffer
    "bq" 'kill-buffer-and-window
    "bQ" 'save-buffers-kill-emacs
    "ew" 'whitespace-mode
    "ff" 'ido-find-file
    "gs" 'magit-status
    "wh" 'windmove-left
    "wj" 'windmove-down
    "wk" 'windmove-up
    "wl" 'windmove-right
    "pf" 'projectile-find-file
    "pp" 'projectile-switch-project))

(add-hook 'after-init-hook 'emma|set-keybindings)

(provide 'mod-keybindings)
;;; mod-keybindings.el ends here
