(defvar emma-dir (file-name-directory load-file-name)
  "Emma root directory.")

(defvar emma-rc-dir (expand-file-name "~/.emma")
  "Emma configuration directory.")

(defvar emma-deps (expand-file-name "deps" user-emacs-directory)
  "Emma dependency directory.")

(provide 'emma-global-vars)
