(defvar emma-dir (file-name-directory load-file-name)
  "Emma root directory.")

(defvar emma-rc-dir (expand-file-name "~/.emma")
  "Emma configuration directory.")

(defvar emma-vendor (expand-file-name "vendor" user-emacs-directory)
  "Emma vendor directory.")

(provide 'emma-global-vars)
