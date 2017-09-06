;;; core/autoload/ui.el -*- lexical-binding: t; -*-

;;;###autoload
(defun njord/toggle-fullscreen ()
  "Toggle fullscreen Emacs (non-native on MacOS)."
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (unless (frame-parameter nil 'fullscreen)
     'fullboth)))

;;;###autoload
(defun njord/toggle-line-numbers (&optional arg)
  "Toggle `linum-mode'."
  (interactive "P")
  (cond ((boundp 'display-line-numbers)
         (setq display-line-numbers
               (pcase arg
                 ('(4) 'relative)
                 (1 t)
                 (-1 nil)
                 (_ (not display-line-numbers)))))
        ((featurep 'nlinum)
         (nlinum-mode (or arg (if nlinum-mode -1 +1))))
        (t
         (error "No line number plugin detected"))))

;;;###autoload
(defun njord-resize-window (new-size &optional horizontal)
  "Resize a window to NEW-SIZE. If HORIZONTAL, do it width-wise."
  (enlarge-window (- new-size (if horizontal (window-width) (window-height)))
                  horizontal))

;;;###autoload
(defun njord/window-zoom ()
  "Maximize and isolate the current buffer. Activate again to undo this. If the
window changes before then, the undo expires."
  (interactive)
  (if (and (one-window-p)
           (assoc ?_ register-alist))
      (jump-to-register ?_)
    (window-configuration-to-register ?_)
    (delete-other-windows)))

(defvar njord--window-enlargened nil)
;;;###autoload
(defun njord/window-enlargen ()
  "Enlargen the current window. Activate again to undo."
  (interactive)
  (setq njord--window-enlargened
        (if (and njord--window-enlargened
                 (assoc ?_ register-alist))
            (ignore (jump-to-register ?_))
          (window-configuration-to-register ?_)
          (njord-resize-window (truncate (/ (frame-width)  1.2)) t)
          (njord-resize-window (truncate (/ (frame-height) 1.2)))
          t)))

;;;###autoload
(defun njord/delete-frame ()
  "Delete the current frame, but ask for confirmation if it isn't empty."
  (interactive)
  (if (cdr (frame-list))
      (when (njord-quit-p "Close frame?")
        (delete-frame))
    (save-buffers-kill-emacs)))

;;;###autoload
(defun njord/reload-theme ()
  "Reset the color theme currently in use."
  (interactive)
  (let ((theme (or (car-safe custom-enabled-themes) njord-theme)))
    (when theme
      (mapc #'disable-theme custom-enabled-themes))
    (run-hooks 'njord-pre-reload-theme-hook)
    (njord|init-ui)
    (run-hooks 'njord-post-reload-theme-hook)))

;;;###autoload
(define-minor-mode njord-big-font-mode
  "A global mode that resizes the font, for streams, screen-sharing and
presentations."
  :init-value nil
  :lighter " BIG"
  :global t
  (unless (fontp njord-big-font)
    (user-error "`njord-big-font' isn't set to a valid font"))
  (if njord-big-font-mode
      (set-frame-font njord-big-font t t)
    (set-frame-font njord-font t t)))
