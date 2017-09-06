;;; core/autoload/buffers.el -*- lexical-binding: t; -*-

(defvar-local njord-buffer--narrowed-origin nil)

;;;###autoload
(defvar njord-real-buffer-functions '()
  "A list of functions that are run to determine if a buffer is real.")

(defvar-local njord-real-buffer-p nil
  "If non-nil, this buffer should be considered real no matter what.")

;;;###autoload
(defvar njord-fallback-buffer "*scratch*"
  "The name of the buffer to fall back to if no other buffers exist (will create
it if it doesn't exist).")

;;;###autoload
(defun njord-fallback-buffer ()
  "Returns the fallback buffer, creating it if necessary. By default this is the
scratch buffer."
  (get-buffer-create njord-fallback-buffer))

;;;###autoload
(defun njord-narrow-buffer (beg end &optional clone-p)
  "Restrict editing in this buffer to the current region, indirectly. With CLONE-P,
clone the buffer and hard-narrow the selection. If mark isn't active, then widen
the buffer (if narrowed).
Inspired from http://demonastery.org/2013/04/emacs-evil-narrow-region/"
  (interactive "r")
  (cond ((region-active-p)
         (deactivate-mark)
         (when clone-p
           (let ((old-buf (current-buffer)))
             (switch-to-buffer (clone-indirect-buffer nil nil))
             (setq njord-buffer--narrowed-origin old-buf)))
         (narrow-to-region beg end))
        (njord-buffer--narrowed-origin
         (kill-this-buffer)
         (switch-to-buffer njord-buffer--narrowed-origin)
         (setq njord-buffer--narrowed-origin nil))
        (t
         (widen))))


;; Buffer Life and Death ;;;;;;;;;;;;;;;
;;;###autoload
(defalias 'njord-buffer-list #'buffer-list)

;;;###autoload
(defun njord-project-buffer-list ()
  "Return a list of buffers belonging to the current project.
If no project is active, return all buffers."
  (let ((buffers (njord-buffer-list)))
    (if-let (project-root (njord-project-root t))
        (cl-loop for buf in buffers
                 if (projectile-project-buffer-p buf project-root)
                 collect buf)
      buffers)))

;;;###autoload
(defun njord-real-buffer-list (&optional buffer-list)
  "Return a list of buffers that satify `njord-real-buffer-p'."
  (cl-loop for buf in (or buffer-list (njord-buffer-list))
           if (njord-real-buffer-p buf)
           collect buf))

;;;###autoload
(defun njord-buffers-in-mode (modes &optional buffer-list derived-p)
  "Return a list of buffers whose `major-mode' is `eq' to MODE(S).
If DERIVED-P, test with `derived-mode-p', otherwise use `eq'."
  (let ((modes (njord-enlist modes)))
    (cl-remove-if-not (if derived-p
                          (lambda (buf)
                            (with-current-buffer buf
                              (apply #'derived-mode-p modes)))
                        (lambda (buf)
                          (memq (buffer-local-value 'major-mode buf) modes)))
                      (or buffer-list (njord-buffer-list)))))

;;;###autoload
(defun njord-visible-windows (&optional window-list)
  "Return a list of the visible, non-popup windows."
  (cl-loop for win in (or window-list (window-list))
           unless (njord-popup-p win)
           collect win))

;;;###autoload
(defun njord-visible-buffers (&optional buffer-list)
  "Return a list of visible buffers (i.e. not buried)."
  (cl-loop for buf in (or buffer-list (njord-buffer-list))
           when (get-buffer-window buf)
           collect buf))

;;;###autoload
(defun njord-buried-buffers (&optional buffer-list)
  "Get a list of buffers that are buried."
  (cl-loop for buf in (or buffer-list (njord-buffer-list))
           unless (get-buffer-window buf)
           collect buf))

;;;###autoload
(defun njord-matching-buffers (pattern &optional buffer-list)
  "Get a list of all buffers that match the regex PATTERN."
  (cl-loop for buf in (or buffer-list (njord-buffer-list))
           when (string-match-p pattern (buffer-name buf))
           collect buf))

(defun njord--cycle-real-buffers (&optional n)
  "Switch to the next buffer N times (previous, if N < 0), skipping over unreal
buffers. If there's nothing left, switch to `njord-fallback-buffer'. See
`njord-real-buffer-p' for what 'real' means."
  (let ((buffers (delq (current-buffer) (njord-real-buffer-list)))
        (project-dir (njord-project-root)))
    (cond ((or (not buffers)
               (zerop (% n (1+ (length buffers)))))
           (set-window-buffer nil (njord-fallback-buffer)))
          ((= (length buffers) 1)
           (set-window-buffer nil (car buffers)))
          (t
           ;; Why this instead of switching straight to the Nth buffer in
           ;; BUFFERS? Because `switch-to-next-buffer' and
           ;; `switch-to-prev-buffer' properly update buffer list order.
           (cl-loop with move-func =
                    (if (> n 0) #'switch-to-next-buffer #'switch-to-prev-buffer)
                    for i to 20
                    while (not (memq (current-buffer) buffers))
                    do
                    (dotimes (_i (abs n))
                      (funcall move-func)))))
    (when (eq (current-buffer) (njord-fallback-buffer))
      (cd project-dir))
    (current-buffer)))

;;;###autoload
(defun njord-real-buffer-p (&optional buffer-or-name)
  "Returns t if BUFFER-OR-NAME is a 'real' buffer. Real means it a) isn't a
popup window/buffer and b) isn't a special buffer."
  (let ((buf (window-normalize-buffer buffer-or-name)))
    (or (buffer-local-value 'njord-real-buffer-p buf)
        (run-hook-with-args-until-success 'njord-real-buffer-functions buf)
        (not (or (njord-popup-p buf)
                 (minibufferp buf)
                 (string-match-p "^\\s-*\\*" (buffer-name buf))
                 (not (buffer-file-name buf)))))))

;;;###autoload
(defun njord/next-buffer ()
  "Switch to the next real buffer, skipping special buffers. See
`njord-real-buffer-p' for what 'real' means."
  (interactive)
  (njord--cycle-real-buffers +1))

;;;###autoload
(defun njord/previous-buffer ()
  "Switch to the previous real buffer, skipping special buffers. See
`njord-real-buffer-p' for what 'real' means."
  (interactive)
  (njord--cycle-real-buffers -1))

;;;###autoload
(defun njord-kill-buffer (&optional buffer dont-save)
  "Kill BUFFER (falls back to current buffer if omitted) then switch to a real
buffer, but only bury the buffer if it is present in another window.
See `njord-real-buffer-p' for what 'real' means."
  (setq buffer (or buffer (current-buffer)))
  (when (and (bufferp buffer) (buffer-live-p buffer))
    (let ((buffer-win (get-buffer-window buffer))
          (only-buffer-window-p (= 1 (length (get-buffer-window-list buffer nil t)))))
      ;; deal with unsaved buffers
      (when (and only-buffer-window-p
                 (buffer-file-name buffer)
                 (buffer-modified-p buffer))
        (with-current-buffer buffer
          (if (and (not dont-save)
                   (yes-or-no-p "Buffer is unsaved, save it?"))
              (save-buffer)
            (set-buffer-modified-p nil))))
      (if buffer-win
          ;; deal with dedicated windows
          (if (window-dedicated-p buffer-win)
              (unless (window--delete buffer-win t t)
                (split-window buffer-win)
                (window--delete buffer-win t t))
            ;; cycle to a real buffer
            (with-selected-window buffer-win
              (njord--cycle-real-buffers -1)
              (when buffer-win
                (unrecord-window-buffer buffer-win buffer))
              (when only-buffer-window-p
                (kill-buffer buffer)))
            (not (eq (current-buffer) buffer)))
        (kill-buffer buffer)))))

;;;###autoload
(defun njord-force-kill-buffer (&optional buffer dont-save)
  "Kill BUFFER globally and ensure all windows previously showing BUFFER have
switched to a real buffer."
  (interactive)
  (let* ((buffer (or buffer (current-buffer)))
         (windows (get-buffer-window-list buffer nil t)))
    (njord-kill-buffer buffer dont-save)
    (dolist (win windows)
      (with-selected-window win
        (unless (njord-real-buffer-p)
          (njord/previous-buffer))))))

;;;###autoload
(defun njord-kill-buffer-and-windows (buffer)
  "Kill the buffer and delete all the windows it's displayed in."
  (dolist (window (get-buffer-window-list buffer))
    (unless (one-window-p t)
      (delete-window window)))
  (kill-buffer buffer))

;;;###autoload
(defun njord-kill-process-buffers ()
  "Kill all processes that have no visible associated buffers. Return number of
processes killed."
  (interactive)
  (let ((n 0))
    (dolist (p (process-list))
      (let ((process-buffer (process-buffer p)))
        (when (and (process-live-p p)
                   (not (string= (process-name p) "server"))
                   (or (not process-buffer)
                       (and (bufferp process-buffer)
                            (not (buffer-live-p process-buffer)))))
          (delete-process p)
          (cl-incf n))))
    n))

;;;###autoload
(defun njord-kill-matching-buffers (pattern &optional buffer-list)
  "Kill all buffers (in current workspace OR in BUFFER-LIST) that match the
regex PATTERN. Returns the number of killed buffers."
  (let ((buffers (njord-matching-buffers pattern buffer-list)))
    (dolist (buf buffers (length buffers))
      (njord-kill-buffer buf t))))

;;;###autoload
(defun njord/kill-this-buffer ()
  "Use `njord-kill-buffer' on the current buffer."
  (interactive)
  (when (and (not (njord-kill-buffer)) (called-interactively-p 'interactive))
    (message "Nowhere left to go!")))

;;;###autoload
(defun njord/kill-all-buffers (&optional project-p)
  "Kill all buffers.
If PROJECT-P, kill all buffers that belong to the current project."
  (interactive "P")
  (let ((buffers (if project-p (njord-project-buffer-list) (njord-buffer-list))))
    (mapc #'njord-kill-buffer-and-windows buffers)
    (when (called-interactively-p 'interactive)
      (unless (njord-real-buffer-p)
        (switch-to-buffer (njord-fallback-buffer)))
      (message "Killed %s buffers" (length buffers)))))

;;;###autoload
(defun njord/kill-other-buffers (&optional project-p)
  "Kill all other buffers.
If PROJECT-P (universal argument), kill only the other buffers that belong to
the current project."
  (interactive "P")
  (let ((buffers (if project-p (njord-project-buffer-list) (njord-buffer-list)))
        (current-buffer (current-buffer)))
    (dolist (buf buffers)
      (unless (eq buf current-buffer)
        (njord-kill-buffer-and-windows buf)))
    (when (called-interactively-p 'interactive)
      (message "Killed %s buffers" (length buffers)))))

;;;###autoload
(defun njord/kill-matching-buffers (pattern &optional project-p)
  "Kill buffers that match PATTERN in BUFFER-LIST.
If PROJECT-P (universal argument), only kill matching buffers in the current
project."
  (interactive
   (list (read-regexp "Buffer pattern: ")
         current-prefix-arg))
  (let* ((buffers (if project-p (njord-project-buffer-list) (njord-buffer-list)))
         (n (njord-kill-matching-buffers pattern buffers)))
    (when (called-interactively-p 'interactive)
      (message "Killed %s buffers" n))))

;;;###autoload
(defun njord/cleanup-buffers (&optional all-p)
  "Clean up buried and process buffers in the current workspace."
  (interactive "P")
  (let ((buffers (njord-buried-buffers (if all-p (buffer-list))))
        (n 0))
    (mapc #'kill-buffer buffers)
    (setq n (+ n (length buffers) (njord-kill-process-buffers)))
    (when (called-interactively-p 'interactive)
      (message "Cleaned up %s buffers" n))))

;;;###autoload
(defun njord-set-buffer-real (buffer flag)
  "Forcibly mark a buffer's real property, no matter what."
  (with-current-buffer buffer
    (setq njord-real-buffer-p flag)))
