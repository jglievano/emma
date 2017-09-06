;;; core/autoload/message.el -*- lexical-binding: t; -*-

(defconst njord-message-fg
  '((reset      . 0)
    (black      . 30)
    (red        . 31)
    (green      . 32)
    (yellow     . 33)
    (blue       . 34)
    (magenta    . 35)
    (cyan       . 36)
    (white      . 37))
  "List of text colors.")

(defconst njord-message-bg
  '((on-black   . 40)
    (on-red     . 41)
    (on-green   . 42)
    (on-yellow  . 43)
    (on-blue    . 44)
    (on-magenta . 45)
    (on-cyan    . 46)
    (on-white   . 47))
  "List of colors to draw text on.")

(defconst njord-message-fx
  '((bold       . 1)
    (dark       . 2)
    (italic     . 3)
    (underscore . 4)
    (blink      . 5)
    (rapid      . 6)
    (contrary   . 7)
    (concealed  . 8)
    (strike     . 9))
  "List of styles.")

;;;###autoload
(defmacro format! (message &rest args)
  "An alternative to `format' that strips out ANSI codes if used in an
interactive session."
  `(cl-flet*
       (,@(cl-loop for rule
                   in (append njord-message-fg njord-message-bg njord-message-fx)
                   collect
                   `(,(car rule)
                     (lambda (message &rest args)
                       (apply #'njord-ansi-apply ',(car rule) message args))))
        (color
         (lambda (code format &rest args)
           (apply #'njord-ansi-apply code format args))))
     (format ,message ,@args)))

;;;###autoload
(defmacro message! (message &rest args)
  "An alternative to `message' that strips out ANSI codes if used in an
interactive session."
  `(if noninteractive
       (message (format! ,message ,@args))
     (let ((buf (get-buffer-create " *njord messages*")))
       (with-current-buffer buf
         (goto-char (point-max))
         (let ((beg (point))
               end)
           (insert (format! ,message ,@args))
           (insert "\n")
           (setq end (point))
           (ansi-color-apply-on-region beg end)))
       (with-selected-window (njord-popup-buffer buf)
         (goto-char (point-max))))))

;;;###autoload
(defmacro debug! (message &rest args)
  "Out a debug message if `njord-debug-mode' is non-nil. Otherwise, ignore this."
  (when njord-debug-mode
    `(message ,message ,@args)))

;;;###autoload
(defun njord-ansi-apply (code format &rest args)
  (let ((rule (or (assq code njord-message-fg)
                  (assq code njord-message-bg)
                  (assq code njord-message-fx))))
    (format "\e[%dm%s\e[%dm"
            (cdr rule)
            (apply #'format format args)
            0)))
