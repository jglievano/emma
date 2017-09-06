;;; core-popups.el -*- lexical-binding: t; -*-

;; I want a "real"-buffer-first policy in my Emacsian utpoia; popup buffers
;; ought to be second-class citizens to "real" buffers. No need for a wall or
;; controversial immigration policies -- all we need is `shackle' (and it will
;; actually work).
;;
;; The gist is: popups should be displayed on one side of the frame, away from
;; 'real' buffers. They should be easy to dispose of when we don't want to see
;; them and easily brought back in case we change our minds. Also, popups should
;; typically have no mode-line.
;;
;; Be warned, this requires a lot of hackery voodoo that could break with an
;; emacs update or an update to any of the packages it tries to tame (like helm
;; or org-mode).

(defvar njord-popup-history nil
  "A list of popups that were last closed. Used by `njord/popup-restore' and
`njord*popups-save'.")

(defvar njord-popup-remember-history t
  "If non-nil, NJORD will remember the last popup(s) that were open in
`njord-popup-history'.")

(defvar njord-popup-other-window nil
  "The last window selected before a popup was opened.")

(defvar njord-popup-no-fringes t
  "If non-nil, disable fringes in popup windows.")

(defvar njord-popup-windows ()
  "A list of open popup windows.")

(defvar-local njord-popup-rules nil
  "The shackle rule that caused this buffer to be recognized as a popup.")

(defvar njord-popup-window-parameters
  '(:noesc :modeline :autokill :autoclose)
  "A list of window parameters that are set (and cleared) when `njord-popup-mode
is enabled/disabled.'")

(def-setting! :popup (&rest rules)
  "Prepend a new popup rule to `shackle-rules' (see for format details).
Several custom properties have been added that are not part of shackle, but are
recognized by NJORD's popup system. They are:
:noesc      If non-nil, pressing ESC *inside* the popup will close it.
            Used by `njord/popup-close-maybe'.
:modeline   By default, mode-lines are hidden in popups unless this
            is non-nil. If it is a symbol, it'll use `njord-modeline'
            to fetch a modeline config (in `njord-popup-mode').
:autokill   If non-nil, the popup's buffer will be killed when the
            popup is closed. Used by `njord*delete-popup-window'.
            NOTE `njord/popup-restore' can't restore non-file popups
            that have an :autokill property.
:autoclose  If non-nil, close popup if ESC is pressed from outside
            the popup window."
  (if (cl-every #'listp (mapcar #'njord-unquote rules))
      `(setq shackle-rules (nconc (list ,@rules) shackle-rules))
    `(push (list ,@rules) shackle-rules)))


;;
;; Bootstrap
;;

(def-package! shackle
  :demand t
  :init
  (setq shackle-default-alignment 'below
        shackle-default-size 8
        shackle-rules
        '(("^\\*ftp " :noselect t :autokill t :noesc t)
          ;; njord
          ("^\\*njord:" :regexp t :size 0.35 :noesc t :select t :modeline t)
          ("^\\*njord " :regexp t :noselect t :autokill t :autoclose t)
          ;; built-in (emacs)
          ("*ert*" :same t :modeline t)
          ("*info*" :size 0.5 :select t :autokill t)
          ("*Backtrace*" :size 20 :noselect t)
          ("*Warnings*"  :size 8  :noselect t)
          ("*Messages*"  :size 12 :noselect t)
          ("*Help*" :size 0.3)
          ("^\\*.*Shell Command.*\\*$" :regexp t :size 20 :noselect t :autokill t)
          (apropos-mode :size 0.3 :autokill t :autoclose t)
          (Buffer-menu-mode :size 20 :autokill t)
          (comint-mode :noesc t)
          (grep-mode :size 25 :noselect t :autokill t)
          (profiler-report-mode :size 0.3 :regexp t :autokill t :modeline minimal)
          (tabulated-list-mode :noesc t)
          (special-mode :noselect t :autokill t :autoclose t)
          ("^\\*"  :regexp t :noselect t :autokill t)
          ("^ \\*" :regexp t :size 12 :noselect t :autokill t :autoclose t)))

  :config
  (add-hook 'njord-post-init-hook #'shackle-mode)

  (defun njord*shackle-always-align (plist)
    "Ensure popups are always aligned and selected by default. Eliminates the need
for :align t on every rule."
    (when plist
      (unless (or (plist-member plist :align)
                  (plist-member plist :same)
                  (plist-member plist :frame))
        (plist-put plist :align t))
      (unless (or (plist-member plist :select)
                  (plist-member plist :noselect))
        (plist-put plist :select t)))
    plist)
  (advice-add #'shackle--match :filter-return #'njord*shackle-always-align))


;;
;; Integration
;;

;; Tell `window-state-get' and `current-window-configuration' to recognize these
;; custom parameters. Helpful for `persp-mode' and persisting window configs
;; that have popups in them.
(dolist (param (cons 'popup njord-popup-window-parameters))
  (push (cons param 'writable) window-persistent-parameters))

(defvar njord-popup-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [escape]    'njord/popup-close-maybe)
    (define-key map (kbd "ESC") 'njord/popup-close-maybe)
    (define-key map [remap njord-kill-buffer] 'kill-this-buffer)
    (define-key map [remap njord/kill-this-buffer] 'kill-this-buffer)
    (define-key map [remap split-window-right] 'ignore)
    (define-key map [remap split-window-below] 'ignore)
    (define-key map [remap split-window-horizontally] 'ignore)
    (define-key map [remap split-window-vertically] 'ignore)
    (define-key map [remap mouse-split-window-horizontally] 'ignore)
    (define-key map [remap mouse-split-window-vertically] 'ignore)
    map)
  "Active keymap in popup windows.")

(define-minor-mode njord-popup-mode
  "Minor mode for popup windows."
  :init-value nil
  :keymap njord-popup-mode-map
  (let ((window (selected-window)))
    ;; If `njord-popup-rules' isn't set for some reason, try to set it
    (when-let (plist (and (not njord-popup-rules)
                          (window-parameter window 'popup)))
      (setq-local njord-popup-rules (window-parameter window 'popup)))
    ;; Ensure that buffer-opening functions/commands (like
    ;; `switch-to-buffer-other-window' won't use this window).
    (set-window-parameter window 'no-other-window njord-popup-mode)
    ;; Makes popup window resist interactively changing its buffer.
    (set-window-dedicated-p window njord-popup-mode)
    (cond (njord-popup-mode
           (when njord-popup-no-fringes
             (set-window-fringes window 0 0 fringes-outside-margins))
           ;; Save metadata into window parameters so it can be saved by window
           ;; config persisting plugins like workgroups or persp-mode.
           (set-window-parameter window 'popup (or njord-popup-rules t))
           (when njord-popup-rules
             (cl-loop for param in njord-popup-window-parameters
                      when (plist-get njord-popup-rules param)
                      do (set-window-parameter window param it))))

          (t
           (when njord-popup-no-fringes
             (set-window-fringes window
                                 njord-fringe-size njord-fringe-size
                                 fringes-outside-margins))
           ;; Ensure window parameters are cleaned up
           (set-window-parameter window 'popup nil)
           (dolist (param njord-popup-window-parameters)
             (set-window-parameter window param nil))))))

;; Major mode changes (and other things) may call `kill-all-local-variables',
;; turning off things like `njord-popup-mode'. This prevents that.
(put 'njord-popup-mode  'permanent-local t)
(put 'njord-popup-rules 'permanent-local t)

(defun njord|hide-modeline-in-popup ()
  "Don't show modeline in popup windows without a :modeline rule. If one exists
and it's a symbol, use `njord-modeline' to grab the format. If non-nil, show the
mode-line as normal. If nil (or omitted, by default), then hide the modeline
entirely."
  (if njord-popup-mode
      (let ((modeline (plist-get njord-popup-rules :modeline)))
        (cond ((or (eq modeline 'nil)
                   (not modeline))
               (njord-hide-modeline-mode +1))
              ((and (symbolp modeline)
                    (not (eq modeline 't)))
               (setq-local njord--modeline-format (njord-modeline modeline))
               (when njord--modeline-format
                 (njord-hide-modeline-mode +1)))))
    (when njord-hide-modeline-mode
      (njord-hide-modeline-mode -1))))
(add-hook 'njord-popup-mode-hook #'njord|hide-modeline-in-popup)

;;
(defun njord*popup-init (orig-fn &rest args)
  "Initializes a window as a popup window by enabling `njord-popup-mode' in it
and setting `njord-popup-rules' within it. Returns the window."
  (unless (njord-popup-p)
    (setq njord-popup-other-window (selected-window)))
  (let* ((plist (or (nth 2 args)
                    (cond ((windowp (car args))
                           (shackle-match (window-buffer (car args))))
                          ((bufferp (car args))
                           (shackle-match (car args))))))
         (buffer (get-buffer (car args)))
         (window-min-height (if (plist-get plist :modeline) 4 2))
         window)
    (when (and (njord-real-buffer-p buffer)
               (get-buffer-window-list buffer nil t))
      (setq plist (append (list :autokill t) plist))
      (setcar args (clone-indirect-buffer (buffer-name (car args)) nil t)))
    (unless (setq window (apply orig-fn args))
      (error "No popup window was found for %s: %s" (car args) plist))
    (cl-pushnew window njord-popup-windows :test #'eq)
    (with-selected-window window
      (unless (eq plist t)
        (setq-local njord-popup-rules plist))
      (njord-popup-mode +1))
    window))

(defun njord*popups-save (orig-fn &rest args)
  "Sets aside all popups before executing the original function, usually to
prevent the popup(s) from messing up the UI (or vice versa)."
  (let ((in-popup-p (njord-popup-p))
        (popups (njord-popup-windows))
        (njord-popup-remember-history t))
    (when popups
      (mapc #'njord/popup-close popups))
    (unwind-protect (apply orig-fn args)
      (when popups
        (let ((origin (selected-window)))
          (njord/popup-restore)
          (unless in-popup-p
            (select-window origin)))))))

(defun njord*delete-popup-window (&optional window)
  "Ensure that popups are deleted properly, and killed if they have :autokill
properties."
  (let ((window (or window (selected-window))))
    (when (njord-popup-p window)
      (setq njord-popup-windows (delq window njord-popup-windows))
      (when njord-popup-remember-history
        (setq njord-popup-history (list (njord--popup-data window))))
      (let ((autokill-p (plist-get njord-popup-rules :autokill)))
        (with-selected-window window
          (njord-popup-mode -1)
          (when autokill-p
            (kill-buffer (current-buffer))))))))

(advice-add #'shackle-display-buffer :around #'njord*popup-init)
(advice-add #'balance-windows :around #'njord*popups-save)
(advice-add #'delete-window :before #'njord*delete-popup-window)


;;
;; Hacks
;;

(progn ; hacks for built-in functions
  (defun njord*buffer-menu (&optional arg)
    "Open `buffer-menu' in a popup window."
    (interactive "P")
    (let ((buf (list-buffers-noselect arg)))
      (njord-popup-buffer buf)
      (with-current-buffer buf
        (setq mode-line-format "Commands: d, s, x, u; f, o, 1, 2, m, v; ~, %; q to quit; ? for help."))))
  (advice-add #'buffer-menu :override #'njord*buffer-menu)

  (defun njord*suppress-pop-to-buffer-same-window (orig-fn &rest args)
    (cl-letf (((symbol-function 'pop-to-buffer-same-window)
               (symbol-function 'pop-to-buffer)))
      (apply orig-fn args)))
  (advice-add #'info :around #'njord*suppress-pop-to-buffer-same-window))


(after! comint
  (defun njord|popup-close-comint-buffer ()
    (when (and (njord-popup-p)
               (derived-mode-p 'comint-mode)
               (not (process-live-p (get-buffer-process (current-buffer)))))
      (delete-window)))
  (add-hook '+evil-esc-hook #'njord|popup-close-comint-buffer t))


(after! eshell
  ;; By tying buffer life to its process, we ensure that we land back in the
  ;; eshell buffer after term dies. May cause problems with short-lived
  ;; processes.
  ;; FIXME replace with a 'kill buffer' keybinding.
  (setq eshell-destroy-buffer-when-process-dies t)

  ;; When eshell runs a visual command (see `eshell-visual-commands'), it spawns
  ;; a term buffer to run it in, but where it spawns it is the problem...
  (defun njord*eshell-undedicate-popup (orig-fn &rest args)
    "Force spawned term buffer to share with the eshell popup (if necessary)."
    (when (njord-popup-p)
      (set-window-dedicated-p nil nil)
      (add-transient-hook! #'eshell-query-kill-processes :after
        (set-window-dedicated-p nil t)))
    (apply orig-fn args))
  (advice-add #'eshell-exec-visual :around #'njord*eshell-undedicate-popup))


(after! evil
  (let ((map njord-popup-mode-map))
    (define-key map [remap evil-window-delete]           #'njord/popup-close)
    (define-key map [remap evil-save-modified-and-close] #'njord/popup-close)
    (define-key map [remap evil-window-move-very-bottom] #'ignore)
    (define-key map [remap evil-window-move-very-top]    #'ignore)
    (define-key map [remap evil-window-move-far-left]    #'ignore)
    (define-key map [remap evil-window-move-far-right]   #'ignore)
    (define-key map [remap evil-window-split]            #'ignore)
    (define-key map [remap evil-window-vsplit]           #'ignore))

  (defun njord|popup-close-maybe ()
    "If current window is a popup, close it. If minibuffer is open, close it. If
not in a popup, close all popups with an :autoclose property."
    (cond ((njord-popup-p)
           (unless (njord-popup-prop :noesc)
             (delete-window)))
          (t
           (njord/popup-close-all))))
  (add-hook '+evil-esc-hook #'njord|popup-close-maybe t)

  ;; Make evil-mode cooperate with popups
  (advice-add #'evil-command-window :override #'njord*popup-evil-command-window)
  (advice-add #'evil-command-window-execute :override #'njord*popup-evil-command-window-execute)

  (defun njord*popup-evil-command-window (hist cmd-key execute-fn)
    "The evil command window has a mind of its own (uses `switch-to-buffer'). We
monkey patch it to use pop-to-buffer, and to remember the previous window."
    (when (eq major-mode 'evil-command-window-mode)
      (user-error "Cannot recursively open command line window"))
    (dolist (win (window-list))
      (when (equal (buffer-name (window-buffer win))
                   "*Command Line*")
        (kill-buffer (window-buffer win))
        (delete-window win)))
    (setq evil-command-window-current-buffer (current-buffer))
    (ignore-errors (kill-buffer "*Command Line*"))
    (with-current-buffer (pop-to-buffer "*Command Line*")
      (setq-local evil-command-window-execute-fn execute-fn)
      (setq-local evil-command-window-cmd-key cmd-key)
      (evil-command-window-mode)
      (evil-command-window-insert-commands hist)))

  (defun njord*popup-evil-command-window-execute ()
    "Execute the command under the cursor in the appropriate buffer, rather than
the command buffer."
    (interactive)
    (let ((result (buffer-substring (line-beginning-position)
                                    (line-end-position)))
          (execute-fn evil-command-window-execute-fn)
          (popup (selected-window)))
      (select-window njord-popup-other-window)
      (unless (equal evil-command-window-current-buffer (current-buffer))
        (user-error "Originating buffer is no longer active"))
      ;; (kill-buffer "*Command Line*")
      (njord/popup-close popup)
      (funcall execute-fn result)
      (setq evil-command-window-current-buffer nil)))

  ;; Don't mess with popups
  (advice-add #'njord-evil-window-move        :around #'njord*popups-save)
  (advice-add #'evil-window-move-very-bottom :around #'njord*popups-save)
  (advice-add #'evil-window-move-very-top    :around #'njord*popups-save)
  (advice-add #'evil-window-move-far-left    :around #'njord*popups-save)
  (advice-add #'evil-window-move-far-right   :around #'njord*popups-save)

  ;; Don't block moving to/from popup windows
  (defun njord*ignore-window-parameters-in-popups (dir &optional arg window)
    (window-in-direction (cond ((eq dir 'up)   'above)
                               ((eq dir 'down) 'below)
                               (t dir))
                         window t arg windmove-wrap-around t))
  (advice-add #'windmove-find-other-window :override #'njord*ignore-window-parameters-in-popups))


(after! helm
  ;; Helm tries to clean up after itself, but shackle has already done this.
  ;; This fixes that. To reproduce, add a helm rule in `shackle-rules', open two
  ;; splits side-by-side, move to the buffer on the right and invoke helm. It
  ;; will close all but the left-most buffer.
  (setq-default helm-reuse-last-window-split-state t
                helm-split-window-in-side-p t)

  (after! helm-swoop
    (setq helm-swoop-split-window-function #'pop-to-buffer))

  (after! helm-ag
    ;; This prevents helm-ag from switching between windows and buffers.
    (defun njord*helm-ag-edit-done (orig-fn &rest args)
      (cl-letf (((symbol-function 'select-window) #'ignore))
        (apply orig-fn args))
      (njord/popup-close))
    (advice-add #'helm-ag--edit-commit :around #'njord*helm-ag-edit-done)
    (advice-add #'helm-ag--edit-abort  :around #'njord*helm-ag-edit-done)

    (defun njord*helm-ag-edit (orig-fn &rest args)
      (cl-letf (((symbol-function 'other-window) #'ignore)
                ((symbol-function 'switch-to-buffer) #'njord-popup-buffer))
        (apply orig-fn args)
        (with-current-buffer (get-buffer "*helm-ag-edit*")
          (use-local-map helm-ag-edit-map))))
    (advice-add #'helm-ag--edit :around #'njord*helm-ag-edit)))


(defsubst njord--switch-from-popup (location)
  (njord/popup-close)
  (switch-to-buffer (car location) nil t)
  (if (not (cdr location))
      (message "Unable to find location in file")
    (goto-char (cdr location))
    (recenter)))

(after! help-mode
  ;; Help buffers use `other-window' to decide where to open followed links,
  ;; which can be unpredictable. It should *only* replace the original buffer we
  ;; opened the popup from. To fix this these three button types need to be
  ;; redefined to set aside the popup before following a link.
  (define-button-type 'help-function-def
    :supertype 'help-xref
    'help-function
    (lambda (fun file)
      (require 'find-func)
      (when (eq file 'C-source)
        (setq file (help-C-file-name (indirect-function fun) 'fun)))
      (njord--switch-from-popup (find-function-search-for-symbol fun nil file))))

  (define-button-type 'help-variable-def
    :supertype 'help-xref
    'help-function
    (lambda (var &optional file)
      (when (eq file 'C-source)
        (setq file (help-C-file-name var 'var)))
      (njord--switch-from-popup (find-variable-noselect var file))))

  (define-button-type 'help-face-def
    :supertype 'help-xref
    'help-function
    (lambda (fun file)
      (require 'find-func)
      (njord--switch-from-popup (find-function-search-for-symbol fun 'defface file)))))


(after! magit
  (set! :popup "^\\*magit" :regexp t :size 0.5 :noesc t :autokill t)

  ;; magit doesn't need much coercing. It works with shackle as is, except for
  ;; one problem: following non-file magit links tends to open additional
  ;; popups. We want all this to be contained within one window, so...
  (defun njord-magit-popup-buffer (buffer)
    "Pop up the magit window with shackle."
    (cond ((njord-popup-p)
           (prog1 (njord-popup-switch-to-buffer buffer)
             (njord-hide-modeline-mode +1)))
          (t
           (magit-display-buffer-traditional buffer))))

  (defun njord-magit-quit-window (_kill-buffer)
    "Close the current magit window properly."
    (let ((last (current-buffer)))
      (cond ((when-let (dest (njord-buffers-in-mode
                              'magit-mode
                              (cl-loop for buf in (window-prev-buffers)
                                       unless (eq (car buf) last)
                                       collect (car buf))
                              t))
               (njord-popup-switch-to-buffer (car dest)))
             (kill-buffer last))
            (t
             (mapc #'kill-buffer
                   (njord-buffers-in-mode '(magit-mode magit-process-mode)
                                         (buffer-list) t))))))

  (setq magit-display-buffer-function #'njord-magit-popup-buffer
        magit-bury-buffer-function #'njord-magit-quit-window))


(after! mu4e
  (defun njord*mu4e-popup-window (buf _height)
    (njord-popup-buffer buf :size 10 :noselect t)
    buf)
  (advice-add #'mu4e~temp-window :override #'njord*mu4e-popup-window))


(after! multi-term
  (setq multi-term-buffer-name "njord:terminal"))


(after! neotree
  ;; Neotree has its own window/popup management built-in, which is difficult to
  ;; police. For example, switching perspectives will cause neotree to forget it
  ;; is a neotree pane.
  ;;
  ;; By handing neotree over to shackle, which is better integrated into the
  ;; rest of my config (and persp-mode), this is no longer a problem.
  (set! :popup " *NeoTree*" :align 'left :size 25)

  (defun +evil-neotree-display-fn (buf _alist)
    "Hand neotree off to shackle."
    (let ((win (njord-popup-buffer buf)))
      (setq neo-global--buffer (window-buffer win)
            neo-global--window win)))
  (setq neo-display-action '(+evil-neotree-display-fn))

  (defun +evil|neotree-fix-popup ()
    "Repair neotree state whenever its popup state is restored. This ensures
that `njord*popup-save' won't break it."
    (when (equal (buffer-name) neo-buffer-name)
      (setq neo-global--window (selected-window))))
  (add-hook 'njord-popup-mode-hook #'+evil|neotree-fix-popup))


(after! persp-mode
  (defun njord*persp-mode-restore-popups (&rest _)
    "Restore popup windows when loading a perspective from file."
    (dolist (window (window-list))
      (when-let (plist (window-parameter window 'popup))
        (with-selected-window window
          (unless njord-popup-mode
            (setq-local njord-popup-rules plist)
            (njord-popup-mode +1))))))
  (advice-add #'persp-load-state-from-file :after #'njord*persp-mode-restore-popups))


(after! quickrun
  ;; don't auto-focus quickrun windows, shackle handles that
  (setq quickrun-focus-p nil))


(after! twittering-mode
  (setq twittering-pop-to-buffer-function #'pop-to-buffer))


(after! wgrep
  ;; close the popup after you're done with a wgrep buffer
  (advice-add #'wgrep-abort-changes :after #'njord/popup-close)
  (advice-add #'wgrep-finish-edit   :after #'njord/popup-close))


(after! xref
  (defun njord*xref-follow-and-close (orig-fn &rest args)
    "Jump to the xref on the current line, select its window and close the popup
you came from."
    (interactive)
    (let ((popup-p (njord-popup-p))
          (window (selected-window)))
      (apply orig-fn args)
      (when popup-p (njord/popup-close window))))
  (advice-add #'xref-goto-xref :around #'njord*xref-follow-and-close))


;;
;; Major modes
;;

(after! plantuml-mode
  (defun njord*plantuml-preview-in-popup-window (orig-fn &rest args)
    (save-window-excursion
      (apply orig-fn args))
    (pop-to-buffer plantuml-preview-buffer))
  (advice-add #'plantuml-preview-string
              :around #'njord*plantuml-preview-in-popup-window))

;; Ensure these settings are attached to org-load-hook as late as possible,
;; giving other modules a chance to add their own hooks.
(defun njord|init-org-popups ()
  (add-hook! 'org-load-hook
    (set! :popup
      '("*Calendar*"         :size 0.4 :noselect t)
      '(" *Org todo*"        :size 5   :noselect t)
      '("*Org Note*"         :size 10)
      '("*Org Select*"       :size 20  :noselect t)
      '("*Org Links*"        :size 5   :noselect t)
      '("*Org Export Dispatcher*" :noselect t)
      '(" *Agenda Commands*" :noselect t)
      '("^\\*Org Agenda"     :regexp t :size 30)
      '("*Org Clock*"        :noselect t)
      '("^\\*Org Src"        :regexp t :size 0.35 :noesc t)
      '("*Edit Formulas*"    :size 10)
      '("^\\*Org-Babel"      :regexp t :size 25 :noselect t)
      '("^CAPTURE.*\\.org$"  :regexp t :size 20))

    ;; Org has its own window management system with a scorched earth philosophy
    ;; I'm not fond of. i.e. it kills all windows and monopolizes the frame. No
    ;; thanks. We can do better with shackle's help.
    (defun njord*suppress-delete-other-windows (orig-fn &rest args)
      (cl-letf (((symbol-function 'delete-other-windows)
                 (symbol-function 'ignore)))
        (apply orig-fn args)))
    (advice-add #'org-add-log-note :around #'njord*suppress-delete-other-windows)
    (advice-add #'org-capture-place-template :around #'njord*suppress-delete-other-windows)
    (advice-add #'org-export--dispatch-ui :around #'njord*suppress-delete-other-windows)

    ;; `org-edit-src-code' simply clones and narrows the buffer to a src block,
    ;; so we are secretly manipulating the same buffer. Since truely killing it
    ;; would kill the original org buffer we've got to do things differently.
    (defun njord*org-src-switch-to-buffer (buffer _context)
      (if (eq org-src-window-setup 'switch-invisibly)
          (set-buffer buffer)
        (pop-to-buffer buffer)))
    (advice-add #'org-src-switch-to-buffer :override #'njord*org-src-switch-to-buffer)

    ;; Ensure todo, agenda, and other minor popups handed off to shackle.
    (defun njord*org-pop-to-buffer (&rest args)
      (let ((buf (car args)))
        (pop-to-buffer
         (cond ((stringp buf) (get-buffer-create buf))
               ((bufferp buf) buf)
               (t (error "Invalid buffer %s" buf))))))
    (advice-add #'org-switch-to-buffer-other-window :override #'njord*org-pop-to-buffer)

    (after! org-agenda
      (setq org-agenda-window-setup 'other-window
            org-agenda-restore-windows-after-quit nil)

      ;; Hide modeline in org-agenda
      (add-hook 'org-agenda-finalize-hook #'njord-hide-modeline-mode)
      ;; Don't monopolize frame!
      (advice-add #'org-agenda :around #'njord*suppress-delete-other-windows)

      (map! :map org-agenda-mode-map
            :m [escape] 'org-agenda-Quit
            :m "ESC"    'org-agenda-Quit)
      (let ((map org-agenda-mode-map))
        (define-key map "q" 'org-agenda-Quit)
        (define-key map "Q" 'org-agenda-Quit)))))
(add-hook 'njord-init-hook #'njord|init-org-popups)

(provide 'core-popups)
