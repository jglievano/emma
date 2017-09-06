;;; core-keybinds.el -*- lexical-binding: t; -*-

;; A centralized keybinds system, integrated with `which-key' to preview
;; available keybindings. All built into one powerful macro: `map!'. If evil is
;; never loaded, then evil bindings set with `map!' will be ignored.

(defvar njord-leader-key "SPC"
  "The leader prefix key, for global commands.")

(defvar njord-localleader-key "SPC m"
  "The localleader prefix key, for major-mode specific commands.")

(defvar njord-evil-state-alist
  '((?n . normal)
    (?v . visual)
    (?i . insert)
    (?e . emacs)
    (?o . operator)
    (?m . motion)
    (?r . replace))
  "A list of cons cells that map a letter to a evil state symbol.")


;;
(def-package! which-key
  :demand t
  :config
  (setq which-key-sort-order #'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 5)
  ;; embolden local bindings
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  (which-key-setup-side-window-bottom)
  (add-hook 'njord-init-hook #'which-key-mode))


;;
(defun njord--keybind-register (key desc &optional modes)
  "Register a description for KEY with `which-key' in MODES.
  KEYS should be a string in kbd format.
  DESC should be a string describing what KEY does.
  MODES should be a list of major mode symbols."
  (if modes
      (dolist (mode modes)
        (which-key-add-major-mode-key-based-replacements mode key desc))
    (which-key-add-key-based-replacements key desc)))


(defun njord--keyword-to-states (keyword)
  "Convert a KEYWORD into a list of evil state symbols.
For example, :nvi will map to (list 'normal 'visual 'insert). See
`njord-evil-state-alist' to customize this."
  (cl-loop for l across (substring (symbol-name keyword) 1)
           if (cdr (assq l njord-evil-state-alist))
             collect it
           else
             do (error "not a valid state: %s" l)))


;; Register keywords for proper indentation (see `map!')
(put ':after        'lisp-indent-function 'defun)
(put ':desc         'lisp-indent-function 'defun)
(put ':leader       'lisp-indent-function 'defun)
(put ':local        'lisp-indent-function 'defun)
(put ':localleader  'lisp-indent-function 'defun)
(put ':map          'lisp-indent-function 'defun)
(put ':map*         'lisp-indent-function 'defun)
(put ':mode         'lisp-indent-function 'defun)
(put ':prefix       'lisp-indent-function 'defun)
(put ':textobj      'lisp-indent-function 'defun)
(put ':unless       'lisp-indent-function 'defun)
(put ':when         'lisp-indent-function 'defun)

;; specials
(defvar njord--keymaps nil)
(defvar njord--prefix  nil)
(defvar njord--defer   nil)
(defvar njord--local   nil)

(defmacro map! (&rest rest)
  "A nightmare of a key-binding macro that will use `evil-define-key*',
`define-key', `local-set-key' and `global-set-key' depending on context and
plist key flags (and whether evil is loaded or not). It was designed to make
binding multiple keys more concise, like in vim.
If evil isn't loaded, it will ignore evil-specific bindings.
States
    :n  normal
    :v  visual
    :i  insert
    :e  emacs
    :o  operator
    :m  motion
    :r  replace
    These can be combined (order doesn't matter), e.g. :nvi will apply to
    normal, visual and insert mode. The state resets after the following
    key=>def pair.
    If states are omitted the keybind will be global.
    This can be customized with `njord-evil-state-alist'.
    :textobj is a special state that takes a key and two commands, one for the
    inner binding, another for the outer.
Flags
    (:mode [MODE(s)] [...])    inner keybinds are applied to major MODE(s)
    (:map [KEYMAP(s)] [...])   inner keybinds are applied to KEYMAP(S)
    (:map* [KEYMAP(s)] [...])  same as :map, but deferred
    (:prefix [PREFIX] [...])   assign prefix to all inner keybindings
    (:after [FEATURE] [...])   apply keybinds when [FEATURE] loads
    (:local [...])             make bindings buffer local; incompatible with keymaps!
Conditional keybinds
    (:when [CONDITION] [...])
    (:unless [CONDITION] [...])
Example
    (map! :map magit-mode-map
          :m \"C-r\" 'do-something           ; assign C-r in motion state
          :nv \"q\" 'magit-mode-quit-window  ; assign to 'q' in normal and visual states
          \"C-x C-r\" 'a-global-keybind
          (:when IS-MAC
           :n \"M-s\" 'some-fn
           :i \"M-o\" (lambda (interactive) (message \"Hi\"))))"
  (let ((njord--keymaps njord--keymaps)
        (njord--prefix  njord--prefix)
        (njord--defer   njord--defer)
        (njord--local   njord--local)
        key def states forms desc modes)
    (while rest
      (setq key (pop rest))
      (cond
       ;; it's a sub expr
       ((listp key)
        (push (macroexpand `(map! ,@key)) forms))

       ;; it's a flag
       ((keywordp key)
        (cond ((eq key :leader)
               (push 'njord-leader-key rest)
               (setq key :prefix
                     desc "<leader>"))
              ((eq key :localleader)
               (push 'njord-localleader-key rest)
               (setq key :prefix
                     desc "<localleader>")))
        (pcase key
          (:when    (push `(if ,(pop rest)       ,(macroexpand `(map! ,@rest))) forms) (setq rest '()))
          (:unless  (push `(if (not ,(pop rest)) ,(macroexpand `(map! ,@rest))) forms) (setq rest '()))
          (:after   (push `(after! ,(pop rest)   ,(macroexpand `(map! ,@rest))) forms) (setq rest '()))
          (:desc    (setq desc (pop rest)))
          (:map*    (setq njord--defer t) (push :map rest))
          (:map
            (setq njord--keymaps (njord-enlist (pop rest))))
          (:mode
            (setq modes (njord-enlist (pop rest)))
            (unless njord--keymaps
              (setq njord--keymaps
                    (cl-loop for m in modes
                             collect (intern (format "%s-map" (symbol-name m)))))))
          (:textobj
            (let* ((key (pop rest))
                   (inner (pop rest))
                   (outer (pop rest)))
              (push (macroexpand `(map! (:map evil-inner-text-objects-map ,key ,inner)
                                        (:map evil-outer-text-objects-map ,key ,outer)))
                    forms)))
          (:prefix
            (let ((def (pop rest)))
              (setq njord--prefix `(vconcat ,njord--prefix (kbd ,def)))
              (when desc
                (push `(njord--keybind-register ,(key-description (eval njord--prefix))
                                               ,desc ',modes)
                      forms)
                (setq desc nil))))
          (:local
           (setq njord--local t))
          (_ ; might be a state njord--prefix
           (setq states (njord--keyword-to-states key)))))

       ;; It's a key-def pair
       ((or (stringp key)
            (characterp key)
            (vectorp key)
            (symbolp key))
        (unwind-protect
            (catch 'skip
              (when (symbolp key)
                (setq key `(kbd ,key)))
              (when (stringp key)
                (setq key (kbd key)))
              (when njord--prefix
                (setq key (append njord--prefix (list key))))
              (unless (> (length rest) 0)
                (user-error "map! has no definition for %s key" key))
              (setq def (pop rest))
              (when desc
                (push `(njord--keybind-register ,(key-description (eval key))
                                              ,desc ',modes)
                      forms))
              (cond ((and njord--local njord--keymaps)
                     (push `(lwarn 'njord-map :warning
                                   "Can't local bind '%s' key to a keymap; skipped"
                                   ,key)
                           forms)
                     (throw 'skip 'local))
                    ((and njord--keymaps states)
                     (unless (featurep 'evil) (throw 'skip 'evil))
                     (dolist (keymap njord--keymaps)
                       (push `(,(if njord--defer 'evil-define-key 'evil-define-key*)
                               ',states ,keymap ,key ,def)
                             forms)))
                    (states
                     (unless (featurep 'evil) (throw 'skip 'evil))
                     (dolist (state states)
                       (push `(define-key
                                ,(intern (format "evil-%s-state-%smap" state (if njord--local "local-" "")))
                                ,key ,def)
                             forms)))
                    (njord--keymaps
                     (dolist (keymap njord--keymaps)
                       (push `(define-key ,keymap ,key ,def) forms)))
                    (t
                     (push `(,(if njord--local 'local-set-key 'global-set-key) ,key ,def)
                           forms))))
          (setq states '()
                njord--local nil
                desc nil)))

       (t (user-error "Invalid key %s" key))))
    `(progn ,@(nreverse forms))))

(provide 'core-keybinds)
