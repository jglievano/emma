;;; core-projects.el -*- lexical-binding: t; -*-

(defvar emma-project-hook nil
  "Hook run when a project is enabled. The name of the project's mode and its
state are passed in.")

(def-package! projectile
  :demand t
  :init (add-hook 'emma-init-hook #'projectile-mode)
  :config
  (setq projectile-cache-file (concat emma-cache-dir "projectile.cache")
        projectile-enable-caching (not noninteractive)
        projectile-file-exists-remote-cache-expire nil
        projectile-indexing-method 'alien
        projectile-known-projects-file (concat emma-cache-dir "projectile.projects")
        projectile-require-project-root nil
        projectile-project-root-files
        '(".git" ".hg" ".svn" ".project" "package.json" "setup.py" "Gemfile"
          "build.gradle")
        projectile-other-file-alist
        (append '(("less" "css")
                  ("styl" "css")
                  ("sass" "css")
                  ("scss" "css")
                  ("css" "scss" "sass" "less" "styl")
                  ("jade" "html")
                  ("pug" "html")
                  ("html" "jade" "pug" "jsx" "tsx"))
                projectile-other-file-alist)
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
        projectile-globally-ignored-files '(".DS_Store" "Icon")
        projectile-globally-ignored-directories
        (append (list emma-local-dir ".sync")
                projectile-globally-ignored-directories))

  ;; Projectile root-searching functions can cause an infinite loop on TRAMP
  ;; connections, so disable them.
  (defun emma*projectile-locate-dominating-file (orig-fn &rest args)
    "Don't traverse the file system if a remote connection."
    (unless (file-remote-p default-directory)
      (apply orig-fn args)))
  (advice-add #'projectile-locate-dominating-file :around #'emma*projectile-locate-dominating-file)

  (defun emma*projectile-cache-current-file (orig-fun &rest args)
    "Don't cache ignored files."
    (unless (cl-some (lambda (path)
                       (string-prefix-p buffer-file-name
                                        (expand-file-name path)))
                     (projectile-ignored-directories))
      (apply orig-fun args)))
  (advice-add #'projectile-cache-current-file :around #'emma*projectile-cache-current-file))


;;
;; Library
;;

(defun emma-project-p (&optional strict-p)
  "Whether or not this buffer is currently in a project or not."
  (let ((projectile-require-project-root strict-p))
    (projectile-project-p)))

(defun emma-project-root (&optional strict-p)
  "Get the path to the root of your project."
  (let ((projectile-require-project-root strict-p))
    (ignore-errors (projectile-project-root))))

(defun emma*project-root (&rest _)
  "An advice function used to replace project-root-detection functions in other
libraries."
  (emma-project-root))

(defmacro emma-project-has! (files)
  "Checks if the project has the specified FILES, relative to the project root,
unless the path begins with ./ or ../, in which case it's relative to
`default-directory'. Recognizes (and ...) and/or (or ...) forms."
  (emma--resolve-paths files (emma-project-root)))


;;
;; Projects
;;

(defvar-local emma-project nil
  "A list of project mode symbols to enable. Used for .dir-locals.el.")

(defun emma|autoload-project-mode ()
  "Auto-enable projects listed in `emma-project', which is meant to be set from
.dir-locals.el files."
  (dolist (mode emma-project)
    (funcall mode)))
(add-hook 'after-change-major-mode-hook #'emma|autoload-project-mode)

(defmacro def-project-mode! (name &rest plist)
  "Define a project minor-mode named NAME and declare where and how it is
activated. Project modes allow you to configure 'sub-modes' for major-modes that
are specific to a specific folder, certain project structure, framework or
arbitrary context you define. These project modes can have their own settings,
keymaps, hooks, snippets, etc.

This creates NAME-hook and NAME-map as well.

A project can be enabled through .dir-locals.el too, if `emma-project' is set to
the name (symbol) of the project mode(s) to enable.

PLIST may contain any of these properties, which are all checked to see if NAME
should be activated. If they are *all* true, NAME is activated.

  :modes MODES -- if buffers are derived from MODES (one or a list of symbols).

  :files FILES -- if project contains FILES; takes a string or a form comprised
    of nested (and ...) and/or (or ...) forms. Each path is relative to the
    project root, however, if prefixed with a '.' or '..', it is relative to the
    current buffer.

  :match REGEXP -- if file name matches REGEXP

  :when PREDICATE -- if PREDICATE returns true (can be a form or the symbol of a
    function)

  :init FORM -- FORM to run the first time this project mode is enabled.

Relevant: `emma-project-hook'."
  (declare (indent 1))
  (let ((modes (plist-get plist :modes))
        (files (plist-get plist :files))
        (when  (plist-get plist :when))
        (match (plist-get plist :match))
        (init-form (plist-get plist :init))
        (keymap-sym (intern (format "%s-map" name))))
    `(progn
       (defvar ,keymap-sym (make-sparse-keymap)
         ,(concat "Keymap for `" (symbol-name name) "'"))
       (define-minor-mode ,name
         "A project minor mode."
         :init-value nil
         :keymap ,keymap-sym)
       ,(when (or modes match files when)
          `(associate! ,name
             :modes ,modes
             :match ,match
             :files ,files
             :when ,when))
       (add-hook! ,name
         (run-hook-with-args 'emma-project-hook ',name))
       ,(when init-form
          `(add-transient-hook! ',(intern (format "%s-hook" name))
             ,init-form)))))

(provide 'core-projects)
;;; core-projects.el ends here
