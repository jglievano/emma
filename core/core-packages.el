;;; core-packages.el --- package management system -*- lexical-binding: t; -*-

;; Emacs package management is opinionated. Unfortunately, so am I. I've bound
;; together `use-package', `quelpa' and package.el to create my own,
;; rolling-release, lazily-loaded package management system for Emacs.
;;
;; The three key commands are:
;;
;; + `make install` or `emma/packages-install': Installs packages that are
;;   wanted, but not installed.
;; + `make update` or `emma/packages-update': Updates packages that are
;;   out-of-date.
;; + `make autoremove` or `emma/packages-autoremove': Uninstalls packages that
;;   are no longer needed.
;;
;; This system reads packages.el files located in each activated module (and one
;; in `emma-core-dir'). These contain `package!` blocks that tell EMMA what
;; plugins to install and where from.
;;
;; Why all the trouble? Because:
;; 1. Scriptability: I live in the command line. I want a programmable
;;    alternative to `list-packages' for updating and installing packages.
;; 2. Flexibility: I want packages from sources other than ELPA. Primarily
;;    github, because certain plugins are out-of-date through official channels,
;;    have changed hands, have a superior fork, or simply aren't in any ELPA
;;    repo.
;; 3. Stability: I used Cask before this. It would error out with cyrptic errors
;;    depending on the version of Emacs I used and the alignment of the planets.
;;    No more.
;; 4. Performance: A minor point, but this system is lazy-loaded (more so if you
;;    byte-compile). Not having to initialize package.el (or check that your
;;    packages are installed) every time you start up Emacs affords us precious
;;    seconds.
;; 5. Simplicity: No Cask, no external dependencies (unless you count make),
;;    just Emacs. Arguably, my config is still over-complicated, but shhh, it's
;;    fine. Everything is fine.
;;
;; You should be able to use package.el commands without any conflicts, but to
;; be absolutely certain use the emma alternatives:
;;
;;    + `package-install':          `emma/install-package'
;;    + `package-reinstall':        `emma/reinstall-package'
;;    + `package-delete':           `emma/delete-package'
;;    + `package-update':           `emma/update-package'
;;    + `package-autoremove':       `emma/packages-autoremove'
;;    + `package-refresh-contents': `emma/refresh-packages'
;;
;; See core/autoload/packages.el for more functions.

(defvar emma-init-p nil
  "Non-nil if emma is done initializing (once `emma-post-init-hook' is done). If
this is nil after Emacs has started something is wrong.")

(defvar emma-package-init-p nil
  "If non-nil, emma's package system has been initialized (by
`emma-initialize'). This will be nill if you byte-compile your configuration (as
intended).")

(defvar emma-init-time nil
  "The time it took, in seconds, for EMMA Emacs to initialize.")

(defvar emma-modules ()
  "A hash table of enabled modules. Set by `emma-initialize-modules'.")

(defvar emma-packages ()
  "A list of enabled packages. Each element is a sublist, whose CAR is the
package's name as a symbol, and whose CDR is the plist supplied to its
`package!' declaration. Set by `emma-initialize-packages'.")

(defvar emma-core-packages
  '(persistent-soft quelpa use-package)
  "A list of packages that must be installed (and will be auto-installed if
missing) and shouldn't be deleted.")

(defvar emma-disabled-packages ()
  "A list of packages that should be ignored by `def-package!'.")

(defvar emma-reload-hook nil
  "A list of hooks to run when `emma/reload' is called.")

(defvar emma--site-load-path load-path
  "The load path of built in Emacs libraries.")

(defvar emma--package-load-path ()
  "The load path of package libraries installed via ELPA or QUELPA.")

(defvar emma--base-load-path
  (append (list emma-core-dir emma-modules-dir)
          emma--site-load-path)
  "A backup of `load-path' before it was altered by `emma-initialize'. Used as a
base by `emma!' and for calculating how many packages exist.")

(defvar emma--module nil)
(defvar emma--refresh-p nil)

(setq load-prefer-newer (or noninteractive emma-debug-mode)
      package--init-file-ensured t
      package-user-dir emma-vendor-dir
      package-enable-at-startup nil
      package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/"))

      ;; security settings
      gnutls-verify-error (not (getenv "INSECURE")) ; you shouldn't use this
      tls-checktrust gnutls-verify-error
      tls-program (list "gnutls-cli --x509cafile %t -p %p %h"
                        "gnutls-cli -p %p %h"
                        "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof")

      use-package-always-defer t
      use-package-always-ensure nil
      use-package-debug nil
      use-package-verbose emma-debug-mode
      use-package-minimum-reported-time (if emma-debug-mode 0 0.1)

      ;; Don't track MELPA, we'll use package.el for that
      quelpa-checkout-melpa-p nil
      quelpa-update-melpa-p nil
      quelpa-melpa-recipe-stores nil
      quelpa-self-upgrade-p nil
      quelpa-verbose emma-debug-mode
      quelpa-dir (expand-file-name "quelpa" emma-packages-dir)

      byte-compile-dynamic nil
      byte-compile-verbose emma-debug-mode
      byte-compile-warnings '(not free-vars
                                  unresolved
                                  noruntime
                                  lexical
                                  make-local))

(defun emma-initialize (&optional force-p)
  "If core/core.el is byte-compiled, this function will be avoided
to speed up startup."
  ;; Called early during initialization; only use native functions!
  (when (or (not emma-package-init-p) force-p)
    (unless noninteractive
      (message "Emma initialized"))

    (setq load-path emma--base-load-path
          package-activated-list nil)

    ;; Ensure core folders exist
    (dolist (dir (list emma-local-dir emma-etc-dir emma-cache-dir package-user-dir))
      (unless (file-directory-p dir)
        (make-directory dir t)))

    (package-initialize t)

    (setq emma--package-load-path (directory-files package-user-dir t "^[^.]" t)
          load-path (append load-path emma--package-load-path))

    ;; Ensure core packages are installed
    (dolist (pkg emma-core-packages)
      (unless (package-installed-p pkg)
        (unless emma--refresh-p
          (package-refresh-contents)
          (setq emma--refresh-p t))
        (let ((inhibit-message t))
          (package-install pkg))
        (if (package-installed-p pkg)
            (message "Installed %s" pkg)
          (error "Couldn't install %s" pkg))))

    (load "use-package" nil t)

    (setq emma-package-init-p t)))

(defun emma-initialize-autoloads ()
  "Ensures that `emma-autoload-file' exists and is loaded. Otherwise run
`emma/reload-autoloads' to generate it."
  (unless (file-exists-p emma-autoload-file)
    (quiet! (emma/reload-autoloads))))

(defun doom*++*emma-initialize-packages (&optional force-p load-p)
  "Crawls across your emacs.d to fill `emma-modules' (from init.el) and
`emma-packages' (from packages.el files), if they aren't set already.

If FORCE-P is non-nil, do it even if they are.

This aggressively reloads core autoload files."
  (emma-initialize force-p)
  (let ((noninteractive t)
        (load-fn
         (lambda (file &optional noerror)
           (condition-case-unless-debug ex
               (load file noerror :nomessage :nosuffix)
             ('error
              (error (format "(emma-initialize-packages) %s in %s: %s"
                             (car ex)
                             (file-relative-name file emma-emacs-dir)
                             (error-message-string ex))
                     :error))))))
    (when (or force-p (not emma-modules))
      (setq emma-modules nil)
      (funcall load-fn (expand-file-name "init.el" emma-emacs-dir))
      (when load-p
        (let (noninteractive)
          (funcall load-fn (emma-module-path :private user-login-name "init.el") t)
          (funcall load-fn (expand-file-name "core.el" emma-core-dir)))
        (mapc load-fn (file-expand-wildcards (expand-file-name "autoload/*.el" emma-core-dir))))
      (emma|finalize))
    (when (or force-p (not emma-packages))
      (setq emma-packages nil)
      (funcall load-fn (expand-file-name "packages.el" emma-core-dir))
      (cl-loop for (module . submodule) in (emma--module-pairs)
               for path = (emma-module-path module submodule "packages.el")
               do
               (let ((emma--module (cons module submodule)))
                 (funcall load-fn path t))))))

(defun doom*++*emma-initialize-modules (modules)
  "Adds MODULES to `emma-modules'. MODULES must be in mplist format.

  e.g '(:feature evil :lang emacs-lisp javascript java)"
  (unless emma-modules
    (setq emma-modules (make-hash-table :test #'equal :size (+ 5 (length modules)))))
  (let (mode)
    (dolist (m modules)
      (cond ((keywordp m)
             (setq mode m))
            ((not mode)
             (error "No namespace specified on `emma!' for %s" m))
            ((listp m)
             (emma-module-enable mode (car m) (cdr m)))
            (t
             (emma-module-enable mode m))))))

(defun doom*++*emma-module-path (module submodule &optional file)
  "Get the full path to a module: e.g. :lang emacs-lisp maps to
~/.emacs.d/modules/lang/emacs-lisp/ and will append FILE if non-nil."
  (when (keywordp module)
    (setq module (substring (symbol-name module) 1)))
  (when (symbolp submodule)
    (setq submodule (symbol-name submodule)))
  (expand-file-name (concat module "/" submodule "/" file)
                    emma-modules-dir))

(defun doom*++*emma-module-flags (module submodule)
  "Returns a list of flags provided for MODULE SUBMODULE."
  (and (hash-table-p emma-modules)
       (gethash (cons module submodule) emma-modules)))

(defun doom*++*emma-module-loaded-p (module submodule)
  "Returns t if MODULE->SUBMODULE is present in `emma-modules'."
  (and (emma-module-flags module submodule) t))

(defun doom*++*emma-module-enable (module submodule &optional flags)
  "Adds MODULE and SUBMODULE to `emma-modules', overwriting it if it exists.

MODULE is a keyword, SUBMODULE is a symbol. e.g. :lang 'emacs-lisp.

Used by `require!' and `depends-on!'."
  (puthash (cons module submodule)
           (emma-enlist (or flags t))
           emma-modules))

(defun doom*++*emma--module-pairs ()
  "Returns `emma-modules' as a list of (MODULE . SUBMODULE) cons cells. The list
is sorted by order of insertion unless ALL-P is non-nil. If ALL-P is non-nil,
include all modules, enabled or otherwise."
  (unless (hash-table-p emma-modules)
    (error "emma-modules is uninitialized"))
  (cl-loop for key being the hash-keys of emma-modules
           collect key))

(defun doom*++*emma--module-paths (&optional append-file)
  "Returns a list of absolute file paths to activated modules, with APPEND-FILE
added, if the file exists."
  (cl-loop for (module . submodule) in (emma--module-pairs)
           for path = (emma-module-path module submodule append-file)
           if (file-exists-p path)
           collect path))



(defun emma--display-benchmark ()
  (message "Loaded %s packages in %.03fs"
           (- (length load-path) (length emma--base-load-path))
           (setq emma-init-time (float-time (time-subtract after-init-time before-init-time)))))

;; TODO: (autoload 'use-package "use-package" nil nil 'macro)

(defmacro emma! (&rest modules)
  "Bootstrap EMMA Emacs.")

(defmacro doom*++*emma! (&rest modules)
  "MODULES is an malformed plist of modules to load."
  (emma-initialize-modules modules)
  (when (and user-login-name
             (not (emma-module-loaded-p :private (intern user-login-name))))
    (emma-module-enable :private user-login-name))
  `(let (file-name-handler-alist)
     (setq emma-modules ',emma-modules)

     (unless noninteractive
       (load ,(emma-module-path :private user-login-name "init") t t)
       ,@(cl-loop for (module . submodule) in (emma--module-pairs)
                  collect `(require! ,module ,submodule nil t))

       (when (display-graphic-p)
         (require 'server)
         (unless (server-running-p)
           (server-start)))

       (add-hook 'emma-init-hook #'emma--display-benchmark t))))

(defmacro def-package! (name &rest plist)
  "A thin wrapper around `use-package'.

Ignores the package if its NAME is present in `emma-disabled-packages'."
  (when (and (memq name emma-disabled-packages)
             (not (memq :disabled plist)))
    (setq plist (append (list :disabled t) plist)))
  `(use-package ,name ,@plist))

(defmacro doom*++*def-package-hook! (package when &rest body)
  "Reconfigures a package's `def-package!' block.

Under the hood, this uses use-package's `use-package-inject-hooks'.

PACKAGE is a symbol; the package's name.
WHEN should be one of the following:
  :pre-init :post-init :pre-config :post-config :disable

If WHEN is :disable then BODY is ignored, and EMMA will be instructed to ignore
all `def-package!' blocks for PACKAGE.

WARNING: If :pre-init or :pre-config hooks return nil, the original
`def-package!''s :init/:config block (respectively) is overwritten, so remember
to have them return non-nil (or exploit that to overwrite Emma's config)."
  (declare (indent defun))
  (cond ((eq when :disable)
         (push package emma-disabled-packages)
         nil)
        ((memq when '(:pre-init :post-init :pre-config :post-config))
         `(progn
            (setq use-package-inject-hooks t)
            (add-hook!
              ',(intern (format "use-package--%s--%s-hook"
                                package
                                (substring (symbol-name when) 1)))
              ,@body)))
        (t
         (error "'%s' isn't a valid hook for def-package-hook!" when))))

(defmacro load! (filesym &optional path noerror)
  "Load a file relative to the current executing file (`load-file-name').

FILESYM is either a symbol or string representing the file to load. PATH is
where to look for the file (a string representing a directory path), by default
it is relative to `load-file-name', `byte-compile-current-file' or
`buffer-file-name' (in that order).

If NOERROR is non-nil, don't throw an error if the file doesn't exist."
  (let ((path (or (and path (or (and (symbolp path) (symbol-value path))
                                (and (stringp path) path)
                                (and (listp path) (eval path))))
                  (and load-file-name (file-name-directory load-file-name))
                  (and (bound-and-true-p byte-compile-current-file)
                       (file-name-directory byte-compile-current-file))
                  (and buffer-file-name
                       (file-name-directory buffer-file-name))))
        (filename (cond ((stringp filesym) filesym)
                        ((symbolp filesym) (symbol-name filesym))
                        (t (error "load! expected a string or symbol, got %s (a %s)"
                                  filesym (type-of filesym))))))
    (unless path
      (error "Could not find %s" filename))
    (let ((file (expand-file-name (concat filename ".el") path)))
      (if (file-exists-p file)
          `(load ,(file-name-sans-extension file) ,noerror
                 ,(not emma-debug-mode))
        (unless noerror
          (error "Could not load! file %s" file))))))

(defmacro doom*++*require! (module submodule &optional flags reload-p)
  "Loads the module specified by MODULE (a property) and SUBMODULE (a symbol).

The module is only loaded once. If RELOAD-P is non-nil, load it again."
  (let ((loaded-p (emma-module-loaded-p module submodule)))
    (when (or reload-p (not loaded-p))
      (unless loaded-p
        (emma-module-enable module submodule flags))
      `(condition-case-unless-debug ex
           (let ((emma--module ',(cons module submodule)))
             (load! config ,(emma-module-path module submodule) t))
         ('error
          (lwarn 'emma-modules :error
                 "%s in '%s %s' -> %s"
                 (car ex) ,module ',submodule
                 (error-message-string ex)))))))

(defmacro doom*++*featurep! (module &optional submodule flag)
  "A convenience macro wrapper for `emma-module-loaded-p'. It is evaluated at
compile-time/macro-expansion time."
  (unless submodule
    (unless emma--module
      (error "featurep! was used incorrectly (emma--module wasn't unset)"))
    (setq flag module
          module (car emma--module)
          submodule (cdr emma--module)))
  (if flag
      (and (memq flag (emma-module-flags module submodule)) t)
    (emma-module-loaded-p module submodule)))


;;
;; Declarative macros
;;
(defmacro package! (name &rest plist)
  "Declares a package and how to install it.

This macro is declarative and does not load nor install packages. It is used to
populate `emma-packages' with metadata about the packages Emma needs to keep
track of.

Only use this macro in a module's packages.el file.

Accepts the following properties:

  :ignore FORM Do not install this package if FORM is non-nil."
  (declare (indent defun))
  (let* ((old-plist (assq name emma-packages)))
    (when-let (ignore (plist-get plist :ignore))
      (plist-put plist :ignore (eval ignore)))
    (dolist (prop '(:ignore))
      (when-let (val (plist-get plist prop))
        (plist-put plist prop (eval val))))
    `(progn
       (when ,(and old-plist t)
         (assq-delete-all ',name emma-packages))
       (push ',(cons name plist) emma-packages))))

(defmacro doom*++*package! (name &rest plist)
  (declare (indent defun))
  (let* ((old-plist (assq name emma-packages))
         (pkg-recipe (or (plist-get plist :recipe)
                         (and old-plist (plist-get old-plist :recipe))))
         (pkg-pin    (or (plist-get plist :pin)
                         (and old-plist (plist-get old-plist :pin)))))
    (when pkg-recipe
      (when (= 0 (% (length pkg-recipe) 2))
        (plist-put plist :recipe (cons name pkg-recipe)))
      (when pkg-pin
        (plist-put plist :pin nil)))
    (dolist (prop '(:ignore :freeze))
      (when-let (val (plist-get plist prop))
        (plist-put plist prop (eval val))))
    `(progn
       (when ,(and pkg-pin t)
         (cl-pushnew (cons ',name ,pkg-pin) package-pinned-packages
                     :test #'eq :key #'car))
       (when ,(and old-plist t)
         (assq-delete-all ',name emma-packages))
       (push ',(cons name plist) emma-packages))))

(defmacro doom*++*depends-on! (module submodule)
  "Declares that this module depends on another.

Only use this macro in a module's packages.el file.

MODULE is a keyword, and SUBMODULE is a symbol. Under the hood, this simply
loads MODULE SUBMODULE's packages.el file."
  (emma-module-enable module submodule)
  `(load! packages ,(emma-module-path module submodule) t))

(defun doom*++*emma/reload ()
  "Reload `load-path' and recompile files (if necessary).

Use this when `load-path' is out of sync with your plugins. This should only
happen if you manually modify/update/install packages from outside Emacs, while
an Emacs session is running.

This isn't necessary if you use Emma's package management commands because they
call `emma/reload' remotely (through emacsclient)."
  (interactive)
  (cond (noninteractive
         (message "Reloading...")
         (require 'server)
         (unless (ignore-errors (server-eval-at "server" '(emma/reload)))
           (message "Recompiling")
           (emma/recompile)))
        (t
         (emma-initialize t)
         (emma/recompile)
         (message "Reloaded %d packages" (length emma--package-load-path))
         (run-with-timer 1 nil #'redraw-display)
         (run-hooks 'emma-reload-hook))))

(defun doom*++*emma/reload-autoloads ()
  "Refreshes the autoloads.el file, specified by `emma-autoload-file'.

It scans and reads core/autoload/*.el, modules/*/*/autoload.el and
modules/*/*/autoload/*.el, and generates an autoloads file at the path specified
by `emma-autoload-file'. This file tells Emacs where to find lazy-loaded
functions.

This should be run whenever init.el or an autoload file is modified. Running
'make autoloads' from the commandline executes this command."
  (interactive)
  ;; This function must not use autoloaded functions or external dependencies.
  ;; It must assume nothing is set up!
  (emma-initialize-packages (not noninteractive))
  (let ((evil-p (emma-module-loaded-p :feature 'evil))
        (targets
         (file-expand-wildcards
          (expand-file-name "autoload/*.el" emma-core-dir))))
    (dolist (path (emma--module-paths))
      (let ((auto-dir  (expand-file-name "autoload" path))
            (auto-file (expand-file-name "autoload.el" path)))
        (when (file-exists-p auto-file)
          (push auto-file targets))
        (when (file-directory-p auto-dir)
          (dolist (file (file-expand-wildcards (expand-file-name "*.el" auto-dir) t))
            ;; Make evil*.el autoload files a special case; don't load
            ;; them unless evil is enabled.
            (unless (and (string-prefix-p "evil" (file-name-nondirectory file))
                         (not evil-p))
              (push file targets))))))
    (when (file-exists-p emma-autoload-file)
      (delete-file emma-autoload-file)
      (message "Deleted old autoloads.el"))
    (dolist (file (reverse targets))
      (message (if (update-file-autoloads file nil emma-autoload-file)
                   "Nothing in %s"
                 "Scanned %s")
               (file-relative-name file emma-emacs-dir)))
    (let ((buf (get-file-buffer emma-autoload-file))
          current-sexp)
      (unwind-protect
          (condition-case-unless-debug ex
              (with-current-buffer buf
                (save-buffer)
                (goto-char (point-min))
                (while (re-search-forward "^(" nil t)
                  (save-excursion
                    (backward-char)
                    (setq current-sexp (read (thing-at-point 'sexp t)))
                    (eval current-sexp t))
                  (forward-char))
                (message "Finished generating autoloads.el!"))
            ('error
             (delete-file emma-autoload-file)
             (error "Error in autoloads.el: (%s %s ...) %s -- %s"
                    (nth 0 current-sexp)
                    (nth 1 current-sexp)
                    (car ex) (error-message-string ex))))
        (kill-buffer buf)))))

(defun doom*++*emma/compile (&optional lite-p only-recompile-p)
  "Byte compiles your emacs configuration.

Specifically, this byte-compiles init.el, core/*.el, core/autoload/*.el &
modules/*/*/**.el. It ignores unit tests and files with `no-byte-compile'
enabled.

EMMA Emacs was designed to benefit from byte-compilation, but the process may
take a while. Also, while your config files are byte-compiled, changes to them
will not take effect! Use `emma/clean-compiled' or `make clean' to undo
byte-compilation.

If LITE-P is non-nil, only compile the core EMMA files (init.el & core/**/*.el).

If ONLY-RECOMPILE-P is non-nil, only recompile out-of-date files."
  (interactive "P")
  ;; Ensure all relevant config files are loaded and up-to-date. This way we
  ;; don't need eval-when-compile and require blocks scattered all over.
  (emma-initialize-packages t noninteractive)
  (let ((targets
         (cond ((equal (car command-line-args-left) "--")
                (cl-loop for file in (cdr command-line-args-left)
                         if (file-exists-p file)
                         collect (expand-file-name file)
                         finally do (setq command-line-args-left nil)) )
               (t
                (append (list (expand-file-name "init.el" emma-emacs-dir)
                              emma-core-dir)
                        (unless lite-p (emma--module-paths))))))
        (total-success 0)
        (total-fail 0)
        (total-nocomp 0)
        (use-package-expand-minimally t))
    (let ((el-files (cl-loop for path in targets
                             if (file-directory-p path)
                               nconc (nreverse (directory-files-recursively path "\\.el$"))
                             else if (file-exists-p path)
                               collect path)))
      (dolist (file el-files)
        (when (or (not only-recompile-p)
                  (let ((elc-file (byte-compile-dest-file file)))
                    (and (file-exists-p elc-file)
                         (file-newer-than-file-p file elc-file))))
          (let ((result (if (string-match-p "/test/.+\\.el$" file)
                            'no-byte-compile
                          (byte-compile-file file)))
                (short-name (file-relative-name file emma-emacs-dir)))
            (cl-incf
             (cond ((eq result 'no-byte-compile)
                    (message! (dark (white "Ignored %s" short-name)))
                    total-nocomp)
                   ((null result)
                    (message! (red "Failed to compile %s" short-name))
                    total-fail)
                   (t
                    (message! (green "Compiled %s" short-name))
                    total-success))))))
      (message!
       (bold
        (color (if (= total-fail 0) 'green 'red)
               "%s %s file(s) %s"
               (if only-recompile-p "Recompiled" "Compiled")
               (format (if el-files "%d/%d" "%d")
                       total-success
                       (- (length el-files) total-nocomp))
               (format "(%s ignored)" total-nocomp)))))))

(defun doom*++*emma/recompile ()
  "Recompile any out-of-date compiled *.el files in your Emacs configuration."
  (interactive)
  (emma/compile nil :recompile)
  ;; Forcibly recompile core.el in case `load-path' has changed
  (byte-recompile-file (expand-file-name "core.el" emma-core-dir) t))

(defun doom*++*emma/recompile-packages ()
  "Recompile all installed elpa packages. If you're getting odd errors after
upgrading Emacs, this may fix it."
  (interactive)
  (byte-recompile-directory package-user-dir 0 t))

(defun doom*++*emma/reset ()
  "Clear the local cache completely (in `emma-cache-dir').

This resets Emacs to a blank slate. You must restart Emacs for some components
to feel its effects."
  (interactive)
  (delete-directory emma-cache-dir t)
  (make-directory emma-cache-dir t))

(defun doom*++*emma/clean-compiled-files ()
  "Delete all compiled elc files in your Emacs configuration.

This excludes compiled packages in `emma-packages-dir'."
  (interactive)
  (let ((targets (append (list (expand-file-name "init.elc" emma-emacs-dir))
                         (directory-files-recursively emma-core-dir "\\.elc$")
                         (directory-files-recursively emma-modules-dir "\\.elc$"))))
    (unless (cl-loop for path in targets
                     if (file-exists-p path)
                       collect path
                       and do (delete-file path)
                       and do (message "Deleted %s" (file-relative-name path)))
      (message "Everything is clean"))))


;;
;; Package.el modifications
;;

;; Updates QUELPA after deleting a package
(advice-add #'package-delete :after #'emma*package-delete)

;; It isn't safe to use `package-autoremove', so get rid of it
(advice-add #'package-autoremove :override #'emma/packages-autoremove)

(provide 'core-packages)
;;; core-packages.el ends here
