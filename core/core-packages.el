;;; core-packages.el --- Package management system. -*- lexical-binding: t; -*-

(defvar njord-init-p nil
  "Non-nil if Njord is done initializing.")

(defvar njord-package-init-p nil
  "If non-nil, Njord's package system has initialized.")

(defvar njord-init-time nil
  "Time it took in seconds for Njord to initialize.")

(defvar njord-modules ()
  "A hash table of enabled modules. Set by `njord-initialize-modules'.")

(defvar njord-packages ()
  "A list of enabled packages. Each element is a sublist, whose CAR is the
package's name as a symbol and whose CDR is the plist supplied to its
`package!' declaration. Set by `njord-initialize-packages'.")

(defvar njord-core-packages
  '(persistent-soft quelpa use-package)
  "A list of packages that must be installed.")

(defvar njord-disabled-packages ()
  "A list of packages that should be ignored by `def-package!'.")

(defvar njord-reload-hook nil
  "A list of hooks to run when `njord/reload' is called.")

(defvar njord--site-load-path load-path
  "The load path of built in Emacs libraries.")

(defvar njord--package-load-path ()
  "The load path of package libraries installed via ELPA or QUELPA.")

(defvar njord--base-load-path
  (append (list njord-core-dir njord-modules-dir)
          njord--site-load-path)
  "A backup of `load-path' before altered by `njord-initialize'. Used as a base
by `njord!' and for calculating how many packages exist.")

(defvar njord--module nil)
(defvar njord--refresh-p nil)

(setq load-prefer-newer (or noninteractive njord-debug-mode)
      package--init-file-ensured t
      package-user-dir (expand-file-name "elpa" njord-packages-dir)
      package-enable-at-startup nil
      package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/"))
      gnutls-verify-error (not (getenv "INSECURE"))
      tls-checktrust gnutls-verify-error
      tls-program (list "gnutls-cli --x509cafile %t -p %p %h"
                        "gnutls-cli -p %p %h"
                        "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof")
      use-package-always-defer t
      use-package-always-ensure nil
      use-package-debug nil
      use-package-verbose njord-debug-mode
      use-package-minimum-reported-time (if njord-debug-mode 0 0.1)
      quelpa-checkout-melpa-p nil
      quelpa-update-melpa-p nil
      quelpa-melpa-recipe-stores nil
      quelpa-self-upgrade-p nil
      quelpa-verbose njord-debug-mode
      quelpa-dir (expand-file-name "quelpa" njord-packages-dir)

      byte-compile-dynamic nil
      byte-compile-verbose njord-debug-mode
      byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

(defun njord-initialize (&optional force-p)
  "Initialize installed packages and ensure the core packages are installed. If
you byte-compile core/core.el, this function will be avoided to speed up
startup."
  (when (or (not njord-package-init-p) force-p)
    (unless noninteractive
      (message "Njord initialized."))

    (setq load-path njord--base-load-path
          package-activated-list nil)

    (dolist (dir (list njord-local-dir
                       njord-etc-dir
                       njord-cache-dir
                       package-user-dir))
      (unless (file-directory-p dir)
        (make-directory dir t)))

    (package-initialize t)

    (setq njord--package-load-path (directory-files package-user-dir t
                                                    "^[^.]" t)
          load-path (append load-path njord--package-load-path))

    ;; Ensure core packages are installed.
    (dolist (pkg njord-core-packages)
      (unless (package-installed-p pkg)
        (unless njord--refresh-p
          (package-refresh-contents)
          (setq njord--refresh-p t))
        (let ((inhibit-message t))
          (package-install pkg))
        (if (package-installed-p pkg)
            (message "Installed %s" pkg)
          (error "Could NOT install %s" pkg))))
    (load "quelpa" nil t)
    (load "use-package" nil t)

    (setq njord-package-init-p t)))

(defun njord-initialize-autoloads ()
  "Ensures that `njord-autoload-file' exists and is loaded. Otherwise run
`njord/reload-autoloads' to generate it."
  (unless (file-exists-p njord-autoload-file)
    (quiet! (njord/reload-autoloads))))

(defun njord-initialize-packages (&optional force-p load-p)
  "Crawls across emacs.d/ to fill `njord-modules' and `njord-packages'.

If FORCE-P is non-nil, do it even if they are already filled."
  (njord-initialize force-p)
  (let ((noninteractive t)
        (load-fn
         (lambda (file &optional noerror)
           (condition-case-unless-debug ex
               (load file noerror :nomessage :nosuffix)
             ('error
              (error (format "(njord-initialize-packages) %s in %s: %s"
                             (car ex)
                             (file-relative-name file njord-emacs-dir)
                             (error-message-string ex))
                     :error))))))
    (when (or force-p (not njord-modules))
      (setq njord-modules nil)
      (funcall load-fn (expand-file-name "init.el" njord-emacs-dir))
      (when load-p
        (let (noninteractive)
          (funcall load-fn (njord-module-path :private user-login-name "init.el")
                   t)
          (funcall load-fn (expand-file-name "core.el" njord-core-dir)))
        (mapc load-fn (file-expand-wildcards
                       (expand-file-name "autoload/*.el"
                                         njord-core-dir))))
      (njord|finalize))
    (when (or force-p (not njord-packages))
      (setq njord-packages nil)
      (funcall load-fn (expand-file-name "packages.el" njord-core-dir))
      (cl-loop for (module . submodule) in (njord--module-pairs)
               for path = (njord-module-path module submodule "packages.el")
               do
               (let ((njord--module (cons module submodule)))
                 (funcall load-fn path t))))))

(defun njord-initialize-modules (modules)
  "Adds MODULES to `njord-modules'. MODULES must be in mplist format."
  (unless njord-modules
    (setq njord-modules (make-hash-table :test #'equal
                                         :size (+ 5 (length modules)))))
  (let (mode)
    (dolist (m modules)
      (cond ((keywordp m)
             (setq mode m))
            ((not mode)
             (error "No namespace specified on `njord!' for %s" m))
            ((listp m)
             (njord-module-enable mode (car m) (cdr m)))
            (t (njord-module-enable mode m))))))

(defun njord-module-path (module submodule &optional file)
  "Get the full path to a module."
  (when (keywordp module)
    (setq module (substring (symbol-name module) 1)))
  (when (symbolp submodule)
    (setq submodule (symbol-name submodule)))
  (expand-file-name (concat module "/" submodule "/" file)
                    njord-modules-dir))

(defun njord-module-flags (module submodule)
  "Returns a list of flags provided by MODULE SUBMODULE."
  (and (hash-table-p njord-modules)
       (gethash (cons module submodule) njord-modules)))

(defun njord-module-loaded-p (module submodule)
  "Returns t if MODULE->SUBMODULE is present in `njord-modules'."
  (and (njord-module-flags module submodule) t))

(defun njord-module-enable (module submodule &optional flags)
  "Adds MODULE and SUBMODULE to `njord-modules', overwriting it if it exists.
MODULE is a keyword, SUBMODULE is a symbol."
  (puthash (cons module submodule)
           (njord-enlist (or flags t))
           njord-modules))

(defun njord--module-pairs ()
  "Returns `njord-modules' as a list of (MODULE . SUBMODULE) cons cells."
  (unless (hash-table-p njord-modules)
    (error "njord-modules is uninitialized"))
  (cl-loop for key being the hash-keys of njord-modules
           collect key))

(defun njord--module-paths (&optional append-file)
  "Returns a list of absolute file paths to activated modules, with APPEND-FILE
added if the file exists."
  (cl-loop for (module . submodule) in (njord--module-pairs)
           for path = (njord-module-path module submodule append-file)
           if (file-exists-p path)
           collect path))

(defun njord--display-benchmark ()
  (message "Loaded %s packages in %.03fs"
           (- (length load-path) (length njord--base-load-path))
           (setq njord-init-time (float-time
                                  (time-subtract after-init-time
                                                 before-init-time)))))

(autoload 'use-package "use-package" nil nil 'macro)

(defmacro njord! (&rest modules)
  "Bootstrap Njord Emacs.

MODULES is a malformed plist of modules to load."
  (njord-initialize-modules modules)
  (when (and user-login-name
             (not (njord-module-loaded-p :private (intern user-login-name))))
    (njord-module-enable :private user-login-name))
  `(let (file-name-handler-alist)
     (setq njord-modules `,njord-modules)
     (unless noninteractive
       (load ,(njord-module-path :private user-login-name "init") t t)
       ,@ (cl-loop for (module . submodule) in (njord--module-pairs)
                   collect `(require! ,module ,submodule nil t))

          (when (display-graphic-p)
            (require 'server)
            (unless (server-running-p)
              (server-start)))
          (add-hook 'njord-init-hook #'njord--display-benchmark t))))

(defmacro def-package! (name &rest plist)
  "A thin wrapper around `use-package'."
  (when (and (memq njord-disabled-packages)
             (not (memq :disabled plist)))
    (setq plist (append (list :disabled t) plist)))
  `(use-package ,name ,@plist))

(defmacro def-package-hook! (package when &rest body)
  "Reconfigures a package's `def-package!' block.

PACKAGE is the package's name.
WHEN should be :pre-init, :post-init, :pre-config, :post-config or :disable."
  (declare (indent defun))
  (cond ((eq when :disable)
         (push package njord-disabled-packages)
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
         (error "'%s' is not a valid hook for def-package-hook!" when))))

(defmacro load! (filesym &optional path noerror)
  "Load a file relative to the current executing file (`load-file-name').

FILESYM is either a symbol or string representing the file to load. PATH is
where to look for the file.

If NOERROR is non-nil, do not throw an error if the file does not exists."
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
                        (t (error
                            "load! expected a string or symbol, got %s (a %s)"
                            filesym (type-of filesym))))))
    (unless path
      (error "Could not find %s" filename))
    (let ((file (expand-file-name (concat filename ".el") path)))
      (if (file-exists-p file)
          `(load ,(file-name-sans-extension file) ,noerror
                 ,(not njord-debug-mode))
        (unless noerror
          (error "Could not load! file %s" file))))))

(defmacro require! (module submodule &optional flags reload-p)
  "Loads the module specified by MODULE and SUBMODULE."
  (let ((loaded-p (njord-module-loaded-p module submodule)))
    (when (or reload-p (not loaded-p))
      (unless loaded-p
        (njord-module-enable module submodule flags))
      `(condition-case-unless-debug ex
           (let ((njord--module ',(cons module submodule)))
             (load! config ,(njord-module-path module submodule) t))
         ('error
          (lwarn 'njord-modules :error
                 "%s in '%s %s' -> %s"
                 (car ex) ,module ',submodule
                 (error-message-string ex)))))))

(defmacro featurep! (module &optional submodule flag)
  "A convenience macro wrapper for `njord-module-loaded-p'."
  (unless submodule
    (unless njord--module
      (error "featurep! was used incorrectly"))
    (setq flag module
          module (car njord--module)
          submodule (cdr njord--module)))
  (if flag
      (and (memq flag (njord-module-flags module submodule)) t)
    (njord-module-loaded-p module submodule)))

(defmacro package! (name &rest plist)
  "Declares a package and how to install it.

Accepts the following properties:

  :recipe RECIPE    Takes a melpa-style recipe.
  :pin ARCHIVE-NAME Instructs elpa to only look for this spackage in
                    ARCHIVE-NAME.
  :ignore FORM      Do not install this package if FORM is non-nil.
  :freeze FORM      Do not update this package if FORM is non-nil."
  (declare (indent defun))
  (let* ((old-plist (assq name njord-packages))
         (pkg-recipe (or (plist-get plist :recipe)
                         (and old-plist (plist-get old-plist :recipe))))
         (pkg-pin (or (plist-get plist :pin)
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
         (assq-delete-all ',name njord-packages))
       (push ',(cons name plist) njord-packages))))

(defmacro depends-on! (module submodule)
  "Declares that this module depends on another."
  (njord-module-enable module submodule)
  `(load! packages ,(njord-module-path module submodule) t))

(defun njord/reload ()
  "Reload `load-path' and recompile files."
  (interactive)
  (cond (noninteractive
         (message "Reloading...")
         (require 'server)
         (unless (ignore-errors (server-eval-at "server" '(njord/reload)))
           (message "Recompiling")
           (njord/recompile)))
        (t
         (njord-initialize t)
         (njord/recompile)
         (message "Reloaded %d packages" (length njord--package-load-path))
         (run-with-timer 1 nil #'redraw-display)
         (run-hooks 'njord-reload-hook))))

(defun njord/reload-autoloads ()
  "Refreshes the autoloads.el file."
  (interactive)
  (njord-initialize-packages (not noninteractive))
  (let ((evil-p (njord-module-loaded-p :feature 'evil))
        (targets
         (file-expand-wildcards
          (expand-file-name "autoload/*.el" njord-core-dir))))
    (dolist (path (njord--module-paths))
      (let ((auto-dir (expand-file-name "autoload" path))
            (auto-file (expand-file-name "autoload.el" path)))
        (when (file-exists-p auto-file)
          (push auto-file targets))
        (when (file-directory-p auto-dir)
          (dolist (file (file-expand-wildcards
                         (expand-file-name "*.el" auto-dir) t))
            (unless (and (string-prefix-p "evil" (file-name-nondirectory file))
                         (not evil-p))
              (push file targets))))))
    (when (file-exists-p njord-autoload-file)
      (delete-file njord-autoload-file)
      (message "Deleted old autoloads.el"))
    (dolist (file (reverse targets))
      (message (if (update-file-autoloads file nil njord-autoload-file)
                   "Nothing in %s"
                 "Scanned %s")
               (file-relative-name file njord-emacs-dir)))
    (let ((buf (get-file-buffer njord-autoload-file)) current-sexp)
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
             (delete-file njord-autoload-file)
             (error "Error in autoloads.el: (%s %s ...) %s -- %s"
                    (nth 0 current-sexp)
                    (nth 1 current-sexp)
                    (car ex) (error-message-string ex))))
        (kill-buffer buf)))))

(defun njord/compile (&optional lite-p only-recompile-p)
  "Byte compile Emacs configuration."
  (interactive "P")
  (njord-initialize-packages t noninteractive)
  (let ((targets
         (cond ((equal (car command-line-args-left) "--")
                (cl-loop for file in (cdr command-line-args-left)
                         if (file-exists-p file)
                         collect (expand-file-name file)
                         finally do (setq command-line-args-left nil)))
               (t
                (append (list (expand-file-name "init.el" njord-emacs-dir)
                              njord-core-dir)
                        (unless lite-p (njord--module-paths))))))
        (total-success 0)
        (total-fail 0)
        (total-nocomp 0)
        (use-package-expand-minimally t))
    (let ((el-files (cl-loop for path in targets
                             if (file-directory-p path)
                             nconc (nreverse (directory-files-recursively
                                              path "\\.el$"))
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
                (short-name (file-relative-name file njord-emacs-dir)))
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

(defun njord/recompile ()
  "Recompile any outdated compiled .el file."
  (interactive)
  (njord/compile nil :recompile)
  (byte-recompile-file (expand-file-name "core.el" njord-core-dir) t))

(defun njord/recompile-packages ()
  "Recompile all installed elpa packages."
  (interactive)
  (byte-recompile-directory package-user-dir 0 t))

(defun njord/reset ()
  "Clear the local cache completely."
  (interactive)
  (delete-directory njord-cache-dir t)
  (make-directory njord-cache-dir t))

(defun njord/clean-compiled-files ()
  "Delete all compiled .elc files."
  (interactive)
  (let ((targets (append (list (expand-file-name "init.elc" njord-emacs-dir))
                         (directory-files-recursively njord-core-dir "\\.elc$")
                         (directory-files-recursively njord-modules-dir
                                                      "\\.elc$"))))
    (unless (cl-loop for path in targets
                     if (file-exists-p path)
                     collect path
                     and do (delete-file path)
                     and do (message "Deleted %s" (file-relative-name path)))
      (message "Everything is clean"))))

(advice-add #'package-delete :after #'njord*package-delete)

(advice-add #'package-autoremove :override #'njord/packages-autoremove)

(provide 'core-packages)
