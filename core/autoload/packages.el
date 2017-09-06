;;; core/autoload/packages.el -*- lexical-binding: t; -*-

(defvar njord--last-refresh nil)

;;;###autoload
(defun njord-refresh-packages (&optional force-p)
  "Refresh ELPA packages."
  (njord-initialize)
  (when force-p
    (njord-refresh-clear-cache))
  (unless (or (persistent-soft-fetch 'last-pkg-refresh "emacs")
              njord--refresh-p)
    (condition-case-unless-debug ex
        (progn
          (message "Refreshing package archives")
          (package-refresh-contents)
          (persistent-soft-store 'last-pkg-refresh t "emacs" 900))
    ('error
     (njord-refresh-clear-cache)
     (message "Failed to refresh packages: (%s) %s"
              (car ex) (error-message-string ex))))))

;;;###autoload
(defun njord-refresh-clear-cache ()
  "Clear the cache for `njord-refresh-packages'."
  (setq njord--refresh-p nil)
  (persistent-soft-store 'last-pkg-refresh nil "emacs"))

;;;###autoload
(defun njord-package-backend (name)
  "Get which backend the package NAME was installed with. Can either be elpa,
quelpa or nil (if not installed)."
  (cl-assert (symbolp name) t)
  (njord-initialize-packages)
  (cond ((let ((plist (cdr (assq name njord-packages))))
           (and (not (plist-get plist :pin))
                (or (quelpa-setup-p)
                    (error "Could not initialize quelpa"))
                (or (assq name quelpa-cache)
                    (plist-get plist :recipe))))
         'quelpa)
        ((assq name package-alist)
         'elpa)
        (t
         (error "%s package not installed" name))))

;;;###autoload
(defun njord-package-outdated-p (name)
  "Determine whether NAME (a symbol) is outdated or not. If outdated, returns a
list, whose car is NAME, and cdr the current version list and latest version
list of the package."
  (cl-assert (symbolp name) t)
  (njord-initialize-packages)
  (when-let (desc (cadr (assq name package-alist)))
    (let* ((old-version (package-desc-version desc))
           (new-version
            (pcase (njord-package-backend name)
              ('quelpa
               (let ((recipe (plist-get (cdr (assq name njord-packages)) :recipe))
                     (dir (expand-file-name (symbol-name name) quelpa-build-dir))
                     (inhibit-message (not njord-debug-mode))
                     (quelpa-upgrade-p t))
                 (if-let (ver (quelpa-checkout recipe dir))
                     (version-to-list ver)
                   old-version)))
              ('elpa
               (let ((desc (cadr (assq name package-archive-contents))))
                 (when (package-desc-p desc)
                   (package-desc-version desc)))))))
      (when (and (listp old-version) (listp new-version)
                 (version-list-< old-version new-version))
        (list name old-version new-version)))))

;;;###autoload
(defun njord-package-prop (name prop)
  "Return PROPerty in NAME's plist."
  (cl-assert (symbolp name) t)
  (cl-assert (keywordp prop) t)
  (njord-initialize-packages)
  (plist-get (cdr (assq name njord-packages)) prop))

;;;###autoload
(defun njord-get-packages (&optional installed-only-p)
  "Retrieves a list of explicitly installed packages (i.e. non-dependencies).
Each element is a cons cell, whose car is the package symbol and whose cdr is
the quelpa recipe (if any).
BACKEND can be 'quelpa or 'elpa, and will instruct this function to return only
the packages relevant to that backend.
Warning: this function is expensive; it re-evaluates all of njord's config files.
Be careful not to use it in a loop.
If INSTALLED-ONLY-P, only return packages that are installed."
  (njord-initialize-packages t)
  (cl-loop with packages = (append njord-core-packages (mapcar #'car njord-packages))
           for sym in (cl-delete-duplicates packages)
           if (and (or (not installed-only-p)
                       (package-installed-p sym))
                   (or (assq sym njord-packages)
                       (and (assq sym package-alist)
                            (list sym))))
           collect it))

;;;###autoload
(defun njord-get-depending-on (name)
  "Return a list of packages that depend on the package named NAME."
  (njord-initialize)
  (when-let (desc (cadr (assq name package-alist)))
    (mapcar #'package-desc-name (package--used-elsewhere-p desc nil t))))

;;;###autoload
(defun njord-get-dependencies-for (name &optional only)
  "Return a list of dependencies for a package."
  (njord-initialize)
  (package--get-deps name only))

;;;###autoload
(defun njord-get-outdated-packages (&optional include-frozen-p)
  "Return a list of packages that are out of date. Each element is a list,
containing (PACKAGE-SYMBOL OLD-VERSION-LIST NEW-VERSION-LIST).
If INCLUDE-FROZEN-P is non-nil, check frozen packages as well.
Used by `njord/packages-update'."
  (let (quelpa-pkgs elpa-pkgs)
    ;; Separate quelpa from elpa packages
    (dolist (pkg (njord-get-packages t))
      (let ((sym (car pkg)))
        (when (and (or (not (njord-package-prop sym :freeze))
                       include-frozen-p)
                   (not (njord-package-prop sym :ignore)))
          (push sym
                (if (eq (njord-package-backend sym) 'quelpa)
                    quelpa-pkgs
                  elpa-pkgs)))))
    ;; The bottleneck in this process is quelpa's version checks, so partition
    ;; and check them asynchronously.
    (let* ((max-threads 3) ; TODO Do real CPU core/thread count
           (min-per-part 2)
           (per-part (max min-per-part (ceiling (/ (length quelpa-pkgs) (float max-threads)))))
           (leftover (mod (length quelpa-pkgs) per-part))
           parts
           futures)
      (while quelpa-pkgs
        (let (part)
          (dotimes (_i (+ per-part leftover))
            (when-let (p (pop quelpa-pkgs))
              (push p part)))
          (setq leftover 0)
          (push (nreverse part) parts)))
      (dolist (part (reverse parts))
        (debug! "New thread for: %s" part)
        (push (async-start
               `(lambda ()
                  (setq user-emacs-directory ,user-emacs-directory)
                  (let ((noninteractive t))
                    (load ,(expand-file-name "core.el" njord-core-dir)))
                  (delq nil (mapcar #'njord-package-outdated-p ',part))))
              futures))
      (apply #'append
             (delq nil (mapcar #'njord-package-outdated-p elpa-pkgs))
             (mapcar #'async-get futures)))))

;;;###autoload
(defun njord-get-orphaned-packages ()
  "Return a list of symbols representing packages that are no longer needed or
depended on.
Used by `njord/packages-autoremove'."
  (njord-initialize-packages t)
  (let ((package-selected-packages
         (append (mapcar #'car njord-packages) njord-core-packages)))
    (package--removable-packages)))

;;;###autoload
(defun njord-get-missing-packages (&optional include-ignored-p)
  "Return a list of requested packages that aren't installed or built-in, but
are enabled (with a `package!' directive). Each element is a list whose CAR is
the package symbol, and whose CDR is a plist taken from that package's
`package!' declaration.
If INCLUDE-IGNORED-P is non-nil, includes missing packages that are ignored,
i.e. they have an :ignore property.
Used by `njord/packages-install'."
  (cl-loop for pkgsym in (njord-get-packages)
           unless
           (let ((pkg (car pkgsym)))
             (or (assq pkg package-alist)
                 (unless include-ignored-p (njord-package-prop pkg :ignore))
                 (and (not (plist-get (assq pkg njord-packages) :pin))
                      (assq pkg package--builtins))))
           collect pkgsym))

;;;###autoload
(defun njord*package-delete (desc &rest _)
  "Update `quelpa-cache' upon a successful `package-delete'."
  (let ((name (package-desc-name desc)))
    (when (and (not (package-installed-p name))
               (quelpa-setup-p)
               (assq name quelpa-cache))
      (setq quelpa-cache (assq-delete-all name quelpa-cache))
      (quelpa-save-cache)
      (let ((path (expand-file-name (symbol-name name) quelpa-build-dir)))
        (when (file-exists-p path)
          (delete-directory path t))))))

;;; Private functions
(defsubst njord--sort-alpha (it other)
  (string-lessp (symbol-name (car it))
                (symbol-name (car other))))

(defun njord--packages-choose (prompt)
  (njord-initialize)
  (let ((table (cl-loop for pkg in package-alist
                        unless (package-built-in-p (cdr pkg))
                        collect (cons (package-desc-full-name (cdr pkg))
                                      (cdr pkg)))))
    (cdr (assoc (completing-read prompt
                                 (mapcar #'car table)
                                 nil t)
                table))))

(defmacro njord--condition-case! (&rest body)
  `(condition-case-unless-debug ex
       (condition-case ex2
           (progn ,@body)
         ('file-error
          (message! (bold (red "  FILE ERROR: %s" (error-message-string ex2))))
          (message! "  Trying again...")
          (quiet! (njord-refresh-packages t))
          ,@body))
     ('user-error
      (message! (bold (red "  ERROR: (%s) %s"
                           (car ex)
                           (error-message-string ex)))))
     ('error
      (njord-refresh-clear-cache)
      (message! (bold (red "  FATAL ERROR: (%s) %s"
                           (car ex)
                           (error-message-string ex)))))))


;;
;; Main functions
;;

(defun njord-install-package (name &optional plist)
  "Installs package NAME with optional quelpa RECIPE (see `quelpa-recipe' for an
example; the package name can be omitted)."
  (njord-initialize-packages)
  (when (package-installed-p name)
    (user-error "%s is already installed" name))
  (let ((plist (or plist (cdr (assq name njord-packages))))
        (inhibit-message (not njord-debug-mode))
        (recipe (plist-get plist :recipe))
        quelpa-upgrade-p)
    (cond (recipe (quelpa recipe))
          (t (package-install name)))
    (when (package-installed-p name)
      (cl-pushnew (cons name plist) njord-packages :test #'eq :key #'car)
      t)))

(defun njord-update-package (name &optional force-p)
  "Updates package NAME (a symbol) if it is out of date, using quelpa or
package.el as appropriate."
  (njord-initialize)
  (unless (package-installed-p name)
    (user-error "%s isn't installed" name))
  (when (or force-p (njord-package-outdated-p name))
    (let ((inhibit-message (not njord-debug-mode))
          (desc (cadr (assq name package-alist))))
      (pcase (njord-package-backend name)
        ('quelpa
         (or (quelpa-setup-p)
             (error "Failed to initialize quelpa"))
         (let ((quelpa-upgrade-p t))
           (quelpa (assq name quelpa-cache))))
        ('elpa
         (let* ((archive (cadr (assq name package-archive-contents)))
                (packages
                 (if (package-desc-p archive)
                     (package-compute-transaction (list archive) (package-desc-reqs archive))
                   (package-compute-transaction () (list (list archive))))))
           (package-download-transaction packages))))
      (unless (njord-package-outdated-p name)
        (when-let (old-dir (package-desc-dir desc))
          (when (file-directory-p old-dir)
            (delete-directory old-dir t)))
        t))))

(defun njord-delete-package (name &optional force-p)
  "Uninstalls package NAME if it exists, and clears it from `quelpa-cache'."
  (njord-initialize)
  (unless (package-installed-p name)
    (user-error "%s isn't installed" name))
  (let ((inhibit-message (not njord-debug-mode))
        quelpa-p)
    (unless (quelpa-setup-p)
      (error "Could not initialize QUELPA"))
    (when (assq name quelpa-cache)
      (setq quelpa-cache (assq-delete-all name quelpa-cache))
      (quelpa-save-cache)
      (setq quelpa-p t))
    (package-delete (cadr (assq name package-alist)) force-p)
    (unless (package-installed-p name)
      (let ((pkg-build-dir (expand-file-name (symbol-name name) quelpa-build-dir)))
        (when (and quelpa-p (file-directory-p pkg-build-dir))
          (delete-directory pkg-build-dir t)))
      t)))


;;
;; Interactive commands
;;

;;;###autoload
(defun njord/packages-install ()
  "Interactive command for installing missing packages."
  (interactive)
  (let ((packages (njord-get-missing-packages)))
    (cond ((not packages)
           (message! (green "No packages to install!")))

          ((not (or (getenv "YES")
                    (y-or-n-p
                     (format "%s packages will be installed:\n\n%s\n\nProceed?"
                             (length packages)
                             (mapconcat (lambda (pkg)
                                          (format "+ %s (%s)"
                                                  (car pkg)
                                                  (if (plist-get (cdr pkg) :recipe)
                                                      "QUELPA"
                                                    "ELPA")))
                                        (sort (cl-copy-list packages) #'njord--sort-alpha)
                                        "\n")))))
           (message! (yellow "Aborted!")))

          (t
           (njord-refresh-packages njord-debug-mode)
           (dolist (pkg packages)
             (message! "Installing %s" (car pkg))
             (njord--condition-case!
              (message! "  %s%s"
                        (cond ((package-installed-p (car pkg))
                               (dark (white "ALREADY INSTALLED")))
                              ((njord-install-package (car pkg) (cdr pkg))
                               (green "DONE"))
                              (t
                               (red "FAILED")))
                        (if (plist-member (cdr pkg) :pin)
                            (format " [pinned: %s]" (plist-get (cdr pkg) :pin))
                          "")))))

          (message! (bold (green "Finished!")))
          (njord/reload))))

;;;###autoload
(defun njord/packages-update ()
  "Interactive command for updating packages."
  (interactive)
  (njord-refresh-packages njord-debug-mode)
  (message! "Looking for outdated packages...")
  (let ((packages (sort (njord-get-outdated-packages) #'njord--sort-alpha)))
    (cond ((not packages)
           (message! (green "Everything is up-to-date")))

          ((not (or (getenv "YES")
                    (y-or-n-p
                     (format "%s packages will be updated:\n\n%s\n\nProceed?"
                             (length packages)
                             (let ((max-len
                                    (or (car (sort (mapcar (lambda (it) (length (symbol-name (car it)))) packages)
                                                   (lambda (it other) (> it other))))
                                        10)))
                               (mapconcat
                                (lambda (pkg)
                                  (format (format "+ %%-%ds %%-%ds -> %%s" (+ max-len 2) 14)
                                          (symbol-name (car pkg))
                                          (package-version-join (cadr pkg))
                                          (package-version-join (cl-caddr pkg))))
                                packages
                                "\n"))))))
           (message! (yellow "Aborted!")))

          (t
           (dolist (pkg packages)
             (message! "Updating %s" (car pkg))
             (njord--condition-case!
              (message!
               (let ((result (njord-update-package (car pkg) t)))
                 (color (if result 'green 'red)
                        "  %s"
                        (if result "DONE" "FAILED"))))))

           (message! (bold (green "Finished!")))
           (njord/reload)))))

;;;###autoload
(defun njord/packages-autoremove ()
  "Interactive command for auto-removing orphaned packages."
  (interactive)
  (let ((packages (njord-get-orphaned-packages)))
    (cond ((not packages)
           (message! (green "No unused packages to remove")))

          ((not (or (getenv "YES")
                    (y-or-n-p
                     (format "%s packages will be deleted:\n\n%s\n\nProceed?"
                             (length packages)
                             (mapconcat (lambda (sym) (format "+ %s (%s)" sym
                                                         (pcase (njord-package-backend sym)
                                                           ('quelpa "QUELPA")
                                                           ('elpa   "ELPA"))))
                                        (sort (cl-copy-list packages) #'string-lessp)
                                        "\n")))))
           (message! (yellow "Aborted!")))

          (t
           (dolist (pkg packages)
             (njord--condition-case!
              (message!
               (let ((result (njord-delete-package pkg t)))
                 (color (if result 'green 'red)
                        "%s %s"
                        (if result "Removed" "Failed to remove")
                        pkg)))))

           (message! (bold (green "Finished!")))
           (njord/reload)))))

;;;###autoload
(defalias 'njord/install-package #'package-install)

;;;###autoload
(defun njord/reinstall-package (desc)
  "Reinstalls package package with optional quelpa RECIPE (see `quelpa-recipe' for
an example; the package package can be omitted)."
  (declare (interactive-only t))
  (interactive
   (list (njord--packages-choose "Reinstall package: ")))
  (let ((package (package-desc-name desc)))
    (njord-delete-package package t)
    (njord-install-package package (cdr (assq package njord-packages)))))

;;;###autoload
(defun njord/delete-package (desc)
  "Prompts the user with a list of packages and deletes the selected package.
Use this interactively. Use `njord-delete-package' for direct calls."
  (declare (interactive-only t))
  (interactive
   (list (njord--packages-choose "Delete package: ")))
  (let ((package (package-desc-name desc)))
    (if (package-installed-p package)
        (if (y-or-n-p (format "%s will be deleted. Confirm?" package))
            (message "%s %s"
                     (if (njord-delete-package package t) "Deleted" "Failed to delete")
                     package)
          (message "Aborted"))
      (message "%s isn't installed" package))))

;;;###autoload
(defun njord/update-package (pkg)
  "Prompts the user with a list of outdated packages and updates the selected
package. Use this interactively. Use `njord-update-package' for direct
calls."
  (declare (interactive-only t))
  (interactive
   (let* ((packages (njord-get-outdated-packages))
          (package (if packages
                       (completing-read "Update package: "
                                        (mapcar #'car packages)
                                        nil t)
                     (user-error "All packages are up to date"))))
     (list (cdr (assq (car (assoc package package-alist)) packages)))))
  (cl-destructuring-bind (package old-version new-version) pkg
    (if-let (desc (njord-package-outdated-p package))
        (let ((old-v-str (package-version-join old-version))
              (new-v-str (package-version-join new-version)))
          (if (y-or-n-p (format "%s will be updated from %s to %s. Update?"
                                package old-v-str new-v-str))
              (message "%s %s (%s => %s)"
                       (if (njord-update-package package t) "Updated" "Failed to update")
                       package old-v-str new-v-str)
            (message "Aborted")))
      (message "%s is up-to-date" package))))

;;;###autoload
(defun njord/refresh-packages (&optional force-p)
  "Synchronize package metadata with the sources in `package-archives'. If
FORCE-P (the universal argument) is set, ignore the cache."
  (declare (interactive-only t))
  (interactive "P")
  (njord-refresh-packages force-p))
