;; -*- lexical-binding: t -*-
;;
;; Copyright (C) 2022 Paul Onions
;;
;; Author: Paul Onions <paul.onions@acm.org>
;; Keywords: FriCAS, computer algebra, extensions, tools
;; URL: https://github.com/pdo/frimacs
;;
;; SPDX-License-Identifier: MIT
;;
;; This file is free software, see the LICENCE file in this directory
;; for more information.
;;
;; Some useful functions to help build and maintain the frimacs
;; package.
;;
;; This file is intentionally not loaded as part of the frimacs
;; package.  Instead it is suggested to use
;; `emacs-lisp-byte-compile-and-load' on this file to access these
;; functions.  This will require that frimacs has been loaded first.
;; If this is not the case and you do not want to install the frimacs
;; package (or cannot because it doesn't exist yet), use:-
;;
;;   (load-file "/path/to/frimacs.el")
;;
;; first from an IELM buffer.
;;
(require 'subr-x)
(require 'package)
(require 'package-x)

(require 'frimacs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data file creation routines

(defvar frimacs-build-source-dir
  (file-name-directory (or load-file-name (buffer-file-name)))
  "The frimacs project source code directory.")

(defvar frimacs-build-data-dir
  (concat frimacs-build-source-dir "data/")
  "The frimacs project data directory.")

(defvar frimacs-build-query-buffer-name "*frimacs-build-query*"
  "Name of buffer in which to process FriCAS query results.")

(defun frimacs-get-constructor-names-list (type)
  "Query the FriCAS process and return a list of constructor names.

TYPE should be either :package, :domain or :category."
  (with-current-buffer (get-buffer-create frimacs-build-query-buffer-name)
    (erase-buffer)
    (frimacs-process-redirect-send-command
     (cond ((eql type :package)
            "((getDatabase$OperationsQuery) \"k\").('kind=\"p\").'name")
           ((eql type :domain)
            "((getDatabase$OperationsQuery) \"k\").('kind=\"d\").'name")
           ((eql type :category)
            "((getDatabase$OperationsQuery) \"k\").('kind=\"c\").'name"))
     (current-buffer))
    (goto-char (point-min))
    (let ((names nil))
      (while (re-search-forward "\"\\([^\"]+\\)\"" nil t)
        (setq names (cons (match-string 1) names)))
      (reverse names))))

(defun frimacs-get-operation-names-list ()
  "Query the FriCAS process and return a list of operation names."
  (with-current-buffer (get-buffer-create frimacs-build-query-buffer-name)
    (erase-buffer)
    (frimacs-process-redirect-send-command
     "((getDatabase$OperationsQuery) \"o\").'name"
     (current-buffer))
    (goto-char (point-min))
    (let ((names nil))
      (while (re-search-forward "\"\\([^\"]+\\)\"" nil t)
        (let ((name (match-string 1)))
          (unless (member name names)
            (setq names (cons name names)))))
      (reverse names))))

(defun frimacs-get-abbreviation (constructor-name)
  "Return the abbreviation for the given constructor name."
  (with-current-buffer (get-buffer-create frimacs-build-query-buffer-name)
    (erase-buffer)
    (frimacs-process-redirect-send-command
     (format ")abbrev query %s" constructor-name)
     (current-buffer))
    (goto-char (point-min))
    (when (re-search-forward "\\([[:word:]]+\\)[[:space:]]+abbreviates[[:space:]]+\\(package\\|domain\\|category\\)[[:space:]]+\\([[:word:]]+\\)" nil t)
      (match-string 1))))

(defun frimacs-make-abbreviations-alist (names-list)
  "Return a list of (abbrev . name) pairs."
  (mapcar (lambda (name)
            (cons (frimacs-get-abbreviation name) name))
          names-list))

(defun frimacs-make-standard-package-info-file ()
  (frimacs-write-data-file (frimacs-make-abbreviations-alist
                            (frimacs-get-constructor-names-list :package))
                           (concat frimacs-build-data-dir
                                   frimacs-standard-package-info-file)))

(defun frimacs-make-standard-domain-info-file ()
  (frimacs-write-data-file (frimacs-make-abbreviations-alist
                            (frimacs-get-constructor-names-list :domain))
                           (concat frimacs-build-data-dir
                                   frimacs-standard-domain-info-file)))

(defun frimacs-make-standard-category-info-file ()
  (frimacs-write-data-file (frimacs-make-abbreviations-alist
                            (frimacs-get-constructor-names-list :category))
                           (concat frimacs-build-data-dir
                                   frimacs-standard-category-info-file)))

(defun frimacs-make-standard-operation-info-file ()
  (frimacs-write-data-file (frimacs-get-operation-names-list)
                           (concat frimacs-build-data-dir
                                   frimacs-standard-operation-info-file)))

(defun frimacs-make-standard-info-files ()
  (interactive)
  (frimacs-make-standard-package-info-file)
  (frimacs-make-standard-domain-info-file)
  (frimacs-make-standard-category-info-file)
  (frimacs-make-standard-operation-info-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Emacs package creation routines

(defun frimacs-build-gen-version-string (&optional time)
  (concat (format-time-string "%Y%m%d." (or time (current-time)))
          (string-trim-left (format-time-string "%k%M" (or time (current-time))))))

(defun frimacs-build-parse-version-string (str)
  (mapcar 'string-to-number (split-string str "\\.")))

(defvar frimacs-build-frimacs-filespecs
  '("LICENCE" "*.el" ("data" "data/*.dat")
    (:exclude "frimacs-build-utils.el")))

(defvar frimacs-build-ob-fricas-filespecs
  '("LICENCE" "extras/ob-fricas.el"))

(defvar frimacs-build-company-frimacs-filespecs
  '("LICENCE" "extras/company-frimacs.el"))

(defun frimacs-build-emacs-package-dir (pkg-filespecs src-dir dst-dir)
  (let ((default-directory src-dir))
    (dolist (filespec pkg-filespecs)
      (cond ((stringp filespec)
             (dolist (file (file-expand-wildcards filespec))
               (copy-file file dst-dir t)))
            ((and (listp filespec) (stringp (car filespec)))
             (let ((sub-dir (concat dst-dir (car filespec) "/")))
               (make-directory sub-dir)
               (dolist (spec (cdr filespec))
                 (dolist (file (file-expand-wildcards spec))
                   (copy-file file sub-dir t)))))
            ((and (listp filespec) (eql (car filespec) :exclude))
             (dolist (file (cdr filespec))
               (delete-file (concat dst-dir file))))))))

(defun frimacs-build-emacs-package-tarfile (pkg-name pkg-ver pkg-filespecs src-dir)
  (let* ((tar-name (concat pkg-name "-" pkg-ver))
         (tar-dir  (concat src-dir tar-name "/"))
         (tar-file (concat src-dir tar-name ".tar")))
    (make-directory tar-dir)
    (frimacs-build-emacs-package-dir pkg-filespecs src-dir tar-dir)
    (let* ((default-directory src-dir)
           (command (concat "tar cvf " tar-file " " tar-name)))
      (call-process-shell-command command nil "*Frimacs Build Emacs Package*" t)
      tar-file)))

(defun frimacs-build-emacs-package (src-dir arc-dir pkg-filespecs pkg-name pkg-ver)
  (unless (and src-dir (file-accessible-directory-p src-dir))
    (error "Cannot find source directory: %s" src-dir))
  (unless (and arc-dir (file-accessible-directory-p arc-dir))
    (error "Cannot find archive directory: %s" arc-dir))
  (let ((tarfile (frimacs-build-emacs-package-tarfile pkg-name pkg-ver pkg-filespecs src-dir))
        (package-archive-upload-base arc-dir))
    (package-upload-file tarfile)))

(defun frimacs-build-interactive-args ()
  (list (read-directory-name "Project source directory: " frimacs-build-source-dir)
        (completing-read "Package archive: " (mapcar 'car package-archives))
        (read-string "Version string: " (frimacs-build-gen-version-string))))

(defun frimacs-build-frimacs-package (src-dir archive pkg-ver)
  "Build and upload the frimacs Emacs package.

Specifying project source directory, package archive name and
package version string.  The package archive name should be one
of those specified in the `package-archives' variable."
  (interactive (frimacs-build-interactive-args))
  (let* ((pkg-name "frimacs")
         (pkg-defn `(define-package ,pkg-name ,pkg-ver
                      "An environment for working with the FriCAS computer algebra system."
                      nil))
         (pkg-filespecs (cons (concat pkg-name "-pkg.el")
                              frimacs-build-frimacs-filespecs))
         (default-directory src-dir))
    (write-region (format "%S" pkg-defn) nil (concat pkg-name "-pkg.el"))
    (frimacs-build-emacs-package src-dir (cdr (assoc archive package-archives))
                                 pkg-filespecs pkg-name pkg-ver)))

(defun frimacs-build-ob-fricas-package (src-dir archive pkg-ver)
  "Build and upload the ob-fricas Emacs package.

Specifying project source directory, package archive name and
package version string.  The package archive name should be one
of those specified in the `package-archives' variable."
  (interactive (frimacs-build-interactive-args))
  (let* ((pkg-name "ob-fricas")
         (pkg-defn `(define-package ,pkg-name ,pkg-ver
                      "A FriCAS backend for Org-Babel."
                      ((frimacs ,pkg-ver))))
         (pkg-filespecs (cons (concat pkg-name "-pkg.el")
                              frimacs-build-ob-fricas-filespecs))
         (default-directory src-dir))
    (write-region (format "%S" pkg-defn) nil (concat pkg-name "-pkg.el"))
    (frimacs-build-emacs-package src-dir (cdr (assoc archive package-archives))
                                 pkg-filespecs pkg-name pkg-ver)))

(defun frimacs-build-company-frimacs-package (src-dir archive pkg-ver)
  "Build and upload the company-frimacs Emacs package.

Specifying project source directory, package archive name and
package version string.  The package archive name should be one
of those specified in the `package-archives' variable."
  (interactive (frimacs-build-interactive-args))
  (let* ((pkg-name "company-frimacs")
         (pkg-defn `(define-package ,pkg-name ,pkg-ver
                      "A frimacs backend for company-mode."
                      ((frimacs ,pkg-ver))))
         (pkg-filespecs (cons (concat pkg-name "-pkg.el")
                              frimacs-build-company-frimacs-filespecs))
         (default-directory src-dir))
    (write-region (format "%S" pkg-defn) nil (concat pkg-name "-pkg.el"))
    (frimacs-build-emacs-package src-dir (cdr (assoc archive package-archives))
                                 pkg-filespecs pkg-name pkg-ver)))

(defun frimacs-build-all-emacs-packages (src-dir archive pkg-ver)
  "Build and upload all frimacs project packages.

Specifying project source directory, package archive name and
package version string.  The package archive name should be one
of those specified in the `package-archives' variable.

All packages will have the same version number."
  (interactive (frimacs-build-interactive-args))
  (frimacs-build-frimacs-package src-dir archive pkg-ver)
  (frimacs-build-ob-fricas-package src-dir archive pkg-ver)
  (frimacs-build-company-frimacs-package src-dir archive pkg-ver))

(defun frimacs-upgrade-all-emacs-packages (src-dir archive pkg-ver)
  "Build, upload and install all frimacs project packages.

Specifying project source directory, package archive name and
package version string.  The package archive name should be one
of those specified in the `package-archives' variable.

Any already installed versions will be removed first.  All
generated packages will have the same version number."
  (interactive (frimacs-build-interactive-args))
  (message "Building new packages")
  (frimacs-build-frimacs-package src-dir archive pkg-ver)
  (frimacs-build-ob-fricas-package src-dir archive pkg-ver)
  (frimacs-build-company-frimacs-package src-dir archive pkg-ver)
  (let ((installed-frimacs-pkg
         (cadr (assoc 'frimacs package-alist)))
        (installed-ob-fricas-pkg
         (cadr (assoc 'ob-fricas package-alist)))
        (installed-company-frimacs-pkg
         (cadr (assoc 'company-frimacs package-alist)))
        (updated-frimacs-pkg
         (package-desc-create :name 'frimacs
                              :version (frimacs-build-parse-version-string pkg-ver)
                              :kind 'tar
                              :archive archive))
        (updated-ob-fricas-pkg
         (package-desc-create :name 'ob-fricas
                              :version (frimacs-build-parse-version-string pkg-ver)
                              :kind 'tar
                              :archive archive))
        (updated-company-frimacs-pkg
         (package-desc-create :name 'company-frimacs
                              :version (frimacs-build-parse-version-string pkg-ver)
                              :kind 'tar
                              :archive archive)))
    (message "Removing installed packages")
    (when installed-company-frimacs-pkg
      (message (concat "Removing company-frimacs "
                       (format "%s" (package-desc-version installed-company-frimacs-pkg))))
      (package-delete installed-company-frimacs-pkg))
    (when installed-ob-fricas-pkg
      (message (concat "Removing ob-fricas "
                       (format "%s" (package-desc-version installed-ob-fricas-pkg))))
      (package-delete installed-ob-fricas-pkg))
    (when installed-frimacs-pkg
      (message (concat "Removing frimacs "
                       (format "%s" (package-desc-version installed-frimacs-pkg))))
      (package-delete installed-frimacs-pkg))
    (message "Installing new packages")
    (message (concat "Installing frimacs "
                     (format "%s" (package-desc-version updated-frimacs-pkg))))  
    (package-install updated-frimacs-pkg)
    (message (concat "Installing ob-fricas "
                     (format "%s" (package-desc-version updated-ob-fricas-pkg))))
    (package-install updated-ob-fricas-pkg)
    (message (concat "Installing company-frimacs "
                     (format "%s" (package-desc-version updated-company-frimacs-pkg))))
    (package-install updated-company-frimacs-pkg)
    (message "All frimacs packages installed")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Force reload of all source files from this directory

(defvar frimacs-build-source-files
  '("frimacs-base"
    "frimacs-help-mode"
    "frimacs-process-mode"
    "frimacs-input-mode"
    "frimacs-spad-mode"
    "frimacs-boot-mode"
    "frimacs-buffer-menu"
    "frimacs-selector"))

(defun frimacs-build-compile ()
  "Compile all files in `frimacs-build-source-dir' directory."
  (interactive)
  (dolist (file frimacs-build-source-files)
    (byte-compile-file (concat frimacs-build-source-dir file ".el"))))

(defun frimacs-build-load ()
  "Load all files in `frimacs-build-source-dir' directory."
  (interactive)
  (dolist (file frimacs-build-source-files)
    (load (concat frimacs-build-source-dir file))))

(defun frimacs-build-build ()
  "Compile and load all files in `frimacs-build-source-dir' directory."
  (interactive)
  (frimacs-build-compile)
  (frimacs-build-load))
