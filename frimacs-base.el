;;; frimacs-base.el --- Part of frimacs  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Paul Onions

;; Author: Paul Onions <paul.onions@acm.org>
;; Keywords: FriCAS, computer algebra, extensions, tools
;; URL: https://github.com/pdo/frimacs

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: MIT

;; This file is free software, see the LICENCE file in this directory
;; for more information.

;;; Commentary:

;; Basic setup for Frimacs.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizations
;;
;;;###autoload
(defgroup frimacs nil
  "An environment for working with the FriCAS computer algebra system."
  :group 'languages)

(defcustom frimacs-select-popup-windows t
  "Set non-nil to automatically switch to popup windows."
  :type 'boolean
  :group 'frimacs)

(defcustom frimacs-select-displayed-repl nil
  "Set non-nil to automatically switch to displayed REPL buffer."
  :type 'boolean
  :group 'frimacs)

(defcustom frimacs-standard-package-info-file "fricas-standard-package-info.dat"
  "File from which to `read' standard package information."
  :type 'string
  :group 'frimacs)

(defcustom frimacs-standard-domain-info-file "fricas-standard-domain-info.dat"
  "File from which to `read' standard domain information."
  :type 'string
  :group 'frimacs)

(defcustom frimacs-standard-category-info-file "fricas-standard-category-info.dat"
  "File from which to `read' standard category information."
  :type 'string
  :group 'frimacs)

(defcustom frimacs-standard-operation-info-file "fricas-standard-operation-info.dat"
  "File from which to `read' standard operation information."
  :type 'string
  :group 'frimacs)

(defface frimacs-package-name '((t :inherit font-lock-constant-face))
  "Face used for displaying package names."
  :group 'frimacs)

(defface frimacs-domain-name '((t :inherit font-lock-builtin-face))
  "Face used for displaying domain names."
  :group 'frimacs)

(defface frimacs-category-name '((t :inherit font-lock-type-face))
  "Face used for displaying category names."
  :group 'frimacs)

(defface frimacs-operation-name '((t :inherit font-lock-function-name-face))
  "Face used for displaying operation names."
  :group 'frimacs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions for generating/loading pre-computed data
;;
(defvar frimacs-source-dir
  (file-name-directory (or load-file-name (buffer-file-name)))
  "Frimacs source directory.")

(defvar frimacs-data-dir
  (concat frimacs-source-dir "data/")
  "Frimacs data directory.")

(defun frimacs-write-data-file (obj filename)
  "Write OBJ to FILENAME using function `pp', the pretty-printer.

The directory in which to write the file defaults to the value of
the variable `frimacs-data-dir'.  This can be overridden by
specifying a different path in the FILENAME string (either
relative or absolute)."
  (let ((default-directory frimacs-data-dir))
    (with-temp-buffer
      (insert ";; -*-no-byte-compile: t; -*-\n")
      (pp obj (current-buffer))
      (write-region (point-min) (point-max) filename))))

(defun frimacs-read-data-file (filename)
  "Read a Lisp object from FILENAME using function `read'.

The directory in which FILENAME resides is assumed to be the
value of the variable `frimacs-data-dir'.  This can be overridden
by specifying a different path in the FILENAME string (either
relative or absolute)."
  (let ((default-directory frimacs-data-dir))
    (with-temp-buffer
      (insert-file-contents filename)
      (goto-char (point-min))
      (read (current-buffer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load standard package/domain/category/operation names files
;;
(message "Loading Frimacs data files")
(message "Loading Frimacs standard package information")

(defvar frimacs-standard-package-info
  (frimacs-read-data-file frimacs-standard-package-info-file)
  "A list of standard FriCAS package (abbrev . name) pairs.")

(defvar frimacs-standard-package-names
  (mapcar #'cdr frimacs-standard-package-info)
  "A list of standard FriCAS package names.")
  
(defvar frimacs-standard-package-names-regexp
  (concat "\\<" (regexp-opt (mapcar #'regexp-quote frimacs-standard-package-names)) "\\>")
  "Regular expression for FriCAS standard package names.")

(defvar frimacs-standard-package-abbreviations
  (remove nil (mapcar #'car frimacs-standard-package-info))
  "A list of standard FriCAS package abbreviations.")

(defvar frimacs-standard-package-abbreviations-regexp
  (concat "\\<" (regexp-opt (mapcar #'regexp-quote frimacs-standard-package-abbreviations)) "\\>")
  "Regular expression for FriCAS standard package abbreviations.")

(defvar frimacs-standard-package-names-and-abbreviations
  (append frimacs-standard-package-names
          frimacs-standard-package-abbreviations)
  "Standard FriCAS package names and abbreviations.")

(message "Loading Frimacs standard domain information")

(defvar frimacs-standard-domain-info
  (frimacs-read-data-file frimacs-standard-domain-info-file)
  "A list of standard FriCAS domain (abbrev . name) pairs.")

(defvar frimacs-standard-domain-names
  (mapcar #'cdr frimacs-standard-domain-info)
  "A list of standard FriCAS domain names.")
  
(defvar frimacs-standard-domain-names-regexp
  (concat "\\<" (regexp-opt (mapcar #'regexp-quote frimacs-standard-domain-names)) "\\>")
  "Regular expression for FriCAS standard domain names.")

(defvar frimacs-standard-domain-abbreviations
  (remove nil (mapcar #'car frimacs-standard-domain-info))
  "A list of standard FriCAS domain abbreviations.")

(defvar frimacs-standard-domain-abbreviations-regexp
  (concat "\\<" (regexp-opt (mapcar #'regexp-quote frimacs-standard-domain-abbreviations)) "\\>")
  "Regular expression for FriCAS standard domain abbreviations.")

(defvar frimacs-standard-domain-names-and-abbreviations
  (append frimacs-standard-domain-names
          frimacs-standard-domain-abbreviations)
  "Standard FriCAS domain names and abbreviations.")

(message "Loading Frimacs standard category information")

(defvar frimacs-standard-category-info
  (frimacs-read-data-file frimacs-standard-category-info-file)
  "A list of standard FriCAS category (abbrev . name) pairs.")

(defvar frimacs-standard-category-names
  (mapcar #'cdr frimacs-standard-category-info)
  "A list of standard FriCAS category names.")
  
(defvar frimacs-standard-category-names-regexp
  (concat "\\<" (regexp-opt (mapcar #'regexp-quote frimacs-standard-category-names)) "\\>")
  "Regular expression for FriCAS standard category names.")

(defvar frimacs-standard-category-abbreviations
  (remove nil (mapcar #'car frimacs-standard-category-info))
  "A list of standard FriCAS category abbreviations.")

(defvar frimacs-standard-category-abbreviations-regexp
  (concat "\\<" (regexp-opt (mapcar #'regexp-quote frimacs-standard-category-abbreviations)) "\\>")
  "Regular expression for FriCAS standard category abbreviations.")

(defvar frimacs-standard-category-names-and-abbreviations
  (append frimacs-standard-category-names
          frimacs-standard-category-abbreviations)
  "Standard FriCAS category names and abbreviations.")

(message "Loading Frimacs standard operation information")

(defvar frimacs-standard-operation-info
  (frimacs-read-data-file frimacs-standard-operation-info-file)
  "A list of standard FriCAS operation names.")

(defvar frimacs-standard-operation-names
  frimacs-standard-operation-info
  "A list of standard FriCAS operation names.")

(defvar frimacs-standard-operation-names-regexp
  (concat "\\<" (regexp-opt (mapcar #'regexp-quote frimacs-standard-operation-names)) "\\>")
  "Regular expression for FriCAS standard operation names.")

(message "Done loading Frimacs data files")

;; Lists combining package, domain & category names and/or abbreviations
(defvar frimacs-standard-constructor-names
  (append frimacs-standard-package-names
          frimacs-standard-domain-names
          frimacs-standard-category-names)
  "Standard FriCAS constructor names.")

(defvar frimacs-standard-constructor-abbreviations
  (append frimacs-standard-package-abbreviations
          frimacs-standard-domain-abbreviations
          frimacs-standard-category-abbreviations)
  "Standard FriCAS constructor abbreviations.")

(defvar frimacs-standard-constructor-names-and-abbreviations
  (append frimacs-standard-constructor-names
          frimacs-standard-constructor-abbreviations)
  "Standard FriCAS constructor names and abbreviations.")

;; Lists combining all constructor and operation names and abbreviations
(defvar frimacs-standard-names
  (append frimacs-standard-constructor-names
          frimacs-standard-operation-names)
  "Standard FriCAS names (package, domain, category & operation).")

(defvar frimacs-standard-names-and-abbreviations
  (append frimacs-standard-constructor-names-and-abbreviations
          frimacs-standard-operation-names)
  "Standard FriCAS names and abbreviations.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common syntax table
;;
(defvar frimacs-common-syntax-table
  (let ((table (copy-syntax-table prog-mode-syntax-table)))
    (modify-syntax-entry ?_ "\\" table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\t " " table)
    (modify-syntax-entry ?\\ "." table)
    (modify-syntax-entry ?* "." table)
    (modify-syntax-entry ?/ "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?# "." table)
    (modify-syntax-entry ?$ "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?% "w" table)
    (modify-syntax-entry ?! "w" table)
    (modify-syntax-entry ?? "w" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    table)
  "The Frimacs common syntax table.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common indentation routines
;;
(defun frimacs-find-previous-indent (&optional bound)
  "Find the indentation level of the previous non-blank line.

If BOUND is non-nil then find the indentation level of the most
recent line whose indentation level is strictly less then BOUND."
  (save-excursion
    (beginning-of-line)
    (let ((bound-satisfied nil)
          (indent 0))
      (while (not bound-satisfied)
        (setq indent (if (re-search-backward "^\\([[:blank:]]*\\)[[:graph:]]" nil t)
                         (- (match-end 1) (match-beginning 1))
                       0))
        (when (or (not bound) (< indent bound))
          (setq bound-satisfied t)))
      indent)))

(defun frimacs-compute-indent-increment (regexp step)
  "Compute the required increase in indentation level.

If the previous non-blank line matches REGEXP then return STEP,
otherwise return 0."
  (save-excursion
    (beginning-of-line)
    (let ((limit (point)))
      (re-search-backward "[[:graph:]]")
      (beginning-of-line)
      (if (re-search-forward regexp limit t)
          step
        0))))

(defun frimacs-in-indent-space ()
  "Determine if point is inside the current line's indentation space."
  (let ((match nil))
    (save-excursion
      (end-of-line)
      (let ((eol (point)))
        (beginning-of-line)
        (setq match (re-search-forward "[[:blank:]]*\\([[:graph:]]\\|$\\)" eol))))
    (and match (< (point) (match-beginning 1)))))

(defun frimacs-set-current-indent (amount)
  "Set the indentation level of the current line to AMOUNT.

If point is within the indentation space then move it to the end
of the space, to the specified indentation level."
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward "^\\([[:blank:]]*\\)" nil t)
        (replace-match (make-string amount ?\ ))))
  (let ((left-of-indent (- amount (current-column))))
    (when (> left-of-indent 0)
      (forward-char left-of-indent))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common filling routines
;;
(defun frimacs-fill-paragraph (&optional justify region)
  "Find current paragraph limits and call `fill-region-as-paragraph'.

Use JUSTIFY justification mode, and if REGION indicates an active
region then use its limits instead of trying to find our own."
  (let ((start nil)
        (end nil))
    (if region
        (progn
          (setq start (region-beginning))
          (setq end (region-end)))
      (save-excursion
        (beginning-of-line)
        (while (looking-at-p "^[[:blank:]]*\\(\\+\\+\\|--\\)[[:blank:]]*[[:graph:]]+")
          (forward-line -1))
        (forward-line)
        (setq start (point))
        (while (looking-at-p "^[[:blank:]]*\\(\\+\\+\\|--\\)[[:blank:]]*[[:graph:]]+")
          (forward-line +1))
        (setq end (point))))
    (fill-region-as-paragraph start end justify)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common keymap (including the ``Frimacs'' menu)
;;
(defvar frimacs-menu-compile-buffer-enable nil)
(defvar frimacs-menu-compile-file-enable   nil)

(defvar frimacs-menu-read-buffer-enable   nil)
(defvar frimacs-menu-read-file-enable     nil)
(defvar frimacs-menu-read-region-enable   nil)
(defvar frimacs-menu-read-pile-enable nil)

(make-variable-buffer-local 'frimacs-menu-compile-buffer-enable)
(make-variable-buffer-local 'frimacs-menu-compile-file-enable)

(make-variable-buffer-local 'frimacs-menu-read-buffer-enable)
(make-variable-buffer-local 'frimacs-menu-read-file-enable)
(make-variable-buffer-local 'frimacs-menu-read-region-enable)
(make-variable-buffer-local 'frimacs-menu-read-pile-enable)

(defun frimacs-edit-customization-group ()
  "Enter the Emacs customization editor for the Frimacs group."
  (interactive)
  (customize-group 'frimacs))

(defvar frimacs-common-keymap
  (let ((map (make-sparse-keymap "Frimacs"))
        (menu-map (make-sparse-keymap "Frimacs")))
    (set-keymap-parent map prog-mode-map)
    ;; Key assignments
    (define-key map (kbd "C-c C-d p") 'frimacs-process-show-package)
    (define-key map (kbd "C-c C-d d") 'frimacs-process-show-domain)
    (define-key map (kbd "C-c C-d c") 'frimacs-process-show-category)
    (define-key map (kbd "C-c C-d k") 'frimacs-process-show-constructor)
    (define-key map (kbd "C-c C-d o") 'frimacs-process-display-operation)
    (define-key map (kbd "C-c C-d a") 'frimacs-process-apropos-thing-at-point)
    (define-key map (kbd "C-c C-w")   'frimacs-process-webview-constructor)
    (define-key map (kbd "C-c C-s")   'frimacs-process-edit-constructor-source)
    (define-key map (kbd "C-c C-b k") 'frimacs-process-compile-buffer)
    (define-key map (kbd "C-c C-k")   'frimacs-process-compile-file)
    (define-key map (kbd "C-c C-b r") 'frimacs-process-read-buffer)
    (define-key map (kbd "C-c C-r")   'frimacs-process-read-file)
    (define-key map (kbd "C-c C-y")   'frimacs-process-read-region)
    (define-key map (kbd "C-c C-c")   'frimacs-process-read-pile)
    (define-key map (kbd "C-c C-e")   'frimacs-process-eval-region)
    ;; Menu items
    (define-key map [menu-bar frimacs-menu] (cons "Frimacs" menu-map))
    (define-key menu-map [frimacs-menu-run-fricas]
      '(menu-item "Run FriCAS" run-fricas))
    (define-key menu-map [frimacs-menu-start-hyperdoc]
      '(menu-item "Start HyperDoc" frimacs-process-start-hyperdoc))
    (define-key menu-map [frimacs-menu-separator-4]
      '(menu-item "--"))
    (define-key menu-map [frimacs-menu-edit-customization-group]
      '(menu-item "Emacs Customizations" frimacs-edit-customization-group))
    (define-key menu-map [frimacs-menu-separator-3]
      '(menu-item "--"))
    (define-key menu-map [frimacs-menu-read-pile]
      '(menu-item "Read Pile" frimacs-process-read-pile
                  :enable frimacs-menu-read-pile-enable))
    (define-key menu-map [frimacs-menu-read-region]
      '(menu-item "Read Region" frimacs-process-read-region
                  :enable frimacs-menu-read-region-enable))
    (define-key menu-map [frimacs-menu-read-buffer]
      '(menu-item "Read Buffer" frimacs-process-read-buffer
                  :enable frimacs-menu-read-buffer-enable))
    (define-key menu-map [frimacs-menu-read-file]
      '(menu-item "Read File..." frimacs-process-read-file
                  :enable frimacs-menu-read-file-enable))
    (define-key menu-map [frimacs-menu-separator-2]
      '(menu-item "--"))
    (define-key menu-map [frimacs-menu-compile-buffer]
      '(menu-item "Compile Buffer" frimacs-process-compile-buffer
                  :enable frimacs-menu-compile-buffer-enable))
    (define-key menu-map [frimacs-menu-compile-file]
      '(menu-item "Compile File..." frimacs-process-compile-file
                  :enable frimacs-menu-compile-file-enable))
    (define-key menu-map [frimacs-menu-separator-1]
      '(menu-item "--"))
    (define-key menu-map [frimacs-menu-webview-constructor]
      '(menu-item "View Constructor Web Doc..." frimacs-process-webview-constructor))
    (define-key menu-map [frimacs-menu-edit-constructor-source]
      '(menu-item "Find Constructor Source..." frimacs-process-edit-constructor-source))
    (define-key menu-map [frimacs-menu-separator-0]
      '(menu-item "--"))
    (define-key menu-map [frimacs-menu-apropos]
      '(menu-item "Apropos (at point)..." frimacs-process-apropos-thing-at-point))
    (define-key menu-map [frimacs-menu-display-operation]
      '(menu-item "Display Operation..." frimacs-process-display-operation))
    (define-key menu-map [frimacs-menu-show-constructor]
      '(menu-item "Show Constructor..." frimacs-process-show-constructor))
    (define-key menu-map [frimacs-menu-show-category]
      '(menu-item "Show Category..." frimacs-process-show-category))
    (define-key menu-map [frimacs-menu-show-domain]
      '(menu-item "Show Domain..." frimacs-process-show-domain))
    (define-key menu-map [frimacs-menu-show-package]
      '(menu-item "Show Package..." frimacs-process-show-package))
    map)
  "The Frimacs keymap.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions
;;
(defun frimacs-move-to-next-line ()
  "Move to beginning of next line.

Move beyond current line and all subsequent
continuation-lines (underscores escape new lines) to the beginning
of the next non-blank line."
  (let ((posn nil)
        (n 1)
        (done nil))
    (while (not done)
      (let ((p (line-end-position n)))
        (cond ((eql p posn)
               (setq done t))
              ((eql (char-before p) ?_)
               (setq posn p)
               (cl-incf n))
              (t
               (setq posn p)
               (setq done t)))))
    (goto-char posn)
    (re-search-forward "^.+$" nil t)
    (beginning-of-line)))

(defun frimacs-get-rest-of-line ()
  "Return the remainder of the current line.

Return a string containing the remainder of the current
line (from point), and the concatenation of all subsequent
continuation-lines (underscores escape new lines)."
  (let ((posns nil)
        (n 1)
        (done nil))
    (while (not done)
      (let ((p (line-end-position n)))
        (cond ((eql p (car posns))
               (setq done t))
              ((eql (char-before p) ?_)
               (push p posns)
               (cl-incf n))
              (t
               (push p posns)
               (setq done t)))))
    (let ((line "")
          (beg (point)))
      (dolist (end (reverse posns))
        (let ((end-excl-underscore (if (eql (char-before end) ?_) (1- end) end)))
          (setq line (concat line (buffer-substring-no-properties beg end-excl-underscore))))
        (setq beg (1+ end)))
      line)))

(defun frimacs-flash-region (start end)
  "Flash the region with an overlay.

Region is between START and END positions."
  (let ((ovl (make-overlay start end)))
    (overlay-put ovl 'face 'secondary-selection)
    (run-with-timer 0.5 nil 'delete-overlay ovl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Developer utils
;;
(defvar frimacs-debug nil)

(defmacro frimacs-debug-message (msg)
  "A debug messaging macro for development.

Display MSG in the minibuffer when `frimacs-debug' is non-nil."
  (if frimacs-debug
      `(message ,msg)
    nil))

(provide 'frimacs-base)

;;; frimacs-base.el ends here
