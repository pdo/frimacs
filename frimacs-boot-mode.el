;;; frimacs-boot-mode.el --- Part of frimacs  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Paul Onions

;; Author: Paul Onions <paul.onions@acm.org>
;; Keywords: FriCAS, computer algebra, extensions, tools
;; URL: https://github.com/pdo/frimacs

;; This file is not part of GNU Emacs.

;; This file is free software, see the LICENCE file in this directory
;; for copying terms.

;;; Commentary:

;; A major mode for the FriCAS Boot language.  Boot is an internal
;; language of this computer algebra system that is used to implement
;; the SPAD compiler and interactive language interpreter.  It has a
;; relatively direct mapping to Common Lisp.

;;; Code:

(require 'frimacs-base)

(defface frimacs-boot-keyword '((t :inherit font-lock-keyword-face))
  "Face used for displaying FriCAS Boot keywords."
  :group 'frimacs)

(defcustom frimacs-boot-indentation-step 2
  "Indentation step to use in `frimacs-boot-mode' buffers."
  :type 'integer
  :group 'frimacs)

(defcustom frimacs-boot-source-dirs
  '("<fricas-source-dir>/src/interp/"
    "<fricas-source-dir>/src/boot/")
  "A list of directories in which to search for Boot language source code."
  :type 'list
  :group 'frimacs)

(defvar frimacs-boot-mode-syntax-table
  (let ((table (make-syntax-table prog-mode-syntax-table)))
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
  "The `frimacs-boot-mode' syntax table.")

(defvar frimacs-boot-keyword-names
  (list "and" "by" "case" "cross" "else" "for" "if" "in" "is" "isnt" "of"
        "or" "repeat" "return" "structure" "then" "until" "where" "while"))

(defvar frimacs-boot-keywords-regexp
  (concat "\\<" (regexp-opt frimacs-boot-keyword-names) "\\>")
  "Regular expression for FriCAS Boot keywords.")

(defvar frimacs-boot-imenu-generic-expression
  '(("Variable" "^\\(\\$[[:word:]]+\\)" 1)
    ("Function" "^\\([[:word:]]+\\).+==" 1))
  "Setting for `imenu-generic-expression' in `frimacs-boot-mode'.")

(defvar frimacs-boot-keyword-face 'frimacs-boot-keyword)

(defvar frimacs-boot-font-lock-keywords
  (list (cons frimacs-boot-keywords-regexp 'frimacs-boot-keyword-face)))

(defvar frimacs-boot-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map frimacs-common-keymap)
    map)
  "The `frimacs-boot-mode' local keymap.")

(defvar frimacs-boot-mode-hook nil
  "Hook for customizing `frimacs-boot-mode'.")

(defvar frimacs-boot-indentation-increase-regexp
  "\\(then$\\|else$\\|repeat$\\|==$\\)"
  "When to increase next line's indentation level.")

(defun frimacs-boot-indent-line ()
  "Indent current line."
  (if (eql (char-syntax (char-before)) ?w)
      (complete-symbol nil)
    (let ((computed-indent (+ (frimacs-find-previous-indent)
                              (frimacs-compute-indent-increment
                               frimacs-boot-indentation-increase-regexp
                               frimacs-boot-indentation-step))))
      (if (or (eql (current-column) 0)
              (frimacs-in-indent-space))
          (frimacs-set-current-indent computed-indent)
        (frimacs-set-current-indent (frimacs-find-previous-indent (current-column)))))))

(defvar frimacs-boot-syntax-propertize-fn
  (syntax-propertize-rules
   ("\\(-\\)\\(-\\)"     (1 (string-to-syntax "< 1"))
                         (2 (string-to-syntax "< 2")))
   ("\\(\\+\\)\\(\\+\\)" (1 (string-to-syntax "< 1"))
                         (2 (string-to-syntax "< 2")))))

(defvar frimacs-boot-etags-regexp
  "/\\(\\$?\\w+\\??\\)/\\1/"
  "Regular expression for matching Boot function and variable definitions.")

(defun frimacs-boot-make-etags-files ()
  "Create etags files in the directories specified by `frimacs-boot-source-dirs'."
  (interactive)
  (let ((ropt (concat " --regex='" frimacs-boot-etags-regexp "'")))
    (dolist (dir frimacs-boot-source-dirs)
      (let ((cmd (concat "find " dir " -name '*.boot' -print"
                         " | etags --language=none " ropt " -o " dir "/TAGS -"
                         " ; echo Done")))
        (message "Running shell command: %s" cmd)
        (shell-command cmd)))))

;;;###autoload
(define-derived-mode frimacs-boot-mode prog-mode "Frimacs Boot"
  "Major mode for the FriCAS Boot language."
  :group 'frimacs
  (setq font-lock-defaults (list frimacs-boot-font-lock-keywords))
  (setq electric-indent-inhibit t)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'syntax-propertize-function)
  (make-local-variable 'adaptive-fill-first-line-regexp)
  (make-local-variable 'adaptive-fill-regexp)
  (make-local-variable 'fill-paragraph-function)
  (setq indent-line-function #'frimacs-boot-indent-line)
  (setq syntax-propertize-function frimacs-boot-syntax-propertize-fn)
  (setq adaptive-fill-first-line-regexp "[[:blank:]]*\\(\\+\\+\\|--\\)[[:blank:]]?")
  (setq adaptive-fill-regexp "[[:blank:]]*\\(\\+\\+\\|--\\)[[:blank:]]?")
  (setq fill-paragraph-function (function frimacs-fill-paragraph))
  (setq imenu-generic-expression frimacs-boot-imenu-generic-expression)
  (setq frimacs-menu-compile-buffer-enable nil)
  (setq frimacs-menu-compile-file-enable nil)
  (setq frimacs-menu-read-buffer-enable nil)
  (setq frimacs-menu-read-file-enable nil)
  (setq frimacs-menu-read-region-enable nil)
  (setq frimacs-menu-read-pile-enable nil))

(provide 'frimacs-boot-mode)

;;; frimacs-boot-mode.el ends here
