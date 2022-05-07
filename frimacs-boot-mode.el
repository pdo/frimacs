;; -*- lexical-binding: t -*-
;;
;; Author: Paul Onions <paul.onions@acm.org>
;; Keywords: FriCAS
;;
;; Copyright (C) 2022 Paul Onions
;;
;; This file is free software, see the LICENCE file in this directory
;; for copying terms.
;;
;; A major mode for the FriCAS Boot language.  Boot is an internal
;; language of this computer algebra system that is used to implement
;; the SPAD compiler and interactive language interpreter.  It has a
;; relatively direct mapping to Common Lisp.
;;
(require 'frimacs-base)

(defface frimacs-boot-keyword '((t :inherit font-lock-keyword-face))
  "Face used for displaying FriCAS Boot keywords."
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
  "The frimacs-boot-mode syntax table.")

(defvar frimacs-boot-keyword-names
  (list "and" "by" "case" "cross" "else" "for" "if" "in" "is" "isnt" "of"
        "or" "repeat" "return" "structure" "then" "until" "where" "while"))

(defvar frimacs-boot-keywords-regexp
  (concat "\\<" (regexp-opt frimacs-boot-keyword-names) "\\>")
  "Regular expression for FriCAS Boot keywords.")

(defvar frimacs-boot-keyword-face 'frimacs-boot-keyword)

(defvar frimacs-boot-font-lock-keywords
  (list (cons frimacs-boot-keywords-regexp 'frimacs-boot-keyword-face)))

(defvar frimacs-boot-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map frimacs-common-keymap)
    map)
  "The frimacs-boot-mode local keymap.")

(defvar frimacs-boot-mode-hook nil
  "Hook for customizing frimacs-boot-mode.")

(defvar frimacs-boot-syntax-propertize-fn
  (syntax-propertize-rules
   ("\\(-\\)\\(-\\)"     (1 (string-to-syntax "< 1"))
                         (2 (string-to-syntax "< 2")))
   ("\\(\\+\\)\\(\\+\\)" (1 (string-to-syntax "< 1"))
                         (2 (string-to-syntax "< 2")))))

;;;###autoload
(define-derived-mode frimacs-boot-mode prog-mode "Frimacs Boot"
  "Major mode for the FriCAS Boot language."
  :group 'frimacs
  (setq font-lock-defaults (list frimacs-boot-font-lock-keywords))
  (make-local-variable 'syntax-propertize-function)
  (make-local-variable 'adaptive-fill-first-line-regexp)
  (make-local-variable 'adaptive-fill-regexp)
  (make-local-variable 'fill-paragraph-function)
  (setq syntax-propertize-function frimacs-boot-syntax-propertize-fn)
  (setq adaptive-fill-first-line-regexp "[[:blank:]]*\\(\\+\\+\\|--\\)[[:blank:]]?")
  (setq adaptive-fill-regexp "[[:blank:]]*\\(\\+\\+\\|--\\)[[:blank:]]?")
  (setq fill-paragraph-function (function frimacs-fill-paragraph))
  (setq frimacs-menu-compile-buffer-enable nil)
  (setq frimacs-menu-compile-file-enable nil)
  (setq frimacs-menu-read-buffer-enable nil)
  (setq frimacs-menu-read-file-enable nil)
  (setq frimacs-menu-read-region-enable nil)
  (setq frimacs-menu-read-pile-enable nil))

(provide 'frimacs-boot-mode)
