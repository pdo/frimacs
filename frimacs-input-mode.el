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
;; A major mode for the FriCAS interactive language, e.g. .input files.
;;
(require 'frimacs-base)
(require 'frimacs-help-mode)
(require 'frimacs-process-mode)
(require 'imenu)

(defface frimacs-input-doc-comment '((t :inherit font-lock-doc-face))
  "Face used for displaying FriCAS interactive language documentation comments."
  :group 'frimacs)

(defface frimacs-input-keyword '((t :inherit font-lock-keyword-face))
  "Face used for displaying FriCAS interactive language keywords."
  :group 'frimacs)

(defcustom frimacs-input-indentation-step 2
  "Indentation step to use in `frimacs-input-mode' buffers."
  :type 'integer
  :group 'frimacs)

(defvar frimacs-input-mode-syntax-table
  (copy-syntax-table frimacs-common-syntax-table)
  "The `frimacs-input-mode' syntax table.")

(defvar frimacs-input-doc-comment-regexp
  "\\+\\+.*$"
  "A FriCAS documentation comment.")

(defvar frimacs-input-keyword-names
  (list "has"
        "if" "then" "else"
        "for" "in" "by" "while" "repeat" "return" "break"))

(defvar frimacs-input-keywords-regexp
  (concat "\\<" (regexp-opt frimacs-input-keyword-names) "\\>")
  "Regular expression for FriCAS interactive language keywords.")

(defvar frimacs-input-imenu-generic-expression
  '(("Variable" "^\\([[:word:]]+\\).+:=" 1)
    ("Function" "^\\([[:word:]]+\\).+==\\([^>]\\|$\\)" 1)
    ("Macro" "^\\([[:word:]]+\\).+==>" 1))
  "Setting for `imenu-generic-expression' in `frimacs-input-mode'.")

(defvar frimacs-input-doc-comment-face 'frimacs-input-doc-comment)
(defvar frimacs-input-keyword-face     'frimacs-input-keyword)
(defvar frimacs-input-package-face     'frimacs-package-name)
(defvar frimacs-input-domain-face      'frimacs-domain-name)
(defvar frimacs-input-category-face    'frimacs-category-name)
(defvar frimacs-input-operation-face   'frimacs-operation-name)

(defvar frimacs-input-font-lock-keywords
  (list (cons frimacs-input-doc-comment-regexp               'frimacs-input-doc-comment-face)
        (cons frimacs-input-keywords-regexp                  'frimacs-input-keyword-face)
        (cons frimacs-standard-package-names-regexp          'frimacs-input-package-face)
        (cons frimacs-standard-package-abbreviations-regexp  'frimacs-input-package-face)
        (cons frimacs-standard-domain-names-regexp           'frimacs-input-domain-face)
        (cons frimacs-standard-domain-abbreviations-regexp   'frimacs-input-domain-face)
        (cons frimacs-standard-category-names-regexp         'frimacs-input-category-face)
        (cons frimacs-standard-category-abbreviations-regexp 'frimacs-input-category-face)))

(defvar frimacs-input-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map frimacs-common-keymap)
    (define-key map (kbd "C-<return>") 'frimacs-input-send-line)
    map)
  "The `frimacs-input-mode' local keymap.")

(defvar frimacs-input-mode-hook nil
  "Hook for customizing `frimacs-input-mode'.")

(defun frimacs-input-read-buffer ()
  "Read the current file into FriCAS."
  (interactive)
  (frimacs-process-read-file buffer-file-name))

(defun frimacs-input-send-line ()
  "Send the current line to FriCAS."
  (interactive)
  (let ((str (save-excursion
               (beginning-of-line)
               (frimacs-get-rest-of-line))))
    (frimacs-process-eval-string str)
    (frimacs-move-to-next-line)))

(defun frimacs-input-complete-symbol ()
  "Lookup symbol at point as possible prefix of a FriCAS name."
  (and (looking-back "[[:word:]]+" nil t)
       (list (match-beginning 0)
             (match-end 0)
             frimacs-standard-names-and-abbreviations)))

(defun frimacs-input-interactive-complete ()
  "Complete symbol at point."
  (interactive)
  (if (and (boundp 'company-mode) company-mode)
      (company-complete)
    (complete-symbol nil)))

(defvar frimacs-input-indentation-increase-regexp
  "\\(^[[:blank:]]*if\\|else$\\|repeat$\\|==$\\)"
  "When to increase next line's indentation level.")

(defun frimacs-input-indent-line ()
  "Indent current line."
  (if (eql (char-syntax (char-before)) ?w)
      (frimacs-input-interactive-complete)
    (let ((computed-indent (+ (frimacs-find-previous-indent)
                              (frimacs-compute-indent-increment
                               frimacs-input-indentation-increase-regexp
                               frimacs-input-indentation-step))))
      (if (or (eql (current-column) 0)
              (frimacs-in-indent-space))
          (frimacs-set-current-indent computed-indent)
        (frimacs-set-current-indent (frimacs-find-previous-indent (current-column)))))))

(defun frimacs-input-syntax-propertize (start end)
  "Apply appropriate text properties to buffer between START and END."
  ;; Highlight operation names
  (remove-text-properties start end '(font-lock-face nil))
  (goto-char start)
  (while (and (< (point) end) (re-search-forward "\\([[:word:]]+\\)\\([[:blank:]]+[[:word:]]\\|[[:blank:]]*(\\)" end t))
    (let ((matched (match-string 1)))
      (when (and matched (> (length matched) 1)
                 (member matched frimacs-standard-operation-names))
        (put-text-property (match-beginning 1) (match-end 1)
                           'font-lock-face frimacs-input-operation-face)))
    (goto-char (1- (point))))  ; unswallow word or '(' character
  ;; Mark comment syntax
  (goto-char start)
  (funcall (syntax-propertize-rules
            ("\\(-\\)\\(-\\)"
             (1 (string-to-syntax "< 1"))
             (2 (string-to-syntax "< 2")))
            ("\\(\\+\\)\\(\\+\\)"
             (1 (string-to-syntax "< 1"))
             (2 (string-to-syntax "< 2"))))
           start end))

;;;###autoload
(define-derived-mode frimacs-input-mode prog-mode "Frimacs Input"
  "Major mode for the FriCAS interactive language."
  :group 'frimacs
  (setq font-lock-defaults (list 'frimacs-input-font-lock-keywords))
  (setq electric-indent-inhibit t)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'completion-at-point-functions)
  (make-local-variable 'syntax-propertize-function)
  (make-local-variable 'adaptive-fill-first-line-regexp)
  (make-local-variable 'adaptive-fill-regexp)
  (make-local-variable 'fill-paragraph-function)
  (setq indent-line-function 'frimacs-input-indent-line)
  (setq completion-at-point-functions '(frimacs-input-complete-symbol))
  (setq syntax-propertize-function (function frimacs-input-syntax-propertize))
  (setq adaptive-fill-first-line-regexp "[[:blank:]]*\\(\\+\\+\\|--\\)[[:blank:]]?")
  (setq adaptive-fill-regexp "[[:blank:]]*\\(\\+\\+\\|--\\)[[:blank:]]?")
  (setq fill-paragraph-function (function frimacs-fill-paragraph))
  (setq imenu-generic-expression frimacs-input-imenu-generic-expression)
  (setq frimacs-menu-compile-buffer-enable nil)
  (setq frimacs-menu-compile-file-enable nil)
  (setq frimacs-menu-read-buffer-enable t)
  (setq frimacs-menu-read-file-enable t)
  (setq frimacs-menu-read-region-enable t)
  (setq frimacs-menu-read-pile-enable t))

(provide 'frimacs-input-mode)
