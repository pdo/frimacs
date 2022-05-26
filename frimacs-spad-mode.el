;;; frimacs-spad-mode.el --- Part of frimacs  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Paul Onions

;; Author: Paul Onions <paul.onions@acm.org>
;; Keywords: FriCAS, computer algebra, extensions, tools
;; URL: https://github.com/pdo/frimacs

;; This file is not part of GNU Emacs.

;; This file is free software, see the LICENCE file in this directory
;; for copying terms.

;;; Commentary:

;; A major mode for the SPAD language.  SPAD is the library language used
;; by the FriCAS computer algebra system.

;;; Code:

(require 'frimacs-base)
(require 'frimacs-help-mode)
(require 'frimacs-process-mode)

(defface frimacs-spad-doc-comment '((t :inherit font-lock-doc-face))
  "Face used for displaying SPAD documentation comments."
  :group 'frimacs)

(defface frimacs-spad-keyword '((t :inherit font-lock-keyword-face))
  "Face used for displaying SPAD keywords."
  :group 'frimacs)

(defcustom frimacs-spad-indentation-step 2
  "Indentation step to use in `frimacs-spad-mode' buffers."
  :type 'integer
  :group 'frimacs)

(defvar frimacs-spad-mode-syntax-table
  (copy-syntax-table frimacs-common-syntax-table)
  "The `frimacs-spad-mode' syntax table.")

(defvar frimacs-spad-doc-comment-regexp
  "\\+\\+.*$"
  "A SPAD documentation comment.")

(defvar frimacs-spad-keyword-names
  (list "add" "with" "has" "is"
        "if" "then" "else"
        "for" "in" "by" "while" "repeat" "return" "break"))

(defvar frimacs-spad-keywords-regexp
  (concat "\\<" (regexp-opt frimacs-spad-keyword-names) "\\>")
  "Regular expression for SPAD keywords.")

(defvar frimacs-spad-doc-comment-face 'frimacs-spad-doc-comment)
(defvar frimacs-spad-keyword-face     'frimacs-spad-keyword)
(defvar frimacs-spad-package-face     'frimacs-package-name)
(defvar frimacs-spad-domain-face      'frimacs-domain-name)
(defvar frimacs-spad-category-face    'frimacs-category-name)
(defvar frimacs-spad-operation-face   'frimacs-operation-name)

(defvar frimacs-spad-font-lock-keywords
  (list (cons frimacs-spad-doc-comment-regexp        'frimacs-spad-doc-comment-face)
        (cons frimacs-spad-keywords-regexp           'frimacs-spad-keyword-face)
        (cons frimacs-standard-package-names-regexp  'frimacs-spad-package-face)
        (cons frimacs-standard-domain-names-regexp   'frimacs-spad-domain-face)
        (cons frimacs-standard-category-names-regexp 'frimacs-spad-category-face)))

(defvar frimacs-spad-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map frimacs-common-keymap)
    map)
  "The `frimacs-spad-mode' local keymap.")

(defvar frimacs-spad-mode-hook nil
  "Hook for customizing `frimacs-spad-mode'.")

(defun frimacs-spad-complete-symbol ()
  "Attempt to complete the FriCAS symbol at point."
  (and (looking-back "[[:word:]]+" nil t)
       (list (match-beginning 0)
             (match-end 0)
             frimacs-standard-names)))

(defvar frimacs-spad-indentation-increase-regexp
  "\\(^[[:blank:]]*if\\|else$\\|repeat$\\|==$\\|:$\\|with\\|add\\)"
  "Increase next line's indentation level if matched.")

(defun frimacs-spad-indent-line ()
  "Indent current line."
  (if (eql (char-syntax (char-before)) ?w)
      (complete-symbol nil)
    (let ((computed-indent (+ (frimacs-find-previous-indent)
                              (frimacs-compute-indent-increment
                               frimacs-spad-indentation-increase-regexp
                               frimacs-spad-indentation-step))))
      (if (or (eql (current-column) 0)
              (frimacs-in-indent-space))
          (frimacs-set-current-indent computed-indent)
        (frimacs-set-current-indent (frimacs-find-previous-indent (current-column)))))))

(defun frimacs-spad-syntax-propertize (start end)
  "Apply appropriate text properties to buffer between START and END."
  ;; Highlight operation names
  (remove-text-properties start end '(font-lock-face nil))
  (goto-char start)
  (while (and (< (point) end) (re-search-forward "\\([[:word:]]+\\)\\([[:blank:]]+[[:word:]]\\|[[:blank:]]*(\\)" end t))
    (let ((matched (match-string 1)))
      (when (and matched (> (length matched) 1)
                 (member matched frimacs-standard-operation-names))
        (put-text-property (match-beginning 1) (match-end 1)
                           'font-lock-face frimacs-spad-operation-face)))
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
(define-derived-mode frimacs-spad-mode prog-mode "Frimacs SPAD"
  "Major mode for the FriCAS SPAD language."
  :group 'frimacs
  (setq font-lock-defaults (list frimacs-spad-font-lock-keywords))
  (setq electric-indent-inhibit t)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'completion-at-point-functions)
  (make-local-variable 'syntax-propertize-function)
  (make-local-variable 'adaptive-fill-first-line-regexp)
  (make-local-variable 'adaptive-fill-regexp)
  (make-local-variable 'fill-paragraph-function)
  (setq indent-line-function #'frimacs-spad-indent-line)
  (setq completion-at-point-functions (list #'frimacs-spad-complete-symbol))
  (setq syntax-propertize-function #'frimacs-spad-syntax-propertize)
  (setq adaptive-fill-first-line-regexp "[[:blank:]]*\\(\\+\\+\\|--\\)[[:blank:]]?")
  (setq adaptive-fill-regexp "[[:blank:]]*\\(\\+\\+\\|--\\)[[:blank:]]?")
  (setq fill-paragraph-function #'frimacs-fill-paragraph)
  (setq frimacs-menu-compile-buffer-enable t)
  (setq frimacs-menu-compile-file-enable t)
  (setq frimacs-menu-read-buffer-enable nil)
  (setq frimacs-menu-read-file-enable nil)
  (setq frimacs-menu-read-region-enable t)
  (setq frimacs-menu-read-pile-enable nil))

(provide 'frimacs-spad-mode)

;;; frimacs-spad-mode.el ends here
