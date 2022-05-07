;;; frimacs-company.el -- A Frimacs backend for company-mode -*- lexical-binding: t -*-

;; Copyright (C) 2022 Paul Onions

;; Author: Paul Onions <paul.onions@acm.org>
;; Keywords: FriCAS

;; This file is free software, see the LICENCE file in this directory
;; for copying terms.

;; Package-Requires: ((emacs "24.2") (company "0.9") (frimacs "20220101"))

;;; Commentary:

;; Backend routines to support company-mode name completion in frimacs
;; buffers.

;; Note: this package (frimacs-company) can be considered to be the
;; successor to the company-axiom package.  Currently frimacs-company
;; offers essentially the same functionality as company-axiom did, but
;; with a dependency on frimacs instead of axiom-environment, and with
;; all function and variable names changed.  This has been done in a
;; fit of honesty, to acknowledge the fact that all development has
;; been done with FriCAS from day one, and that it should be FriCAS
;; that takes centre stage in future developments.

;;; Code:

(require 'cl-lib)
(require 'frimacs)

;;;###autoload
(defun frimacs-company-backend (command &optional arg &rest ignored)
  "A company backend for frimacs.

See company documentation for COMMAND, ARG and IGNORED syntax."
  (interactive
   (company-begin-backend 'frimacs-company-backend))
  (cl-case command
    (prefix
     (and (or (eql major-mode 'frimacs-process-mode)
              (eql major-mode 'frimacs-input-mode)
              (eql major-mode 'frimacs-spad-mode))
          (company-grab-symbol)))
    (candidates
     (cl-remove-if-not
      (lambda (c) (string-prefix-p arg c))
      (if (eql major-mode 'frimacs-spad-mode)
          frimacs-standard-names
        frimacs-standard-names-and-abbreviations)))
    (annotation
     (cl-case (car (frimacs-process-constructor-type arg))
       (:package  " [P]")
       (:domain   " [D]")
       (:category " [C]")))
    (doc-buffer
     (cond ((not (get-buffer frimacs-process-repl-buffer-name))
            nil)
           ((frimacs-process-verify-operation-name arg)
            (frimacs-process-document-operation arg))
           ((frimacs-process-verify-constructor-name-or-abbrev arg)
            (frimacs-process-document-constructor arg))))
    (location
     (when (frimacs-process-verify-constructor-name-or-abbrev arg)
       (let ((src-info (frimacs-process-find-constructor-source arg)))
         (cons (cl-first src-info) (cl-second src-info)))))))

;;;###autoload
(eval-after-load 'company
  '(add-to-list 'company-backends 'frimacs-company-backend))

(provide 'frimacs-company)
