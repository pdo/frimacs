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
;; Backend routines to support company-mode name completion in frimacs
;; buffers.
;;
(require 'cl-lib)

(with-eval-after-load 'company
  (require 'frimacs-help-mode)
  (require 'frimacs-process-mode)
  (require 'frimacs-input-mode)
  (require 'frimacs-spad-mode)
  
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

  (add-to-list 'company-backends 'frimacs-company-backend))

(provide 'frimacs-company)
