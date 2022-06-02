;;; company-frimacs.el --- A Frimacs backend for Company  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Paul Onions

;; Author: Paul Onions <paul.onions@acm.org>
;; Keywords: FriCAS, computer algebra, extensions, tools
;; URL: https://github.com/pdo/frimacs
;; Package-Requires: ((emacs "26.1") (company "0.9") (frimacs "1.0"))
;; Version: 1.0

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: MIT

;; This file is free software, see the LICENCE file in this directory
;; for more information.

;;; Commentary:

;; Backend routines to support company-mode name completion in frimacs
;; buffers.

;; To enable this extension put
;;
;;  (require 'company-frimacs)
;;
;; in your ~/.emacs initialisation file.
;;

;;; Code:

(require 'cl-lib)

(require 'company)

(require 'frimacs-help-mode)
(require 'frimacs-process-mode)
(require 'frimacs-input-mode)
(require 'frimacs-spad-mode)
  
(defun company-frimacs (command &optional arg &rest ignored)
  "A company backend for frimacs.
See company documentation for COMMAND, ARG and IGNORED syntax."
  (interactive (list 'interactive))
  (cl-case command
    (interactive
     (company-begin-backend 'company-frimacs))
    (prefix
     (and (or (eql major-mode 'frimacs-process-mode)
              (eql major-mode 'frimacs-input-mode)
              (eql major-mode 'frimacs-spad-mode))
          (and (looking-back "[[:word:]]+" nil t)
               (buffer-substring-no-properties
                (match-beginning 0) (match-end 0)))))
    (candidates
     (cl-remove-if-not
      (lambda (c) (string-prefix-p arg c))
      (if (eql major-mode 'frimacs-spad-mode)
          frimacs-standard-names
        frimacs-standard-names-and-abbreviations)))
    (annotation
     (cl-case (car (frimacs-process-constructor-type arg))
       (:package  " [Pkg]")
       (:domain   " [Dom]")
       (:category " [Cat]")))
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

(add-to-list 'company-backends #'company-frimacs)

(provide 'company-frimacs)

;;; company-frimacs.el ends here
