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
;; A major mode to use in Frimacs help text buffers.
;;
(require 'frimacs-base)

(defvar frimacs-help-package-face  'frimacs-package-name)
(defvar frimacs-help-domain-face   'frimacs-domain-name)
(defvar frimacs-help-category-face 'frimacs-category-name)

(defvar frimacs-help-mode-syntax-table
  (copy-syntax-table frimacs-common-syntax-table)
  "The frimacs-help-mode syntax table.")

(defvar frimacs-help-font-lock-keywords
  (list (cons frimacs-standard-package-names-regexp          'frimacs-help-package-face)
        (cons frimacs-standard-package-abbreviations-regexp  'frimacs-help-package-face)
        (cons frimacs-standard-domain-names-regexp           'frimacs-help-domain-face)
        (cons frimacs-standard-domain-abbreviations-regexp   'frimacs-help-domain-face)
        (cons frimacs-standard-category-names-regexp         'frimacs-help-category-face)
        (cons frimacs-standard-category-abbreviations-regexp 'frimacs-help-category-face)))

(defvar frimacs-help-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map frimacs-common-keymap)
    (define-key map (kbd "q") 'quit-window)
    map)
  "The frimacs-help-mode local keymap.")

(defvar frimacs-help-mode-hook nil
  "Hook for customizing frimacs-help-mode.")

;;;###autoload
(define-derived-mode frimacs-help-mode fundamental-mode "Frimacs Help"
  "Major mode for Frimacs Help buffers."
  :group 'frimacs
  (setq font-lock-defaults (list frimacs-help-font-lock-keywords))
  (setq frimacs-menu-compile-buffer-enable nil)
  (setq frimacs-menu-compile-file-enable nil)
  (setq frimacs-menu-read-buffer-enable nil)
  (setq frimacs-menu-read-file-enable nil)
  (setq frimacs-menu-read-region-enable t)
  (setq frimacs-menu-read-pile-enable nil))

(provide 'frimacs-help-mode)
