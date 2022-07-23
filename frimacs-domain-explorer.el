;;; frimacs-domain-explorer.el --- Part of frimacs  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Paul Onions

;; Author: Paul Onions <paul.onions@acm.org>
;; Keywords: FriCAS, computer algebra, extensions, tools
;; URL: https://github.com/pdo/frimacs

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: MIT

;; This file is free software, see the LICENCE file in this directory
;; for more information.

;;; Commentary:

;; A utility for exploring domains and their operations.

;; Expects a package called FRIMACS to be available in the running
;; FriCAS system.  It can be built from the following source:-
;;
;;  )abbrev package FRIMACSX FrimacsExplorer
;;  FrimacsExplorer(t:Type): Exports == Implementation where
;;    Exports ==> with
;;      showOpImpSummary : () -> Void
;;    Implementation ==> add
;;      showOpImpSummary() == showImp(t)$Lisp
;;
;; This is still a work-in-progress.  It is exploratory code that may
;; change in fundamental ways at any time.
;;
;; It's also rather crude right now, but even so may be useful to
;; others -- that's why it's published here.

;;; Code:

(require 'cl-lib)
(require 'widget)
(require 'frimacs-base)
(require 'frimacs-process-mode)
(require 'frimacs-help-mode)

(defcustom frimacs-domex-ui-bufname "*Frimacs Domain Explorer %s*"
  "Name of the buffer in which to display the domain explorer.

The %s will be replaced with a session ID number, obtained from
'frimacs-domex-ui-next-id'."
  :type 'string
  :group 'frimacs)

(defvar frimacs-domex-ui-next-id 1)

(defcustom frimacs-domex-query-bufname "*Frimacs Domain Explorer Query*"
  "Name of the buffer in which to capture a domain explorer query."
  :type 'string
  :group 'frimacs)

(defface frimacs-domex-content-face
  '((t :box nil))
  "Face for domain explorer content text."
  :group 'frimacs)

(defvar frimacs-domex-domain-widget nil)
(defvar frimacs-domex-button-widget nil)
(defvar frimacs-domex-content-widget nil)

(defvar frimacs-domex-package-face  'frimacs-package-name)
(defvar frimacs-domex-domain-face   'frimacs-domain-name)
(defvar frimacs-domex-category-face 'frimacs-category-name)

(defvar frimacs-domex-mode-syntax-table
  (copy-syntax-table frimacs-common-syntax-table)
  "The `frimacs-domex-mode' syntax table.")

(defvar frimacs-domex-font-lock-keywords
  (list (cons frimacs-standard-package-names-regexp          'frimacs-domex-package-face)
        (cons frimacs-standard-package-abbreviations-regexp  'frimacs-domex-package-face)
        (cons frimacs-standard-domain-names-regexp           'frimacs-domex-domain-face)
        (cons frimacs-standard-domain-abbreviations-regexp   'frimacs-domex-domain-face)
        (cons frimacs-standard-category-names-regexp         'frimacs-domex-category-face)
        (cons frimacs-standard-category-abbreviations-regexp 'frimacs-domex-category-face)))

(defvar frimacs-domex-mode-map
  (let ((map (copy-keymap widget-keymap)))
    (set-keymap-parent map frimacs-common-keymap)
    map)
  "The `frimacs-domex-mode' local keymap.")

(defvar frimacs-domex-mode-hook nil
  "Hook for customizing `frimacs-domex-mode'.")

(defun frimacs-domex-show-implementation (&rest _ignore)
  "Get implementation details of specified domain."
  (if (not (get-buffer frimacs-process-repl-buffer-name))
      (message frimacs-process-not-running-message)
    (let ((domain (widget-value frimacs-domex-domain-widget))
          (text ""))
      (save-excursion
        (with-current-buffer (get-buffer-create frimacs-domex-query-bufname)
          (erase-buffer)
          (frimacs-help-mode)
          (frimacs-process-redirect-send-command
           (format ")lisp (|parseAndEvalToString| \"showOpImpSummary()$FRIMACSX(%s)\")" domain)
           (current-buffer))
          (frimacs-process-make-all-clickables)
          (setq text (buffer-substring (point-min) (point-max)))))
      (save-excursion
        (widget-value-set frimacs-domex-content-widget "")
        (widget-value-set frimacs-domex-content-widget text)))))

(defun frimacs-domex-prepare-buffer (id)
  "Setup the domain explorer buffer, named according to the given ID."
  (with-current-buffer (get-buffer-create
                        (format frimacs-domex-ui-bufname id))
    (erase-buffer)
    (remove-overlays)
    (frimacs-domex-mode)
    (widget-insert "Frimacs Domain Explorer\n\n")
    (setq frimacs-domex-domain-widget
          (widget-create 'editable-field
                         :format "Domain: %v"
                         ;; :keymap frimacs-common-keymap
                         :action #'frimacs-domex-show-implementation
                         "Polynomial(Fraction Integer)"))
    (widget-insert "\n")
    (setq frimacs-domex-button-widget
          (widget-create 'push-button
                         :notify #'frimacs-domex-show-implementation
                         "Get Implementation Summary"))
    (widget-insert "\n")
    (setq frimacs-domex-content-widget
          (widget-create 'text
                         :format "\n%v\n"
                         :value-face 'frimacs-domex-content-face
                         ""))
    (widget-setup)
    (goto-char (point-min))
    (widget-forward 1)))

;;;###autoload
(define-derived-mode frimacs-domex-mode special-mode "Frimacs Domain Explorer"
  "Major mode for the Frimacs domain explorer."
  :group 'frimacs
  (make-local-variable 'frimacs-domex-domain-widget)
  (make-local-variable 'frimacs-domex-button-widget)
  (make-local-variable 'frimacs-domex-content-widget)
  (setq font-lock-defaults (list frimacs-help-font-lock-keywords))
  (setq truncate-lines t)
  (setq buffer-read-only nil)
  (setq frimacs-menu-compile-buffer-enable nil)
  (setq frimacs-menu-compile-file-enable nil)
  (setq frimacs-menu-read-buffer-enable nil)
  (setq frimacs-menu-read-file-enable nil)
  (setq frimacs-menu-read-region-enable nil)
  (setq frimacs-menu-read-pile-enable nil))

;;;###autoload
(defun frimacs-domain-explorer ()
  "Show the Frimacs domain explorer."
  (interactive)
  (bury-buffer (get-buffer-create frimacs-domex-query-bufname))
  (let ((id frimacs-domex-ui-next-id))
    (setq frimacs-domex-ui-next-id (+ id 1))
    (frimacs-domex-prepare-buffer id)
    (let ((popup (display-buffer (format frimacs-domex-ui-bufname id))))
      (when (and popup frimacs-select-popup-windows)
        (select-window popup)))))

(provide 'frimacs-domain-explorer)

;;; frimacs-domain-explorer.el ends here
