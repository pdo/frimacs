;;; frimacs-selector.el --- Part of frimacs  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Paul Onions

;; Author: Paul Onions <paul.onions@acm.org>
;; Keywords: FriCAS, computer algebra, extensions, tools
;; URL: https://github.com/pdo/frimacs

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: MIT

;; This file is free software, see the LICENCE file in this directory
;; for more information.

;;; Commentary:

;; A utility for quickly selecting a buffer from the Frimacs environment.
;;
;; Inspired by (and code borrowed from) the Slime selector function.

;;; Code:

(require 'cl-lib)
(require 'frimacs-buffer-menu)

(defcustom frimacs-selector-help-buffer-name "*Frimacs Selector Help*"
  "Frimacs selector help buffer name."
  :type 'string
  :group 'frimacs)

(defvar frimacs-selector-functions nil
  "List of functions for the `frimacs-selector' function.

Each element is a list (KEY DESCRIPTION FUNCTION), where
DESCRIPTION is a one-line description of the command.")

;;;###autoload
(defun frimacs-selector ()
  "Invoke a selector function by entering a single character.

The user is prompted for a single character indicating the
desired function.  The `?' character describes the available
functions.  See `define-frimacs-selector-function' for defining new
functions."
  (interactive)
  (message "Select [%s]: "
           (apply #'string (mapcar #'car frimacs-selector-functions)))
  (let* ((ch (save-window-excursion
               (select-window (minibuffer-window))
               (read-char)))
         (selector-entry (assq ch frimacs-selector-functions)))
    (cond ((null selector-entry)
           (message "No function for character: ?\\%c" ch)
           (ding)
           (sleep-for 1)
           (discard-input)
           (frimacs-selector))
          (t
           (funcall (cl-third selector-entry))))))

(defmacro define-frimacs-selector-function (key description &rest body)
  "Define a new `frimacs-selector' function.

KEY is the key the user will enter to choose this function.
DESCRIPTION is a one-line sentence describing the function.
BODY is a series of forms which are evaluated when the command
is chosen."
  (let ((function `(lambda ()
                     (progn ,@body))))
    `(progn
       (setq frimacs-selector-functions
             (cons (list ,key ,description ,function)
                   (assq-delete-all ,key frimacs-selector-functions))))))

(define-frimacs-selector-function ?? "Frimacs selector help"
  (ignore-errors (kill-buffer frimacs-selector-help-buffer-name))
  (with-current-buffer (get-buffer-create frimacs-selector-help-buffer-name)
    (insert "Selector Methods:\n\n")
    (dolist (entry frimacs-selector-functions)
      (insert (format "%c:\t%s\n" (cl-first entry) (cl-second entry))))
    (help-mode)
    (display-buffer (current-buffer) nil t)
    (shrink-window-if-larger-than-buffer
     (get-buffer-window (current-buffer))))
  (frimacs-selector)
  (current-buffer))

(define-frimacs-selector-function ?q "Quit selector"
  (let ((help-buffer (get-buffer frimacs-selector-help-buffer-name)))
    (when help-buffer
      (delete-window (get-buffer-window help-buffer))
      (kill-buffer help-buffer))))

(define-frimacs-selector-function ?r "Switch to FriCAS REPL buffer"
  (let ((buf (get-buffer frimacs-process-repl-buffer-name)))
    (if buf
        (switch-to-buffer buf)
      (message "FriCAS REPL not available, try M-x run-fricas"))))

(defun frimacs-find-recent-buffer (mode)
  "Find a recently used buffer of the given MODE."
  (let ((bufs (buffer-list (window-frame nil)))
        (buf nil))
    (while (and bufs (null buf))
      (save-excursion
        (with-current-buffer (cl-first bufs)
          (when (eql major-mode mode)
            (setq buf (current-buffer)))))
      (setq bufs (cl-rest bufs)))
    buf))

(define-frimacs-selector-function ?i "Switch to most recent Frimacs Input buffer"
  (let ((buf (frimacs-find-recent-buffer 'frimacs-input-mode)))
    (if buf
        (switch-to-buffer buf)
      (message "No Frimacs Input buffer found"))))

(define-frimacs-selector-function ?s "Switch to most recent Frimacs SPAD buffer"
  (let ((buf (frimacs-find-recent-buffer 'frimacs-spad-mode)))
    (if buf
        (switch-to-buffer buf)
      (message "No Frimacs SPAD buffer found"))))

(define-frimacs-selector-function ?b "List Frimacs buffers"
  (frimacs-buffer-menu))

(define-frimacs-selector-function ?a "List all Frimacs buffers"
  (frimacs-buffer-menu))

(provide 'frimacs-selector)

;;; frimacs-selector.el ends here
