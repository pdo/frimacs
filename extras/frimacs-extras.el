;;; frimacs-extras.el --- Extra stuff for Frimacs

;; Copyright (C) 2022 Paul Onions

;; Author: Paul Onions <paul.onions@acm.org>
;; Keywords: FriCAS, computer algebra, extensions, tools
;; URL: https://github.com/pdo/frimacs

;;; Commentary:

;; This directory contains extra capabilities that may be useful to
;; you when using FriCAS inside Emacs with frimacs.  Like frimacs
;; itself they are all available as packages (from MELPA) to be
;; installed as desired, but they can also be installed directly from
;; a local clone of this repository without using the Emacs packaging
;; system.
;;
;; Loading all extensions directly from a local clone of the source
;; repository can be done in your ~/.emacs file after loading frimacs
;; itself.  For example:-
;;
;;  (load-file "<frimacs-local-repo>/frimacs.el")
;;  (load-file "<frimacs-local-repo>/extras/frimacs-extras.el")
;;
;; On the other hand, if you are using the Emacs package system to
;; install frimacs and (possibly) its extensions, then there is no
;; need to use this file.  However, you may still want to include some
;; of the forms used below into your ~/.emacs configuration.

;;; Code

;; Ensure frimacs is loaded
(require 'frimacs)

;; Add this directory to the load path
(add-to-list 'load-path (file-name-directory (or load-file-name (buffer-file-name))))

;; Load ob-fricas if/when org-babel is available
(with-eval-after-load 'ob
  (require 'ob-fricas))

;; Load company-frimacs if/when company-mode is available
(with-eval-after-load 'company
  (require 'company-frimacs))

;; A useful shortcut
(defalias 'run-fricas #'frimacs-run-fricas)

;; Confirm we're loaded
(provide 'frimacs-extras)

;;; frimacs-extras.el ends here
