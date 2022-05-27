;;; frimacs-extras.el --- Extra stuff for Frimacs

;; Copyright (C) 2022 Paul Onions

;; Author: Paul Onions <paul.onions@acm.org>
;; Keywords: FriCAS, computer algebra, extensions, tools
;; URL: https://github.com/pdo/frimacs

;;; Commentary:

;; If you are loading frimacs directly from a local clone of its
;; source repository, then you can load this file from your ~/.emacs
;; after loading frimacs.el.  For example:-
;;
;;  (load-file "<frimacs-local-repo>/frimacs.el")
;;  (load-file "<frimacs-local-repo>/extras/frimacs-extras.el")
;;
;; On the other hand, if you are using the Emacs package system to
;; install frimacs and ob-fricas, then there is no need to use this
;; file.

;;; Code

(require 'frimacs)

;; Add this directory to the load path
(add-to-list 'load-path (file-name-directory (or load-file-name (buffer-file-name))))

;; Load ob-fricas
(require 'ob-fricas)

;; Confirm we're loaded
(provide 'frimacs-extras)

;;; frimacs-extras.el ends here
