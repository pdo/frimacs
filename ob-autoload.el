;;; ob-autoload.el --- Part of frimacs  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Paul Onions

;; Author: Paul Onions <paul.onions@acm.org>
;; Keywords: FriCAS, computer algebra, extensions, tools
;; URL: https://github.com/pdo/frimacs

;; This file is not part of GNU Emacs.

;; This file is free software, see the LICENCE file in this directory
;; for copying terms.

;;; Commentary:

;; Make `ob-fricas' automatically load so that org-babel knows about
;; the Frimacs backend.

;;; Code:

;;;###autoload
(require 'ob-fricas)

;;; ob-autoload.el ends here
