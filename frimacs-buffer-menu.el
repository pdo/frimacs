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
;; A utility for displaying all Frimacs buffers.
;;
(defcustom frimacs-buffer-menu-bufname "*Frimacs Buffer Menu*"
  "Name of the buffer in which to display the buffer menu."
  :type 'string
  :group 'frimacs)

(defcustom frimacs-buffer-menu-startcolumn-bufprop 0
  "Starting column from which to display buffer properties."
  :type 'integer
  :group 'frimacs)

(defcustom frimacs-buffer-menu-startcolumn-bufname 3
  "Starting column from which to display buffer name."
  :type 'integer
  :group 'frimacs)

(defcustom frimacs-buffer-menu-startcolumn-bufpath 36
  "Starting column from which to display buffer path."
  :type 'integer
  :group 'frimacs)

(defface frimacs-buffer-menu-group-heading '((t :weight bold))
  "Face used for displaying Frimacs Buffer Menu group headings."
  :group 'frimacs)

(defvar frimacs-buffer-menu-invoking-buffer nil
  "Buffer from which ``frimacs-buffer-menu'' was invoked.")

(defvar frimacs-buffer-menu-startpoint-input 0
  "Starting point of Input buffer list display.")

(defvar frimacs-buffer-menu-startpoint-spad 0
  "Starting point of SPAD buffer list display.")

(defvar frimacs-buffer-menu-startpoint-help 0
  "Starting point of Help buffer list display.")

(defvar frimacs-buffer-menu-startpoint-cursor 0
  "Starting point of cursor in Frimacs Buffer Menu buffer.")

(defvar frimacs-buffer-menu-mode-map
  (let ((map (make-sparse-keymap "Frimacs")))
    (suppress-keymap map t)
    (define-key map (kbd "<tab>") 'frimacs-buffer-menu-cycle-groups-forward)
    (define-key map (kbd "TAB") 'frimacs-buffer-menu-cycle-groups-forward)
    (define-key map (kbd "<backtab>") 'frimacs-buffer-menu-cycle-groups-backward)
    (define-key map (kbd "S-<tab>") 'frimacs-buffer-menu-cycle-groups-backward)
    (define-key map (kbd "M-TAB") 'frimacs-buffer-menu-cycle-groups-backward)
    (define-key map (kbd "C-g") 'frimacs-buffer-menu-quit)
    (define-key map (kbd "q") 'frimacs-buffer-menu-quit)
    (define-key map (kbd "?") 'describe-mode)
    (define-key map (kbd "SPC") 'frimacs-buffer-menu-cycle-forward)
    (define-key map (kbd "b") 'frimacs-buffer-menu-cycle-backward)
    (define-key map (kbd "v") 'frimacs-buffer-menu-select)
    (define-key map (kbd "RET") 'frimacs-buffer-menu-select)
    (define-key map (kbd "<mouse-1>") 'frimacs-buffer-menu-mouse-select)
    (define-key map (kbd "o") 'frimacs-buffer-menu-select-other-window)
    (define-key map (kbd "K") 'frimacs-buffer-menu-kill-buffer)
    (define-key map (kbd "B") 'frimacs-buffer-menu-bury-buffer)
    map)
  "The `frimacs-buffer-menu-mode' local keymap.")

(defun frimacs-buffer-menu-quit ()
  "Quit the frimacs buffer menu."
  (interactive)
  (quit-window))

(defun frimacs-buffer-menu-get-bufname ()
  "Return name of buffer described by current line of buffer menu."
  (save-excursion
    (beginning-of-line)
    (forward-char frimacs-buffer-menu-startcolumn-bufname)
    (get-text-property (point) 'buffer-name)))

(defun frimacs-buffer-menu-kill-buffer ()
  "Kill this line's buffer."
  (interactive)
  (let ((selected-buffer (frimacs-buffer-menu-get-bufname))
        (menu-location (point)))
    (cond (selected-buffer
           (kill-buffer selected-buffer)
           (frimacs-buffer-menu-prepare-buffer)
           (switch-to-buffer frimacs-buffer-menu-bufname)
           (goto-char menu-location))
          (t
           (message "No buffer on this line")))))

(defun frimacs-buffer-menu-bury-buffer ()
  "Bury this line's buffer."
  (interactive)
  (let ((selected-buffer (frimacs-buffer-menu-get-bufname))
        (menu-location (point)))
    (cond (selected-buffer
           (bury-buffer selected-buffer)
           (frimacs-buffer-menu-prepare-buffer)
           (switch-to-buffer frimacs-buffer-menu-bufname)
           (goto-char menu-location))
          (t
           (message "No buffer on this line")))))

(defun frimacs-buffer-menu-select ()
  "Select this line's buffer in this window."
  (interactive)
  (let ((selected-buffer (frimacs-buffer-menu-get-bufname)))
    (cond (selected-buffer
           (switch-to-buffer selected-buffer)
           (kill-buffer frimacs-buffer-menu-bufname))
          (t
           (message "No buffer on this line")))))

(defun frimacs-buffer-menu-select-other-window ()
  "Display this line's buffer in another window."
  (interactive)
  (let ((selected-buffer (frimacs-buffer-menu-get-bufname)))
    (cond (selected-buffer
           (display-buffer selected-buffer '(display-buffer-reuse-window (inhibit-same-window . t))))
          (t
           (message "No buffer on this line")))))

(defun frimacs-buffer-menu-mouse-select (event)
  "Select the buffer whose line is clicked on, through EVENT."
  (interactive "e")
  (let (buffer)
    (with-current-buffer (window-buffer (posn-window (event-end event)))
      (save-excursion
        (goto-char (posn-point (event-end event)))
        (setq buffer (frimacs-buffer-menu-get-bufname))))
    (select-window (posn-window (event-end event)))
    (switch-to-buffer buffer)))

(defun frimacs-buffer-menu-cycle-forward ()
  "Move to next active line of buffer menu."
  (interactive)
  (let ((newpoint nil))
    (save-excursion
      (let ((keep-searching t))
        (while keep-searching
          (if (> (forward-line 1) 0)
              (setq keep-searching nil)
            (when (frimacs-buffer-menu-get-bufname)
              (setq keep-searching nil)
              (setq newpoint (point)))))))
    (when newpoint
      (goto-char newpoint))))

(defun frimacs-buffer-menu-cycle-backward ()
  "Move to previous active line of buffer menu."
  (interactive)
  (let ((newpoint nil))
    (save-excursion
      (let ((keep-searching t))
        (while keep-searching
          (if (< (forward-line -1) 0)
              (setq keep-searching nil)
            (when (frimacs-buffer-menu-get-bufname)
              (setq keep-searching nil)
              (setq newpoint (point)))))))
    (when newpoint
      (goto-char newpoint))))

(defun frimacs-buffer-menu-cycle-groups-forward ()
  "Move cursor to start of next buffer group."
  (interactive)
  (cond ((< (point) frimacs-buffer-menu-startpoint-input)
         (goto-char frimacs-buffer-menu-startpoint-input))
        ((< (point) frimacs-buffer-menu-startpoint-spad)
         (goto-char frimacs-buffer-menu-startpoint-spad))
        ((< (point) frimacs-buffer-menu-startpoint-help)
         (goto-char frimacs-buffer-menu-startpoint-help))
        (t
         (goto-char frimacs-buffer-menu-startpoint-input))))

(defun frimacs-buffer-menu-cycle-groups-backward ()
  "Move cursor to start of previous buffer group."
  (interactive)
  (cond ((> (point) frimacs-buffer-menu-startpoint-help)
         (goto-char frimacs-buffer-menu-startpoint-help))
        ((> (point) frimacs-buffer-menu-startpoint-spad)
         (goto-char frimacs-buffer-menu-startpoint-spad))
        ((> (point) frimacs-buffer-menu-startpoint-input)
         (goto-char frimacs-buffer-menu-startpoint-input))
        (t
         (goto-char frimacs-buffer-menu-startpoint-help))))

(defun frimacs-buffer-menu-make-truncated-name (name max-length)
  "Construct printed NAME, truncating to MAX-LENGTH if necessary."
  (let ((left-trunc-length 12))
    (cond ((or (null max-length) (<= (length name) max-length))
           name)
          (t
           (concat (substring name 0 left-trunc-length)
                   "..."
                   (substring name (- (length name)
                                      (- max-length (+ left-trunc-length 3)))))))))

(defun frimacs-buffer-menu-prepare-buffer ()
  "Setup the Frimacs Buffer Menu buffer."
  (with-current-buffer (get-buffer-create frimacs-buffer-menu-bufname)
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq standard-output (current-buffer))
    ;; Record the column where buffer names start.
    (dolist (show-type (list :input :spad :help))
      (let ((heading-startpoint (point)))
        (cond ((eql show-type :input)
               (princ "INPUT BUFFERS\n")
               (put-text-property heading-startpoint (point) 'face 'frimacs-buffer-menu-group-heading)
               (setq frimacs-buffer-menu-startpoint-input (point))
               (setq frimacs-buffer-menu-startpoint-cursor (point)))
              ((eql show-type :spad)
               (princ "SPAD BUFFERS\n")
               (put-text-property heading-startpoint (point) 'face 'frimacs-buffer-menu-group-heading)
               (setq frimacs-buffer-menu-startpoint-spad (point)))
              ((eql show-type :help)
               (princ "HELP BUFFERS\n")
               (put-text-property heading-startpoint (point) 'face 'frimacs-buffer-menu-group-heading)
               (setq frimacs-buffer-menu-startpoint-help (point)))))
      (dolist (buffer (buffer-list))
        (let (this-buffer-read-only
              this-buffer-hidden
              this-buffer-name
              this-buffer-mode
              this-buffer-filename
              name-startpoint
              name-endpoint)
          (with-current-buffer buffer
            (setq this-buffer-read-only buffer-read-only
                  this-buffer-name (buffer-name)
                  this-buffer-mode major-mode
                  this-buffer-filename (buffer-file-name))
            (setq this-buffer-hidden (eql (elt this-buffer-name 0) ?\s)))
          (when (and (or (and (eql show-type :input) (eql this-buffer-mode 'frimacs-input-mode))
                         (and (eql show-type :spad) (eql this-buffer-mode 'frimacs-spad-mode))
                         (and (eql show-type :help) (eql this-buffer-mode 'frimacs-help-mode)))
                     (not this-buffer-hidden))
            (indent-to frimacs-buffer-menu-startcolumn-bufprop)
            (princ (if (buffer-modified-p buffer) "*" " "))
            (princ (if this-buffer-read-only "%" " "))
            (indent-to frimacs-buffer-menu-startcolumn-bufname)
            (setq name-startpoint (point))
            (princ (frimacs-buffer-menu-make-truncated-name
                    this-buffer-name
                    (if (or (eql show-type :input) (eql show-type :spad))
                        (1- (- frimacs-buffer-menu-startcolumn-bufpath
                               frimacs-buffer-menu-startcolumn-bufname))
                      nil)))
            (setq name-endpoint (point))
            ;; Put the buffer name into a text property
            ;; so we don't have to extract it from the text.
            ;; This way we avoid problems with unusual buffer names.
            (put-text-property name-startpoint name-endpoint 'buffer-name this-buffer-name)
            (put-text-property name-startpoint name-endpoint 'mouse-face 'highlight)
            (put-text-property name-startpoint name-endpoint 'help-echo this-buffer-name)
            (indent-to frimacs-buffer-menu-startcolumn-bufpath 1)
            (when this-buffer-filename
              (princ (abbreviate-file-name this-buffer-filename)))
            (princ "\n"))))
      (princ "\n"))
    (frimacs-buffer-menu-mode)
    (goto-char frimacs-buffer-menu-startpoint-cursor)))

;;;###autoload
(define-derived-mode frimacs-buffer-menu-mode special-mode "Frimacs Buffer Menu"
  "Major mode for giving users quick-access to Frimacs buffers.
\\<frimacs-buffer-menu-mode-map>
\\[frimacs-buffer-menu-cycle-groups-forward] -- move to next buffer group (file, directory or scratch).
\\[frimacs-buffer-menu-cycle-groups-backward] -- move to previous buffer group (file, directory or scratch).
\\[frimacs-buffer-menu-cycle-forward] -- move to next active line.
\\[frimacs-buffer-menu-cycle-backward] -- move to previous active line.
\\[frimacs-buffer-menu-select] -- select the buffer named on the current line.
\\[frimacs-buffer-menu-mouse-select] -- select the buffer clicked on.
\\[frimacs-buffer-menu-select-other-window] -- display the buffer named on the current line in another window.
\\[frimacs-buffer-menu-kill-buffer] -- kill the buffer named on the current line.
\\[frimacs-buffer-menu-bury-buffer] -- bury the buffer named on the current line (move to bottom of list).
\\[frimacs-buffer-menu-quit] -- kill the menu buffer, return to previous buffer.
\\[describe-mode] -- describe mode."
  :group 'frimacs
  (setq truncate-lines t))

;;;###autoload
(defun frimacs-buffer-menu ()
  "Display a list of Frimacs buffers."
  (interactive)
  (setq frimacs-buffer-menu-invoking-buffer (current-buffer))
  (frimacs-buffer-menu-prepare-buffer)
  (let ((popup (display-buffer frimacs-buffer-menu-bufname '(display-buffer-same-window) t)))
    (when (and popup frimacs-select-popup-windows)
      (select-window popup))))

(provide 'frimacs-buffer-menu)
