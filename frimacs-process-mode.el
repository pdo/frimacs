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
;; A mode for launching and interacting with a running FriCAS system.
;;
(require 'cl-lib)
(require 'frimacs-base)
(require 'frimacs-help-mode)
(require 'comint)

(defcustom frimacs-process-repl-buffer-name "*FriCAS REPL*"
  "Default `frimacs-process-mode' buffer name.

Must begin and end with an asterisk."
  :type 'string
  :group 'frimacs)

(defcustom frimacs-process-program "fricas -noht"
  "Command line to invoke FriCAS."
  :type 'string
  :group 'frimacs)

(defcustom frimacs-process-prompt-regexp "^.*(\\([[:digit:]]+\\|NIL\\)) ->\\|^->"
  "Regexp to recognize prompts from FriCAS."
  :type 'regexp
  :group 'frimacs)

(defcustom frimacs-process-break-prompt-regexp "^0]"
  "Regexp to recognize a Lisp BREAK prompt."
  :type 'regexp
  :group 'frimacs)

(defcustom frimacs-process-preamble ""
  "Initial commands to push to FriCAS."
  :type 'string
  :group 'frimacs)

(defcustom frimacs-process-compile-file-result-directory ""
  "Directory in which to place compiled object files.

Only used when variable
`frimacs-process-compile-file-use-result-directory' is non-NIL."
  :type 'string
  :group 'frimacs)

(defcustom frimacs-process-compile-file-use-result-directory nil
  "Non-nil to place compilation results in a central directory.

When non-nil place compiled object files in the directory named
by variable `frimacs-process-compile-file-result-directory',
otherwise they will be placed in the same directory as the source
file."
  :type 'boolean
  :group 'frimacs)

(defcustom frimacs-process-compile-file-buffer-name "*FriCAS Compilation*"
  "A buffer in which to echo compiler output."
  :type 'string
  :group 'frimacs)

(defcustom frimacs-process-query-buffer-name "*FriCAS Query*"
  "FriCAS query result buffer name."
  :type 'string
  :group 'frimacs)

(defcustom frimacs-process-popup-buffer-name-root "FriCAS"
  "Starting text of name used by popup query buffers."
  :type 'string
  :group 'frimacs)

(defcustom frimacs-process-webview-url "http://fricas.github.io/api/"
  "The base URL for SPAD constructor documentation."
  :type 'string
  :group 'frimacs)

(defcustom frimacs-process-spad-source-dirs
  '("./" "/usr/local/fricas/lib/fricas/target/i686-apple-darwin14.1.0/src/algebra/")
  "A list of directories in which to search for SPAD source code."
  :type 'list
  :group 'frimacs)

(defcustom frimacs-process-enable-pretty-print nil
  "Enable pretty printing of FriCAS output.
You need `mml2svg' binary inside your PATH to use it.
You can install it with command:
sudo npm install --global mathjax-node-cli"
  :type 'boolean
  :group 'frimacs)

(defcustom frimacs-process-pretty-print-separator "\n"
  "Separate pretty printed output from normal one."
  :type 'string
  :group 'frimacs)

(defcustom frimacs-process-pretty-print-mml2svg-binary-path "mml2svg"
  "Path to mml2svg binary for pretty printing.
No need to change it if `mml2svg' inside your PATH."
  :type 'string
  :group 'frimacs)

(defcustom frimacs-process-show-svg nil
  "Enable showing svg images."
  :type 'boolean
  :group 'frimacs)

(defcustom frimacs-process-embed-gnu-draw nil
  "Enable embedded gnu draw images by gnuplot."
  :type 'boolean
  :group 'frimacs)

(defcustom frimacs-process-gnuplot-binary-path "gnuplot"
  "Path to gnuplot for embedding gnu draw images."
  :type 'string
  :group 'frimacs)

(defvar-local frimacs-process--processed-plots nil)

(defvar-local frimacs-process--plots-queue nil)

(defvar-local frimacs-process--processed-svg nil)

(defvar-local frimacs-process--svg-queue nil)

(defvar frimacs-process-mode-hook nil
  "Hook for customizing `frimacs-process-mode'.")

(defvar frimacs-process-mode-syntax-table
  (copy-syntax-table frimacs-common-syntax-table)
  "The `frimacs-process-mode' syntax table.")

(defvar frimacs-process-mode-map
  (let ((map (copy-keymap frimacs-common-keymap)))
    (set-keymap-parent map comint-mode-map)
    (define-key map (kbd "C-c C-c") 'comint-interrupt-subjob)
    map)
  "Keymap for `frimacs-process-mode'.")

(defvar frimacs-process-not-running-message
  "FriCAS not running, try M-x run-fricas")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility macros
;;
(defmacro with-frimacs-process-query-buffer (&rest body)
  "Set `current-buffer' to a query result buffer, with dynamic extent.

Use this instead of `with-temp-buffer' so that the buffer can be
easily examined when things go wrong.  The buffer switched to is
actually the buffer called `frimacs-process-query-buffer-name',
which is cleared when the dynamic extent of this form is entered,
before the BODY forms are evaluated.

IMPORTANT NOTE: Unlike `with-temp-buffer', this means that nested
calls are NOT ALLOWED."
  `(with-current-buffer (get-buffer-create frimacs-process-query-buffer-name)
     (fundamental-mode)
     (erase-buffer)
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Command utility functions
;;
(defun frimacs-process-insert-command (command)
  "Send COMMAND, a string, to FriCAS.

The COMMAND and its output are inserted in the FriCAS REPL buffer
at the current `process-mark', which may be before the end of the
buffer if the user is part-way through editing the next command."
  (with-current-buffer frimacs-process-repl-buffer-name
    (let ((proc (get-buffer-process (current-buffer)))
          (command-text command)
          (pending-text ""))
      ;; Remove newlines from end of command string
      (while (and (> (length command-text) 0)
                  (char-equal ?\n (aref command-text (1- (length command-text)))))
        (setq command-text (substring command-text 0 (1- (length command-text)))))
      ;; Contrary to what it says in the documentation of `comint-send-input',
      ;; calling it sends _all_ text from the process mark to the _end_ of
      ;; the buffer to the process.  So we need to temporarily remove any
      ;; text the user is currently typing at the end of the buffer before
      ;; calling `comint-send-input', then restore it afterwards.
      (when (> (point-max) (process-mark proc))
        (setq pending-text (delete-and-extract-region (process-mark proc) (point-max))))
      (goto-char (process-mark proc))
      (insert command-text)
      (comint-send-input nil t)
      (insert pending-text))))

;;;###autoload
(defun frimacs-process-redirect-send-command (command output-buffer &optional display echo-cmd echo-result
                                                    op-cmd op-prompt)
  "Send COMMAND to FriCAS and put result in OUTPUT-BUFFER.

If DISPLAY is non-nil then display the output buffer.

If ECHO-CMD is non-nil then copy the command to the FriCAS REPL
buffer, and if ECHO-RESULT is non-nil then also copy the result.

If OP-CMD is non-nil then include command in output to
OUTPUT-BUFFER.  If OP-PROMPT is non-nil then also include
prompt in output to OUTPUT-BUFFER."
  (with-current-buffer frimacs-process-repl-buffer-name
    (let ((proc (get-buffer-process (current-buffer))))
      (when op-prompt
        (let* ((real-bol (+ (point) (save-excursion (skip-chars-backward "^\n"))))
               (prompt (buffer-substring-no-properties real-bol (point))))
          (with-current-buffer output-buffer
            (insert prompt))))
      (when op-cmd
        (with-current-buffer output-buffer
          (insert command "\n")))
      (when echo-cmd
        (goto-char (process-mark proc))
        (insert-before-markers command "\n"))
      (comint-redirect-send-command command output-buffer echo-result (not display))
      (sit-for 0.1)  ; this seems to help us capture all output reliably
      (while (not comint-redirect-completed)
        (accept-process-output proc)
        (redisplay))
      (frimacs-process-sanitize-redirected-output output-buffer)  ; clean up output text
      (when (and echo-cmd (not echo-result))  ; get prompt back
        (frimacs-process-insert-command "")))))

(defun frimacs-process-sanitize-redirected-output (buffer)
  "Clean up redirected command's output text in BUFFER.

Remove 'erase' characters and the characters they erase from all
lines of output.  This is necessary when FriCAS is run with
'sman' enabled."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (1+ (point-min)))
      (let ((done nil))
        (while (and (< (point) (point-max))
                    (not done))
          (cond ((eql (char-after) ?\b)
                 (delete-char +1)
                 (when (> (point) (point-min))
                   (delete-char -1)))
                (t
                 (goto-char (1+ (point))))))))))

(defun frimacs-process-get-old-input ()
  "A Frimacs-specific replacement for `comint-get-old-input'.

Return the concatenation of the current line and all subsequent
continuation-lines (underscores escape new lines)."
  (comint-bol)
  (frimacs-get-rest-of-line))

;;;###autoload
(defun frimacs-process-find-constructor-source (name-or-abbrev)
  "Attempt to find the SPAD source for the given NAME-OR-ABBREV constructor.

Invoke a grep `shell-command' looking in the directories specified by
`frimacs-process-spad-source-dirs'.  Return a list containing
a filename and a line number."
  (let ((filename "")
	(line-number 0))
    (dolist (dir frimacs-process-spad-source-dirs)
      (unless (> line-number 0)
	(let ((grep-out (with-temp-buffer
			  (shell-command
			   (concat "grep -n ')abbrev .*\\<" name-or-abbrev "\\>' " dir "*.spad")
			   t nil)
			  (buffer-substring-no-properties (point-min) (point-max)))))
	  (when (> (length grep-out) 0)
	    (string-match "\\(.+\\):\\(.+\\):" grep-out)
	    (setq filename (substring grep-out 0 (match-end 1)))
	    (setq line-number (string-to-number (substring grep-out (1+ (match-end 1)) (match-end 2))))))))
    (when (and (> (length filename) 0) (> line-number 0))
      (list filename line-number))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Directory tracking -- track Frimacs's notion of ``current directory''
;;
(defun frimacs-process-force-cd-update (&optional no-msg)
  "Force update of buffer-local variable `default-directory'.

Also return the directory as a string.  If NO-MSG is non-nil then
don't display the `default-directory' in a message."
  (interactive)
  (let ((dirname nil))
    (with-frimacs-process-query-buffer
      (frimacs-process-redirect-send-command ")cd ." (current-buffer))
      (goto-char (point-min))
      (let ((dirname-start (search-forward-regexp "default directory is[[:space:]]+" nil t))
            (dirname-end (progn
                           (search-forward-regexp "[[:blank:]]*$" nil t)
                           (match-beginning 0))))
        (when (and dirname-start dirname-end)
          (setq dirname (expand-file-name (file-name-as-directory (buffer-substring dirname-start dirname-end)))))
        (frimacs-debug-message (format "CD: %S %S %S" dirname-start dirname-end dirname))))
    (when dirname
      (with-current-buffer frimacs-process-repl-buffer-name
        (setq default-directory dirname)
        (unless no-msg
          (message (format "Current directory now: %s" dirname)))))
    dirname))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evaluating a string
;;
(defun frimacs-process-eval-string (str &optional no-display)
  "Evaluate the given string, STR, in FriCAS.

If NO-DISPLAY is non-nil don't display the FriCAS buffer."
  (if (null (get-buffer frimacs-process-repl-buffer-name))
      (message frimacs-process-not-running-message)
    (unless no-display
      (let ((win (display-buffer frimacs-process-repl-buffer-name nil t)))
        (when frimacs-select-displayed-repl
          (select-window win))))
    (frimacs-process-insert-command str)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evaluating a region
;;
;;;###autoload
(defun frimacs-process-eval-region (start end &optional no-display)
  "Evaluate the given region (between START and END) in FriCAS.

If NO-DISPLAY is non-nil don't display the FriCAS buffer."
  (interactive "r\nP")
  (frimacs-process-eval-string (buffer-substring-no-properties start end) no-display))

;;;###autoload
(defun frimacs-process-read-region (start end &optional no-display)
  "Copy region between START and END into a temporary file and )read it.

If NO-DISPLAY is non-nil don't display the FriCAS buffer."
  (interactive "r\nP")
  (if (null (get-buffer frimacs-process-repl-buffer-name))
      (message frimacs-process-not-running-message)
    (let ((tmp-filename (make-temp-file "frimacs" nil ".input")))
      (write-region start end tmp-filename)
      (unless no-display
        (let ((win (display-buffer frimacs-process-repl-buffer-name nil t)))
          (when frimacs-select-displayed-repl
            (select-window win))))
      (frimacs-process-insert-command (format ")read %s" tmp-filename)))))

(defun frimacs-process-read-pile (&optional no-display)
  "Read the current pile into FriCAS.

If NO-DISPLAY is non-nil don't display the FriCAS buffer."
  (interactive "P")
  (let ((start (point))
        (end (point)))
    (save-excursion
      (beginning-of-line)
      (while (and (not (eql (point) (point-min)))
                  (member (char-after) (list 9 10 12 13 32)))
        (forward-line -1))
      (setq start (point)))
    (save-excursion
      (beginning-of-line)
      (forward-line +1)
      (while (and (not (eql (point) (point-max)))
                  (member (char-after) (list 9 10 12 13 32)))
        (forward-line +1))
      (setq end (point)))
    (frimacs-flash-region start end)
    (frimacs-process-read-region start end no-display)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reading and compiling files
;;
;;;###autoload
(defun frimacs-process-read-file (filename &optional no-display)
  "Tell FriCAS to read FILENAME.

If NO-DISPLAY is nil then also display the FriCAS REPL buffer."
  (interactive (list (read-file-name "Read file: " nil nil nil (file-name-nondirectory (or (buffer-file-name) "")))
                     current-prefix-arg))
  (if (not (get-buffer frimacs-process-repl-buffer-name))
      (message frimacs-process-not-running-message)
    (progn
      (unless no-display
        (let ((win (display-buffer frimacs-process-repl-buffer-name nil t)))
          (when frimacs-select-displayed-repl
            (select-window win))))
      (frimacs-process-insert-command (format ")read %s" (expand-file-name filename))))))

;;;###autoload
(defun frimacs-process-read-buffer (&optional no-display)
  "Read the current buffer into FriCAS.

If NO-DISPLAY is nil then also display the FriCAS REPL buffer."
  (interactive "P")
  (let ((file (if (and (buffer-file-name)
                       (not (buffer-modified-p)))
                  (buffer-file-name)
                (let ((tmp-file (make-temp-file "frimacs" nil ".input")))
                  (write-region (point-min) (point-max) tmp-file)
                  tmp-file))))
    (frimacs-process-read-file file no-display)))

;;;###autoload
(defun frimacs-process-compile-file (filename &optional no-display)
  "Tell FriCAS to compile FILENAME.

If NO-DISPLAY is nil then display the FriCAS compilation results
buffer, otherwise do not display it."
  (interactive (list (read-file-name "Compile file: " nil nil nil (file-name-nondirectory (or (buffer-file-name) "")))
                     current-prefix-arg))
  (if (not (get-buffer frimacs-process-repl-buffer-name))
      (message frimacs-process-not-running-message)
    (with-current-buffer frimacs-process-repl-buffer-name
      (let ((current-dir (frimacs-process-force-cd-update t))
            (result-dir (if frimacs-process-compile-file-use-result-directory
                            (file-name-as-directory (expand-file-name frimacs-process-compile-file-result-directory))
                          (file-name-directory (expand-file-name filename)))))
        (with-current-buffer (get-buffer-create frimacs-process-compile-file-buffer-name)
          (setq buffer-read-only nil)
          (erase-buffer)
          (frimacs-help-mode)
          (unless no-display
            (display-buffer frimacs-process-compile-file-buffer-name nil t)
            (redisplay t))
          (frimacs-process-redirect-send-command (format ")cd %s" result-dir) (current-buffer) (not no-display))
          (frimacs-process-redirect-send-command (format ")compile %s" (expand-file-name filename)) (current-buffer) (not no-display))
          (frimacs-process-redirect-send-command (format ")cd %s" current-dir) (current-buffer) (not no-display))
          (set-buffer-modified-p nil)
          (setq buffer-read-only t))))
      (when (and frimacs-select-displayed-repl (not no-display))
        (select-window (display-buffer frimacs-process-compile-file-buffer-name nil t)))))

;;;###autoload
(defun frimacs-process-compile-buffer (&optional no-display)
  "Compile the current buffer in FriCAS.

If NO-DISPLAY is nil then display the FriCAS compilation results
buffer, otherwise do not display it."
  (interactive "P")
  (let ((file (if (and (buffer-file-name)
                       (not (buffer-modified-p)))
                  (buffer-file-name)
                (let ((tmp-file (make-temp-file "frimacs" nil ".spad")))
                  (write-region (point-min) (point-max) tmp-file)
                  tmp-file))))
    (frimacs-process-compile-file file no-display)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Browsing/inspection utility functions
;;
(defun frimacs-process-package-name (name-or-abbrev)
  "Convert NAME-OR-ABBREV to a package name, if possible."
  (let ((rslt (assoc name-or-abbrev frimacs-standard-package-info)))
    (if rslt
        (cdr rslt)
      name-or-abbrev)))

(defun frimacs-process-package-abbrev (name-or-abbrev)
  "Convert NAME-OR-ABBREV to a package abbreviation, if possible."
  (let ((rslt (rassoc name-or-abbrev frimacs-standard-package-info)))
    (if rslt
        (car rslt)
      name-or-abbrev)))

(defun frimacs-process-domain-name (name-or-abbrev)
  "Convert NAME-OR-ABBREV to a domain name, if possible."
  (let ((rslt (assoc name-or-abbrev frimacs-standard-domain-info)))
    (if rslt
        (cdr rslt)
      name-or-abbrev)))

(defun frimacs-process-domain-abbrev (name-or-abbrev)
  "Convert NAME-OR-ABBREV to a domain abbreviation, if possible."
  (let ((rslt (rassoc name-or-abbrev frimacs-standard-domain-info)))
    (if rslt
        (car rslt)
      name-or-abbrev)))

(defun frimacs-process-category-name (name-or-abbrev)
  "Convert NAME-OR-ABBREV to a category name, if possible."
  (let ((rslt (assoc name-or-abbrev frimacs-standard-category-info)))
    (if rslt
        (cdr rslt)
      name-or-abbrev)))

(defun frimacs-process-category-abbrev (name-or-abbrev)
  "Convert NAME-OR-ABBREV to a category abbreviation, if possible."
  (let ((rslt (rassoc name-or-abbrev frimacs-standard-category-info)))
    (if rslt
        (car rslt)
      name-or-abbrev)))

(defun frimacs-process-constructor-name (name-or-abbrev)
  "Convert NAME-OR-ABBREV to a constructor name, if possible."
  (let ((rslt (or (assoc name-or-abbrev frimacs-standard-package-info)
                  (assoc name-or-abbrev frimacs-standard-domain-info)
                  (assoc name-or-abbrev frimacs-standard-category-info))))
    (if rslt
        (cdr rslt)
      name-or-abbrev)))

(defun frimacs-process-constructor-abbrev (name-or-abbrev)
  "Convert NAME-OR-ABBREV to a constructor abbreviation, if possible."
  (let ((rslt (or (rassoc name-or-abbrev frimacs-standard-package-info)
                  (rassoc name-or-abbrev frimacs-standard-domain-info)
                  (rassoc name-or-abbrev frimacs-standard-category-info))))
    (if rslt
        (car rslt)
      name-or-abbrev)))

(defun frimacs-process-verify-package-name-or-abbrev (name-or-abbrev)
  "Return package name if NAME-OR-ABBREV is valid, or nil otherwise."
  (let ((fquery (assoc name-or-abbrev frimacs-standard-package-info))
        (rquery (rassoc name-or-abbrev frimacs-standard-package-info)))
    (or (cdr fquery) (cdr rquery))))

(defun frimacs-process-verify-domain-name-or-abbrev (name-or-abbrev)
  "Return domain name if NAME-OR-ABBREV is valid, or nil otherwise."
  (let ((fquery (assoc name-or-abbrev frimacs-standard-domain-info))
        (rquery (rassoc name-or-abbrev frimacs-standard-domain-info)))
    (or (cdr fquery) (cdr rquery))))

(defun frimacs-process-verify-category-name-or-abbrev (name-or-abbrev)
  "Return category name if NAME-OR-ABBREV is valid, or nil otherwise."
  (let ((fquery (assoc name-or-abbrev frimacs-standard-category-info))
        (rquery (rassoc name-or-abbrev frimacs-standard-category-info)))
    (or (cdr fquery) (cdr rquery))))

(defun frimacs-process-verify-constructor-name-or-abbrev (name-or-abbrev)
  "Return constructor name if NAME-OR-ABBREV is valid, or nil otherwise."
  (or (frimacs-process-verify-package-name-or-abbrev name-or-abbrev)
      (frimacs-process-verify-domain-name-or-abbrev name-or-abbrev)
      (frimacs-process-verify-category-name-or-abbrev name-or-abbrev)))

(defun frimacs-process-verify-operation-name (name)
  "Verify NAME is a valid operation name."
  (car (member name frimacs-standard-operation-info)))

(defun frimacs-process-constructor-type (name-or-abbrev)
  "Get constructor type of NAME-OR-ABBREV."
  (cond ((member name-or-abbrev frimacs-standard-package-names)
         (cons :package :name))
        ((member name-or-abbrev frimacs-standard-package-abbreviations)
         (cons :package :abbrev))
        ((member name-or-abbrev frimacs-standard-domain-names)
         (cons :domain :name))
        ((member name-or-abbrev frimacs-standard-domain-abbreviations)
         (cons :domain :abbrev))
        ((member name-or-abbrev frimacs-standard-category-names)
         (cons :category :name))
        ((member name-or-abbrev frimacs-standard-category-abbreviations)
         (cons :category :abbrev))
        (t
         (cons :constructor :unknown))))

(defun frimacs-process-constructor-buffer-name (name-or-abbrev)
  "Generate help buffer name for constructor NAME-OR-ABBREV."
  (let ((ctype (car (frimacs-process-constructor-type name-or-abbrev))))
    (format "*%s %s: %s*"
            frimacs-process-popup-buffer-name-root
            (capitalize (cl-subseq (symbol-name ctype) 1))
            (cond ((eq ctype :package)
                   (frimacs-process-package-name name-or-abbrev))
                  ((eq ctype :domain)
                   (frimacs-process-domain-name name-or-abbrev))
                  ((eq ctype :category)
                   (frimacs-process-category-name name-or-abbrev))
                  (t
                   name-or-abbrev)))))

(defun frimacs-process-operation-buffer-name (operation-name)
  "Generate help buffer name for given OPERATION-NAME."
  (format "*%s %s: %s*"
          frimacs-process-popup-buffer-name-root
          "Operation"
          operation-name))

(defun frimacs-process-display-thing ()
  "Display help buffer for thing at point."
  (interactive)
  (let ((name (thing-at-point 'word)))
    (if (not (get-buffer frimacs-process-repl-buffer-name))
        (message frimacs-process-not-running-message)
      (unless (equal "" name)
        (cond ((member name frimacs-standard-constructor-names-and-abbreviations)
               (frimacs-process-show-constructor name))
              (t
               (frimacs-process-display-operation name)))))))

(defvar frimacs-process-clickable-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'frimacs-process-display-thing)
    (define-key map [mouse-2] 'frimacs-process-display-thing)
    map)
  "Keymap for clickable items in a Frimacs-Help buffer.")

(defun frimacs-process-make-clickable (begin end tooltip-text)
  "Make region between BEGIN and END clickable, showing TOOLTIP-TEXT."
  (add-text-properties begin end
                       (list 'mouse-face 'highlight
                             'help-echo tooltip-text
                             'keymap frimacs-process-clickable-map
                             'follow-link 'mouse-face)))

(defun frimacs-process-make-all-clickables ()
  "Make all clickable regions in current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[[:word:]]+" nil t)
      (let* ((word (match-string-no-properties 0))
             (info (cond ((member word frimacs-standard-package-names-and-abbreviations)
                          (cons t (concat (frimacs-process-package-abbrev word) " = "
                                          (frimacs-process-package-name word) " [P]")))
                         ((member word frimacs-standard-domain-names-and-abbreviations)
                          (cons t (concat (frimacs-process-domain-abbrev word) " = "
                                          (frimacs-process-domain-name word) " [D]")))
                         ((member word frimacs-standard-category-names-and-abbreviations)
                          (cons t (concat (frimacs-process-category-abbrev word) " = "
                                          (frimacs-process-category-name word) " [C]")))
                         ((member word frimacs-standard-operation-names)
                          (cons t nil)))))
        (when (car info)
          (frimacs-process-make-clickable (match-beginning 0) (match-end 0) (cdr info)))))))

(defun frimacs-process-document-constructor (name-or-abbrev &optional force-update)
  "Construct a buffer containing documentation for NAME-OR-ABBREV.

If FORCE-UPDATE is non-nil then update any previously generated buffer."
  (if (not (get-buffer frimacs-process-repl-buffer-name))
      (progn (message frimacs-process-not-running-message) nil)
    (unless (equal "" name-or-abbrev)
      (let ((bufname (frimacs-process-constructor-buffer-name name-or-abbrev)))
        (when (or (not (get-buffer bufname)) force-update)
          (with-current-buffer (get-buffer-create bufname)
            (setq buffer-read-only nil)
            (erase-buffer)
            (frimacs-help-mode)
            (frimacs-process-redirect-send-command (format ")show %s" name-or-abbrev) (current-buffer) t nil nil)
            (frimacs-process-make-all-clickables)
            (set-buffer-modified-p nil)
            (setq buffer-read-only t)))
        (get-buffer bufname)))))

;;;###autoload
(defun frimacs-process-show-constructor (name-or-abbrev &optional force-update)
  "Show information about NAME-OR-ABBREV in a popup buffer.

Works by calling ``)show NAME-OR-ABBREV'' in FriCAS and capturing
its output.  When called interactively completion is performed
over all standard constructor names (packages, domains and
categories) and their abbreviations.

If the buffer already exists (from a previous call) then just switch
to it, unless FORCE-UPDATE is non-nil in which case the buffer is
reconstructed with another query to FriCAS.

Interactively, FORCE-UPDATE can be set with a prefix argument."
  (interactive (list (completing-read
                      "Constructor: "
                      frimacs-standard-constructor-names-and-abbreviations
                      nil 'confirm
                      (frimacs-process-verify-constructor-name-or-abbrev (thing-at-point 'word)))
                     current-prefix-arg))
  (let ((buf (frimacs-process-document-constructor name-or-abbrev force-update)))
    (when buf
      (let ((popup (display-buffer buf nil t)))
        (when (and popup frimacs-select-popup-windows)
          (select-window popup))))))

;;;###autoload
(defun frimacs-process-show-package (name-or-abbrev &optional force-update)
  "Show information about NAME-OR-ABBREV in a popup buffer.

Works by calling ``)show NAME-OR-ABBREV'' in FriCAS and capturing
its output.  When called interactively completion is performed
over all standard package names.

If the buffer already exists (from a previous call) then just switch
to it, unless FORCE-UPDATE is non-nil in which case the buffer is
reconstructed with another query to FriCAS.

Interactively, FORCE-UPDATE can be set with a prefix argument."
  (interactive (list (completing-read
                      "Package: " frimacs-standard-package-names-and-abbreviations nil 'confirm
                      (frimacs-process-verify-package-name-or-abbrev (thing-at-point 'word)))
                     current-prefix-arg))
  (frimacs-process-show-constructor name-or-abbrev force-update))

;;;###autoload
(defun frimacs-process-show-domain (name-or-abbrev &optional force-update)
  "Show information about NAME-OR-ABBREV in a popup buffer.

Works by calling ``)show NAME-OR-ABBREV'' in FriCAS and capturing
its output.  When called interactively completion is performed
over all standard domain names.

If the buffer already exists (from a previous call) then just switch
to it, unless FORCE-UPDATE is non-nil in which case the buffer is
reconstructed with another query to FriCAS.

Interactively, FORCE-UPDATE can be set with a prefix argument."
  (interactive (list (completing-read
                      "Domain: " frimacs-standard-domain-names-and-abbreviations nil 'confirm
                      (frimacs-process-verify-domain-name-or-abbrev (thing-at-point 'word)))
                     current-prefix-arg))
  (frimacs-process-show-constructor name-or-abbrev force-update))

;;;###autoload
(defun frimacs-process-show-category (name-or-abbrev &optional force-update)
  "Show information about NAME-OR-ABBREV in a popup buffer.

Works by calling ``)show NAME-OR-ABBREV'' in FriCAS and capturing
its output.  When called interactively completion is performed
over all standard category names.

If the buffer already exists (from a previous call) then just switch
to it, unless FORCE-UPDATE is non-nil in which case the buffer is
reconstructed with another query to FriCAS.

Interactively, FORCE-UPDATE can be set with a prefix argument."
  (interactive (list (completing-read
                      "Category: " frimacs-standard-category-names-and-abbreviations nil 'confirm
                      (frimacs-process-verify-category-name-or-abbrev (thing-at-point 'word)))
                     current-prefix-arg))
  (frimacs-process-show-constructor name-or-abbrev force-update))

(defun frimacs-process-document-operation (operation-name &optional force-update)
  "Create a buffer containing documentation for OPERATION-NAME.

If FORCE-UPDATE is non-nil then update any previously generated buffer."
  (if (not (get-buffer frimacs-process-repl-buffer-name))
      (progn (message frimacs-process-not-running-message) nil)
    (unless (equal "" operation-name)
      (let ((bufname (frimacs-process-operation-buffer-name operation-name)))
        (when (or (not (get-buffer bufname)) force-update)
          (with-current-buffer (get-buffer-create bufname)
            (setq buffer-read-only nil)
            (erase-buffer)
            (frimacs-help-mode)
            (frimacs-process-redirect-send-command (format ")display operation %s" operation-name) (current-buffer) t nil nil)
            (frimacs-process-make-all-clickables)
            (set-buffer-modified-p nil)
            (setq buffer-read-only t)))
        (get-buffer bufname)))))

;;;###autoload
(defun frimacs-process-display-operation (operation-name &optional force-update)
  "Show information about OPERATION-NAME in a popup buffer.

Works by calling ``)display operation OPERATION-NAME'' in FriCAS
and capturing its output.  When called interactively completion
is performed over all standard operation names.

If the buffer already exists (from a previous call) then just switch
to it, unless FORCE-UPDATE is non-nil in which case the buffer is
reconstructed with another query to FriCAS.

Interactively, FORCE-UPDATE can be set with a prefix argument."
  (interactive (list (completing-read
                      "Operation: " frimacs-standard-operation-names nil 'confirm
                      (frimacs-process-verify-operation-name (thing-at-point 'word)))
                     current-prefix-arg))
  (let ((buf (frimacs-process-document-operation operation-name force-update)))
    (when buf
      (let ((popup (display-buffer buf nil t)))
        (when (and popup frimacs-select-popup-windows)
          (select-window popup))))))

;;;###autoload
(defun frimacs-process-apropos-thing-at-point (name &optional is-constructor)
  "Show information about NAME in a popup buffer.

When called interactively NAME defaults to the word around point, and
completion is performed over all standard constructor and operation
names.

If NAME is a standard constructor name then call ``)show NAME''
in FriCAS and capture its output, otherwise assume it's an
operation name and call ``)display operation NAME'' instead.
This can be overridden by setting IS-CONSTRUCTOR non-nil, in
which case ``)show NAME'' will always be called.  Interactively
this can be done with a prefix argument."
  (interactive (list (completing-read "Apropos: " frimacs-standard-names-and-abbreviations
                                      nil 'confirm (thing-at-point 'word))
                     current-prefix-arg))
  (if (not (get-buffer frimacs-process-repl-buffer-name))
      (message frimacs-process-not-running-message)
    (unless (equal "" name)
      (cond ((or (member name frimacs-standard-constructor-names-and-abbreviations) is-constructor)
             (frimacs-process-show-constructor name t))
            (t
             (frimacs-process-display-operation name t))))))

;;;###autoload
(defun frimacs-process-webview-constructor (name-or-abbrev)
  "Show information about NAME-OR-ABBREV in a web browser.

Invokes `browse-url' on a URL made by appending the given
constructor name and .html to the base URL held in customizable
variable `frimacs-process-webview-url'."
  (interactive (list (completing-read
                      "Show web-page for constructor: " frimacs-standard-constructor-names-and-abbreviations nil 'confirm
                      (frimacs-process-verify-constructor-name-or-abbrev (thing-at-point 'word)))))
  (let ((url (concat frimacs-process-webview-url
                     (frimacs-process-constructor-name name-or-abbrev)
                      ".html")))
    (browse-url url)))

;;;###autoload
(defun frimacs-process-edit-constructor-source (name-or-abbrev)
  "Open the SPAD source file containing NAME-OR-ABBREV."
  (interactive (list (completing-read
                      "Find source for constructor: "
                      frimacs-standard-constructor-names-and-abbreviations
                      nil 'confirm
                      (frimacs-process-verify-constructor-name-or-abbrev (thing-at-point 'word)))))
  (let ((location (frimacs-process-find-constructor-source name-or-abbrev)))
    (if location
	(let ((buf (find-file (cl-first location))))
	  (switch-to-buffer buf)
          (goto-char (point-min))
          (forward-line (cl-second location)))
      (message "Source not found"))))

;;;###autoload
(defun frimacs-process-start-hyperdoc ()
  "Start the HyperDoc GUI from within FriCAS."
  (interactive)
  (if (null (get-buffer frimacs-process-repl-buffer-name))
      (message frimacs-process-not-running-message)
    (frimacs-process-insert-command ")hd")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-completion functions
;;
(defun frimacs-process-list-filenames (dir &optional filter)
  "List all filenames in DIR.

Optionally FILTER may be set to `:dirs' or `:files'."
  (with-current-buffer frimacs-process-repl-buffer-name
    (let* ((absolute-dir (cond ((null dir)
                                default-directory)
                               ((not (file-name-absolute-p dir))
                                (concat default-directory dir))
                               (t
                                dir)))
           (dir-files (directory-files absolute-dir dir))
           (subdirs nil)
           (subfiles nil))
      (dolist (file dir-files)
        (if (file-directory-p file)
            (push (file-name-as-directory (file-name-nondirectory file)) subdirs)
          (push (file-name-nondirectory file) subfiles)))
      (cond ((eql filter :dirs)
             subdirs)
            ((eql filter :files)
             subfiles)
            (t
             (append subdirs subfiles))))))

(defun frimacs-process-complete-command-filename (&optional filter)
  "Complete the symbol at point as a filename.

Optionally FILTER may be set to `:dirs' or `:files'."
  (let ((partial-start nil)
        (partial-end nil)
        (line-end nil))
    (save-excursion
      (setq partial-end (point))
      (end-of-line)
      (setq line-end (point))
      (beginning-of-line)
      (setq partial-start (search-forward-regexp ")[[:word:]]+[[:blank:]]+" line-end t))
      (when partial-start
        (when (> partial-start partial-end)
          (setq partial-start partial-end))
        (let* ((partial (buffer-substring-no-properties partial-start partial-end))
               (dir-path (file-name-directory partial))
               (file-prefix (file-name-nondirectory partial))
               (partial-split (- partial-end (length file-prefix))))
          (list partial-split
                partial-end
                (frimacs-process-list-filenames dir-path filter)))))))

(defun frimacs-process-complete-command-line ()
  "Attempt to complete a FriCAS command (e.g. \")cd <dirname>\")."
  (let ((filter nil))
    (save-excursion
      (beginning-of-line)
      (setq filter (cond ((looking-at "[[:blank:]]*)cd[[:blank:]]+")
                          :dirs)
                         ((looking-at "[[:blank:]]*)read[[:blank:]]+")
                          :all)
                         ((looking-at "[[:blank:]]*)compile[[:blank:]]+")
                          :all)
                         ((looking-at "[[:blank:]]*)library[[:blank:]]+")
                          :all)
                         ((looking-at "[[:blank:]]*)edit[[:blank:]]+")
                          :all))))
    (and filter (frimacs-process-complete-command-filename filter))))

(defun frimacs-process-complete-symbol ()
  "Attempt to complete the FriCAS symbol at point."
  (and (looking-back "[[:word:]]+" nil t)
       (list (match-beginning 0)
             (match-end 0)
             frimacs-standard-names-and-abbreviations)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indenting functions
;;
(defun frimacs-process-is-command-line ()
  "Return non-nil if current line is a FriCAS command line."
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:blank:]]*)[[:word:]]+[[:blank:]]+")))

(defun frimacs-process-interactive-complete ()
  "Use `company-complete' if available to complete symbol at point."
  (interactive)
  (if (and (boundp 'company-mode) company-mode)
      (company-complete)
    (complete-symbol nil)))

(defun frimacs-process-indent-line ()
  "Indent current line."
  (if (or (frimacs-process-is-command-line)
          (eql (char-syntax (char-before)) ?w))
      (frimacs-process-interactive-complete)
    (indent-relative-first-indent-point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; frimacs-process-mode -- derived from COMINT mode
;;
(defvar frimacs-process-package-face  'frimacs-package-name)
(defvar frimacs-process-domain-face   'frimacs-domain-name)
(defvar frimacs-process-category-face 'frimacs-category-name)

(defvar frimacs-process-font-lock-keywords
  (list (cons frimacs-standard-package-names-regexp          'frimacs-process-package-face)
        (cons frimacs-standard-package-abbreviations-regexp  'frimacs-process-package-face)
        (cons frimacs-standard-domain-names-regexp           'frimacs-process-domain-face)
        (cons frimacs-standard-domain-abbreviations-regexp   'frimacs-process-domain-face)
        (cons frimacs-standard-category-names-regexp         'frimacs-process-category-face)
        (cons frimacs-standard-category-abbreviations-regexp 'frimacs-process-category-face)))

;;;###autoload
(define-derived-mode frimacs-process-mode comint-mode "Frimacs Process"
  "Major mode for interaction with a running FriCAS program."
  :group 'frimacs
  (setq comint-prompt-regexp (concat "\\(" frimacs-process-prompt-regexp
                                     "\\|" frimacs-process-break-prompt-regexp "\\)"))
  (setq comint-get-old-input (function frimacs-process-get-old-input))
  (setq font-lock-defaults (list frimacs-process-font-lock-keywords))
  (setq electric-indent-inhibit t)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'completion-at-point-functions)
  (make-local-variable 'comint-input-filter-functions)
  (make-local-variable 'comint-output-filter-functions)
  (setq indent-line-function 'frimacs-process-indent-line)
  (setq completion-at-point-functions '(frimacs-process-complete-command-line
                                        frimacs-process-complete-symbol))
  (setq frimacs-menu-compile-buffer-enable nil)
  (setq frimacs-menu-compile-file-enable t)
  (setq frimacs-menu-read-buffer-enable nil)
  (setq frimacs-menu-read-file-enable t)
  (setq frimacs-menu-read-region-enable t)
  (setq frimacs-menu-read-pile-enable nil)
  (let ((schedule-cd-update nil)
        (process-buffer (current-buffer)))
    (add-hook 'comint-input-filter-functions
              (lambda (str)  ; lexical closure
                (when (or (string-match "^)cd" str)
                          (string-match "^)read" str))
                  (setq schedule-cd-update t))
                str))
    (add-hook 'comint-output-filter-functions
              (lambda (str)  ; lexical closure
                (when (and (string-match frimacs-process-prompt-regexp str)
                           schedule-cd-update)
                  (setq schedule-cd-update nil)
                  (let ((frimacs-process-repl-buffer-name process-buffer))  ; dynamic binding
                    (frimacs-process-force-cd-update)))
                (frimacs-process--replace-mathml)
                (frimacs-process--plot)
		(frimacs-process--show-svg)))
    (unless (equal "" frimacs-process-preamble)
      (frimacs-process-insert-command frimacs-process-preamble))
    (setq schedule-cd-update t)
    (while schedule-cd-update
      (sit-for 1))))

(defun frimacs-process-start (process-cmd)
  "Start FriCAS in a buffer using the command given by PROCESS-CMD.

The name of the buffer is given by variable
`frimacs-process-repl-buffer-name', and uses major mode
`frimacs-process-mode'.  Return the buffer in which the process is
started.  If there is a process already running then simply
return it.

Also, if environment variable FRICASEDITOR is not already set
then set it to a string that will invoke the emacsclient program."
  (with-current-buffer (get-buffer-create frimacs-process-repl-buffer-name)
    (when (null (getenv "FRICASEDITOR"))
      (setenv "FRICASEDITOR" "emacsclient -n +$line $name")
      (setenv "FRICASDITOR" "emacsclient -n +$line $name"))  ; workaround bug in current FriCAS
    (when (not (comint-check-proc (current-buffer)))
      (let ((cmdlist (split-string process-cmd)))
        (apply (function make-comint)
               (substring frimacs-process-repl-buffer-name 1 -1)
               (car cmdlist) nil (cdr cmdlist)))
      (frimacs-process-mode))
    (when frimacs-process-enable-pretty-print
     (frimacs-process-insert-command ")set output mathml on"))
    (current-buffer)))

(defun frimacs-process--replace-mathml ()
  "Replace MathML output with rendered svg."
  (when frimacs-process-enable-pretty-print
    (with-current-buffer frimacs-process-repl-buffer-name
      (save-excursion
        (ignore-errors
          (let* ((beg (progn
		        (search-backward "<math")
		        (point)))
	         (end (progn
		        (search-forward "</math>\n")
		        (point)))
	         (data (buffer-substring-no-properties beg end))
                 (file (make-temp-file "result" nil ".svg"))
                 (buf (find-file-noselect file t t))
                 (err-buf (generate-new-buffer "*mml2svg stderr*")))
            (make-process
             :name "mml2svg"
             :command (list frimacs-process-pretty-print-mml2svg-binary-path data)
             :buffer buf
             :stderr err-buf
             :noquery t
             :sentinel (lambda (_ event)
                         (if (string= event "finished\n")
                             (progn
                               (with-current-buffer buf
                                 (basic-save-buffer)
                                 (kill-this-buffer))
                               (with-current-buffer err-buf
                                 (kill-this-buffer))
                               (with-current-buffer frimacs-process-repl-buffer-name
                                 (save-excursion
                                   (goto-char (point-max))
                                   (let ((beg (progn
                                                (search-backward data)
                                                (point)))
                                         (end (progn
                                                (search-forward data)
                                                (point))))
                                     (delete-region beg end)
                                     (insert frimacs-process-pretty-print-separator)
                                     (insert-image-file file))))))))))))))

(defun frimacs-process--plot (&optional input)
  "Plot embedded svg images using gnuplot.
If no INPUT provided it tries to plot previous input."
  (if frimacs-process-embed-gnu-draw
      (ignore-errors
        (if (and (not input)
                 frimacs-process--plots-queue)
            (cl-mapc 'frimacs-process--plot frimacs-process--plots-queue))
        (let* ((prev-input (if input
                               input
                             (string-trim (substring-no-properties (comint-previous-input-string 0)))))
               (processed (cl-find-if (lambda (s) (string= prev-input s)) frimacs-process--processed-plots))
               (file (if (string-prefix-p "gnuDraw(" prev-input)
                         (progn
                           (string-match "\"\\([^\"]+\\)\"" prev-input)
                           (match-string 1 prev-input))))
               (file-exists (file-exists-p file))
               (file-size (if file-exists (file-attribute-size (file-attributes file))))
               (process (and
                         file-exists
                         (not processed)
                         file-size
                         (> file-size 0)))
               (output-file (if process
                                (make-temp-file "result" nil ".svg")))
               (buf (if process (find-file-noselect output-file t t)))
               (err-buf (if process (generate-new-buffer "*gnuplot stderr*"))))
          (if (and
               (not input)
               (not processed)
               file)
              (cl-pushnew prev-input frimacs-process--plots-queue :test 'string=))
          (if process
              (progn
                (push prev-input frimacs-process--processed-plots)
                (setq frimacs-process--plots-queue
                      (cl-remove-if
                       (lambda (s) (string= prev-input s))
                       frimacs-process--plots-queue))
                (make-process
                 :name "gnuplot"
                 :command (list frimacs-process-gnuplot-binary-path
                                "-e" "set term svg" "-c" file)
                 :buffer buf
                 :stderr err-buf
                 :noquery t
                 :sentinel
                 (lambda (_ event)
	           (if (string= event "finished\n")
                       (progn
                         (with-current-buffer buf
		           (basic-save-buffer)
		           (kill-this-buffer))
                         (with-current-buffer err-buf
		           (kill-this-buffer))
                         (with-current-buffer frimacs-process-repl-buffer-name
		           (save-excursion
                             (goto-char (point-max))
                             (search-backward prev-input)
                             (end-of-line)
                             (insert "\n")
                             (insert-image-file output-file)
		             (insert "\n")))))))))))))

(defun frimacs-process--show-svg (&optional input)
  "Show svg images created by modern graphics backend.
If no INPUT provided it tries to show previous input."
  (if frimacs-process-show-svg
      (ignore-errors
        (if (and (not input)
                 frimacs-process--svg-queue)
            (cl-mapc 'frimacs-process--show-svg frimacs-process--svg-queue))
        (let* ((prev-input (if input
                               input
                             (string-trim (substring-no-properties (comint-previous-input-string 0)))))
               (processed (cl-find-if (lambda (s) (string= prev-input s)) frimacs-process--processed-svg))
               (file (if (string-prefix-p "writeSvg" prev-input)
                         (progn
                           (string-match "\"\\([^\"]+.*\.svg\\)\"" prev-input)
                           (match-string 1 prev-input))))
               (file-exists (file-exists-p file))
               (file-size (if file-exists (file-attribute-size (file-attributes file))))
               (process (and
                         file-exists
                         (not processed)
                         file-size
                         (> file-size 0))))
          (if (and
               (not input)
               (not processed)
               file)
              (cl-pushnew prev-input frimacs-process--svg-queue :test 'string=))
          (if process
              (progn
                (push prev-input frimacs-process--processed-svg)
                (setq frimacs-process--svg-queue
                      (cl-remove-if
                       (lambda (s) (string= prev-input s))
                       frimacs-process--svg-queue))
		(with-current-buffer frimacs-process-repl-buffer-name
		  (save-excursion
                    (goto-char (point-max))
                    (search-backward prev-input)
                    (end-of-line)
                    (insert "\n")
                    (insert-image-file file)
		    (insert "\n")))))))))

;;;###autoload
(defun run-fricas (cmd)
  "Launch FriCAS using the given command line.

The name of the buffer is given by variable
`frimacs-process-repl-buffer-name', and uses major mode `frimacs-process-mode'.
With a prefix argument, allow CMD to be edited first (default is value
of `frimacs-process-program').  If there is a process already running
then simply switch to it."
  (interactive (list (if current-prefix-arg
                         (read-string "Run FriCAS: " frimacs-process-program)
                       frimacs-process-program)))
  (let ((buf (frimacs-process-start cmd)))
    (pop-to-buffer buf)))

(provide 'frimacs-process-mode)
