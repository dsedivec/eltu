;;; eltu -- Update Emacs TAGS files as they change  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)

(defcustom eltu-ctags-executable "ctags"
  "Path to your ctags program.")

(defcustom eltu-ctags-arguments nil
  "List of additional arguments to ctags."
  :type '(repeat string))

(defcustom eltu-backend 'eltu-backend-python
  "Backend function to use to update the tags file.

Function will be called with no arguments.  Current buffer will
be the tags table's buffer."
  ;; XXX test this
  :type '(choice
          (const :tag "Python (2.6-2.7 required)" eltu-backend-python)
          (const :tag "Emacs Lisp (may be slow)" eltu-backend-emacs)
          (function :tag "Other function")))

(defvar eltu-files-to-update nil
  "List of files that need their tags updated.")

(defvar eltu-process-var nil
  "Names a buffer-local variable holding a backend command process.")

;; XXX really separate vars?  why?
;; XXX Yes, this is probably dumb, see eltu-update-tags.
(defvar eltu-ctags-process nil)

(defvar eltu-python-process nil)

(defconst eltu-dir (expand-file-name (file-name-directory
                                      (or load-file-name buffer-file-name)))
  "Directory where eltu is installed.

Used to find our backend Python script if `eltu-backend-python'
is in use.")

(defvar eltu-noisy nil
  "If T then eltu will log extra messages about what it's doing.")

(defun eltu-noisy (message &rest args)
  (when eltu-noisy
    (let ((inhibit-message t))
      (message (concat "eltu-noisy: " (apply #'format message args))))))

(defun eltu-remove-file-from-current-tags-table (abs-file-name)
  "Remove all tags for ABS-FILE-NAME from currently selected tags table."
  (unless (file-name-absolute-p abs-file-name)
    ;; We don't know what a relative file-name should be relative to.
    (error "Must provide absolute file name"))
  (save-excursion
    ;; Only if there is a current tags table.
    (when (visit-tags-table-buffer)
      (eltu-noisy "Will try and delete tags for %S" abs-file-name)
      (cl-assert (buffer-file-name) nil "Tags buffer has no file name?")
      ;; Search for the absolute file name as well as the file name
      ;; relative to the tags table.
      (let* ((rel-file-name (file-relative-name abs-file-name
                                                (file-name-directory
                                                 (buffer-file-name))))
             (regex (rx-to-string `(seq line-start
                                        (or ,abs-file-name
                                            (seq (? "./") ,rel-file-name))
                                        "," (* digit)
                                        line-end)))
             ;;(regex (concat "^"
             ;;
             ;;               (regexp-opt (list abs-file-name rel-file-name))
             ;;               ",[0-9]*$"))
             start)
        (eltu-noisy "Delete tags regex: %s" regex)
        (goto-char (point-min))
        (eltu-noisy "Starting delete search at %S" (current-time))
        (while (re-search-forward regex nil t)
          (cond
            ;; Format for each file in TAGS is
            ;;
            ;;     \f\nfile_name,NNN\ntags...
            ;;
            ;; Where NNN is probably the size of the following tags
            ;; for this file.  etags.el implies NNN might be optional.
            ;; (We don't use it.)
            ;;
            ;; Make sure we found our actual desired file name and not
            ;; some kind of weird garbage by looking back for the
            ;; \f\n.
            ((and (zerop (forward-line -1))
                  (looking-at "\f\n"))
             (eltu-noisy "Deleting some tags near %S" (point))
             (setq start (point))
             (forward-line 2)
             ;; Find next \f, which is start of next file in tags
             ;; file, or else we hit EOF.
             (if (re-search-forward "^\f$" nil t)
                 (forward-line 0)
               (goto-char (point-max)))
             (delete-region start (point)))
            (t
             ;; Not a real match, skip over it.
             (eltu-noisy "False tags match near %S" (point))
             (forward-line 2))))
        (eltu-noisy "Finished delete search at %S" (current-time)))
      (when (buffer-modified-p)
        (save-buffer)
        (eltu-noisy "save-buffer complete at %S" (current-time))
        ;; Revert to cause completion tables and such to be rebuilt.
        ;; Code stolen from tags-verify-table.
        ;;(revert-buffer t t)
        ;;(eltu-noisy "revert-buffer complete at %S" (current-time))
        ;; revert-buffer is slow.  Maybe just clear modtime to hint it
        ;; to reload when next you try and find tags?
        (set-visited-file-modtime '(0 0 0 0))
        (eltu-noisy "set-visited-file-modtime complete at %S" (current-time))
        (tags-table-mode)))))

(defun eltu-after-save-hook ()
  "Rebuild tags for the current buffer's file."
  (cl-pushnew (buffer-file-name) eltu-files-to-update)
  (eltu-update-tags))

(defun eltu-update-file ()
  "Update tags for the current buffer's file."
  (interactive)
  (unless (buffer-file-name)
    (error "This file has no buffer"))
  (message "Updating tags for %s" (buffer-file-name))
  (eltu-after-save-hook))

;; XXX process-var may not be needed, just use names?
(defun eltu-run-backend-command (name process-var command)
  "Run backend COMMAND named NAME, set PROCESS-VAR to the process."
  (let ((buffer (generate-new-buffer (format "*%s*" name))))
    (condition-case err
        (progn
          (eltu-noisy "Running: %S" command)
          (set process-var
               (make-process :name name
                             :command command
                             :sentinel #'eltu-backend-command-sentinel
                             :buffer buffer))
          (with-current-buffer buffer
            (set (make-local-variable 'eltu-process-var) process-var)))
      (error
       ;; make-process failed, don't litter buffers.
       (kill-buffer buffer)
       (signal (car err) (cdr err))))))

(defun eltu-backend-command-sentinel (process event)
  "Sentinel function for an eltu PROCESS, handles EVENT."
  (with-current-buffer (process-buffer process)
    (unless (eq process (symbol-value eltu-process-var))
      (error "Unexpected process, expected %S got %S"
             (symbol-value eltu-process-var) process))
    (when (process-live-p process)
      (error "Sentinel expected process to be dead, after event %S" event))
    (eltu-noisy "Process sentinel: %S" (string-trim event))
    (set eltu-process-var nil)
    (cond
      ((string= event "finished\n")
       (eltu-noisy "Process exited successfully")
       (kill-buffer)
       (when eltu-files-to-update
         ;; Files were changed while ctags was running.  Kick off
         ;; another update.
         (eltu-update-tags)))
      (t
       (pop-to-buffer (process-buffer process) nil t)
       (error "Error updating tags file: %s" (string-trim event))))))

(defun eltu-get-ctags-command ()
  "Return partial ctags command.
Includes ctags executable name, \"-a\" switch, and
`eltu-ctags-arguments'.  Does not include \"-f\" nor the list of
files to update."
  (append (list eltu-ctags-executable "-e" "-a")
          eltu-ctags-arguments))

(defun eltu-backend-emacs ()
  "Update tags for the files in `eltu-files-to-update'.

Tags are deleted from the current file entirely in Emacs, which can be slow."
  (eltu-noisy "eltu-update-tags called")
  (when (not (and eltu-ctags-process (process-live-p eltu-ctags-process)))
    (eltu-noisy "Will try and update tags, after deleting some")
    (mapc #'eltu-remove-file-from-current-tags-table eltu-files-to-update)
    (let ((command (append (eltu-get-ctags-command)
                           (list "-f" (buffer-file-name))
                           eltu-files-to-update)))
      (eltu-noisy "Tags command: %s" (mapconcat #'identity command " "))
      (eltu-run-backend-command "eltu-ctags" 'eltu-ctags-process command)
      (setq eltu-files-to-update nil))))

(defun eltu-backend-python ()
  (when (not (and eltu-python-process (process-live-p eltu-python-process)))
    (eltu-noisy "Gearing up Python backend")
    (let* ((ctags-command-json (json-encode (eltu-get-ctags-command)))
           ;; XXX factor string out to variable
           (python-script (expand-file-name "eltu_update_tags.py"
                                            eltu-dir))
           ;; XXX factor string out to variable
           (python-command (append (list "python2" python-script
                                         ctags-command-json (buffer-file-name))
                                   eltu-files-to-update)))
      (eltu-run-backend-command "eltu-python" 'eltu-python-process
                                python-command)
      (setq eltu-files-to-update nil))))

(defun eltu-update-tags ()
  "Update tags for the files in `eltu-files-to-update'."
  (eltu-noisy "May update tags")
  (when (and eltu-files-to-update
             (visit-tags-table-buffer))
    (eltu-noisy "Will update tags with backend %S" eltu-backend)
    (funcall eltu-backend)))

(define-minor-mode eltu-mode
    "Minor mode to automatically update the TAGS file after saving this buffer."
  nil nil nil
  (if eltu-mode
      (add-hook 'after-save-hook #'eltu-after-save-hook nil t)
    (remove-hook 'after-save-hook #'eltu-after-save-hook t)))

(provide 'eltu)

;;; eltu.el ends here
