;;; eltu.el --- Emacs Lisp TAGS Updater  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017  Dale Sedivec
;;
;; Author: Dale Sedivec <dale@codefu.org>
;; Keywords: tags ctags etags
;; Version: 0.3
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)

(defcustom eltu-tags-command-style 'exuberant-ctags
  "Defines the command line interface for your tags command.

This can be one of the symbols exuberant-ctags or etags."
  :type '(choice
          (const :tag "Exuberant Ctags" exuberant-ctags)
          (const :tag "etags" etags)))

(defcustom eltu-exuberant-ctags-command "ctags"
  "Command to run for Exuberant Ctags."
  :type 'string)

(defcustom eltu-exuberant-ctags-additional-arguments nil
  "List of additional arguments to be passed to Exuberant Ctags."
  :type '(repeat string))

(defcustom eltu-etags-command "etags"
  "Command to run for etags."
  :type 'string)

(defcustom eltu-etags-additional-arguments nil
  "List of additional arguments to be passed to etags."
  :type '(repeat string))

(defcustom eltu-backend 'eltu-backend-python
  "Backend to use to update the tags file.

The function will be called with no arguments and the current
buffer will be the tags table's buffer.  The backend is expected
to update the tags for the files in the list
`eltu-files-to-update'.  The backend function may need to update
variable `eltu-update-in-progress', and must set it to an
appropriate value to indicate when the tags update is complete.

Backends should eventually call `eltu-update-tags' again if
`eltu-files-to-update' has accumulated new files to be updated
while the backend was busy updating tags.

Note that this is different from the tags program that will
actually be called to generate tags.  See
`eltu-tags-command-style` and friends for those settings.
Backends should probably use `eltu-get-tags-command' to get the
ctags/etags command line to be used."
  :type '(choice
          (const :tag "Python (2.6-2.7 required)" eltu-backend-python)
          (const :tag "Emacs Lisp (may be slow)" eltu-backend-emacs)
          (function :tag "Other backend function")))

(defconst eltu-dir (expand-file-name (file-name-directory
                                      (or load-file-name buffer-file-name)))
  "Directory where eltu is installed.

Used to find our backend Python script if `eltu-backend-python'
is in use.")

(defvar eltu-files-to-update nil
  "List of files that need their tags updated.")

(defvar eltu-update-in-progress nil
  "Indicates whether or not tags are currently being updated.
If NIL, tags not currently being updated.  If a process, tags are
being updated if the process is live.  Otherwise, tags are being
updated.  See also `eltu-update-in-progress-p'.

`eltu-update-tags' will set this to T, but the backend is
responsible for updating this variable and ultimately indicating
the completion of tags updating.")

(defvar eltu-debug nil
  "If T then eltu will log extra messages about what it's doing.")

(defvar eltu-buffer-name "*eltu*"
  "Buffer used for logging output of eltu work and external commands.")

(defun eltu-update-in-progress-p ()
  "Return true if a tags update is already in progress."
  (if (processp eltu-update-in-progress)
      (process-live-p eltu-update-in-progress)
    eltu-update-in-progress))

(defun eltu-get-tags-command (tags-file files-to-update)
  "Return a tags command to generate TAGS-FILE from FILES-TO-UPDATE."
  (append (cl-ecase eltu-tags-command-style
            (exuberant-ctags (append (list eltu-exuberant-ctags-command
                                           "-e" "-a" "-f" tags-file)
                                     eltu-exuberant-ctags-additional-arguments))
            (etags (append (list eltu-etags-command "-a" "-o" tags-file)
                           eltu-etags-additional-arguments)))
          files-to-update))

(defun eltu-log-debug (message &rest args)
  "Write MESSAGE, interpolating ARGS, if `eltu-debug' is true."
  (when eltu-debug
    (let ((inhibit-message t))
      (message (concat "eltu debug: " (apply #'format message args))))))

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

(defun eltu-start-backend-command (command)
  "Run COMMAND with output to the `eltu-buffer-name' buffer.

This function sets `eltu-update-in-progress' to the process that
this function creates.  When the process exits,
`eltu-update-in-progress' will be set to NIL.

Note that `eltu-update-tags' will be called after this process
exits if `eltu-files-to-update' is non-NIL.  Put another way, if
files were updated while the backend process was running, we will
try and update tags for those changed files once the process
created by this function exits.  See also
`eltu-backend-command-sentinel'."
  (cl-assert (listp command))
  (let ((buffer (get-buffer-create eltu-buffer-name)))
    (progn
      (eltu-log-debug "Running: %S" command)
      (setq eltu-update-in-progress
            (make-process :name (concat "eltu:" (car command))
                          :command command
                          :sentinel #'eltu-backend-command-sentinel
                          :buffer buffer)))))

(defun eltu-backend-command-sentinel (process event)
  "Sentinel function for eltu backend PROCESS, handles EVENT."
  (with-current-buffer (process-buffer process)
    (when (process-live-p process)
      (error "Sentinel expected process to be dead, after event %s" event))
    (eltu-log-debug "Process sentinel: %S" (string-trim event))
    (setq eltu-update-in-progress nil)
    (cond
      ((string= event "finished\n")
       (eltu-log-debug "Process exited successfully")
       (when eltu-files-to-update
         ;; Files were changed while ctags was running.  Kick off
         ;; another update.
         (eltu-update-tags)))
      (t
       (pop-to-buffer (process-buffer process) nil t)
       (error "Error updating tags file: %s" (string-trim event))))))

(defun eltu-remove-files-from-current-tags-table (file-names)
  "Remove tags for all FILE-NAMES from the current buffer.

The current buffer must be a TAGS buffer."
  (save-excursion
    (cl-assert (buffer-file-name) nil "Tags buffer has no file name?")
    (let* ((tags-file-dir (file-name-directory (buffer-file-name)))
           file-patterns regex start)
      (mapc (lambda (file-name)
              (unless (file-name-absolute-p file-name)
                (error "Not an absolute file name: %s" file-name))
              (push (regexp-quote file-name) file-patterns)
              (concat "(?:./)?"
                      (regexp-quote (file-relative-name file-name
                                                        tags-file-dir))))
            file-names)
      (setq regex (concat "^(?:" (mapconcat #'identity file-patterns "|")
                          "),[[:digit:]]*$" ))
      (eltu-log-debug "Delete tags regex: %s" regex)
      (goto-char (point-min))
      (eltu-log-debug "Starting delete search at %S" (current-time))
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
           (eltu-log-debug "Deleting some tags near %S" (point))
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
           (eltu-log-debug "False tags match near %S" (point))
           (forward-line 2))))
      (eltu-log-debug "Finished delete search at %S" (current-time)))
    (when (buffer-modified-p)
      (save-buffer)
      (eltu-log-debug "save-buffer complete at %S" (current-time))
      ;; Revert to cause completion tables and such to be rebuilt.
      ;; Code stolen from tags-verify-table.
      ;;(revert-buffer t t)
      ;;(tags-table-mode)
      ;;(eltu-log-debug "revert-buffer complete at %S" (current-time))
      ;; revert-buffer is slow.  Maybe just clear modtime to hint it
      ;; to reload when next you try and find tags?
      ;;(set-visited-file-modtime '(0 0 0 0))
      ;;(eltu-log-debug "set-visited-file-modtime complete at %S"
      ;;                (current-time))
      ;; Wait, that's probably dumb.  Why not just kill the buffer
      ;; to cause it to be reloaded?
      (kill-buffer))))

(defun eltu-backend-emacs (files-to-update)
  "Update tags for the files in FILES-TO-UPDATE.

Tags are deleted from the current file entirely in Emacs, which
can be slow."
  (eltu-log-debug "eltu-update-tags called")
  (eltu-remove-files-from-current-tags-table files-to-update)
  ;; We're letting your tags program update the TAGS file in place.
  ;; That may be a bad idea.
  (let ((command (eltu-get-tags-command (buffer-file-name) files-to-update)))
    (eltu-log-debug "Tags command: %s" (mapconcat #'identity command " "))
    (eltu-start-backend-command command)))

(defun eltu-backend-python (files-to-update)
  "Update FILES-TO-UPDATE in the tags file, which is current buffer.

Uses an external Python script to update the tags file in the
background, without blocking Emacs.  Probably faster than having
Emacs do it, too."
  (eltu-log-debug "Gearing up Python backend")
  (let* ((tags-file-dir (file-name-directory (buffer-file-name)))
         ;; Make temp file in same directory as tags file so that we
         ;; can rename(2).
         (temp-file (make-temp-file (expand-file-name "eltu-TAGS"
                                                      tags-file-dir))))
    (condition-case err
        (let* ((tags-command-json (json-encode
                                   (eltu-get-tags-command temp-file
                                                          files-to-update)))
               (python-script (expand-file-name "eltu_update_tags.py"
                                                eltu-dir))
               ;; XXX factor string out to variable?
               (python-command (append (list "python" python-script
                                             tags-command-json temp-file
                                             (buffer-file-name))
                                       files-to-update)))
          (eltu-start-backend-command python-command))
      (error
       (ignore-errors
         (delete-file temp-file))
       (signal (car err) (cdr err))))))

(defun eltu-update-tags ()
  "Update tags for the files in `eltu-files-to-update'.

Sets `eltu-update-in-progress', clears `eltu-files-to-update',
and calls the function in `eltu-backend' with the list of files
to update."
  (eltu-log-debug "May update tags")
  (when (and eltu-files-to-update
             (visit-tags-table-buffer))
    (when (eltu-update-in-progress-p)
      (error "`eltu-update-tags' called while update already in progress"))
    (eltu-log-debug "Will update tags with backend %S" eltu-backend)
    (let ((files-to-update eltu-files-to-update))
      (setq eltu-files-to-update nil
            eltu-update-in-progress t)
      (condition-case err
          (funcall eltu-backend files-to-update)
        (error
         (setq eltu-update-in-progress nil)
         ;; Should we put eltu-files-to-update back to its original value?
         (signal (car err) (cdr err)))))))

;;;###autoload
(define-minor-mode eltu-mode
    "Minor mode to automatically update the TAGS file after saving this buffer."
  nil nil nil
  (if eltu-mode
      (add-hook 'after-save-hook #'eltu-after-save-hook nil t)
    (remove-hook 'after-save-hook #'eltu-after-save-hook t)))

(provide 'eltu)

;;; eltu.el ends here
