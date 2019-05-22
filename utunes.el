;;; utunes.el --- Microscopic music library manager. -*- lexical-binding: t -*-

;; Copyright (C) 2019 Radon Rosborough

;; Author: Radon Rosborough <radon.neon@gmail.com>
;; Created: 30 Apr 2019
;; Homepage: https://github.com/raxod502/utunes
;; Keywords: applications
;; Package-Requires: ((emacs "26"))
;; Version: 0

;;; Commentary:

;; µTunes attempts to replace major functionality of a full-featured
;; music library manager such as iTunes with an absolute minimum
;; number of lines of code. The interface, as such, is designed to
;; maximize flexibility while minimizing implementation complexity and
;; number of abstractions.

;; Please see <https://github.com/raxod502/utunes> for more
;; information.

;;; Code:

;; To see the outline of this file, run M-x outline-minor-mode and
;; then press C-c @ C-t. To also show the top-level functions and
;; variable declarations in each section, run M-x occur with the
;; following query: ^;;;;* \|^(

(require 'cl-lib)
(require 'subr-x)

(defgroup utunes nil
  "Emacs frontend for the microscopic music library manager."
  :group 'applications
  :prefix "utunes-")

(defcustom utunes-log-buffer-name "*utunes-log*"
  "Name of the buffer used for diagnostic output from µTunes commands."
  :type 'string)

(defun utunes--get-log-buffer ()
  "Return the buffer for diagnostic output from µTunes commands.
If the buffer does not exist, create it first."
  (if-let ((buf (get-buffer utunes-log-buffer-name)))
      buf
    (with-current-buffer (get-buffer-create utunes-log-buffer-name)
      (special-mode)
      (current-buffer))))

(defun utunes--capitalize (str)
  "Capitalize only the first character of STR."
  (if (string-empty-p str)
      str
    (concat (upcase (substring str 0 1)) (substring str 1))))

(defun utunes--process-sentinel (proc event)
  "Process sentinel for µTunes backend commands.
PROC is the process and EVENT is a string describing the event
that happened. See
<https://www.gnu.org/software/emacs/manual/html_node/elisp/Sentinels.html>."
  (unless (process-live-p proc)
    (with-current-buffer (utunes--get-log-buffer)
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-max))
          (insert "[" (utunes--capitalize (string-trim event)) "]\n")))
      (unless (= 0 (process-exit-status proc))
        (message "µTunes backend command failed")
        (pop-to-buffer (current-buffer))
        (goto-char (point-max))))
    (setq utunes--current-process nil)))

(defvar utunes--last-process nil
  "µTunes process currently or last running.
Used to prevent running more than one µTunes command
concurrently.")

(cl-defun utunes-run-backend-command (&key args stdin stdout)
  "Run a µTunes backend command with given command-line ARGS.
ARGS should not include \"utunes\".

If provided, STDIN should be a buffer whose contents will be fed
to the command on stdin. Similarly, if provided, STDOUT should be
a buffer to whose contents the command will append from stdout.

If a µTunes command is already running, return an error."
  (cl-block nil
    (when (process-live-p utunes--last-process)
      (error "µTunes process already running"))
    (with-current-buffer (utunes--get-log-buffer)
      (let ((args (cons "utunes" args))
            (inhibit-read-only t))
        (save-excursion
          (goto-char (point-max))
          (unless (= (point) (point-min))
            (insert "\n"))
          (insert "$ " (mapconcat #'shell-quote-argument args " ") "\n"))
        (condition-case _err
            (setq utunes--last-process
                  (make-process
                   :name "utunes"
                   :buffer stdout
                   :command args
                   :stderr (current-buffer)
                   :sentinel #'utunes--process-sentinel))
          (file-missing
           (insert "[Command not found]\n")
           (cl-return)))
        (set-process-sentinel (get-buffer-process (current-buffer))
                              #'ignore)
        (when stdin
          (with-current-buffer stdin
            (process-send-string
             utunes--last-process
             (point-min) (point-max))))))))

;;;; Closing remarks

(provide 'utunes)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; End:

;;; el-patch.el ends here
