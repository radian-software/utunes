;;; utunes.el --- Microscopic music library manager -*- lexical-binding: t -*-

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

(require 'array)
(require 'cl-lib)
(require 'json)
(require 'let-alist)
(require 'map)
(require 'subr-x)

(defgroup utunes nil
  "Emacs frontend for the microscopic music library manager."
  :group 'applications
  :prefix "utunes-")

(defcustom utunes-log-buffer-name "*utunes-log*"
  "Name of the buffer used for diagnostic output from µTunes commands."
  :type 'string)

(defcustom utunes-delimiter "|"
  "String used as the column delimiter in tabular output from µTunes."
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
          (insert "[" (utunes--capitalize (string-trim event)) "]\n"))))
    (if (= 0 (process-exit-status proc))
        (when utunes--last-callback
          (funcall utunes--last-callback))
      (message "µTunes backend command failed")
      (with-current-buffer (utunes--get-log-buffer)
        (pop-to-buffer (current-buffer))
        (goto-char (point-max))))
    (setq utunes--current-process nil)))

(defvar utunes--last-process nil
  "µTunes process currently or last running.
Used to prevent running more than one µTunes command
concurrently.")

(defvar utunes--last-callback nil
  "µTunes callback for currently or last running process.")

(cl-defun utunes-command (&key args stdin stdout callback)
  "Run a µTunes backend command with given command-line ARGS.
ARGS should not include \"utunes\".

If provided, STDIN should be a buffer whose contents will be fed
to the command on stdin. Similarly, if provided, STDOUT should be
a buffer to whose contents the command will append from stdout.

If a µTunes command is already running, return an error.

If the µTunes command fails unexpectedly or due to user error,
display a message in the minibuffer and pop to the log buffer.

If CALLBACK is provided, call it with no arguments if the command
succeeds."
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
        (setq utunes--last-callback callback)
        (set-process-sentinel (get-buffer-process (current-buffer))
                              #'ignore)
        (when stdin
          (with-current-buffer stdin
            (process-send-region
             utunes--last-process
             (point-min) (point-max))
            (process-send-eof utunes--last-process)))))))

(defvar utunes--scratch-buffer (get-buffer-create " *utunes-scratch*")
  "Buffer used for scratch work in spawning processes.")

(defun utunes--map-alist-keys (func alist)
  "Return a new alist generated by applying FUNC to each key of ALIST."
  (mapcar (lambda (link) (cons (funcall func (car link)) (cdr link)))
          alist))

(defun utunes--kebab-to-snake (alist)
  "Turn the keys in ALIST from kebab-case to snake_case.
Return a new alist, without modifying the original."
  (utunes--map-alist-keys
   (lambda (key)
     (intern (replace-regexp-in-string
              "-" "_" (symbol-name key))))
   alist))

(defun utunes--snake-to-kebab (alist)
  "Turn the keys in ALIST from snake_case to kebab-case.
Return a new alist, without modifying the original."
  (utunes--map-alist-keys
   (lambda (key)
     (intern (replace-regexp-in-string
              "_" "-" (symbol-name key))))
   alist))

(cl-defun utunes-read (format &key filters sorts illegal-chars callback)
  "Invoke 'utunes read'. All keyword arguments are optional.
Write output into the buffer that is current when this function
is called, at point.

FORMAT is a string in Python str.format syntax.

FILTERS (defaults to nil) is an alist mapping field
names (strings) to Python regexps (strings). It may contain more
than one link with the same field name.

SORTS (defaults to nil) is an alist mapping field names (strings)
to sort qualifiers (symbols, one of `alpha', `alpha-reverse',
`random', `numeric', or `numeric-reverse'). The alist may contain
more than one link with the same field name (although the utility
of this is dubious), and its order is significant.

ILLEGAL-CHARS, if given, is a string.

If CALLBACK is given, invoke it with no arguments once the
command returns (if it succeeds). Do not change which buffer is
current."
  (utunes-command
   :args `("read"
           ,@(mapcan
              (lambda (filter)
                `("-f" ,(format "%s=%s" (car filter) (cdr filter))))
              filters)
           ,@(mapcan
              (lambda (sort)
                `("-s"
                  ,(format
                    "%s:%s"
                    (pcase (cdr sort)
                      (`alpha "s")
                      (`alpha-reverse "r")
                      (`random "x")
                      (`numeric "S")
                      (`numeric-reverse "R"))
                    (car sort))))
              sorts)
           "-i" ,(or illegal-chars "")
           "--" ,format)
   :stdout (current-buffer)
   :callback callback))

(cl-defun utunes-read-to-table (fields &key filters sorts callback)
  "Read given FIELDS in tabular format into the current buffer.
All keyword arguments are optional.

FIELDS is a list of strings. FILTERS, SORTS, and CALLBACK are as
described in `utunes-read' (which see).

The inserted table has no extraneous spacing."
  (utunes-read
   (concat
    (mapconcat
     (lambda (field)
       (format "{%s}" field))
     fields
     utunes-delimiter)
    "\n")
   :filters filters
   :sorts sorts
   :illegal-chars utunes-delimiter
   :callback callback))

(cl-defun utunes-read-to-list (fields &key filters sorts callback)
  "Read given FIELDS into a list of alists.
All keyword arguments are optional.

FIELDS is a list of strings. FILTERS and SORTS are as described
in `utunes-read' (which see). CALLBACK is invoked with a list of
alists once the command returns, provided that it succeeds. Each
element of the list represents one returned song. The keys are
field names (strings), and the values are the corresponding field
values (strings)."
  (with-current-buffer utunes--scratch-buffer
    (erase-buffer)
    (utunes-read-to-table
     fields
     :filters filters
     :sorts sorts
     :callback
     (lambda ()
       (when callback
         (funcall
          callback
          (with-current-buffer utunes--scratch-buffer
            (mapcar
             (lambda (line)
               (cl-mapcar
                #'cons
                fields
                (split-string line (regexp-quote utunes-delimiter))))
             (split-string (buffer-string) "\n" 'omit-nulls)))))))))

(cl-defun utunes-write (regex &key playlist callback)
  "Invoke 'utunes write'. Read input from the current buffer.
All keyword arguments are optional.

The entire buffer is sent on stdin. To prevent this, use
narrowing.

REGEX is a Python-style regexp, a string. PLAYLIST, if given, is
a string. CALLBACK, if given, is invoked with no arguments once
the command returns (if it succeeds)."
  (utunes-command
   :args `("write" "--" ,regex ,@(when playlist `(,playlist)))
   :stdin (current-buffer)
   :callback callback))

(cl-defun utunes-playback (&key input callback)
  "Invoke 'utunes playback'. All arguments are optional.

INPUT (defaults to nil) is an alist which is turned into JSON and
fed to µTunes on stdin. The resulting JSON on stdout is decoded
and turned back into an alist. Then CALLBACK, if non-nil, is
invoked with the alist as its single argument.

The keys of the input and output alists are symbols (`playlist',
`seek-end', etc.), and the values are either strings,
booleans (nil or t), integers, or floating-point numbers, as
appropriate."
  (let* ((json-false nil)
         (json-null :json-null))
    (with-temp-buffer
      ;; json.el will mistake nil for false due to our settings above.
      ;; But we actually want nil (an empty alist) to turn into an
      ;; empty map. The following is an easy workaround.
      (insert (if input
                  (json-encode (utunes--kebab-to-snake input))
                "{}"))
      (let ((stdin (current-buffer)))
        (with-current-buffer utunes--scratch-buffer
          (erase-buffer)
          (utunes-command
           :args '("playback")
           :stdin stdin
           :stdout utunes--scratch-buffer
           :callback (lambda ()
                       (when callback
                         (funcall
                          callback
                          (with-current-buffer utunes--scratch-buffer
                            (goto-char (point-min))
                            (utunes--snake-to-kebab
                             (let ((json-false nil))
                               (json-read)))))))))))))

(defun utunes--format-time (seconds)
  "Format a number of SECONDS as a human-readable string.
SECONDS may be integer or floating-point. For example:

  (utunes--format-time 75.1)

returns:

  \"1m15s\""
  (if (>= seconds 1)
      (format-seconds "%yy%dd%hh%mm%ss%z" seconds)
    "0s"))

;;;###autoload
(defun utunes-status (&optional state)
  "Report the current playback status in the echo area.
This function is asynchronous.

If STATE is given, use it instead of invoking `utunes-playback'
to get the playback state."
  (interactive)
  (let ((callback
         (lambda (resp)
           (let-alist resp
             (cond
              ((null .seek)
               (message "No track selected"))
              ((or (null .playlist) (null .index) (null .seek-end))
               (error "utunes-status: null playlist, index, or seek-end"))
              (t
               (utunes-read-to-list
                '("album" "song" "album_artist")
                :filters `(("playlist" . ,(format "%s:%d" .playlist .index)))
                :callback
                (lambda (songs)
                  (unless (= (length songs) 1)
                    (error "utunes-status: songs list is not of length 1" songs))
                  (let ((data (car songs)))
                    (utunes-read-to-list
                     '("id")
                     :filters `(("playlist" . ,(format "%s:[^:]+" .playlist)))
                     :callback
                     (lambda (playlist-songs)
                       (message
                        "%s [%s/%s] %s - %s (%s) | %d/%d from %s"
                        (if .playing "PLAYING" "PAUSED")
                        (utunes--format-time .seek)
                        (utunes--format-time .seek-end)
                        (alist-get "song" data nil nil #'equal)
                        (alist-get "album" data nil nil #'equal)
                        (alist-get "album_artist" data nil nil #'equal)
                        .index
                        (length playlist-songs)
                        .playlist))))))))))))
    (if state
        (funcall callback state)
      (utunes-playback :callback callback))))

;;;###autoload
(defun utunes-toggle ()
  "Toggle playback state between playing and paused."
  (interactive)
  (utunes-playback
   :input
   `((playing . "toggle"))
   :callback #'utunes-status))

;;;###autoload
(defun utunes-rewind ()
  "Seek to beginning of current song."
  (interactive)
  (utunes-playback
   :input '((seek . 0))
   :callback #'utunes-status))

;;;###autoload
(defun utunes-prev (&optional toggle-play-pause)
  "Go back to previous song. With prefix argument, toggle play/pause state."
  (interactive "P")
  (utunes-playback
   :callback
   (lambda (resp)
     (let-alist resp
       (if .index
           (utunes-playback
            :input
            `((index . ,(1- .index))
              (playing . ,(xor .playing toggle-play-pause)))
            :callback #'utunes-status))))))

;;;###autoload
(defun utunes-next (&optional toggle-play-pause)
  "Skip to next song. With prefix argument, toggle play/pause state."
  (interactive "P")
  (utunes-playback
   :callback
   (lambda (resp)
     (let-alist resp
       (if .index
           (utunes-playback
            :input
            `((index . ,(1+ .index))
              (playing . ,(xor .playing toggle-play-pause)))
            :callback #'utunes-status))))))

;;;; Closing remarks

(provide 'utunes)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; End:

;;; utunes.el ends here
