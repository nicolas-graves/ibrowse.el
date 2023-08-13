;;; ibrowse-sql.el --- Interact with your browser -*- lexical-binding: t -*-

;; Copyright © 2022 Nicolas Graves <ngraves@ngraves.fr>
;; Some snippets have been written by
;; Copyright © 2021 BlueBoxWare
;; Copyright © 2016 Taichi Kawabata <kawabata.taichi@gmail.com>

;; Author: Nicolas Graves <ngraves@ngraves.fr>
;; Version: 0.2.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: comm, data, files, tools
;; URL: https://git.sr.ht/~ngraves/ibrowse.el

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Interact with your browser from Emacs
;; Currently opening an immutable database file is not allowed with the builtin
;; sqlite implementation, test with this command for instance :
;; (sqlite-open (concat "file:" ibrowse-history-db "?immutable=1"))

;;; Code:

(require 'ibrowse-core)

;;; Variables

(defvar ibrowse-sql-candidates nil
  "The ibrowse alist cache.")

(defvar ibrowse-sql-queue nil
  "Queue of SQL commands to execute.")

(defvar ibrowse-sql-timer nil
  "Timer that is set up for retrying SQL command execution.")

(defvar ibrowse-sql-retry-interval 5
  "Interval in seconds between retries of SQL commands.")

(defun ibrowse-sql--apply-command
    (database command &optional callback immutable-p)
  "Apply the SQL COMMAND using the SQL DATABASE, then call CALLBACK.

If IMMUTABLE-P, use the immutable option for the database."
  (with-temp-buffer
    (let ((database (if immutable-p
                        (concat "file:" database "?immutable=1")
                      database)))
      (if (zerop (call-process "sqlite3" nil t nil "-ascii" database command))
          (progn
            (when ibrowse-sql-timer
              (cancel-timer ibrowse-sql-timer)
              (setq ibrowse-sql-timer nil))
            (when callback
              (funcall callback (buffer-string))))
        (let ((err-msg (buffer-string)))
          (if (string-match-p "database is locked" err-msg)
              (progn
                (message
                 "Database is locked, command will be retried in %s seconds."
                 ibrowse-sql-retry-interval)
                (push (cons database command) ibrowse-sql-queue)
                (run-hooks 'ibrowse-sql-queue-changed-hook))
            (error "Command sqlite3 failed: %s: %s" command err-msg)))))))

(defun ibrowse-sql--retry-commands ()
  "Retry SQL commands in `ibrowse-sql-queue'."
  (when ibrowse-sql-queue
    (let* ((entry (pop ibrowse-sql-queue))
           (database (car entry))
           (command (cdr entry)))
      (ibrowse-sql--apply-command database command))))

(defun ibrowse-sql--initiate-retry-timer ()
  "Initiates a timer retrying commands in `ibrowse-sql-queue'.
Commands are retried while the queue is not empty."
  (when (and ibrowse-sql-queue (not ibrowse-sql-timer))
    (setq ibrowse-sql-timer
          (run-with-timer ibrowse-sql-retry-interval
                          ibrowse-sql-retry-interval
                          #'ibrowse-sql--retry-commands))))

(add-hook 'ibrowse-sql-queue-changed-hook 'ibrowse-sql--initiate-retry-timer)

(defun ibrowse-sql-check-active-commands ()
  "Check if there are active SQL commands before killing Emacs.
If `ibrowse-sql-queue' is not empty, ask the user for confirmation
before killing Emacs."
  (or (not ibrowse-sql-queue)
      (y-or-n-p
       (concat "Active SQL commands exist, "
               "you might want to close your browser; "
               "kill them and exit anyway? "))))

(add-hook 'kill-emacs-query-functions #'ibrowse-sql-check-active-commands)

(defun ibrowse-sql--read-callback (_)
  "Function applied to the result of the SQL query."
  (let (result)
    (goto-char (point-min))
    ;; -ascii delimited by 0x1F and 0x1E
    (while (re-search-forward (rx (group (+? anything)) "\x1e") nil t)
      (push (split-string (match-string 1) "\x1f") result))
    (nreverse result)))

(defun ibrowse-sql--get-candidates (db query &optional candidate-format)
  "Build candidates from DB and QUERY and format them with CANDIDATE-FORMAT."
  (unless ibrowse-sql-candidates
    (message "[ibrowse] Building cache...")
    (let ((candidates (ibrowse-sql--apply-command
                       db query #'ibrowse-sql--read-callback t)))
      (setq ibrowse-sql-candidates
            (if candidate-format
                (mapcar candidate-format candidates)
              candidates))))
  ibrowse-sql-candidates)

(defun ibrowse-sql-update-browser! ()
  "Update `ibrowse-sql-candidates' if you have changed your current browser."
  (setq ibrowse-sql-candidates nil))

(add-hook 'ibrowse-core-update-hook 'ibrowse-sql-update-browser! -80)

(provide 'ibrowse-sql)
;;; ibrowse-sql.el ends here
