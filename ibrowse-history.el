;; ibrowse-history.el --- Interact with your browser from Emacs -*- lexical-binding: t -*-

;; Copyright © 2019 Xu Chunyang <mail@xuchunyang.me> (original author)
;; Copyright © 2022 Nicolas Graves <ngraves@ngraves.fr> (heavy rewrite and adding deletions)

;; Author: Nicolas Graves <ngraves@ngraves.fr>
;; Version: 0.0.0
;; Package-Requires: ((emacs "24.3") (let-alist "1.0.4") (seq "1.11") (dash "2.12.1"))
;; Keywords: browser, tabs, switch
;; URL: https://git.sr.ht/~ngraves/ibrowse-history.el

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

;;; Code:

(require 'json)
(require 'dash)
(require 'seq)
(require 'ibrowse-core)

(eval-when-compile
  (require 'subr-x)
  (require 'url-parse))

;;; Backend

(defvar ibrowse-history-file (concat ibrowse-chromium-default-folder "History")
  "Chrome history SQLite database file.")

(defvar ibrowse-history-sql
  "SELECT title, url, id, last_visit_time FROM urls ORDER BY id DESC LIMIT 100000"
  "The SQL used to extract history.

If you have too many history and worry about the memory use,
consider adjusting the SQL.  For your reference, I have 41525
history items and it takes about 7.4M memory in Emacs.

Don't change \"select url, title, last_visit_time\" part.")

(defun ibrowse-history--apply-sql-command (callback file sql-function)
  (if (zerop
       (call-process "sqlite3" nil t nil
                     "-ascii"
                     file
                     sql-function))
      (funcall callback file)
    (error "Command sqlite3 failed: %s" (buffer-string))))

(defun ibrowse-history--sql-command-read-callback (tmp)
  (let (result)
    (goto-char (point-min))
    ;; -ascii delimited by 0x1F and 0x1E
    (while (re-search-forward (rx (group (+? anything)) "\x1e") nil t)
      (push (split-string (match-string 1) "\x1f") result))
    (delete-file tmp)
    (nreverse result)))

(defun ibrowse-history--extract-fields (callback)
  "Read `ibrowse-history-file'."
  (ibrowse-history-file-check)
  (with-temp-buffer
    (let ((tmp (make-temp-name "ibrowse-history")))
      (copy-file ibrowse-history-file tmp)
      (ibrowse-history--apply-sql-command
       callback
       tmp
       ibrowse-history-sql))))

(defvar ibrowse-history-candidates nil
  "The `ibrowse-history' cache.")

(defun ibrowse-history--get-candidates ()
  "Build candidates."
  (unless ibrowse-history-candidates
    (message "[ibrowse-history] Building cache...")
    (setq ibrowse-history-candidates
          (mapcar
           (pcase-lambda (`(,title ,url ,id ,last-visit-time))
             (let ((display
                    (format "%s | %s"
                            (format-time-string
                             "%Y-%m-%d"
                             (- (/ (string-to-number last-visit-time) 1000000)
                                ;; https://stackoverflow.com/a/26233663/2999892
                                11644473600))
                            title)))
               (list display url id)))
           (ibrowse-history--extract-fields
            #'ibrowse-history--sql-command-read-callback))))
  ibrowse-history-candidates)

(defun ibrowse-history-delete-sql (id)
  (concat "DELETE FROM urls WHERE id=" id ";"
          "DELETE FROM visits WHERE url=" id ";"))

(defun ibrowse-history-delete-item (id)
  "Read `ibrowse-history-file'."
  (ibrowse-history-file-check)
  (with-temp-buffer
    (ibrowse-history--apply-sql-command
     (lambda (file) nil)
     ibrowse-history-file
     (ibrowse-history-delete-sql id)))
  ;; Delete cache.
  (setq ibrowse-history-candidates nil))

;;; Interaction

;;;###autoload
(defun ibrowse-history-browse-url-by-name ()
  "Browse item from history by name."
  (interactive)
  (ibrowse-action-item-by-name
   "Browse item from history by name:"
   #'ibrowse-history--get-candidates
   #'ibrowse-action--first->second
   #'ibrowse-action-open-url))

;;;###autoload
(defun ibrowse-history-copy-url-by-name ()
  "Copy item from history by name."
  (interactive)
  (ibrowse-action-item-by-name
   "Browse item from history by name:"
   #'ibrowse-history--get-candidates
   #'ibrowse-action--first->second
   #'kill-new))

;;;###autoload
(defun ibrowse-history-delete-by-name ()
  "Delete browser item from history by name.

It is currently not possible to delete history items while browsing,
because chromium-based browsers have an EXCLUSIVE lock on the relying
SQlite database."
  (interactive)
  (ibrowse-action-item-by-name
   "Delete browser item from history by name:"
   #'ibrowse-history--get-candidates
   #'ibrowse-action--first->third
   #'ibrowse-history-delete-item))

(provide 'ibrowse-history)
;;; ibrowse-history.el ends here
