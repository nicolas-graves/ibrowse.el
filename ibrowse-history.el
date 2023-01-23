;;; ibrowse-history.el --- Interact with your browser -*- lexical-binding: t -*-

;; Copyright © 2019 Xu Chunyang <mail@xuchunyang.me> (original author)
;; Copyright © 2022 Nicolas Graves <ngraves@ngraves.fr> (heavy rewrite and adding deletions)

;; Author: Nicolas Graves <ngraves@ngraves.fr>
;; Version: 0.1.6
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
;; Dependency: sqlite

;;; Code:

(require 'ibrowse-core)

(eval-when-compile
  (require 'subr-x)
  (require 'url-parse))

;;; Backend

(defvar ibrowse-history-file (concat ibrowse-core-default-folder "History")
  "Chrome history SQLite database file.")

(defvar ibrowse-history-sql
  "SELECT title, url, id, last_visit_time FROM urls ORDER BY id DESC LIMIT 100000"
  "The SQL command used to extract history.

If you have too many history and worry about the memory use,
consider adjusting the SQL.

Don't change \"select title, url, id, last_visit_time\" part.")

(defun ibrowse-history-delete-sql (id)
  "The SQL command used to delete the item ID from history."
  (concat "DELETE FROM urls WHERE id=" id ";"
          "DELETE FROM visits WHERE url=" id ";"))

(defun ibrowse-history--apply-sql-command (callback file sql-function)
  "Apply the SQL-FUNCTION command using the SQL FILE, then call CALLBACK."
  (if (zerop
       (call-process "sqlite3" nil t nil
                     "-ascii"
                     file
                     sql-function))
      (funcall callback file)
    (error "Command sqlite3 failed: %s" (buffer-string))))

(defun ibrowse-history--sql-command-read-callback (file)
  "Function applied to the result of the SQL query, using the file FILE."
  (let (result)
    (goto-char (point-min))
    ;; -ascii delimited by 0x1F and 0x1E
    (while (re-search-forward (rx (group (+? anything)) "\x1e") nil t)
      (push (split-string (match-string 1) "\x1f") result))
    (delete-file file)
    (nreverse result)))

(defun ibrowse-history--extract-fields (callback)
  "Read `ibrowse-history-file' and call the CALLBACK function."
  (ibrowse-core--file-check "History")
  (with-temp-buffer
    (let ((tmp (make-temp-name "ibrowse-history")))
      (copy-file ibrowse-history-file tmp)
      (ibrowse-history--apply-sql-command
       callback
       tmp
       ibrowse-history-sql))))

(defun ibrowse-history-format-title (title last-visit-time)
  "Format TITLE and LAST-VISIT-TIME for `completing-read'."
  (format "%s | %s"
          (format-time-string
           "%F" (- (/ (string-to-number last-visit-time) 1000000)
                   ;; https://stackoverflow.com/a/26233663/2999892
                   11644473600))
          title))

(defvar ibrowse-history-candidates nil
  "The `ibrowse-history' cache.")

(defun ibrowse-history--get-candidates ()
  "Build candidates."
  (unless ibrowse-history-candidates
    (message "[ibrowse-history] Building cache...")
    (setq ibrowse-history-candidates
          (mapcar
           (pcase-lambda (`(,title ,url ,id ,last-visit-time))
             (list (ibrowse-history-format-title title last-visit-time) url id))
           (ibrowse-history--extract-fields
            #'ibrowse-history--sql-command-read-callback))))
  ibrowse-history-candidates)

;;; Interaction

(defun ibrowse-history-delete-item (_title _url id)
  "Delete browser ID item using sqlite."
  (ibrowse-core--file-check "History")
  (with-temp-buffer
    (ibrowse-history--apply-sql-command
     (lambda (_) nil)
     ibrowse-history-file
     (ibrowse-history-delete-sql id)))
  ;; Delete cache.
  (setq ibrowse-history-candidates nil))

(defun ibrowse-history-act (prompt action)
  "Wrapper transmitting arguments PROMPT and ACTION to \
`ibrowse-core-act' for `ibrowse-history'."
  (ibrowse-core-act prompt
                    #'ibrowse-history--get-candidates
                    action
                    'ibrowse-history))

;;;###autoload
(defun ibrowse-history-browse-url ()
  "Select and browse item from history."
  (interactive)
  (ibrowse-history-act
   "Browse from browser history:"
   #'ibrowse-core--browse-url))

;;;###autoload
(defun ibrowse-history-copy-url ()
  "Select and copy url from history."
  (interactive)
  (ibrowse-history-act
   "Copy url from browser history:"
   #'ibrowse-core--copy-url))

;;;###autoload
(defun ibrowse-history-insert-org-link ()
  "Insert org-link from history."
  (interactive)
  (ibrowse-history-act
   "Insert org-link from browser history:"
   #'ibrowse-core--insert-org-link))

;;;###autoload
(defun ibrowse-history-insert-markdown-link ()
  "Insert markdown-link from history."
  (interactive)
  (ibrowse-history-act
   "Insert markdown-link from browser history:"
   #'ibrowse-core--insert-markdown-link))

;;;###autoload
(defun ibrowse-history-delete ()
  "Select and delete browser item from history.

It is currently not possible to delete history items while browsing,
because chromium-based browsers have an EXCLUSIVE lock on the relying
SQlite database."
  (interactive)
  (ibrowse-history-act
   "Delete item from browser history:"
   #'ibrowse-history-delete-item))

;;; Embark

(defvar ibrowse-history-embark-actions
  (let ((map (make-sparse-keymap)))
    (define-key map "b" #'ibrowse-history-browse-url)
    (define-key map "d" #'ibrowse-history-delete)
    (define-key map "u" #'ibrowse-history-copy-url)
    (define-key map "o" #'ibrowse-history-insert-org-link)
    (define-key map "m" #'ibrowse-history-insert-markdown-link)
    map)
    "Keymap for actions for browser history items.")

(defvar embark-keymap-alist)
(with-eval-after-load 'embark
  (add-to-list
   'embark-keymap-alist
   '(ibrowse-history . ibrowse-history-embark-actions)))

(provide 'ibrowse-history)
;;; ibrowse-history.el ends here
