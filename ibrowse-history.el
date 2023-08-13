;;; ibrowse-history.el --- Interact with your browser -*- lexical-binding: t -*-

;; Copyright © 2019 Xu Chunyang <mail@xuchunyang.me> (original author, Chromium)
;; Copyright © 2019 Zhu Zihao <all_but_last@163.com> (original author, Firefox)
;; Copyright © 2022, 2023 Nicolas Graves <ngraves@ngraves.fr> (heavy rewrite)

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
;; Dependency: sqlite

;;; Code:

(require 'ibrowse-core)
(require 'ibrowse-sql)

(eval-when-compile
  (require 'subr-x)
  (require 'url-parse))

;;; Settings

(defun ibrowse-history-get-db ()
  "Get the SQLite database file containing history."
  (pcase ibrowse-core-browser
    ('Chromium (concat ibrowse-core-browser-dir "History"))
    ('Firefox (concat ibrowse-core-browser-dir "places.sqlite"))))

(defvar ibrowse-history-db (ibrowse-history-get-db)
  "SQLite database file containing history.")

(defvar ibrowse-history-limit 100000
  "Limit set to the database history extraction.")

(defun ibrowse-history-get-sql ()
  "Get a template of the SQL command used to extract history.

If you have too many history and worry about the memory use,
consider adjusting `ibrowse-history-limit'."
  (pcase ibrowse-core-browser
    ('Chromium ; https://stackoverflow.com/a/26233663/2999892
     "\
SELECT title, url, id, strftime('%%Y-%%m-%%d', last_visit_time/1000000-11644473600,'unixepoch')
FROM urls
ORDER BY id
DESC LIMIT %s;")
    ('Firefox
     "\
SELECT p.title, p.url, p.id, MAX(strftime('%%Y-%%m-%%d', h.visit_date/1000000,'unixepoch')) AS visit_date
FROM moz_historyvisits AS h
INNER JOIN moz_places AS p
WHERE h.place_id = p.id
GROUP BY p.id
ORDER BY visit_date
DESC LIMIT %s;")))

(defvar ibrowse-history-sql (ibrowse-history-get-sql)
  "A template of the SQL command used to extract history.")

(defun ibrowse-history-get-delete-sql ()
  "The SQL command used to delete the item ID from history."
  (pcase ibrowse-core-browser
    ('Chromium
     "\
BEGIN TRANSACTION;
DELETE FROM urls WHERE id = %s;
DELETE FROM visits WHERE id = %s;
COMMIT TRANSACTION;")
    ('Firefox
     "\
BEGIN TRANSACTION;
DELETE FROM moz_places WHERE id = %s;
DELETE FROM moz_historyvisits WHERE place_id = %s;
COMMIT TRANSACTION;")))

(defvar ibrowse-history-delete-sql (ibrowse-history-get-delete-sql)
  "A template of the SQL command used to delete an item from history.")

(defun ibrowse-history-candidate-format (candidate)
  "Format a CANDIDATE from ibrowse-history."
  (cl-destructuring-bind (title url id date) candidate
    (list (format "%s| %s | %s" date title url) url id)))

(defun ibrowse-history--get-candidates ()
  "Wrapper around `ibrowse-sql--get-candidates'."
  (ibrowse-core--file-check 'ibrowse-history-db)
  (ibrowse-sql--get-candidates ibrowse-history-db
                               (format ibrowse-history-sql ibrowse-history-limit)
                               #'ibrowse-history-candidate-format))

;;; Interaction
(defun ibrowse-history-completing-read (prompt)
  "Wrapper around `ibrowse-core-completing-read' with PROMPT."
  (ibrowse-core-completing-read prompt
                                #'ibrowse-history--get-candidates
                                'ibrowse-history))

;;;###autoload
(defun ibrowse-history-browse-url (item)
  "Select and browse url from ITEM in history."
  (interactive
   (list (ibrowse-history-completing-read "Browse from history:")))
  (browse-url (cadr item)))

;;;###autoload
(defun ibrowse-history-copy-url (item)
  "Select and copy url from ITEM in history."
  (interactive
   (list (ibrowse-history-completing-read "Copy url from history:")))
  (kill-new (cadr item)))

;;;###autoload
(defun ibrowse-history-insert-link (item)
  "Insert link from ITEM in history.
See `ibrowse-core--insert-link' for more details."
  (interactive
   (list (ibrowse-history-completing-read "Insert link from history:")))
  (ibrowse-core--insert-link item))

;;;###autoload
(defun ibrowse-history-delete (item)
  "Select and delete browser ITEM from history.

It is currently not possible to delete history items while browsing,
because web browsers have an EXCLUSIVE lock on their SQlite database."
  (interactive
   (list (ibrowse-history-completing-read "Delete item from history:")))
  (ibrowse-core--file-check 'ibrowse-history-db)
  (let ((id (caddr item)))
    (with-temp-buffer
      (ibrowse-sql--apply-command ibrowse-history-db
                                  (format ibrowse-history-delete-sql id id))))
  ;; Delete cache.
  (setq ibrowse-sql-candidates nil))

;;; Embark

(defvar ibrowse-history-embark-actions
  (let ((map (make-sparse-keymap)))
    (define-key map "b" #'ibrowse-history-browse-url)
    (define-key map "d" #'ibrowse-history-delete)
    (define-key map "u" #'ibrowse-history-copy-url)
    (define-key map "i" #'ibrowse-history-insert-link)
    map)
    "Keymap for actions for browser history items.")

(defvar embark-keymap-alist)
(with-eval-after-load 'embark
  (add-to-list
   'embark-keymap-alist
   '(ibrowse-history . ibrowse-history-embark-actions)))

(defun ibrowse-history-update-browser! ()
  "Update variables if you have changed your current browser.

More precisely, this function updates `ibrowse-sql-candidates' and
`ibrowse-history-db'."
  (setq ibrowse-history-db (ibrowse-history-get-db))
  (setq ibrowse-history-sql (ibrowse-history-get-sql))
  (setq ibrowse-history-delete-sql (ibrowse-history-get-delete-sql))
  (setq ibrowse-sql-candidates nil))

(add-hook 'ibrowse-core-update-hook 'ibrowse-history-update-browser!)

(provide 'ibrowse-history)
;;; ibrowse-history.el ends here
