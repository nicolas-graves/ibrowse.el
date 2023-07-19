;;; ibrowse-history.el --- Interact with your browser -*- lexical-binding: t -*-

;; Copyright © 2019 Xu Chunyang <mail@xuchunyang.me> (original author, Chromium)
;; Copyright © 2019 Zhu Zihao <all_but_last@163.com> (original author, Firefox)
;; Copyright © 2022, 2023 Nicolas Graves <ngraves@ngraves.fr> (heavy rewrite)

;; Author: Nicolas Graves <ngraves@ngraves.fr>
;; Version: 0.1.8
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
  (pcase ibrowse-browser
    ('Chromium (concat ibrowse-browser-dir "History"))
    ('Firefox (concat ibrowse-browser-dir "places.sqlite"))))

(defvar ibrowse-history-db (ibrowse-history-get-db)
  "SQLite database file containing history.")

(defvar ibrowse-history-limit 100000
  "Limit set to the database history extraction.")

;;; Backend

(defun ibrowse-history-sql ()
  "The SQL command used to extract history.

If you have too many history and worry about the memory use,
consider adjusting `ibrowse-history-limit'."
  (let ((limit (number-to-string ibrowse-history-limit)))
    (pcase ibrowse-browser
      ('Chromium
       (concat
        "SELECT title, url, id, " ; https://stackoverflow.com/a/26233663/2999892
        "strftime('%Y-%m-%d', last_visit_time/1000000-11644473600,'unixepoch') "
        "FROM urls "
        "ORDER BY id "
        "DESC LIMIT " limit ";"))
      ('Firefox
       (concat
        "SELECT p.title, p.url, p.id, "
        "MAX(strftime('%Y-%m-%d', h.visit_date/1000000,'unixepoch')) AS visit_date "
        "FROM moz_historyvisits AS h "
        "INNER JOIN moz_places AS p "
        "WHERE h.place_id = p.id "
        "GROUP BY p.id "
        "ORDER BY visit_date "
        "DESC LIMIT " limit ";")))))

(defun ibrowse-history-delete-sql (id)
  "The SQL command used to delete the item ID from history."
  (pcase ibrowse-browser
    ('Chromium
     (concat
      "BEGIN TRANSACTION; "
      "DELETE FROM urls WHERE id = " id "; "
      "DELETE FROM visits WHERE id = " id "; "
      "COMMIT TRANSACTION;"))
    ('Firefox
     (concat
      "BEGIN TRANSACTION; "
      "DELETE FROM moz_places WHERE id = " id "; "
      "DELETE FROM moz_historyvisits WHERE place_id = " id "; "
      "COMMIT TRANSACTION;"))))

(defun ibrowse-history-candidate-format (candidate)
  "Format a CANDIDATE from ibrowse-history."
  (cl-destructuring-bind (title url id date) candidate
    (list (format "%s| %s | %s" date title url) url id)))

(defun ibrowse-history--get-candidates ()
  "Wrapper around `ibrowse-sql--get-candidates'."
  (ibrowse-core--file-check 'ibrowse-history-db)
  (ibrowse-sql--get-candidates ibrowse-history-db
                               #'ibrowse-history-sql
                               #'ibrowse-history-candidate-format))

;;; Interaction

(defun ibrowse-history-delete-item (_title _url id)
  "Delete browser ID item using sqlite."
  (ibrowse-core--file-check 'ibrowse-history-db)
  (with-temp-buffer
    (ibrowse-sql--apply-command ibrowse-history-db
                                (ibrowse-history-delete-sql id)))
  ;; Delete cache.
  (setq ibrowse-sql-candidates nil))

(defun ibrowse-history-act (prompt action)
  "Wrapper transmitting PROMPT and ACTION to `ibrowse-core-act'."
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

(defun ibrowse-history-update-browser! ()
  "Update variables if you have changed your current browser.

More precisely, this function updates `ibrowse-sql-candidates' and
`ibrowse-history-db'."
  (setq ibrowse-history-db (ibrowse-history-get-db))
  (setq ibrowse-sql-candidates nil))

(add-hook 'ibrowse-update-hook 'ibrowse-history-update-browser!)

(provide 'ibrowse-history)
;;; ibrowse-history.el ends here
