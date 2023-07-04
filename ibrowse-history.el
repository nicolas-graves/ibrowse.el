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
(require 'emacsql-compiler)

(eval-when-compile
  (require 'subr-x)
  (require 'url-parse))

;;; Backend

(defun ibrowse-history-guess-file ()
  "Guess the sql file containing history."
  (let* ((history-file (concat ibrowse-core-db-dir "History"))
         (places-file (concat ibrowse-core-db-dir "places.sqlite")))
    (cond ((file-exists-p history-file) history-file)
          ((file-exists-p places-file) places-file)
          (t (user-error "The history file has not been found!")))))

(defvar ibrowse-history-file (ibrowse-history-guess-file)
  "Browser history SQLite database file.")

(defvar ibrowse-history--temp-db-path
  (expand-file-name (make-temp-name "ibrowse-db") temporary-file-directory))

(defvar ibrowse-history-candidates nil
  "The `ibrowse-history' alist cache.")

(defun ibrowse-history--ensure-db! (&optional force-update?)
  "Ensure database by copying it to system temp file directory with a temp name.

If FORCE-UPDATE? is non-nil and database was copied, delete it first."
  (cl-flet ((update-db! ()
              ;; The copy is necessary because our SQL query action
              ;; may conflict with running browser.
              (copy-file ibrowse-history-file
                         ibrowse-history--temp-db-path)
              (setq ibrowse-history-candidates nil)))
    (let* ((path ibrowse-history--temp-db-path))
      (if (file-exists-p path)
          (when force-update?
            (delete-file path)
            (update-db!))
        (update-db!))
      nil)))

(defsubst ibrowse-history--prepare-sql-stmt (sql-args-list)
  "Prepare a series of SQL commands.
SQL-ARGS-LIST should be a list of SQL command s-expressions SQL DSL.
Returns a single string of SQL commands separated by semicolons."
  (mapconcat
   (lambda (sql-args)
     (concat (emacsql-format (emacsql-prepare sql-args)) ";"))
   sql-args-list
   "\n"))

(defvar ibrowse-history-limit 100000
  "Limit set to the database history extraction.")

(defun ibrowse-history-sql ()
  "The SQL command used to extract history.

If you have too many history and worry about the memory use,
consider adjusting the SQL."
  (pcase ibrowse-core-browser
    ('Chromium `([:select [title url id last_visit_time]
                  :from urls
                  :order-by (desc id)
                  :limit ,ibrowse-history-limit]))
    ('Firefox  `([:select [p:title p:url p:id h:visit_date]
                  :from (as moz_historyvisits h)
                  :inner-join (as moz_places p)
                  :where (= h:place_id p:id)
                  :order-by (desc h:visit_date)
                  :limit ,ibrowse-history-limit]))))

(defun ibrowse-history-delete-sql (id)
  "The SQL command used to delete the item ID from history."
  (let ((num-id (string-to-number id)))
    (pcase ibrowse-core-browser
      ('Chromium `([:delete :from urls :where (= id ,num-id)]
                   [:delete :from visits :where (= url ,num-id)]))
      ('Firefox `([:delete :from moz_places :where (= id ,num-id)]
                  [:delete :from moz_historyvisits :where (= place_id ,num-id)])))))

(defun ibrowse-history--apply-sql-command (callback file queries)
  "Apply the SQL QUERIES list using the SQL FILE, then call CALLBACK."
  (let ((sql-command (ibrowse-history--prepare-sql-stmt queries)))
    (if (zerop (call-process "sqlite3" nil t nil "-ascii" file sql-command))
        (funcall callback file)
      (error "Command sqlite3 failed: %s: %s" sql-command (buffer-string)))))

(defun ibrowse-history--sql-command-read-callback (_)
  "Function applied to the result of the SQL query."
  (let (result)
    (goto-char (point-min))
    ;; -ascii delimited by 0x1F and 0x1E
    (while (re-search-forward (rx (group (+? anything)) "\x1e") nil t)
      (push (split-string (match-string 1) "\x1f") result))
    (nreverse result)))

(defun ibrowse-history--extract-fields (callback)
  "Read `ibrowse-history-file' and call the CALLBACK function."
  (ibrowse-core--file-check ibrowse-history-file "ibrowse-history-file")
  (ibrowse-history--ensure-db!)
  (with-temp-buffer
    (ibrowse-history--apply-sql-command
     callback
     ibrowse-history--temp-db-path
     (ibrowse-history-sql))))

(defun ibrowse-history-format (date-in-ms &rest rest)
  "Format DATE-IN-MS with additional REST variables for `completing-read'."
  (let* ((date (/ (string-to-number date-in-ms) 1000000))
         (date (pcase ibrowse-core-browser
                 ('Chromium (- date 11644473600)) ; https://stackoverflow.com/a/26233663/2999892
                 ('Firefox date))))
    (format "%s| %s" (format-time-string "%F"  date) (string-join rest " | "))))

(defun ibrowse-history--get-candidates ()
  "Build candidates."
  (unless ibrowse-history-candidates
    (message "[ibrowse-history] Building cache...")
    (setq ibrowse-history-candidates
          (mapcar
           (pcase-lambda (`(,title ,url ,id ,last-visit-time))
             (list (ibrowse-history-format last-visit-time title url) url id))
           (ibrowse-history--extract-fields
            #'ibrowse-history--sql-command-read-callback))))
  ibrowse-history-candidates)

;;; Interaction

(defun ibrowse-history-delete-item (_title _url id)
  "Delete browser ID item using sqlite."
  (ibrowse-core--file-check ibrowse-history-file "ibrowse-history-file")
  (with-temp-buffer
    (ibrowse-history--apply-sql-command
     (lambda (_) nil)
     ibrowse-history-file
     (ibrowse-history-delete-sql id)))
  ;; Delete cache.
  (ibrowse-history--ensure-db! t))

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

(provide 'ibrowse-history)
;;; ibrowse-history.el ends here
