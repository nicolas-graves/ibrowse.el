;;; ibrowse-sql.el --- Interact with your browser -*- lexical-binding: t -*-

;; Copyright © 2022 Nicolas Graves <ngraves@ngraves.fr>
;; Some snippets have been written by
;; Copyright © 2021 BlueBoxWare
;; Copyright © 2016 Taichi Kawabata <kawabata.taichi@gmail.com>

;; Author: Nicolas Graves <ngraves@ngraves.fr>
;; Version: 0.1.4
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

;;; Code:

(require 'ibrowse-core)
(require 'emacsql-compiler)

;;; Variables

(defvar ibrowse-sql-chromium-profile "Default"
  "Name of the Chromium profile to use.")

(defvar ibrowse-sql-candidates nil
  "The ibrowse alist cache.")

(defun ibrowse-sql-get-chromium-dir ()
  "Try to get the Chromium data directory."
  (let ((chromium-dirlist
         (seq-filter
          (lambda (p)
            (file-exists-p
             (substitute-in-file-name
              (concat p "/" ibrowse-sql-chromium-profile "/History"))))
          '("~/.config/chromium"
            "~/.config/google-chrome"
            "$LOCALAPPDATA/Google/Chrome/User Data"
            "$LOCALAPPDATA/Chromium/User Data"
            "$USERPROFILE/Local Settings/Application Data/Google/Chrome/User Data"
            "$USERPROFILE/Local Settings/Application Data/Chromium/User Data"
            "$LOCALAPPDATA/Microsoft/Edge/User Data"
            "$USERPROFILE/Local Settings/Application Data/Microsoft/Edge/User Data"
            "~/Library/Application Support/Google/Chrome"
            "~/Library/Application Support/Chromium"
            "~/.config/vivaldi"
            "$LOCALAPPDATA/Vivaldi/User Data"
            "$USERPROFILE/Local Settings/Application Data/Vivaldi/User Data"
            "~/Library/Application Support/Vivaldi"
            "~/AppData/Local/Google/Chrome/User Data/"))))
    (concat
     (expand-file-name
      (car (seq-sort #'file-newer-than-file-p chromium-dirlist))
      (getenv "HOME"))
     "/" ibrowse-sql-chromium-profile "/")))

(defun ibrowse-sql-get-firefox-dir ()
  "Try to get the Firefox data directory."
  (let* ((firefox-dirlist
          (append
           (file-expand-wildcards "~/.mozilla/firefox/*.default")
           (file-expand-wildcards
            (expand-file-name "Mozilla/Firefox/Profiles/*"
                              (getenv "APPDATA"))))))
    (concat
     (expand-file-name
      (car (seq-sort #'file-newer-than-file-p firefox-dirlist))
      (getenv "HOME"))
     "/")))

(defun ibrowse-sql--most-recent (file-list)
  "Return the most recent edit time from FILE-LIST."
  (apply #'max
           (mapcar (lambda (f)
                     (float-time
                      (file-attribute-modification-time (file-attributes f))))
                   file-list)))

(defun ibrowse-sql-guess-db-dir! ()
  "Guess the directory containing main database files.

These main database files are `History' and `Bookmarks' in the case of
Chromium, `places.sqlite' in the case of Firefox.  By default, the
chosen directory will be the most recently used profile."
  (let* ((chromium-dir (ibrowse-sql-get-chromium-dir))
         (firefox-dir (ibrowse-sql-get-firefox-dir)))
    (if (and chromium-dir firefox-dir)
        (if (let* ((chromium-files (list (concat chromium-dir "History")
                                         (concat chromium-dir "History-journal")))
                   (firefox-files
                    (delq nil
                          (list (concat firefox-dir "places.sqlite")
                                (let ((f (concat firefox-dir "places.sqlite-wal")))
                                  (if (file-exists-p f) f nil)))))
                   (chromium-latest (ibrowse-sql--most-recent chromium-files))
                   (firefox-latest (ibrowse-sql--most-recent firefox-files)))
              (> chromium-latest firefox-latest))
            (progn (setq ibrowse-browser 'Chromium)
                   chromium-dir)
          (progn (setq ibrowse-browser 'Firefox)
                 firefox-dir))
      (user-error "The browser database directory is not found!"))))

(defun ibrowse-sql-get-db-dir ()
  "Get the directory containing main database files."
  (pcase ibrowse-browser
    ('Chromium (ibrowse-sql-get-chromium-dir))
    ('Firefox (ibrowse-sql-get-firefox-dir))
    (_ (ibrowse-sql-guess-db-dir!))))

(defvar ibrowse-sql-db-dir (ibrowse-sql-guess-db-dir!)
  "Browser database directory.")

(defun ibrowse-update-browser ()
  "Update variables if you have changed your current browser.

More precisely, it updates `ibrowse-sql-db-dir' and
`ibrowse-browser' variables."
  (interactive)
  (setq ibrowse-sql-db-dir (ibrowse-sql-guess-db-dir!))
  (setq ibrowse-sql-candidates nil))

(defun ibrowse-sql--ensure-db (file tempfile &optional force-update?)
  "Ensure database by copying FILE to TEMPFILE.

If FORCE-UPDATE? is non-nil and database was copied, delete it first."
  (cl-flet ((update-db ()
              ;; The copy is necessary because our SQL query action
              ;; may conflict with running browser.
              (copy-file file tempfile)))
    (if (file-exists-p tempfile)
        (when force-update?
          (delete-file tempfile)
          (update-db))
      (update-db))
    nil))

(defsubst ibrowse-sql--prepare-stmt (sql-args-list)
  "Prepare a series of SQL commands.
SQL-ARGS-LIST should be a list of SQL command s-expressions SQL DSL.
Returns a single string of SQL commands separated by semicolons."
  (mapconcat
   (lambda (sql-args)
     (concat (emacsql-format (emacsql-prepare sql-args)) ";"))
   sql-args-list
   "\n"))

(defun ibrowse-sql--apply-command (file queries &optional callback)
  "Apply the SQL QUERIES list using the SQL FILE, then call CALLBACK."
  (let ((sql-command (ibrowse-sql--prepare-stmt queries)))
    (if (zerop (call-process "sqlite3" nil t nil "-ascii" file sql-command))
        (when callback
          (funcall callback file))
      (error "Command sqlite3 failed: %s: %s" sql-command (buffer-string)))))

(defun ibrowse-sql--read-callback (_)
  "Function applied to the result of the SQL query."
  (let (result)
    (goto-char (point-min))
    ;; -ascii delimited by 0x1F and 0x1E
    (while (re-search-forward (rx (group (+? anything)) "\x1e") nil t)
      (push (split-string (match-string 1) "\x1f") result))
    (nreverse result)))

(defun ibrowse-sql--extract-fields (db temp-db query varname &optional callback)
  "QUERY the database TEMP-DB, and call the CALLBACK function.

VARNAME is used to carry the name of the origin of the call, for debug
purposes.  DB is the original database.  Nothing is done when CALLBACK
is not provided."
  (ibrowse-core--file-check db varname)
  (ibrowse-sql--ensure-db db temp-db)
  (setq ibrowse-sql-candidates nil)
  (with-temp-buffer
    (ibrowse-sql--apply-command temp-db (funcall query) callback)))

(defun ibrowse-sql--get-candidates
    (db temp-db query varname &optional candidate-format)
  "Build candidates and format them with CANDIDATE-FORMAT.

The candidates are generated by calling `ibrowse-sql--extract-fields'
with arguments DB, TEMP-DB, QUERY and VARNAME."
  (unless ibrowse-sql-candidates
    (message "[ibrowse] Building cache...")
    (let ((candidates (ibrowse-sql--extract-fields db temp-db query varname
                                                   #'ibrowse-sql--read-callback)))
      (setq ibrowse-sql-candidates
            (if candidate-format
                (mapcar candidate-format candidates)
              candidates))))
  ibrowse-sql-candidates)

(provide 'ibrowse-sql)
;;; ibrowse-sql.el ends here
