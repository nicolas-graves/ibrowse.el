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

(defun ibrowse-sql-guess-db-dir ()
  "Guess the directory containing main database files.

These main database files are `History' and `Bookmarks' in the case of
Chromium, `places.sqlite' in the case of Firefox.  By default, the
chosen directory will be the most recently used profile."
  (let* ((chromium-dirlist
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
             "~/AppData/Local/Google/Chrome/User Data/")))
         (firefox-dirlist
          (append
           (file-expand-wildcards "~/.mozilla/firefox/*.default")
           (file-expand-wildcards
            (expand-file-name "Mozilla/Firefox/Profiles/*"
                              (getenv "APPDATA")))))
         (chromium-directory
          (when (or (eq ibrowse-core-browser 'Chromium) (not ibrowse-core-browser))
            (concat
             (expand-file-name
              (car (seq-sort #'file-newer-than-file-p chromium-dirlist))
              (getenv "HOME"))
             "/" ibrowse-sql-chromium-profile "/")))
         (firefox-directory
          (when (or (eq ibrowse-core-browser 'Firefox) (not ibrowse-core-browser))
            (concat
             (expand-file-name
              (car (seq-sort #'file-newer-than-file-p firefox-dirlist))
              (getenv "HOME"))
             "/"))))
    (cond
     ((and chromium-directory firefox-directory)
      (if (file-newer-than-file-p
           (concat chromium-directory "History")
           (concat firefox-directory "places.sqlite"))
          (progn
            (setq ibrowse-core-browser 'Chromium)
            chromium-directory)
        (progn
          (setq ibrowse-core-browser 'Firefox)
          firefox-directory)))
     (chromium-directory chromium-directory)
     (firefox-directory firefox-directory)
     (t (user-error "The browser database directory is not found!")))))

(defvar ibrowse-sql-db-dir (ibrowse-sql-guess-db-dir)
  "Browser database directory.")

(defsubst ibrowse-sql--prepare-stmt (sql-args-list)
  "Prepare a series of SQL commands.
SQL-ARGS-LIST should be a list of SQL command s-expressions SQL DSL.
Returns a single string of SQL commands separated by semicolons."
  (mapconcat
   (lambda (sql-args)
     (concat (emacsql-format (emacsql-prepare sql-args)) ";"))
   sql-args-list
   "\n"))

(defun ibrowse-sql--apply-command (callback file queries)
  "Apply the SQL QUERIES list using the SQL FILE, then call CALLBACK."
  (let ((sql-command (ibrowse-sql--prepare-stmt queries)))
    (if (zerop (call-process "sqlite3" nil t nil "-ascii" file sql-command))
        (funcall callback file)
      (error "Command sqlite3 failed: %s: %s" sql-command (buffer-string)))))

(defun ibrowse-sql--read-callback (_)
  "Function applied to the result of the SQL query."
  (let (result)
    (goto-char (point-min))
    ;; -ascii delimited by 0x1F and 0x1E
    (while (re-search-forward (rx (group (+? anything)) "\x1e") nil t)
      (push (split-string (match-string 1) "\x1f") result))
    (nreverse result)))

(provide 'ibrowse-sql)
;;; ibrowse-sql.el ends here
