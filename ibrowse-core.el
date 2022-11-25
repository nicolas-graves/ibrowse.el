;; ibrowse-core.el --- Interact with your browser from Emacs -*- lexical-binding: t -*-

;; Copyright © 2022 Nicolas Graves <ngraves@ngraves.fr>
;; Some snippets have been written by
;; Copyright © 2021 BlueBoxWare
;; Copyright © 2016 Taichi Kawabata <kawabata.taichi@gmail.com>

;; Author: Nicolas Graves <ngraves@ngraves.fr>
;; Version: 0.0.0
;; Package-Requires: ((emacs "25.1"))
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

;;; Variables

(defconst ibrowse-core--cdp-debugging-port
  "9222")

(defun ibrowse-core--cdp-url (query)
  "Return the url of the chromium developer protocol QUERY."
  (format "http://localhost:%s/json/%s"
          ibrowse-core--cdp-debugging-port
          query))

;; Change this variable to use another profile.
(defvar ibrowse-core-default-folder-name "Default")

(defvar ibrowse-core-base-folder-list
  '("~/.config/google-chrome"
    "~/.config/chromium"
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
    "~/AppData/Local/Google/Chrome/User Data/Default/"))

(defun ibrowse-core-guess-default-folder ()
  "Guess the folder containing the History and Bookmarks files."
  (concat
   (car
    (seq-sort
     #'file-newer-than-file-p
     (seq-filter
      (lambda (p)
        (substitute-in-file-name
         (concat p "/" ibrowse-core-default-folder-name "/History")))
      ibrowse-core-base-folder-list)))
   "/" ibrowse-core-default-folder-name "/"))

(defvar ibrowse-core-default-folder (ibrowse-core-guess-default-folder)
  "Chromium-based browsers profile folder.")

;;; Functions

(defun ibrowse-core--file-check (file)
  "Check if FILE exists."
  (pcase (concat ibrowse-core-default-folder file)
    ('nil (user-error "`ibrowse-history-file' is not set"))
    ((pred file-exists-p) nil)
    (f (user-error "'%s' doesn't exist, please reset `ibrowse-history-file'" f))))

(defun ibrowse-core--copy-url (_title url _id)
  "Action to copy URL."
  (kill-new url))

(defun ibrowse-core--browse-url (_title url _id)
  "Action to browse URL."
  (browse-url url))

(defun ibrowse-core--insert-url (_title url _id)
  "Insert URL in the current buffer."
  (with-current-buffer (insert url)))

(defun ibrowse-core--insert-org-link (title url _id)
  "Insert TITLE and URL as Org link if `org-insert-link' is available.
Does nothing if `org-insert-link' is unavailable."
  (if (fboundp 'org-insert-link)
      (with-current-buffer (org-insert-link '() url title))))

(defun ibrowse-core--insert-markdown-link (title url)
  "Insert TITLE and URL as markdown link.
Does nothing if `markdown-insert-inline-link' is unavailable."
  (if (fboundp 'markdown-insert-inline-link)
      (with-current-buffer (markdown-insert-inline-link title url))))

(defun ibrowse-core-act-by-name (prompt get-candidates action)
  "GET-CANDIDATES using PROMPT and call the function ACTION on the \
selected item."
  (let* ((candidates (funcall get-candidates))
         (selected   (completing-read prompt candidates)))
    (pcase (assoc selected candidates)
      (`(,title ,url ,id)
       (funcall action title url id)))))

(provide 'ibrowse-core)
;;; ibrowse-core.el ends here
