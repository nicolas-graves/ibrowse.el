;;; ibrowse-core.el --- Interact with your browser -*- lexical-binding: t -*-

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

(require 'seq)
(require 'cl-lib)

;;; Variables

(defgroup ibrowse nil
  "Group for ibrowse customizations."
  :group 'applications
  :prefix "ibrowse-")

(defconst ibrowse-core--cdp-debugging-port
  "9222")

(defcustom ibrowse-core-browser nil
  "The browser choice for ibrowse.

This variable can take one of the three symbols: 'Chromium, 'Firefox or nil.
When nil, the most recently used profile (Chromium or Firefox) will be chosen."
  :type '(choice (const :tag "Chromium" Chromium)
                 (const :tag "Firefox" Firefox)
                 (const :tag "Auto" nil))
  :group 'ibrowse)

(defun ibrowse-core--cdp-url (query)
  "Return the url of the chromium developer protocol QUERY."
  (format "http://localhost:%s/json/%s"
          ibrowse-core--cdp-debugging-port
          query))

(defvar ibrowse-core-chromium-profile "Default"
  "Name of the Chromium profile to use.")

(defun ibrowse-core-guess-db-dir ()
  "Guess the directory containing main database files.

These main database files are `History' and `Bookmarks' in the case of
Chromium, `places.sqlite' in the case of Firefox.  By default, the
chosen directory will be the most recently used profile."
  (let* ((chromium-dirlist
          (seq-filter
           (lambda (p)
             (file-exists-p
              (substitute-in-file-name
               (concat p "/" ibrowse-core-chromium-profile "/History"))))
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
             "/" ibrowse-core-chromium-profile "/")))
         (firefox-directory
          (when (or (eq ibrowse-core-browser 'Firefox) (not ibrowse-core-browser))
            (concat
             (car (seq-sort #'file-newer-than-file-p firefox-dirlist)) "/"))))
    (cond
     ((and chromium-directory firefox-directory)
      (if (file-newer-than-file-p
           (concat chromium-directory "History")
           (concat firefox-directory "places.sqlite"))
          chromium-directory
        firefox-directory))
     (chromium-directory chromium-directory)
     (firefox-directory firefox-directory)
     (t (user-error "The browser database directory is not found!")))))

(defvar ibrowse-core-db-dir (ibrowse-core-guess-db-dir)
  "Browser database directory.")

;;; Functions

(defun ibrowse-core--file-check (file)
  "Check if FILE exists."
  (pcase (concat ibrowse-core-db-dir file)
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

(defun ibrowse-core-act (prompt get-candidates action categ)
  "GET-CANDIDATES using PROMPT and call the function ACTION on the \
selected item."
  (let* ((candidates (funcall get-candidates))
         (selected   (completing-read
                      prompt
                      (lambda (string predicate action)
                        (if (eq action 'metadata)
                            `(metadata (category . ,categ))
                          (complete-with-action
                           action candidates string predicate))))))
    (pcase (assoc selected candidates)
      (`(,title ,url ,id)
       (funcall action title url id)))))

(provide 'ibrowse-core)
;;; ibrowse-core.el ends here
