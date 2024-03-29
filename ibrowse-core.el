;;; ibrowse-core.el --- Interact with your browser -*- lexical-binding: t -*-

;; Copyright © 2022 Nicolas Graves <ngraves@ngraves.fr>
;; Some snippets have been written by
;; Copyright © 2021 BlueBoxWare
;; Copyright © 2016 Taichi Kawabata <kawabata.taichi@gmail.com>

;; Author: Nicolas Graves <ngraves@ngraves.fr>
;; Version: 0.2.3
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

;;; Code:

(require 'seq)
(require 'cl-lib)
(autoload 'org-insert-link "org")
(autoload 'markdown-insert-inline-link "markdown")

;;; Variables

(defgroup ibrowse-core nil
  "Group for ibrowse customizations."
  :group 'applications
  :prefix "ibrowse-core")

(defcustom ibrowse-core-browser nil
  "The browser choice for ibrowse.

This variable can take one of the three symbols: 'Chromium, 'Firefox or nil.
When nil, the most recently used profile (Chromium or Firefox) will be chosen."
  :type '(choice (const :tag "Chromium" Chromium)
                 (const :tag "Firefox" Firefox)
                 (const :tag "Auto" nil))
  :group 'ibrowse-core)

(defcustom ibrowse-core-browser-dir nil
  "The data directory of `ibrowse-core-browser'."
  :type '(choice (string :tag "String value")
                 (const :tag "No value" nil))
  :group 'ibrowse-core)

(defcustom ibrowse-core-update-hook nil
  "Hooks to run when `ibrowse-core-update-browser' is invoked."
  :type 'hook
  :group 'ibrowse-core)

(defvar ibrowse-core-chromium-profile "Default"
  "Name of the Chromium profile to use.")

;;; Functions

(defun ibrowse-core-get-chromium-dir ()
  "Try to get the Chromium data directory."
  (let ((chromium-dirlist
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
            "~/AppData/Local/Google/Chrome/User Data/"))))
    (when chromium-dirlist
      (concat
       (expand-file-name
        (car (seq-sort #'file-newer-than-file-p chromium-dirlist))
        (getenv "HOME"))
       "/" ibrowse-core-chromium-profile "/"))))

(defun ibrowse-core-get-firefox-dir ()
  "Try to get the Firefox data directory."
  (let* ((firefox-dirlist
          (append
           (file-expand-wildcards "~/.mozilla/firefox/*default")
           (file-expand-wildcards
            (expand-file-name "Mozilla/Firefox/Profiles/*"
                              (getenv "APPDATA"))))))
    (when firefox-dirlist
      (concat
       (expand-file-name
        (car (seq-sort #'file-newer-than-file-p firefox-dirlist))
        (getenv "HOME"))
       "/"))))

(defun ibrowse-core--most-recent (file-list)
  "Return the most recent edit time from FILE-LIST."
  (apply #'max
           (mapcar (lambda (f)
                     (float-time
                      (file-attribute-modification-time (file-attributes f))))
                   file-list)))

(defun ibrowse-core-guess ()
  "Guess the directory containing main database files.

These main database files are `History' and `Bookmarks' in the case of
Chromium, `places.sqlite' in the case of Firefox.  By default, the
chosen directory will be the most recently used profile."
  (let* ((chromium-dir (ibrowse-core-get-chromium-dir))
         (firefox-dir (ibrowse-core-get-firefox-dir)))
    (cond
     ((and chromium-dir firefox-dir)
      (let* ((chromium-files
              (delq nil
                    (list (concat chromium-dir "History")
                          (let ((f (concat chromium-dir "History-journal")))
                            (if (file-exists-p f) f nil)))))
             (firefox-files
              (delq nil
                    (list (concat firefox-dir "places.sqlite")
                          (let ((f (concat firefox-dir "places.sqlite-wal")))
                            (if (file-exists-p f) f nil)))))
             (chromium-latest (ibrowse-core--most-recent chromium-files))
             (firefox-latest (ibrowse-core--most-recent firefox-files)))
        (if (> chromium-latest firefox-latest)
            (cl-values 'Chromium chromium-dir)
          (cl-values 'Firefox firefox-dir))))
     (chromium-dir (cl-values 'Chromium chromium-dir))
     (firefox-dir (cl-values 'Firefox firefox-dir))
     (t (user-error "The browser database directory is not found!")))))

(defun ibrowse-core--file-check (var)
  "Check if the file which symbol is VAR exists."
  (pcase (symbol-value var)
    ('nil (user-error "`%s' returns nil!" var))
    ((pred file-exists-p) nil)
    (f (user-error "'%s' doesn't exist, please inspect `%s'" f var))))

(defun ibrowse-core--insert-link (item &optional _ws)
  "Insert ITEM in the current buffer.
If called in `org-mode', insert an org link.
If called in `markdown-mode', insert a markdown link.
Elsewhere, insert the url."
  (pcase item
    (`(,title ,url ,_id)
     (cond
      ((derived-mode-p 'org-mode)
       (with-current-buffer (org-insert-link '() url title)))
      ((derived-mode-p 'markdown-mode)
       (with-current-buffer (markdown-insert-inline-link title url)))
      (t
       (with-current-buffer (insert url)))))))

(defun ibrowse-core-completing-read (prompt get-candidates categ)
  "GET-CANDIDATES using `completing-read' with PROMPT and CATEG.
GET-CANDIDATES is a function which returns candidates."
  (let* ((candidates (funcall get-candidates))
         (selected   (completing-read
                      prompt
                      (lambda (string predicate action)
                        (if (eq action 'metadata)
                            `(metadata (category . ,categ))
                          (complete-with-action
                           action candidates string predicate))))))
    (assoc selected candidates)))

(defun ibrowse-core-update-browser! ()
  "Update variables if you have changed your current browser.

More precisely, this function updates `ibrowse-core-browser' and
`ibrowse-core-browser-dir'."
  (cl-multiple-value-setq
      (ibrowse-core-browser ibrowse-core-browser-dir) (ibrowse-core-guess)))

(defun ibrowse-core-update-browser ()
  "Update all necessary variables if you have changed your current browser."
  (interactive)
  (run-hooks 'ibrowse-core-update-hook))

;; -90: Always the first hook to be run.
(add-hook 'ibrowse-core-update-hook 'ibrowse-core-update-browser! -90)

(provide 'ibrowse-core)

;; Do not compile (byte-compilation in a clean environment fails)
(cl-eval-when (load)
  (ibrowse-core-update-browser!))
;;; ibrowse-core.el ends here
