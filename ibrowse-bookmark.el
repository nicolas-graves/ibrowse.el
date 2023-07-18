;;; ibrowse-bookmark.el --- Interact with your browser -*- lexical-binding: t -*-

;; Copyright © 2022 Nicolas Graves <ngraves@ngraves.fr>
;; Copyright © 2021 BlueBoxWare

;; Author: Nicolas Graves <ngraves@ngraves.fr>
;; Version: 0.1.8
;; Keywords: comm, data, files, tools
;; URL: https://git.sr.ht/~ngraves/ibrowse.el

;;; License:

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
(require 'ibrowse-core)

(if (executable-find "chromium")
    (require 'ibrowse-bookmark-chromium))
(if (executable-find "firefox")
    (require 'ibrowse-bookmark-firefox))

;;; Settings

(defun ibrowse-bookmark-get-file ()
  "Get the SQLite database file containing bookmarks."
  (pcase ibrowse-browser
    ('Chromium (concat ibrowse-browser-dir "Bookmarks"))
    ('Firefox (concat ibrowse-browser-dir "places.sqlite"))))

(defvar ibrowse-bookmark-file (ibrowse-bookmark-get-file)
  "The file of SQLite database containing bookmarks.")

(defvar ibrowse-bookmark--temp-db
  (expand-file-name (make-temp-name "ibrowse-db") temporary-file-directory)
  "Temporary copy of the Firefox bookmark SQLite database file.")

;;; Backend / Helpers

(defun ibrowse-bookmark--get-candidates ()
  "In the case of Chromium: get an alist with candidates.
In the case of Firefox: wrapper around `ibrowse-sql--get-candidates'."
  (pcase ibrowse-browser
    ('Chromium (if (fboundp 'ibrowse-bookmark-chromium--get-candidates)
                   (funcall #'ibrowse-bookmark-chromium--get-candidates)
                 (error "Chromium functions not available")))
    ('Firefox (if (fboundp 'ibrowse-bookmark-firefox--get-candidates)
                  (funcall #'ibrowse-bookmark-firefox--get-candidates)
                (error "Firefox functions not available")))))

;;; Actions / Interaction

(defun ibrowse-bookmark--delete-item (title url id)
  "Delete item from bookmarks.  Item is a list of TITLE URL and ID."
  (ibrowse-core--file-check ibrowse-bookmark-file "ibrowse-bookmark-file")
  (pcase ibrowse-browser
    ('Chromium (if (fboundp 'ibrowse-bookmark-chromium--delete-item)
                   (funcall #'ibrowse-bookmark-chromium--delete-item
                            title url id)
                 (error "Chromium functions not available")))
    ('Firefox (if (fboundp 'ibrowse-bookmark-firefox--delete-item)
                  (funcall #'ibrowse-bookmark-firefox--delete-item
                           title url id)
                (error "Firefox functions not available")))))

(defun ibrowse-bookmark-add-item-1 (title url)
  "Same as `ibrowse-add-item' on TITLE and URL, but never prompt."
  (ibrowse-core--file-check ibrowse-bookmark-file "ibrowse-bookmark-file")
  (pcase ibrowse-browser
    ('Chromium (if (fboundp 'ibrowse-bookmark-chromium-add-item-1)
                   (funcall #'ibrowse-bookmark-chromium-add-item-1
                            title url)
                 (error "Chromium functions not available")))
    ('Firefox (if (fboundp 'ibrowse-bookmark-firefox-add-item-1)
                  (funcall #'ibrowse-bookmark-firefox-add-item-1
                           title url)
                (error "Firefox functions not available")))))

;;;###autoload
(defun ibrowse-bookmark-add-item (&optional title url)
  "Add the item constructed from TITLE and URL to bookmarks.

Item is a list of TITLE URL and a recursion id to put the bookmark in
the Bookmarks bar directory."
  (interactive)
  (unless (and title url)
    (setq title (read-string "Title: "))
    (setq url (read-string "Url: ")))
  (ibrowse-bookmark-add-item-1 title url))

(defun ibrowse-bookmark-act (prompt action)
  "Wrapper transmitting PROMPT and ACTION to `ibrowse-core-act'."
  (ibrowse-core-act prompt
                    #'ibrowse-bookmark--get-candidates
                    action
                    'ibrowse-bookmark))

;;;###autoload
(defun ibrowse-bookmark-browse-url ()
  "Select and browse item from bookmarks."
  (interactive)
  (ibrowse-bookmark-act
   "Browse item from browser bookmark:"
   #'ibrowse-core--browse-url))

;;;###autoload
(defun ibrowse-bookmark-copy-url ()
  "Select and copy item from bookmarks."
  (interactive)
  (ibrowse-bookmark-act
   "Copy url from browser bookmark:"
   #'ibrowse-core--copy-url))

;;;###autoload
(defun ibrowse-bookmark-insert-org-link ()
  "Insert org-link from bookmarks."
  (interactive)
  (ibrowse-bookmark-act
   "Insert org-link from browser bookmark:"
   #'ibrowse-core--insert-org-link))

;;;###autoload
(defun ibrowse-bookmark-insert-markdown-link ()
  "Insert markdown-link from bookmarks."
  (interactive)
  (ibrowse-bookmark-act
   "Insert markdown-link from browser bookmark:"
   #'ibrowse-core--insert-markdown-link))

;;;###autoload
(defun ibrowse-bookmark-delete ()
  "Delete item from bookmarks."
  (interactive)
  (ibrowse-bookmark-act
   "Delete item from browser bookmarks:"
   #'ibrowse-bookmark--delete-item))

;;; Embark

(defvar ibrowse-bookmark-embark-actions
  (let ((map (make-sparse-keymap)))
    (define-key map "b" #'ibrowse-bookmark-browse-url)
    (define-key map "d" #'ibrowse-bookmark-delete)
    (define-key map "u" #'ibrowse-bookmark-copy-url)
    (define-key map "o" #'ibrowse-bookmark-insert-org-link)
    (define-key map "m" #'ibrowse-bookmark-insert-markdown-link)
    map)
  "Keymap for actions for browser bookmark items.")

(defvar embark-keymap-alist)
(with-eval-after-load 'embark
  (add-to-list
   'embark-keymap-alist
   '(ibrowse-bookmark . ibrowse-bookmark-embark-actions)))

(defun ibrowse-bookmark-update-browser! ()
  "Update `ibrowse-bookmark-file' if you have changed your current browser."
  (setq ibrowse-bookmark-file (ibrowse-bookmark-get-file)))

(add-hook 'ibrowse-update-hook 'ibrowse-bookmark-update-browser!)

(provide 'ibrowse-bookmark)
;;; ibrowse-bookmark.el ends here
