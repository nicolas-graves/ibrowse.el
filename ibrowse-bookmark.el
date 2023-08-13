;;; ibrowse-bookmark.el --- Interact with your browser -*- lexical-binding: t -*-

;; Copyright © 2022 Nicolas Graves <ngraves@ngraves.fr>
;; Copyright © 2021 BlueBoxWare

;; Author: Nicolas Graves <ngraves@ngraves.fr>
;; Version: 0.2.3
;; Package-Requires: ((emacs "27.1"))
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
  (pcase ibrowse-core-browser
    ('Chromium (concat ibrowse-core-browser-dir "Bookmarks"))
    ('Firefox (concat ibrowse-core-browser-dir "places.sqlite"))))

(defvar ibrowse-bookmark-file (ibrowse-bookmark-get-file)
  "The file of SQLite database containing bookmarks.")

;;; Backend / Helpers

(defun ibrowse-bookmark--get-candidates ()
  "In the case of Chromium: get an alist with candidates.
In the case of Firefox: wrapper around `ibrowse-sql--get-candidates'."
  (ibrowse-core--file-check 'ibrowse-bookmark-file)
  (pcase ibrowse-core-browser
    ('Chromium (if (fboundp 'ibrowse-bookmark-chromium--get-candidates)
                   (funcall #'ibrowse-bookmark-chromium--get-candidates)
                 (error "Chromium functions not available")))
    ('Firefox (if (fboundp 'ibrowse-bookmark-firefox--get-candidates)
                  (funcall #'ibrowse-bookmark-firefox--get-candidates)
                (error "Firefox functions not available")))))

;;; Actions / Interaction
(defun ibrowse-bookmark-add-item-1 (title url)
  "Same as `ibrowse-add-item' on TITLE and URL, but never prompt."
  (ibrowse-core--file-check 'ibrowse-bookmark-file)
  (pcase ibrowse-core-browser
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

(defun ibrowse-bookmark-completing-read (prompt)
  "Wrapper around `ibrowse-core-completing-read' with PROMPT."
  (ibrowse-core-completing-read prompt
                                #'ibrowse-bookmark--get-candidates
                                'ibrowse-bookmark))

;;;###autoload
(defun ibrowse-bookmark-browse-url (item)
  "Select and browse url from ITEM in bookmarks."
  (interactive
   (list (ibrowse-bookmark-completing-read "Browse item from bookmark:")))
  (browse-url (cadr item)))

;;;###autoload
(defun ibrowse-bookmark-copy-url (item)
  "Select and copy url from ITEM in bookmarks."
  (interactive
   (list (ibrowse-bookmark-completing-read "Copy url from bookmark:")))
  (kill-new (cadr item)))

;;;###autoload
(defun ibrowse-bookmark-insert-link (item)
  "Insert link from ITEM in bookmarks.
See `ibrowse-core--insert-link' for more details."
  (interactive
   (list (ibrowse-bookmark-completing-read "Insert link from bookmark:")))
  (ibrowse-core--insert-link item))

;;;###autoload
(defun ibrowse-bookmark-delete (item)
  "Delete ITEM from bookmarks."
  (interactive
   (list (ibrowse-bookmark-completing-read "Delete item from bookmarks:")))
  (ibrowse-core--file-check 'ibrowse-bookmark-file)
  (pcase ibrowse-core-browser
    ('Chromium (if (fboundp 'ibrowse-bookmark-chromium--delete-item)
                   (funcall #'ibrowse-bookmark-chromium--delete-item item)
                 (error "Chromium functions not available")))
    ('Firefox (if (fboundp 'ibrowse-bookmark-firefox--delete-item)
                  (funcall #'ibrowse-bookmark-firefox--delete-item item)
                (error "Firefox functions not available")))))

;;; Embark

(defvar ibrowse-bookmark-embark-actions
  (let ((map (make-sparse-keymap)))
    (define-key map "b" #'ibrowse-bookmark-browse-url)
    (define-key map "d" #'ibrowse-bookmark-delete)
    (define-key map "u" #'ibrowse-bookmark-copy-url)
    (define-key map "i" #'ibrowse-bookmark-insert-link)
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

(add-hook 'ibrowse-core-update-hook 'ibrowse-bookmark-update-browser!)

(provide 'ibrowse-bookmark)
;;; ibrowse-bookmark.el ends here
