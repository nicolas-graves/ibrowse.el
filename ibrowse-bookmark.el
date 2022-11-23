;; ibrowser-bookmark.el --- Interact with your browser from Emacs -*- lexical-binding: t -*-

;; Copyright © 2021 BlueBoxWare (original author)
;; Copyright © 2022 Nicolas Graves <ngraves@ngraves.fr>

;; Author: Nicolas Graves <ngraves@ngraves.fr>
;; Version: 0.0.0
;; Package-Requires: ((emacs "24.3") (let-alist "1.0.4") (seq "1.11") (dash "2.12.1"))
;; Keywords: browser, tabs, switch
;; URL: https://git.sr.ht/~ngraves/ibrowser-bookmark.el

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
;;   Soft requirement: jq (URL <https://stedolan.github.io/jq/>).
;;   Counsel-chrome-bm will work without jq, but it will be slow when you have
;;   a lot of bookmarks.


;;; Code:

(require 'json)
(require 'dash)
(require 'seq)
(require 'ibrowse-core)

;;; Backend

(defvar ibrowse-bookmark-file
  (concat ibrowse-chromium-default-folder "Bookmarks")
  "Chromium-based browsers Bookmarks file.")

(defun ibrowse-bookmarks--extract-fields (item)
  "Prepare a  search result ITEM for display."
  (let-alist item
    (if (string= .type "url")
        (cons .name .url))))

(defun ibrowse-bookmarks--get-candidates ()
  "Get an alist with candidates."
  (with-temp-buffer
    (insert-file-contents ibrowse-bookmark-file)
    (seq-map #'browser-bookmarks--extract-fields
             (let-alist (json-parse-buffer :object-type 'alist)
               .roots.bookmark_bar.children))))

;;; Interaction
;; ibrowse-bookmark-browse-url-by-name
;; ibrowse-bookmark-delete-by-name
;; ibrowse-bookmark-copy-url-by-name
;; ibrowse-bookmark-add

(provide 'ibrowse-bookmark)
;;; ibrowse-bookmark.el ends here
