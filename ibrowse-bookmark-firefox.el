;;; ibrowse-bookmark-firefox.el --- Interact with your browser -*- lexical-binding: t -*-

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
(require 'ibrowse-sql)

;;; Settings
(defvar ibrowse-bookmark-file)

(defun ibrowse-bookmark-firefox--random-alphanumeric (length)
  "Generate a random alphanumeric symbol of LENGTH."
  (let* ((charset "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
         (result ""))
    (intern
     (dotimes (_ length result)
       (setq result (concat result (string (seq-random-elt charset))))))))

(defvar ibrowse-bookmark-firefox--sql
  "\
SELECT bm.title, p.url, p.id
FROM moz_bookmarks AS bm INNER JOIN moz_places AS p
WHERE bm.fk = p.id AND bm.type = 1;"
  "The SQL command used to extract bookmarks.
bm.type 1 ensures we extract bookmarks and not folders or separators.")

(defvar ibrowse-bookmark-firefox--delete-sql
  "\
BEGIN TRANSACTION;
DELETE FROM moz_places WHERE id = %s;
DELETE FROM moz_bookmarks WHERE fk = %s;
COMMIT TRANSACTION;"
  "The SQL command used to delete the item ID from bookmarks.")

(defvar ibrowse-bookmark-firefox--max-id
  "SELECT max(id) FROM moz_places;"
  "The SQL command used to return the max id from moz_places.")

(defvar ibrowse-bookmark-firefox--add-sql
    "\
BEGIN TRANSACTION;
INSERT INTO moz_places VALUES (
%s, --id
%s, --url
%s, --title
NULL, --rev_host
NULL, --visit_count
0, --hidden
0, --typed
-1, --frecency
NULL, --last-visit-date
%s, --guid
0, --foreign_count
0, --url_hash
NULL, --description
NULL, --preview_image_url
NULL, --site-name
NULL, --origin_id
0); --recalc_frecency
INSERT INTO moz_bookmarks VALUES (
NULL, --id
1, --type 1=bookmark
%s, --fk
3, --parent 3=unfiled
NULL, --position
%s, --title
NULL, --keyword_id
NULL, --folder_type
NULL, --dateAdded
NULL, --lastModified
%s, --guid
0, --syncStatus
1); --syncChangeCounter
COMMIT TRANSACTION;"
    "The SQL command used to add a new bookmark.")

(defun ibrowse-bookmark-firefox--get-candidates ()
  "Wrapper around `ibrowse-sql--get-candidates'."
  (ibrowse-sql--get-candidates ibrowse-bookmark-file
                               ibrowse-bookmark-firefox--sql))

(defun ibrowse-bookmark-firefox--delete-item (item)
  "Delete ITEM from bookmarks.  ITEM is a list of title, url and id."
  (let ((id (caddr item)))
    (with-temp-buffer
      (ibrowse-sql--apply-command
       ibrowse-bookmark-file
       (format ibrowse-bookmark-firefox--delete-sql id id))))
  (setq ibrowse-sql-candidates nil))

(defun ibrowse-bookmark-firefox-add-item-1 (title url)
  "Same as `ibrowse-add-item' on TITLE and URL, but never prompt."
  (with-temp-buffer
    (setq ibrowse-sql-candidates nil) ; ensures we get the last id.
    (let ((guid (ibrowse-bookmark-firefox--random-alphanumeric 12))
          (id (1+
               (string-to-number
                (caar
                 (ibrowse-sql--apply-command ibrowse-bookmark-file
                                             ibrowse-bookmark-firefox--max-id
                                             #'ibrowse-sql--read-callback
                                             t))))))
      (ibrowse-sql--apply-command ibrowse-bookmark-file
                                  (format ibrowse-bookmark-firefox--add-sql
                                          id url title guid id title guid))))
  (setq ibrowse-sql-candidates nil))

(provide 'ibrowse-bookmark-firefox)
;;; ibrowse-bookmark-firefox.el ends here
