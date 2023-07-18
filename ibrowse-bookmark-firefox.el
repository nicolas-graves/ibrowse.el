;;; ibrowse-bookmark-firefox.el --- Interact with your browser -*- lexical-binding: t -*-

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
(require 'ibrowse-sql)

;;; Settings
(defvar ibrowse-bookmark-file)
(defvar ibrowse-bookmark--temp-db)

(defun ibrowse-bookmark--random-alphanumeric (length)
  "Generate a random alphanumeric symbol of LENGTH."
  (let* ((charset "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
         (result ""))
    (intern
     (dotimes (_ length result)
       (setq result (concat result (string (seq-random-elt charset))))))))

(defun ibrowse-bookmark-sql ()
  "The SQL command used to extract bookmarks.
bm:type 1 ensures we extract bookmarks and not folders or separators."
  (list [:select [bm:title p:url p:id]
         :from (as moz_bookmarks bm)
         :inner-join (as moz_places p)
         :where (and (= bm:fk p:id) (= bm:type 1))]))

(defun ibrowse-bookmark-delete-sql (id)
  "The SQL command used to delete the item ID from bookmarks."
  (let ((num-id (string-to-number id)))
    `([:begin-transaction]
      [:delete :from moz_places :where (= id ,num-id)]
      [:delete :from moz_bookmarks :where (= fk ,num-id)]
      [:commit-transaction])))

(defun ibrowse-bookmark--max-id ()
  "The SQL command used to return the max id from moz_places."
  `([:select (funcall max id) :from moz_places]))

(defun ibrowse-bookmark-add-sql (title url)
  "The SQL commands used to add a new bookmark from TITLE and URL."
  (let ((guid (ibrowse-bookmark--random-alphanumeric 12))
        (id (1+
             (string-to-number
              (caar
               (ibrowse-sql--extract-fields ibrowse-bookmark-file
                                            ibrowse-bookmark--temp-db
                                            #'ibrowse-bookmark--max-id
                                            "ibrowse-bookmark-file"
                                            #'ibrowse-sql--read-callback))))))
    `([:begin-transaction]
      [:insert-into moz_places
       :values [,id ;id
                ,(intern url) ;url
                ,(intern title) ;title
                nil ; rev_host
                nil ; visit_count
                0 ; hidden
                0 ; typed
                -1 ; frecency
                nil ; last_visit_date
                ,guid ; guid
                0 ; foreign_count
                0 ; url_hash
                nil ; description
                nil ; preview_image_url
                nil ; site_name
                nil ; origin_id
                0 ; recalc_frecency
                ]]
     [:insert-into moz_bookmarks
      :values [nil ;id
               1 ; type 1=bookmark, 2=folders, 3=separators
               ,id ; fk
               3 ; parent 3=unfiled
               nil ; position
               ,(intern title) ; title
               nil ; keyword_id
               nil ; folder_type
               nil ; dateAdded
               nil ; lastModified
               ,guid ; guid
               0 ; syncStatus
               1 ; syncChangeCounter
               ]]
     [:commit-transaction])))

(defun ibrowse-bookmark-firefox--get-candidates ()
  "Wrapper around `ibrowse-sql--get-candidates'."
  (ibrowse-sql--get-candidates ibrowse-bookmark-file
                               ibrowse-bookmark--temp-db
                               #'ibrowse-bookmark-sql
                               "ibrowse-bookmark-file"))

(defun ibrowse-bookmark-firefox--delete-item (_title _url id)
  "Delete item from bookmarks.  Item is a list of TITLE URL and ID."
  (with-temp-buffer
    (ibrowse-sql--apply-command ibrowse-bookmark-file
                                (ibrowse-sql--prepare-stmt
                                 (ibrowse-bookmark-delete-sql id))))
  ;; Delete cache.
  (ibrowse-sql--ensure-db ibrowse-bookmark-file ibrowse-bookmark--temp-db t)
  (setq ibrowse-sql-candidates nil))

(defun ibrowse-bookmark-firefox-add-item-1 (title url)
  "Same as `ibrowse-add-item' on TITLE and URL, but never prompt."
  (with-temp-buffer
       (ibrowse-sql--apply-command ibrowse-bookmark-file
                                   (ibrowse-sql--prepare-stmt
                                    (ibrowse-bookmark-add-sql title url))))
     ;; Delete cache.
     (ibrowse-sql--ensure-db ibrowse-bookmark-file
                             ibrowse-bookmark--temp-db t)
     (setq ibrowse-sql-candidates nil))

(provide 'ibrowse-bookmark-firefox)
;;; ibrowse-bookmark-firefox.el ends here
