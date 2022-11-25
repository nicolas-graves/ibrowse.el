;; ibrowser-bookmark.el --- Interact with your browser from Emacs -*- lexical-binding: t -*-

;; Copyright © 2021 BlueBoxWare (original author)
;; Copyright © 2022 Nicolas Graves <ngraves@ngraves.fr>

;; Author: Nicolas Graves <ngraves@ngraves.fr>
;; Version: 0.0.0
;; Package-Requires: ((emacs "26.1"))
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
(require 'radix-tree)
(require 'ibrowse-core)

;;; Settings

(defvar ibrowse-bookmark-file
  (concat ibrowse-core-default-folder "Bookmarks")
  "Chromium-based browsers Bookmarks file.")

(defconst ibrowse-bookmark--separator ".")

;;; Backend / Helpers

(defun ibrowse-bookmark--generate-item (title-url)
  "Generate a bookmark item entry from TITLE-URL."
  `((date_added . "")
    (date_last_used . "")
    (guid . "")
    (id . "")
    (meta_info
     (power_bookmark_meta . ""))
    (name . ,(car title-url))
    (type . "url")
    (url . ,(cdr title-url))))

(defun ibrowse-bookmark--generate-folder (content name)
  "Generate a bookmark folder entry from CONTENT and with the foldername NAME."
  `((children . ,(vconcat (ibrowse-bookmark--generate-children content)))
    (date_added . "")
    (date_last_used . "")
    (date_modified . "")
    (guid . "")
    (id . "")
    (name . ,name)
    (type . "folder")))

(defun ibrowse-bookmark--generate-children (content)
  "Generate a bookmark children array from folder entry from CONTENT."
  (mapcar
   (lambda (cand)
     (if (not (nested-alist-p cand))
         (ibrowse-bookmark--generate-item cand)
       ;; Case when a bookmark begins with the same name as a folder.
       (if (not (string-match-p "\\." (car cand)))
           (ibrowse-bookmark--generate-children
            (mapcar
             (lambda (y) (cons (concat (car cand) (car y)) (cdr y)))
             (cdr cand)))
         ;; Case when better split the folder.
         (if (not (string-suffix-p "." (car cand)))
             (let ((prefix (car (split-string (car cand) "\\."))))
               (ibrowse-bookmark--generate-folder
                (radix-tree-subtree (list cand) (concat prefix ".")) prefix))
           (ibrowse-bookmark--generate-folder
            (radix-tree-subtree (list cand) (car cand))
            (string-remove-suffix "." (car cand)))))))
   content))

(defun ibrowse-bookmark--generate-file (bookmark-list)
  "Generate the content of a bookmark file from BOOKMARK-LIST."
  (let* ((radix        (ibrowse-bookmark--radix-tree (list bookmark-list)))
         (bookmark-bar (radix-tree-subtree (car radix) ".Bookmarks bar."))
         (other        (radix-tree-subtree (cadr radix) ".Other bookmarks."))
         (synced       (radix-tree-subtree (caddr radix) ".Mobile bookmarks.")))
    `((checksum . "")
      (roots
       (bookmark_bar . ,(ibrowse-bookmark--generate-folder bookmark-bar "Bookmarks bar"))
       (other .        ,(ibrowse-bookmark--generate-folder other "Other bookmarks"))
       (synced .       ,(ibrowse-bookmark--generate-folder synced "Mobile bookmarks")))
      (version . 1))))

(defun ibrowse-bookmark--write-file (bookmark-list filename)
  "Write the file generated from `ibrowse-bookmark--generate-file' and \
BOOKMARK-LIST to FILENAME."
  (with-temp-file filename
    (insert (json-encode (ibrowse-bookmark--generate-file bookmark-list)))))

(defun ibrowse-bookmark--check-for-problems ()
  "Check for issues and throws an error if any issue is found."
  (cond ((not ibrowse-bookmark-file)
         (error "Variable ibrowse-bookmark-file is not set"))
        ((not (file-readable-p ibrowse-bookmark-file))
         (error "Can not read file %s" (expand-file-name
                                        ibrowse-bookmark-file)))))

(defun ibrowse-bookmark--extract-fields (item recursion-id)
  "Prepare a search result ITEM for display and store folder data to \
RECURSION-ID."
  (let-alist item
    (if (and .children (string= .type "folder"))
        (seq-mapcat
         (lambda (x)
           (ibrowse-bookmark--extract-fields
            x
            (concat recursion-id ibrowse-bookmark--separator .name)))
         .children)
      (if (string= .type "url")
          (list
           (list .name .url
                 (concat recursion-id ibrowse-bookmark--separator .name)))))))

(defun ibrowse-bookmark--get-candidates ()
  "Get an alist with candidates."
  (seq-mapcat
   #'identity
   (delq nil
         (mapcar (lambda (x)
                   (ibrowse-bookmark--extract-fields (cdr x) ""))
                 (alist-get 'roots (json-read-file ibrowse-bookmark-file))))))

(defun ibrowse-bookmark--radix-tree (bookmark-list)
  "Generate a radix-tree from BOOKMARK-LIST."
  (mapcar
   (lambda (sublist)
     (seq-reduce (lambda (acc x) (radix-tree-insert acc (caddr x) (cadr x)))
                 sublist
                 radix-tree-empty))
   bookmark-list))

;;; Actions / Interaction

(defun ibrowse-bookmark--delete-item (title url id)
  "Delete item from bookmarks by name."
  (ibrowse-bookmark--write-file
   (delete `(,title ,url ,id) (ibrowse-bookmark--get-candidates))
   ibrowse-bookmark-file))

(defun ibrowse-bookmark-add-item-1 (title url)
  "Add item to bookmarks."
  (ibrowse-bookmark--write-file
   (append `((,title ,url ,(concat ".Bookmarks bar." title)))
           (ibrowse-bookmark--get-candidates))
   ibrowse-bookmark-file))

;;;###autoload
(defun ibrowse-bookmark-add-item (&optional title url)
  "Delete item from bookmarks by name."
  (interactive)
  (unless (and title url)
    (setq title (read-string "Title: "))
    (setq url (read-string "Url: ")))
  (ibrowse-bookmark-add-item-1 title url))

;;;###autoload
(defun ibrowse-bookmark-browse-url-by-name ()
  "Select and browse item from bookmarks by name."
  (interactive)
  (ibrowse-core-act-by-name
   "Browse item from history by name:"
   #'ibrowse-bookmark--get-candidates
   #'ibrowse-core--browse-url))

;;;###autoload
(defun ibrowse-bookmark-copy-url-by-name ()
  "Select and copy item from bookmarks by name."
  (interactive)
  (ibrowse-core-act-by-name
   "Browse item from history by name:"
   #'ibrowse-bookmark--get-candidates
   #'ibrowse-core--copy-url))

;;;###autoload
(defun ibrowse-bookmark-insert-org-link-by-name ()
  "Insert org-link from bookmarks by name."
  (interactive)
  (ibrowse-core-act-by-name
   "Copy url of browser bookmark by name:"
   #'ibrowse-bookmark--get-candidates
   #'ibrowse-core--insert-org-link))

;;;###autoload
(defun ibrowse-bookmark-insert-markdown-link-by-name ()
  "Insert markdown-link from bookmarks by name."
  (interactive)
  (ibrowse-core-act-by-name
   "Copy url of browser bookmark by name:"
   #'ibrowse-bookmark--get-candidates
   #'ibrowse-core--insert-markdown-link))

;;;###autoload
(defun ibrowse-bookmark-delete-by-name ()
  "Delete item from bookmarks by name."
  (interactive)
  (ibrowse-core-act-by-name
   "Delete item from history by name:"
   #'ibrowse-bookmark--get-candidates
   #'ibrowse-bookmark--delete-item))

(provide 'ibrowse-bookmark)
;;; ibrowse-bookmark.el ends here
