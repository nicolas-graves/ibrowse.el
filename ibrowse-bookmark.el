;;; ibrowse-bookmark.el --- Interact with your browser -*- lexical-binding: t -*-

;; Copyright © 2022 Nicolas Graves <ngraves@ngraves.fr>
;; Copyright © 2021 BlueBoxWare

;; Author: Nicolas Graves <ngraves@ngraves.fr>
;; Version: 0.1.5
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
  "Write the file generated from BOOKMARK-LIST to FILENAME.

The file is generated by applying `ibrowse-bookmark--generate-file' to
BOOKMARK-LIST."
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
  "Delete item from bookmarks.  Item is a list of TITLE URL and ID."
  (ibrowse-bookmark--write-file
   (delete `(,title ,url ,id) (ibrowse-bookmark--get-candidates))
   ibrowse-bookmark-file))

(defun ibrowse-bookmark-add-item-1 (title url)
  "Same as `ibrowse-add-item' on TITLE and URL, but never prompt."
  (ibrowse-bookmark--write-file
   (append `((,title ,url ,(concat ".Bookmarks bar." title)))
           (ibrowse-bookmark--get-candidates))
   ibrowse-bookmark-file))

;;;###autoload
(defun ibrowse-bookmark-add-item (&optional title url)
  "Add the item constructed from TITLE and URL to bookmarks.

Item is a list of TITLE URL and a recursion id to put the bookmark in
the Bookmarks bar folder."
  (interactive)
  (unless (and title url)
    (setq title (read-string "Title: "))
    (setq url (read-string "Url: ")))
  (ibrowse-bookmark-add-item-1 title url))

;;;###autoload
(defun ibrowse-bookmark-browse-url ()
  "Select and browse item from bookmarks."
  (interactive)
  (ibrowse-core-act-by-name
   "Browse item from browser bookmark:"
   #'ibrowse-bookmark--get-candidates
   #'ibrowse-core--browse-url
   'ibrowse-bookmark))

;;;###autoload
(defun ibrowse-bookmark-copy-url ()
  "Select and copy item from bookmarks."
  (interactive)
  (ibrowse-core-act-by-name
   "Copy url from browser bookmark:"
   #'ibrowse-bookmark--get-candidates
   #'ibrowse-core--copy-url
   'ibrowse-bookmark))

;;;###autoload
(defun ibrowse-bookmark-insert-org-link ()
  "Insert org-link from bookmarks."
  (interactive)
  (ibrowse-core-act-by-name
   "Insert org-link from browser bookmark:"
   #'ibrowse-bookmark--get-candidates
   #'ibrowse-core--insert-org-link
   'ibrowse-bookmark))

;;;###autoload
(defun ibrowse-bookmark-insert-markdown-link ()
  "Insert markdown-link from bookmarks."
  (interactive)
  (ibrowse-core-act-by-name
   "Insert markdown-link from browser bookmark:"
   #'ibrowse-bookmark--get-candidates
   #'ibrowse-core--insert-markdown-link
   'ibrowse-bookmark))

;;;###autoload
(defun ibrowse-bookmark-delete ()
  "Delete item from bookmarks."
  (interactive)
  (ibrowse-core-act-by-name
   "Delete item from browser bookmarks:"
   #'ibrowse-bookmark--get-candidates
   #'ibrowse-bookmark--delete-item
   'ibrowse-bookmark))

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

(provide 'ibrowse-bookmark)
;;; ibrowse-bookmark.el ends here
