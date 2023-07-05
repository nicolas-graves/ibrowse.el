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
(require 'radix-tree)
(require 'ibrowse-core)
(require 'ibrowse-sql)

;;; Settings

(defun ibrowse-bookmark-guess-file ()
  "Guess the file containing bookmarks."
  (let* ((history-file (concat ibrowse-sql-db-dir "Bookmarks"))
         (places-file (concat ibrowse-sql-db-dir "places.sqlite")))
    (cond ((file-exists-p history-file) history-file)
          ((file-exists-p places-file) places-file)
          (t (user-error "The bookmarks file has not been found!")))))

(defvar ibrowse-bookmark-file (ibrowse-bookmark-guess-file)
  "Browser bookmarks file.

It is either SQLite database for Firefox, or a simple file for
Chromium.")

(defvar ibrowse-bookmark--temp-db
  (expand-file-name (make-temp-name "ibrowse-db") temporary-file-directory)
  "Temporary copy of the Firefox bookmark SQLite database file.")

(defconst ibrowse-bookmark--separator ".")

;;; Backend / Helpers
;;;; Backend / Helper for Chromium

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

(defun ibrowse-bookmark--generate-dir (content name)
  "Generate a bookmark directory entry from CONTENT named NAME."
  `((children . ,(vconcat (ibrowse-bookmark--generate-children content)))
    (date_added . "")
    (date_last_used . "")
    (date_modified . "")
    (guid . "")
    (id . "")
    (name . ,name)
    (type . "folder")))

(defun ibrowse-bookmark--generate-children (content)
  "Generate a bookmark children array from directory entry from CONTENT."
  (mapcar
   (lambda (cand)
     (if (not (nested-alist-p cand))
         (ibrowse-bookmark--generate-item cand)
       ;; Case when a bookmark begins with the same name as a directory.
       (if (not (string-match-p "\\." (car cand)))
           (ibrowse-bookmark--generate-children
            (mapcar
             (lambda (y) (cons (concat (car cand) (car y)) (cdr y)))
             (cdr cand)))
         ;; Case when better split the directory.
         (if (not (string-suffix-p "." (car cand)))
             (let ((prefix (car (split-string (car cand) "\\."))))
               (ibrowse-bookmark--generate-dir
                (radix-tree-subtree (list cand) (concat prefix ".")) prefix))
           (ibrowse-bookmark--generate-dir
            (radix-tree-subtree (list cand) (car cand))
            (string-remove-suffix "." (car cand)))))))
   content))

(defun ibrowse-bookmark--generate-json (bookmark-list)
  "Generate the content of a bookmark file from BOOKMARK-LIST."
  (let* ((radix        (ibrowse-bookmark--radix-tree (list bookmark-list)))
         (bookmark-bar (radix-tree-subtree (car radix) ".Bookmarks bar."))
         (other        (radix-tree-subtree (cadr radix) ".Other bookmarks."))
         (synced       (radix-tree-subtree (caddr radix) ".Mobile bookmarks.")))
    `((checksum . "")
      (roots
       (bookmark_bar . ,(ibrowse-bookmark--generate-dir bookmark-bar "Bookmarks bar"))
       (other .        ,(ibrowse-bookmark--generate-dir other "Other bookmarks"))
       (synced .       ,(ibrowse-bookmark--generate-dir synced "Mobile bookmarks")))
      (version . 1))))

(defun ibrowse-bookmark--write-file (bookmark-list filename)
  "Write the file generated from BOOKMARK-LIST to FILENAME.

The file is generated by applying `ibrowse-bookmark--generate-json' to
BOOKMARK-LIST."
  (with-temp-file filename
    (insert (json-encode (ibrowse-bookmark--generate-json bookmark-list)))))

(defun ibrowse-bookmark--extract-fields (item recursion-id)
  "Prepare a search result ITEM for display and store directory to \
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

(defun ibrowse-bookmark--radix-tree (bookmark-list)
  "Generate a radix-tree from BOOKMARK-LIST."
  (mapcar
   (lambda (sublist)
     (seq-reduce (lambda (acc x) (radix-tree-insert acc (caddr x) (cadr x)))
                 sublist
                 radix-tree-empty))
   bookmark-list))

;;;; Backend / Helper for Firefox

(defun ibrowse-bookmark-sql ()
  "The SQL command used to extract bookmarks.
bm:type 1 ensures we extract bookmarks and not folders or separators."
  (list [:select [bm:title p:url p:id]
         :from (as moz_bookmarks bm)
         :inner-join (as moz_places p)
         :where (and (= bm:fk p:id) (= bm:type 1))]))

(defun ibrowse-bookmark-delete-sql (id)
  "The SQL command used to delete the item ID from history."
  (let ((num-id (string-to-number id)))
    `([:delete :from moz_places :where (= id ,num-id)]
      [:delete :from moz_bookmarks :where (= fk ,num-id)])))

;;;; Common backend / helper

(defun ibrowse-bookmark--get-candidates ()
  "In the case of Chromium: get an alist with candidates.
In the case of Firefox: wrapper around `ibrowse-sql--get-candidates'."
  (pcase ibrowse-core-browser
    ('Chromium
     (seq-mapcat
      #'identity
      (delq nil
            (mapcar (lambda (x)
                      (ibrowse-bookmark--extract-fields (cdr x) ""))
                    (alist-get 'roots (json-read-file ibrowse-bookmark-file))))))
    ('Firefox
     (ibrowse-sql--get-candidates ibrowse-bookmark-file
                                  ibrowse-bookmark--temp-db
                                  #'ibrowse-bookmark-sql
                                  "ibrowse-bookmark-file"))))

;;; Actions / Interaction

(defun ibrowse-bookmark--delete-item (title url id)
  "Delete item from bookmarks.  Item is a list of TITLE URL and ID."
  (ibrowse-core--file-check ibrowse-bookmark-file "ibrowse-bookmark-file")
  (pcase ibrowse-core-browser
    ('Chromium
     (ibrowse-bookmark--write-file
      (delete `(,title ,url ,id) (ibrowse-bookmark--get-candidates))
      ibrowse-bookmark-file))
    ('Firefox
     (with-temp-buffer
       (ibrowse-sql--apply-command
        (lambda (_) nil)
        ibrowse-bookmark-file
        (ibrowse-bookmark-delete-sql id)))
     ;; Delete cache.
     (ibrowse-sql--ensure-db ibrowse-bookmark-file ibrowse-bookmark--temp-db t)
     (setq ibrowse-sql-candidates nil))))

(defun ibrowse-bookmark-add-item-1 (title url)
  "Same as `ibrowse-add-item' on TITLE and URL, but never prompt."
  (ibrowse-core--file-check (ibrowse-bookmark-get-file) "ibrowse-bookmark-get-file")
  (pcase ibrowse-core-browser
    ('Chromium
     (ibrowse-bookmark--write-file
      (append `((,title ,url ,(concat ".Bookmarks bar." title)))
              (ibrowse-bookmark--get-candidates))
      (ibrowse-bookmark-get-file)))
    ('Firefox
     (with-temp-buffer
       (ibrowse-sql--apply-command
        (lambda (_) nil)
        (ibrowse-bookmark-get-file)
        (ibrowse-bookmark-add-sql title url)))
     ;; Delete cache.
     (ibrowse-sql--ensure-db ibrowse-bookmark-file ibrowse-bookmark--temp-db t)
     (setq ibrowse-sql-candidates nil))))

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

(provide 'ibrowse-bookmark)
;;; ibrowse-bookmark.el ends here
