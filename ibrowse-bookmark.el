;; ibrowser-bookmark.el --- Interact with your browser from Emacs -*- lexical-binding: t -*-

;; Copyright © 2021 BlueBoxWare (original author)
;; Copyright © 2022 Nicolas Graves <ngraves@ngraves.fr>

;; Author: Nicolas Graves <ngraves@ngraves.fr>
;; Version: 0.0.0
;; Package-Requires: ((emacs "25.1"))
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

;;; Settings

(defvar ibrowse-bookmark-file
  (concat ibrowse-core-default-folder "Bookmarks")
  "Chromium-based browsers Bookmarks file.")

(defcustom ibrowse-bookmark-jq nil
  "Path to jq (URL <https://stedolan.github.io/jq/>) if jq is not on the PATH."
  :type 'file)

(defcustom ibrowse-bookmark-no-jq nil
  "When true, do not attempt to use jq but always use native json functionality."
  :type 'boolean)

(defcustom ibrowse-bookmark-ignore-key '("trash")
  "List of keys of the 'roots' object to ignore.
A list of keys of the 'roots' object in the bookmarks file which should be
ignored by `counsel-chrome-bm'.  Case-sensitive.

Bookmarks under these keys and any of their folders will not be included.

Standard keys in a Chrome bookmark file are 'bookmark_bar', 'synced' and
'other'.  Some Chromium based browsers add additional keys.

The default is '(\"trash\"): the \"trash\" key is used by Vivaldi to store
deleted bookmarks.  We don't use `ibrowse-bookmark-ignore-folder' for this
because the name of the folder is localized."
  :type '(list string))

(defcustom ibrowse-bookmark-ignore-folder '()
  "A list of folder names (strings) to ignore by `counsel-chrome-bm'.
Case-sensitive.  Bookmarks in these folders and any subfolders will not be
included."
  :type '(list string))

(defvar ibrowse-bookmark--case-insensitive nil)

(defvar ibrowse-bookmark--last-cmd "")

(defun ibrowse-bookmark--parse-error-msg ()
  "Create error message for parse errors in file `ibrowse-bookmark-file'."
  (format
   "Could not parse %s: not a bookmark file?"
   (expand-file-name ibrowse-bookmark-file)))

;;; Backend / Helpers

(defconst ibrowse-bookmark--separator ?\x1C)

(defun ibrowse-bookmark--get-bookmarks-from-folder (json all)
  "Read bookmarks from JSON folder object.
Ignore ignore lists if ALL is non nil."
  (if (or all (not (member (alist-get 'name json)
                           ibrowse-bookmark-ignore-folder)))
      (let ((bookmarks '()))
        (mapc (lambda (child)
                (if (not (equal (alist-get 'type child)
                                "folder"))
                    (push (concat (alist-get 'name child)
                                  (string ibrowse-bookmark--separator)
                                  (alist-get 'url child))
                          bookmarks))
                (setq bookmarks
                      (nconc bookmarks
                             (ibrowse-bookmark--get-bookmarks-from-folder
                              child all))))
              (alist-get 'children json))
        bookmarks)))

(defun ibrowse-bookmark--read-bookmarks-from-json (json all)
  "Read bookmarks from JSON.
Ignore ignore lists if ALL is non nil."
  (let ((bookmarks '()))
    (mapc (lambda (root)
            (if (or all (not (member (symbol-name (car root))
                                     ibrowse-bookmark-ignore-key)))
                (setq bookmarks
                      (nconc bookmarks
                             (ibrowse-bookmark--get-bookmarks-from-folder
                              root all)))))
          (alist-get 'roots json))
    bookmarks))

(defun ibrowse-bookmark--create-cmd (all str)
  "Create jq query command with search string STR.
Ignore ignore lists if ALL is non nil."
  (let* ((string (if ibrowse-bookmark--case-insensitive (downcase str) str))
         (search-string (json-encode-string string))
         (down-case
          (if ibrowse-bookmark--case-insensitive "ascii_downcase |" "")))
    (concat
     ".roots"
     (if (and ibrowse-bookmark-ignore-key (not all))
         (concat
          " | del("
          (mapconcat (lambda (n)
                       (format ".[%s]" (json-encode-string n)))
                     ibrowse-bookmark-ignore-key ", ")
          ")[] | ")
       "[] | ")
     (if (and ibrowse-bookmark-ignore-folder (not all))
         (concat
          "select("
          (mapconcat (lambda (n)
                       (format "(.name // \"\") != %s" (json-encode-string n)))
                     ibrowse-bookmark-ignore-folder " and ")
          ") | "))
     "recurse(.children[]?"
     (if (and ibrowse-bookmark-ignore-folder (not all))
         (concat
          "; (.type // \"\") != \"folder\" or ("
          (mapconcat (lambda (n)
                       (format "(.name // \"\") != %s" (json-encode-string n)))
                     ibrowse-bookmark-ignore-folder " and ")
          ")"))
     ") | select(.type != \"folder\") | "
     (if str
         (format
          "select(((.name // \"\") | %s contains(%s)) or ((.url // \"\") | %s contains(%s))) | "
          down-case search-string down-case search-string))
     " .name + \""
     (format "\\u%04x" ibrowse-bookmark--separator)
     "\" + .url ")))

(defun ibrowse-bookmark--split (str)
  "Split STR into a cons of title and url."
  (if str
      (if (string= str (ibrowse-bookmark--parse-error-msg)) (cons str "")
        (let ((bm (split-string str (string ibrowse-bookmark--separator))))
          (if (> (length bm) 1)
              (cons (car bm) (cadr bm))
            (cons (car bm) (car bm)))))
    (cons "" "")))

(defun ibrowse-bookmark--split-and-call (func str)
  "Split STR into a cons of title and url and pass it to FUNC."
  (let ((bm (ibrowse-bookmark--split str)))
    (funcall func (car bm) (cdr bm))))

(defun ibrowse-bookmark--jq ()
  "Return jq executable based on settings."
  (if ibrowse-bookmark-jq
      (expand-file-name ibrowse-bookmark-jq)
    (executable-find "jq")))

(defun ibrowse-bookmark--check-for-problems ()
  "Check for issues and throws an error if any issue is found."
  (cond ((not ibrowse-bookmark-file)
         (error "Variable ibrowse-bookmark-file is not set"))
        ((not (file-readable-p ibrowse-bookmark-file))
         (error "Can not read file %s" (expand-file-name
                                        ibrowse-bookmark-file)))
        ((and ibrowse-bookmark-jq
              (not (file-executable-p (expand-file-name ibrowse-bookmark-jq))))
         (error "Can not execute %s" (expand-file-name ibrowse-bookmark-jq)))))

(defun ibrowse-bookmark--read-jq (all str)
  "Run jq with `consult--async-command'.  Use STR as search string.
Ignore ignore lists if ALL is non nil."
  (progn
    (let ((cmd (format
                "%s -r '%s' \"%s\""
                (ibrowse-bookmark--jq)
                (replace-regexp-in-string
                 "'"
                 "'\"'\"'"
                 (ibrowse-bookmark--create-cmd all str))
                (expand-file-name ibrowse-bookmark-file))))
      (setq ibrowse-bookmark--last-cmd cmd)
      (consult--async-command cmd)
      cmd)
    ))

(defun ibrowse-bookmark--read-native (all)
  "Read bookmarks from `ibrowse-bookmark-file'.
Uses native json functionality.  If ALL is non nil, ignore lists are ignored."
  (ibrowse-bookmark--read-bookmarks-from-json
   (condition-case err
       (json-read-file (expand-file-name ibrowse-bookmark-file))
     ((json-end-of-file end-of-buffer)
      (error "%s: %s"
             (ibrowse-bookmark--parse-error-msg)
             (error-message-string err))))
   all))

(defun ibrowse-bookmark--read-filtered-jq (str)
  "Read function for `ibrowse-bookmark--read'.
Search for STR."
  (ibrowse-bookmark--read-jq nil str))

(defun ibrowse-bookmark--read-all-jq (str)
  "Read function for `ibrowse-bookmark--read'.
Search for STR, ignore ignore-lists."
  (ibrowse-bookmark--read-jq t str))

(defun ibrowse-bookmarks--extract-fields (item)
  "Prepare a search result ITEM for display."
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
