;; interactive-browser.el --- Interact with your browser from Emacs -*- lexical-binding: t -*-

;; Copyright © 2022 Nicolas Graves <ngraves@ngraves.fr>

;; Author: Nicolas Graves <ngraves@ngraves.fr>
;; Version: 0.0.0
;; Package-Requires: ((emacs "24.3") (let-alist "1.0.4") (seq "1.11") (dash "2.12.1"))
;; Keywords: browser, tabs, switch
;; URL: https://git.sr.ht/~ngraves/interactive-browser.el

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
(require 'dash)
(require 'seq)

;;; Backend

(defconst cdp--remote-debugging-port
  "9222")

(defun cdp-tabs--url (query)
  "Returns the url of the chromium json list of tabs."
  (format "http://localhost:%s/json/%s"
          cdp--remote-debugging-port
          query))

(defun cdp-tabs--extract-fields (item)
  "Prepare a tabs search result ITEM for display."
  (let-alist item
    (if (string= .type "page")
        (cons .title .id))))

(defun cdp-tabs--get-candidates ()
  "Get an alist with candidates."
  (with-temp-buffer
    (url-insert-file-contents (cdp-tabs--url "list"))
    (map-filter '(lambda (x y) (if x (cons x y)))
                (seq-map #'cdp-tabs--extract-fields
                         (json-parse-buffer :object-type 'alist)))))

(defun cdp-tabs--title->id (selected candidates &rest _)
  (cdr (assoc selected candidates)))

(defun browser-bookmarks--extract-fields (item)
  "Prepare a  search result ITEM for display."
  (let-alist item
    (if (string= .type "url")
        (cons .name .url))))

(defun browser-bookmarks--get-candidates ()
  "Get an alist with candidates."
  (with-temp-buffer
    (insert-file-contents "~/.config/chromium/Default/Bookmarks")
    (seq-map #'browser-bookmarks--extract-fields
    (let-alist (json-parse-buffer :object-type 'alist) .roots.bookmark_bar.children))))

;;; Interaction

(defun cdp-tabs-activate (id)
  (url-retrieve-synchronously (cdp-tabs--url (concat "activate/" id))))

(defun cdp-tabs-close (id)
  (url-retrieve-synchronously (cdp-tabs--url (concat "close/" id))))

(defun cdp-action-tab-by-name (action prompt)
    "Call the function action on the id selected from cdp-tabs--get-candidates."
  (let* ((candidates (cdp-tabs--get-candidates))
         (selected (completing-read prompt candidates))
         (id (cdp-tabs--title->id selected candidates)))
    (funcall action id)))

;;;###autoload
(defun browser-tab-select-tab-by-name ()
  "Activate browser tab by name."
  (interactive)
  (cdp-action-tab-by-name 'cdp-tabs-activate "Select browser tab by name:"))

;;;###autoload
(defun browser-tab-close-tab-by-name ()
  "Close browser tab by name."
  (interactive)
  (cdp-action-tab-by-name 'cdp-tabs-close "Close browser tab by name:"))

(provide 'interactive-browser)
;;; interactive-browser.el ends here
