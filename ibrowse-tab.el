;; ibrowse.el --- Interact with your browser from Emacs -*- lexical-binding: t -*-

;; Copyright © 2022 Nicolas Graves <ngraves@ngraves.fr>

;; Author: Nicolas Graves <ngraves@ngraves.fr>
;; Version: 0.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: browser, tabs, switch
;; URL: https://git.sr.ht/~ngraves/ibrowse.el

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
(require 'ibrowse-core)

;;; Backend

(defun ibrowse-tab--extract-fields (item)
  "Prepare a tab search result ITEM for display."
  (let-alist item
    (if (string= .type "page")
        (list .title .url .id))))

(defun ibrowse-tab--get-candidates ()
  "Get an alist with candidates."
  (with-temp-buffer
    (url-insert-file-contents (ibrowse-core--cdp-url "list"))
    (delq nil
          (seq-map #'ibrowse-tab--extract-fields
                   (json-parse-buffer :object-type 'alist)))))

(defun ibrowse-tab--title->url (selected candidates &rest _)
  (cadr (assoc selected candidates)))

;;; Interaction

(defun ibrowse-tab-activate (_title _url id)
  (url-retrieve-synchronously (ibrowse-core--cdp-url (concat "activate/" id))))

(defun ibrowse-tab-close (_title _url id)
  (url-retrieve-synchronously (ibrowse-core--cdp-url (concat "close/" id))))

;;;###autoload
(defun ibrowse-tab-select-by-name ()
  "Activate browser tab by name."
  (interactive)
  (ibrowse-core-act-by-name
   "Select browser tab by name:"
   #'ibrowse-tab--get-candidates
   #'ibrowse-tab-activate))

;;;###autoload
(defun ibrowse-tab-close-by-name ()
  "Close browser tab by name."
  (interactive)
  (ibrowse-core-act-by-name
   "Close browser tab by name:"
   #'ibrowse-tab--get-candidates
   #'ibrowse-tab-close))

;;;###autoload
(defun ibrowse-tab-copy-url-by-name ()
  "Copy url of the browser tab by name."
  (interactive)
  (ibrowse-core-act-by-name
   "Copy url of browser tab by name:"
   #'ibrowse-tab--get-candidates
   #'ibrowse-core-copy-url))

(provide 'ibrowse-tab)
;;; ibrowse-tab.el ends here
