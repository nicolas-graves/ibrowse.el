;;; ibrowse-tab.el --- Interact with your browser -*- lexical-binding: t -*-

;; Copyright © 2022 Nicolas Graves <ngraves@ngraves.fr>

;; Author: Nicolas Graves <ngraves@ngraves.fr>
;; Version: 0.1.8
;; Keywords: comm, data, files, tools
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

;;; Actions

(defun ibrowse-tab--activate (_title _url id)
  "Active browser tab from ID using the chromium developer protocol."
  (url-retrieve-synchronously (ibrowse-core--cdp-url (concat "activate/" id))))

(defun ibrowse-tab--close (_title _url id)
  "Close browser tab from ID using the chromium developer protocol."
  (url-retrieve-synchronously (ibrowse-core--cdp-url (concat "close/" id))))

(defun ibrowse-tab-act (prompt action)
  "Wrapper transmitting PROMPT and ACTION to `ibrowse-core-act'."
  (ibrowse-core-act prompt
                    #'ibrowse-tab--get-candidates
                    action
                    'ibrowse-tab))

;;;###autoload
(defun ibrowse-tab-select ()
  "Activate browser tab."
  (interactive)
  (ibrowse-tab-act
   "Select browser tab:"
   #'ibrowse-tab--activate))

;;;###autoload
(defun ibrowse-tab-close ()
  "Close browser tab."
  (interactive)
  (ibrowse-tab-act
   "Close browser tab:"
   #'ibrowse-tab--close))

;;;###autoload
(defun ibrowse-tab-copy-url ()
  "Copy url of the browser tab."
  (interactive)
  (ibrowse-tab-act
   "Copy url of browser tab:"
   #'ibrowse-core--copy-url))

;;;###autoload
(defun ibrowse-tab-insert-org-link ()
  "Insert org-link of the browser tab."
  (interactive)
  (ibrowse-tab-act
  "Insert org-link of browser tab:"
   #'ibrowse-core--insert-org-link))

;;;###autoload
(defun ibrowse-tab-insert-markdown-link ()
  "Insert markdown-link of the browser tab."
  (interactive)
  (ibrowse-tab-act
   "Insert markdown-link of browser tab:"
   #'ibrowse-core--insert-markdown-link))

;;; Embark

(defvar ibrowse-tab-embark-actions
  (let ((map (make-sparse-keymap)))
    (define-key map "s" #'ibrowse-tab-select)
    (define-key map "k" #'ibrowse-tab-close)
    (define-key map "u" #'ibrowse-tab-copy-url)
    (define-key map "o" #'ibrowse-tab-insert-org-link)
    (define-key map "m" #'ibrowse-tab-insert-markdown-link)
    map)
  "Keymap for actions for browser tabs.")

(defvar embark-keymap-alist)
(with-eval-after-load 'embark
  (add-to-list
   'embark-keymap-alist
   '(ibrowse-tab . ibrowse-tab-embark-actions)))

(provide 'ibrowse-tab)
;;; ibrowse-tab.el ends here
