;; switch-browser-tabs.el --- Switch between tabs from Emacs -*- lexical-binding: t -*-

;; Copyright © 2022 Nicolas Graves <ngraves@ngraves.fr>

;; Author: Nicolas Graves <ngraves@ngraves.fr>
;; Version: 0.0.0
;; Package-Requires: ((emacs "24.3") (let-alist "1.0.4") (seq "1.11") (dash "2.12.1"))
;; Keywords: browser, tabs, switch
;; URL: https://git.sr.ht/~ngraves/switch-browser-tabs.el

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
;; Switch between tabs from Emacs

;;; Code:

(require 'json)
(require 'dash)
(require 'seq)

;;; Backend

(defconst cdp-tabs--remote-debugging-port
  "9222")

(defun cdp-tabs--url (query)
  "Returns the url of the chromium json list of tabs."
  (format "http://localhost:%s/json/%s"
          cdp-tabs--remote-debugging-port
          query))

(defun cdp-tabs--extract-interesting-fields (item)
  "Prepare a tabs search result ITEM for display."
  (let-alist item
    (if (string= .type "page")
        (cons .title .id))))

(defun cdp-tabs--parse-tabs ()
  "Extract tabs results from browser response."
  (seq-map #'cdp-tabs--extract-interesting-fields
           (json-parse-buffer :object-type 'alist)))

;;; Interaction

(defun tabs--title->id (selected candidates &rest _)
  (cdr (assoc selected candidates)))

(defun browser-tabs-activate (id)
  (url-retrieve-synchronously (cdp-tabs--url (concat "activate/" id))))

;;;###autoload
(defun switch-browser-tabs
  "Just like `browser-tabs-lookup' on BACKEND, but never prompt."
  (interactive)
  (with-temp-buffer
    (url-insert-file-contents (cdp-tabs--url "list"))
    (let* ((candidates (cdp-tabs--parse-tabs))
           (selected (completing-read "Select:" candidates))
           (id (tabs--title->id selected candidates)))
      (browser-tabs-activate id))))

(provide 'switch-browser-tabs)
;;; switch-browser-tabs.el ends here
