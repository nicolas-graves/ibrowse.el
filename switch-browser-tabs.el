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

(require 'wrapi)

;;; Interaction

(defun tabs-lookup--title->id (selected candidates &rest _)
  (cdr (assoc selected candidates)))

(defun browser-tabs-activate (backend id)
  (with-temp-buffer
    (funcall backend 'url (concat "activate/" id))))

(defun switch-browser-tabs--callback (backend)
  "Generate a search results callback for RESULTS-BUFFER.
Results are parsed with (BACKEND 'parse-buffer)."
  (wrapi-generic-url-callback
   (lambda () ;; no allowed errors, so no arguments
     "Parse results of bibliographic search."
     (let* ((candidates (funcall backend 'parse-buffer))
            (selected (completing-read "Select:" candidates))
            (id (tabs-lookup--title->id
                 selected candidates)))
       (browser-tabs-activate backend id)))))

;;; Listing tabs

(defun switch-browser-tabs--1 (backend query)
  "Just like `browser-tabs-lookup' on BACKEND and QUERY, but never prompt."
  (with-current-buffer
      (url-retrieve-synchronously (funcall backend 'url query))
    (switch-browser-tabs--callback backend)))

;;;###autoload
(defun switch-browser-tabs (&optional backend query)
  "Perform a search using BACKEND, and QUERY.
Prompt for any missing or nil arguments.  BACKEND should be a
function obeying the interface described in the docstring of
`biblio-backends'.  Returns the buffer in which results will be
inserted."
  (interactive)
  (unless backend (setq backend #'cdp-tabs-backend))
  (unless query (setq query "list"))
  (switch-browser-tabs--1 backend query))

(provide 'switch-browser-tabs)
;;; switch-browser-tabs.el ends here
