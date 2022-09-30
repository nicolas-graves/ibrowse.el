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

;; (defun browser-tabs-get-id (metadata)
;;   "Compute a url from METADATA.
;; Uses .url, and .doi as a fallback."
;;   (let-alist metadata
;;     (if .url .url
;;       (when .doi
;;         (concat "https://doi.org/" (url-encode-url .doi))))))

;; (defun browser-tabs--selection-activate ()
;;   "Open the web page of the current entry in a web browser."
;;   (interactive)
;;   (-if-let* ((url (biblio-get-url (biblio--selection-metadata-at-point))))
;;       (browse-url url)
;;     (user-error "This record does not contain a URL")))

;; TODO convert things from biblio-selection to embark

;; (defun biblio--selection-copy-callback (bibtex entry)
;;   "Add BIBTEX (from ENTRY) to kill ring."
;;   (kill-new bibtex)
;;   (message "Killed bibtex entry for %S."
;;            (biblio--prepare-title (biblio-alist-get 'title entry))))

;; (defun biblio--selection-copy ()
;;   "Copy BibTeX of current entry at point."
;;   (interactive)
;;   (biblio--selection-forward-bibtex #'biblio--selection-copy-callback))

;; (defun biblio--selection-copy-quit ()
;;   "Copy BibTeX of current entry at point and close results."
;;   (interactive)
;;   (biblio--selection-forward-bibtex #'biblio--selection-copy-callback t))

;;; Printing search results

(defun browser-tabs--tag-backend (backend items)
  "Add (backend . BACKEND) to each alist in ITEMS."
  (seq-map (lambda (i) (cons `(backend . ,backend) i)) items))

(defun browser-tabs--callback (results-buffer backend)
  "Generate a search results callback for RESULTS-BUFFER.
Results are parsed with (BACKEND 'parse-buffer)."
  (wrapi-generic-url-callback
   (lambda () ;; no allowed errors, so no arguments
     "Parse results of bibliographic search."
     (let ((results (browser-tabs--tag-backend
                     backend (funcall backend 'parse-buffer))))
       (with-current-buffer results-buffer
         results)))))

;;; Listing tabs

(defun switch-browser-tabs--1 (backend query)
  "Just like `browser-tabs-lookup' on BACKEND and QUERY, but never prompt."
  (with-temp-buffer
    (wrapi-url-retrieve
     (funcall backend 'url query)
     (browser-tabs--callback (current-buffer) backend))
    (current-buffer)))

;;;###autoload
(defun switch-browser-tabs (&optional backend query)
  "Perform a search using BACKEND, and QUERY.
Prompt for any missing or nil arguments.  BACKEND should be a
function obeying the interface described in the docstring of
`biblio-backends'.  Returns the buffer in which results will be
inserted."
  (interactive)
  (unless backend (setq backend #'chromium-tabs-backend))
  (unless query (setq query "list"))
  (switch-browser-tabs--1 backend query))

(provide 'switch-browser-tabs)
;;; switch-browser-tabs.el ends here
