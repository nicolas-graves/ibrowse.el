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
;; This backend works with all browsers implementing the Chrome
;; Debugging Protocol (abbreviated CDP). It does not work yet for
;; Firefox-based browsers (as of v105.0).

;;; Code:

(require 'switch-browser-tabs)

(defconst cdp-tabs--remote-debugging-port
  "9222")

(defun cdp-tabs--extract-interesting-fields (item)
  "Prepare a tabs search result ITEM for display."
  (let-alist item
    (if (string= .type "page")
        (cons .title .id))))

(defun cdp-tabs--parse-tabs ()
  "Extract tabs results from browser response."
  (wrapi-decode-url-buffer 'utf-8)
  (seq-map #'cdp-tabs--extract-interesting-fields (json-read)))

(defun cdp-tabs--url (query)
  "Returns the url of the chromium json list of tabs."
  (format "http://localhost:%s/json/%s"
          cdp-tabs--remote-debugging-port
          query))

;;;###autoload
(defun cdp-tabs-backend (command &optional arg)
  "A tabs backend for wrapi.el.
COMMAND, ARG, MORE: See `biblio-backends'."
  (pcase command
    (`name "chromium")
    (`url (cdp-tabs--url arg))
    (`parse-buffer (cdp-tabs--parse-tabs))))

(provide 'switch-cdp-tabs)
;;; switch-cdp-tabs.el ends here
