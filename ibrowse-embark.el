;;; ibrowse-embark.el --- Interact with your browser -*- lexical-binding: t -*-

;; Copyright © 2022 Nicolas Graves <ngraves@ngraves.fr>

;; Author: Nicolas Graves <ngraves@ngraves.fr>
;; Version: 0.1.2
;; Package-Requires: ((emacs "24.3") (embark "0.17"))
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

;; TODO send the url to embark-become or embark-act as a URL

;;; Code:

(require 'embark)

;;; ibrowse-tab

;; Declaring function existence for byte-compiling.
(declare-function ibrowse-tab-select               "ibrowse-tab" ())
(declare-function ibrowse-tab-close                "ibrowse-tab" ())
(declare-function ibrowse-tab-copy-url             "ibrowse-tab" ())
(declare-function ibrowse-tab-insert-org-link      "ibrowse-tab" ())
(declare-function ibrowse-tab-insert-markdown-link "ibrowse-tab" ())

(when (fboundp 'ibrowse-tab--get-candidates)
  (embark-define-keymap embark-browser-tab-actions
    "Keymap for actions for browser tabs (when mentioned by title)."
    ("s" ibrowse-tab-select)
    ("k" ibrowse-tab-close)
    ("u" ibrowse-tab-copy-url)
    ("o" ibrowse-tab-insert-org-link)
    ("m" ibrowse-tab-insert-markdown-link))

  (add-to-list
   'embark-keymap-alist
   '(browser-tab . embark-browser-tab-actions)))

;;; ibrowse-history

;; Declaring function existence for byte-compiling.
(declare-function ibrowse-history-browse-url           "ibrowse-history" ())
(declare-function ibrowse-history-delete               "ibrowse-history" ())
(declare-function ibrowse-history-copy-url             "ibrowse-history" ())
(declare-function ibrowse-history-insert-org-link      "ibrowse-history" ())
(declare-function ibrowse-history-insert-markdown-link "ibrowse-history" ())

(when (fboundp 'ibrowse-history--get-candidates)
  (embark-define-keymap embark-browser-history-actions
    "Keymap for actions for browser history items (when mentioned by name)."
    ("b" ibrowse-history-browse-url)
    ("d" ibrowse-history-delete)
    ("u" ibrowse-history-copy-url)
    ("o" ibrowse-history-insert-org-link)
    ("m" ibrowse-history-insert-markdown-link))

  (add-to-list
   'embark-keymap-alist
   '(browser-history . embark-browser-history-actions)))

;;; ibrowse-bookmark

;; Declaring function existence for byte-compiling.
(declare-function ibrowse-bookmark-browse-url           "ibrowse-bookmark" ())
(declare-function ibrowse-bookmark-delete               "ibrowse-bookmark" ())
(declare-function ibrowse-bookmark-copy-url             "ibrowse-bookmark" ())
(declare-function ibrowse-bookmark-insert-org-link      "ibrowse-bookmark" ())
(declare-function ibrowse-bookmark-insert-markdown-link "ibrowse-bookmark" ())

(when (fboundp 'ibrowse-bookmark--get-candidates)
  (embark-define-keymap embark-browser-bookmark-actions
    "Keymap for actions for browser bookmark items (when mentioned by name)."
    ("b" ibrowse-bookmark-browse-url)
    ("d" ibrowse-bookmark-delete)
    ("u" ibrowse-bookmark-copy-url)
    ("o" ibrowse-bookmark-insert-org-link)
    ("m" ibrowse-bookmark-insert-markdown-link))

  (add-to-list
   'embark-keymap-alist
   '(browser-bookmark . embark-browser-bookmark-actions)))

(provide 'ibrowse-embark)

;;; ibrowse-embark.el ends here
