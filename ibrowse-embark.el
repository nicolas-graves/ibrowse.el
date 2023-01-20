;;; ibrowse-embark.el --- Interact with your browser -*- lexical-binding: t -*-

;; Copyright © 2022 Nicolas Graves <ngraves@ngraves.fr>

;; Author: Nicolas Graves <ngraves@ngraves.fr>
;; Version: 0.1.2
;; Package-Requires: ((emacs "24.3"))
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

;; Declaring keymaps.
(defvar embark-browser-tab-actions
  (let ((map (make-sparse-keymap)))
    (define-key map "s" #'ibrowse-tab-select)
    (define-key map "k" #'ibrowse-tab-close)
    (define-key map "u" #'ibrowse-tab-copy-url)
    (define-key map "o" #'ibrowse-tab-insert-org-link)
    (define-key map "m" #'ibrowse-tab-insert-markdown-link)
    map)
  "Keymap for actions for browser tabs.")

(defvar embark-browser-history-actions
  (let ((map (make-sparse-keymap)))
    (define-key map "b" #'ibrowse-history-browse-url)
    (define-key map "d" #'ibrowse-history-delete)
    (define-key map "u" #'ibrowse-history-copy-url)
    (define-key map "o" #'ibrowse-history-insert-org-link)
    (define-key map "m" #'ibrowse-history-insert-markdown-link)
    map)
    "Keymap for actions for browser history items.")

(defvar embark-browser-bookmark-actions
  (let ((map (make-sparse-keymap)))
    (define-key map "b" #'ibrowse-bookmark-browse-url)
    (define-key map "d" #'ibrowse-bookmark-delete)
    (define-key map "u" #'ibrowse-bookmark-copy-url)
    (define-key map "o" #'ibrowse-bookmark-insert-org-link)
    (define-key map "m" #'ibrowse-bookmark-insert-markdown-link)
    map)
  "Keymap for actions for browser bookmark items.")


;; Define embark-keymap-alist.

(defvar embark-keymap-alist)
(with-eval-after-load 'embark
  (when (fboundp 'ibrowse-tab--get-candidates)
    (add-to-list
     'embark-keymap-alist
     '(browser-tab . embark-browser-tab-actions)))
  (when (fboundp 'ibrowse-history--get-candidates)
    (add-to-list
     'embark-keymap-alist
     '(browser-history . embark-browser-history-actions)))
  (when (fboundp 'ibrowse-bookmark--get-candidates)
    (add-to-list
     'embark-keymap-alist
     '(browser-bookmark . embark-browser-bookmark-actions))))

(provide 'ibrowse-embark)

;;; ibrowse-embark.el ends here
