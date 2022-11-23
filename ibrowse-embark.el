;; ibrowse-embark.el --- Interact with your browser from Emacs -*- lexical-binding: t -*-

;; Copyright © 2022 Nicolas Graves <ngraves@ngraves.fr>

;; Author: Nicolas Graves <ngraves@ngraves.fr>
;; Version: 0.0.0
;; Package-Requires: ((emacs "24.3") (let-alist "1.0.4") (seq "1.11"))
;; Keywords: browser, tabs, switch, interactive
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

;; Ideas for some embark-actions for tabs
;; - send the url to embark-become or embark-act as a URL
;; DONE copy the url
;; DONE kill the tab
;; DONE activate the tab
;; NATIVE copy the tab title

(require 'embark)
(require 'ibrowse-tab)

(add-to-list 'marginalia-prompt-categories '("\\<browser tab by name\\>" . browser-tab))

(embark-define-keymap embark-browser-tab-actions
  "Keymap for actions for browser tabs (when mentioned by name)."
  ("s" ibrowse-tab-select-tab-by-name)
  ("k" ibrowse-tab-close-tab-by-name)
  ("u" ibrowse-tab-copy-url-tab-by-name))

(add-to-list 'embark-keymap-alist '(browser-tab . embark-browser-tab-actions))
