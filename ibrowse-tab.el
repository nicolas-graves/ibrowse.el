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
(require 'websocket)

;;; Backend

(defun ibrowse-tab--extract-fields (item)
  "Prepare a tab search result ITEM for display."
  (let-alist item
    (if (string= .type "page")
        (list (or .title .url) .url (or .id .targetId)))))

(defun ibrowse-tab--get-candidates ()
  "Get an alist with candidates."
  (with-temp-buffer
    (url-insert-file-contents (ibrowse-core--cdp-url "list"))
    (delq nil
          (seq-map #'ibrowse-tab--extract-fields
                   (json-parse-buffer :object-type 'alist)))))

(defun ibrowse-tab--get-ws-candidates (result)
  "Get an alist with candidates from RESULT of the websocket message."
  (lambda () (mapcar #'ibrowse-tab--extract-fields
                       (cdr (assoc 'targetInfos result)))))

(defvar ibrowse-tab--ws nil
  "The WebSocket connection to the bidi webdriver.")

(defun ibrowse-tab--firefox-get-ws ()
  "Get the websocket address for Firefox actions."
  (with-temp-buffer
    (url-insert-file-contents (ibrowse-core--cdp-url "version"))
    (let-alist (json-parse-buffer :object-type 'alist)
      .webSocketDebuggerUrl)))

(defun ibrowse-tab--ws-list ()
  "Websocket alist to list tabs."
  `(("id" . ,(number-to-string (random 100000)))
    ("method" . "Target.getTargets")))

(defun ibrowse-tab--ws--activate (id)
  "Return the alist to activate tab ID."
  `(("id" . ,(number-to-string (random 100000)))
    ("method" . "Target.activateTarget")
    ("params" . (("targetId" . ,id)))))

(defun ibrowse-tab--ws--delete (id)
  "Return the alist to delete tab ID."
  `(("id" . ,(number-to-string (random 100000)))
    ("method" . "Target.closeTarget")
    ("params" . (("targetId" . ,id)))))

;;; Actions

(defun ibrowse-tab--activate (_title _url id)
  "Active browser tab from ID using the chromium developer protocol."
  (pcase ibrowse-core-browser
    ('Chromium
      (url-retrieve-synchronously (ibrowse-core--cdp-url (concat "activate/" id))))
    ('Firefox
      (error "Switching tabs is not currently implemented for Firefox"))))

(defun ibrowse-tab--close (_title _url id &optional ws)
  "Close browser tab from ID using the chromium developer protocol.

Optionally use the websocket WS when necessary."
  (pcase ibrowse-core-browser
    ('Chromium
      (url-retrieve-synchronously (ibrowse-core--cdp-url (concat "close/" id))))
    ('Firefox
      (websocket-send-text ws (json-encode (ibrowse-tab--ws--delete id))))))

(defun ibrowse-tab-act (prompt action)
  "Wrapper transmitting PROMPT and ACTION to `ibrowse-core-act'."
  (pcase ibrowse-core-browser
    ('Chromium (ibrowse-core-act prompt
                                 #'ibrowse-tab--get-candidates
                                 action
                                 'ibrowse-tab))
    ('Firefox
      (let ((ws (websocket-open
                 (ibrowse-tab--firefox-get-ws)
                 :on-open
                 (lambda (ws)
                   (message "Websocket connection opened")
                   (websocket-send-text ws (json-encode (ibrowse-tab--ws-list)))
                   (message "Sent %s" (json-encode (ibrowse-tab--ws-list))))
                 :on-close (lambda (_ws)
                             (message "Websocket connection closed"))
                 :on-error (lambda (_ws type err)
                             (message "WebSocket error: %s %s" type err))
                 :on-message
                 (lambda (ws frame)
                   (let* ((message (websocket-frame-text frame))
                          (data (json-parse-string message :object-type 'alist))
                          (result (cdr (assoc 'result data))))
                     (if result
                         (progn
                           (message "Received: %s" result)
                           (ibrowse-core-act prompt
                                           (ibrowse-tab--get-ws-candidates result)
                                           (lambda (title url id)
                                             (funcall action title url id ws))
                                           'ibrowse-tab))))))))
        ;; Keep the websocket connection in a global variable so it's not garbage collected
        (setq ibrowse-tab--ws ws)))))

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
