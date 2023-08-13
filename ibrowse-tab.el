;;; ibrowse-tab.el --- Interact with your browser -*- lexical-binding: t -*-

;; Copyright © 2022 Nicolas Graves <ngraves@ngraves.fr>

;; Author: Nicolas Graves <ngraves@ngraves.fr>
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1"))
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

(defconst ibrowse-tab--cdp-debugging-port
  "9222")

(defun ibrowse-tab--cdp-url (query)
  "Return the url of the chromium developer protocol QUERY."
  (format "http://localhost:%s/json/%s"
          ibrowse-tab--cdp-debugging-port
          query))

(defun ibrowse-tab--firefox-get-ws ()
  "Get the websocket address for Firefox actions."
  (with-temp-buffer
    (url-insert-file-contents (ibrowse-tab--cdp-url "version"))
    (let-alist (json-parse-buffer :object-type 'alist)
      .webSocketDebuggerUrl)))

(defvar ibrowse-tab--ws nil
  "The WebSocket connection to the BiDi Webdriver.")

(defvar ibrowse-tab--ws-result nil
  "Last result parsed by the Websocket.")

(defun ibrowse-tab--extract-fields (item)
  "Prepare a tab search result ITEM for display."
  (let-alist item
    (if (string= .type "page")
        (list (or .title .url) .url (or .id .targetId)))))

(defun ibrowse-tab--ws-list ()
  "Websocket alist to list tabs."
  `(("id" . ,(number-to-string (random 100000)))
    ("method" . "Target.getTargets")))

(defun ibrowse-tab--ws--delete (id)
  "Return the alist to delete tab ID."
  `(("id" . ,(number-to-string (random 100000)))
    ("method" . "Target.closeTarget")
    ("params" . (("targetId" . ,id)))))

(defun ibrowse-tab--get-ws-candidates (result)
  "Get an alist with candidates from RESULT of the websocket message."
  (mapcar #'ibrowse-tab--extract-fields
            (cdr (assoc 'targetInfos result))))

(defun ibrowse-tab--get-cdp-candidates ()
  "Get an alist with candidates."
  (with-temp-buffer
    (url-insert-file-contents (ibrowse-tab--cdp-url "list"))
    (delq nil
          (seq-map #'ibrowse-tab--extract-fields
                   (json-parse-buffer :object-type 'alist)))))


;;; Actions
(defun ibrowse-tab-completing-read (prompt)
  "Wrapper aroudn `ibrowse-core-completing-read' with PROMPT."
  (pcase ibrowse-core-browser
    ('Chromium (ibrowse-core-completing-read prompt
                                             #'ibrowse-tab--get-cdp-candidates
                                             'ibrowse-history))
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
                (lambda (_ws frame)
                  (let* ((message (websocket-frame-text frame))
                         (data (json-parse-string message :object-type 'alist))
                         (result (cdr (assoc 'result data))))
                    (if result
                        (progn
                          (message "Received: %s" result)
                          (let ((item (ibrowse-core-completing-read
                                       prompt
                                       (lambda ()
                                         (ibrowse-tab--get-ws-candidates result))
                                       'ibrowse-tab)))
                            (setq ibrowse-tab--ws-result item)))))))))
       ;; Keep the websocket connection in a global variable so it's not garbage collected
       (setq ibrowse-tab--ws ws)
       ibrowse-tab--ws-result))))

;;;###autoload
(defun ibrowse-tab-select (item)
  "Activate browser tab from ITEM."
  (interactive
   (list (ibrowse-tab-completing-read "Select browser tab:")))
  (pcase ibrowse-core-browser
    ('Chromium
     (url-retrieve-synchronously
      (ibrowse-tab--cdp-url (concat "activate/" (caddr item)))))
    ('Firefox
     (error "Switching tabs is not currently implemented for Firefox"))))

;;;###autoload
(defun ibrowse-tab-close (item)
  "Close browser tab from ITEM.

Optionally use the websocket WS when necessary."
  (interactive
   (list (ibrowse-tab-completing-read "Close browser tab:")))
  (pcase ibrowse-core-browser
    ('Chromium
     (url-retrieve-synchronously
      (ibrowse-tab--cdp-url (concat "close/" (caddr item)))))
    ('Firefox
     (websocket-send-text
      ibrowse-tab--ws
      (json-encode (ibrowse-tab--ws--delete (caddr item)))))))

;;;###autoload
(defun ibrowse-tab-copy-url (item)
  "Select and copy url from ITEM in tabs."
  (interactive
   (list (ibrowse-tab-completing-read "Copy url from tab:")))
  (kill-new (cadr item)))

;;;###autoload
(defun ibrowse-tab-insert-org-link (item)
  "Insert org-link from ITEM in tabs."
  (interactive
   (list (ibrowse-tab-completing-read "Insert org-link from tab:")))
  (ibrowse-core--insert-org-link item))

;;;###autoload
(defun ibrowse-tab-insert-markdown-link (item)
  "Insert markdown-link from ITEM in tabs."
  (interactive
   (list (ibrowse-tab-completing-read "Insert markdown-link from tab:")))
  (ibrowse-core--insert-markdown-link item))

;; TODO ibrowse-tab-previous ibrowse-tab-next ibrowse-tab-send-to-a-new-window
;; TODO ibrowse-core-browse (should send to the default search engine instead of nil)
;; TODO What should we do with tabs that have the same name ? completing read would only show them once

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
