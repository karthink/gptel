;;; gptel-oauth.el --- OAuth utilities for gptel  -*- lexical-binding: t; -*-

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides common OAuth 2.0 device flow utilities, an HTTP client with
;; status-code introspection, and token persistence used across gptel
;; backends (GitHub Copilot, ...).

;;; Code:

(require 'cl-lib)
(require 'browse-url)
(require 'url-http)
(eval-and-compile (require 'gptel-request))

(declare-function gptel--json-encode "gptel-request")
(declare-function gptel--json-read "gptel-request")

(defun gptel-oauth-save-token (file token)
  "Save TOKEN plist to FILE."
  (let ((print-length nil)
        (print-level nil)
        (coding-system-for-write 'utf-8-unix))
    (make-directory (file-name-directory file) t)
    (write-region (prin1-to-string token) nil file nil :silent)
    token))

(defun gptel-oauth-restore-token (file)
  "Restore token plist from FILE."
  (when (file-exists-p file)
    ;; Set coding system to auto-dos so files with CR EOL read properly.
    (let ((coding-system-for-read 'utf-8-auto-dos))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally file)
        (goto-char (point-min))
        (condition-case nil
            (read (current-buffer))
          (error nil))))))

(defun gptel-oauth-device-auth-prompt (user-code verification-uri)
  "Prompt the user to authorize the device flow using USER-CODE and VERIFICATION-URI.
Copies USER-CODE to the clipboard and conditionally opens a browser."
  (let ((in-ssh-session (or (getenv "SSH_CLIENT")
                            (getenv "SSH_CONNECTION")
                            (getenv "SSH_TTY"))))
    (ignore-errors (gui-set-selection 'CLIPBOARD user-code))
    (if in-ssh-session
        (progn
          (message "Device Code: %s (copied to clipboard)" user-code)
          (read-from-minibuffer
           (format "Code %s is copied. Visit %s in your local browser, enter the code, and authorize.  Press ENTER after authorizing. "
                   user-code verification-uri)))
      (read-from-minibuffer
       (format "Your one-time code %s is copied. Press ENTER to open the authorization page. If your browser does not open automatically, browse to %s."
               user-code verification-uri))
      (browse-url verification-uri)
      (read-from-minibuffer "Press ENTER after authorizing. "))))

(cl-defun gptel-oauth-request (url &key (method 'post) data content-type headers)
  "Retrieve URL synchronously and return (:status N :body PLIST :raw STRING).

METHOD is a symbol, typically 'get or 'post.
CONTENT-TYPE defaults to \"application/json\".  When CONTENT-TYPE is
\"application/x-www-form-urlencoded\", DATA should be an already-encoded string.
When CONTENT-TYPE is \"application/json\", DATA should be a plist.
HEADERS is an alist of additional headers."
  (let* ((content-type (or content-type "application/json"))
         (url-request-method (upcase (symbol-name method)))
         (url-request-data
          (when data
            (encode-coding-string
             (cond
              ((string-prefix-p "application/json" content-type)
               (gptel--json-encode data))
              (t data))
             'utf-8)))
         (url-request-extra-headers
          `(("Content-Type" . ,content-type)
            ("Accept" . "application/json")
            ,@headers))
         (url-mime-accept-string "application/json")
         (buf (url-retrieve-synchronously url 'silent)))
    (unwind-protect
        (if (not (buffer-live-p buf))
            (list :status nil :body nil :raw "")
          (with-current-buffer buf
            (let ((status (bound-and-true-p url-http-response-status))
                  (raw-body "")
                  (parsed nil))
              (when (bound-and-true-p url-http-end-of-headers)
                (goto-char url-http-end-of-headers)
                (setq raw-body (buffer-substring-no-properties (point) (point-max)))
                (condition-case nil
                    (progn
                      (goto-char url-http-end-of-headers)
                      (setq parsed (gptel--json-read)))
                  (error nil)))
              (list :status status :body parsed :raw raw-body))))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(provide 'gptel-oauth)

;;; gptel-oauth.el ends here
