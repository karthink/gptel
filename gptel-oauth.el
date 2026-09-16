;;; gptel-oauth.el --- OAuth utilities for gptel  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026  Karthik Chikmagalur

;; SPDX-License-Identifier: GPL-3.0-or-later

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

;; Provides common OAuth 2.0 utilities for gptel backends: device flow
;; prompting, PKCE (RFC 7636), base64url encoding, URL parameter encoding,
;; JWT payload parsing, browser authorization-code flow, and token
;; persistence.

;;; Code:

(require 'browse-url)
(require 'url-http)
(require 'gptel-request)

;;; Token Storage

(defun gptel-oauth--write-token (file token)
  "Write TOKEN to FILE.

TOKEN is a plist suitable for later restoration from FILE."
  (let ((print-length nil)
        (print-level nil)
        (coding-system-for-write 'utf-8-unix))
    (make-directory (file-name-directory file) t)
    (write-region (prin1-to-string token) nil file nil :silent)
    token))

(defun gptel-oauth--read-token (file)
  "Read a token plist from FILE.

Returns nil if FILE does not exist or cannot be read."
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

;;; PKCE Implementation

(defun gptel-oauth--base64url-encode (str)
  "Return STR encoded as base64url.

Omits trailing padding from the encoded result."
  (let ((b64 (base64-encode-string str t)))
    (setq b64 (replace-regexp-in-string "+" "-" b64))
    (setq b64 (replace-regexp-in-string "/" "_" b64))
    (replace-regexp-in-string "=+$" "" b64)))

(defun gptel-oauth--base64url-decode (str)
  "Decode base64url string STR.

Adds any required padding before decoding STR."
  (let* ((s (replace-regexp-in-string "-" "+" str))
         (s (replace-regexp-in-string "_" "/" s))
         (pad (% (length s) 4)))
    (when (> pad 0)
      (setq s (concat s (make-string (- 4 pad) ?=))))
    (decode-coding-string (base64-decode-string s) 'utf-8 t)))

(defun gptel-oauth--generate-code-verifier ()
  "Generate a PKCE code verifier.

Uses `random' to build a verifier string acceptable to PKCE."
  (let ((chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~"))
    (apply #'string
           (cl-loop repeat 128
                    collect (aref chars (random (length chars)))))))

(defun gptel-oauth--generate-code-challenge (verifier)
  "Generate a PKCE code challenge from VERIFIER, a code verifier string."
  (gptel-oauth--base64url-encode
   (secure-hash 'sha256 verifier nil nil t)))

;;; OAuth Flow

(defun gptel-oauth--device-auth-prompt (user-code verification-uri)
  "Prompt for device authorization.

Copies USER-CODE to the clipboard and opens VERIFICATION-URI
when appropriate for the current session."
  (let ((in-ssh-session (or (getenv "SSH_CLIENT")
                            (getenv "SSH_CONNECTION")
                            (getenv "SSH_TTY"))))
    (ignore-errors (gui-set-selection 'CLIPBOARD user-code))
    (if in-ssh-session
        (progn
          (message "Device Code: %s (copied to clipboard)" user-code)
          (read-from-minibuffer
           (format "(One-time code %s copied) Visit %s in your local browser, \
enter the code and authorize.  Press ENTER after authorizing: "
                   user-code verification-uri)))
      (read-from-minibuffer
       (format "(One-time code %s copied) Press ENTER to open the authorization page. \
If your browser does not open automatically, browse to %s: "
               user-code verification-uri))
      (browse-url verification-uri)
      (read-from-minibuffer
       (format "(One-time code %s copied) Press ENTER after authorizing: "
               user-code)))))

(defun gptel-oauth--read-code (authorization-url redirect-path state port timeout)
  "Open AUTHORIZATION-URL and return an OAuth authorization code.

REDIRECT-PATH and STATE validate the callback.  PORT specifies the
loopback callback server and TIMEOUT is the maximum wait in seconds.
When running over SSH, prompt for the callback URL after authorization
instead of starting a local server."
  (cl-labels
      ((callback-code (target)
         (let* ((query-start (string-search "?" target))
                (path (if query-start (substring target 0 query-start) target))
                (query (and query-start
                            (url-parse-query-string
                             (substring target (1+ query-start)))))
                (callback-state (cadr (assoc "state" query)))
                (callback-code (cadr (assoc "code" query)))
                (callback-error (cadr (assoc "error" query)))
                (callback-error-description
                 (cadr (assoc "error_description" query))))
           (cond
            ((not (equal path redirect-path))
             (user-error "This is not an OAuth callback"))
            (callback-error
             (user-error "%s" (or callback-error-description callback-error)))
            ((not (equal callback-state state))
             (user-error "OAuth state did not match"))
            (callback-code)
            (t (user-error "OAuth callback did not include a code")))))
       (send-response (process status title body)
         (let ((payload (format "<!doctype html><meta charset=\"utf-8\"><title>%s</title><p>%s</p>"
                                title body)))
           (process-send-string
            process
            (format "HTTP/1.1 %s %s\r\nContent-Type: text/html; \\
charset=utf-8\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
                    status title (string-bytes payload) payload)))))
    (if (or (getenv "SSH_CLIENT")
            (getenv "SSH_CONNECTION")
            (getenv "SSH_TTY"))
        (progn
          (message "OAuth authorization URL: %s" authorization-url)
          (ignore-errors (gui-set-selection 'CLIPBOARD authorization-url))
          (callback-code
           (url-filename
            (url-generic-parse-url
             (read-from-minibuffer
              "OAuth URL copied to clipboard.  Open it in your local browser \
and authorize, then paste the callback URL from the browser's address bar: ")))))
      (let ((deadline (+ (float-time) timeout))
            code error server)
        (cl-labels
            ((finish (process status title body &optional result failure)
               (send-response process status title body)
               (when result (setq code result))
               (when failure (setq error failure))
               (delete-process process))
             (filter (process string)
               (let ((request (concat (or (process-get process :gptel-request) "")
                                      string)))
                 (process-put process :gptel-request request)
                 (when (string-match-p "\r\n\r\n" request)
                   (condition-case err
                       (finish process "200" "OAuth Complete"
                               "OAuth authorization succeeded.  You may close this tab."
                               (and (string-match "\\`GET \\([^ ]+\\) HTTP/" request)
                                    (callback-code (match-string 1 request))))
                     (user-error
                      (finish process "400" "OAuth Error"
                              "OAuth authorization failed.  You may close this tab."
                              nil (error-message-string err))))))))
          (unwind-protect
              (progn
                (setq server
                      (make-network-process
                       :name "gptel-oauth-callback"
                       :server t
                       :host "localhost"
                       :service port
                       :filter #'filter
                       :noquery t))
                (message "OAuth authorization URL: %s" authorization-url)
                (ignore-errors (gui-set-selection 'CLIPBOARD authorization-url))
                (read-from-minibuffer
                 (format "OAuth URL copied to clipboard.  \
Press ENTER to open the authorization page.  \
If your browser does not open automatically, browse to %s: "
                         authorization-url))
                (browse-url authorization-url)
                (while (and (not code) (not error) (< (float-time) deadline))
                  (accept-process-output nil 1))
                (cond
                 (code code)
                 (error (user-error "%s" error))
                 (t (user-error "Timed out waiting for OAuth callback"))))
            (when (process-live-p server)
              (delete-process server))))))))

;;; URL / JWT helpers

(defun gptel-oauth--jwt-payload (jwt-string)
  "Return the payload of JWT-STRING as a plist.

Returns nil if JWT-STRING cannot be decoded or parsed."
  (condition-case nil
      (let* ((parts (split-string jwt-string "\\."))
             (payload (nth 1 parts)))
        (when payload
          (gptel--json-read-string
           (gptel-oauth--base64url-decode payload))))
    (error nil)))

(provide 'gptel-oauth)

;;; gptel-oauth.el ends here
