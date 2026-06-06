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
