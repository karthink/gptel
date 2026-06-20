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

;;; Token Storage Customization

(defcustom gptel-oauth-token-load-function nil
  "Function to load OAuth tokens.
Default is nil, meaning use backend-specific defaults.
When set, this function is called with BACKEND-TYPE and ACCOUNT-HINT
and should return the token plist or nil.  BACKEND-TYPE is a symbol
indicating which OAuth backend (e.g., 'gptel-gh, 'gptel-openai-oauth).
Backends can customize this before use."
  :type '(or null function)
  :group 'gptel)

(defcustom gptel-oauth-token-save-function nil
  "Function to save OAuth tokens.
Default is nil, meaning use backend-specific defaults.
When set, this function is called with BACKEND-TYPE, ACCOUNT-HINT and TOKEN.
BACKEND-TYPE is a symbol indicating which OAuth backend (e.g., 'gptel-gh,
'gptel-openai-oauth)."
  :type '(or null function)
  :group 'gptel)

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

(defun gptel-oauth--device-auth-prompt (user-code verification-uri &optional account-hint)
  "Prompt for device authorization for ACCOUNT-HINT.

Copies USER-CODE to the clipboard and opens VERIFICATION-URI
when appropriate for the current session.
If ACCOUNT-HINT is provided, displays which account is being logged into."
  (let ((account-text (cond
                       ((null account-hint) "Login")
                       ((string= account-hint "") "Login for the default account.")
                       (t (format "Login for '%s'." account-hint))))
        (in-ssh-session (or (getenv "SSH_CLIENT")
                            (getenv "SSH_CONNECTION")
                            (getenv "SSH_TTY"))))
    (ignore-errors (gui-set-selection 'CLIPBOARD user-code))
    (if in-ssh-session
        (progn
          (message "Device Code: %s (copied to clipboard)" user-code)
          (read-from-minibuffer
           (format "%s (One-time code %s copied) Visit %s in your local browser, \
enter the code and authorize.  Press ENTER after authorizing: "
                   account-text user-code verification-uri)))
      (read-from-minibuffer
       (format "%s (One-time code %s copied) Press ENTER to open the authorization page. \
If your browser does not open automatically, browse to %s: "
               account-text user-code verification-uri))
      (browse-url verification-uri)
      (read-from-minibuffer
       (format "%s (One-time code %s copied) Press ENTER after authorizing: "
               account-text user-code)))))

;;; Account hint validation

(defun gptel-oauth--validate-account-hint (account-hint)
  "Validate ACCOUNT-HINT format for account identification.

An empty string is considered valid for representing a default account.

Valid account hints may contain alphanumeric characters, hyphens, and underscores,
but cannot begin or end with a hyphen or underscore."
  (cond
   ;; Ensure that the account-hint is a string
   ((not (stringp account-hint)) (user-error "Provided account hint is not a string"))

   ;; Handle empty string case
   ((= (length account-hint) 0) t)

   ;; We have some characters, ensure they conform to reasonable rules
   ((string-match-p "\\`[0-9A-Za-z]\\(?:[0-9A-Za-z]\\|-[0-9A-Za-z]\\|_[0-9A-Za-z]\\)*\\'" account-hint) t)
   (t (user-error "Account hint '%s' contains invalid characters" account-hint))))

;;; Backend and token management helpers

(defun gptel-oauth--get-backends-by (predicate account-hint-accessor account-hint)
  "Get backends matching PREDICATE with ACCOUNT-HINT-ACCESSOR equal to ACCOUNT-HINT.

PREDICATE is a function that takes a backend and returns t if it matches.
ACCOUNT-HINT-ACCESSOR is a function that takes a backend and returns its account hint.
ACCOUNT-HINT is the account hint string to match against."
  (seq-filter (lambda (b) (and (funcall predicate b)
                               (string= (funcall account-hint-accessor b) account-hint)))
              (mapcar #'cdr gptel--known-backends)))

(defun gptel-oauth--generate-token-filename (base-file account-hint)
  "Generate token filename for ACCOUNT-HINT using BASE-FILE.

BASE-FILE is the base token filename without account hint suffix.
ACCOUNT-HINT is the account hint string.

If ACCOUNT-HINT is empty, returns BASE-FILE.
Otherwise, returns BASE-FILE with '_' and ACCOUNT-HINT appended."
  (gptel-oauth--validate-account-hint account-hint)
  (if (= (length account-hint) 0)
      base-file
    (concat base-file "_" account-hint)))

(defun gptel-oauth--load-token (predicate account-hint-accessor token-getter token-setter
                                          load-function account-hint)
  "Load token using custom LOAD-FUNCTION, with propagation to matching backends.

PREDICATE is a function to identify backend type.
ACCOUNT-HINT-ACCESSOR extracts account hint from a backend.
TOKEN-GETTER is a function that takes a backend and returns its token.
TOKEN-SETTER is a function that takes a backend and a token and sets the backend's token.
LOAD-FUNCTION is the customizable function to load token from storage.
ACCOUNT-HINT is the account hint for this load operation.

Returns the token plist, or nil if no token found.
If a cached token exists in gptel-backend, returns that immediately.
Otherwise, loads via LOAD-FUNCTION and propagates to all backends
matching PREDICATE and ACCOUNT-HINT."
  (if-let* ((backend gptel-backend)
             ((funcall predicate backend))
             (cached-token (funcall token-getter backend)))
      cached-token
    (let ((token (funcall load-function account-hint)))
      (when (and token (not (equal token "")))
        ;; Propagate to all matching backends
        (dolist (b (gptel-oauth--get-backends-by predicate account-hint-accessor account-hint) token)
          (funcall token-setter b token))))))

(defun gptel-oauth--save-token (predicate account-hint-accessor token-setter
                                          save-function account-hint token)
  "Save TOKEN for ACCOUNT-HINT, with propagation to matching backends.

PREDICATE is a function to identify backend type.
ACCOUNT-HINT-ACCESSOR extracts account hint from a backend.
TOKEN-SETTER is a function that takes a backend and a token and sets the backend's token.
SAVE-FUNCTION is the customizable function to save token to storage.
ACCOUNT-HINT is the account hint to save token for.
TOKEN is the token plist to save.

Updates token on all backends matching PREDICATE and ACCOUNT-HINT,
then calls SAVE-FUNCTION."
  (dolist (b (gptel-oauth--get-backends-by predicate account-hint-accessor account-hint))
    (funcall token-setter b token))
  (funcall save-function account-hint token))

(defun gptel-oauth--get-registered-account-hints (predicate account-hint-accessor default-placeholder)
  "Get list of registered account hints from backends matching PREDICATE.

PREDICATE is a function that takes a backend and returns t if it matches.
ACCOUNT-HINT-ACCESSOR is a function that extracts the account hint from a backend.
DEFAULT-PLACEHOLDER is the string to use for backends with empty account hints.

Returns a list of unique account hint strings, or nil if no matching backends exist.
Empty account hints are replaced with DEFAULT-PLACEHOLDER."
  (when-let* ((backends (seq-filter predicate (mapcar #'cdr gptel--known-backends)))
              (account-hints (mapcar
                              (lambda (b)
                                (let ((hint (funcall account-hint-accessor b)))
                                  (if (string= hint "")
                                      default-placeholder
                                    hint)))
                              backends)))
    (seq-uniq account-hints)))

(defun gptel-oauth--read-account-hint (predicate account-hint-accessor prompt-text default-placeholder &optional error-msg)
  "Interactively read an account hint for login.

PREDICATE is a function to identify backend type.
ACCOUNT-HINT-ACCESSOR extracts account hint from a backend.
PROMPT-TEXT is the text to display when prompting user to choose.
DEFAULT-PLACEHOLDER is the string used to represent the default account.
ERROR-MSG is an optional error message to display when no backends are registered.

Returns the selected account hint as a string, with DEFAULT-PLACEHOLDER
converted to empty string."
  (let* ((known-account-hints (or (gptel-oauth--get-registered-account-hints
                                   predicate account-hint-accessor default-placeholder)
                                  '()))
         (chosen-account-hint (cond
                               ((= 0 (length known-account-hints))
                                (user-error (or error-msg
                                                "No backends registered")))
                               ((= 1 (length known-account-hints))
                                (car known-account-hints))
                               (t
                                (completing-read prompt-text
                                                 known-account-hints nil t)))))
    (if (string= chosen-account-hint default-placeholder)
        ""
      chosen-account-hint)))

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
