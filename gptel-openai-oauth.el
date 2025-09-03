;;; gptel-openai-oauth.el --- gptel support for OpenAI subscription plan  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>

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

;;

;;; Code:
(require 'cl-lib)
(require 'gptel-openai-responses)
(require 'gptel-oauth)

(defconst gptel--openai-oauth-client-id "app_EMoamEEZ73f0CkXaXp7hrann")
(defconst gptel--openai-oauth-url "https://auth.openai.com")

;; TODO: Change system to be able to store more than one backend token.
(defvar gptel--openai-oauth-token-file
  (expand-file-name ".cache/gptel-openai/openai-oauth-token"
                    user-emacs-directory))

(defcustom gptel-openai-oauth-login-method 'authorization-code
  "OAuth login method used by `gptel-openai-oauth-login'.

The `authorization-code' method uses OAuth 2.0 Authorization Code
Flow with PKCE and a localhost callback.  It is not supported when
Emacs is running over SSH.  The `device' method uses OAuth 2.0
Device Authorization Grant."
  :type '(choice (const :tag "Authorization Code Flow with PKCE" authorization-code)
                 (const :tag "Device Authorization Grant" device))
  :group 'gptel)

(defconst gptel--openai-oauth-redirect-port 1455)
(defconst gptel--openai-oauth-redirect-timeout 300)
(defconst gptel--openai-oauth-redirect-path "/auth/callback")

;;;; OpenAI OAuth backend
(cl-defstruct (gptel-openai-oauth (:constructor gptel--make-openai-oauth)
                                  (:copier nil)
                                  (:include gptel-openai-responses))
  token
  account-hint)

(cl-defmethod gptel--request-data ((_backend gptel-openai-oauth) _prompts)
  "Return request data for the OpenAI OAuth backend.

Removes unsupported temperature settings from the payload."
  (let ((prompts-plist (cl-call-next-method)))
    (when (plist-member prompts-plist :temperature)
      (display-warning
       '(gptel gptel-openai-oauth)
       "Codex models do not support setting request temperature, ignoring `gptel-temperature'")
      (cl-remf prompts-plist :temperature))
    (when (plist-member prompts-plist :max_output_tokens)
      (display-warning
       '(gptel gptel-openai-oauth)
       "Codex models do not support setting request max_output_tokens, ignoring `gptel-max-tokens'")
      (cl-remf prompts-plist :max_output_tokens))
    prompts-plist))

;;;; OpenAI OAuth methods
;;;;; OpenAI device-based Oauth

(defconst gptel--openai-oauth-poll-interval 2)
(defconst gptel--openai-oauth-poll-timeout 30)

;; TODO: Handle HTTP errors upstream, and figure out what payload-level errors
;; here will look like
(defun gptel--openai-oauth-poll-token (device-auth-id user-code)
  "Poll for a device authorization token.

Polls OpenAI with DEVICE-AUTH-ID and USER-CODE until an
authorization code is returned, a terminal error occurs, or the
request times out."
  (let ((deadline (+ (float-time) gptel--openai-oauth-poll-timeout))
        response)
    (while (and (not response)
                (< (float-time) deadline))
      (setq response
            (gptel--url-retrieve
                (concat gptel--openai-oauth-url "/api/accounts/deviceauth/token")
              :method 'post
              :data (list :device_auth_id device-auth-id :user_code user-code)
              :headers `(("User-Agent" . ,(format "Emacs %s" emacs-version)))))
      (cond
       ((plist-get response :authorization_code))
       ((plist-get response :error)
        (user-error "%s"
                    (or (plist-get response :error_description)
                        (plist-get response :message)
                        (format "%s" (plist-get response :error))
                        "OpenAI OAuth device authorization failed")))
       ((and (plist-get response :status)
             (not (equal (plist-get response :status) "success")))
        (user-error "OpenAI OAuth device authorization failed: %S" response))
       (t
        (setq response nil)
        (with-temp-message
            (format "Waiting for OpenAI to authenticate (-%d seconds...)"
                    (- deadline (float-time)))
          (sleep-for gptel--openai-oauth-poll-interval)))))
    (or response
        (user-error "Timed out waiting for OpenAI OAuth device authorization"))))

(defun gptel--openai-oauth-login-with-device-code (backend)
  "Authenticate BACKEND using OpenAI Device Authorization Grant."
  (pcase-let (((map :device_auth_id :user_code)
               (gptel--url-retrieve
                   (concat gptel--openai-oauth-url "/api/accounts/deviceauth/usercode")
                 :method 'post
                 :data (list :client_id gptel--openai-oauth-client-id)
                 :headers `(("User-Agent" . ,(format "Emacs %s" emacs-version)))))
              (verification-uri (concat gptel--openai-oauth-url "/codex/device")))
    ;; User authentication for user_code
    (gptel-oauth--device-auth-prompt user_code verification-uri)
    (pcase-let* (((map :authorization_code :code_verifier)
                  (gptel--openai-oauth-poll-token device_auth_id user_code))
                 (token-plist
                  (gptel--url-retrieve (concat gptel--openai-oauth-url "/oauth/token")
                    :method 'post
                    :data (url-build-query-string
                           `(("grant_type"    "authorization_code")
                             ("code"          ,authorization_code)
                             ("redirect_uri"  "https://auth.openai.com/deviceauth/callback")
                             ("client_id"     ,gptel--openai-oauth-client-id)
                             ("code_verifier" ,code_verifier)))
                    :content-type "application/x-www-form-urlencoded")))
      ;; TODO Handle case where access_token was not granted
      (gptel--openai-oauth-persist backend token-plist))))

;;;;; OpenAI authorization-code-based Oauth

(defun gptel--openai-oauth-authorization-url (redirect-uri verifier state)
  "Return an OpenAI authorization URL for REDIRECT-URI.

VERIFIER is used to derive the PKCE code challenge.  STATE is
included in the authorization request and checked in the callback."
  (concat
   gptel--openai-oauth-url "/oauth/authorize?"
   (url-build-query-string
    `(("response_type" "code")
      ("client_id" ,gptel--openai-oauth-client-id)
      ("redirect_uri" ,(url-hexify-string redirect-uri))
      ("scope" "openid profile email offline_access")
      ("code_challenge" ,(gptel-oauth--generate-code-challenge verifier))
      ("code_challenge_method" "S256")
      ("id_token_add_organizations" "true")
      ("prompt" "login")
      ("codex_cli_simplified_flow" "true")
      ("state" ,state)
      ("originator" "gptel")))))

(defun gptel--openai-oauth-callback-request (request)
  "Parse callback REQUEST and return a plist with :path and :query."
  (when (string-match "\\`GET \\([^ ]+\\) HTTP/" request)
    (let* ((target (match-string 1 request))
           (query-start (string-search "?" target))
           (path (if query-start
                     (substring target 0 query-start)
                   target))
           (query (and query-start
                       (substring target (1+ query-start)))))
      (list :path path
            :query (and query (url-parse-query-string query))))))

(defun gptel--openai-oauth-send-callback-response (process status title body)
  "Send PROCESS an HTTP response with STATUS, TITLE and BODY."
  (let ((payload (format "<!doctype html><meta charset=\"utf-8\"><title>%s</title><p>%s</p>"
                         title body)))
    (process-send-string
     process
     (format "HTTP/1.1 %s %s\r\nContent-Type: text/html; \
charset=utf-8\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
             status title (string-bytes payload) payload))))

;; TODO: Support over SSH connections can be added by (i) not starting a server,
;; and (ii) asking the user to copy the callback URL from the browser's URL bar
;; into Emacs.
(defun gptel--openai-oauth-read-code (authorization-url state)
  "Open AUTHORIZATION-URL and wait for a localhost callback matching STATE."
  (when (or (getenv "SSH_CLIENT")
            (getenv "SSH_CONNECTION")
            (getenv "SSH_TTY"))
    (user-error
     (concat "OpenAI authorization-code login requires a local browser "
             "callback and is not supported over SSH.  Set "
             "`gptel-openai-oauth-login-method' to `device' and retry")))
  (let ((deadline (+ (float-time) gptel--openai-oauth-redirect-timeout))
        code error server)
    (cl-labels
        ((finish (process status title body &optional result failure)
           (gptel--openai-oauth-send-callback-response process status title body)
           (when result (setq code result))
           (when failure (setq error failure))
           (delete-process process))
         (filter (process string)
           (let ((request (concat (or (process-get process :gptel-request) "")
                                  string)))
             (process-put process :gptel-request request)
             (when (string-match-p "\r\n\r\n" request)
               (pcase-let* ((`(:path ,path :query ,query)
                             (gptel--openai-oauth-callback-request request))
                            (callback-state (cadr (assoc "state" query)))
                            (callback-code (cadr (assoc "code" query)))
                            (callback-error (cadr (assoc "error" query)))
                            (callback-error-description
                             (cadr (assoc "error_description" query))))
                 (cond
                  ((not (equal path gptel--openai-oauth-redirect-path))
                   (finish process "404" "Not Found"
                           "This is not an OpenAI OAuth callback."))
                  (callback-error
                   (finish process "400" "OpenAI OAuth Error"
                           "OpenAI OAuth authorization failed.  You may close this tab."
                           nil
                           (or callback-error-description callback-error)))
                  ((not (equal callback-state state))
                   (finish process "400" "OpenAI OAuth Error"
                           "OpenAI OAuth state did not match.  You may close this tab."
                           nil "OpenAI OAuth state did not match"))
                  (callback-code
                   (finish process "200" "OpenAI OAuth Complete"
                           "OpenAI OAuth authorization succeeded.  You may close this tab."
                           callback-code))
                  (t
                   (finish process "400" "OpenAI OAuth Error"
                           "OpenAI OAuth callback did not include a code.  You may close this tab."
                           nil "OpenAI OAuth callback did not include a code"))))))))
      (unwind-protect
          (progn
            (setq server
                  (make-network-process
                   :name "gptel-openai-oauth-callback"
                   :server t
                   :host "localhost"
                   :service gptel--openai-oauth-redirect-port
                   :filter #'filter
                   :noquery t))
            (message "OpenAI OAuth authorization URL: %s" authorization-url)
            (ignore-errors (gui-set-selection 'CLIPBOARD authorization-url))
            (read-from-minibuffer
             (format "OpenAI OAuth URL copied to clipboard.  \
Press ENTER to open the authorization page.  \
If your browser does not open automatically, browse to %s: "
                     authorization-url))
            (browse-url authorization-url)
            (while (and (not code) (not error) (< (float-time) deadline))
              (accept-process-output nil 1))
            (cond
             (code code)
             (error (user-error "%s" error))
             (t (user-error "Timed out waiting for OpenAI OAuth callback"))))
        (when (process-live-p server)
          (delete-process server))))))

(defun gptel--openai-oauth-login-with-authorization-code (backend)
  "Authenticate BACKEND using OpenAI Authorization Code Flow with PKCE."
  (let* ((redirect-uri (format "http://localhost:%d%s"
                               gptel--openai-oauth-redirect-port
                               gptel--openai-oauth-redirect-path))
         (verifier (gptel-oauth--generate-code-verifier))
         (state (secure-hash 'sha256 (format "%s%s" (float-time) (random))))
         (authorization-url
          (gptel--openai-oauth-authorization-url redirect-uri verifier state))
         (code (gptel--openai-oauth-read-code authorization-url state))
         (token-plist
          (gptel--url-retrieve (concat gptel--openai-oauth-url "/oauth/token")
            :method 'post
            :data (url-build-query-string
                   `(("grant_type" "authorization_code")
                     ("client_id" ,gptel--openai-oauth-client-id)
                     ("code" ,code)
                     ("code_verifier" ,verifier)
                     ("redirect_uri" ,(url-hexify-string redirect-uri))))
            :content-type "application/x-www-form-urlencoded")))
    (gptel--openai-oauth-persist backend token-plist)))

;;;; OpenAI Oauth login and token handling

(defun gptel-openai-oauth-login (&optional account-hint method)
  "Authenticate using the OpenAI device flow for ACCOUNT-HINT.

If ACCOUNT-HINT is nil, use the default account.  Interactively, prompt
for an account hint from registered backends.
METHOD can be `authorization-code' for OAuth 2.0 Authorization
Code Flow with PKCE or `device' for OAuth 2.0 Device
Authorization Grant.  If METHOD is nil, use
`gptel-openai-oauth-login-method'."
  (interactive (list
                (gptel-oauth--read-account-hint
                 #'gptel-openai-oauth-p #'gptel-openai-oauth-account-hint
                 "Choose OpenAI account: "
                 "[Default account]"
                 "No OpenAI OAuth backends registered.  \
Please set one up with `gptel-make-openai-oauth' first")))
  (let ((oauth-backends (gptel--openai-oauth-get-backends-by-account-hint account-hint)))
    ;; It shall only be possible to login when there exists a corresponding backend
    (unless oauth-backends
      (user-error "No OpenAI OAuth backend found for account hint '%s'" account-hint))
    (let* ((backend (car oauth-backends))
           (token-plist
            (pcase (or method gptel-openai-oauth-login-method)
              ('authorization-code
               (gptel--openai-oauth-login-with-authorization-code backend))
              ('device
               (gptel--openai-oauth-login-with-device-code backend))
              (login-method
               (user-error "Unknown OpenAI OAuth login method: %S" login-method)))))
      (when (and (called-interactively-p 'interactive)
                 (plist-get token-plist :access_token))
        (message "Successfully logged in to OpenAI OAuth."))
      token-plist)))

(defun gptel--openai-oauth-persist (backend token-plist)
  "Persist TOKEN-PLIST for BACKEND.

Normalizes TOKEN-PLIST for storage, writes it to disk, and stores
it in BACKEND."
  (pcase-let (((map :access_token :expires_in :id_token :refresh_token) token-plist))
    ;; TODO Handle case where access_token was not granted
    (unless (and access_token expires_in refresh_token)
      (user-error "OpenAI OAuth Authentication failed"))
    (let ((token-processed
           (list :expires_at (+ (float-time) expires_in)
                 :access_token access_token
                 :refresh_token refresh_token
                 :id_token (gptel-oauth--jwt-payload id_token))))
      (gptel--openai-oauth-save-token (gptel-openai-oauth-account-hint backend) token-processed)
      (setf (gptel-openai-oauth-token backend) token-processed)
      token-plist)))

(defun gptel--openai-oauth-refresh (backend refresh-token)
  "Refresh BACKEND using REFRESH-TOKEN.

Returns the refreshed token payload after persisting it."
  (gptel--openai-oauth-persist
   backend
   (gptel--url-retrieve (concat gptel--openai-oauth-url "/oauth/token")
     :method 'post
     :data (url-build-query-string
            `(("grant_type"    "refresh_token")
              ("refresh_token" ,refresh-token)
              ("client_id"     ,gptel--openai-oauth-client-id)))
     :content-type "application/x-www-form-urlencoded")))

(defun gptel--openai-oauth-ensure (&optional backend)
  "Ensure BACKEND has a valid OpenAI OAuth token.

If BACKEND is nil, use `gptel-backend'.  Restore, refresh, or
reauthenticate as needed."
  (unless backend (setq backend gptel-backend))
  (unless (gptel-openai-oauth-token backend)
    (let ((account-hint (gptel-openai-oauth-account-hint backend)))
      (if-let* ((token-plist (gptel--openai-oauth-load-token account-hint)))
          (setf (gptel-openai-oauth-token backend) token-plist)
        (gptel-openai-oauth-login account-hint))))
  
  (let ((token-plist (gptel-openai-oauth-token backend)))
    (if-let* ((expiry (plist-get token-plist :expires_at))
              ;; Buffer of 10 second for expiry, to be made customizable later.
              ((> expiry (+ (float-time) 10))))
        t
      (if-let* ((refresh (plist-get token-plist :refresh_token)))
          (gptel--openai-oauth-refresh backend refresh)
        (gptel-openai-oauth-login (gptel-openai-oauth-account-hint backend)))))

(defun gptel--openai-oauth-get-backends-by-account-hint (account-hint)
  "Get all OpenAI OAuth backends for a specific account hint."
  (gptel-oauth--get-backends-by #'gptel-openai-oauth-p #'gptel-openai-oauth-account-hint account-hint))

(defun gptel--openai-oauth-generate-token-filename (account-hint)
  "Generate token filename for OpenAI OAuth backend with ACCOUNT-HINT."
  (gptel-oauth--generate-token-filename
   gptel--openai-oauth-token-file
   #'gptel-oauth--validate-account-hint
   account-hint))

(defun gptel--openai-oauth-restore-token-from-file (account-hint)
  "Restore OpenAI OAuth token from file using ACCOUNT-HINT."
  (gptel-oauth--restore-token-from-file #'gptel--openai-oauth-generate-token-filename account-hint))

(defun gptel--openai-oauth-save-token-to-file (account-hint token)
  "Save OpenAI OAuth TOKEN to file using ACCOUNT-HINT."
  (gptel-oauth--save-token-to-file #'gptel--openai-oauth-generate-token-filename account-hint token))

(defun gptel--openai-oauth-load-token (account-hint)
  "Load OpenAI OAuth token using customizable load function."
  (gptel-oauth--load-token
   #'gptel-openai-oauth-p
   #'gptel-openai-oauth-account-hint
   #'gptel-openai-oauth-token
   (lambda (b token) (setf (gptel-openai-oauth-token b) token))
   (or gptel-oauth-token-load-function 'gptel--openai-oauth-restore-token-from-file)
   account-hint)))

(defun gptel--openai-oauth-save-token (account-hint token)
  "Save OpenAI OAuth token using customizable save function."
  (gptel-oauth--save-token
   #'gptel-openai-oauth-p
   #'gptel-openai-oauth-account-hint
   (lambda (b token) (setf (gptel-openai-oauth-token b) token))
   (or gptel-oauth-token-save-function 'gptel--openai-oauth-save-token-to-file)
   account-hint
   token))

;;;; Oauth backend management

(defun gptel--openai-oauth-header (_info)
  "Return authentication headers for the current OpenAI OAuth backend.

_INFO is ignored.  Ensures `gptel-backend' has a valid token
before constructing the headers."
  (gptel--openai-oauth-ensure gptel-backend)
  (let* ((token (gptel-openai-oauth-token gptel-backend))
         (key (plist-get token :access_token))
         (account-id
          (or (map-nested-elt
               token '( :id_token :https://api.openai.com/auth
                        :chatgpt_account_id))
              (map-nested-elt
               token '( :id_token :https://api.openai.com/auth
                        :organizations 0 :id)))))
    (append
     `(("Authorization" . ,(concat "Bearer " key))
       ("Originator"    . "gptel"))
     (and account-id `(("ChatGPT-Account-Id" . ,account-id))))))

;;;###autoload
(cl-defun gptel-make-openai-oauth
    (name &key curl-args (stream t) request-params
          (header #'gptel--openai-oauth-header)
          (host "chatgpt.com")
          (protocol "https")
          (endpoint "/backend-api/codex/responses")
          (account-hint "")
          (models
           '( gpt-5.2 gpt-5.3-codex gpt-5.3-codex-spark gpt-5.4-mini
              gpt-5.4 gpt-5.5 gpt-5.6-sol gpt-5.6-terra gpt-5.6-luna)))
  "Register a ChatGPT Plus/Pro OAuth backend for gptel with NAME.

This backend uses ChatGPT OAuth tokens (not OpenAI API keys) and
targets the Codex endpoint on chatgpt.com.  Run
`gptel-openai-oauth-login' once to authenticate.

Keyword arguments:

ACCOUNT-HINT (optional) is an indicator of which OpenAI account to associate
the backend with. This enables backends to be logged in as a separate user. Note
that this is only a hint and will be used when a token is saved/loaded.

For other keyword argument meanings, see `gptel-make-openai'."
  (declare (indent 1))
  (gptel-oauth--validate-account-hint account-hint)
  (let ((backend (gptel--make-openai-oauth
                  :curl-args curl-args
                  :name name
                  :host host
                  :header header
                  :key nil
                  :models (gptel--process-models models)
                  :protocol protocol
                  :endpoint endpoint
                  :stream stream
                  :request-params request-params
                  :account-hint account-hint
                  :url (if protocol
                           (concat protocol "://" host endpoint)
                         (concat host endpoint)))))
    (prog1 backend
      (setf (alist-get name gptel--known-backends nil nil #'equal)
            backend))))

(provide 'gptel-openai-oauth)
;;; gptel-openai-oauth.el ends here
