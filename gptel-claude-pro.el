;;; gptel-claude-pro.el --- Claude Pro/Max OAuth support for gptel  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Karthik Chikmagalur

;; Author: Karthik Chikmagalur
;; Keywords: comm, convenience

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

;; This file adds OAuth authentication support for Claude Pro/Max plans to gptel.
;; It allows users to authenticate with their Claude Pro account using OAuth
;; instead of API keys, billing against their Pro/Max plan.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'gptel)
(require 'gptel-anthropic)

(declare-function gptel-context--collect-media "gptel-context")
(declare-function gptel--base64-encode "gptel")

;; OAuth Configuration
(defconst gptel-claude-pro--client-id "9d1c250a-e61b-44d9-88ed-5944d1962f5e"
  "OAuth client ID for Claude Pro authentication.")

(defconst gptel-claude-pro--auth-url "https://claude.ai/oauth/authorize"
  "OAuth authorization URL for Claude Pro.")

(defconst gptel-claude-pro--token-url "https://api.anthropic.com/oauth/token"
  "OAuth token exchange URL for Claude Pro.")

(defconst gptel-claude-pro--redirect-uri "http://localhost:8765/callback"
  "OAuth redirect URI for receiving authorization code.")

(defconst gptel-claude-pro--scope "claude:api"
  "OAuth scope for Claude Pro API access.")

(defvar gptel-claude-pro--token-cache nil
  "Cache for OAuth tokens. Structure: (access-token . expiry-time).")

(defvar gptel-claude-pro--refresh-token nil
  "Refresh token for obtaining new access tokens.")

(defvar gptel-claude-pro--server-process nil
  "Process for the local OAuth callback server.")

(defvar gptel-claude-pro--auth-code nil
  "Authorization code received from OAuth flow.")

(defvar gptel-claude-pro--pkce-verifier nil
  "PKCE code verifier for OAuth flow.")

;; Custom variables
(defcustom gptel-claude-pro-token-file
  (expand-file-name "gptel-claude-pro-token" user-emacs-directory)
  "File for storing encrypted OAuth tokens."
  :type 'file
  :group 'gptel)

(defcustom gptel-claude-pro-auto-refresh t
  "Whether to automatically refresh tokens before expiry."
  :type 'boolean
  :group 'gptel)

;; Backend structure
(cl-defstruct (gptel-claude-pro (:constructor gptel--make-claude-pro)
                                (:copier nil)
                                (:include gptel-anthropic)))

;; PKCE (Proof Key for Code Exchange) implementation
(defun gptel-claude-pro--generate-pkce ()
  "Generate PKCE code verifier and challenge for OAuth flow."
  (let* ((verifier (gptel-claude-pro--random-string 128))
         (challenge (gptel-claude-pro--base64url-encode
                    (secure-hash 'sha256 verifier nil nil t))))
    (setq gptel-claude-pro--pkce-verifier verifier)
    (cons verifier challenge)))

(defun gptel-claude-pro--random-string (length)
  "Generate a random string of LENGTH characters."
  (let ((chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~"))
    (apply #'string
           (cl-loop repeat length
                    collect (aref chars (random (length chars)))))))

(defun gptel-claude-pro--base64url-encode (string)
  "Base64url encode STRING (without padding)."
  (let ((b64 (base64-encode-string string t)))
    (replace-regexp-in-string
     "=" ""
     (replace-regexp-in-string
      "/" "_"
      (replace-regexp-in-string "\\+" "-" b64)))))

;; OAuth callback server
(defun gptel-claude-pro--start-callback-server ()
  "Start a local server to receive OAuth callback."
  (when gptel-claude-pro--server-process
    (delete-process gptel-claude-pro--server-process))
  
  (setq gptel-claude-pro--server-process
        (make-network-process
         :name "gptel-claude-pro-oauth-server"
         :buffer "*gptel-claude-pro-oauth*"
         :family 'ipv4
         :service 8765
         :server t
         :sentinel #'gptel-claude-pro--server-sentinel
         :filter #'gptel-claude-pro--server-filter)))

(defun gptel-claude-pro--server-filter (process string)
  "Handle incoming OAuth callback from PROCESS with STRING."
  (when (string-match "GET /callback\\?code=\\([^& \r\n]+\\)" string)
    (let ((code (match-string 1 string)))
      (setq gptel-claude-pro--auth-code code)
      
      ;; Send success response to browser
      (process-send-string
       process
       (concat "HTTP/1.1 200 OK\r\n"
               "Content-Type: text/html\r\n"
               "Connection: close\r\n\r\n"
               "<html><body><h1>Authentication successful!</h1>"
               "<p>You can close this window and return to Emacs.</p>"
               "</body></html>\r\n"))
      
      ;; Close the connection
      (delete-process process)
      
      ;; Exchange code for tokens
      (gptel-claude-pro--exchange-code code))))

(defun gptel-claude-pro--server-sentinel (process event)
  "Handle server PROCESS events EVENT."
  (when (string-match "deleted\\|connection broken" event)
    (when (eq process gptel-claude-pro--server-process)
      (setq gptel-claude-pro--server-process nil))))

;; OAuth flow
(defun gptel-claude-pro--launch-browser ()
  "Launch browser for OAuth authorization."
  (let* ((pkce (gptel-claude-pro--generate-pkce))
         (challenge (cdr pkce))
         (state (gptel-claude-pro--random-string 32))
         (auth-url (concat gptel-claude-pro--auth-url
                          "?response_type=code"
                          "&client_id=" (url-hexify-string gptel-claude-pro--client-id)
                          "&redirect_uri=" (url-hexify-string gptel-claude-pro--redirect-uri)
                          "&scope=" (url-hexify-string gptel-claude-pro--scope)
                          "&state=" state
                          "&code_challenge=" challenge
                          "&code_challenge_method=S256")))
    
    ;; Start callback server
    (gptel-claude-pro--start-callback-server)
    
    ;; Open browser
    (browse-url auth-url)
    
    (message "Please authenticate in your browser. Waiting for callback...")))

(defun gptel-claude-pro--exchange-code (code)
  "Exchange authorization CODE for access and refresh tokens."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/x-www-form-urlencoded")))
        (url-request-data
         (concat "grant_type=authorization_code"
                 "&code=" (url-hexify-string code)
                 "&redirect_uri=" (url-hexify-string gptel-claude-pro--redirect-uri)
                 "&client_id=" (url-hexify-string gptel-claude-pro--client-id)
                 "&code_verifier=" gptel-claude-pro--pkce-verifier)))
    
    (url-retrieve
     gptel-claude-pro--token-url
     #'gptel-claude-pro--handle-token-response
     nil t)))

(defun gptel-claude-pro--handle-token-response (status)
  "Handle token response with STATUS from OAuth server."
  (if-let ((error-val (plist-get status :error)))
      (message "OAuth error: %s" error-val)
    (goto-char (point-min))
    (re-search-forward "\r?\n\r?\n")
    (let* ((json-object-type 'plist)
           (response (json-read))
           (access-token (plist-get response :access_token))
           (refresh-token (plist-get response :refresh_token))
           (expires-in (plist-get response :expires_in)))
      
      (when access-token
        ;; Store tokens
        (gptel-claude-pro--store-token access-token expires-in)
        (setq gptel-claude-pro--refresh-token refresh-token)
        
        ;; Save refresh token to file
        (gptel-claude-pro--save-refresh-token refresh-token)
        
        (message "Claude Pro authentication successful!")
        
        ;; Stop callback server
        (when gptel-claude-pro--server-process
          (delete-process gptel-claude-pro--server-process))))))

(defun gptel-claude-pro--refresh-token ()
  "Refresh the access token using the refresh token."
  (unless gptel-claude-pro--refresh-token
    (gptel-claude-pro--load-refresh-token))
  
  (if gptel-claude-pro--refresh-token
      (let ((url-request-method "POST")
            (url-request-extra-headers
             '(("Content-Type" . "application/x-www-form-urlencoded")))
            (url-request-data
             (concat "grant_type=refresh_token"
                     "&refresh_token=" (url-hexify-string gptel-claude-pro--refresh-token)
                     "&client_id=" (url-hexify-string gptel-claude-pro--client-id))))
        
        (with-current-buffer
            (url-retrieve-synchronously gptel-claude-pro--token-url t)
          (goto-char (point-min))
          (re-search-forward "\r?\n\r?\n")
          (let* ((json-object-type 'plist)
                 (response (json-read))
                 (access-token (plist-get response :access_token))
                 (expires-in (plist-get response :expires_in)))
            
            (if access-token
                (progn
                  (gptel-claude-pro--store-token access-token expires-in)
                  access-token)
              (error "Failed to refresh token")))))
    (error "No refresh token available. Please authenticate again")))

;; Token management
(defun gptel-claude-pro--store-token (token expires-in)
  "Store access TOKEN with EXPIRES-IN seconds."
  (let ((expiry-time (time-add (current-time) (seconds-to-time expires-in))))
    (setq gptel-claude-pro--token-cache (cons token expiry-time))))

(defun gptel-claude-pro--get-token ()
  "Get a valid access token, refreshing if necessary."
  (cond
   ;; No token cached
   ((null gptel-claude-pro--token-cache)
    (if gptel-claude-pro--refresh-token
        (gptel-claude-pro--refresh-token)
      (gptel-claude-pro-authenticate)
      nil))
   
   ;; Token expired or about to expire (5 minute buffer)
   ((time-less-p (cdr gptel-claude-pro--token-cache)
                 (time-add (current-time) (seconds-to-time 300)))
    (gptel-claude-pro--refresh-token))
   
   ;; Token still valid
   (t (car gptel-claude-pro--token-cache))))

;; Token persistence
(defun gptel-claude-pro--save-refresh-token (token)
  "Save refresh TOKEN to encrypted file."
  (let ((coding-system-for-write 'utf-8))
    (with-temp-file gptel-claude-pro-token-file
      (insert (format "%S" token))))
  ;; Set restrictive permissions
  (set-file-modes gptel-claude-pro-token-file #o600))

(defun gptel-claude-pro--load-refresh-token ()
  "Load refresh token from file."
  (when (file-exists-p gptel-claude-pro-token-file)
    (with-temp-buffer
      (insert-file-contents gptel-claude-pro-token-file)
      (setq gptel-claude-pro--refresh-token (read (current-buffer))))))

;; Override curl args to include OAuth token
(defun gptel-claude-pro--curl-args ()
  "Build curl arguments for BACKEND with PROMPTS, including OAuth token."
  (let ((token (gptel-claude-pro--get-token)))
    (unless token
      (error "No valid OAuth token. Please authenticate first"))
    
    ;; Get base args from parent method
    (let ((args (cl-call-next-method)))
      ;; Replace API key header with OAuth Bearer token
      (setf (nth (cl-position "-H" args :test #'string=) args)
            (format "-HAuthorization: Bearer %s" token))
      args)))

;; Public interface
;;;###autoload
(defun gptel-claude-pro-authenticate ()
  "Authenticate with Claude Pro using OAuth."
  (interactive)
  (gptel-claude-pro--launch-browser))

;;;###autoload
(defun gptel-claude-pro-logout ()
  "Logout from Claude Pro by clearing stored tokens."
  (interactive)
  (setq gptel-claude-pro--token-cache nil
        gptel-claude-pro--refresh-token nil)
  (when (file-exists-p gptel-claude-pro-token-file)
    (delete-file gptel-claude-pro-token-file))
  (message "Logged out from Claude Pro"))

;;;###autoload
(cl-defun gptel-make-claude-pro
    (name &key
          (stream t)
          (models gptel--anthropic-models)
          (host "api.anthropic.com")
          (protocol "https")
          (endpoint "/v1/messages")
          curl-args
          request-params)
  "Register a Claude Pro backend with OAuth authentication.

NAME is a string naming this backend.

Keywords:

STREAM (boolean) specifies whether to use streaming responses.

MODELS is a list of model symbols. Defaults to Claude models
available in Pro plans.

HOST, PROTOCOL, ENDPOINT configure the API connection.

CURL-ARGS are additional arguments for Curl.

REQUEST-PARAMS are additional request parameters."
  (declare (indent 1))
  
  ;; Load any saved refresh token
  (gptel-claude-pro--load-refresh-token)
  
  (let ((backend (gptel--make-claude-pro
                  :name name
                  :host host
                  :protocol protocol
                  :endpoint endpoint
                  :stream stream
                  :models (gptel--process-models models)
                  :curl-args (lambda () (append curl-args (gptel-claude-pro--curl-args)))
                  :request-params request-params
                  :url (lambda ()
                         (concat protocol "://" host endpoint)))))
    
    ;; Register the backend
    (setf (alist-get name gptel--known-backends nil nil #'equal) backend)
    
    ;; Return the backend
    backend))

(provide 'gptel-claude-pro)
;;; gptel-claude-pro.el ends here
