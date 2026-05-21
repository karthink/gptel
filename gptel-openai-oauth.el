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
(require 'gptel-openai-responses)
(require 'gptel-oauth)

(defconst gptel--openai-oauth-client-id "app_EMoamEEZ73f0CkXaXp7hrann")
(defconst gptel--openai-oauth-url "https://auth.openai.com")

;; TODO: Change system to be able to store more than one backend token.
(defvar gptel--openai-oauth-token-file
  (expand-file-name ".cache/gptel-openai/openai-oauth-token"
                    user-emacs-directory))

;;;; OpenAI OAuth backend
(cl-defstruct (gptel-openai-oauth (:constructor gptel--make-openai-oauth)
                                  (:copier nil)
                                  (:include gptel-openai-responses))
  token)

(cl-defmethod gptel--request-data ((_backend gptel-openai-oauth) _prompts)
  "Return request data for the OpenAI OAuth backend.

Removes unsupported temperature settings from the payload."
  (let ((prompts-plist (cl-call-next-method)))
    (when (plist-member prompts-plist :temperature)
      (display-warning
       '(gptel gptel-openai-oauth)
       "Codex models do not support setting request temperature, ignoring `gptel-temperature'")
      (cl-remf prompts-plist :temperature))
    prompts-plist))

;;;; OpenAI OAuth login

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

(defun gptel-openai-oauth-login (&optional backend)
  "Authenticate BACKEND using the OpenAI device flow.

If BACKEND is nil, use `gptel-backend'.  Interactively, prompt
for or infer an OpenAI OAuth backend."
  ;; TODO: Handle the case when there are multiple openai-oauth backends
  ;; defined.  We want to pick the one to log into.
  (interactive (list
                (cond
                 ((gptel-openai-oauth-p gptel-backend)
                  gptel-backend)
                 ((cl-find-if #'gptel-openai-oauth-p gptel--known-backends
                              :key #'cdr))
                 (t (user-error "No OpenAI OAuth backend found.  \
Please set one up with `gptel-make-openai-oauth' first")))))
  (unless backend (setq backend gptel-backend))
  (unless (gptel-openai-oauth-p backend)
    (user-error "%s is not an OpenAI OAuth backend" (gptel-backend-name backend)))
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
      (gptel-oauth--write-token gptel--openai-oauth-token-file token-processed)
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
    (if-let* ((token-plist (gptel-oauth--read-token
                            gptel--openai-oauth-token-file)))
        (setf (gptel-openai-oauth-token backend) token-plist)
      (gptel-openai-oauth-login backend)))
  
  (let ((token-plist (gptel-openai-oauth-token backend)))
    (if-let* ((expiry (plist-get token-plist :expires_at))
              ;; Buffer of 10 second for expiry, to be made customizable later.
              ((> expiry (+ (float-time) 10))))
        t
      (if-let* ((refresh (plist-get token-plist :refresh_token)))
          (gptel--openai-oauth-refresh backend refresh)
        (gptel-openai-oauth-login backend)))))

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
          (models 
           '(gpt-5.2 gpt-5.3-codex gpt-5.3-codex-spark gpt-5.4-mini gpt-5.4)))
  "Register a ChatGPT Plus/Pro OAuth backend for gptel with NAME.

This backend uses ChatGPT OAuth tokens (not OpenAI API keys) and
targets the Codex endpoint on chatgpt.com.  Run
`gptel-openai-oauth-login' once to authenticate.

For keyword argument meanings, see `gptel-make-openai'."
  (declare (indent 1))
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
                  :url (if protocol
                           (concat protocol "://" host endpoint)
                         (concat host endpoint)))))
    (prog1 backend
      (setf (alist-get name gptel--known-backends nil nil #'equal)
            backend))))

(provide 'gptel-openai-oauth)
;;; gptel-openai-oauth.el ends here
