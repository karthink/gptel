;;; gptel-openai.el ---  ChatGPT suppport for gptel  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025  Karthik Chikmagalur

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

;; This file adds support for the ChatGPT API to gptel

;;; Code:
(require 'cl-generic)
(eval-when-compile
  (require 'cl-lib))
(require 'cl-lib)
(require 'map)
(require 'browse-url)
(require 'url-util)

(defvar gptel-model)
(defvar gptel-stream)
(defvar gptel-use-curl)
(defvar gptel-backend)
(defvar gptel-temperature)
(defvar gptel-max-tokens)
(defvar gptel--system-message)
(defvar json-object-type)
(defvar gptel-mode)
(defvar gptel-track-response)
(defvar gptel-track-media)
(defvar gptel-use-tools)
(defvar gptel-tools)
(defvar gptel--schema)
(defvar gptel--request-params)
(declare-function gptel-context--collect-media "gptel-context")
(declare-function gptel--base64-encode "gptel")
(declare-function gptel--trim-prefixes "gptel")
(declare-function gptel--parse-media-links "gptel")
(declare-function gptel--model-capable-p "gptel")
(declare-function gptel--model-name "gptel")
(declare-function gptel--get-api-key "gptel")
(declare-function gptel--insert-file-string "gptel")
(declare-function prop-match-value "text-property-search")
(declare-function text-property-search-backward "text-property-search")
(declare-function json-read "json")
(declare-function gptel-prompt-prefix-string "gptel")
(declare-function gptel-response-prefix-string "gptel")
(declare-function gptel--merge-plists "gptel")
(declare-function gptel--model-request-params "gptel")
(declare-function gptel-context--wrap "gptel-context")
(declare-function gptel--inject-prompt "gptel")
(declare-function gptel--parse-tools "gptel")
(declare-function gptel--parse-schema "gptel")
(declare-function gptel--preprocess-schema "gptel")
(declare-function gptel--dispatch-schema-type "gptel")

;; JSON conversion semantics used by gptel
;; empty object "{}" => empty list '() == nil
;; null              => :null
;; false             => :json-false

;; TODO(tool) Except when reading JSON from a string, where null => nil

(defmacro gptel--json-read ()
  (if (fboundp 'json-parse-buffer)
      `(json-parse-buffer
        :object-type 'plist
        :null-object :null
        :false-object :json-false)
    (require 'json)
    (defvar json-object-type)
    (defvar json-null)
    (declare-function json-read "json" ())
    `(let ((json-object-type 'plist)
           (json-null :null))
      (json-read))))

(defmacro gptel--json-read-string (str)
  (if (fboundp 'json-parse-string)
      `(json-parse-string ,str
        :object-type 'plist
        :null-object nil
        :false-object :json-false)
    (require 'json)
    (defvar json-object-type)
    (declare-function json-read-from-string "json" ())
    `(let ((json-object-type 'plist))
      (json-read-from-string ,str))))

(defmacro gptel--json-encode (object)
  (if (fboundp 'json-serialize)
      `(json-serialize ,object
        :null-object :null
        :false-object :json-false)
    (require 'json)
    (defvar json-false)
    (defvar json-null)
    (declare-function json-encode "json" (object))
    `(let ((json-false :json-false)
           (json-null  :null))
      (json-encode ,object))))

(defun gptel--process-models (models)
  "Convert items in MODELS to symbols with appropriate properties."
  (let ((models-processed))
    (dolist (model models)
      (cl-etypecase model
        (string (push (intern model) models-processed))
        (symbol (push model models-processed))
        (cons
         (cl-destructuring-bind (name . props) model
           (setf (symbol-plist name)
                 ;; MAYBE: Merging existing symbol plists is safer, but makes it
                 ;; difficult to reset a symbol plist, since removing keys from
                 ;; it (as opposed to setting them to nil) is more work.
                 ;;
                 ;; (map-merge 'plist (symbol-plist name) props)
                 props)
           (push name models-processed)))))
    (nreverse models-processed)))

;;; Common backend struct for LLM support
(defvar gptel--known-backends nil
  "Alist of LLM backends known to gptel.

This is an alist mapping user-provided names to backend structs,
see `gptel-backend'.

You can have more than one backend pointing to the same resource
with differing settings.")

(defun gptel-get-backend (name)
  "Return gptel backend with NAME.

Throw an error if there is no match."
  (or (alist-get name gptel--known-backends nil nil #'equal)
      (user-error "Backend %s is not known to be defined"
                  name)))

(gv-define-setter gptel-get-backend (val name)
  `(setf (alist-get ,name gptel--known-backends
          nil t #'equal)
    ,val))

(cl-defstruct
    (gptel-backend (:constructor gptel--make-backend)
                   (:copier gptel--copy-backend))
  name host header protocol stream
  endpoint key models url request-params
  curl-args
  (coding-system nil :documentation "Can be set to `binary' if the backend expects non UTF-8 output."))

;;; OpenAI (ChatGPT)
(cl-defstruct (gptel-openai (:constructor gptel--make-openai)
                            (:copier nil)
                            (:include gptel-backend)))

(cl-defstruct (gptel-openai-chatgpt (:constructor gptel--make-openai-chatgpt)
                                    (:copier nil)
                                    (:include gptel-openai))
  token)

(defconst gptel--openai-chatgpt-client-id "app_EMoamEEZ73f0CkXaXp7hrann")
(defconst gptel--openai-chatgpt-issuer "https://auth.openai.com")
(defconst gptel--openai-chatgpt-safety-margin 30
  "Seconds before expiry when ChatGPT OAuth tokens are refreshed.")

(defcustom gptel-openai-chatgpt-token-file
  (expand-file-name ".cache/gptel/chatgpt-token" user-emacs-directory)
  "File where ChatGPT OAuth tokens are cached for gptel backends."
  :type 'file
  :group 'gptel)

(defcustom gptel-openai-chatgpt-instructions
  "You are a coding assistant."
  "Default instructions sent to ChatGPT Codex OAuth backend.

If `gptel--system-message' is non-nil for a request, it is used
instead."
  :type 'string
  :group 'gptel)

(defvar url-http-end-of-headers)

(defun gptel--openai-chatgpt-backend (&optional backend)
  "Return ChatGPT OAuth backend.

If BACKEND is non-nil, return it after verifying it is a
`gptel-openai-chatgpt' backend.  Otherwise use `gptel-backend', then
fall back to the first known ChatGPT OAuth backend."
  (cond
   ((and backend (gptel-openai-chatgpt-p backend)) backend)
   ((and (boundp 'gptel-backend)
         gptel-backend
         (gptel-openai-chatgpt-p gptel-backend))
    gptel-backend)
   ((let (found)
      (dolist (backend (mapcar #'cdr gptel--known-backends) found)
        (when (gptel-openai-chatgpt-p backend)
          (setq found backend)))))
   (t (user-error "No ChatGPT OAuth backend found. Use `gptel-make-openai-chatgpt' first"))))

(defun gptel--openai-chatgpt-save-token (token)
  "Persist ChatGPT OAuth TOKEN to `gptel-openai-chatgpt-token-file'."
  (let ((print-length nil)
        (print-level nil)
        (coding-system-for-write 'utf-8-unix))
    (make-directory (file-name-directory gptel-openai-chatgpt-token-file) t)
    (write-region (prin1-to-string token) nil gptel-openai-chatgpt-token-file nil :silent)
    token))

(defun gptel--openai-chatgpt-restore-token ()
  "Restore ChatGPT OAuth token from `gptel-openai-chatgpt-token-file'."
  (when (file-exists-p gptel-openai-chatgpt-token-file)
    (let ((coding-system-for-read 'utf-8-auto-dos))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally gptel-openai-chatgpt-token-file)
        (goto-char (point-min))
        (read (current-buffer))))))

(defun gptel--openai-chatgpt-form-encode (pairs)
  "URL encode alist PAIRS as x-www-form-urlencoded data."
  (mapconcat
   (lambda (pair)
     (format "%s=%s"
             (url-hexify-string (car pair))
             (url-hexify-string (cdr pair))))
   pairs "&"))

(defun gptel--openai-chatgpt-request (url data headers &optional form-encoded)
  "POST DATA to URL with HEADERS and return a plist response.

If FORM-ENCODED is non-nil, DATA is sent as x-www-form-urlencoded;
otherwise DATA is JSON-encoded.  Return plist with keys :status,
:body and :raw."
  (let* ((url-request-method "POST")
         (url-request-data
          (if form-encoded
              data
            (encode-coding-string (gptel--json-encode data) 'utf-8)))
         (url-mime-accept-string "application/json")
         (url-request-extra-headers
          `(("content-type" . ,(if form-encoded
                                    "application/x-www-form-urlencoded"
                                  "application/json"))
            ,@headers))
         (buffer (url-retrieve-synchronously url 'silent)))
    (unless buffer
      (error "Request failed for %s" url))
    (with-current-buffer buffer
      (unwind-protect
          (let* ((status (progn
                           (goto-char (point-min))
                           (if (re-search-forward "HTTP/[.0-9]+ +\\([0-9]+\\)" nil t)
                               (string-to-number (match-string 1))
                             0)))
                 (raw (progn
                        (goto-char (or url-http-end-of-headers (point-min)))
                        (buffer-substring-no-properties (point) (point-max))))
                 (body (unless (string-empty-p (string-trim raw))
                         (condition-case nil
                             (progn
                               (goto-char (or url-http-end-of-headers (point-min)))
                               (gptel--json-read))
                           (error nil)))))
            (list :status status :body body :raw raw))
        (kill-buffer buffer)))))

(defun gptel--openai-chatgpt-base64url-decode (input)
  "Decode base64url INPUT and return a decoded string."
  (let* ((base64 (replace-regexp-in-string "_" "/"
                   (replace-regexp-in-string "-" "+" input)))
         (padding (mod (- 4 (mod (length base64) 4)) 4))
         (padded (concat base64 (make-string padding ?=))))
    (decode-coding-string (base64-decode-string padded) 'utf-8 t)))

(defun gptel--openai-chatgpt-jwt-payload (jwt)
  "Return decoded JWT payload string from JWT, or nil."
  (when (and (stringp jwt) (string-match-p "\\.`[^.]+\\.[^.]+\\.[^.]+\\'" jwt))
    (condition-case nil
        (gptel--openai-chatgpt-base64url-decode (cadr (split-string jwt "\\.")))
      (error nil))))

(defun gptel--openai-chatgpt-extract-account-id (token)
  "Extract ChatGPT account id from OAuth TOKEN payload."
  (let* ((payload (or (gptel--openai-chatgpt-jwt-payload (plist-get token :id_token))
                      (gptel--openai-chatgpt-jwt-payload (plist-get token :access_token)))))
    (when payload
      (or (when (string-match "\"chatgpt_account_id\"[ \t\n\r]*:[ \t\n\r]*\"\\([^\"]+\\)\"" payload)
            (match-string 1 payload))
          (when (string-match "\"organizations\"[ \t\n\r]*:[ \t\n\r]*\\[[^]]*\"id\"[ \t\n\r]*:[ \t\n\r]*\"\\([^\"]+\\)\"" payload)
            (match-string 1 payload))))))

(defun gptel--openai-chatgpt-refresh-token (backend)
  "Refresh OAuth token for ChatGPT BACKEND."
  (let* ((token (gptel-openai-chatgpt-token backend))
         (refresh-token (plist-get token :refresh_token)))
    (unless refresh-token
      (user-error "Missing ChatGPT refresh token. Run `M-x gptel-openai-chatgpt-login'"))
    (let* ((resp (gptel--openai-chatgpt-request
                  (concat gptel--openai-chatgpt-issuer "/oauth/token")
                  (gptel--openai-chatgpt-form-encode
                   `(("grant_type" . "refresh_token")
                     ("refresh_token" . ,refresh-token)
                     ("client_id" . ,gptel--openai-chatgpt-client-id)))
                  nil t))
           (status (plist-get resp :status))
           (body (plist-get resp :body)))
      (unless (and (eql status 200) body)
        (user-error "Failed to refresh ChatGPT token (HTTP %s): %s"
                    status
                    (or (plist-get body :error)
                        (plist-get body :error_description)
                        (plist-get resp :raw))))
      (unless (plist-get body :refresh_token)
        (plist-put body :refresh_token refresh-token))
      (plist-put body :account_id (or (gptel--openai-chatgpt-extract-account-id body)
                                      (plist-get token :account_id)))
      (plist-put body :expires_at
                 (+ (float-time) (or (plist-get body :expires_in) 3600)
                    (- gptel--openai-chatgpt-safety-margin)))
      (setf (gptel-openai-chatgpt-token backend) body)
      (gptel--openai-chatgpt-save-token body)
      body)))

(defun gptel--openai-chatgpt-ensure-token (&optional backend)
  "Ensure ChatGPT OAuth token exists and is valid for BACKEND."
  (let* ((backend (gptel--openai-chatgpt-backend backend))
         (token (or (gptel-openai-chatgpt-token backend)
                    (gptel--openai-chatgpt-restore-token))))
    (unless token
      (if noninteractive
          (user-error "No ChatGPT token found. Run `M-x gptel-openai-chatgpt-login' first")
        (gptel-openai-chatgpt-login backend)
        (setq token (gptel-openai-chatgpt-token backend))))
    (setf (gptel-openai-chatgpt-token backend) token)
    (when (<= (or (plist-get token :expires_at) 0)
              (+ (float-time) gptel--openai-chatgpt-safety-margin))
      (setq token (gptel--openai-chatgpt-refresh-token backend)))
    token))

(defun gptel--openai-chatgpt-header ()
  "Return headers for ChatGPT OAuth requests."
  (let* ((token (gptel--openai-chatgpt-ensure-token))
         (headers
          `(("Authorization" . ,(concat "Bearer " (plist-get token :access_token)))
            ("originator" . "gptel"))))
    (when-let* ((account-id (plist-get token :account_id)))
      (push (cons "ChatGPT-Account-Id" account-id) headers))
    headers))

(defun gptel--openai-chatgpt-plist-remove-keys (plist keys)
  "Return copy of PLIST without properties in KEYS."
  (let (result)
    (while plist
      (let ((k (pop plist))
            (v (pop plist)))
        (unless (memq k keys)
          (setq result (append result (list k v))))))
    result))

(defun gptel--openai-chatgpt-response-text (response)
  "Extract response text from RESPONSE object.

RESPONSE can be either a full response object or a wrapper containing
it in :response."
  (let* ((obj (or (plist-get response :response) response))
         (direct (plist-get obj :output_text)))
    (or (and (stringp direct) direct)
        (when-let* ((output (plist-get obj :output))
                    ((vectorp output)))
          (mapconcat
           #'identity
           (delq nil
                 (mapcar
                  (lambda (item)
                    (when-let* ((content (plist-get item :content))
                                ((vectorp content)))
                      (mapconcat
                       #'identity
                       (delq nil
                             (mapcar
                              (lambda (part)
                                (or (and (string= (plist-get part :type) "output_text")
                                         (plist-get part :text))
                                    (and (stringp (plist-get part :text))
                                         (plist-get part :text))))
                              content))
                       "")))
                  output))
           "\n")))))

(cl-defmethod gptel--request-data ((_backend gptel-openai-chatgpt) _prompts)
  "Build request payload for ChatGPT Codex endpoint.

This endpoint is Responses-API compatible and requires `instructions',
`store: false' and streaming mode."
  (let* ((system gptel--system-message)
         (gptel--system-message nil)
         (gptel-temperature nil)
         (gptel-max-tokens nil)
         (payload (cl-call-next-method)))
    (setq payload (plist-put payload :input (plist-get payload :messages)))
    (setq payload (gptel--openai-chatgpt-plist-remove-keys
                   payload
                   '(:messages :temperature :max_tokens :max_completion_tokens)))
    (setq payload (plist-put payload :instructions
                             (or (and (stringp system) (not (string-empty-p system)) system)
                                 gptel-openai-chatgpt-instructions)))
    (setq payload (plist-put payload :store :json-false))
    (setq payload (plist-put payload :stream t))
    payload))

(cl-defmethod gptel-curl--parse-stream ((_backend gptel-openai-chatgpt) info)
  "Parse ChatGPT Codex streaming responses and return text delta."
  (let (chunks)
    (condition-case nil
        (while (re-search-forward "^data:" nil t)
          (save-match-data
            (if (looking-at " *\\[DONE\\]")
                nil
              (when-let* ((response (gptel--json-read))
                          (type (plist-get response :type)))
                (cond
                 ((string= type "response.output_text.delta")
                  (when-let* ((delta (plist-get response :delta)))
                    (plist-put info :chatgpt-seen-delta t)
                    (push delta chunks)))
                 ((string= type "response.completed")
                  (when-let* ((text (gptel--openai-chatgpt-response-text response))
                              ((not (string-empty-p text))))
                    (unless (or chunks (plist-get info :chatgpt-seen-delta))
                      (push text chunks)))))))))
      (error (goto-char (match-beginning 0))))
    (apply #'concat (nreverse chunks))))

(cl-defmethod gptel--parse-response ((_backend gptel-openai-chatgpt) response _info)
  "Parse non-streaming ChatGPT Codex RESPONSE and return response text."
  (when-let* ((text (gptel--openai-chatgpt-response-text response))
              ((not (string-empty-p text))))
    text))

;;;###autoload
(defun gptel-openai-chatgpt-login (&optional backend)
  "Login to ChatGPT OAuth for gptel backend BACKEND.

Uses the headless device flow compatible with ChatGPT Plus/Pro Codex
access.  Tokens are cached in `gptel-openai-chatgpt-token-file'."
  (interactive)
  (let* ((backend (gptel--openai-chatgpt-backend backend))
         (device-resp
          (gptel--openai-chatgpt-request
           (concat gptel--openai-chatgpt-issuer "/api/accounts/deviceauth/usercode")
           `(:client_id ,gptel--openai-chatgpt-client-id)
           `(("User-Agent" . ,(format "gptel/%s" (or emacs-version "emacs"))))))
         (device-body (plist-get device-resp :body))
         (status (plist-get device-resp :status)))
    (unless (and (eql status 200) device-body)
      (user-error "Failed to start ChatGPT login (HTTP %s): %s"
                  status (or (plist-get device-resp :raw) "unknown response")))
    (let* ((device-auth-id (plist-get device-body :device_auth_id))
           (user-code (plist-get device-body :user_code))
           (interval (max 1 (truncate (string-to-number (or (plist-get device-body :interval) "5")))))
           auth-code
           code-verifier)
      (unless (and device-auth-id user-code)
        (user-error "ChatGPT login response missing device auth fields"))
      (gui-set-selection 'CLIPBOARD user-code)
      (message "ChatGPT code copied: %s" user-code)
      (browse-url (concat gptel--openai-chatgpt-issuer "/codex/device"))
      (read-from-minibuffer
       (format "Enter code %s at %s/codex/device, then press ENTER to continue. "
               user-code gptel--openai-chatgpt-issuer))
      (while (not auth-code)
        (let* ((poll-resp
                (gptel--openai-chatgpt-request
                 (concat gptel--openai-chatgpt-issuer "/api/accounts/deviceauth/token")
                 `(:device_auth_id ,device-auth-id :user_code ,user-code)
                 `(("User-Agent" . ,(format "gptel/%s" (or emacs-version "emacs"))))))
               (poll-status (plist-get poll-resp :status))
               (poll-body (plist-get poll-resp :body)))
          (cond
           ((and (eql poll-status 200) poll-body)
            (setq auth-code (plist-get poll-body :authorization_code)
                  code-verifier (plist-get poll-body :code_verifier)))
           ((memq poll-status '(403 404))
            (sleep-for (+ interval 3)))
           (t
            (user-error "ChatGPT authorization failed (HTTP %s): %s"
                        poll-status
                        (or (plist-get poll-resp :raw) "unknown response"))))))
      (unless (and auth-code code-verifier)
        (user-error "ChatGPT authorization did not return exchange credentials"))
      (let* ((token-resp
              (gptel--openai-chatgpt-request
               (concat gptel--openai-chatgpt-issuer "/oauth/token")
               (gptel--openai-chatgpt-form-encode
                `(("grant_type" . "authorization_code")
                  ("code" . ,auth-code)
                  ("redirect_uri" . "https://auth.openai.com/deviceauth/callback")
                  ("client_id" . ,gptel--openai-chatgpt-client-id)
                  ("code_verifier" . ,code-verifier)))
               nil t))
             (token-status (plist-get token-resp :status))
             (token (plist-get token-resp :body)))
        (unless (and (eql token-status 200) token)
          (user-error "ChatGPT token exchange failed (HTTP %s): %s"
                      token-status
                      (or (plist-get token-resp :raw) "unknown response")))
        (plist-put token :account_id (gptel--openai-chatgpt-extract-account-id token))
        (plist-put token :expires_at
                   (+ (float-time) (or (plist-get token :expires_in) 3600)
                      (- gptel--openai-chatgpt-safety-margin)))
        (setf (gptel-openai-chatgpt-token backend) token)
        (gptel--openai-chatgpt-save-token token)
        (message "Successfully logged in to ChatGPT for gptel.")))))

;;;###autoload
(defun gptel-openai-chatgpt-logout (&optional backend)
  "Clear cached ChatGPT OAuth credentials for BACKEND."
  (interactive)
  (let ((backend (gptel--openai-chatgpt-backend backend)))
    (setf (gptel-openai-chatgpt-token backend) nil)
    (when (file-exists-p gptel-openai-chatgpt-token-file)
      (delete-file gptel-openai-chatgpt-token-file))
    (message "Cleared ChatGPT OAuth credentials.")))

;;;###autoload
(cl-defun gptel-make-openai-chatgpt
    (name &key curl-args request-params stream
          (header #'gptel--openai-chatgpt-header)
          (host "chatgpt.com")
          (protocol "https")
          (endpoint "/backend-api/codex/responses")
          (models '(gpt-5.1-codex-max gpt-5.1-codex-mini gpt-5.1-codex
                    gpt-5.2 gpt-5.2-codex gpt-5.3-codex)))
  "Register a ChatGPT Plus/Pro OAuth backend for gptel with NAME.

This backend uses ChatGPT OAuth tokens (not OpenAI API keys) and
targets the Codex endpoint on chatgpt.com.  Run
`gptel-openai-chatgpt-login' once to authenticate.

For keyword argument meanings, see `gptel-make-openai'."
  (declare (indent 1))
  (let ((backend (gptel--make-openai-chatgpt
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

;; How the following function works:
;;
;; The OpenAI API returns a stream of data chunks.  Each data chunk has a
;; component that can be parsed as JSON.  Besides metadata, each chunk has
;; either some text or part of a tool call.
;;
;; If we find text, we collect it in a list, concat them at the end and return
;; it.
;;
;; If we find part of a tool call, we begin collecting the pieces in
;; INFO -> :tool-use.
;;
;; Tool call arguments are themselves JSON encoded strings can be spread across
;; chunks.  We collect them in INFO -> :partial_json.  The end of a tool call
;; chunk is marked by the beginning of another, or by the end of the stream.  In
;; either case we flaten the :partial_json we have thus far, add it to the tool
;; call spec in :tool-use and reset it.
;;
;; If we find reasoning text, collect it in INFO -> :reasoning, to be consumed
;; by the stream filter (and eventually the callback).  We also collect it in
;; INFO -> :reasoning-chunks, in case we need to send it back along with tool
;; call results.
;;
;; Finally we append any tool calls and accumulated reasoning text (from
;; :reasoning-chunks) to the (INFO -> :data -> :messages) list of prompts.

(cl-defmethod gptel-curl--parse-stream ((_backend gptel-openai) info)
  "Parse an OpenAI API data stream.

Return the text response accumulated since the last call to this
function.  Additionally, mutate state INFO to add tool-use
information if the stream contains it."
  (let* ((content-strs))
    (condition-case nil
        (while (re-search-forward "^data:" nil t)
          (save-match-data
            (if (looking-at " *\\[DONE\\]")
                ;; The stream has ended, so we do the following thing (if we found tool calls)
                ;; - pack tool calls into the messages prompts list to send (INFO -> :data -> :messages)
                ;; - collect tool calls (formatted differently) into (INFO -> :tool-use)
                ;; - Clear any reasoning content chunks we've captured
                (progn
                  (when-let* ((tool-use (plist-get info :tool-use))
                              (args (apply #'concat (nreverse (plist-get info :partial_json))))
                              (func (plist-get (car tool-use) :function)))
                    (plist-put func :arguments args) ;Update arguments for last recorded tool
                    (gptel--inject-prompt
                     (plist-get info :backend) (plist-get info :data)
                     `( :role "assistant" :content :null :tool_calls ,(vconcat tool-use) ; :refusal :null
                        ;; Return reasoning if available
                        ,@(and-let* ((chunks (nreverse (plist-get info :reasoning-chunks)))
                                     (reasoning-field (pop chunks))) ;chunks is (:reasoning.* "chunk1" "chunk2" ...)
                            (list reasoning-field (apply #'concat chunks)))))
                    (cl-loop
                     for tool-call in tool-use ; Construct the call specs for running the function calls
                     for spec = (plist-get tool-call :function)
                     collect (list :id (plist-get tool-call :id)
                                   :name (plist-get spec :name)
                                   :args (ignore-errors (gptel--json-read-string
                                                         (plist-get spec :arguments))))
                     into call-specs
                     finally (plist-put info :tool-use call-specs)))
                  (when (plist-member info :reasoning-chunks) (plist-put info :reasoning-chunks nil)))
              (when-let* ((response (gptel--json-read))
                          (delta (map-nested-elt response '(:choices 0 :delta))))
                (if-let* ((content (plist-get delta :content))
                          ((not (or (eq content :null) (string-empty-p content)))))
                    (push content content-strs)
                  ;; No text content, so look for tool calls
                  (when-let* ((tool-call (map-nested-elt delta '(:tool_calls 0)))
                              (func (plist-get tool-call :function)))
                    (if (and-let* ((func-name (plist-get func :name)) ((not (eq func-name :null))))
                          ;; TEMP: This check is for litellm compatibility, should be removed
                          (not (equal func-name "null"))) ; new tool block begins
                        (progn
                          (when-let* ((partial (plist-get info :partial_json)))
                            (let* ((prev-tool-call (car (plist-get info :tool-use)))
                                   (prev-func (plist-get prev-tool-call :function)))
                              (plist-put prev-func :arguments ;update args for old tool block
                                         (apply #'concat (nreverse (plist-get info :partial_json)))))
                            (plist-put info :partial_json nil)) ;clear out finished chain of partial args
                          ;; Start new chain of partial argument strings
                          (plist-put info :partial_json (list (plist-get func :arguments)))
                          ;; NOTE: Do NOT use `push' for this, it prepends and we lose the reference
                          (plist-put info :tool-use (cons tool-call (plist-get info :tool-use))))
                      ;; old tool block continues, so continue collecting arguments in :partial_json 
                      (push (plist-get func :arguments) (plist-get info :partial_json)))))
                ;; Check for reasoning blocks, currently only used by Openrouter
                (unless (eq (plist-get info :reasoning-block) 'done)
                  (if-let* ((reasoning-plist ;reasoning-plist is (:reasoning.* "chunk" ...) or nil
                             (or (plist-member delta :reasoning) ;for Openrouter and co
                                 (plist-member delta :reasoning_content))) ;for Deepseek, Llama.cpp
                            (reasoning-chunk (cadr reasoning-plist))
                            ((not (or (eq reasoning-chunk :null) (string-empty-p reasoning-chunk)))))
                      (progn (plist-put info :reasoning ;For stream filter consumption
                                        (concat (plist-get info :reasoning) reasoning-chunk))
                             (plist-put info :reasoning-chunks ;To include with tool call results, if any
                                        (cons reasoning-chunk (or (plist-get info :reasoning-chunks)
                                                                  (list (car reasoning-plist))))))
                    ;; Done with reasoning if we get non-empty content
                    (if-let* (((plist-member info :reasoning)) ;Is this a reasoning model?
                              (c (plist-get delta :content)) ;Started receiving text content?
                              ((not (or (eq c :null) (string-blank-p c)))))
                        (plist-put info :reasoning-block t)))))))) ;Signal end of reasoning block
      (error (goto-char (match-beginning 0))))
    (apply #'concat (nreverse content-strs))))

(cl-defmethod gptel--parse-response ((_backend gptel-openai) response info)
  "Parse an OpenAI (non-streaming) RESPONSE and return response text.

Mutate state INFO with response metadata."
  (let* ((choice0 (map-nested-elt response '(:choices 0)))
         (message (plist-get choice0 :message))
         (content (plist-get message :content)))
    (plist-put info :stop-reason
               (plist-get choice0 :finish_reason))
    (plist-put info :output-tokens
               (map-nested-elt response '(:usage :completion_tokens)))
    ;; OpenAI returns either non-blank text content or a tool call, not both.
    ;; However OpenAI-compatible APIs like llama.cpp can include both (#819), so
    ;; we check for both tool calls and responses independently.
    (when-let* ((tool-calls (plist-get message :tool_calls))
                ((not (eq tool-calls :null))))
      (gptel--inject-prompt        ; First add the tool call to the prompts list
       (plist-get info :backend) (plist-get info :data) message)
      (cl-loop             ;Then capture the tool call data for running the tool
       for tool-call across tool-calls  ;replace ":arguments" with ":args"
       for call-spec = (copy-sequence (plist-get tool-call :function))
       do (ignore-errors (plist-put call-spec :args
                                    (gptel--json-read-string
                                     (plist-get call-spec :arguments))))
       (plist-put call-spec :arguments nil)
       (plist-put call-spec :id (plist-get tool-call :id))
       collect call-spec into tool-use
       finally (plist-put info :tool-use tool-use)))
    (when-let* ((reasoning (or (plist-get message :reasoning) ;for Openrouter and co
                               (plist-get message :reasoning_content))) ;for Deepseek, Llama.cpp
                ((and (stringp reasoning) (not (string-empty-p reasoning)))))
      (plist-put info :reasoning reasoning))
    (when (and content (not (or (eq content :null) (string-empty-p content))))
      content)))

(cl-defmethod gptel--request-data ((backend gptel-openai) prompts)
  "JSON encode PROMPTS for sending to ChatGPT."
  (when gptel--system-message
    (push (list :role "system"
                :content gptel--system-message)
          prompts))
  (let ((prompts-plist
         `(:model ,(gptel--model-name gptel-model)
           :messages [,@prompts]
           :stream ,(or gptel-stream :json-false)))
        (reasoning-model-p ; TODO: Embed this capability in the model's properties
         (memq gptel-model '(o1 o1-preview o1-mini o3-mini o3 o4-mini
                                gpt-5 gpt-5-mini gpt-5-nano gpt-5.1 gpt-5.2))))
    (when (and gptel-temperature (not reasoning-model-p))
      (plist-put prompts-plist :temperature gptel-temperature))
    (when gptel-use-tools
      (when (eq gptel-use-tools 'force)
        (plist-put prompts-plist :tool_choice "required"))
      (when gptel-tools
        (plist-put prompts-plist :tools
                   (gptel--parse-tools backend gptel-tools))
        (unless reasoning-model-p
          (plist-put prompts-plist :parallel_tool_calls t))))
    (when gptel-max-tokens
      ;; HACK: The OpenAI API has deprecated max_tokens, but we still need it
      ;; for OpenAI-compatible APIs like GPT4All (#485)
      (plist-put prompts-plist
                 (if reasoning-model-p :max_completion_tokens :max_tokens)
                 gptel-max-tokens))
    (when gptel--schema
      (plist-put prompts-plist
                 :response_format (gptel--parse-schema backend gptel--schema)))
    ;; Merge request params with model and backend params.
    (gptel--merge-plists
     prompts-plist
     gptel--request-params
     (gptel-backend-request-params gptel-backend)
     (gptel--model-request-params  gptel-model))))

(cl-defmethod gptel--parse-schema ((_backend gptel-openai) schema)
  (list :type "json_schema"
        :json_schema
        (list :name (md5 (format "%s" (random)))
              :schema (gptel--preprocess-schema
                       (gptel--dispatch-schema-type schema))
              :strict t)))

;; NOTE: No `gptel--parse-tools' method required for gptel-openai, since this is
;; handled by its defgeneric implementation

(cl-defmethod gptel--parse-tool-results ((_backend gptel-openai) tool-use)
  "Return a prompt containing tool call results in TOOL-USE."
  ;; (declare (side-effect-free t))
  (mapcar
   (lambda (tool-call)
     (list
      :role "tool"
      :tool_call_id (plist-get tool-call :id)
      :content (plist-get tool-call :result)))
   tool-use))

;; TODO: Remove these functions (#792)
(defun gptel--openai-format-tool-id (tool-id)
  "Format TOOL-ID for OpenAI.

If the ID has the format used by a different backend, use as-is."
  (unless tool-id
    (setq tool-id (substring
                   (md5 (format "%s%s" (random) (float-time)))
                   nil 24)))
  (if (or (string-prefix-p "toolu_" tool-id) ;#747
          (string-prefix-p "call_"  tool-id))
      tool-id
    (format "call_%s" tool-id)))

(defun gptel--openai-unformat-tool-id (tool-id)
  (or (and (string-match "call_\\(.+\\)" tool-id)
           (match-string 1 tool-id))
      tool-id))

;; NOTE: No `gptel--inject-prompt' method required for gptel-openai, since this
;; is handled by its defgeneric implementation

(cl-defmethod gptel--parse-list ((backend gptel-openai) prompt-list)
  (if (consp (car prompt-list))
      (let ((full-prompt))              ; Advanced format, list of lists
        (dolist (entry prompt-list)
          (pcase entry
            (`(prompt . ,msg)
             (push (list :role "user" :content (or (car-safe msg) msg)) full-prompt))
            (`(response . ,msg)
             (push (list :role "assistant" :content (or (car-safe msg) msg)) full-prompt))
            (`(tool . ,call)
             (unless (plist-get call :id)
               (plist-put call :id (gptel--openai-format-tool-id nil)))
             (push
              (list
               :role "assistant"
               :tool_calls
               (vector
                (list :type "function"
                      :id (plist-get call :id)
                      :function `( :name ,(plist-get call :name)
                                   :arguments ,(decode-coding-string
                                                (gptel--json-encode (plist-get call :args))
                                                'utf-8 t)))))
              full-prompt)
             (push (car (gptel--parse-tool-results backend (list (cdr entry)))) full-prompt))))
        (nreverse full-prompt))
    (cl-loop for text in prompt-list    ; Simple format, list of strings
             for role = t then (not role)
             if text collect
             (list :role (if role "user" "assistant") :content text))))

(cl-defmethod gptel--parse-buffer ((backend gptel-openai) &optional max-entries)
  (let ((prompts) (prev-pt (point)))
    (if (or gptel-mode gptel-track-response)
        (while (and (or (not max-entries) (>= max-entries 0))
                    (/= prev-pt (point-min))
                    (goto-char (previous-single-property-change
                                (point) 'gptel nil (point-min))))
          (pcase (get-char-property (point) 'gptel)
            ('response
             (when-let* ((content (gptel--trim-prefixes
                                   (buffer-substring-no-properties (point) prev-pt))))
               (push (list :role "assistant" :content content) prompts)))
            (`(tool . ,id)
             (save-excursion
               (condition-case nil
                   (let* ((tool-call (read (current-buffer)))
                          (name (plist-get tool-call :name))
                          (arguments (decode-coding-string
                                      (gptel--json-encode (plist-get tool-call :args))
                                      'utf-8 t)))
                     (setq id (gptel--openai-format-tool-id id))
                     (plist-put tool-call :id id)
                     (plist-put tool-call :result
                                (string-trim (buffer-substring-no-properties
                                              (point) prev-pt)))
                     (push (car (gptel--parse-tool-results backend (list tool-call)))
                           prompts)
                     (push (list :role "assistant"
                                 :tool_calls
                                 (vector (list :type "function"
                                               :id id
                                               :function `( :name ,name
                                                            :arguments ,arguments))))
                           prompts))
                 ((end-of-file invalid-read-syntax)
                  (message (format "Could not parse tool-call %s on line %s"
                                   id (line-number-at-pos (point))))))))
            ('ignore)
            ('nil
             (and max-entries (cl-decf max-entries))
             (if gptel-track-media
                 (when-let* ((content (gptel--openai-parse-multipart
                                       (gptel--parse-media-links major-mode
                                                                 (point) prev-pt))))
                   (when (> (length content) 0)
                     (push (list :role "user" :content content) prompts)))
               (when-let* ((content (gptel--trim-prefixes (buffer-substring-no-properties
                                                           (point) prev-pt))))
                 (push (list :role "user" :content content) prompts)))))
          (setq prev-pt (point)))
      (let ((content (string-trim (buffer-substring-no-properties
                                    (point-min) (point-max)))))
        (push (list :role "user" :content content) prompts)))
    prompts))

;; TODO This could be a generic function
(defun gptel--openai-parse-multipart (parts)
  "Convert a multipart prompt PARTS to the OpenAI API format.

The input is an alist of the form
 ((:text \"some text\")
  (:media \"/path/to/media.png\" :mime \"image/png\")
  (:text \"More text\")).

The output is a vector of entries in a backend-appropriate
format."
  (cl-loop
   for part in parts
   for n upfrom 1
   with last = (length parts)
   for text = (plist-get part :text)
   for media = (plist-get part :media)
   if text do
   (and (or (= n 1) (= n last)) (setq text (gptel--trim-prefixes text)))
   and if text
   collect `(:type "text" :text ,text) into parts-array end
   else if media collect
   `(:type "image_url"
     :image_url (:url ,(concat "data:" (plist-get part :mime)
                        ";base64," (gptel--base64-encode media))))
   into parts-array
   else if (plist-get part :textfile) collect
   `(:type "text"
     :text ,(with-temp-buffer
              (gptel--insert-file-string (plist-get part :textfile))
              (buffer-string)))
   into parts-array end and
   if (plist-get part :url)
   collect
   `(:type "image_url"
     :image_url (:url ,(plist-get part :url)))
   into parts-array
   finally return (vconcat parts-array)))

(cl-defmethod gptel--inject-media ((_backend gptel-openai) prompts)
  "Wrap the first user prompt in PROMPTS with included media files.

Media files, if present, are placed in `gptel-context'."
  (when-let* ((media-list (gptel-context--collect-media)))
    (cl-callf (lambda (current)
                (vconcat
                 (gptel--openai-parse-multipart media-list)
                 (cl-typecase current
                   (string `((:type "text" :text ,current)))
                   (vector current)
                   (t current))))
        (plist-get (car prompts) :content))))

;;;###autoload
(cl-defun gptel-make-openai
    (name &key curl-args models stream key request-params
          (header
           (lambda () (when-let* ((key (gptel--get-api-key)))
                   `(("Authorization" . ,(concat "Bearer " key))))))
          (host "api.openai.com")
          (protocol "https")
          (endpoint "/v1/chat/completions"))
  "Register an OpenAI API-compatible backend for gptel with NAME.

Keyword arguments:

CURL-ARGS (optional) is a list of additional Curl arguments.

HOST (optional) is the API host, typically \"api.openai.com\".

MODELS is a list of available model names, as symbols.
Additionally, you can specify supported LLM capabilities like
vision or tool-use by appending a plist to the model with more
information, in the form

 (model-name . plist)

For a list of currently recognized plist keys, see
`gptel--openai-models'.  An example of a model specification
including both kinds of specs:

:models
\\='(gpt-3.5-turbo                         ;Simple specs
  gpt-4-turbo
  (gpt-4o-mini                          ;Full spec
   :description
   \"Affordable and intelligent small model for lightweight tasks\"
   :capabilities (media tool json url)
   :mime-types
   (\"image/jpeg\" \"image/png\" \"image/gif\" \"image/webp\")))

STREAM is a boolean to toggle streaming responses, defaults to
false.

PROTOCOL (optional) specifies the protocol, https by default.

ENDPOINT (optional) is the API endpoint for completions, defaults to
\"/v1/chat/completions\".

HEADER (optional) is for additional headers to send with each
request.  It should be an alist or a function that returns an
alist, like:
 ((\"Content-Type\" . \"application/json\"))

KEY (optional) is a variable whose value is the API key, or
function that returns the key.

REQUEST-PARAMS (optional) is a plist of additional HTTP request
parameters (as plist keys) and values supported by the API.  Use
these to set parameters that gptel does not provide user options
for."
  (declare (indent 1))
  (let ((backend (gptel--make-openai
                  :curl-args curl-args
                  :name name
                  :host host
                  :header header
                  :key key
                  :models (gptel--process-models models)
                  :protocol protocol
                  :endpoint endpoint
                  :stream stream
                  :request-params request-params
                  :url (if protocol
                           (concat protocol "://" host endpoint)
                         (concat host endpoint)))))
    (prog1 backend
      (setf (alist-get name gptel--known-backends
                       nil nil #'equal)
                  backend))))

;;; Azure
;;;###autoload
(cl-defun gptel-make-azure
    (name &key curl-args host
          (protocol "https")
          (header (lambda () `(("api-key" . ,(gptel--get-api-key)))))
          (key 'gptel-api-key)
          models stream endpoint request-params)
  "Register an Azure backend for gptel with NAME.

Keyword arguments:

CURL-ARGS (optional) is a list of additional Curl arguments.

HOST is the API host.

MODELS is a list of available model names, as symbols.

STREAM is a boolean to toggle streaming responses, defaults to
false.

PROTOCOL (optional) specifies the protocol, https by default.

ENDPOINT is the API endpoint for completions.

HEADER (optional) is for additional headers to send with each
request.  It should be an alist or a function that retuns an
alist, like:
 ((\"Content-Type\" . \"application/json\"))

KEY (optional) is a variable whose value is the API key, or
function that returns the key.

REQUEST-PARAMS (optional) is a plist of additional HTTP request
parameters (as plist keys) and values supported by the API.  Use
these to set parameters that gptel does not provide user options
for.

Example:
-------

 (gptel-make-azure
  \"Azure-1\"
  :protocol \"https\"
  :host \"RESOURCE_NAME.openai.azure.com\"
  :endpoint
  \"/openai/deployments/DEPLOYMENT_NAME/completions?api-version=2023-05-15\"
  :stream t
  :models \\='(gpt-3.5-turbo gpt-4))"
  (declare (indent 1))
  (let ((backend (gptel--make-openai
                  :curl-args curl-args
                  :name name
                  :host host
                  :header header
                  :key key
                  :models (gptel--process-models models)
                  :protocol protocol
                  :endpoint endpoint
                  :stream stream
                  :request-params request-params
                  :url (if protocol
                           (concat protocol "://" host endpoint)
                         (concat host endpoint)))))
    (prog1 backend
      (setf (alist-get name gptel--known-backends
                       nil nil #'equal)
            backend))))

;; GPT4All
;;;###autoload
(defalias 'gptel-make-gpt4all 'gptel-make-openai
  "Register a GPT4All backend for gptel with NAME.

Keyword arguments:

CURL-ARGS (optional) is a list of additional Curl arguments.

HOST is where GPT4All runs (with port), typically localhost:4891

MODELS is a list of available model names, as symbols.

STREAM is a boolean to toggle streaming responses, defaults to
false.

PROTOCOL specifies the protocol, https by default.

ENDPOINT (optional) is the API endpoint for completions, defaults to
\"/api/v1/completions\"

HEADER (optional) is for additional headers to send with each
request. It should be an alist or a function that retuns an
alist, like:
((\"Content-Type\" . \"application/json\"))

KEY (optional) is a variable whose value is the API key, or
function that returns the key. This is typically not required for
local models like GPT4All.

REQUEST-PARAMS (optional) is a plist of additional HTTP request
parameters (as plist keys) and values supported by the API.  Use
these to set parameters that gptel does not provide user options
for.

Example:
-------

(gptel-make-gpt4all
 \"GPT4All\"
 :protocol \"http\"
 :host \"localhost:4891\"
 :models \\='(mistral-7b-openorca.Q4_0.gguf))")

(provide 'gptel-openai)
;;; gptel-openai.el ends here

;; Local Variables:
;; byte-compile-warnings: (not docstrings)
;; End:
