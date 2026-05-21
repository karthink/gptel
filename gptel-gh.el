;;; gptel-gh.el ---  Github Copilot AI suppport for gptel  -*- lexical-binding: t; -*-

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

;; This file adds support for Github Copilot API to gptel

;;; Code:

(require 'cl-lib)
(require 'map)
(eval-and-compile
  (require 'gptel-request)
  (require 'gptel-oauth)
  (require 'gptel-openai)
  (require 'gptel-openai-responses))

;;; Github Copilot
(defconst gptel--gh-models
  '((gpt-4.1
     :description "Flagship model for complex tasks"
     :capabilities (media tool-use json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 111
     :input-cost 0
     :output-cost 0
     :cutoff-date "2024-04")
    (gpt-4o
     :description
     "Advanced model for complex tasks; cheaper & faster than GPT-Turbo"
     :capabilities (media tool-use json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 64
     :input-cost 0
     :output-cost 0
     :cutoff-date "2023-09")
    (gpt-5-mini
     :description "Faster, more cost-efficient version of GPT-5"
     :capabilities (media tool-use json url responses-api)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 128
     :input-cost 0
     :output-cost 0
     :cutoff-date "2024-06")
    (gpt-5.1
     :description "The best model for coding and agentic tasks"
     :capabilities (media tool-use json url responses-api)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 128
     :input-cost 1
     :output-cost 1
     :cutoff-date "2024-09")
    (gpt-5.1-codex
     :description "Flagship model for coding, reasoning, and agentic tasks across domains"
     :capabilities (media tool-use json url responses-api)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 128
     :input-cost 1
     :output-cost 1
     :cutoff-date "2024-09")
    (gpt-5.1-codex-max
     :description "Flagship model for coding, reasoning, and agentic tasks across domains"
     :capabilities (media tool-use json url responses-api)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 128
     :input-cost 1
     :output-cost 1
     :cutoff-date "2024-09")
    (gpt-5.1-codex-mini
     :description "Flagship model for coding, reasoning, and agentic tasks across domains"
     :capabilities (media tool-use json url responses-api)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 128
     :input-cost 1
     :output-cost 1
     :cutoff-date "2024-09")
    (gpt-5.2
     :description "The best model for coding and agentic tasks"
     :capabilities (media tool-use json url responses-api)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 128
     :input-cost 1
     :output-cost 1
     :cutoff-date "2025-08")
    (gpt-5.2-codex
     :description "The best model for coding and agentic tasks"
     :capabilities (media tool-use json url responses-api)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 272
     :input-cost 1
     :output-cost 1
     :cutoff-date "2025-08")
    (gpt-5.3-codex
     :description "The most capable agentic coding model to date"
     :capabilities (media tool-use json url responses-api)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 400
     :input-cost 1
     :output-cost 1
     :cutoff-date "2025-08")
    (gpt-5.4
     :description "Best intelligence at scale for agentic, coding, and professional workflows"
     :capabilities (media tool-use json url responses-api)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 400
     :input-cost 1
     :output-cost 1
     :cutoff-date "2025-08")
    (gpt-5.4-mini
     :description "Strongest mini model yet for coding, computer use, and subagent"
     :capabilities (media tool-use json url responses-api)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 400
     :input-cost 0.33
     :output-cost 0.33
     :cutoff-date "2025-08")
    (gpt-5.5
     :description "GitHub Copilot GPT-5.5"
     :capabilities (media tool-use json url responses-api)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 400
     :input-cost 1
     :output-cost 1
     :cutoff-date "2026-04")
    (claude-haiku-4.5
     :description "Near-frontier intelligence at blazing speeds with extended thinking"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 128
     :input-cost 0.33
     :output-cost 0.33
     :cutoff-date "2025-02")
    (claude-opus-4.5
     :description "Most capable model for complex reasoning and advanced coding"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 128
     :input-cost 3
     :output-cost 3
     :cutoff-date "2025-03")
    (claude-opus-4.6
     :description "Most capable model for complex reasoning and advanced coding"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 128
     :input-cost 3
     :output-cost 3
     :cutoff-date "2025-03")
    (claude-opus-4.7
     :description "Most capable model for complex reasoning and advanced coding"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 128
     :input-cost 3
     :output-cost 3
     :cutoff-date "2025-03")
    (claude-sonnet-4
     :description "High-performance model with exceptional reasoning and efficiency"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 128
     :input-cost 1
     :output-cost 1
     :cutoff-date "2025-03")
    (claude-sonnet-4.5
     :description "High-performance model with exceptional reasoning and efficiency"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 128
     :input-cost 1
     :output-cost 1
     :cutoff-date "2025-03")
    (claude-sonnet-4.6
     :description "High-performance model with exceptional reasoning and efficiency"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 128
     :input-cost 1
     :output-cost 1
     :cutoff-date "2025-03")
    (gemini-2.5-pro
     :description "Next gen, high speed, multimodal for a diverse variety of tasks"
     :capabilities (tool-use json media)
     :mime-types ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
                  "application/pdf" "text/plain" "text/csv" "text/html")
     :context-window 109
     :input-cost 1
     :output-cost 1
     :cutoff-date "2025-01")
    (gemini-3-flash-preview
     :description "Most intelligent Gemini model built for speed"
     :capabilities (tool-use json media audio video)
     :mime-types ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
                  "application/pdf" "text/plain" "text/csv" "text/html"
                  "audio/mpeg" "audio/wav" "audio/ogg" "audio/flac" "audio/aac" "audio/mp3"
                  "video/mp4" "video/mpeg" "video/avi" "video/quicktime" "video/webm")
     :context-window 109
     :input-cost 0.33
     :output-cost 0.33
     :cutoff-date "2025-01")
    (gemini-3-pro-preview
     :description "Most intelligent Gemini model with SOTA reasoning and multimodal understanding"
     :capabilities (tool-use json media audio video)
     :mime-types ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
                  "application/pdf" "text/plain" "text/csv" "text/html"
                  "audio/mpeg" "audio/wav" "audio/ogg" "audio/flac" "audio/aac" "audio/mp3"
                  "video/mp4" "video/mpeg" "video/avi" "video/quicktime" "video/webm")
     :context-window 109
     :input-cost 1
     :output-cost 1
     :cutoff-date "2025-01")
    (gemini-3.1-pro-preview
     :description "Most intelligent Gemini model with SOTA reasoning and multimodal understanding"
     :capabilities (tool-use json media audio video)
     :mime-types ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
                  "application/pdf" "text/plain" "text/csv" "text/html"
                  "audio/mpeg" "audio/wav" "audio/ogg" "audio/flac" "audio/aac" "audio/mp3"
                  "video/mp4" "video/mpeg" "video/avi" "video/quicktime" "video/webm")
     :context-window 109
     :input-cost 1
     :output-cost 1
     :cutoff-date "2025-01")
    (grok-code-fast-1
     :description "Fast reasoning model for agentic coding"
     :capabilities '(tool-use json reasoning)
     :context-window 109
     :input-cost 0.25
     :output-cost 1.5
     :cutoff-date "2025-08")))

(cl-defstruct (gptel--gh (:include gptel-openai)
                         (:copier nil)
                         (:constructor gptel--make-gh))
  token github-token sessionid machineid responses-backend)

(defcustom gptel-gh-github-token-file (expand-file-name ".cache/copilot-chat/github-token"
                                                        user-emacs-directory)
  "File where the GitHub token is stored."
  :type 'string
  :group 'gptel)

(defcustom gptel-gh-token-file (expand-file-name ".cache/copilot-chat/token"
                                                 user-emacs-directory)
  "File where the chat token is cached."
  :type 'string
  :group 'gptel)

(defconst gptel--gh-auth-common-headers
  `(("editor-plugin-version" . "gptel/*")
    ("editor-version" . ,(concat "emacs/" emacs-version))))

(defconst gptel--gh-client-id "Iv1.b507a08c87ecfe98")

;; https://en.wikipedia.org/wiki/Universally_unique_identifier#Version_4_(random)
(defun gptel--gh-uuid ()
  "Generate a UUID v4-1."
  (format "%04x%04x-%04x-4%03x-8%03x-%04x%04x%04x"
          (random #x10000) (random #x10000)
          (random #x10000)
          (random #x1000)
          (random #x1000)
          (random #x10000) (random #x10000) (random #x10000)))

(defun gptel--gh-machine-id ()
  "Generate a machine ID."
  (let ((hex-chars "0123456789abcdef")
        (length 65)
        hex)
    (dotimes (_ length)
      (setq hex (nconc hex (list (aref hex-chars (random 16))))))
    (apply #'string hex)))

(defun gptel-gh-login ()
  "Login to GitHub Copilot API.

This will prompt you to authorize in a browser and store the token.

In SSH sessions, the URL and code will be displayed for manual entry
instead of attempting to open a browser automatically."
  (interactive)
  ;; Determine which GitHub backend to use
  (let ((gh-backend
         (cond
          ;; If current backend is GitHub, use it
          ((and (boundp 'gptel-backend)
                gptel-backend
                (gptel--gh-p gptel-backend))
           gptel-backend)
          ;; Otherwise, find any GitHub backend
          ((cl-find-if (lambda (b) (gptel--gh-p b))
                       (mapcar #'cdr gptel--known-backends)))
          ;; No GitHub backend found
          (t (user-error "No GitHub Copilot backend found.  \
Please set one up with `gptel-make-gh-copilot' first")))))
    (pcase-let (((map :device_code :user_code :verification_uri)
                 (gptel--url-retrieve
                     "https://github.com/login/device/code"
                   :method 'post
                   :headers gptel--gh-auth-common-headers
                   :data `( :client_id ,gptel--gh-client-id
                            :scope "read:user"))))
      (gptel-oauth--device-auth-prompt user_code verification_uri)
      ;; Use gh-backend for token storage
      (let ((resp-body (gptel--url-retrieve
                           "https://github.com/login/oauth/access_token"
                         :method 'post
                         :headers gptel--gh-auth-common-headers
                         :data `( :client_id ,gptel--gh-client-id
                                  :device_code ,device_code
                                  :grant_type "urn:ietf:params:oauth:grant-type:device_code"))))
        (thread-last
            (plist-get resp-body :access_token)
          (gptel-oauth--write-token gptel-gh-github-token-file)
          (setf (gptel--gh-github-token gh-backend)))))
    ;; Check gh-backend for success
    (if (and (gptel--gh-github-token gh-backend)
             (not (string-empty-p
                   (gptel--gh-github-token gh-backend))))
        (message "Successfully logged in to GitHub Copilot.")
      (user-error "Error: You might not have access to GitHub Copilot Chat!"))))

(defun gptel--gh-renew-token ()
  "Renew session token."
  (let ((token
         (gptel--url-retrieve
             "https://api.github.com/copilot_internal/v2/token"
           :method 'get
           :headers `(("authorization"
                       . ,(format "token %s" (gptel--gh-github-token gptel-backend)))
                      ,@gptel--gh-auth-common-headers))))
    (if (not (plist-get token :token))
        (progn
          (setf (gptel--gh-github-token gptel-backend) nil)
          (user-error "Error: You might not have access to GitHub Copilot Chat!"))
      (thread-last token
        (gptel-oauth--write-token gptel-gh-token-file)
        (setf (gptel--gh-token gptel-backend))))))

(defun gptel--gh-auth ()
  "Authenticate with GitHub Copilot API.

We first need github authorization (github token).
Then we need a session token."
  (unless (gptel--gh-github-token gptel-backend)
    (let ((token (gptel-oauth--read-token gptel-gh-github-token-file)))
      (if token
          (setf (gptel--gh-github-token gptel-backend) token)
        (gptel-gh-login))))

  (when (null (gptel--gh-token gptel-backend))
    ;; try to load token from `gptel-gh-token-file'
    (setf (gptel--gh-token gptel-backend)
          (gptel-oauth--read-token gptel-gh-token-file)))

  (pcase-let (((map :token :expires_at)
               (gptel--gh-token gptel-backend)))
    (when (or (null token)
              (and expires_at
                   (> (round (float-time (current-time)))
                      expires_at)))
      (gptel--gh-renew-token))))

(cl-defmethod gptel-curl--parse-stream ((backend gptel--gh) info)
  (let ((model (plist-get info :model)))
   (if (gptel--model-capable-p 'responses-api model)
       ;; Defer to gptel-openai-responses backend
       (gptel-curl--parse-stream
        (gptel--gh-responses-backend backend) info)
     (cl-call-next-method))))

(cl-defmethod gptel--parse-response ((backend gptel--gh) response info)
  (let ((model (plist-get info :model)))
    (if (gptel--model-capable-p 'responses-api model)
       ;; Defer to gptel-openai-responses backend
       (gptel--parse-response
        (gptel--gh-responses-backend backend) response info)
     (cl-call-next-method))))

(cl-defmethod gptel--request-data ((backend gptel--gh) prompts)
  (if (gptel--model-capable-p 'responses-api gptel-model)
      (gptel--request-data (gptel--gh-responses-backend backend) prompts)
    (cl-call-next-method)))

(cl-defmethod gptel--parse-schema ((backend gptel--gh) schema)
  (if (gptel--model-capable-p 'responses-api gptel-model)
      (gptel--parse-schema (gptel--gh-responses-backend backend) schema)
    (cl-call-next-method)))

(cl-defmethod gptel--parse-tools ((backend gptel--gh) tools)
  (if (gptel--model-capable-p 'responses-api gptel-model)
      (gptel--parse-tools (gptel--gh-responses-backend backend) tools)
    (cl-call-next-method)))

(cl-defmethod gptel--inject-tool-call ((backend gptel--gh) data tool-call new-call)
  (if (gptel--model-capable-p 'responses-api gptel-model)
      (gptel--inject-tool-call (gptel--gh-responses-backend backend) data tool-call new-call)
    (cl-call-next-method)))

(cl-defmethod gptel--parse-tool-results ((backend gptel--gh) tool-use)
  (if (gptel--model-capable-p 'responses-api gptel-model)
      (gptel--parse-tool-results (gptel--gh-responses-backend backend) tool-use)
    (cl-call-next-method)))

(cl-defmethod gptel--inject-prompt ((backend gptel--gh) data new-prompt &optional position)
  (if (gptel--model-capable-p 'responses-api gptel-model)
      (gptel--inject-prompt (gptel--gh-responses-backend backend) data new-prompt position)
    (cl-call-next-method)))

(cl-defmethod gptel--parse-list ((backend gptel--gh) prompt-list)
  (if (gptel--model-capable-p 'responses-api gptel-model)
      (gptel--parse-list (gptel--gh-responses-backend backend) prompt-list)
    (cl-call-next-method)))

(cl-defmethod gptel--parse-buffer ((backend gptel--gh) &optional max-entries)
  (if (gptel--model-capable-p 'responses-api gptel-model)
      (gptel--parse-buffer (gptel--gh-responses-backend backend) max-entries)
    (cl-call-next-method)))

(cl-defmethod gptel--inject-media ((backend gptel--gh) prompts)
  (if (gptel--model-capable-p 'responses-api gptel-model)
      (gptel--inject-media (gptel--gh-responses-backend backend) prompts)
    (cl-call-next-method)))

;;;###autoload
(cl-defun gptel-make-gh-copilot
    (name &key curl-args request-params
          (header
           (lambda (info) (gptel--gh-auth)
             `(("openai-intent" . "conversation-panel")
               ("authorization" . ,( concat "Bearer "
                                     (plist-get (gptel--gh-token gptel-backend) :token)))
               ("x-initiator"  . ,(or (plist-get info :gh-initiator) ;tool call return turn
                                      (prog1 "user"                  ;user turn
                                        (plist-put info :gh-initiator "agent"))))
               ("x-request-id" . ,(gptel--gh-uuid))
               ("vscode-sessionid" . ,(or (gptel--gh-sessionid gptel-backend) ""))
               ("vscode-machineid" . ,(or (gptel--gh-machineid gptel-backend) ""))
               ,@(when (and gptel-track-media
                            (gptel--model-capable-p 'media))
                   `(("copilot-vision-request" . "true")))
               ("copilot-integration-id" . "vscode-chat"))))
          (host "api.githubcopilot.com")
          (protocol "https")
          (endpoint "/chat/completions")
          (stream t)
          (models gptel--gh-models))
  "Register a Github Copilot chat backend for gptel with NAME.

Keyword arguments:

CURL-ARGS (optional) is a list of additional Curl arguments.

HOST (optional) is the API host, typically \"api.githubcopilot.com\".

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

Defaults to a list of models supported by GitHub Copilot.

STREAM is a boolean to toggle streaming responses, defaults to
false.

PROTOCOL (optional) specifies the protocol, https by default.

ENDPOINT (optional) is the API endpoint for completions, defaults to
\"/chat/completions\".

HEADER (optional) is for additional headers to send with each
request.  It should be an alist or a function that returns an
alist, like:
 ((\"Content-Type\" . \"application/json\"))

Defaults to headers required by GitHub Copilot.

REQUEST-PARAMS (optional) is a plist of additional HTTP request
parameters (as plist keys) and values supported by the API.  Use
these to set parameters that gptel does not provide user options
for."
  (declare (indent 1))
  (let* ((url (lambda (_info)
                (concat protocol "://" host
                        (if (gptel--model-capable-p 'responses-api gptel-model)
                            "/v1/responses" endpoint))))
         (backend (gptel--make-gh
                   :name name
                   :host host
                   :header header
                   :models (gptel--process-models models)
                   :protocol protocol
                   :endpoint endpoint
                   :stream stream
                   :request-params request-params
                   :curl-args curl-args
                   :url url
                   :machineid (gptel--gh-machine-id)
                   :responses-backend
                   (gptel--make-openai-responses
                    :name name
                    :host host
                    :header header
                    :protocol protocol
                    :endpoint "/v1/responses"
                    :stream stream
                    :request-params request-params
                    :curl-args curl-args
                    :url url))))
    (setf (alist-get name gptel--known-backends nil nil #'equal) backend)
    backend))

(provide 'gptel-gh)
;;; gptel-gh.el ends here

;; Local Variables:
;; byte-compile-warnings: (not docstrings)
;; End:
