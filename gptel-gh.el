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

(eval-when-compile
  (require 'cl-lib))
(require 'map)
(require 'gptel)
(require 'browse-url)

;;; Github Copilot
(defconst gptel--gh-models
  '((gpt-4o-2024-11-20
     :description
     "Advanced model for complex tasks; cheaper & faster than GPT-Turbo"
     :capabilities (media tool-use json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 128 :input-cost 2.5 :output-cost 10 :cutoff-date "2023-10")
    (gpt-4.5-preview
     :description "Largest and most capable GPT model to date"
     :capabilities (tool-use url)
     :context-window 128
     :input-cost 75
     :output-cost 150
     :cutoff-date "2023-10")
    (o1
     :description "Reasoning model designed to solve hard problems across domains"
     :capabilities (reasoning)
     :context-window 200
     :input-cost 15
     :output-cost 60
     :cutoff-date "2023-10"
     :request-params (:stream :json-false))
    (o3-mini
     :description "High intelligence at the same cost and latency targets of o1-mini"
     :context-window 200
     :input-cost 3
     :output-cost 12
     :cutoff-date "2023-10"
     :capabilities (reasoning)
     :request-params (:stream :json-false))
    (claude-3.5-sonnet
     :description "Highest level of intelligence and capability"
     :capabilities (tool-use cache)
     :context-window 200
     :input-cost 3
     :output-cost 15
     :cutoff-date "2024-04")
    (claude-3.7-sonnet
     :description "Hybrid model capable of standard thinking and extended thinking modes"
     :capabilities (tool-use cache)
     :context-window 200
     :input-cost 3
     :output-cost 15
     :cutoff-date "2025-02")
    (claude-3.7-sonnet-thought
     :description "Claude 3.7 Sonnet Thinking"
     :capabilities (tool-use cache)
     :context-window 200
     :input-cost 3
     :output-cost 15
     :cutoff-date "2025-02")
    (gemini-2.0-flash-001
     :description "Next gen, high speed, multimodal for a diverse variety of tasks"
     :capabilities (tool-use json)
     :context-window 1000
     :input-cost 0.10
     :output-cost 0.40
     :cutoff-date "2024-08")
    (gemini-2.5-pro
     :description "Next gen, high speed, multimodal for a diverse variety of tasks"
     :capabilities (tool-use json)
     :context-window 1000
     :input-cost 0.10
     :output-cost 0.40
     :cutoff-date "2024-08")))

(cl-defstruct (gptel--gh (:include gptel-openai)
                         (:copier nil)
                         (:constructor gptel--make-gh))
  token github-token sessionid machineid)

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

(defun gptel--gh-restore (file)
  "Restore saved object from FILE."
  (when (file-exists-p file)
    ;; We set the coding system to `utf-8-auto-dos' when reading so that
    ;; files with CR EOL can still be read properly
    (let ((coding-system-for-read 'utf-8-auto-dos))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally file)
        (goto-char (point-min))
        (read (current-buffer))))))

(defun gptel--gh-save (file obj)
  "Save OBJ to FILE."
  (let ((print-length nil)
        (print-level nil)
        (coding-system-for-write 'utf-8-unix))
    (make-directory (file-name-directory file) t)
    (write-region (prin1-to-string obj) nil file nil :silent)
    obj))

(defun gptel--gh-login()
  "Manage github login."
  (pcase-let (((map :device_code :user_code :verification_uri)
               (gptel--url-retrieve
                "https://github.com/login/device/code"
                :method 'post
                :headers gptel--gh-auth-common-headers
                :data `( :client_id ,gptel--gh-client-id
                         :scope "read:user"))))
    (gui-set-selection 'CLIPBOARD user_code)
    (read-from-minibuffer
     (format "Your one-time code %s is copied. \
Press ENTER to open GitHub in your browser. \
If your browser does not open automatically, browse to %s."
             user_code verification_uri))
    (browse-url verification_uri)
    (read-from-minibuffer "Press ENTER after authorizing.")
    (thread-last
      (plist-get
       (gptel--url-retrieve
        "https://github.com/login/oauth/access_token"
        :method 'post
        :headers gptel--gh-auth-common-headers
        :data `( :client_id ,gptel--gh-client-id
                 :device_code ,device_code
                 :grant_type "urn:ietf:params:oauth:grant-type:device_code"))
       :access_token)
      (gptel--gh-save gptel-gh-github-token-file)
      (setf (gptel--gh-github-token gptel-backend)))))

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
          (user-error "Error: You might not have access to Github Copilot Chat!"))
      (thread-last
        (gptel--gh-save gptel-gh-token-file token)
        (setf (gptel--gh-token gptel-backend))))))

(defun gptel--gh-auth ()
  "Authenticate with GitHub Copilot API.

We first need github authorization (github token).
Then we need a session token."
  (unless (gptel--gh-github-token gptel-backend)
    (let ((token (gptel--gh-restore gptel-gh-github-token-file)))
      (if token
          (setf (gptel--gh-github-token gptel-backend) token)
        (gptel--gh-login))))

  (when (null (gptel--gh-token gptel-backend))
    ;; try to load token from `gptel-gh-token-file'
    (setf (gptel--gh-token gptel-backend)
          (gptel--gh-restore gptel-gh-token-file)))

  (pcase-let (((map :token :expires_at)
               (gptel--gh-token gptel-backend)))
    (when (or (null token)
              (and expires_at
                   (> (round (float-time (current-time)))
                      expires_at)))
      (gptel--gh-renew-token))))

;;;###autoload
(cl-defun gptel-make-gh-copilot
    (name &key curl-args request-params
          (header (lambda ()
                    (gptel--gh-auth)
                    `(("openai-intent" . "conversation-panel")
                      ("authorization" . ,(concat "Bearer "
                                           (plist-get (gptel--gh-token gptel-backend) :token)))
                      ("x-request-id" . ,(gptel--gh-uuid))
                      ("vscode-sessionid" . ,(or (gptel--gh-sessionid gptel-backend) ""))
                      ("vscode-machineid" . ,(or (gptel--gh-machineid gptel-backend) ""))
                      ("copilot-vision-request" . "true")
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
  (let ((backend (gptel--make-gh
                  :name name
                  :host host
                  :header header
                  :models (gptel--process-models models)
                  :protocol protocol
                  :endpoint endpoint
                  :stream stream
                  :request-params request-params
                  :curl-args curl-args
                  :url (concat protocol "://" host endpoint)
                  :machineid (gptel--gh-machine-id))))
    (setf (alist-get name gptel--known-backends nil nil #'equal) backend)
    backend))

(provide 'gptel-gh)
;;; gptel-gh.el ends here
