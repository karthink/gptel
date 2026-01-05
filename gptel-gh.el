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
  '((gpt-4o
     :description
     "Advanced model for complex tasks; cheaper & faster than GPT-Turbo"
     :capabilities (media tool-use json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 128 :input-cost 0 :output-cost 0 :cutoff-date "2023-10")
    (gpt-4.1
     :description "Flagship model for complex tasks"
     :capabilities (media tool-use json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 128
     :input-cost 0
     :output-cost 0
     :cutoff-date "2024-05")
    (gpt-41-copilot
     :description "Flagship model for complex tasks"
     :capabilities (media tool-use json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 200
     :input-cost 0
     :output-cost 0
     :cutoff-date "2024-05")
    (gpt-5
     :description "Flagship model for coding, reasoning, and agentic tasks across domains"
     :capabilities (media tool-use json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 264
     :input-cost 1
     :output-cost 1
     :cutoff-date "2024-09")
    (gpt-5.1-codex-max
     :description "Flagship model for coding, reasoning, and agentic tasks across domains"
     :capabilities (media tool-use json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 400
     :input-cost 1
     :output-cost 1
     :cutoff-date "2024-09")
    (gpt-5-mini
     :description "Faster, more cost-efficient version of GPT-5"
     :capabilities (media tool-use json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 264
     :input-cost 0
     :output-cost 0
     :cutoff-date "2024-09")
    (gpt-5.1
     :description "The best model for coding and agentic tasks"
     :capabilities (media tool-use json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 400
     :input-cost 1
     :output-cost 1
     :cutoff-date "2024-09")
    (gpt-5.2
     :description "The best model for coding and agentic tasks"
     :capabilities (media tool-use json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 400
     :input-cost 1
     :output-cost 1
     :cutoff-date "2025-08")
    (gpt-5.2-codex
     :description "The best model for coding and agentic tasks"
     :capabilities (media tool-use json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 400
     :input-cost 1
     :output-cost 1
     :cutoff-date "2025-08")
    (claude-sonnet-4
     :description "High-performance model with exceptional reasoning and efficiency"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 216
     :input-cost 1
     :output-cost 1
     :cutoff-date "2025-03")
    (claude-sonnet-4.5
     :description "High-performance model with exceptional reasoning and efficiency"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 144
     :input-cost 1
     :output-cost 1
     :cutoff-date "2025-07")
    (claude-haiku-4.5
     :description "Near-frontier intelligence at blazing speeds with extended thinking"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 144
     :input-cost 0.33
     :output-cost 0.33
     :cutoff-date "2025-02")
    (claude-opus-41
     :description "Most capable model for complex reasoning and advanced coding"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 80
     :input-cost 10
     :output-cost 10
     :cutoff-date "2025-03")
    (claude-opus-4.5
     :description "Most capable model for complex reasoning and advanced coding"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 144
     :input-cost 3
     :output-cost 3
     :cutoff-date "2025-05")
    (claude-opus-4.6
     :description "Most capable model for complex reasoning and advanced coding"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 200
     :input-cost 3
     :output-cost 3
     :cutoff-date "2025-08")
    (gemini-2.5-pro
     :description "Next gen, high speed, multimodal for a diverse variety of tasks"
     :capabilities (tool-use json media)
     :mime-types ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
                  "application/pdf" "text/plain" "text/csv" "text/html")
     :context-window 128
     :input-cost 1
     :output-cost 1
     :cutoff-date "2024-08")
    (gemini-3-flash-preview
     :description "Most intelligent Gemini model built for speed"
     :capabilities (tool-use json media audio video)
     :mime-types ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
                  "application/pdf" "text/plain" "text/csv" "text/html"
                  "audio/mpeg" "audio/wav" "audio/ogg" "audio/flac" "audio/aac" "audio/mp3"
                  "video/mp4" "video/mpeg" "video/avi" "video/quicktime" "video/webm")
     :context-window 128
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
     :context-window 128
     :input-cost 1
     :output-cost 1
     :cutoff-date "2025-01")
    (grok-code-fast-1
     :description "Fast reasoning model for agentic coding"
     :capabilities '(tool-use json reasoning)
     :context-window 128
     :input-cost 0.2
     :output-cost 1.5)))

(cl-defstruct (gptel--gh (:include gptel-openai)
                         (:copier nil)
                         (:constructor gptel--make-gh))
  token github-token sessionid machineid github-username)

(defcustom gptel-gh-github-token-file (expand-file-name ".cache/copilot-chat/github-token"
                                                        user-emacs-directory)
  "File where the GitHub token is stored."
  :type 'string
  :group 'gptel)

(defcustom gptel-gh-github-token-load-function 'gptel--gh-restore-github-token-from-file
  "Function to load the current github token. Default behavior is file-based based on `gptel-gh-github-token-file'."
  :type 'function
  :group 'gptel)

(defcustom gptel-gh-github-token-save-function 'gptel--gh-save-github-token-from-file
  "Function to save the new github token. Default behavior is file-based based on `gptel-gh-github-token-file'."
  :type 'function
  :group 'gptel)

(defconst gptel--gh-auth-common-headers
  `(("editor-plugin-version" . "gptel/*")
    ("editor-version" . ,(concat "emacs/" emacs-version))))

(defconst gptel--gh-client-id "Iv1.b507a08c87ecfe98")
(defconst gptel--gh-default-username-placeholder "[Default account]")

(defun gptel--gh-get-registered-usernames ()
  (if-let* ((gh-backends (seq-filter
                          (lambda (b)
                            (gptel--gh-p b))
                          (mapcar #'cdr gptel--known-backends)))
            (usernames (mapcar
                        (lambda (b)
                          (let ((username (gptel--gh-github-username b)))
                            (if (string= username "")
                                gptel--gh-default-username-placeholder
                              username)))
                        gh-backends)))
      (seq-uniq usernames)))

(defun gptel--gh-get-backends-by-username (github-username)
  (seq-filter (lambda (b) (and (gptel--gh-p b)
                               (string= (gptel--gh-github-username b)
                                        github-username)))
              (mapcar #'cdr gptel--known-backends)))

(defun gptel--gh-load-github-token (github-username)
  "Function that ensures that the GitHub OAuth token cache is used and is set."
  (if (gptel--gh-github-token gptel-backend)
      (gptel--gh-github-token gptel-backend)
    (let ((token (funcall gptel-gh-github-token-load-function github-username)))
      (if (string= token "")
          ;; Empty string should be interpreted as no data. Return nil so that a
          ;; proper login is performed.
          nil
        ;; Iterate over the known backends for the same username and set the GitHub token
        (dolist (b (gptel--gh-get-backends-by-username github-username) token)
          (setf (gptel--gh-github-token b) token))))))

(defun gptel--gh-save-github-token (github-username token)
  "Function that updates the GitHub OAuth token cache and calls the save function."
  ;; Update the token for all connected backends
  (dolist (b (gptel--gh-get-backends-by-username github-username))
    (setf (gptel--gh-github-token b) token))
  (funcall gptel-gh-github-token-save-function github-username
           (setf (gptel--gh-github-token gptel-backend) token)))

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

(defun gptel--gh-validate-github-username (github-username)
  "Ensures that the given GITHUB-USERNAME conforms to the rules from GitHub:

'Username may only contain alphanumeric characters or single hyphens,
and cannot begin or end with a hyphen.'

An empty string value is considered to be a general account to be used.
"
  (cond
   ;; Ensure that the username is a string
   ((not (stringp github-username)) (user-error "Provided GitHub username is not string"))

   ;; Length of zero is a default value
   ((= (length github-username) 0) t)

   ;; We have some characters, ensure that they conform to the rules
   ((string-match-p "\\`[0-9A-Za-z]\\(?:[0-9A-Za-z]\\|-[0-9A-Za-z]\\)*\\'" github-username) t)
   (t (user-error "Provided GitHub username '%s' doesn't conform to GitHub's username rules"
                  github-username))))

(defun gptel--gh-generate-github-token-filename (github-username)
  (gptel--gh-validate-github-username github-username)
  (if (= (length github-username) 0)
      gptel-gh-github-token-file
    (concat gptel-gh-github-token-file "_" github-username)))

(defun gptel--gh-restore-github-token-from-file (github-username)
  "Restore GitHub token from the file gptel-gh-github-token-file."
  (gptel--gh-restore-from-file (gptel--gh-generate-github-token-filename github-username)))

(defun gptel--gh-save-github-token-from-file (github-username token)
  "Save GitHub token to the file gptel-gh-github-token-file."
  (gptel--gh-save-to-file (gptel--gh-generate-github-token-filename github-username) token))

(defun gptel--gh-restore-from-file (file)
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

(defun gptel--gh-save-to-file (file obj)
  "Save OBJ to FILE."
  (let ((print-length nil)
        (print-level nil)
        (coding-system-for-write 'utf-8-unix))
    (make-directory (file-name-directory file) t)
    (write-region (prin1-to-string obj) nil file nil :silent)
    obj))

(defun gptel-gh-login (github-username)
  "Login to GitHub Copilot API.

This will prompt you to authorize in a browser and store the token.

GITHUB-USERNAME is used to provide a hint which account to login to.
It must match an existing backend.

In SSH sessions, the URL and code will be displayed for manual entry
instead of attempting to open a browser automatically."
  (interactive (list (let ((known-usernames (gptel--gh-get-registered-usernames)))
                       (cond
                        ((= 0 (length known-usernames))
                         (user-error "No GitHub copilot backends registered"))
                        ((= 1 (length known-usernames))
                         (car known-usernames))
                        (t
                         (let ((choice (completing-read "Choose GitHub username: " known-usernames
                                                        nil t nil nil nil nil)))
                           (if (string= choice gptel--gh-default-username-placeholder)
                               ""
                             choice)))))))
  (message "Logging in using '%s'" github-username)
  (let ((gh-backends (gptel--gh-get-backends-by-username github-username))
        ;; Detect SSH sessions
        (in-ssh-session (or (getenv "SSH_CLIENT")
                            (getenv "SSH_CONNECTION")
                            (getenv "SSH_TTY"))))
    ;; It shall only be possible to login when there exists a corresponding backend
    (if (= (length gh-backends) 0)
        (user-error "No GitHub CoPilot backend found for username '%s'" github-username))
    (pcase-let (((map :device_code :user_code :verification_uri)
                 (gptel--url-retrieve
                     "https://github.com/login/device/code"
                   :method 'post
                   :headers gptel--gh-auth-common-headers
                   :data `( :client_id ,gptel--gh-client-id
                            :scope "read:user"))))
      (gui-set-selection 'CLIPBOARD user_code)
      (let ((username-text (if (string= github-username "")
                               "Login for the default account."
                             (format "Login for '%s'." github-username))))
        (if in-ssh-session
            ;; SSH session: display URL and code, don't auto-open browser
            (progn
              (message "GitHub Device Code: %s (copied to clipboard)" user_code)
              (read-from-minibuffer
               (format "%s Code %s is copied. Visit %s \
in your local browser, enter the code, and authorize.  Press ENTER after authorizing. "
                       username-text user_code verification_uri)))
          ;; Local session: auto-open browser
          (read-from-minibuffer
           (format "%s Your one-time code %s is copied. \
Press ENTER to open GitHub in your browser. \
If your browser does not open automatically, browse to %s."
                   username-text user_code verification_uri))
          (browse-url verification_uri)
          (read-from-minibuffer "Press ENTER after authorizing.")))
      (let ((github-token
             (plist-get
              (gptel--url-retrieve
                  "https://github.com/login/oauth/access_token"
                :method 'post
                :headers gptel--gh-auth-common-headers
                :data `( :client_id ,gptel--gh-client-id
                         :device_code ,device_code
                         :grant_type "urn:ietf:params:oauth:grant-type:device_code"))
              :access_token)))
        (if (or (null github-token) (string-empty-p github-token))
            (user-error "Error: You might not have access to GitHub Copilot Chat!"))
        (message "Successfully logged in to GitHub Copilot")
        (gptel--gh-save-github-token github-username github-token)))))

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
        (user-error "Error: You might not have access to GitHub Copilot Chat!")
      (setf (gptel--gh-token gptel-backend) token))))

(defun gptel--gh-auth ()
  "Authenticate with GitHub Copilot API.

We first need github authorization (github token).
Then we need a session token."
  (unless (gptel--gh-github-token gptel-backend)
    (let* ((github-username (gptel--gh-github-username gptel-backend))
           (token (gptel--gh-load-github-token github-username)))
      (if token
          (setf (gptel--gh-github-token gptel-backend) token)
        (gptel-gh-login github-username))))

  (pcase-let (((map :token :expires_at)
               (gptel--gh-token gptel-backend)))
    (when (or (null token)
              (and expires_at
                   (> (round (float-time)) expires_at)))
      (gptel--gh-renew-token))))

;;;###autoload
(cl-defun gptel-make-gh-copilot
    (name &key (github-username "") curl-args request-params
          (header (lambda ()
                    (gptel--gh-auth)
                    `(("openai-intent" . "conversation-panel")
                      ("authorization" . ,(concat "Bearer "
                                                  (plist-get (gptel--gh-token gptel-backend) :token)))
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

GITHUB-USERNAME (optional) is an indicator of which GitHub account to associate
the backend with. This enables backends to be logged in as a separate user. Note
that this is only a hint and will be used when a GitHub is saved/loaded.

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
  (gptel--gh-validate-github-username github-username)
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
                  :github-username github-username
                  :url (concat protocol "://" host endpoint)
                  :machineid (gptel--gh-machine-id))))
    (setf (alist-get name gptel--known-backends nil nil #'equal) backend)
    backend))

(provide 'gptel-gh)
;;; gptel-gh.el ends here

;; Local Variables:
;; byte-compile-warnings: (not docstrings)
;; End:
