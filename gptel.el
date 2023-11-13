;;; gptel.el --- A simple multi-LLM client      -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Karthik Chikmagalur

;; Author: Karthik Chikmagalur
;; Version: 0.5.0
;; Package-Requires: ((emacs "27.1") (transient "0.4.0"))
;; Keywords: convenience
;; URL: https://github.com/karthink/gptel

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

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; gptel is a simple Large Language Model chat client, with support for multiple models/backends.
;;
;; gptel supports ChatGPT, Azure, and local models using Ollama and GPT4All.
;;
;;  Features:
;;  - It’s async and fast, streams responses.
;;  - Interact with LLMs from anywhere in Emacs (any buffer, shell, minibuffer,
;;    wherever)
;;  - LLM responses are in Markdown or Org markup.
;;  - Supports conversations and multiple independent sessions.
;;  - Save chats as regular Markdown/Org/Text files and resume them later.
;;  - You can go back and edit your previous prompts or LLM responses when
;;    continuing a conversation. These will be fed back to the model.
;;
;; Requirements for ChatGPT/Azure:
;;
;; - You need an OpenAI API key. Set the variable `gptel-api-key' to the key or
;;   to a function of no arguments that returns the key. (It tries to use
;;   `auth-source' by default)
;;
;; - For Azure: define a gptel-backend with `gptel-make-azure', which see.
;;
;; For local models using Ollama or GPT4All:
;;
;; - The model has to be running on an accessible address (or localhost)
;; - Define a gptel-backend with `gptel-make-ollama' or `gptel-make-gpt4all',
;;   which see.
;;
;; Usage:
;;
;; gptel can be used in any buffer or in a dedicated chat buffer. The
;; interaction model is simple: Type in a query and the response will be
;; inserted below. You can continue the conversation by typing below the
;; response.
;;
;; To use this in a dedicated buffer:
;; - M-x gptel: Start a ChatGPT session
;; - C-u M-x gptel: Start another session or multiple independent ChatGPT sessions
;;
;; - In the chat session: Press `C-c RET' (`gptel-send') to send your prompt.
;;   Use a prefix argument (`C-u C-c RET') to access a menu. In this menu you
;;   can set chat parameters like the system directives, active backend or
;;   model, or choose to redirect the input or output elsewhere (such as to the
;;   kill ring).
;;
;; - If using `org-mode': You can save this buffer to a file. When opening this
;;   file, turning on `gptel-mode' will allow resuming the conversation.
;;
;; To use this in any buffer:
;;
;; - Select a region of text and call `gptel-send'. Call with a prefix argument
;;   to access the menu. The contents of the buffer up to (point) are used
;;   if no region is selected.
;; - You can select previous prompts and responses to continue the conversation.
;;
;; Finally, gptel offers a general purpose API for writing LLM ineractions
;; that suit how you work, see `gptel-request'.

;;; Code:
(declare-function markdown-mode "markdown-mode")
(declare-function gptel-curl-get-response "gptel-curl")
(declare-function gptel-menu "gptel-transient")
(declare-function pulse-momentary-highlight-region "pulse")

;; Functions used for saving/restoring gptel state in Org buffers
(defvar org-entry-property-inherited-from)
(declare-function org-entry-get "org")
(declare-function org-entry-put "org")
(declare-function org-with-wide-buffer "org-macs")
(declare-function org-set-property "org")
(declare-function org-property-values "org")
(declare-function org-open-line "org")
(declare-function org-at-heading-p "org")
(declare-function org-get-heading "org")

(eval-when-compile
  (require 'subr-x)
  (require 'cl-lib))

(require 'url)
(require 'json)
(require 'map)
(require 'text-property-search)
(require 'cl-generic)
(require 'gptel-openai)

(defgroup gptel nil
  "Interact with ChatGPT from anywhere in Emacs."
  :group 'hypermedia)

;; (defcustom gptel-host "api.openai.com"
;;   "The API host queried by gptel."
;;   :group 'gptel
;;   :type 'string)
(make-obsolete-variable
 'gptel-host
 "Use `gptel-make-openai' instead."
 "0.5.0")

(defcustom gptel-proxy ""
  "Path to a proxy to use for gptel interactions.
Passed to curl via --proxy arg, for example \"proxy.yourorg.com:80\"
Leave it empty if you don't use a proxy."
  :group 'gptel
  :type 'string)

(defcustom gptel-api-key #'gptel-api-key-from-auth-source
  "An API key (string) for the default LLM backend.

OpenAI by default.

Can also be a function of no arguments that returns an API
key (more secure) for the active backend."
  :group 'gptel
  :type '(choice
          (string :tag "API key")
          (function :tag "Function that returns the API key")))

(defcustom gptel-stream t
  "Whether responses from ChatGPT be played back as they are received.

This option is ignored unless Curl is in use (see `gptel-use-curl').

When set to nil, Emacs waits for the full response and inserts it
all at once. This wait is asynchronous.

\='tis a bit silly."
  :group 'gptel
  :type 'boolean)
(make-obsolete-variable 'gptel-playback 'gptel-stream "0.3.0")

(defcustom gptel-use-curl (and (executable-find "curl") t)
  "Whether gptel should prefer Curl when available."
  :group 'gptel
  :type 'boolean)

(defcustom gptel-response-filter-functions
  '(gptel--convert-org)
  "Abnormal hook for transforming the response from ChatGPT.

This is useful if you want to format the response in some way,
such as filling paragraphs, adding annotations or recording
information in the response like links.

Each function in this hook receives two arguments, the response
string to transform and the ChatGPT interaction buffer. It should
return the transformed string."
  :group 'gptel
  :type 'hook)

(defcustom gptel-pre-response-hook nil
  "Hook run before inserting ChatGPT's response into the current buffer.

This hook is called in the buffer from which the prompt was sent
to ChatGPT. Note: this hook only runs if the request succeeds."
  :group 'gptel
  :type 'hook)

(defcustom gptel-post-response-hook nil
  "Hook run after inserting ChatGPT's response into the current buffer.

This hook is called in the buffer from which the prompt was sent
to ChatGPT. Note: this hook runs even if the request fails."
  :group 'gptel
  :type 'hook)

(defvar gptel-default-session "*ChatGPT*")
(defcustom gptel-default-mode (if (fboundp 'markdown-mode)
                               'markdown-mode
                             'text-mode)
  "The default major mode for dedicated chat buffers.

If `markdown-mode' is available, it is used. Otherwise gptel
defaults to `text-mode'."
  :group 'gptel
  :type 'symbol)

;; TODO: Handle `prog-mode' using the `comment-start' variable
(defcustom gptel-prompt-prefix-alist
  '((markdown-mode . "### ")
    (org-mode . "*** ")
    (text-mode . "### "))
  "String inserted after the response from ChatGPT.

This is an alist mapping major modes to the prefix strings. This
is only inserted in dedicated gptel buffers."
  :group 'gptel
  :type '(alist :key-type symbol :value-type string))

(defcustom gptel-crowdsourced-prompts-file
  (let ((cache-dir (or (getenv "XDG_CACHE_HOME")
                       (getenv "XDG_DATA_HOME")
                       user-emacs-directory)))
    (expand-file-name "gptel-crowdsourced-prompts.csv" cache-dir))
  "File used to store crowdsourced system prompts.

These are prompts cached from an online source (see
`gptel--crowdsourced-prompts-url'), and can be set from the
transient menu interface provided by `gptel-menu'."
  :group 'gptel
  :type 'file)

;; FIXME This is convoluted, but it's not worth adding the `compat' dependency
;; just for a couple of helper functions either.
(cl-macrolet
    ((gptel--compat
      () (if (version< "28.1" emacs-version)
             (macroexp-progn
              `((defalias 'gptel--button-buttonize #'button-buttonize)
                (defalias 'gptel--always #'always)))
           (macroexp-progn
            `((defun gptel--always (&rest _)
               "Always return t." t)
              (defun gptel--button-buttonize (string callback)
               "Make STRING into a button and return it.
When clicked, CALLBACK will be called."
               (propertize string
                'face 'button
                'button t
                'follow-link t
                'category t
                'button-data nil
                'keymap button-map
                'action callback)))))))
  (gptel--compat))

;; Model and interaction parameters
(defcustom gptel-directives
  '((default . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
    (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
    (writing . "You are a large language model and a writing assistant. Respond concisely.")
    (chat . "You are a large language model and a conversation partner. Respond concisely."))
  "System prompts (directives) for ChatGPT.

These are system instructions sent at the beginning of each
request to ChatGPT.

Each entry in this alist maps a symbol naming the directive to
the string that is sent. To set the directive for a chat session
interactively call `gptel-send' with a prefix argument."
  :group 'gptel
  :safe #'gptel--always
  :type '(alist :key-type symbol :value-type string))

(defvar-local gptel--system-message (alist-get 'default gptel-directives))
(put 'gptel--system-message 'safe-local-variable #'gptel--always)

(defcustom gptel-max-tokens nil
  "Max tokens per response.

This is roughly the number of words in the response. 100-300 is a
reasonable range for short answers, 400 or more for longer
responses.

To set the target token count for a chat session interactively
call `gptel-send' with a prefix argument.

If left unset, ChatGPT will target about 40% of the total token
count of the conversation so far in each message, so messages
will get progressively longer!"
  :local t
  :safe #'gptel--always
  :group 'gptel
  :type '(choice (integer :tag "Specify Token count")
                 (const :tag "Default" nil)))

(defcustom gptel-model "gpt-3.5-turbo"
  "GPT Model for chat.

The current options for ChatGPT are
- \"gpt-3.5-turbo\"
- \"gpt-3.5-turbo-16k\"
- \"gpt-4\" (experimental)
- \"gpt-4-32k\" (experimental)
 
To set the model for a chat session interactively call
`gptel-send' with a prefix argument."
  :local t
  :safe #'gptel--always
  :group 'gptel
  :type '(choice
          (const :tag "GPT 3.5 turbo" "gpt-3.5-turbo")
          (const :tag "GPT 3.5 turbo 16k" "gpt-3.5-turbo-16k")
          (const :tag "GPT 4 (experimental)" "gpt-4")
          (const :tag "GPT 4 32k (experimental)" "gpt-4-32k")))

(defcustom gptel-temperature 1.0
  "\"Temperature\" of ChatGPT response.

This is a number between 0.0 and 2.0 that controls the randomness
of the response, with 2.0 being the most random.

To set the temperature for a chat session interactively call
`gptel-send' with a prefix argument."
  :local t
  :safe #'gptel--always
  :group 'gptel
  :type 'number)

(defvar gptel--known-backends nil
  "Alist of LLM backends known to gptel.

This is an alist mapping user-provided names to backend structs,
see `gptel-backend'.

You can have more than one backend pointing to the same resource
with differing settings.")

(defvar gptel--openai
  (gptel-make-openai
   "ChatGPT"
   :header (lambda () `(("Authorization" . ,(concat "Bearer " (gptel--get-api-key)))))
   :key 'gptel-api-key
   :stream t
   :models '("gpt-3.5-turbo" "gpt-3.5-turbo-16k" "gpt-4" "gpt-4-32k")))

(defvar-local gptel-backend gptel--openai)

(defvar-local gptel--bounds nil)
(put 'gptel--bounds 'safe-local-variable #'gptel--always)

(defvar-local gptel--num-messages-to-send nil)
(put 'gptel--num-messages-to-send 'safe-local-variable #'gptel--always)

(defvar gptel--debug nil
  "Enable printing debug messages.

Also shows the response buffer when making requests.")

(defun gptel-api-key-from-auth-source (&optional host user)
  "Lookup api key in the auth source.
By default, the LLM host for the active backend is used as HOST,
and \"apikey\" as USER."
  (if-let ((secret
            (plist-get
             (car (auth-source-search
                   :host (or host (gptel-backend-host gptel-backend))
                   :user (or user "apikey")
                   :require '(:secret)))
                              :secret)))
      (if (functionp secret)
          (encode-coding-string (funcall secret) 'utf-8)
        secret)
    (user-error "No `gptel-api-key' found in the auth source")))

;; FIXME Should we utf-8 encode the api-key here?
(defun gptel--get-api-key (&optional key)
  "Get api key from KEY, or from `gptel-api-key'."
  (when-let* ((key-sym (or key (gptel-backend-key gptel-backend))))
    (cl-typecase key-sym
      (function (funcall key-sym))
      (string key-sym)
      (symbol (gptel--get-api-key
               (symbol-value key-sym)))
      (t (error "`gptel-api-key' is not valid")))))

(defsubst gptel--numberize (val)
  "Ensure VAL is a number."
  (if (stringp val) (string-to-number val) val))

(defmacro gptel--at-word-end (&rest body)
  "Execute BODY at end of the current word or punctuation."
  `(save-excursion
     (skip-syntax-forward "w.")
     ,@body))

(defun gptel-prompt-string ()
  (or (alist-get major-mode gptel-prompt-prefix-alist) ""))

(defun gptel--restore-state ()
  "Restore gptel state when turning on `gptel-mode'.

Currently saving and restoring state is implemented only for
`org-mode' buffers."
  (when (buffer-file-name)
    (pcase major-mode
      ('org-mode
       (save-restriction
         (widen)
         (condition-case-unless-debug nil
             (progn
               (when-let ((bounds (org-entry-get (point-min) "GPTEL_BOUNDS")))
                 (mapc (pcase-lambda (`(,beg . ,end))
                         (add-text-properties
                          beg end '(gptel response rear-nonsticky t)))
                       (read bounds))
                 (message "gptel chat restored."))
               (when-let ((model (org-entry-get (point-min) "GPTEL_MODEL")))
                 (setq-local gptel-model model))
               (when-let ((system (org-entry-get (point-min) "GPTEL_SYSTEM")))
                 (setq-local gptel--system-message system))
               (when-let ((temp (org-entry-get (point-min) "GPTEL_TEMPERATURE")))
                 (setq-local gptel-temperature (gptel--numberize temp))))
           (error (message "Could not restore gptel state, sorry!")))))
      (_ (when gptel--bounds
           (mapc (pcase-lambda (`(,beg . ,end))
                         (put-text-property beg end 'gptel 'response))
                 gptel--bounds)
           (message "gptel chat restored."))))))

(defun gptel--save-state ()
  "Write the gptel state to the buffer.

This enables saving the chat session when writing the buffer to
disk.  To restore a chat session, turn on `gptel-mode' after
opening the file."
  (pcase major-mode
    ('org-mode
     (org-with-wide-buffer
      (goto-char (point-min))
      (when (org-at-heading-p)
        (org-open-line 1))
      (org-entry-put (point-min) "GPTEL_MODEL" gptel-model)
      (unless (equal (default-value 'gptel-temperature) gptel-temperature)
        (org-entry-put (point-min) "GPTEL_TEMPERATURE"
                       (number-to-string gptel-temperature)))
      (unless (string= (default-value 'gptel--system-message)
                       gptel--system-message)
        (org-entry-put (point-min) "GPTEL_SYSTEM"
                       gptel--system-message))
      (when gptel-max-tokens
        (org-entry-put
         (point-min) "GPTEL_MAX_TOKENS" gptel-max-tokens))
      ;; Save response boundaries
      (letrec ((write-bounds
                (lambda (attempts)
                  (let* ((bounds (gptel--get-bounds))
                         (offset (caar bounds))
                         (offset-marker (set-marker (make-marker) offset)))
                    (org-entry-put (point-min) "GPTEL_BOUNDS"
                                   (prin1-to-string (gptel--get-bounds)))
                    (when (and (not (= (marker-position offset-marker) offset))
                               (> attempts 0))
                      (funcall write-bounds (1- attempts)))))))
        (funcall write-bounds 6))))
    (_ (save-excursion
         (save-restriction
           (add-file-local-variable 'gptel-model gptel-model)
           (unless (equal (default-value 'gptel-temperature) gptel-temperature)
             (add-file-local-variable 'gptel-temperature gptel-temperature))
           (unless (string= (default-value 'gptel--system-message)
                            gptel--system-message)
             (add-file-local-variable 'gptel--system-message gptel--system-message))
           (when gptel-max-tokens
             (add-file-local-variable 'gptel-max-tokens gptel-max-tokens))
           (add-file-local-variable 'gptel--bounds (gptel--get-bounds)))))))

(defun gptel--get-bounds ()
  "Return the gptel response boundaries as an alist."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (let ((prop) (bounds))
        (while (setq prop (text-property-search-backward
                           'gptel 'response t))
          (push (cons (prop-match-beginning prop)
                      (prop-match-end prop))
                bounds))
        bounds))))

(defvar-local gptel--old-header-line nil)
;;;###autoload
(define-minor-mode gptel-mode
  "Minor mode for interacting with ChatGPT."
  :lighter " GPT"
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c RET") #'gptel-send)
    map)
  (if gptel-mode
      (progn
        (unless (memq major-mode '(org-mode markdown-mode text-mode))
          (gptel-mode -1)
          (user-error (format "`gptel-mode' is not supported in `%s'." major-mode)))
        (add-hook 'before-save-hook #'gptel--save-state nil t)
        (gptel--restore-state)
        (setq gptel--old-header-line header-line-format
              header-line-format
              (list '(:eval (concat (propertize " " 'display '(space :align-to 0))
                                    (format "%s" (gptel-backend-name gptel-backend))))
                    (propertize " Ready" 'face 'success)
                    '(:eval
                      (let* ((l1 (length gptel-model))
                             (num-exchanges
                              (if gptel--num-messages-to-send
                                  (format "[Send: %s exchanges]" gptel--num-messages-to-send)
                                "[Send: buffer]"))
                             (l2 (length num-exchanges)))
                       (concat
                        (propertize
                         " " 'display `(space :align-to ,(max 1 (- (window-width) (+ 2 l1 l2)))))
                        (propertize
                         (gptel--button-buttonize num-exchanges
                          (lambda (&rest _) (gptel-menu)))
                         'mouse-face 'highlight
                         'help-echo
                         "Number of past exchanges to include with each request")
                        " "
                        (propertize
                         (gptel--button-buttonize (concat "[" gptel-model "]")
                          (lambda (&rest _) (gptel-menu)))
                         'mouse-face 'highlight
                         'help-echo "GPT model in use")))))))
    (setq header-line-format gptel--old-header-line)))

(defun gptel--update-header-line (msg face)
  "Update header line with status MSG in FACE."
  (and gptel-mode (consp header-line-format)
    (setf (nth 1 header-line-format)
          (propertize msg 'face face))
    (force-mode-line-update)))

(cl-defun gptel-request
    (&optional prompt &key callback
               (buffer (current-buffer))
               position context
               (stream nil) (in-place nil)
               (system gptel--system-message))
  "Request a response from the `gptel-backend' for PROMPT.

Note: This function is not fully self-contained. Consider
let-binding the parameters `gptel-backend' and `gptel-model'
around calls to it as required.

If PROMPT is
- a string, it is used to create a full prompt suitable for
  sending to ChatGPT.
- nil but region is active, the region contents are used.
- nil, the current buffer's contents up to (point) are used.
  Previous responses from ChatGPT are identified as responses.
- A list of plists, it is used as is.

Keyword arguments:

CALLBACK, if supplied, is a function of two arguments, called
with the RESPONSE (a string) and INFO (a plist):

(callback RESPONSE INFO)

RESPONSE is nil if there was no response or an error.

The INFO plist has (at least) the following keys:
:prompt       - The full prompt that was sent with the request
:position     - marker at the point the request was sent.
:buffer       - The buffer current when the request was sent.
:status       - Short string describing the result of the request

Example of a callback that messages the user with the response
and info:

(lambda (response info)
  (if response
      (let ((posn (marker-position (plist-get info :position)))
            (buf  (buffer-name (plist-get info :buffer))))
        (message \"Response for request from %S at %d: %s\"
                 buf posn response))
    (message \"gptel-request failed with message: %s\"
             (plist-get info :status))))

Or, for just the response:

(lambda (response _)
  ;; Do something with response
  (message (rot13-string response)))

If CALLBACK is omitted, the response is inserted at the point the
request was sent.

BUFFER is the buffer the request belongs to. If omitted the
current buffer is recorded.

POSITION is a buffer position (integer or marker). If omitted,
the value of (point) or (region-end) is recorded, depending on
whether the region is active.

CONTEXT is any additional data needed for the callback to run. It
is included in the INFO argument to the callback.

SYSTEM is the system message (chat directive) sent to ChatGPT. If
omitted, the value of `gptel--system-message' for the current
buffer is used.

The following keywords are mainly for internal use:

IN-PLACE is a boolean used by the default callback when inserting
the response to determine if delimiters are needed between the
prompt and the response.

STREAM is a boolean that determines if the response should be
streamed, as in `gptel-stream'. Do not set this if you are
specifying a custom CALLBACK!

Model parameters can be let-bound around calls to this function."
  (let* ((gptel-stream stream)
         (start-marker
          (cond
           ((null position)
            (if (use-region-p)
                (set-marker (make-marker) (region-end))
              (gptel--at-word-end (point-marker))))
           ((markerp position) position)
           ((integerp position)
            (set-marker (make-marker) position buffer))))
         (full-prompt
          (cond
           ((null prompt)
            (let ((gptel--system-message system))
              (gptel--create-prompt start-marker)))
           ((stringp prompt)
            ;; FIXME Dear reader, welcome to Jank City:
            (with-temp-buffer
              (let ((gptel--system-message system)
                    (gptel-backend (buffer-local-value 'gptel-backend buffer)))
                (insert prompt)
                (gptel--create-prompt))))
           ((consp prompt) prompt)))
         (info (list :prompt full-prompt
                     :buffer buffer
                     :position start-marker)))
    (when context (plist-put info :context context))
    (when in-place (plist-put info :in-place in-place))
    (funcall
     (if gptel-use-curl
         #'gptel-curl-get-response #'gptel--url-get-response)
     info callback)))

;; TODO: Handle multiple requests(#15). (Only one request from one buffer at a time?)
;;;###autoload
(defun gptel-send (&optional arg)
  "Submit this prompt to ChatGPT.

With prefix arg ARG activate a transient menu with more options
instead."
  (interactive "P")
  (if (and arg (require 'gptel-transient nil t))
      (call-interactively #'gptel-menu)
  (message "Querying %s..." (gptel-backend-name gptel-backend))
  (let* ((response-pt
          (if (use-region-p)
              (set-marker (make-marker) (region-end))
            (gptel--at-word-end (point-marker))))
         (gptel-buffer (current-buffer))
         (full-prompt (gptel--create-prompt response-pt)))
    (funcall
     (if gptel-use-curl
         #'gptel-curl-get-response #'gptel--url-get-response)
     (list :prompt full-prompt
           :buffer gptel-buffer
           :position response-pt)))
    (gptel--update-header-line " Waiting..." 'warning)))

(defun gptel--insert-response (response info)
  "Insert RESPONSE from ChatGPT into the gptel buffer.

INFO is a plist containing information relevant to this buffer.
See `gptel--url-get-response' for details."
  (let* ((status-str  (plist-get info :status))
         (gptel-buffer (plist-get info :buffer))
         (start-marker (plist-get info :position)))
    ;; Handle read-only buffers
    (when (with-current-buffer gptel-buffer
            (or buffer-read-only
                (get-char-property start-marker 'read-only)))
      (message "Buffer is read only, displaying reply in buffer \"*ChatGPT response*\"")
      (display-buffer
       (with-current-buffer (get-buffer-create "*ChatGPT response*")
         (goto-char (point-max))
         (move-marker start-marker (point) (current-buffer))
         (current-buffer))
       '((display-buffer-reuse-window
          display-buffer-pop-up-window)
         (reusable-frames . visible))))
    ;; Insert response and status message/error message
    (with-current-buffer gptel-buffer
      (if response
          (progn
            (setq response (gptel--transform-response
                               response gptel-buffer))
            (save-excursion
              (add-text-properties
               0 (length response) '(gptel response rear-nonsticky t)
               response)
              (with-current-buffer (marker-buffer start-marker)
                (goto-char start-marker)
                (run-hooks 'gptel-pre-response-hook)
                (unless (or (bobp) (plist-get info :in-place))
                  (insert "\n\n"))
                (let ((p (point)))
                  (insert response)
                  (pulse-momentary-highlight-region p (point)))
                (when gptel-mode (insert "\n\n" (gptel-prompt-string))))
              (when gptel-mode (gptel--update-header-line " Ready" 'success))))
        (gptel--update-header-line
         (format " Response Error: %s" status-str) 'error)
        (message "ChatGPT response error: (%s) %s"
                 status-str (plist-get info :error)))
      (run-hooks 'gptel-post-response-hook))))

(defun gptel-set-topic ()
  "Set a topic and limit this conversation to the current heading.

This limits the context sent to ChatGPT to the text between the
current heading and the cursor position."
  (interactive)
  (pcase major-mode
    ('org-mode
     (org-set-property
      "GPTEL_TOPIC"
      (completing-read "Set topic as: "
                       (org-property-values "GPTEL_TOPIC")
                       nil nil (downcase
                                (truncate-string-to-width
                                 (substring-no-properties
                                  (replace-regexp-in-string
                                   "\\s-+" "-"
                                   (org-get-heading)))
                                 50)))))
    ('markdown-mode
     (message
      "Support for multiple topics per buffer is not implemented for `markdown-mode'."))))

(defun gptel--get-topic-start ()
  "If a conversation topic is set, return it."
  (pcase major-mode
    ('org-mode
     (when (org-entry-get (point) "GPTEL_TOPIC" 'inherit)
         (marker-position org-entry-property-inherited-from)))
    ('markdown-mode nil)))

(defun gptel--create-prompt (&optional prompt-end)
  "Return a full conversation prompt from the contents of this buffer.

If `gptel--num-messages-to-send' is set, limit to that many
recent exchanges.

If the region is active limit the prompt to the region contents
instead.

If PROMPT-END (a marker) is provided, end the prompt contents
there."
  (save-excursion
    (save-restriction
      (cond
       ((use-region-p)
        ;; Narrow to region
        (narrow-to-region (region-beginning) (region-end))
        (goto-char (point-max)))
       ((when-let ((topic-start (gptel--get-topic-start)))
          ;; Narrow to topic
          (narrow-to-region topic-start (or prompt-end (point-max)))
          (goto-char (point-max))))
       (t (goto-char (or prompt-end (point-max)))))
      (let ((max-entries (and gptel--num-messages-to-send
                              (* 2 gptel--num-messages-to-send))))
        (gptel--parse-buffer gptel-backend max-entries)))))

(cl-defgeneric gptel--parse-buffer (backend max-entries)
  "Parse the current buffer backwards from point and return a list
of prompts.

BACKEND is the LLM backend in use.

MAX-ENTRIES is the number of queries/responses to include for
contexbt.")

(cl-defgeneric gptel--request-data (backend prompts)
  "Generate a plist of all data for an LLM query.

BACKEND is the LLM backend in use.

PROMPTS is the plist of previous user queries and LLM responses.")

;; TODO: Use `run-hook-wrapped' with an accumulator instead to handle
;; buffer-local hooks, etc.
(defun gptel--transform-response (content-str buffer)
  "Filter CONTENT-STR through `gptel-response-filter-functions`.

BUFFER is passed along with CONTENT-STR to each function in this
hook."
  (let ((filtered-str content-str))
    (dolist (filter-func gptel-response-filter-functions filtered-str)
      (condition-case nil
          (when (functionp filter-func)
            (setq filtered-str
                  (funcall filter-func filtered-str buffer)))
        (error
         (display-warning '(gptel filter-functions)
                          (format "Function %S returned an error"
                                  filter-func)))))))

(defun gptel--convert-org (content buffer)
  "Transform CONTENT according to required major-mode.

Currently only `org-mode' is handled.

BUFFER is the interaction buffer for ChatGPT."
  (pcase (buffer-local-value 'major-mode buffer)
    ('org-mode (gptel--convert-markdown->org content))
    (_ content)))

(defun gptel--url-get-response (info &optional callback)
  "Fetch response to prompt in INFO from ChatGPT.

INFO is a plist with the following keys:
- :prompt (the prompt being sent)
- :buffer (the gptel buffer)
- :position (marker at which to insert the response).

Call CALLBACK with the response and INFO afterwards. If omitted
the response is inserted into the current buffer after point."
  (let* ((inhibit-message t)
         (message-log-max nil)
         (backend gptel-backend)
         (url-request-method "POST")
         (url-request-extra-headers
          (append '(("Content-Type" . "application/json"))
                  (when-let ((backend-header (gptel-backend-header gptel-backend)))
                    (if (functionp backend-header)
                        (funcall backend-header)
                      backend-header))))
        (url-request-data
         (encode-coding-string
          (json-encode (gptel--request-data
                        gptel-backend (plist-get info :prompt)))
          'utf-8)))
    (url-retrieve (gptel-backend-url gptel-backend)
                  (lambda (_)
                    (pcase-let ((`(,response ,http-msg ,error)
                                 (gptel--url-parse-response backend (current-buffer))))
                      (plist-put info :status http-msg)
                      (when error (plist-put info :error error))
                      (funcall (or callback #'gptel--insert-response)
                               response info)
                      (kill-buffer)))
                  nil t nil)))

(cl-defgeneric gptel--parse-response (backend response proc-info)
  "Response extractor for LLM requests.

BACKEND is the LLM backend in use.

RESPONSE is the parsed JSON of the response, as a plist.

PROC-INFO is a plist with process information and other context.
See `gptel-curl--get-response' for its contents.")

(defun gptel--url-parse-response (backend response-buffer)
  "Parse response in RESPONSE-BUFFER."
  (when (buffer-live-p response-buffer)
    (when gptel--debug
      (with-current-buffer response-buffer
        (clone-buffer "*gptel-error*" 'show)))
    (with-current-buffer response-buffer
      (if-let* ((http-msg (string-trim (buffer-substring (line-beginning-position)
                                                         (line-end-position))))
                (json-object-type 'plist)
                (response (progn (forward-paragraph)
                                 (let ((json-str (decode-coding-string
                                                  (buffer-substring-no-properties (point) (point-max))
                                                  'utf-8)))
                                   (condition-case nil
                                       (json-read-from-string json-str)
                                     (json-readtable-error 'json-read-error))))))
          (cond
           ((string-match-p "200 OK" http-msg)
            (list (string-trim (gptel--parse-response backend response
                                             '(:buffer response-buffer)))
                   http-msg))
           ((plist-get response :error)
            (let* ((error-data (plist-get response :error))
                   (error-msg (plist-get error-data :message))
                   (error-type (plist-get error-data :type))
                   (backend-name (gptel-backend-name backend)))
              (if (stringp error-data)
                  (progn (message "%s error: (%s) %s" backend-name http-msg error-data)
                         (setq error-msg (string-trim error-data)))
                (when (stringp error-msg)
                  (message "%s error: (%s) %s" backend-name http-msg (string-trim error-msg)))
                (when error-type (setq http-msg (concat "("  http-msg ") " (string-trim error-type)))))
              (list nil (concat "(" http-msg ") " (or error-msg "")))))
           ((eq response 'json-read-error)
            (list nil (concat "(" http-msg ") Malformed JSON in response.") "json-read-error"))
           (t (list nil (concat "(" http-msg ") Could not parse HTTP response.")
                    "Could not parse HTTP response.")))
        (list nil (concat "(" http-msg ") Could not parse HTTP response.")
              "Could not parse HTTP response.")))))

;;;###autoload
(defun gptel (name &optional _ initial)
  "Switch to or start ChatGPT session with NAME.

With a prefix arg, query for a (new) session name.

Ask for API-KEY if `gptel-api-key' is unset.

If region is active, use it as the INITIAL prompt. Returns the
buffer created or switched to."
  (interactive (list (if current-prefix-arg
                         (read-string "Session name: " (generate-new-buffer-name gptel-default-session))
                       gptel-default-session)
                     (let ((backend (default-value 'gptel-backend)))
                       (condition-case nil
                           (gptel--get-api-key
                            (gptel-backend-key backend))
                         ((error user-error)
                          (setq gptel-api-key
                                (read-passwd
                                 (format "%s API key: "
                                         (gptel-backend-name backend)))))))
                     (and (use-region-p)
                          (buffer-substring (region-beginning)
                                            (region-end)))))
  ;; (unless api-key
  ;;   (user-error "No API key available"))
  (with-current-buffer (get-buffer-create name)
    (cond ;Set major mode
     ((eq major-mode gptel-default-mode))
     ((eq gptel-default-mode 'text-mode)
      (text-mode)
      (visual-line-mode 1))
     (t (funcall gptel-default-mode)))
    (unless gptel-mode (gptel-mode 1))
    (if (bobp) (insert (or initial (gptel-prompt-string))))
    (goto-char (point-max))
    (skip-chars-backward "\t\r\n")
    (when (called-interactively-p 'gptel)
      (pop-to-buffer (current-buffer))
      (message "Send your query with %s!"
               (substitute-command-keys "\\[gptel-send]")))
    (current-buffer)))

(defun gptel--convert-markdown->org (str)
  "Convert string STR from markdown to org markup.

This is a very basic converter that handles only a few markup
elements."
  (interactive)
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (while (re-search-forward "`\\|\\*\\{1,2\\}\\|_" nil t)
      (pcase (match-string 0)
        ("`" (if (looking-at "``")
                 (progn (backward-char)
                        (delete-char 3)
                        (insert "#+begin_src ")
                        (when (re-search-forward "^```" nil t)
                          (replace-match "#+end_src")))
               (replace-match "=")))
        ("**" (cond
               ((looking-at "\\*\\(?:[[:word:]]\\|\s\\)")
                (delete-char 1))
               ((looking-back "\\(?:[[:word:]]\\|\s\\)\\*\\{2\\}"
                              (max (- (point) 3) (point-min)))
                (backward-delete-char 1))))
        ((or "_" "*")
         (if (save-match-data
               (and (looking-back "\\(?:[[:space:]]\\|\s\\)\\(?:_\\|\\*\\)"
                                  (max (- (point) 2) (point-min)))
                    (not (looking-at "[[:space:]]\\|\s"))))
             ;; Possible beginning of italics
             (and
              (save-excursion
                (when (and (re-search-forward (regexp-quote (match-string 0)) nil t)
                           (looking-at "[[:space]]\\|\s")
                           (not (looking-back "\\(?:[[:space]]\\|\s\\)\\(?:_\\|\\*\\)"
                                              (max (- (point) 2) (point-min)))))
                  (backward-delete-char 1)
                  (insert "/") t))
              (progn (backward-delete-char 1)
                     (insert "/")))))))
    (buffer-string)))

(defun gptel--stream-convert-markdown->org ()
  "Return a Markdown to Org converter.

This function parses a stream of Markdown text to Org
continuously when it is called with successive chunks of the
text stream."
  (letrec ((in-src-block)
           (temp-buf (generate-new-buffer-name "*gptel-temp*"))
           (start-pt (make-marker))
           (cleanup-fn
            (lambda ()
              (when (buffer-live-p (get-buffer temp-buf))
                (set-marker start-pt nil)
                (kill-buffer temp-buf))
              (remove-hook 'gptel-post-response-hook cleanup-fn))))
    (add-hook 'gptel-post-response-hook cleanup-fn)
    (lambda (str)
      (let ((noop-p))
        (with-current-buffer (get-buffer-create temp-buf)
          (save-excursion (goto-char (point-max))
                          (insert str))
          (when (marker-position start-pt) (goto-char start-pt))
          (save-excursion
            (while (re-search-forward "`\\|\\*\\{1,2\\}\\|_" nil t)
              (pcase (match-string 0)
                ("`"
                 (cond
                  ((looking-at "``")
                   (backward-char 1)
                   (delete-char 3)
                   (if in-src-block
                       (progn (insert "#+end_src")
                              (setq in-src-block nil))
                     (insert "#+begin_src ")
                     (setq in-src-block t)))
                  ((looking-at "`\\|$")
                   (setq noop-p t)
                   (set-marker start-pt (1- (point)))
                   (unless (eobp) (forward-char 1)))
                  ((not in-src-block) (replace-match "="))))
                ((and "**" (guard (not in-src-block)))
                 (cond
                  ((looking-at "\\*\\(?:[[:word:]]\\|\s\\)")
                   (delete-char 1))
                  ((looking-back "\\(?:[[:word:]]\\|\s\\)\\*\\{2\\}"
                                 (max (- (point) 3) (point-min)))
                   (backward-delete-char 1))))
                ((and (or "_" "*") (guard (not in-src-block)))
                 (when (save-match-data
                         (save-excursion
                           (backward-char 2)
                           (or
                            (looking-at
                             "[^[:space:][:punct:]\n]\\(?:_\\|\\*\\)\\(?:[[:space:][:punct:]]\\|$\\)")
                            (looking-at
                             "\\(?:[[:space:][:punct:]]\\)\\(?:_\\|\\*\\)\\([^[:space:][:punct:]]\\|$\\)"))))
                   (backward-delete-char 1)
                   (insert "/"))))))
          (if noop-p
              (buffer-substring (point) start-pt)
            (prog1 (buffer-substring (point) (point-max))
                   (set-marker start-pt (point-max)))))))))

(provide 'gptel)
;;; gptel.el ends here
