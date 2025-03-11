;;; gptel-openai.el ---  ChatGPT suppport for gptel  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Karthik Chikmagalur

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
(require 'map)

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
(declare-function gptel-context--collect-media "gptel-context")
(declare-function gptel--base64-encode "gptel")
(declare-function gptel--trim-prefixes "gptel")
(declare-function gptel--parse-media-links "gptel")
(declare-function gptel--model-capable-p "gptel")
(declare-function gptel--model-name "gptel")
(declare-function gptel--get-api-key "gptel")
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
  curl-args)

;;; OpenAI (ChatGPT)
(cl-defstruct (gptel-openai (:constructor gptel--make-openai)
                            (:copier nil)
                            (:include gptel-backend)))

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
;; call spec in :tool-use and reset it.  Finally we append the tool calls to the
;; (INFO -> :data -> :messages) list of prompts.

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
                (when-let* ((tool-use (plist-get info :tool-use))
                            (args (apply #'concat (nreverse (plist-get info :partial_json))))
                            (func (plist-get (car tool-use) :function)))
                  (plist-put func :arguments args) ;Update arguments for last recorded tool
                  (gptel--inject-prompt
                   (plist-get info :backend) (plist-get info :data)
                   `(:role "assistant" :content :null :tool_calls ,(vconcat tool-use))) ; :refusal :null
                  (cl-loop
                   for tool-call in tool-use ; Construct the call specs for running the function calls
                   for spec = (plist-get tool-call :function)
                   collect (list :id (gptel--openai-unformat-tool-id (plist-get tool-call :id))
                                 :name (plist-get spec :name)
                                 :args (ignore-errors (gptel--json-read-string
                                                       (plist-get spec :arguments))))
                   into call-specs
                   finally (plist-put info :tool-use call-specs)))
              (when-let* ((response (gptel--json-read))
                          (delta (map-nested-elt response '(:choices 0 :delta))))
                (if-let* ((content (plist-get delta :content))
                          ((not (eq content :null))))
                    (push content content-strs)
                  ;; No text content, so look for tool calls
                  (when-let* ((tool-call (map-nested-elt delta '(:tool_calls 0)))
                              (func (plist-get tool-call :function)))
                    (if (plist-get func :name) ;new tool block begins
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
                ;; MAYBE: Should this be moved to a dedicated Openrouter backend?
                (unless (or (eq (plist-get info :reasoning-block) 'done)
                            (not (plist-member delta :reasoning)))
                  (if-let* ((reasoning-chunk (plist-get delta :reasoning)) ;for openrouter
                            ((not (eq reasoning-chunk :null))))
                      (plist-put info :reasoning
                                 (concat (plist-get info :reasoning) reasoning-chunk))
                    ;; Done with reasoning if we get non-empty content
                    (if-let* ((c (plist-get delta :content))
                              ((not (or (eq c :null) (string-empty-p c)))))
                        (if (plist-member info :reasoning) ;Is this a reasoning model?
                            (plist-put info :reasoning-block t) ;End of streaming reasoning block
                          (plist-put info :reasoning-block 'done))))))))) ;Not using a reasoning model
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
    ;; OpenAI returns either non-blank text content or a tool call, not both
    (if (and content (not (or (eq content :null) (string-empty-p content))))
        (prog1 content
          (when-let* ((reasoning (plist-get message :reasoning)) ;look for reasoning blocks
                      ((and (stringp reasoning) (not (string-empty-p reasoning)))))
            (plist-put info :reasoning reasoning)))
      (prog1 nil                        ; Look for tool calls only if no content
        (when-let* ((tool-calls (plist-get message :tool_calls)))
          (gptel--inject-prompt    ; First add the tool call to the prompts list
           (plist-get info :backend) (plist-get info :data) message)
          (cl-loop         ;Then capture the tool call data for running the tool
           for tool-call across tool-calls ;replace ":arguments" with ":args"
           for call-spec = (copy-sequence (plist-get tool-call :function))
           do (ignore-errors (plist-put call-spec :args
                                        (gptel--json-read-string
                                         (plist-get call-spec :arguments))))
           (plist-put call-spec :arguments nil)
           (plist-put call-spec :id (gptel--openai-unformat-tool-id (plist-get tool-call :id)))
           collect call-spec into tool-use
           finally (plist-put info :tool-use tool-use)))))))

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
         (memq gptel-model '(o1 o1-preview o1-mini o3-mini o3))))
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
    ;; Merge request params with model and backend params.
    (gptel--merge-plists
     prompts-plist
     (gptel-backend-request-params gptel-backend)
     (gptel--model-request-params  gptel-model))))

;; NOTE: No `gptel--parse-tools' method required for gptel-openai, since this is
;; handled by its defgeneric implementation

(cl-defmethod gptel--parse-tool-results ((_backend gptel-openai) tool-use)
  "Return a prompt containing tool call results in TOOL-USE."
  ;; (declare (side-effect-free t))
  (mapcar
   (lambda (tool-call)
     (list
      :role "tool"
      :tool_call_id (gptel--openai-format-tool-id
                     (plist-get tool-call :id))
      :content (plist-get tool-call :result)))
   tool-use))

(defun gptel--openai-format-tool-id (tool-id)
  (format "call_%s" tool-id))

(defun gptel--openai-unformat-tool-id (tool-id)
  (or (and (string-match "call_\\(.+\\)" tool-id)
           (match-string 1 tool-id))
      (progn
        (message "Unexpected tool_call_id format: %s" tool-id)
        tool-id)))

;; NOTE: No `gptel--inject-prompt' method required for gptel-openai, since this
;; is handled by its defgeneric implementation

(cl-defmethod gptel--parse-list ((_backend gptel-openai) prompt-list)
  (cl-loop for text in prompt-list
           for role = t then (not role)
           if text collect
           (list :role (if role "user" "assistant") :content text)))

(cl-defmethod gptel--parse-buffer ((backend gptel-openai) &optional max-entries)
  (let ((prompts) (prev-pt (point))
        (include-media (and gptel-track-media
                            (or (gptel--model-capable-p 'media)
                                (gptel--model-capable-p 'url)))))
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
                          (arguments (gptel--json-encode (plist-get tool-call :args))))
                     (plist-put tool-call :id id)
                     (plist-put tool-call :result
                                (string-trim (buffer-substring-no-properties
                                              (point) prev-pt)))
                     (push (car (gptel--parse-tool-results backend (list tool-call)))
                           prompts)
                     (push (list :role "assistant"
                                 :tool_calls
                                 (vector (list :type "function"
                                               :id (gptel--openai-format-tool-id id)
                                               :function `( :name ,name
                                                            :arguments ,arguments))))
                           prompts))
                 ((end-of-file invalid-read-syntax)
                  (message (format "Could not parse tool-call %s on line %s"
                                   id (line-number-at-pos (point))))))))
            ('ignore)
            ('nil
             (and max-entries (cl-decf max-entries))
             (if include-media
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
   else if media
   collect
   `(:type "image_url"
     :image_url (:url ,(concat "data:" (plist-get part :mime)
                        ";base64," (gptel--base64-encode media))))
   into parts-array end and
   if (plist-get part :url)
   collect
   `(:type "image_url"
     :image_url (:url ,(plist-get part :url)))
   into parts-array
   finally return (vconcat parts-array)))

;; TODO: Does this need to be a generic function?
(cl-defmethod gptel--wrap-user-prompt ((_backend gptel-openai) prompts
                                       &optional inject-media)
  "Wrap the last user prompt in PROMPTS with the context string.

If INJECT-MEDIA is non-nil wrap it with base64-encoded media
files in the context."
  (if inject-media
      ;; Wrap the first user prompt with included media files/contexts
      (when-let* ((media-list (gptel-context--collect-media)))
        (cl-callf (lambda (current)
                    (vconcat
                     (gptel--openai-parse-multipart media-list)
                     (cl-typecase current
                       (string `((:type "text" :text ,current)))
                       (vector current)
                       (t current))))
            (plist-get (car prompts) :content)))
    ;; Wrap the last user prompt with included text contexts
    (cl-callf (lambda (current)
                (cl-etypecase current
                  (string (gptel-context--wrap current))
                  (vector (if-let* ((wrapped (gptel-context--wrap nil)))
                              (vconcat `((:type "text" :text ,wrapped))
                                       current)
                            current))))
        (plist-get (car (last prompts)) :content))))

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
`gptel--openai-models'. An example of a model specification
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
