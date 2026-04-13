;;; gptel-openai-responses.el --- Responses API support for gptel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Keywords: comm

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
(require 'cl-generic)
(require 'cl-lib)
(require 'map)
(require 'gptel-request)
(require 'gptel-openai)

(defvar gptel-mode)
(declare-function gptel-context--collect-media "gptel-context")

;;; OpenAI Responses
(defun gptel--openai-responses-update-tokens (usage info)
  "Update token usage information from USAGE.
USAGE is part of the response, INFO is the request plist."
  (when usage
    (let* ((tokens (plist-get info :tokens))
           (input (+ (or (plist-get usage :input_tokens) 0)
                     (or (plist-get tokens :input) 0)))
           (output (+ (or (plist-get usage :output_tokens) 0)
                      (or (plist-get tokens :output) 0)))
           (cached (+ (or (map-nested-elt
                           usage '(:input_tokens_details :cached_tokens))
                          0)
                      (or (plist-get tokens :cached) 0))))
      (list :input input :output output :cached cached))))

(cl-defmethod gptel-curl--parse-stream ((_backend gptel-openai-responses) info)
  "Parse an OpenAI Responses API data stream.

Return the text response accumulated since the last call to this
function.  Additionally, mutate state INFO to add tool-use
information if the stream contains it."
  (let ((content-strs) wait)
    (condition-case nil
        (while (and (not wait) (re-search-forward "^event: *\\(.+\\)" nil t))
          (let ((event-type (match-string 1)) data)
            (forward-line 1)
            (if (not (looking-at "data:" t))
                (progn (goto-char (match-beginning 0)) ;not enough data, reset
                       (setq wait t))
              (forward-char 5)
              (setq data (gptel--json-read))
              (pcase event-type
                ;; Text content delta
                ("response.output_text.delta"
                 (when-let* ((delta (plist-get data :delta))
                             ((not (string-empty-p delta))))
                   (push delta content-strs)))
                ;; Function call arguments delta
                ("response.function_call_arguments.delta"
                 (when-let* ((delta (plist-get data :delta)))
                   (plist-put info :partial_json
                              (cons delta (plist-get info :partial_json)))))
                ;; Function call completed (user-defined tools)
                ("response.output_item.done"
                 (when-let* ((item (plist-get data :item))
                             ((equal (plist-get item :type) "function_call"))
                             (tool-call
                              (list :id (plist-get item :call_id)
                                    :name (plist-get item :name)
                                    :args (ignore-errors
                                            (gptel--json-read-string
                                             (plist-get item :arguments))))))
                   (plist-put info :tool-use
                              (cons tool-call (plist-get info :tool-use)))
                   (plist-put info :partial_json nil)))
                ;; Reasoning content
                ((or "response.reasoning_summary_text.delta"
                     "response.reasoning.delta")
                 (when-let* ((delta (plist-get data :delta)))
                   (plist-put info :reasoning
                              (concat (plist-get info :reasoning) delta))))
                ((or "response.reasoning_summary_text.done"
                     "response.reasoning.done")
                 (plist-put info :reasoning-block t))
                ;; NOTE: backend tools are not supported in gptel yet, this
                ;; parsing is for the future
                ;; Web search completed (server-side tool)
                ("response.web_search_call.completed"
                 (push "\n[Web search completed]" content-strs))
                ;; Code interpreter output (server-side tool)
                ("response.code_interpreter_call.completed"
                 (when-let* ((item (plist-get data :item))
                             (results (plist-get item :results)))
                   (cl-loop
                    for result across results
                    if (equal (plist-get result :type) "logs")
                    do (push (format "\n```\n%s\n```" (plist-get result :logs))
                             content-strs))))
                ;; Response completed
                ("response.completed"
                 (when-let* ((tool-use (plist-get info :tool-use)))
                   ;; Inject tool calls into prompt data for continuation
                   ;; TODO(responses-api) Avoid re-encoding these tool calls,
                   ;; especially :arguments
                   (gptel--inject-prompt
                    (plist-get info :backend) (plist-get info :data)
                    (mapcar (lambda (tc)
                              (list :type "function_call"
                                    :call_id (plist-get tc :id)
                                    :name (plist-get tc :name)
                                    :arguments
                                    (gptel--json-encode (plist-get tc :args))))
                            tool-use)))
                 (when-let* ((resp (plist-get data :response)))
                   (plist-put info :stop-reason (plist-get resp :status))
                   (plist-put info :tokens (gptel--openai-responses-update-tokens
                                            (plist-get resp :usage) info))))))))
      (error (goto-char (match-beginning 0))))
    (apply #'concat (nreverse content-strs))))

(cl-defmethod gptel--parse-response ((_backend gptel-openai-responses) response info)
  "Parse an OpenAI Responses API RESPONSE and return response text.
Mutate state INFO with response metadata."
  (let ((output-items (plist-get response :output))
        (content-strs) (tool-use) (tool-calls))
    ;; Store usage info
    (plist-put info :stop-reason (plist-get response :status))
    (plist-put info :tokens (gptel--openai-responses-update-tokens
                             (plist-get response :usage) info))
    ;; Process output items
    (cl-loop
     for item across output-items
     for item-type = (plist-get item :type)
     do
     (pcase item-type
       ;; Text message output
       ("message"
        (when-let* ((content (plist-get item :content)))
          (cl-loop
           for part across content
           for part-type = (plist-get part :type)
           if (equal part-type "output_text")
           do (push (plist-get part :text) content-strs)
           else if (equal part-type "refusal")
           do (push (format "[Refused: %s]" (plist-get part :refusal))
                    content-strs))))
       ;; Function call from model (user-defined tools)
       ("function_call"
        (push item tool-calls)
        (push (list :id (plist-get item :call_id)
                    :name (plist-get item :name)
                    :args (ignore-errors
                            (gptel--json-read-string
                             (plist-get item :arguments))))
              tool-use))
       ;; Reasoning summary
       ("reasoning"
        (cl-loop with summary = (plist-get item :summary)
                 with content = (plist-get item :content)
                 for s across
                 (if (length= content 0) summary content)
                 collect (plist-get s :text) into reasoning
                 finally do
                 (plist-put info :reasoning (apply #'concat reasoning))))
       ;; Web search results (server-side tool)
       ("web_search_call"
        (when-let* ((status (plist-get item :status))
                    ((equal status "completed")))
          ;; Results are inline, just note that search was done
          (push "\n[Web search completed]" content-strs)))
       ;; Code interpreter output (server-side tool)
       ("code_interpreter_call"
        (when-let* ((status (plist-get item :status))
                    ((equal status "completed"))
                    (results (plist-get item :results)))
          (cl-loop
           for result across results
           for result-type = (plist-get result :type)
           if (equal result-type "logs")
           do (push (format "\n```\n%s\n```" (plist-get result :logs))
                    content-strs))))
       ;; File search results (server-side tool)
       ("file_search_call"
        (when-let* ((status (plist-get item :status))
                    ((equal status "completed"))
                    (results (plist-get item :results)))
          (push (format "\n[File search: %d results]" (length results))
                content-strs)))))
    ;; Store tool calls for user-defined function tools
    (when tool-use
      (plist-put info :tool-use (nreverse tool-use))
      ;; Inject into prompts for conversation continuity
      (gptel--inject-prompt
       (plist-get info :backend) (plist-get info :data)
       (nreverse tool-calls)))
    ;; Return concatenated content
    (when content-strs
      (apply #'concat (nreverse content-strs)))))

(cl-defmethod gptel--request-data ((backend gptel-openai-responses) prompts)
  "JSON encode PROMPTS for sending to OpenAI Responses API."
  (let ((prompts-plist
         `( :model ,(gptel--model-name gptel-model)
            :input ,(vconcat prompts)
            ;; Stateless: don't store responses server-side, don't use
            ;; previous_response_id. Each request contains full context.
            :store :json-false
            :stream ,(or gptel-stream :json-false)))
        (o-model-p (memq gptel-model '(o1 o1-preview o1-mini o3-mini o3 o4-mini))))
    ;; System message becomes instructions
    (when gptel--system-message
      (plist-put prompts-plist :instructions gptel--system-message))
    ;; Temperature
    (when (and gptel-temperature (not o-model-p))
      (plist-put prompts-plist :temperature gptel-temperature))
    ;; Max tokens
    (when gptel-max-tokens
      (plist-put prompts-plist :max_output_tokens gptel-max-tokens))
    (when gptel-use-tools
      (let ((tools-array
             (vconcat
              (when gptel-tools
                (gptel--parse-tools backend gptel-tools)))))
        (when (> (length tools-array) 0)
          (plist-put prompts-plist :tools tools-array))
        (when (eq gptel-use-tools 'force)
          (plist-put prompts-plist :tool_choice "required"))))
    ;; Structured output via text format
    (when gptel--schema
      (plist-put prompts-plist :text
                 (list :format
                       (list :type "json_schema"
                             :name (md5 (format "%s" (random)))
                             :schema (gptel--preprocess-schema
                                      (gptel--dispatch-schema-type gptel--schema))
                             :strict t))))
    ;; Merge request params
    (gptel--merge-plists
     prompts-plist
     gptel--request-params
     (gptel-backend-request-params gptel-backend)
     (gptel--model-request-params gptel-model))))

;; Helper functions for Responses API format conversion

(cl-defmethod gptel--parse-schema ((_backend gptel-openai-responses) schema)
  "Parse SCHEMA for Responses API structured output.
In Responses API, the schema format uses text.format instead of response_format."
  (list :type "json_schema"
        :name (md5 (format "%s" (random)))
        :schema (gptel--preprocess-schema
                 (gptel--dispatch-schema-type schema))
        :strict t))

(cl-defmethod gptel--parse-tools ((_backend gptel-openai-responses) tools)
  "Parse TOOLS and return a list of prompts.

TOOLS is a list of `gptel-tool' structs, which see.

_BACKEND is the LLM backend in use.  This is the default
implementation, used by OpenAI-compatible APIs and Ollama."
  (vconcat
   (mapcar
    (lambda (tool)
      (nconc
       (list
        :type "function"
        :name (gptel-tool-name tool)
        :description (gptel-tool-description tool))
       (if (gptel-tool-args tool)
           (list
            :parameters
            (list :type "object"
                  ;; gptel's tool args spec is close to the JSON schema, except
                  ;; that we use (:name "argname" ...)
                  ;; instead of  (:argname (...)), and
                  ;; (:optional t) for each arg instead of (:required [...])
                  ;; for all args at once.  Handle this difference by
                  ;; modifying a copy of the gptel tool arg spec.
                  :properties
                  (cl-loop
                   for arg in (gptel-tool-args tool)
                   for argspec = (copy-sequence arg)
                   for name = (plist-get arg :name) ;handled differently
                   for newname = (or (and (keywordp name) name)
                                     (make-symbol (concat ":" name)))
                   do                  ;ARGSPEC is ARG without unrecognized keys
                   (cl-remf argspec :name)
                   (cl-remf argspec :optional)
                   if (equal (plist-get arg :type) "object")
                   do (unless (plist-member argspec :required)
                        (plist-put argspec :required []))
                   (plist-put argspec :additionalProperties :json-false)
                   append (list newname argspec))
                  :required
                  (vconcat
                   (delq nil (mapcar
                              (lambda (arg) (and (not (plist-get arg :optional))
                                            (plist-get arg :name)))
                              (gptel-tool-args tool))))
                  :additionalProperties :json-false))
         (list :parameters (list :type "object" :properties nil)))))
    (ensure-list tools))))

(cl-defmethod gptel--inject-tool-call ((_backend gptel-openai-responses) data tool-call new-call)
  "Replace TOOL-CALL in query DATA with NEW-CALL.

BACKEND is the `gptel-backend'.  See the generic function documentation
for details.  This implementation handles the OpenAI Responses API."
  (if-let* ((input (plist-get data :input))
            (indexed-call
             (cl-loop for item across input
                      for i upfrom 0
                      if (and (equal (plist-get item :type) "function_call")
                              (equal (plist-get item :call_id) (plist-get tool-call :id)))
                      return (cons i item)))
            (index (car indexed-call))
            (call (cdr indexed-call)))
      (if (null new-call)               ;delete tool call if new-call is nil
          (plist-put data :input (vconcat (substring input 0 index)
                                          (substring input (1+ index))))
        (progn
          (when-let* ((args (plist-get new-call :args)))
            (plist-put call :arguments (gptel--json-encode args)))
          (when-let* ((name (plist-get new-call :name)))
            (plist-put call :name name))))
    (display-warning
     '(gptel tool-call)
     (format "Could not inject updated tool-call arguments for tool call %s, %s"
             (plist-get tool-call :name)
             (truncate-string-to-width (prin1-to-string new-call) 50 nil nil t)))))

(cl-defmethod gptel--parse-tool-results ((_backend gptel-openai-responses) tool-use)
  "Format TOOL-USE results for Responses API.
Returns prompts in Responses API format with function_call_output items."
  (mapcar
   (lambda (tool-call)
     (list
      :type "function_call_output"
      :call_id (plist-get tool-call :id)
      :output (plist-get tool-call :result)))
   tool-use))

(cl-defmethod gptel--inject-prompt
  ((_backend gptel-openai-responses) data new-prompt &optional position)
  "Inject NEW-PROMPT into existing prompts in query DATA.

NEW-PROMPT can be a single message or a list of messages.

If POSITION is
- nil, append NEW-PROMPT at the end of DATA
- a non-negative integer, insert it at that position in DATA.
- a negative integer, insert it there counting from the end."
  (when (keywordp (car-safe new-prompt)) ;Is new-prompt one or many?
    (setq new-prompt (list new-prompt)))
  (let ((prompts (plist-get data :input)))
    (pcase position
      ('nil (plist-put data :input (vconcat prompts new-prompt)))
      ((pred integerp)
       (when (< position 0) (setq position (+ (length prompts) position)))
       (plist-put data :input (vconcat (substring prompts 0 position)
                                       new-prompt
                                       (substring prompts position)))))))

(cl-defmethod gptel--parse-list ((backend gptel-openai-responses) prompt-list)
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
              (list :type "function_call"
                    :call_id (plist-get call :id)
                    :name (plist-get call :name)
                    :arguments (decode-coding-string
                                (gptel--json-encode (plist-get call :args))
                                'utf-8 t))
              full-prompt)
             (push (car (gptel--parse-tool-results backend (list (cdr entry)))) full-prompt))))
        (nreverse full-prompt))
    (cl-loop for text in prompt-list    ; Simple format, list of strings
             for role = t then (not role)
             if text collect
             (list :role (if role "user" "assistant") :content text))))

(cl-defmethod gptel--parse-buffer ((backend gptel-openai-responses) &optional max-entries)
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
                     (push (list :type "function_call"
                                 :call_id id
                                 :name name
                                 :arguments arguments)
                           prompts))
                 ((end-of-file invalid-read-syntax)
                  (message (format "Could not parse tool-call %s on line %s"
                                   id (line-number-at-pos (point))))))))
            ('ignore)
            ('nil
             (and max-entries (cl-decf max-entries))
             (if gptel-track-media
                 (when-let* ((content (gptel--openai-responses-parse-multipart
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

(defun gptel--openai-responses-parse-multipart (parts)
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
   collect `(:type "input_text" :text ,text) into parts-array end
   else if media collect
   `(:type "input_image"
           :image_url ,(concat "data:" (plist-get part :mime)
                               ";base64," (gptel--base64-encode media)))
   into parts-array
   else if (plist-get part :textfile) collect
   `(:type "input_text"
           :text ,(with-temp-buffer
                    (gptel--insert-file-string (plist-get part :textfile))
                    (buffer-string)))
   into parts-array end and
   if (plist-get part :url)
   collect
   `(:type "input_image"
           :image_url ,(plist-get part :url))
   into parts-array
   finally return (vconcat parts-array)))

(cl-defmethod gptel--inject-media ((_backend gptel-openai-responses) prompts)
  "Wrap the first user prompt in PROMPTS with included media files.

Media files, if present, are placed in `gptel-context'."
  (when-let* ((media-list (gptel-context--collect-media)))
    (cl-callf (lambda (current)
                (vconcat
                 (gptel--openai-responses-parse-multipart media-list)
                 (cl-typecase current
                   (string `((:type "input_text" :text ,current)))
                   (vector current)
                   (t current))))
        (plist-get (car prompts) :content))))

;;;###autoload
(cl-defun gptel-make-openai-responses
    (name &key curl-args (models gptel--openai-models)
          stream key request-params
          (header
           (lambda (_info) (when-let* ((key (gptel--get-api-key)))
                        `(("Authorization" . ,(concat "Bearer " key))))))
          (host "api.openai.com")
          (protocol "https")
          (endpoint "/v1/responses"))
  "Register an OpenAI Responses API backend for gptel with NAME.

The Responses API is OpenAI's new API for agentic applications that
provides built-in tools like web search, code interpreter, and file
search.

Keyword arguments:

CURL-ARGS (optional) is a list of additional Curl arguments.

HOST (optional) is the API host, typically \"api.openai.com\".

MODELS is a list of available model names, as symbols.
Additionally, you can specify supported LLM capabilities like
vision or tool-use by appending a plist to the model with more
information, in the form

 (model-name . plist)

For a list of currently recognized plist keys, see
`gptel--openai-models'.

STREAM is a boolean to toggle streaming responses, defaults to
false.

PROTOCOL (optional) specifies the protocol, https by default.

ENDPOINT (optional) is the API endpoint for completions, defaults to
\"/v1/responses\".

HEADER (optional) is for additional headers to send with each
request.  It should be an alist or a function that returns an
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

 (gptel-make-openai-responses
  \"OpenAI-Responses\"
  :stream t
  :models '((gpt-4o
             :capabilities (media tool-use json url responses-api)
             :mime-types (\"image/jpeg\" \"image/png\" \"image/gif\" \"image/webp\"))
            (gpt-4o-mini
             :capabilities (media tool-use json url responses-api)
             :mime-types (\"image/jpeg\" \"image/png\" \"image/gif\" \"image/webp\"))))"
  (declare (indent 1))
  (let ((backend (gptel--make-openai-responses
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

(provide 'gptel-openai-responses)
;;; gptel-openai-responses.el ends here

;; Local Variables:
;; byte-compile-warnings: (not docstrings)
;; End:
