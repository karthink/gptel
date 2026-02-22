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
                            (:include gptel-backend))
  (responses-endpoint nil :documentation
   "Endpoint for Responses API, e.g. \"/v1/responses\".
If non-nil and the model has the `responses-api' capability,
this endpoint is used instead of the chat completions endpoint.")
  (builtin-tools nil :documentation
   "List of server-side tool configurations for the Responses API.
Each element is a plist like (:type \"web_search\") or
\(:type \"code_interpreter\" :container (:type \"auto\")).
Supported types: web_search, code_interpreter, file_search."))

(defsubst gptel--openai-responses-api-p (backend)
  "Return non-nil if BACKEND should use the Responses API.
This is true when the backend has a responses-endpoint configured
and the current model has the `responses-api' capability."
  (and (gptel-openai-responses-endpoint backend)
       (gptel--model-capable-p 'responses-api)))

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

(cl-defmethod gptel-curl--parse-stream ((backend gptel-openai) info)
  "Parse an OpenAI API data stream.

Return the text response accumulated since the last call to this
function.  Additionally, mutate state INFO to add tool-use
information if the stream contains it.

Dispatches to Responses API or Chat Completions parser based on backend config."
  (if (gptel--openai-responses-api-p backend)
      (gptel--parse-stream-responses backend info)
    (gptel--parse-stream-chat-completions backend info)))

(defun gptel--parse-stream-chat-completions (_backend info)
  "Parse a Chat Completions API data stream with INFO.
Return the text response accumulated since the last call."
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

(defun gptel--parse-stream-responses (_backend info)
  "Parse a Responses API data stream with INFO.
Return the text response accumulated since the last call."
  (let ((content-strs))
    (condition-case nil
        (while (re-search-forward "^event: *\\(.+\\)" nil t)
          (let ((event-type (match-string 1)))
            (forward-line 1)
            (when (looking-at "data: *")
              (goto-char (match-end 0))
              (pcase event-type
                ;; Response completed
                ("response.completed"
                 (when-let* ((data (ignore-errors (gptel--json-read)))
                             (resp (plist-get data :response)))
                   (plist-put info :stop-reason (plist-get resp :status))
                   (plist-put info :output-tokens
                              (map-nested-elt resp '(:usage :output_tokens)))))
                ;; Text content delta
                ("response.output_text.delta"
                 (when-let* ((data (ignore-errors (gptel--json-read)))
                             (delta (plist-get data :delta))
                             ((not (string-empty-p delta))))
                   (push delta content-strs)))
                ;; Function call arguments delta
                ("response.function_call_arguments.delta"
                 (when-let* ((data (ignore-errors (gptel--json-read)))
                             (delta (plist-get data :delta)))
                   (plist-put info :partial_json
                              (cons delta (plist-get info :partial_json)))))
                ;; Function call completed (user-defined tools)
                ("response.output_item.done"
                 (when-let* ((data (ignore-errors (gptel--json-read)))
                             (item (plist-get data :item))
                             ((equal (plist-get item :type) "function_call")))
                   (let ((tool-call
                          (list :id (plist-get item :call_id)
                                :name (plist-get item :name)
                                :args (ignore-errors
                                        (gptel--json-read-string
                                         (plist-get item :arguments))))))
                     (plist-put info :tool-use
                                (cons tool-call (plist-get info :tool-use)))
                     (plist-put info :partial_json nil))))
                ;; Reasoning content
                ("response.reasoning_summary_text.delta"
                 (when-let* ((data (ignore-errors (gptel--json-read)))
                             (delta (plist-get data :delta)))
                   (plist-put info :reasoning
                              (concat (plist-get info :reasoning) delta))))
                ;; Web search completed (server-side tool)
                ("response.web_search_call.completed"
                 (push "\n[Web search completed]" content-strs))
                ;; Code interpreter output (server-side tool)
                ("response.code_interpreter_call.completed"
                 (when-let* ((data (ignore-errors (gptel--json-read)))
                             (item (plist-get data :item))
                             (results (plist-get item :results)))
                   (cl-loop
                    for result across results
                    if (equal (plist-get result :type) "logs")
                    do (push (format "\n```\n%s\n```" (plist-get result :logs))
                             content-strs))))
                ;; Done event - finalize tool calls
                ("response.done"
                 (when-let* ((tool-use (plist-get info :tool-use)))
                   ;; Inject tool calls into prompt data for continuation
                   (let* ((data (plist-get info :data))
                          (input (plist-get data :input)))
                     (plist-put data :input
                                (vconcat input
                                         (mapcar
                                          (lambda (tc)
                                            (list :type "function_call"
                                                  :call_id (plist-get tc :id)
                                                  :name (plist-get tc :name)
                                                  :arguments
                                                  (gptel--json-encode (plist-get tc :args))))
                                          tool-use))))))))))
      (error (goto-char (match-beginning 0))))
    (apply #'concat (nreverse content-strs))))

(cl-defmethod gptel--parse-response ((backend gptel-openai) response info)
  "Parse an OpenAI RESPONSE and return response text.
Dispatches to Responses API or Chat Completions parser based on response format."
  (if (plist-get response :output)      ; Responses API format has :output
      (gptel--parse-response-responses backend response info)
    (gptel--parse-response-chat-completions backend response info)))

(defun gptel--parse-response-chat-completions (_backend response info)
  "Parse a Chat Completions API RESPONSE and return response text.
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

(defun gptel--parse-response-responses (_backend response info)
  "Parse an OpenAI Responses API RESPONSE and return response text.
Mutate state INFO with response metadata."
  (let ((output-items (plist-get response :output))
        (content-strs)
        (tool-calls))
    ;; Store usage info
    (plist-put info :output-tokens
               (map-nested-elt response '(:usage :output_tokens)))
    (plist-put info :stop-reason (plist-get response :status))
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
           do (push (format "[Refused: %s]" (plist-get part :refusal)) content-strs))))
       ;; Function call from model (user-defined tools)
       ("function_call"
        (push (list :id (plist-get item :call_id)
                    :name (plist-get item :name)
                    :args (ignore-errors
                            (gptel--json-read-string
                             (plist-get item :arguments))))
              tool-calls))
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
           do (push (format "\n```\n%s\n```" (plist-get result :logs)) content-strs))))
       ;; File search results (server-side tool)
       ("file_search_call"
        (when-let* ((status (plist-get item :status))
                    ((equal status "completed"))
                    (results (plist-get item :results)))
          (push (format "\n[File search: %d results]" (length results)) content-strs)))
       ;; Reasoning summary
       ("reasoning"
        (when-let* ((summary (plist-get item :summary)))
          (cl-loop for s across summary
                   do (plist-put info :reasoning
                                 (concat (plist-get info :reasoning)
                                         (plist-get s :text))))))))
    ;; Store tool calls for user-defined function tools
    (when tool-calls
      (plist-put info :tool-use (nreverse tool-calls))
      ;; Inject into prompts for conversation continuity
      (let* ((data (plist-get info :data))
             (input (plist-get data :input)))
        (plist-put data :input
                   (vconcat input
                            (mapcar (lambda (tc)
                                      (list :type "function_call"
                                            :call_id (plist-get tc :id)
                                            :name (plist-get tc :name)
                                            :arguments (gptel--json-encode
                                                        (plist-get tc :args))))
                                    (plist-get info :tool-use))))))
    ;; Return concatenated content
    (when content-strs
      (apply #'concat (nreverse content-strs)))))

(cl-defmethod gptel--request-data ((backend gptel-openai) prompts)
  "JSON encode PROMPTS for sending to OpenAI.
Dispatches to Responses API or Chat Completions based on model capability."
  (if (gptel--openai-responses-api-p backend)
      (gptel--request-data-responses backend prompts)
    (gptel--request-data-chat-completions backend prompts)))

(defun gptel--request-data-chat-completions (backend prompts)
  "JSON encode PROMPTS for OpenAI Chat Completions API.
BACKEND is the gptel-openai backend."
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

(defun gptel--request-data-responses (backend prompts)
  "JSON encode PROMPTS for OpenAI Responses API.
BACKEND is the gptel-openai backend."
  (let* ((input-items (gptel--convert-prompts-to-response-items prompts))
         (prompts-plist
          `(:model ,(gptel--model-name gptel-model)
            :input ,input-items
            ;; Stateless: don't store responses server-side, don't use
            ;; previous_response_id. Each request contains full context.
            :store :json-false
            :stream ,(or gptel-stream :json-false)))
         (reasoning-model-p
          (memq gptel-model '(o1 o1-preview o1-mini o3-mini o3 o4-mini
                                gpt-5 gpt-5-mini gpt-5-nano gpt-5.1 gpt-5.2))))
    ;; System message becomes instructions
    (when gptel--system-message
      (plist-put prompts-plist :instructions gptel--system-message))
    ;; Temperature
    (when (and gptel-temperature (not reasoning-model-p))
      (plist-put prompts-plist :temperature gptel-temperature))
    ;; Max tokens
    (when gptel-max-tokens
      (plist-put prompts-plist :max_output_tokens gptel-max-tokens))
    ;; Tools: merge builtin server-side tools with user function tools
    (when gptel-use-tools
      (let ((tools-array
             (vconcat
              ;; Built-in server-side tools from backend config
              (gptel--format-openai-builtin-tools
               (gptel-openai-builtin-tools backend))
              ;; User-defined function tools - convert to Responses API format
              (when gptel-tools
                (gptel--convert-tools-to-responses-format
                 (gptel--parse-tools backend gptel-tools))))))
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

(defun gptel--convert-prompts-to-response-items (prompts)
  "Convert Chat Completions PROMPTS format to Responses API input items.
System messages are filtered out (handled via :instructions)."
  (vconcat
   (delq nil
         (mapcar
          (lambda (msg)
            (let ((role (plist-get msg :role))
                  (content (plist-get msg :content))
                  (tool-calls (plist-get msg :tool_calls)))
              (cond
               ;; System messages handled separately via :instructions
               ((equal role "system") nil)
               ;; Tool result message
               ((equal role "tool")
                (list :type "function_call_output"
                      :call_id (plist-get msg :tool_call_id)
                      :output (or content "")))
               ;; Assistant message with tool calls
               (tool-calls
                (list :type "function_call"
                      :call_id (plist-get (aref tool-calls 0) :id)
                      :name (map-nested-elt tool-calls '(0 :function :name))
                      :arguments (or (map-nested-elt tool-calls '(0 :function :arguments)) "")))
               ;; User message
               ((equal role "user")
                (list :role "user"
                      :content (gptel--wrap-content-response-items content "input_text")))
               ;; Assistant message
               ((equal role "assistant")
                (list :role "assistant"
                      :content (gptel--wrap-content-response-items content "output_text")))
               ;; Pass through unknown formats
               (t msg))))
          prompts))))

(defun gptel--wrap-content-response-items (content item-type)
  "Wrap CONTENT as Responses API content items of ITEM-TYPE.
ITEM-TYPE should be \"input_text\" for user messages or
\"output_text\" for assistant messages."
  (cl-flet ((convert-part (part)
              (pcase (plist-get part :type)
                ("text" (list :type item-type :text (plist-get part :text)))
                ("image_url"
                 (list :type "input_image"
                       :image_url (plist-get (plist-get part :image_url) :url)))
                (_ part))))
    (cond
     ((stringp content)
      (vector (list :type item-type :text content)))
     ((or (vectorp content)
          (and (listp content) (plist-get (car-safe content) :type)))
      (vconcat (mapcar #'convert-part content)))
     (t (vector (list :type item-type :text (format "%s" content)))))))

(defun gptel--format-openai-builtin-tools (builtin-tools)
  "Format BUILTIN-TOOLS for the OpenAI Responses API.
BUILTIN-TOOLS is a list of plists specifying server-side tools.
Supported types: web_search, code_interpreter, file_search.
These tools are executed by OpenAI's servers."
  (when builtin-tools
    (vconcat
     (mapcar
      (lambda (tool-spec)
        (let ((tool-type (plist-get tool-spec :type)))
          (pcase tool-type
            ("web_search"
             (let ((result (list :type "web_search_preview")))
               (when-let* ((size (plist-get tool-spec :search_context_size)))
                 (plist-put result :search_context_size size))
               (when-let* ((loc (plist-get tool-spec :user_location)))
                 (plist-put result :user_location loc))
               result))
            ("code_interpreter"
             (let ((result (list :type "code_interpreter")))
               (when-let* ((container (plist-get tool-spec :container)))
                 (plist-put result :container container))
               result))
            ("file_search"
             (let ((result (list :type "file_search")))
               (when-let* ((vs-ids (plist-get tool-spec :vector_store_ids)))
                 (plist-put result :vector_store_ids vs-ids))
               (when-let* ((max-results (plist-get tool-spec :max_num_results)))
                 (plist-put result :max_num_results max-results))
               result))
            ;; Pass through unknown types
            (_ tool-spec))))
      builtin-tools))))

(defun gptel--convert-tools-to-responses-format (tools)
  "Convert TOOLS from Chat Completions to Responses API format.
Chat Completions: {:type \"function\" :function {:name ... :parameters ...}}.
Responses API: {:type \"function\" :name ... :parameters ...}."
  (vconcat
   (mapcar
    (lambda (tool)
      (pcase (plist-get tool :type)
        ("function"
         (let ((func (plist-get tool :function)))
           (list :type "function"
                 :name (plist-get func :name)
                 :description (plist-get func :description)
                 :parameters (plist-get func :parameters))))
        (_ tool)))                      ;Pass through non-function tools
    tools)))

(cl-defmethod gptel--parse-schema ((_backend gptel-openai) schema)
  (list :type "json_schema"
        :json_schema
        (list :name (md5 (format "%s" (random)))
              :schema (gptel--preprocess-schema
                       (gptel--dispatch-schema-type schema))
              :strict t)))

;; NOTE: No `gptel--parse-tools' method required for gptel-openai, since this is
;; handled by its defgeneric implementation

(cl-defmethod gptel--parse-tool-results ((backend gptel-openai) tool-use)
  "Return a prompt containing tool call results in TOOL-USE.
Dispatches to Responses API or Chat Completions format."
  (if (gptel--openai-responses-api-p backend)
      (gptel--parse-tool-results-responses tool-use)
    (gptel--parse-tool-results-chat-completions tool-use)))

(defun gptel--parse-tool-results-chat-completions (tool-use)
  "Format TOOL-USE results for Chat Completions API."
  (mapcar
   (lambda (tool-call)
     (list
      :role "tool"
      :tool_call_id (plist-get tool-call :id)
      :content (plist-get tool-call :result)))
   tool-use))

(defun gptel--parse-tool-results-responses (tool-use)
  "Format TOOL-USE results for Responses API."
  (mapcar
   (lambda (tool-call)
     (list
      :type "function_call_output"
      :call_id (plist-get tool-call :id)
      :output (plist-get tool-call :result)))
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
          (endpoint "/v1/chat/completions")
          responses-endpoint
          builtin-tools)
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
   :capabilities (media tool json url responses-api)
   :mime-types
   (\"image/jpeg\" \"image/png\" \"image/gif\" \"image/webp\")))

STREAM is a boolean to toggle streaming responses, defaults to
false.

PROTOCOL (optional) specifies the protocol, https by default.

ENDPOINT (optional) is the API endpoint for completions, defaults to
\"/v1/chat/completions\".

RESPONSES-ENDPOINT (optional) is the API endpoint for the Responses
API, e.g. \"/v1/responses\".  If provided and the model has the
`responses-api' capability, this endpoint will be used instead of
ENDPOINT for enhanced features like built-in server-side tools.

BUILTIN-TOOLS (optional) is a list of server-side tool configurations
for the Responses API.  Each element is a plist specifying a tool:
  (:type \"web_search\")
  (:type \"code_interpreter\" :container (:type \"auto\"))
  (:type \"file_search\" :vector_store_ids [\"vs_xxx\"])
These tools are executed by OpenAI's servers, not locally.

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
                  :responses-endpoint responses-endpoint
                  :builtin-tools builtin-tools
                  :url (cl-flet ((make-url (ep)
                                   (if protocol
                                       (concat protocol "://" host ep)
                                     (concat host ep))))
                         (if responses-endpoint
                             (let ((base-url (make-url endpoint))
                                   (resp-url (make-url responses-endpoint)))
                               (lambda ()
                                 (if (gptel--model-capable-p 'responses-api)
                                     resp-url
                                   base-url)))
                           (make-url endpoint))))))
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
