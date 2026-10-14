;;; gptel-mistral.el ---  Mistral suppport for gptel  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026  Karthik Chikmagalur
;; Copyright (C) 2026  Andrei Mochalov

;; Author: Andrei Mochalov <factyy@gmail.com>

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

;; This file adds support for the Mistral API to gptel (handles the differences between Mistral API and ChatGPT API)

;;; Code:
(require 'cl-generic)
(eval-when-compile (require 'cl-lib))
(require 'map)
(require 'gptel-openai)

(defvar json-object-type)
(defvar gptel-mode)
(declare-function gptel-context--collect-media "gptel-context")
(declare-function json-read "json")
(declare-function gptel-context--wrap "gptel-context")

;; Mistral Completions Backend
(cl-defstruct (gptel-mistral (:constructor gptel--make-mistral)
                            (:copier nil)
                            (:include gptel-openai)))

(defun gptel-mistral--log-dev (format-string &rest args)
  (if (eq gptel-log-level 'debug)
      (with-current-buffer (get-buffer-create "*gptel-dev*")
        (goto-char (point-max))
        (not (insert (apply #'format format-string args))))
    't))

(defun gptel-mistral--extract-text-content (delta)
  (if-let* ((content (plist-get delta :content))
            (content (if (vectorp content)
                         (progn
                           (gptel-mistral--log-dev "trying to find regular text in the content, result: %S\n\n" (seq-find (lambda (content-chunk) (eq (plist-get content-chunk :type) "text")) content))
                           (when-let ((text-chunk (seq-find (lambda (content-chunk)
                                                              (progn (gptel-mistral--log-dev "looking at the chunk: %S, its type: %S\n" content-chunk (plist-get content-chunk :type))
                                                                     (string= (plist-get content-chunk :type) "text")))
                                                            content)))
                             (plist-get text-chunk :text)))
                       content))
            ((gptel-mistral--log-dev "The text content is: %S\n" content))
            ((not (or (null content) (eq content :null) (and (stringp content) (string-empty-p content))))))
      content))


(defun gptel-mistral--extract-thinking-text-content (delta)
  (if-let* ((content (and (plist-member delta :content) (plist-get delta :content)))
            ((vectorp content))
            (thinking-chunk (seq-find (lambda (content-chunk)
                                        (progn (gptel-mistral--log-dev "looking at the chunk: %S, its type: %S, is thinking: %S\n" content-chunk (plist-get content-chunk :type) (string= (plist-get content-chunk :type) "thinking"))
                                               (string= (plist-get content-chunk :type) "thinking")))
                                      content))
            ((gptel-mistral--log-dev "found thinking chunk: %S, plist member: %S, plist get: %S, and result: %S\n" thinking-chunk (plist-member thinking-chunk :thinking) (plist-get thinking-chunk :thinking) (and (plist-member thinking-chunk :thinking) (plist-get thinking-chunk :thinking))))
            (thinking-part (and (plist-member thinking-chunk :thinking) (plist-get thinking-chunk :thinking)))
            ((gptel-mistral--log-dev "found thinking part: %S\n" thinking-part))
            ((and (vectorp thinking-part) (> (length thinking-part) 0)))
            ((gptel-mistral--log-dev "checked thinking part: %S\n" thinking-part))
            (thinking-content (aref thinking-part 0))
            ((gptel-mistral--log-dev "thinking content: %S\n" thinking-content))
            (thinking-text (and (plist-member thinking-content :text) (plist-get thinking-content :text))))
      thinking-text))


(defun gptel-mistral--extract-tool-content (delta)
  (map-nested-elt delta '(:tool_calls 0)))



;; Mistral Responses Backend
;; (cl-defstruct (gptel-mistral-responses (:constructor gptel--make-mistral-responses)
;;                                       (:copier nil)
;;                                       (:include gptel-openai-responses)))

;; How the following function works:
;;
;; The Mistral API (OpenAI compatible but default implementation failing with 3rd party models)
;; returns a stream of data chunks.  Each data chunk has a
;; component that can be parsed as JSON.  Besides metadata, each chunk has
;; either some text or part of a tool call.
;;
;; Finally we append any tool calls and accumulated reasoning text (from
;; :reasoning-chunks) to the (INFO -> :data -> :messages) list of prompts.

(cl-defmethod gptel-curl--parse-stream ((_backend gptel-mistral) info)
  "Parse a Mistral API data stream.

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
                  (gptel-mistral--log-dev "In the `DONE` handler...\n")
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
                  (when (eq (plist-get info :reasoning-block) 'done)
                    (plist-put info :reasoning-block nil))
                  ;; Update token usage if present
                  (when-let* ((last-resp (save-excursion
                                           (forward-line -1)
                                           (and (re-search-backward "^data:" nil t)
                                                (goto-char (match-end 0))
                                                (ignore-errors (gptel--json-read)))))
                              (usage (plist-get last-resp :usage)))
                    (gptel--openai-update-tokens usage info))
                  (when (plist-member info :reasoning-chunks) (plist-put info :reasoning-chunks nil)))
              (when-let* ((response (gptel--json-read))
                          (delta (map-nested-elt response '(:choices 0 :delta))))
                (gptel-mistral--log-dev "\n\nresponse: %S, delta: %S, content: %S, content is a vector: %S\n\n" response delta (plist-get delta :content) (vectorp (plist-get delta :content)))
                (let ((text-content (gptel-mistral--extract-text-content delta))
                      (thinking-text-content (gptel-mistral--extract-thinking-text-content delta))
                      (tool-content (gptel-mistral--extract-tool-content delta)))
                  (if-let* ((content text-content))
                      (progn
                        (gptel-mistral--log-dev "adding content: %S\n\n" content)
                        (push content content-strs))
                    ;; No text content, so look for tool calls
                    (gptel-mistral--log-dev "probable tool chunk: %S\ndelta: %S\n\n" response delta)
                    (when-let* ((tool-call tool-content)
                                (func (plist-get tool-call :function))
                                (tool-index (plist-get tool-call :index)))
                      (gptel-mistral--log-dev "tool call: %S\nfunc: %S\nindex: %S\n" tool-call func tool-index)
                      (gptel-mistral--log-dev "prev tool call: %S\n" (car (plist-get info :tool-use)))
                      (if (and-let* ((func-name (plist-get func :name))
                                     ((not (eq func-name :null))))
                            (not (string-empty-p func-name)))
                          (progn
                            (when-let* ((partial-json (plist-get info :partial_json)))
                              (let* ((prev-tool-call (car (plist-get info :tool-use)))
                                     (prev-tool-index (plist-get prev-tool-call :index))
                                     (prev-tool-func (plist-get prev-tool-call :function)))
                                (gptel-mistral--log-dev "partial json: %S\nprev tool call: %S\nprev func: %S\n" partial-json prev-tool-call prev-tool-func)
                                (gptel-mistral--log-dev "partial json concat: %S\n" (apply #'concat (nreverse (plist-get info :partial_json))))
                                (plist-put prev-tool-func :arguments ;update args for old tool block
                                           (apply #'concat (nreverse (plist-get info :partial_json))))
                                (gptel-mistral--log-dev "after patching arguments partial json prev function arguments: %S\n" (plist-get prev-tool-call :argument)))
                              (plist-put info :partial_json nil) ;clear out finished chain of partial args
                              (gptel-mistral--log-dev "nillified partial json: %S\n\n" (plist-get info :partial_json)))
                            ;; Start new chain of partial argument strings
                            (gptel-mistral--log-dev "starting a new function call, placing arguments: %S\n" (plist-get func :arguments))
                            (plist-put info :partial_json (list (plist-get func :arguments)))
                            (gptel-mistral--log-dev "after placing arguments: %S\n" (plist-get info :partial_json))
                            ;; NOTE: Do NOT use `push' for this, it prepends and we lose the reference
                            (plist-put info :tool-use (cons tool-call (plist-get info :tool-use))))
                        ;; old tool block continues, so continue collecting arguments in :partial_json
                        (let* ((prev-tool-call (car (plist-get info :tool-use)))
                               (prev-tool-index (plist-get prev-tool-call :index)))
                          (when (eq prev-tool-index tool-index)
                            (gptel-mistral--log-dev "pushing the partial json: %S into the arguments: %S\n\n" (plist-get info :partial_json) (plist-get func :arguments))
                            (push (plist-get func :arguments) (plist-get info :partial_json)))))))
                  ;; Check for reasoning blocks
                  (gptel-mistral--log-dev "before reasoning handler, delta: %S, reasoning: %S\n\n" delta (plist-get info :reasoning-block))
                  (unless (eq (plist-get info :reasoning-block) 'done)
                    (gptel-mistral--log-dev "reasoning handler, delta: %S\n\n" delta)
                    (and-let* ((reasoning-plist ;reasoning-plist is (:reasoning.* "chunk" ...) or nil
                                (or (plist-member delta :reasoning) ;for Openrouter and co
                                    (plist-member delta :reasoning_content)
                                    (when-let ((thinking-text thinking-text-content))
                                      (list :reasoning thinking-text))))
                               (reasoning-chunk (cadr reasoning-plist))
                               ((gptel-mistral--log-dev "reasoning chunk: %S\n\n" reasoning-chunk))
                               ((not (or (eq reasoning-chunk :null) (string-empty-p reasoning-chunk)))))
                      (progn (plist-put info :reasoning ;For stream filter consumption
                                        (concat (plist-get info :reasoning) reasoning-chunk))))
                    (gptel-mistral--log-dev "reasoning completion checks, general: %S, text content: %S\n" (plist-member info :reasoning) (gptel-mistral--extract-text-content delta))
                    ;; Done with reasoning if we get non-empty content
                    (when (plist-member info :reasoning)
                      (when (or (and (stringp text-content) (not (string-blank-p text-content))) ;; Started receiving text content?
                                (not (null tool-content)))                                       ;; Started receiving tool call content?
                        (plist-put info :reasoning-block t))))))))) ;Signal end of reasoning block
      (error (goto-char (match-beginning 0))))
    (apply #'concat (nreverse content-strs))))


(defconst gptel--mistral-models
  (gptel--process-models
   '((mistral-large-4-0
      :description "Mistral Large 4 is a state-of-the-art, open-weight, general-purpose multimodal model with a granular Mixture-of-Experts architecture. It features 52B active parameters and 1.05T total parameters, and a 1.6B vision encoder."
      :capabilities (media tool-use json url responses-api)
      :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
      :context-window 1024
      :input-cost 1.36
      :output-cost 4.18)
     (mistral-medium-3.5
      :description "Our frontier-class multimodal model optimized for agentic and coding use cases."
      :capabilities (media tool-use json url responses-api)
      :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
      :context-window 256
      :input-cost 1.50
      :output-cost 7.50)
     (mistral-large-2512
      :description "Mistral Large 3, is a state-of-the-art, open-weight, general-purpose multimodal model with a granular Mixture-of-Experts architecture. It features 41B active parameters and 675B total parameters."
      :capabilities (media tool-use json url responses-api)
      :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
      :context-window 256
      :input-cost 0.50
      :output-cost 1.50)
     (mistral-small-2603
      :description "Our powerful hybrid model unifying instruct, reasoning, and coding capabilities in a single model. 119B parameters with 6.5B active."
      :capabilities (media tool-use json url responses-api)
      :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
      :context-window 256
      :input-cost 0.15
      :output-cost 0.60)
     (zai-glm-5-3
      :description "A third-party open weight text model from Z.ai, hosted by Mistral for long-context coding and agentic workflows. The model is served without Mistral modifications."
      :capabilities (tool-use json url responses-api)
      :context-window 1024
      :input-cost 1.4
      :output-cost 4.4)
     (zai-glm-5-2
      :description "A third-party open weight text model from Z.ai, hosted by Mistral for long-context coding and agentic workflows. The model is served without Mistral modifications."
      :capabilities (tool-use json url responses-api)
      :context-window 1024
      :input-cost 1.4
      :output-cost 4.4)
     ))
  "List of available Mistral models and associated properties.

Each model symbol is associated with the following keys, all optional:

- `:description': a brief description of the model.

- `:capabilities':  a list of capabilities supporte responses-apid by the model.

- `:mime-types': a list of supported MIME types for media files.

- `:context-window': the context window size, in thousands of tokens.

- `:input-cost': the input cost, in US dollars per million tokens.

- `:output-cost': the output cost, in US dollars per million tokens.

- `:cutoff-date': the knowledge cutoff date.

- `:request-params': a plist of additional request parameters to
  include when using this model.

Information about the Mistral models was obtained from the following
sources:

- <https://docs.mistral.ai/models>")

;;;###autoload
(cl-defun gptel-make-mistral
    (name &key curl-args
          (models gptel--mistral-models)
          stream
          key
          request-params
          (header
           (lambda (_info)
             (when-let* ((key (gptel--get-api-key)))
               `(("Authorization" . ,(concat "Bearer " key))))))
          (host "api.mistral.ai")
          (protocol "https")
          (endpoint "/v1/chat/completions"))
  "Register a Mistral API (OpenAI)-compatible backend for gptel with NAME.

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
  (let* ((responses-api (string-match-p "api\\.mistral\\.ai" host))
         ;; Use the OpenAI Responses API if required
         ;; TODO: Find a more reliable way to dispatch.  Checking the host isn't
         ;; reliable.  For example, it won't work when using the Responses API
         ;; via a proxy.
         (constructor #'gptel--make-mistral)
         (endpoint endpoint)
         (backend (funcall constructor
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


(provide 'gptel-mistral)
;;; gptel-mistral.el ends here

;; Local Variables:
;; byte-compile-warnings: (not docstrings)
;; End:
