;;; gptel-gemini.el ---  Gemini suppport for gptel  -*- lexical-binding: t; -*-

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

;; This file adds support for the Gemini API to gptel

;;; Code:
(require 'gptel)
(require 'cl-generic)
(require 'map)
(eval-when-compile (require 'cl-lib))

(declare-function prop-match-value "text-property-search")
(declare-function text-property-search-backward "text-property-search")
(declare-function json-read "json")
(declare-function gptel-context--wrap "gptel-context")
(declare-function gptel-context--collect-media "gptel-context")
(defvar json-object-type)

;;; Gemini
(cl-defstruct
    (gptel-gemini (:constructor gptel--make-gemini)
                  (:copier nil)
                  (:include gptel-backend)))

;; TODO: Using alt=sse in the query url generates an OpenAI style streaming
;; response, with more immediate updates.  Maybe we should switch to that and
;; rewrite the stream parser?
(cl-defmethod gptel-curl--parse-stream ((_backend gptel-gemini) info)
  "Parse a Gemini data stream.

Return the text response accumulated since the last call to this
function.  Additionally, mutate state INFO to add tool-use
information if the stream contains it."
  (let* ((content-strs))
    (condition-case nil
        (while (prog1 (search-forward "{" nil t) ; while-let is Emacs 29.1+ only
                 (backward-char 1))
          (save-match-data
            (when-let* ((response (gptel--json-read))
                        (text (gptel--parse-response
                               (plist-get info :backend)
                               response info 'include)))
              (push text content-strs))))
      (error
       (goto-char (match-beginning 0))))
    (apply #'concat (nreverse content-strs))))

(cl-defmethod gptel--parse-response ((_backend gptel-gemini) response info
                                     &optional include-text)
  "Parse an Gemini (non-streaming) RESPONSE and return response text.

Mutate state INFO with response metadata.

If INCLUDE-TEXT is non-nil, include response text in the prompts
list."
  (let* ((cand0 (map-nested-elt response '(:candidates 0)))
         (parts (map-nested-elt cand0 '(:content :parts))))
    (plist-put info :stop-reason (plist-get cand0 :finishReason))
    (plist-put info :output-tokens
               (map-nested-elt
                response '(:usageMetadata :candidatesTokenCount)))
    (cl-loop
     for part across parts
     for tx = (plist-get part :text)
     if (and tx (not (eq tx :null))) collect tx into content-strs
     else if (plist-get part :functionCall)
     collect (copy-sequence it) into tool-use
     finally do                         ;Add text and tool-calls to prompts list
     (when (or tool-use include-text)
       (let* ((data (plist-get info :data))
              (prompts (plist-get data :contents))
              (last-prompt (aref prompts (1- (length prompts)))))
         (if (equal (plist-get last-prompt :role) "model")
             ;; When streaming, the last prompt may already have the role
             ;; "model" from prior calls to this function.  Append to its parts
             ;; instead of adding a new model role then.
             (plist-put last-prompt :parts
                        (vconcat (plist-get last-prompt :parts) parts))
           (plist-put                   ;otherwise create a new "model" role
            data :contents
            (vconcat prompts `((:role "model" :parts ,parts)))))))
     (when tool-use                    ;Capture tool call data for running tools
       (plist-put info :tool-use
                  (nconc (plist-get info :tool-use) tool-use)))
     finally return
     (and content-strs (apply #'concat content-strs)))))

(cl-defmethod gptel--request-data ((backend gptel-gemini) prompts)
  "JSON encode PROMPTS for sending to Gemini."
  (let ((prompts-plist
         `(:contents [,@prompts]
           :safetySettings [(:category "HARM_CATEGORY_HARASSMENT"
                             :threshold "BLOCK_NONE")
                            (:category "HARM_CATEGORY_SEXUALLY_EXPLICIT"
                             :threshold "BLOCK_NONE")
                            (:category "HARM_CATEGORY_DANGEROUS_CONTENT"
                             :threshold "BLOCK_NONE")
                            (:category "HARM_CATEGORY_HATE_SPEECH"
                             :threshold "BLOCK_NONE")]))
        params)
    (if gptel--system-message
        (plist-put prompts-plist :system_instruction
                   `(:parts (:text ,gptel--system-message))))
    (when gptel-use-tools
      (when (eq gptel-use-tools 'force)
        (plist-put prompts-plist :tool_config
                   '(:function_calling_config (:mode "ANY"))))
      (when gptel-tools
        (plist-put prompts-plist :tools
                   (gptel--parse-tools backend gptel-tools))))
    (when gptel-temperature
      (setq params
            (plist-put params
                       :temperature (max gptel-temperature 1.0))))
    (when gptel-max-tokens
      (setq params
            (plist-put params
                       :maxOutputTokens gptel-max-tokens)))
    (when params
      (plist-put prompts-plist
                 :generationConfig params))
    ;; Merge request params with model and backend params.
    (gptel--merge-plists
     prompts-plist
     (gptel-backend-request-params gptel-backend)
     (gptel--model-request-params  gptel-model))))

(cl-defmethod gptel--parse-tools ((_backend gptel-gemini) tools)
  "Parse TOOLS to the Gemini API tool definition spec.

TOOLS is a list of `gptel-tool' structs, which see."
  (cl-loop
   for tool in (ensure-list tools)
   collect
   (list
    :name (gptel-tool-name tool)
    :description (gptel-tool-description tool)
    :parameters
    (if (not (gptel-tool-args tool))
         :null           ;NOTE: Gemini wants :null if the function takes no args
      (list :type "object"
            :properties
            (cl-loop
             for arg in (gptel-tool-args tool)
             for name = (plist-get arg :name)
             for type = (plist-get arg :type)
             for newname = (or (and (keywordp name) name)
                               (make-symbol (concat ":" name)))
             for enum = (plist-get arg :enum)
             append (list newname
                          `(:type ,(plist-get arg :type)
                            :description ,(plist-get arg :description)
                            ,@(if enum (list :enum (vconcat enum)))
                            ,@(cond
                               ((equal type "object")
                                (list :parameters (plist-get arg :parameters)))
                               ((equal type "array")
                                (list :items (plist-get arg :items)))))))
            :required
            (vconcat
             (delq nil (mapcar
                        (lambda (arg) (and (not (plist-get arg :optional))
                                      (plist-get arg :name)))
                        (gptel-tool-args tool)))))))
   into tool-specs
   finally return `[(:function_declarations ,(vconcat tool-specs))]))

(cl-defmethod gptel--parse-tool-results ((_backend gptel-gemini) tool-use)
  "Return a prompt containing tool call results in TOOL-USE."
  (list
   :role "user"
   :parts
   (vconcat
    (mapcar
     (lambda (tool-call)
       (let ((result (plist-get tool-call :result))
             (name (plist-get tool-call :name)))
         `(:functionResponse
           (:name ,name :response
            (:name ,name :content ,result)))))
     tool-use))))

(cl-defmethod gptel--inject-prompt ((_backend gptel-gemini) data new-prompt &optional _position)
  "Append NEW-PROMPT to existing prompts in query DATA.

See generic implementation for full documentation."
  (let ((prompts (plist-get data :contents)))
    (plist-put data :contents (vconcat prompts (list new-prompt)))))

(cl-defmethod gptel--parse-list ((_backend gptel-gemini) prompt-list)
  (cl-loop for text in prompt-list
           for role = t then (not role)
           if text
           if role
           collect (list :role "user" :parts `[(:text ,text)]) into prompts
           else collect (list :role "model" :parts `(:text ,text)) into prompts
           finally return prompts))

(cl-defmethod gptel--parse-buffer ((backend gptel-gemini) &optional max-entries)
  (let ((prompts) (prev-pt (point))
        (include-media (and gptel-track-media (or (gptel--model-capable-p 'media)
                                                  (gptel--model-capable-p 'url)))))
    (if (or gptel-mode gptel-track-response)
        (while (and (or (not max-entries) (>= max-entries 0))
                    (goto-char (previous-single-property-change
                                (point) 'gptel nil (point-min)))
                    (not (= (point) prev-pt)))
          (pcase (get-char-property (point) 'gptel)
            ('response
             (when-let* ((content (gptel--trim-prefixes
                                   (buffer-substring-no-properties (point) prev-pt))))
               (push (list :role "model" :parts (list :text content)) prompts)))
            (`(tool . ,_id)
             (save-excursion
               (condition-case nil
                   (let* ((tool-call (read (current-buffer)))
                          (name (plist-get tool-call :name))
                          (arguments  (plist-get tool-call :args)))
                     (plist-put tool-call :result
                                (string-trim (buffer-substring-no-properties
                                              (point) prev-pt)))
                     (push (gptel--parse-tool-results backend (list tool-call))
                           prompts)
                     (push (list :role "model"
                                 :parts
                                 (vector `(:functionCall ( :name ,name
                                                           :args ,arguments))))
                           prompts))
                 ((end-of-file invalid-read-syntax)
                  (message (format "Could not parse tool-call on line %s"
                                   (line-number-at-pos (point))))))))
            ('ignore)
            ('nil
             (if include-media
                 (when-let* ((content (gptel--gemini-parse-multipart
                                       (gptel--parse-media-links major-mode (point) prev-pt))))
                   (when (> (length content) 0)
                     (push (list :role "user" :parts content) prompts)))
               (when-let* ((content (gptel--trim-prefixes
                                     (buffer-substring-no-properties
                                      (point) prev-pt))))
                 (push (list :role "user" :parts `[(:text ,content)]) prompts)))))
          (setq prev-pt (point))
          (and max-entries (cl-decf max-entries)))
      (let ((content (string-trim (buffer-substring-no-properties
                                   (point-min) (point-max)))))
        (push (list :role "user" :parts `[(:text ,content)]) prompts)))
    prompts))

(defun gptel--gemini-parse-multipart (parts)
  "Convert a multipart prompt PARTS to the Gemini API format.

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
   (and (or (= n 1) (= n last)) (setq text (gptel--trim-prefixes text))) and
   if text
   collect (list :text text) into parts-array end
   else if media
   collect
   `(:inline_data
     (:mime_type ,(plist-get part :mime)
      :data ,(gptel--base64-encode media)))
   into parts-array
   finally return (vconcat parts-array)))

(cl-defmethod gptel--wrap-user-prompt ((_backend gptel-gemini) prompts
                                       &optional inject-media)
  "Wrap the last user prompt in PROMPTS with the context string.

If INJECT-MEDIA is non-nil wrap it with base64-encoded media
files in the context."
  (if inject-media
      ;; Wrap the first user prompt with included media files/contexts
      (when-let* ((media-list (gptel-context--collect-media)))
        (cl-callf (lambda (current)
                    (vconcat (gptel--gemini-parse-multipart media-list)
                             current))
            (plist-get (car prompts) :parts)))
    ;; Wrap the last user prompt with included text contexts
    (cl-callf (lambda (current)
                (if-let* ((wrapped (gptel-context--wrap nil)))
                    (vconcat `((:text ,wrapped)) current)
                  current))
        (plist-get (car (last prompts)) :parts))))

(defconst gptel--gemini-models
  '((gemini-1.5-pro-latest
     :description "Google's latest model with enhanced capabilities across various tasks"
     :capabilities (tool-use json media)
     :mime-types ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
                  "application/pdf" "text/plain" "text/csv" "text/html")
     :context-window 2000
     ;; input & output price is halved for prompts of 128k tokens or less
     :input-cost 2.50
     :output-cost 10
     :cutoff-date "2024-05")
    (gemini-2.0-flash-exp
     :description "Next generation features, superior speed, native tool use"
     :capabilities (tool-use json media)
     :mime-types ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
                  "application/pdf" "text/plain" "text/csv" "text/html")
     :context-window 1000
     :cutoff-date "2024-12")
    (gemini-1.5-flash
     :description "A faster, more efficient version of Gemini 1.5 optimized for speed"
     :capabilities (tool-use json media)
     :mime-types ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
                  "application/pdf" "text/plain" "text/csv" "text/html")
     :context-window 1000
     ;; input & output price is halved for prompts of 128k tokens or less
     :input-cost 0.15
     :output-cost 0.60
     :cutoff-date "2024-05")
    (gemini-1.5-flash-8b
     :description "High volume and lower intelligence tasks"
     :capabilities (tool-use json media)
     :context-window 1000
     :mime-types ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
                  "application/pdf" "text/plain" "text/csv" "text/html")
     ;; input & output price is halved for prompts of 128k tokens or less
     :input-cost 0.075
     :output-cost 0.30
     :cutoff-date "2024-10")
    (gemini-exp-1206
     :description "Improved coding, reasoning and vision capabilities"
     :capabilities (tool-use json media)
     :mime-types ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
                  "application/pdf" "text/plain" "text/csv" "text/html")
     :cutoff-date "2024-12")
    (gemini-2.0-flash
     :description "Next gen, high speed, multimodal for a diverse variety of tasks"
     :capabilities (tool-use json media)
     :mime-types ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
                  "application/pdf" "text/plain" "text/csv" "text/html")
     :context-window 1000
     :input-cost 0.10
     :output-cost 0.40
     :cutoff-date "2024-08")
    (gemini-2.0-flash-lite-preview-02-05
     :description "Gemini 2.0 Flash model optimized for cost efficiency and low latency"
     :capabilities (json)
     :context-window 1000
     :input-cost 0.075
     :output-cost 0.30
     :cutoff-date "2024-08")
    (gemini-2.0-pro-exp-02-05
     :description "Next gen, high speed, multimodal for a diverse variety of tasks"
     :capabilities (tool-use json)
     :context-window 2000
     :input-cost 0.00
     :output-cost 0.00
     :cutoff-date "2024-08")
    (gemini-2.0-flash-thinking-exp-01-21
     :description "Next gen, high speed, multimodal for a diverse variety of tasks"
     :capabilities (json)
     :input-cost 0.00
     :output-cost 0.00
     :cutoff-date "2024-08")
    (gemini-2.0-flash-exp
     :description "Multi-modal, streaming, tool use 2000 RPM"
     :capabilities (tool-use json media)
     :mime-types ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
                  "application/pdf" "text/plain" "text/csv" "text/html")
     :context-window 1000
     :input-cost 0.00
     :output-cost 0.00
     :cutoff-date "2024-08")
    (gemini-2.5-pro-exp-03-25
     :description "Enhanced thinking and reasoning, multimodal understanding, advanced coding, and more"
     :capabilities (tool-use json media)
     :mime-types ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
                  "application/pdf" "text/plain" "text/csv" "text/html")
     :context-window 1000
     :input-cost 0.00
     :output-cost 0.00
     :cutoff-date "2025-01")
    (gemini-2.0-flash-thinking-exp
     :description "DEPRECATED: Please use gemini-2.0-flash-thinking-exp-01-21 instead."
     :capabilities (tool-use media)
     :context-window 32
     :mime-types ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
                  "text/plain" "text/csv" "text/html")
     :cutoff-date "2024-08"))
  "List of available Gemini models and associated properties.
Keys:

- `:description': a brief description of the model.

- `:capabilities': a list of capabilities supported by the model.

- `:mime-types': a list of supported MIME types for media files.

- `:context-window': the context window size, in thousands of tokens.

- `:input-cost': the input cost, in US dollars per million tokens.

- `:output-cost': the output cost, in US dollars per million tokens.

- `:cutoff-date': the knowledge cutoff date.

- `:request-params': a plist of additional request parameters to
  include when using this model.

Information about the Gemini models was obtained from the following
source:

- <https://ai.google.dev/pricing>
- <https://cloud.google.com/vertex-ai/generative-ai/docs/learn/models>
- <https://ai.google.dev/gemini-api/docs/models>")

;;;###autoload
(cl-defun gptel-make-gemini
    (name &key curl-args header key request-params
          (stream nil)
          (host "generativelanguage.googleapis.com")
          (protocol "https")
          (models gptel--gemini-models)
          (endpoint "/v1beta/models"))

  "Register a Gemini backend for gptel with NAME.

Keyword arguments:

CURL-ARGS (optional) is a list of additional Curl arguments.

HOST (optional) is the API host, defaults to
\"generativelanguage.googleapis.com\".

MODELS is a list of available model names, as symbols.
Additionally, you can specify supported LLM capabilities like
vision or tool-use by appending a plist to the model with more
information, in the form

 (model-name . plist)

For a list of currently recognized plist keys, see
`gptel--gemini-models'. An example of a model specification
including both kinds of specs:

:models
\\='(gemini-2.0-flash-lite              ;Simple specs
  gemini-1.5-flash
  (gemini-1.5-pro-latest                ;Full spec
   :description
   \"Complex reasoning tasks, problem solving and data extraction\"
   :capabilities (tool json)
   :mime-types
   (\"image/jpeg\" \"image/png\" \"image/webp\" \"image/heic\")))


STREAM is a boolean to enable streaming responses, defaults to
false.

PROTOCOL (optional) specifies the protocol, \"https\" by default.

ENDPOINT (optional) is the API endpoint for completions, defaults to
\"/v1beta/models\".

HEADER (optional) is for additional headers to send with each
request.  It should be an alist or a function that retuns an
alist, like:
 ((\"Content-Type\" . \"application/json\"))

KEY (optional) is a variable whose value is the API key, or
function that returns the key.

REQUEST-PARAMS (optional) is a plist of additional HTTP request
parameters (as plist keys) and values supported by the API.  Use
these to set parameters that gptel does not provide user options
for."
  (declare (indent 1))
  (let ((backend (gptel--make-gemini
                  :curl-args curl-args
                  :name name
                  :host host
                  :header header
                  :models (gptel--process-models models)
                  :protocol protocol
                  :endpoint endpoint
                  :stream stream
                  :request-params request-params
                  :key key
                  :url (lambda ()
                         (let ((method
                                (if (and stream gptel-use-curl gptel-stream)
                                    "streamGenerateContent"
                                  "generateContent")))
                           (format "%s://%s%s/%s:%s?key=%s"
                                   protocol
                                   host
                                   endpoint
                                   gptel-model
                                   method
                                   (gptel--get-api-key)))))))
    (prog1 backend
      (setf (alist-get name gptel--known-backends
                       nil nil #'equal)
            backend))))

(provide 'gptel-gemini)
;;; gptel-gemini.el ends here
