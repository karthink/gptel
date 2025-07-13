;;; gptel-anthropic.el ---  Anthropic AI suppport for gptel  -*- lexical-binding: t; -*-

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

;; This file adds support for Anthropic's Messages API to gptel

;;; Code:
(require 'cl-generic)
(require 'cl-lib)
(require 'map)
(require 'gptel)

(defvar json-object-type)

(declare-function prop-match-value "text-property-search")
(declare-function text-property-search-backward "text-property-search")
(declare-function json-read "json" ())
(declare-function gptel-context--wrap "gptel-context")
(declare-function gptel-context--collect-media "gptel-context")

;;; Anthropic (Messages API)
(cl-defstruct (gptel-anthropic (:constructor gptel--make-anthropic)
                               (:copier nil)
                               (:include gptel-backend)))

;; NOTE the crucial difference between
;; - (push val (plist-get info :key)) and
;;   (plist-put (plist-get info :key) (cons val ...))
;;
;; - (setf (plist-get info :key) val) and
;;   (plist-put info :key val)
;;
;; in the following function.  The first variant conses at the head of info, the
;; second one at the tail.  This means only the second option is viable for
;; mutating a plist function argument that's passed by reference.
;; Do NOT change the plist-put to push or setf!

;; NOTE: The stream parser looks complicated only because it handles streaming
;; tool calls, streaming "thinking" blocks and their interactions.  Stream
;; parsing is simple: if you are studying this code, look instead at a commit
;; from before tool-use support was added to gptel.
(cl-defmethod gptel-curl--parse-stream ((_backend gptel-anthropic) info)
  "Parse an Anthropic data stream.

Return the text response accumulated since the last call to this
function.  Additionally, mutate state INFO to add tool-use
information if the stream contains it.  Not my best work, I know."
  (let* ((content-strs)
         (pt (point)))
    (condition-case nil
        (while (re-search-forward "^event: " nil t)
          (setq pt (match-beginning 0))
          (if (equal (line-end-position) (point-max))
              (error "Data block incomplete"))
          (cond
           ((looking-at "content_block_delta") ;collect incremental
            (forward-line 1) (forward-char 5)  ;text, tool or thinking block
            (when-let* ((delta (plist-get (gptel--json-read) :delta)))
              (if-let* ((content (plist-get delta :text))
                        ((not (eq content :null))))
                  (push content content-strs) ;collect text
                (if-let* ((partial-json (plist-get delta :partial_json)))
                    (plist-put          ;collect partial tool input
                     info :partial_json
                     (cons partial-json (plist-get info :partial_json)))
                  (if-let* ((thinking (plist-get delta :thinking)))
                      (plist-put info :reasoning
                                 (concat (plist-get info :reasoning) thinking))
                    (if-let* ((signature (plist-get delta :signature)))
                        (plist-put info :signature
                                   (concat (plist-get info :signature) signature))))))))

           ((looking-at "content_block_start") ;Is the following block text or tool-use?
            (forward-line 1) (forward-char 5)
            (when-let* ((cblock (plist-get (gptel--json-read) :content_block)))
              (pcase (plist-get cblock :type)
                ("text" (push (plist-get cblock :text) content-strs))
                ("tool_use" (plist-put info :tool-use
                                       (cons (list :id (plist-get cblock :id)
                                                   :name (plist-get cblock :name))
                                             (plist-get info :tool-use))))
                ("thinking" (plist-put info :reasoning (plist-get cblock :thinking))
                 (plist-put info :reasoning-block 'in)))))

           ((looking-at "content_block_stop")
            (cond
             ((plist-get info :partial_json)   ;End of tool block
              (condition-case-unless-debug nil ;Combine partial tool inputs
                  (let* ((args-json (apply #'concat (nreverse (plist-get info :partial_json))))
                         (args-decoded  ;Handle blank argument strings
                          (if (string-empty-p args-json)
                              nil (gptel--json-read-string args-json))))
                    ;; Add the input to the tool-call spec
                    (plist-put (car (plist-get info :tool-use)) :input args-decoded))
                ;; If there was an error in reading that tool, we ignore it:
                ;; TODO(tool) handle this error better
                (error (pop (plist-get info :tool-use)))) ;TODO: nreverse :tool-use list
              (plist-put info :partial_json nil))

             ((eq (plist-get info :reasoning-block) 'in) ;End of reasoning block
              (plist-put info :reasoning-block t)))) ;Signal end of reasoning stream to filter

           ((looking-at "message_delta")
            ;; collect stop_reason, usage_tokens and prepare tools
            (forward-line 1) (forward-char 5)
            (when-let* ((tool-use (plist-get info :tool-use))
                        (response (gptel--json-read)))
              (let* ((data (plist-get info :data))
                     (prompts (plist-get data :messages)))
                (plist-put ; Append a COPY of response text + tool-use to the prompts list
                 data :messages
                 (vconcat
                  prompts
                  `((:role "assistant"
                     :content ,(vconcat ;Insert any LLM text and thinking text
                                (and-let* ((reasoning (plist-get info :partial_reasoning)))
                                 `((:type "thinking" :thinking ,reasoning
                                    :signature ,(plist-get info :signature))))
                                (and-let* ((strs (plist-get info :partial_text)))
                                 `((:type "text" :text ,(apply #'concat (nreverse strs)))))
                                (mapcar (lambda (tool-call) ;followed by the tool calls
                                          (append (list :type "tool_use")
                                           (copy-sequence tool-call)))
                                 tool-use))))))
                (plist-put info :partial_text nil) ; Clear any captured text
                ;; Then shape the tool-use block by adding args so we can call the functions
                (mapc (lambda (tool-call)
                        (plist-put tool-call :args (plist-get tool-call :input))
                        (plist-put tool-call :input nil)
                        (plist-put tool-call :id (plist-get tool-call :id)))
                      tool-use))
              (plist-put info :output-tokens
                         (map-nested-elt response '(:usage :output_tokens)))
              (plist-put info :stop-reason
                         (map-nested-elt response '(:delta :stop_reason)))))))
      (error (goto-char pt)))
    (let ((response-text (apply #'concat (nreverse content-strs))))
      (unless (string-empty-p response-text)
        (plist-put info :partial_text
                   (cons response-text (plist-get info :partial_text))))
      (when (plist-get info :tools)
        (when-let* ((reasoning (plist-get info :reasoning)))
          (plist-put info :partial_reasoning
                     (concat (plist-get info :partial_reasoning) reasoning))))
      response-text)))

(cl-defmethod gptel--parse-response ((_backend gptel-anthropic) response info)
  "Parse an Anthropic (non-streaming) RESPONSE and return response text.

Mutate state INFO with response metadata."
  (plist-put info :stop-reason (plist-get response :stop_reason))
  (plist-put info :output-tokens
             (map-nested-elt response '(:usage :output_tokens)))
  (cl-loop
   with content = (plist-get response :content)
   for cblock across content
   for type = (plist-get cblock :type)
   if (equal type "text")
   ;; TODO(tool) can :text be :null?
   collect (plist-get cblock :text) into content-strs
   else if (equal type "tool_use")
   collect cblock into tool-use
   else if (equal type "thinking")
   do
   (plist-put
    info :reasoning
    (concat (plist-get info :reasoning)
            (plist-get cblock :thinking)))
   finally do
   (when tool-use
     ;; First, add the tool call to the prompts list
     (let* ((data (plist-get info :data))
            (prompts (plist-get data :messages)))
       (plist-put
        data :messages
        (vconcat prompts `((:role "assistant" :content ,content)))))
     ;; Then capture the tool call data for running the tool
     (cl-loop
      for call-raw in tool-use
      for call = (copy-sequence call-raw) do
      (plist-put call :args (plist-get call :input))
      (plist-put call :input nil)
      (plist-put call :id (plist-get call :id))
      collect call into calls
      finally do (plist-put info :tool-use calls)))
   finally return
   (and content-strs (apply #'concat content-strs))))

(cl-defmethod gptel--request-data ((backend gptel-anthropic) prompts)
  "JSON encode PROMPTS for sending to ChatGPT."
  (let ((prompts-plist
         `(:model ,(gptel--model-name gptel-model)
           :stream ,(or gptel-stream :json-false)
           :max_tokens ,(or gptel-max-tokens 4096)
           :messages [,@prompts])))
    (when gptel--system-message
      (if (and (or (eq gptel-cache t) (memq 'system gptel-cache))
               (gptel--model-capable-p 'cache))
          ;; gptel--system-message is guaranteed to be a string
          (plist-put prompts-plist :system
                     `[(:type "text" :text ,gptel--system-message
                        :cache_control (:type "ephemeral" :ttl "1h"))])
        (plist-put prompts-plist :system gptel--system-message)))
    (when gptel-temperature
      (plist-put prompts-plist :temperature gptel-temperature))
    (when gptel-use-tools
      (when (eq gptel-use-tools 'force)
        (plist-put prompts-plist :tool_choice '(:type "any")))
      (when gptel-tools
        (let ((tools-array (gptel--parse-tools backend gptel-tools)))
          (plist-put prompts-plist :tools tools-array)
          (when (and (or (eq gptel-cache t) (memq 'tool gptel-cache))
                     (gptel--model-capable-p 'cache))
            (nconc (aref tools-array (1- (length tools-array)))
                   '(:cache_control (:type "ephemeral")))))))
    (when gptel--schema
      (plist-put prompts-plist :tools
                 (vconcat
                  (list (gptel--parse-schema backend gptel--schema))
                  (plist-get prompts-plist :tools)))
      (plist-put prompts-plist :tool_choice
                 `(:type "tool" :name ,gptel--ersatz-json-tool)))
    ;; Merge request params with model and backend params.
    (gptel--merge-plists
     prompts-plist
     gptel--request-params
     (gptel-backend-request-params gptel-backend)
     (gptel--model-request-params  gptel-model))))

(cl-defmethod gptel--parse-schema ((_backend gptel-anthropic) schema)
  ;; Unlike the other backends, Anthropic generates JSON using a tool call.  We
  ;; write the tool here, meant to be added to :tools.
  (list
   :name "response_json"
   :description "Record JSON output according to user prompt"
   :input_schema (gptel--preprocess-schema
                  (gptel--dispatch-schema-type schema))))

(cl-defmethod gptel--parse-tools ((_backend gptel-anthropic) tools)
  "Parse TOOLS to the Anthropic API tool definition spec.

TOOLS is a list of `gptel-tool' structs, which see."
  (vconcat
   (mapcar
    (lambda (tool)
      (list :name (gptel-tool-name tool)
            :description (gptel-tool-description tool)
            :input_schema ;NOTE: Anthropic wants "{}" if the function takes no args, not null
            (list :type "object"
                  ;; See the generic implementation for an explanation of this
                  ;; transformation.
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
                   append (list newname argspec))
                  :required
                  (vconcat
                   (delq nil (mapcar
                              (lambda (arg) (and (not (plist-get arg :optional))
                                            (plist-get arg :name)))
                              (gptel-tool-args tool)))))))
    (ensure-list tools))))

(cl-defmethod gptel--parse-tool-results ((_backend gptel-anthropic) tool-use)
  "Return a prompt containing tool call results in TOOL-USE.

TOOL-USE is a list of plists containing tool names, arguments and call results."
  ;; (declare (side-effect-free t))
  (list
   :role "user"
   :content
   (vconcat
    (mapcar
     (lambda (tool-call)
       (let* ((result (plist-get tool-call :result))
              (formatted
               (list :type "tool_result"
                     :tool_use_id (plist-get tool-call :id)
                     :content (if (stringp result) result
                                (prin1-to-string result)))))
         (prog1 formatted
           (when (plist-get tool-call :error)
             (plist-put formatted :is_error t)))))
     tool-use))))

;; NOTE: No `gptel--inject-prompt' method required for gptel-anthropic, since
;; this is handled by its defgeneric implementation

;; TODO: Remove these functions (#792)
(defun gptel--anthropic-format-tool-id (tool-id)
  (unless tool-id
    (setq tool-id (substring
                   (md5 (format "%s%s" (random) (float-time)))
                   nil 24)))
  (if (or (string-prefix-p "call_" tool-id)
          (string-prefix-p "toolu_" tool-id))
      tool-id
    (format "toolu_%s" tool-id)))

(defun gptel--anthropic-unformat-tool-id (tool-id)
  (or (and (string-match "toolu_\\(.+\\)" tool-id)
           (match-string 1 tool-id))
      tool-id))

(cl-defmethod gptel--parse-list ((backend gptel-anthropic) prompt-list)
  (let ((full-prompt
         (if (consp (car prompt-list))
             (let ((prompts))
               (dolist (entry prompt-list) ; Advanced format, list of lists
                 (pcase entry
                   (`(prompt . ,msg)
                    (push (list :role "user"
                                :content `[(:type "text" :text ,(or (car-safe msg) msg))])
                          prompts))
                   (`(response . ,msg)
                    (push (list :role "assistant"
                                :content `[(:type "text" :text ,(or (car-safe msg) msg))])
                          prompts))
                   (`(tool . ,call)
                    (unless (plist-get call :id)
                      (plist-put call :id (gptel--anthropic-format-tool-id nil)))
                    (push (list :role "assistant"
                                :content `[( :type "tool_use" :id ,(plist-get call :id)
                                             :name ,(plist-get call :name)
                                             :input ,(plist-get call :args))])
                          prompts)
                    (push (gptel--parse-tool-results backend (list (cdr entry))) prompts))))
               (nreverse prompts))
           (cl-loop for text in prompt-list ; Simple format, list of strings
                    for role = t then (not role)
                    if text
                    collect (list :role (if role "user" "assistant")
                                  :content `[(:type "text" :text ,text)])))))
    ;; cache messages if required: add cache_control to the last message
    (when (and (or (eq gptel-cache t) (memq 'message gptel-cache))
               (gptel--model-capable-p 'cache))
      (nconc (aref (plist-get (car (last full-prompt)) :content) 0)
             '(:cache_control (:type "ephemeral" :ttl "1h"))))
    full-prompt))

(cl-defmethod gptel--parse-buffer ((backend gptel-anthropic) &optional max-entries)
  (let ((prompts) (prev-pt (point)))
    (if (or gptel-mode gptel-track-response)
        (while (and (or (not max-entries) (>= max-entries 0))
                    (goto-char (previous-single-property-change
                                (point) 'gptel nil (point-min)))
                    (not (= (point) prev-pt)))
          ;; HACK Until we can find a more robust solution for editing
          ;; responses, ignore prompts containing only whitespace, as the
          ;; Anthropic API can't handle it.  See #452, #409, #406, #351 and #321
          ;; We check for blank prompts by skipping whitespace and comparing
          ;; point against the previous.
          (unless (save-excursion (skip-syntax-forward " ") (>= (point) prev-pt))
            (pcase (get-char-property (point) 'gptel)
              ('response
               (when-let* ((content
                            (gptel--trim-prefixes
                             (buffer-substring-no-properties (point) prev-pt))))
                 (when (not (string-blank-p content))
                   (push (list :role "assistant" :content content) prompts))))
              (`(tool . ,id)
               (save-excursion
                 (condition-case nil
                     (let* ((tool-call (read (current-buffer)))
                            ;; (id (gptel--anthropic-format-tool-id id))
                            (name (plist-get tool-call :name))
                            (arguments (plist-get tool-call :args)))
                       (plist-put tool-call :id id)
                       (plist-put tool-call :result
                                  (string-trim (buffer-substring-no-properties
                                                (point) prev-pt)))
                       (push (gptel--parse-tool-results backend (list tool-call))
                             prompts)
                       (push (list :role "assistant"
                                   :content `[( :type "tool_use" :id ,id :name ,name
                                                :input ,arguments)])
                             prompts))
                   ((end-of-file invalid-read-syntax)
                    (message (format "Could not parse tool-call %s on line %s"
                                     id (line-number-at-pos (point))))))))
              ('ignore)
              ('nil                     ; user role: possibly with media
               (if gptel-track-media
                   (when-let* ((content (gptel--anthropic-parse-multipart
                                         (gptel--parse-media-links major-mode (point) prev-pt))))
                     (when (> (length content) 0)
                       (push (list :role "user" :content content) prompts)))
                 (when-let* ((content (gptel--trim-prefixes
                                       (buffer-substring-no-properties (point) prev-pt))))
                   (push (list :role "user" :content content) prompts))))))
          (setq prev-pt (point))
          (and max-entries (cl-decf max-entries)))
      (when-let* ((content (string-trim (buffer-substring-no-properties
                                         (point-min) (point-max)))))
        ;; XXX fails if content is empty.  The correct error behavior is left to
        ;; a future discussion.
        (push (list :role "user" :content content) prompts)))
    ;; Cache messages if required: add cache_control to the last message
    (if (and (or (eq gptel-cache t) (memq 'message gptel-cache))
             (gptel--model-capable-p 'cache))
        (let ((last-message (plist-get (car (last prompts)) :content)))
          (if (stringp last-message)
              (plist-put
               (car (last prompts)) :content
               `[(:type "text" :text ,last-message
                  :cache_control (:type "ephemeral" :ttl "1h"))])
            (nconc (aref (plist-get (car (last prompts)) :content) 0)
                   '(:cache_control (:type "ephemeral" :ttl "1h"))))))
    prompts))

(defun gptel--anthropic-parse-multipart (parts)
  "Convert a multipart prompt PARTS to the Anthropic API format.

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
   with type
   for text = (plist-get part :text)
   for mime = (plist-get part :mime)
   for media = (plist-get part :media)
   if text do
   (and (or (= n 1) (= n last)) (setq text (gptel--trim-prefixes text))) and
   if text
   collect `(:type "text" :text ,text) into parts-array end
   else if media
   do
   (setq type (cond                     ;Currently supported: Images and PDFs
               ((equal (substring mime 0 5) "image") "image")
               ;; NOTE: Only Claude 3.5 Sonnet supports PDF documents:
               ((equal mime "application/pdf") "document")
               (t (error (concat "(gptel-anthropic) Request aborted: "
                                 "trying to send unsupported MIME type %s")
                         mime))))
   and collect
   `(:type ,type
     :source (:type "base64"
              :media_type ,(plist-get part :mime)
              :data ,(gptel--base64-encode media))
     ;; TODO Make media caching a user option
     ,@(and (gptel--model-capable-p 'cache)
        '(:cache_control (:type "ephemeral"))))
   into parts-array
   else if (plist-get part :textfile) collect
   `(:type "text"
     :text ,(with-temp-buffer
              (gptel--insert-file-string (plist-get part :textfile))
              (buffer-string)))
   into parts-array
   finally return (vconcat parts-array)))

(cl-defmethod gptel--wrap-user-prompt ((_backend gptel-anthropic) prompts
                                       &optional inject-media)
  "Wrap the last user prompt in PROMPTS with the context string.

If INJECT-MEDIA is non-nil wrap it with base64-encoded media
files in the context."
  (if inject-media
      ;; Wrap the first user prompt with included media files/contexts
      (when-let* ((media-list (gptel-context--collect-media)))
        (cl-callf (lambda (current)
                    (vconcat
                     (gptel--anthropic-parse-multipart media-list)
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

;; (if-let* ((context-string (gptel-context--string gptel-context--alist)))
;;     (cl-callf (lambda (previous)
;;                 (cl-typecase previous
;;                   (string (concat context-string previous))
;;                   (vector (vconcat `((:type "text" :text ,previous))
;;                                    previous))
;;                   (t context-string)))
;;         (plist-get (car (last prompts)) :content)))

(defconst gptel--anthropic-models
  '((claude-3-7-sonnet-20250219
     :description "Hybrid model capable of standard thinking and extended thinking modes"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 200
     :input-cost 3
     :output-cost 15
     :cutoff-date "2025-02")
    (claude-sonnet-4-20250514
     :description "High-performance model with exceptional reasoning and efficiency"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 200
     :input-cost 3
     :output-cost 15
     :cutoff-date "2025-03")
    (claude-opus-4-20250514
     :description "Most capable model for complex reasoning and advanced coding"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 200
     :input-cost 15
     :output-cost 75
     :cutoff-date "2025-03")
    (claude-3-5-sonnet-20241022
     :description "Highest level of intelligence and capability"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 200
     :input-cost 3
     :output-cost 15
     :cutoff-date "2024-04")
    (claude-3-5-sonnet-20240620
     :description "Highest level of intelligence and capability (earlier version)"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 200
     :input-cost 3
     :output-cost 15
     :cutoff-date "2024-04")
    (claude-3-5-haiku-20241022
     :description "Intelligence at blazing speeds"
     :capabilities (tool-use cache)
     :context-window 200
     :input-cost 1.00
     :output-cost 5.00
     :cutoff-date "2024-07")
    (claude-3-opus-20240229
     :description "Top-level performance, intelligence, fluency, and understanding"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 200
     :input-cost 15
     :output-cost 75
     :cutoff-date "2023-08")
    (claude-3-sonnet-20240229
     :description "Balance of intelligence and speed (legacy model)"
     :capabilities (media tool-use)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 200
     :input-cost 3
     :output-cost 15
     :cutoff-date "2023-08")
    (claude-3-haiku-20240307
     :description "Fast and most compact model for near-instant responsiveness"
     :capabilities (tool-use cache)
     :context-window 200
     :input-cost 0.25
     :output-cost 1.25
     :cutoff-date "2023-08"))
  "List of available Anthropic models and associated properties.
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

Information about the Anthropic models was obtained from the following
comparison table:

URL `https://docs.anthropic.com/en/docs/about-claude/models#model-comparison-table'")

;;;###autoload
(cl-defun gptel-make-anthropic
    (name &key curl-args stream key request-params
          (header
           (lambda () (when-let* ((key (gptel--get-api-key)))
                   `(("x-api-key" . ,key)
                     ("anthropic-version" . "2023-06-01")
                     ("anthropic-beta" . "extended-cache-ttl-2025-04-11")))))
          (models gptel--anthropic-models)
          (host "api.anthropic.com")
          (protocol "https")
          (endpoint "/v1/messages"))
  "Register an Anthropic API-compatible backend for gptel with NAME.

Keyword arguments:

CURL-ARGS (optional) is a list of additional Curl arguments.

HOST (optional) is the API host, \"api.anthropic.com\" by default.

MODELS is a list of available model names, as symbols.
Additionally, you can specify supported LLM capabilities like
vision or tool-use by appending a plist to the model with more
information, in the form

 (model-name . plist)

For a list of currently recognized plist keys, see
`gptel--anthropic-models'. An example of a model specification
including both kinds of specs:

:models
\\='(claude-3-haiku-20240307               ;Simple specs
  claude-3-opus-20240229
  (claude-3-5-sonnet-20240620           ;Full spec
   :description  \"Balance of intelligence and speed\"
   :capabilities (media tool json)
   :mime-types
   (\"image/jpeg\" \"image/png\" \"image/gif\" \"image/webp\")))

STREAM is a boolean to toggle streaming responses, defaults to
false.

PROTOCOL (optional) specifies the protocol, https by default.

ENDPOINT (optional) is the API endpoint for completions, defaults to
\"/v1/messages\".

HEADER (optional) is for additional headers to send with each
request.  It should be an alist or a function that retuns an
alist, like:
 ((\"Content-Type\" . \"application/json\"))

KEY is a variable whose value is the API key, or function that
returns the key.

REQUEST-PARAMS (optional) is a plist of additional HTTP request
parameters (as plist keys) and values supported by the API.  Use
these to set parameters that gptel does not provide user options
for."
  (declare (indent 1))
  (let ((backend (gptel--make-anthropic
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

(provide 'gptel-anthropic)
;;; gptel-anthropic.el ends here

;; Local Variables:
;; byte-compile-warnings: (not docstrings)
;; End:

