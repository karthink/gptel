;;; gptel-ollama.el --- Ollama support for gptel     -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Keywords: hypermedia

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

;; This file adds support for the Ollama LLM API to gptel

;;; Code:
(require 'gptel)
(require 'cl-generic)

(declare-function json-read "json" ())
(declare-function gptel-context--wrap "gptel-context")
(declare-function gptel-context--collect-media "gptel-context")
(defvar json-object-type)

;;; Ollama
(cl-defstruct (gptel-ollama (:constructor gptel--make-ollama)
                            (:copier nil)
                            (:include gptel-backend)))

;; FIXME(fsm) Remove this variable
(defvar-local gptel--ollama-token-count 0
  "Token count for ollama conversations.

This variable holds the total token count for conversations with
Ollama models.

Intended for internal use only.")

(cl-defmethod gptel-curl--parse-stream ((_backend gptel-ollama) info)
  "Parse response stream for the Ollama API."
  (when (and (bobp) (re-search-forward "^{" nil t))
    (forward-line 0))
  (let* ((content-strs) (content) (pt (point)))
    (condition-case nil
        (while (setq content (gptel--json-read))
          (setq pt (point))
          (let ((done (map-elt content :done))
                (reasoning (map-nested-elt content '(:message :thinking)))
                (response (map-nested-elt content '(:message :content)))
                (tool-calls (map-nested-elt content '(:message :tool_calls))))
            (when (and response (not (eq response :null)))
              (push response content-strs))
            (when (and tool-calls (not (eq tool-calls :null)))
              (gptel--inject-prompt
               (plist-get info :backend) (plist-get info :data)
               `(:role "assistant" :content :null :tool_calls ,(vconcat tool-calls)))
              (cl-loop
               for tool-call across tool-calls  ;replace ":arguments" with ":args"
               for call-spec = (copy-sequence (plist-get tool-call :function))
               do (plist-put call-spec :args
                             (plist-get call-spec :arguments))
               (plist-put call-spec :arguments nil)
               collect call-spec into tool-use
               finally (plist-put info :tool-use tool-use)))
            (if (and reasoning (not (eq reasoning :null)))
                (plist-put info :reasoning
                           (concat (plist-get info :reasoning) reasoning))
              (if (eq 'in (plist-get info :reasoning-block))
                  (plist-put info :reasoning-block t)
                (plist-put info :reasoning-block nil)))
            (unless (eq done :json-false)
              (with-current-buffer (plist-get info :buffer)
                (cl-incf gptel--ollama-token-count
                         (+ (or (map-elt content :prompt_eval_count) 0)
                            (or (map-elt content :eval_count) 0))))
              (goto-char (point-max)))))
      (error (goto-char pt)))
    (apply #'concat (nreverse content-strs))))

(cl-defmethod gptel--parse-response ((_backend gptel-ollama) response info)
  "Parse a one-shot RESPONSE from the Ollama API and return text.

Store response metadata in state INFO."
  (plist-put info :stop-reason (plist-get response :done_reason))
  (plist-put info :output-tokens (plist-get response :eval_count))
  (let* ((message (plist-get response :message))
         (reasoning (plist-get message :thinking))
         (content (plist-get message :content)))
    (when reasoning
      (plist-put info :reasoning reasoning))
    (when-let* ((tool-calls (plist-get message :tool_calls)))
      ;; First add the tool call to the prompts list
      (let* ((data (plist-get info :data))
             (prompts (plist-get data :messages)))
        (plist-put data :messages (vconcat prompts `(,message))))
      ;; Then capture the tool call data for running the tool
      (cl-loop
       for tool-call across tool-calls  ;replace ":arguments" with ":args"
       for call-spec = (copy-sequence (plist-get tool-call :function))
       do (plist-put call-spec :args
                     (plist-get call-spec :arguments))
       (plist-put call-spec :arguments nil)
       collect call-spec into tool-use
       finally (plist-put info :tool-use tool-use)))
    (when (and content (not (or (eq content :null) (string-empty-p content))))
      content)))

(cl-defmethod gptel--request-data ((backend gptel-ollama) prompts)
  "JSON encode PROMPTS for sending to Ollama."
  (when gptel--system-message
    (push (list :role "system"
                :content gptel--system-message)
          prompts))
  (let* ((prompts-plist
          (gptel--merge-plists
           `(:model ,(gptel--model-name gptel-model)
             :messages [,@prompts]
             :stream ,(or gptel-stream :json-false)
             ,@(and gptel--schema
                `(:format ,(gptel--preprocess-schema
                            (gptel--dispatch-schema-type gptel--schema)))))
           gptel--request-params
           (gptel-backend-request-params gptel-backend)
           (gptel--model-request-params  gptel-model)))
         ;; the initial options (if any) from request params
         (options-plist (plist-get prompts-plist :options)))

    (when (and gptel-use-tools gptel-tools)
      ;; TODO(tool): Find out how to force tool use for Ollama
      (plist-put prompts-plist :tools
                 (gptel--parse-tools backend gptel-tools)))
    ;; if the temperature and max-tokens aren't set as
    ;; backend/model-specific, use the global settings
    (when (and gptel-temperature (not (plist-get options-plist :temperature)))
      (setq options-plist
            (plist-put options-plist :temperature gptel-temperature)))
    (when (and gptel-max-tokens (not (plist-get options-plist :num_predict)))
      (setq options-plist
            (plist-put options-plist :num_predict gptel-max-tokens)))
    (plist-put prompts-plist :options options-plist)))

;; NOTE: No `gptel--parse-tools' method required for gptel-ollama, since this is
;; handled by its defgeneric implementation

(cl-defmethod gptel--parse-tool-results ((_backend gptel-ollama) tool-use)
  "Return a prompt containing tool call results in TOOL-USE."
  (mapcar (lambda (tool-call)
            (list :role "tool" :content (plist-get tool-call :result)))
          tool-use))

;; NOTE: No `gptel--inject-prompt' method required for gptel-ollama, since this is
;; handled by its defgeneric implementation

(cl-defmethod gptel--parse-list ((backend gptel-ollama) prompt-list)
  (if (consp (car prompt-list))
      (let ((full-prompt))              ; Advanced format, list of lists
        (dolist (entry prompt-list)
          (pcase entry
            (`(prompt . ,msg)
             (push (list :role "user" :content (or (car-safe msg) msg))
                   full-prompt))
            (`(response . ,msg)
             (push (list :role "assistant" :content (or (car-safe msg) msg))
                   full-prompt))
            (`(tool . ,call)
             (push (list :role "assistant"
                         :content ""
                         :tool_calls `[(:function (:name ,(plist-get call :name)
                                                   :arguments ,(plist-get call :args)))])
                   full-prompt)
             (push (car (gptel--parse-tool-results backend (list (cdr entry))))
                   full-prompt))))
        (nreverse full-prompt))
    (cl-loop for text in prompt-list    ; Simple format, list of strings
             for role = t then (not role)
             if text collect
             (list :role (if role "user" "assistant") :content text))))

(cl-defmethod gptel--parse-buffer ((backend gptel-ollama) &optional max-entries)
  (let ((prompts) (prev-pt (point)))
    (if (or gptel-mode gptel-track-response)
        (while (and (or (not max-entries) (>= max-entries 0))
                    (goto-char (previous-single-property-change
                                (point) 'gptel nil (point-min)))
                    (not (= (point) prev-pt)))
          (pcase (get-char-property (point) 'gptel)
            ('response
             (when-let* ((content (gptel--trim-prefixes
                                   (buffer-substring-no-properties (point) prev-pt))))
               (push (list :role "assistant" :content content) prompts)))
            (`(tool . ,_id)
             (save-excursion
               (condition-case nil
                   (let* ((tool-call (read (current-buffer)))
                          (name (plist-get tool-call :name))
                          (arguments (plist-get tool-call :args)))
                     (plist-put tool-call :result
                                (string-trim (buffer-substring-no-properties
                                              (point) prev-pt)))
                     (push (car (gptel--parse-tool-results backend (list tool-call)))
                           prompts)
                     (push (list :role "assistant"
                                 :content ""
                                 :tool_calls `[(:function (:name ,name :arguments ,arguments))])
                           prompts))
                 ((end-of-file invalid-read-syntax)
                  (message (format "Could not parse tool-call on line %s"
                                   (line-number-at-pos (point))))))))
            ('ignore)
            ('nil
             (if gptel-track-media
                 (when-let* ((content (gptel--ollama-parse-multipart
                                       (gptel--parse-media-links major-mode (point) prev-pt))))
                   (when (> (length content) 0)
                     (push (append '(:role "user") content) prompts)))
               (when-let* ((content (gptel--trim-prefixes (buffer-substring-no-properties
                                                           (point) prev-pt))))
                 (push (list :role "user" :content content) prompts)))))
          (setq prev-pt (point))
          (and max-entries (cl-decf max-entries)))
      (let ((content (string-trim (buffer-substring-no-properties
                                   (point-min) (point-max)))))
        (push (list :role "user" :content content) prompts)))
    prompts))

(defun gptel--ollama-parse-multipart (parts)
  "Convert a multipart prompt PARTS to the Ollama API format.

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
   collect text into text-array end
   else if media
   collect (gptel--base64-encode media) into media-array
   else if (plist-get part :textfile)
   collect
   (with-temp-buffer
     (gptel--insert-file-string (plist-get part :textfile))
     (buffer-string))
   into text-array
   finally return
   `(,@(and text-array  (list :content (mapconcat #'identity text-array " ")))
     ,@(and media-array (list :images  (vconcat media-array))))))

(cl-defmethod gptel--inject-media ((_backend gptel-ollama) prompts)
  "Wrap the first user prompt in PROMPTS with included media files.

Media files, if present, are placed in `gptel-context'."
  (when-let* ((media-list (gptel-context--collect-media))
              (media-processed (gptel--ollama-parse-multipart media-list)))
    (cl-callf (lambda (images)
                (vconcat (plist-get media-processed :images)
                         images))
        (plist-get (car prompts) :images))))

;;;###autoload
(cl-defun gptel-make-ollama
    (name &key curl-args header key models stream request-params
          (host "localhost:11434")
          (protocol "http")
          (endpoint "/api/chat"))
  "Register an Ollama backend for gptel with NAME.

Keyword arguments:

CURL-ARGS (optional) is a list of additional Curl arguments.

HOST is where Ollama runs (with port), defaults to localhost:11434

MODELS is a list of available model names, as symbols.
Additionally, you can specify supported LLM capabilities like
vision or tool-use by appending a plist to the model with more
information, in the form

 (model-name . plist)

Currently recognized plist keys are :description, :capabilities
and :mime-types.  An example of a model specification including
both kinds of specs:

:models
\\='(mistral:latest                        ;Simple specs
  openhermes:latest
  (llava:13b                            ;Full spec
   :description
   \"Llava 1.6: Large Lanuage and Vision Assistant\"
   :capabilities (media)
   :mime-types (\"image/jpeg\" \"image/png\")))


STREAM is a boolean to toggle streaming responses, defaults to
false.

PROTOCOL (optional) specifies the protocol, http by default.

ENDPOINT (optional) is the API endpoint for completions, defaults to
\"/api/generate\".

HEADER (optional) is for additional headers to send with each
request.  It should be an alist or a function that retuns an
alist, like:
 ((\"Content-Type\" . \"application/json\"))

KEY (optional) is a variable whose value is the API key, or
function that returns the key.  This is typically not required
for local models like Ollama.

REQUEST-PARAMS (optional) is a plist of additional HTTP request
parameters (as plist keys) and values supported by the API.  Use
these to set parameters that gptel does not provide user options
for.

Example:
-------

 (gptel-make-ollama
   \"Ollama\"
   :host \"localhost:11434\"
   :models \\='(mistral:latest)
   :stream t)"
  (declare (indent 1))
  (let ((backend (gptel--make-ollama
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

(provide 'gptel-ollama)
;;; gptel-ollama.el ends here


