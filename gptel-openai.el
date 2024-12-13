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

(defmacro gptel--json-read ()
  (if (fboundp 'json-parse-buffer)
      `(json-parse-buffer
        :object-type 'plist
        :null-object nil
        :false-object :json-false)
    (require 'json)
    (defvar json-object-type)
    (declare-function json-read "json" ())
    `(let ((json-object-type 'plist))
      (json-read))))

(defmacro gptel--json-encode (object)
  (if (fboundp 'json-serialize)
      `(json-serialize ,object
        :null-object nil
        :false-object :json-false)
    (require 'json)
    (defvar json-false)
    (declare-function json-encode "json" (object))
    `(let ((json-false :json-false))
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

(cl-defmethod gptel-curl--parse-stream ((_backend gptel-openai) _info)
  (let* ((content-strs))
    (condition-case nil
        (while (re-search-forward "^data:" nil t)
          (save-match-data
            (unless (looking-at " *\\[DONE\\]")
              (when-let* ((response (gptel--json-read))
                          (delta (map-nested-elt
                                  response '(:choices 0 :delta)))
                          (content (plist-get delta :content)))
                (push content content-strs)))))
      (error
       (goto-char (match-beginning 0))))
    (apply #'concat (nreverse content-strs))))

(cl-defmethod gptel--parse-response ((_backend gptel-openai) response _info)
  (map-nested-elt response '(:choices 0 :message :content)))

(cl-defmethod gptel--request-data ((_backend gptel-openai) prompts)
  "JSON encode PROMPTS for sending to ChatGPT."
  (when (and gptel--system-message
             (not (gptel--model-capable-p 'nosystem)))
    (push (list :role "system"
                :content gptel--system-message)
          prompts))
  (let ((prompts-plist
         `(:model ,(gptel--model-name gptel-model)
           :messages [,@prompts]
           :stream ,(or (and gptel-stream gptel-use-curl
                         (gptel-backend-stream gptel-backend))
                     :json-false))))
    (when gptel-temperature
      (plist-put prompts-plist :temperature gptel-temperature))
    (when gptel-max-tokens
      ;; HACK: The OpenAI API has deprecated max_tokens, but we still need it
      ;; for OpenAI-compatible APIs like GPT4All (#485)
      (plist-put prompts-plist (if (memq gptel-model '(o1-preview o1-mini))
                                   :max_completion_tokens :max_tokens)
                 gptel-max-tokens))
    ;; Merge request params with model and backend params.
    (gptel--merge-plists
     prompts-plist
     (gptel-backend-request-params gptel-backend)
     (gptel--model-request-params  gptel-model))))

(cl-defmethod gptel--parse-list ((_backend gptel-openai) prompt-list)
  (cl-loop for text in prompt-list
           for role = t then (not role)
           if text collect
           (list :role (if role "user" "assistant") :content text)))

(cl-defmethod gptel--parse-buffer ((_backend gptel-openai) &optional max-entries)
  (let ((prompts) (prop)
        (include-media (and gptel-track-media
                            (or (gptel--model-capable-p 'media)
                                (gptel--model-capable-p 'url)))))
    (if (or gptel-mode gptel-track-response)
        (while (and
                (or (not max-entries) (>= max-entries 0))
                (setq prop (text-property-search-backward
                            'gptel 'response
                            (when (get-char-property (max (point-min) (1- (point)))
                                                     'gptel)
                              t))))
          (if (prop-match-value prop)   ;assistant role
              (push (list :role "assistant"
                          :content
                          (buffer-substring-no-properties (prop-match-beginning prop)
                                                          (prop-match-end prop)))
                    prompts)
            (if include-media
                (push (list :role "user"
                            :content
                            (gptel--openai-parse-multipart
                             (gptel--parse-media-links
                              major-mode (prop-match-beginning prop) (prop-match-end prop))))
                      prompts)
              (push (list :role "user"
                          :content
                          (gptel--trim-prefixes
                           (buffer-substring-no-properties (prop-match-beginning prop)
                                                           (prop-match-end prop))))
                    prompts)))
          (and max-entries (cl-decf max-entries)))
      (push (list :role "user"
                  :content
                  (gptel--trim-prefixes (buffer-substring-no-properties (point-min) (point-max))))
            prompts))
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
   (and (or (= n 1) (= n last)) (setq text (gptel--trim-prefixes text))) and
   unless (string-empty-p text)
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
      (when-let ((media-list (gptel-context--collect-media)))
        (cl-callf (lambda (current)
                    (vconcat
                     (gptel--openai-parse-multipart media-list)
                     (cl-typecase current
                       (string `((:type "text" :text ,current)))
                       (vector current)
                       (t current))))
            (plist-get (cadr prompts) :content)))
    ;; Wrap the last user prompt with included text contexts
    (cl-callf (lambda (current)
                (cl-etypecase current
                  (string (gptel-context--wrap current))
                  (vector (if-let ((wrapped (gptel-context--wrap nil)))
                              (vconcat `((:type "text" :text ,wrapped))
                                       current)
                            current))))
        (plist-get (car (last prompts)) :content))))

;;;###autoload
(cl-defun gptel-make-openai
    (name &key curl-args models stream key request-params
          (header
           (lambda () (when-let (key (gptel--get-api-key))
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
