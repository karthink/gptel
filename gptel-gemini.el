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

(cl-defmethod gptel-curl--parse-stream ((_backend gptel-gemini) _info)
  (let* ((content-strs))
    (condition-case nil
        ;; while-let is Emacs 29.1+ only
        (while (prog1 (search-forward "{" nil t)
                 (backward-char 1))
          (save-match-data
            (when-let*
                ((response (gptel--json-read))
                 (text (map-nested-elt
                        response '(:candidates 0 :content :parts 0 :text))))
              (push text content-strs))))
      (error
       (goto-char (match-beginning 0))))
    (apply #'concat (nreverse content-strs))))

(cl-defmethod gptel--parse-response ((_backend gptel-gemini) response _info)
  (map-nested-elt response '(:candidates 0 :content :parts 0 :text)))

(cl-defmethod gptel--request-data ((_backend gptel-gemini) prompts)
  "JSON encode PROMPTS for sending to Gemini."
  ;; HACK (backwards compatibility) Prepend the system message to the first user
  ;; prompt, but only for gemini-pro.
  (when (and (equal gptel-model 'gemini-pro) gptel--system-message)
    (cl-callf
        (lambda (msg)
          (vconcat `((:text ,(concat gptel--system-message "\n\n"))) msg))
        (thread-first (car prompts)
                      (plist-get :parts))))
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
    ;; HACK only gemini-pro doesn't support system messages.  Need a less hacky
    ;; way to do this.
    (if (and gptel--system-message
             (not (gptel--model-capable-p 'nosystem))
             (not (equal gptel-model 'gemini-pro)))
      (plist-put prompts-plist :system_instruction
                 `(:parts (:text ,gptel--system-message))))
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

(cl-defmethod gptel--parse-list ((_backend gptel-gemini) prompt-list)
  (cl-loop for text in prompt-list
           for role = t then (not role)
           if text
           if role
           collect (list :role "user" :parts `[(:text ,text)]) into prompts
           else collect (list :role "model" :parts `(:text ,text)) into prompts
           finally return prompts))

(cl-defmethod gptel--parse-buffer ((_backend gptel-gemini) &optional max-entries)
  (let ((prompts) (prop)
        (include-media (and gptel-track-media (or (gptel--model-capable-p 'media)
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
              (push (list :role "model"
                          :parts
                          (list :text (buffer-substring-no-properties (prop-match-beginning prop)
                                                                      (prop-match-end prop))))
                    prompts)
            (if include-media
                (push (list :role "user"
                            :parts (gptel--gemini-parse-multipart
                                    (gptel--parse-media-links
                                     major-mode (prop-match-beginning prop) (prop-match-end prop))))
                      prompts)
              (push (list :role "user"
                          :parts
                          `[(:text ,(gptel--trim-prefixes
                                     (buffer-substring-no-properties (prop-match-beginning prop)
                                      (prop-match-end prop))))])
                    prompts)))
          (and max-entries (cl-decf max-entries)))
      (push (list :role "user"
                  :parts
                  `[(:text ,(string-trim (buffer-substring-no-properties (point-min) (point-max))))])
            prompts))
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
   unless (string-empty-p text)
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
      (when-let ((media-list (gptel-context--collect-media)))
        (cl-callf (lambda (current)
                    (vconcat (gptel--gemini-parse-multipart media-list)
                             current))
            (plist-get (car prompts) :parts)))
    ;; Wrap the last user prompt with included text contexts
    (cl-callf (lambda (current)
                (if-let ((wrapped (gptel-context--wrap nil)))
                    (vconcat `((:text ,wrapped)) current)
                  current))
        (plist-get (car (last prompts)) :parts))))

(defconst gptel--gemini-models
  '((gemini-pro
     :description "The previous generation of Google's multimodal AI model"
     :capabilities (tool json media)
     :mime-types ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
                  "application/pdf" "text/plain" "text/csv" "text/html")
     :context-window 32
     :input-cost 0.50
     :output-cost 1.50
     :cutoff-date "2023-02")
    (gemini-1.5-flash
     :description "A faster, more efficient version of Gemini 1.5 optimized for speed"
     :capabilities (tool json media)
     :mime-types ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
                  "application/pdf" "text/plain" "text/csv" "text/html")
     :context-window 1000
     ;; input & output price is halved for prompts of 128k tokens or less
     :input-cost 0.15
     :output-cost 0.60
     :cutoff-date "2024-05")
    (gemini-1.5-pro-latest
     :description "Google's latest model with enhanced capabilities across various tasks"
     :capabilities (tool json media)
     :mime-types ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
                  "application/pdf" "text/plain" "text/csv" "text/html")
     :context-window 2000
     ;; input & output price is halved for prompts of 128k tokens or less
     :input-cost 2.50
     :output-cost 10
     :cutoff-date "2024-05"))
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
- <https://cloud.google.com/vertex-ai/generative-ai/docs/learn/models>")

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
\\='(gemini-pro                            ;Simple specs
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
                         (let ((method (if (and stream
                                                gptel-stream)
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
