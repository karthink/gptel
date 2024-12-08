;;; gptel-anthropic.el ---  Anthropic AI suppport for gptel  -*- lexical-binding: t; -*-

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

;; This file adds support for Anthropic's Messages API to gptel

;;; Code:
(require 'cl-generic)
(eval-when-compile
  (require 'cl-lib))
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

(cl-defmethod gptel-curl--parse-stream ((_backend gptel-anthropic) _info)
  (let* ((content-strs)
         (pt (point)))
    (condition-case nil
        (while (re-search-forward "^event: " nil t)
          (setq pt (match-beginning 0))
          (if (equal (line-end-position) (point-max))
              (error "Data block incomplete"))
          (when (looking-at "content_block_\\(?:start\\|delta\\|stop\\)")
            (forward-line 1) (forward-char 5)
            (when-let* ((response (gptel--json-read))
                        (content (map-nested-elt
                                  response '(:delta :text))))
              (push content content-strs))))
      (error (goto-char pt)))
    (apply #'concat (nreverse content-strs))))

(cl-defmethod gptel--parse-response ((_backend gptel-anthropic) response _info)
  (map-nested-elt response '(:content 0 :text)))

(cl-defmethod gptel--request-data ((_backend gptel-anthropic) prompts)
  "JSON encode PROMPTS for sending to ChatGPT."
  (let ((prompts-plist
         `(:model ,(gptel--model-name gptel-model)
           :stream ,(or (and gptel-stream gptel-use-curl
                         (gptel-backend-stream gptel-backend))
                     :json-false)
           :max_tokens ,(or gptel-max-tokens 1024)
           :messages [,@prompts])))
    (when (and gptel--system-message
               (not (gptel--model-capable-p 'nosystem)))
      (plist-put prompts-plist :system gptel--system-message))
    (when gptel-temperature
      (plist-put prompts-plist :temperature gptel-temperature))
    ;; Merge request params with model and backend params.
    (gptel--merge-plists
     prompts-plist
     (gptel-backend-request-params gptel-backend)
     (gptel--model-request-params  gptel-model))))

(cl-defmethod gptel--parse-list ((_backend gptel-anthropic) prompt-list)
  (cl-loop for text in prompt-list
           for role = t then (not role)
           if text collect
           (list :role (if role "user" "assistant")
                 :content `[(:type "text" :text ,text)])))

(cl-defmethod gptel--parse-buffer ((_backend gptel-anthropic) &optional max-entries)
  (let ((prompts) (prop) (prev-pt (point))
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
          ;; HACK Until we can find a more robust solution for editing
          ;; responses, ignore prompts containing only whitespace, as the
          ;; Anthropic API can't handle it.  See #452, #409, #406, #351 and #321
          ;; We check for blank prompts by skipping whitespace and comparing
          ;; point against the previous.
          (unless (save-excursion (skip-syntax-forward " ") (>= (point) prev-pt))
            (if (prop-match-value prop) ; assistant role
                (push (list :role "assistant"
                            :content
                            (buffer-substring-no-properties (prop-match-beginning prop)
                                                            (prop-match-end prop)))
                      prompts)
              (if include-media         ; user role: possibly with media
                  (push (list :role "user"
                              :content
                              (gptel--anthropic-parse-multipart
                               (gptel--parse-media-links
                                major-mode (prop-match-beginning prop) (prop-match-end prop))))
                        prompts)
                (push (list :role "user"
                            :content
                            (gptel--trim-prefixes
                             (buffer-substring-no-properties (prop-match-beginning prop)
                                                             (prop-match-end prop))))
                      prompts))))
          (setq prev-pt (point))
          (and max-entries (cl-decf max-entries)))
      (push (list :role "user"
                  :content
                  (string-trim (buffer-substring-no-properties (point-min) (point-max))))
            prompts))
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
   unless (string-empty-p text)
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
   finally return (vconcat parts-array)))

(cl-defmethod gptel--wrap-user-prompt ((_backend gptel-anthropic) prompts
                                       &optional inject-media)
  "Wrap the last user prompt in PROMPTS with the context string.

If INJECT-MEDIA is non-nil wrap it with base64-encoded media
files in the context."
  (if inject-media
      ;; Wrap the first user prompt with included media files/contexts
      (when-let ((media-list (gptel-context--collect-media)))
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
                  (vector (if-let ((wrapped (gptel-context--wrap nil)))
                              (vconcat `((:type "text" :text ,wrapped))
                                       current)
                            current))))
        (plist-get (car (last prompts)) :content))))

;; (if-let ((context-string (gptel-context--string gptel-context--alist)))
;;     (cl-callf (lambda (previous)
;;                 (cl-typecase previous
;;                   (string (concat context-string previous))
;;                   (vector (vconcat `((:type "text" :text ,previous))
;;                                    previous))
;;                   (t context-string)))
;;         (plist-get (car (last prompts)) :content)))

(defconst gptel--anthropic-models
  '((claude-3-5-sonnet-20241022
     :description "Highest level of intelligence and capability"
     :capabilities (media tool cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 200
     :input-cost 3
     :output-cost 15
     :cutoff-date "2024-04")
    (claude-3-5-sonnet-20240620
     :description "Highest level of intelligence and capability (earlier version)"
     :capabilities (media tool cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 200
     :input-cost 3
     :output-cost 15
     :cutoff-date "2024-04")
    (claude-3-5-haiku-20241022
     :description "Intelligence at blazing speeds"
     :capabilities (media tool)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 200
     :input-cost 1.00
     :output-cost 5.00
     :cutoff-date "2024-07")
    (claude-3-opus-20240229
     :description "Top-level performance, intelligence, fluency, and understanding"
     :capabilities (media tool cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 200
     :input-cost 15
     :output-cost 75
     :cutoff-date "2023-08")
    (claude-3-sonnet-20240229
     :description "Balance of intelligence and speed (legacy model)"
     :capabilities (media tool)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 200
     :input-cost 3
     :output-cost 15
     :cutoff-date "2023-08")
    (claude-3-haiku-20240307
     :description "Fast and most compact model for near-instant responsiveness"
     :capabilities (media tool)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
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

<https://docs.anthropic.com/en/docs/about-claude/models#model-comparison-table>")

;;;###autoload
(cl-defun gptel-make-anthropic
    (name &key curl-args stream key request-params
          (header
           (lambda () (when-let (key (gptel--get-api-key))
                   `(("x-api-key" . ,key)
                     ("anthropic-version" . "2023-06-01")
                     ("anthropic-beta" . "pdfs-2024-09-25")
                     ("anthropic-beta" . "prompt-caching-2024-07-31")))))
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
