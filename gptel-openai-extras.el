;;; gptel-openai-extras.el --- Extensions to the OpenAI API -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025  Karthik Chikmagalur

;; Authors: Karthik Chikmagalur <karthikchikmagalur@gmail.com> and pirminj

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

;; This file adds support for Privategpt's Messages API and
;; Perplexity's Citations feature to gptel

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



;;; Privategpt (Messages API)
(cl-defstruct (gptel-privategpt (:constructor gptel--make-privategpt)
                               (:copier nil)
                               (:include gptel-openai))
  context sources)

(defun gptel--privategpt-parse-sources (response)
  (cl-loop with source-alist
           for source across (map-nested-elt response '(:choices 0 :sources))
           for name = (map-nested-elt source '(:document :doc_metadata :file_name))
           for page = (map-nested-elt source '(:document :doc_metadata :page_label))
           do (push page (alist-get name source-alist nil nil #'equal))
           finally return
           (cl-loop for (file-name . file-pages) in source-alist
                    for pages = (delete-dups (delq nil file-pages))
                    if pages
                    collect (format "- %s (page %s)" file-name (mapconcat #'identity pages ", "))
                    into source-items
                    else collect (format "- %s" file-name) into source-items
                    finally return (mapconcat #'identity (cons "\n\nSources:" source-items) "\n"))))

;; FIXME(tool) add tool use
(cl-defmethod gptel-curl--parse-stream ((_backend gptel-privategpt) info)
  (let* ((content-strs))
    (condition-case nil
        (while (re-search-forward "^data:" nil t)
          (save-match-data
            (if (looking-at " *\\[DONE\\]")
                (when-let* ((sources-string (plist-get info :sources)))
                  (push sources-string content-strs))
              (let ((response (gptel--json-read)))
		(unless (or (plist-get info :sources)
                            (not (gptel-privategpt-sources (plist-get info :backend))))
                  (plist-put info :sources (gptel--privategpt-parse-sources response)))
		(let* ((delta (map-nested-elt response '(:choices 0 :delta)))
		       (content (plist-get delta :content)))
		  (push content content-strs))))))
      (error
       (goto-char (match-beginning 0))))
    (apply #'concat (nreverse content-strs))))

;; FIXME(tool) add tool use
(cl-defmethod gptel--parse-response ((_backend gptel-privategpt) response info)
  (let ((response-string (map-nested-elt response '(:choices 0 :message :content)))
        (sources-string (and (gptel-privategpt-sources (plist-get info :backend))
                             (gptel--privategpt-parse-sources response))))
    (concat response-string sources-string)))

(cl-defmethod gptel--request-data ((_backend gptel-privategpt) prompts)
  "JSON encode PROMPTS for sending to ChatGPT."
  (let ((prompts-plist
         `(:model ,(gptel--model-name gptel-model)
	   :messages [,@prompts]
	   :use_context ,(or (gptel-privategpt-context gptel-backend) :json-false)
	   :include_sources ,(or (gptel-privategpt-sources gptel-backend) :json-false)
           :stream ,(or gptel-stream :json-false))))
    (when (and gptel--system-message
               (not (gptel--model-capable-p 'nosystem)))
      (plist-put prompts-plist :system gptel--system-message))
    (when gptel-temperature
      (plist-put prompts-plist :temperature gptel-temperature))
    (when gptel-max-tokens
      (plist-put prompts-plist :max_tokens gptel-max-tokens))
    ;; Merge request params with model and backend params.
    (gptel--merge-plists
     prompts-plist
     (gptel-backend-request-params gptel-backend)
     (gptel--model-request-params  gptel-model))))


;;;###autoload
(cl-defun gptel-make-privategpt
    (name &key curl-args stream key request-params
          (header
           (lambda () (when-let* ((key (gptel--get-api-key)))
		   `(("Authorization" . ,(concat "Bearer " key))))))
          (host "localhost:8001")
          (protocol "http")
	  (models '(private-gpt))
          (endpoint "/v1/chat/completions")
          (context t) (sources t))
  "Register an Privategpt API-compatible backend for gptel with NAME.

Keyword arguments:

CURL-ARGS (optional) is a list of additional Curl arguments.

HOST (optional) is the API host, \"api.privategpt.com\" by default.

MODELS is a list of available model names.

STREAM is a boolean to toggle streaming responses, defaults to
false.

PROTOCOL (optional) specifies the protocol, https by default.

ENDPOINT (optional) is the API endpoint for completions, defaults to
\"/v1/messages\".

HEADER (optional) is for additional headers to send with each
request. It should be an alist or a function that retuns an
alist, like:
((\"Content-Type\" . \"application/json\"))

KEY is a variable whose value is the API key, or function that
returns the key.

CONTEXT and SOURCES: if true (the default), use available context
and provide sources used by the model to generate the response.

REQUEST-PARAMS (optional) is a plist of additional HTTP request
parameters (as plist keys) and values supported by the API.  Use
these to set parameters that gptel does not provide user options
for."
  (declare (indent 1))
  (let ((backend (gptel--make-privategpt
                  :curl-args curl-args
                  :name name
                  :host host
                  :header header
                  :key key
                  :models models
                  :protocol protocol
                  :endpoint endpoint
                  :stream stream
                  :request-params request-params
                  :url (if protocol
                           (concat protocol "://" host endpoint)
                         (concat host endpoint))
                  :context context
                  :sources sources)))
    (prog1 backend
      (setf (alist-get name gptel--known-backends
                       nil nil #'equal)
            backend))))


;;; Perplexity
(cl-defstruct (gptel-perplexity (:constructor gptel--make-perplexity)
				(:copier nil)
				(:include gptel-openai)))

(defsubst gptel--perplexity-parse-citations (citations)
  (let ((counter 0))
    (concat "\n\nCitations:\n"
            (mapconcat (lambda (url)
                         (setq counter (1+ counter))
                         (format "[%d] %s" counter url))
                       citations "\n"))))

(cl-defmethod gptel--parse-response ((_backend gptel-perplexity) response _info)
  "Parse Perplexity response RESPONSE."
  (let ((response-string (map-nested-elt response '(:choices 0 :message :content)))
        (citations-string (when-let* ((citations (map-elt response :citations)))
			    (gptel--perplexity-parse-citations citations))))
    (concat response-string citations-string)))

(cl-defmethod gptel-curl--parse-stream ((_backend gptel-perplexity) info)
  "Parse a Perplexity API data stream with INFO.

If available, collect citations at the end and include them with
the response."
  (let ((resp (cl-call-next-method)))
    (unless (plist-get info :citations)
      (save-excursion
        (goto-char (point-max))
        (when (search-backward (plist-get info :token)
                               (line-beginning-position) t)
          (forward-line 0)
          (when (re-search-backward "^data: " nil t)
            (goto-char (match-end 0))
            (ignore-errors
              (when-let* ((chunk (gptel--json-read))
                          (citations (map-elt chunk :citations)))
                (plist-put info :citations t)
                (setq resp (concat resp (gptel--perplexity-parse-citations
                                         citations)))))))))
    resp))

;;;###autoload
(cl-defun gptel-make-perplexity
    (name &key curl-args stream key
          (header
           (lambda () (when-let* ((key (gptel--get-api-key)))
                   `(("Authorization" . ,(concat "Bearer " key))))))
          (host "api.perplexity.ai")
          (protocol "https")
          ;; https://docs.perplexity.ai/guides/model-cards
          (models '(sonar sonar-pro sonar-reasoning sonar-reasoning-pro sonar-deep-research))
          (endpoint "/chat/completions")
          request-params)
  "Register a Perplexity backend for gptel with NAME.

Keyword arguments:

CURL-ARGS (optional) is a list of additional Curl arguments.

HOST (optional) is the API host, \"api.perplexity.ai\" by default.

MODELS is a list of available model names.

STREAM is a boolean to toggle streaming responses.

PROTOCOL (optional) specifies the protocol, https by default.

ENDPOINT (optional) is the API endpoint for completions.

HEADER (optional) is for additional headers to send with each
request. It should be an alist or a function that returns an
alist.

KEY is a variable whose value is the API key, or function that
returns the key.

REQUEST-PARAMS (optional) is a plist of additional HTTP request
parameters."
  (declare (indent 1))
  (let ((backend (gptel--make-perplexity
                  :curl-args curl-args
                  :name name
                  :host host
                  :header header
                  :key key
                  :models models
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

;;; Deepseek
(cl-defstruct (gptel-deepseek (:include gptel-openai)
                              (:copier nil)
                              (:constructor gptel--make-deepseek)))

(cl-defmethod gptel--parse-buffer :around ((_backend gptel-deepseek) _max-entries)
  "Merge successive prompts in the prompts list that have the same role.

The Deepseek API requires strictly alternating roles (user/assistant) in messages."
  (let* ((prompts (cl-call-next-method))
         (index prompts))
    (prog1 prompts
      (while index
        (let ((p1 (car index))
              (p2 (cadr index))
              (rest (cdr index)))
          (when (and p2 (equal (plist-get p1 :role)
                               (plist-get p2 :role)))
            (setf (plist-get p1 :content)
                  (concat (plist-get p1 :content) "\n"
                          (plist-get p2 :content)))
            (setcdr index (cdr rest)))
          (setq index (cdr index)))))))

;;;###autoload
(cl-defun gptel-make-deepseek
    (name &key curl-args stream key request-params
          (header (lambda () (when-let* ((key (gptel--get-api-key)))
                          `(("Authorization" . ,(concat "Bearer " key))))))
          (host "api.deepseek.com")
          (protocol "https")
          (endpoint "/v1/chat/completions")
          (models '((deepseek-reasoner
                     :capabilities (tool reasoning)
                     :context-window 128
                     :input-cost 0.56
                     :output-cost 1.68)
                    (deepseek-chat
                     :capabilities (tool)
                     :context-window 128
                     :input-cost 0.56
                     :output-cost 1.68))))
  "Register a DeepSeek backend for gptel with NAME.

For the meanings of the keyword arguments, see `gptel-make-openai'."
  (declare (indent 1))
  (let ((backend (gptel--make-deepseek
                  :name name
                  :host host
                  :header header
                  :key key
                  :models (gptel--process-models models)
                  :protocol protocol
                  :endpoint endpoint
                  :stream stream
                  :request-params request-params
                  :curl-args curl-args
                  :url (concat protocol "://" host endpoint))))
    (setf (alist-get name gptel--known-backends nil nil #'equal) backend)
    backend))

;;; xAI
;;;###autoload
(cl-defun gptel-make-xai
    (name &key curl-args stream key request-params
          (header (lambda () (when-let* ((key (gptel--get-api-key)))
                          `(("Authorization" . ,(concat "Bearer " key))))))
          (host "api.x.ai")
          (protocol "https")
          (endpoint "/v1/chat/completions")
          (models
           '((grok-4
              :description "Grok Flagship model"
              :capabilities (tool-use json reasoning)
              :context-window 256
              :input-cost 3
              :output-cost 15)

             (grok-code-fast-1
              :description "Fast reasoning model for agentic coding"
              :capabilities (tool-use json reasoning)
              :context-window 256
              :input-cost 0.2
              :output-cost 1.5)

             (grok-3
              :description "Grok 3"
              :capabilities (tool-use json reasoning)
              :context-window 131
              :input-cost 3
              :output-cost 15)

             (grok-3-fast
              :description "Faster Grok 3"
              :capabilities (tool-use json reasoning)
              :context-window 131
              :input-cost 5
              :output-cost 25)

             (grok-3-mini
              :description "Mini Grok 3"
              :capabilities (tool-use json reasoning)
              :context-window 131
              :input-cost 0.3
              :output-cost 0.5)

             (grok-3-mini-fast
              :description "Faster mini Grok 3"
              :capabilities (tool-use json reasoning)
              :context-window 131072
              :input-cost 0.6
              :output-cost 4)

             (grok-2-vision-1212
              :description "Grok 2 Vision"
              :capabilities (tool-use json media)
              :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
              :context-window 32768
              :input-cost 2
              :output-cost 10))))
  "Register an xAI backend for gptel with NAME.

Keyword arguments:

KEY is a variable whose value is the API key, or function that
returns the key.

STREAM is a boolean to toggle streaming responses, defaults to
false.

The other keyword arguments are all optional.  For their meanings
see `gptel-make-openai'."
  (declare (indent 1))
  (let ((backend (gptel--make-deepseek
                  :name name
                  :host host
                  :header header
                  :key key
                  :models (gptel--process-models models)
                  :protocol protocol
                  :endpoint endpoint
                  :stream stream
                  :request-params request-params
                  :curl-args curl-args
                  :url (concat protocol "://" host endpoint))))
    (setf (alist-get name gptel--known-backends nil nil #'equal) backend)
    backend))


(provide 'gptel-openai-extras)
;;; gptel-openai-extras.el ends here

;; Local Variables:
;; byte-compile-warnings: (not docstrings)
;; End:
