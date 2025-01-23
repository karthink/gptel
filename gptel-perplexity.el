;;; gptel-perplexity.el ---  Perplexity AI support for gptel  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Free Software Contributors

;; Author: pirminj

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

;; This file adds support for Perplexity AI API to gptel

;;; Code:
(require 'cl-generic)
(eval-when-compile
  (require 'cl-lib))
(require 'map)
(require 'gptel)

(defvar json-object-type)

(cl-defstruct (gptel-perplexity (:constructor gptel--make-perplexity)
				(:copier nil)
				(:include gptel-openai)))

(defun gptel--perplexity-parse-citations (response)
  "Parse citations from Perplexity RESPONSE."
  (when-let ((citations (map-elt response :citations)))
    (concat "\n\nSources:\n"
            (mapconcat (lambda (url) (format "- %s" url))
                      citations "\n"))))

(cl-defmethod gptel--parse-response ((_backend gptel-perplexity) response info)
  "Parse Perplexity response RESPONSE with INFO."
  (let ((response-string (map-nested-elt response '(:choices 0 :message :content)))
        (citations-string (gptel--perplexity-parse-citations response)))
    (concat response-string citations-string)))

(cl-defmethod gptel--request-data ((_backend gptel-perplexity) prompts)
  "JSON encode PROMPTS for sending to Perplexity."
  (let ((prompts-plist
         `(:model ,(gptel--model-name gptel-model)
           :messages [,@prompts]
           :stream ,(or gptel-stream :json-false))))
    (when (and gptel--system-message
               (not (gptel--model-capable-p 'nosystem)))
      (plist-put prompts-plist :system gptel--system-message))
    (when gptel-temperature
      (plist-put prompts-plist :temperature gptel-temperature))
    (when gptel-max-tokens
      (plist-put prompts-plist :max_tokens gptel-max-tokens))
    (gptel--merge-plists
     prompts-plist
     (gptel-backend-request-params gptel-backend)
     (gptel--model-request-params gptel-model))))

;;;###autoload
(cl-defun gptel-make-perplexity
    (name &key curl-args stream key
          (header 
           (lambda () (when-let (key (gptel--get-api-key))
                       `(("Authorization" . ,(concat "Bearer " key))))))
          (host "api.perplexity.ai")
          (protocol "https")
          (models '(sonar sonar-pro))
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

(provide 'gptel-perplexity)
;;; gptel-perplexity.el ends here



