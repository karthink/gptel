;;; gptel-privategpt.el ---  Privategpt AI suppport for gptel  -*- lexical-binding: t; -*-

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

;; This file adds support for Privategpt's Messages API to gptel

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

(cl-defmethod gptel-curl--parse-stream ((_backend gptel-privategpt) info)
  (let* ((content-strs))
    (condition-case nil
        (while (re-search-forward "^data:" nil t)
          (save-match-data
            (if (looking-at " *\\[DONE\\]")
                (when-let ((sources-string (plist-get info :sources)))
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
           :stream ,(or (and gptel-stream gptel-use-curl
                         (gptel-backend-stream gptel-backend))
                     :json-false))))
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
           (lambda () (when-let (key (gptel--get-api-key))
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

(provide 'gptel-privategpt)
;;; gptel-backends.el ends here
