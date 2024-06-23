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

(declare-function gptel--get-api-key "gptel")
(declare-function prop-match-value "text-property-search")
(declare-function text-property-search-backward "text-property-search")
(declare-function json-read "json")
(declare-function gptel-prompt-prefix-string "gptel")
(declare-function gptel-response-prefix-string "gptel")
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
  endpoint key models url curl-args)

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
  (let ((prompts-plist
         `(:model ,gptel-model
           :messages [,@prompts]
           :stream ,(or (and gptel-stream gptel-use-curl
                         (gptel-backend-stream gptel-backend))
                     :json-false))))
    (when gptel-temperature
      (plist-put prompts-plist :temperature gptel-temperature))
    (when gptel-max-tokens
      (plist-put prompts-plist :max_tokens gptel-max-tokens))
    prompts-plist))

(cl-defmethod gptel--parse-buffer ((_backend gptel-openai) &optional max-entries)
  (let ((prompts) (prop))
    (if (or gptel-mode gptel-track-response)
        (while (and
                (or (not max-entries) (>= max-entries 0))
                (setq prop (text-property-search-backward
                            'gptel 'response
                            (when (get-char-property (max (point-min) (1- (point)))
                                                     'gptel)
                              t))))
          (push (list :role (if (prop-match-value prop) "assistant" "user")
                      :content
                      (string-trim
                       (buffer-substring-no-properties (prop-match-beginning prop)
                                                       (prop-match-end prop))
                       (format "[\t\r\n ]*\\(?:%s\\)?[\t\r\n ]*"
                               (regexp-quote (gptel-prompt-prefix-string)))
                       (format "[\t\r\n ]*\\(?:%s\\)?[\t\r\n ]*"
                               (regexp-quote (gptel-response-prefix-string)))))
                prompts)
          (and max-entries (cl-decf max-entries)))
      (push (list :role "user"
                  :content
                  (string-trim
                   (buffer-substring-no-properties (prop-match-beginning prop)
                                                   (prop-match-end prop))
                   (format "[\t\r\n ]*\\(?:%s\\)?[\t\r\n ]*"
                           (regexp-quote (gptel-prompt-prefix-string)))
                   (format "[\t\r\n ]*\\(?:%s\\)?[\t\r\n ]*"
                           (regexp-quote (gptel-response-prefix-string)))))
            prompts)
      (and max-entries (cl-decf max-entries)))
    (cons (list :role "system"
                :content gptel--system-message)
          prompts)))

(cl-defmethod gptel--wrap-user-prompt ((_backend gptel-openai) prompts)
  "Wrap the last user prompt in PROMPTS with the context string."
  (cl-callf gptel-context--wrap (plist-get (car (last prompts)) :content)))

;;;###autoload
(cl-defun gptel-make-openai
    (name &key curl-args models stream key
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

MODELS is a list of available model names.

STREAM is a boolean to toggle streaming responses, defaults to
false.

PROTOCOL (optional) specifies the protocol, https by default.

ENDPOINT (optional) is the API endpoint for completions, defaults to
\"/v1/chat/completions\".

HEADER (optional) is for additional headers to send with each
request. It should be an alist or a function that retuns an
alist, like:
((\"Content-Type\" . \"application/json\"))

KEY (optional) is a variable whose value is the API key, or
function that returns the key."
  (declare (indent 1))
  (let ((backend (gptel--make-openai
                  :curl-args curl-args
                  :name name
                  :host host
                  :header header
                  :key key
                  :models models
                  :protocol protocol
                  :endpoint endpoint
                  :stream stream
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
          models stream endpoint)
  "Register an Azure backend for gptel with NAME.

Keyword arguments:

CURL-ARGS (optional) is a list of additional Curl arguments.

HOST is the API host.

MODELS is a list of available model names.

STREAM is a boolean to toggle streaming responses, defaults to
false.

PROTOCOL (optional) specifies the protocol, https by default.

ENDPOINT is the API endpoint for completions.

HEADER (optional) is for additional headers to send with each
request. It should be an alist or a function that retuns an
alist, like:
((\"Content-Type\" . \"application/json\"))

KEY (optional) is a variable whose value is the API key, or
function that returns the key.

Example:
-------

(gptel-make-azure
 \"Azure-1\"
 :protocol \"https\"
 :host \"RESOURCE_NAME.openai.azure.com\"
 :endpoint
 \"/openai/deployments/DEPLOYMENT_NAME/completions?api-version=2023-05-15\"
 :stream t
 :models \\='(\"gpt-3.5-turbo\" \"gpt-4\"))"
  (declare (indent 1))
  (let ((backend (gptel--make-openai
                  :curl-args curl-args
                  :name name
                  :host host
                  :header header
                  :key key
                  :models models
                  :protocol protocol
                  :endpoint endpoint
                  :stream stream
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

HOST is where GPT4All runs (with port), typically localhost:8491

MODELS is a list of available model names.

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

Example:
-------

(gptel-make-gpt4all
 \"GPT4All\"
 :protocol \"http\"
 :host \"localhost:4891\"
 :models \\='(\"mistral-7b-openorca.Q4_0.gguf\"))")

(provide 'gptel-openai)
;;; gptel-backends.el ends here
