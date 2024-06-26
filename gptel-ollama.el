;;; gptel-ollama.el --- Ollama support for gptel     -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Karthik Chikmagalur

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
(defvar json-object-type)

;;; Ollama
(cl-defstruct (gptel-ollama (:constructor gptel--make-ollama)
                            (:copier nil)
                            (:include gptel-backend)))

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
                (response (map-nested-elt content '(:message :content))))
            (push response content-strs)
            (unless (eq done :json-false)
              (with-current-buffer (plist-get info :buffer)
                (cl-incf gptel--ollama-token-count
                         (+ (or (map-elt content :prompt_eval_count) 0)
                            (or (map-elt content :eval_count) 0))))
              (goto-char (point-max)))))
      (error (goto-char pt)))
    (apply #'concat (nreverse content-strs))))

(cl-defmethod gptel--parse-response ((_backend gptel-ollama) response info)
  "Parse a one-shot RESPONSE from the Ollama API."
  (when-let ((context
              (+ (or (map-elt response :prompt_eval_count) 0)
                 (or (map-elt response :eval_count) 0))))
    (with-current-buffer (plist-get info :buffer)
      (cl-incf gptel--ollama-token-count context)))
  (map-nested-elt response '(:message :content)))

(cl-defmethod gptel--request-data ((_backend gptel-ollama) prompts)
  "JSON encode PROMPTS for sending to ChatGPT."
  (let ((prompts-plist
         `(:model ,gptel-model
           :messages [,@prompts]
           :stream ,(or (and gptel-stream gptel-use-curl
                         (gptel-backend-stream gptel-backend))
                     :json-false)))
        ;; TODO num_ctx chosen according to #330, make customizable
        (options-plist '(:num_ctx 8192)))
    (when gptel-temperature
      (setq options-plist
            (plist-put options-plist :temperature
                       gptel-temperature)))
    (when gptel-max-tokens
      (setq options-plist
            (plist-put options-plist :num_predict
                       gptel-max-tokens)))
    (when options-plist
      (plist-put prompts-plist :options options-plist))
    prompts-plist))

(cl-defmethod gptel--parse-buffer ((_backend gptel-ollama) &optional max-entries)
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
                  (string-trim (buffer-substring-no-properties (point-min) (point-max))))
            prompts))
    (cons (list :role "system"
                :content gptel--system-message)
          prompts)))

(cl-defmethod gptel--wrap-user-prompt ((_backend gptel-ollama) prompts)
  "Wrap the last user prompt in PROMPTS with the context string."
  (cl-callf gptel-context--wrap (plist-get (car (last prompts)) :content)))

;;;###autoload
(cl-defun gptel-make-ollama
    (name &key curl-args header key models stream
          (host "localhost:11434")
          (protocol "http")
          (endpoint "/api/chat"))
  "Register an Ollama backend for gptel with NAME.

Keyword arguments:

CURL-ARGS (optional) is a list of additional Curl arguments.

HOST is where Ollama runs (with port), defaults to localhost:11434

MODELS is a list of available model names.

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

Example:
-------

(gptel-make-ollama
  \"Ollama\"
  :host \"localhost:11434\"
  :models \\='(\"mistral:latest\")
  :stream t)"
  (declare (indent 1))
  (let ((backend (gptel--make-ollama
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

(provide 'gptel-ollama)
;;; gptel-ollama.el ends here


