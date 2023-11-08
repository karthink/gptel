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

;;; Ollama
(cl-defstruct (gptel-ollama (:constructor gptel--make-ollama)
                            (:copier nil)
                            (:include gptel-backend)))

(defvar-local gptel--ollama-context nil
  "Context for ollama conversations.

This variable holds the context array for conversations with
Ollama models.")

(cl-defmethod gptel-curl--parse-stream ((_backend gptel-ollama) info)
  ";TODO: "
  (when (bobp)
    (re-search-forward "^{")
    (forward-line 0))
  (let* ((json-object-type 'plist)
         (content-strs)
         (content))
    (condition-case nil
        (while (setq content (json-read))
          (let ((done (map-elt content :done))
                (response (map-elt content :response)))
            (push response content-strs)
            (unless (eq done json-false)
              (with-current-buffer (plist-get info :buffer)
                (setq gptel--ollama-context (map-elt content :context)))
              (goto-char (point-max)))))
      (error (forward-line 0)))
    (apply #'concat (nreverse content-strs))))

(cl-defmethod gptel--parse-response ((_backend gptel-ollama) response info)
  (when-let ((context (map-elt response :context)))
    (with-current-buffer (plist-get info :buffer)
      (setq gptel--ollama-context context)))
  (map-elt response :response))

(cl-defmethod gptel--request-data ((_backend gptel-ollama) prompts)
  "JSON encode PROMPTS for sending to ChatGPT."
  (let ((prompts-plist
         `(:model ,gptel-model
           ,@prompts
           :stream ,(or (and gptel-stream gptel-use-curl
                             (gptel-backend-stream gptel-backend))
                     :json-false))))
    (when gptel--ollama-context
      (plist-put prompts-plist :context gptel--ollama-context))
    prompts-plist))

(cl-defmethod gptel--parse-buffer ((_backend gptel-ollama) &optional _max-entries)
  (let ((prompts)
        (prop (text-property-search-backward
               'gptel 'response
               (when (get-char-property (max (point-min) (1- (point)))
                                        'gptel)
                 t))))
    (if (and (prop-match-p prop)
             (prop-match-value prop))
        (user-error "No user prompt found!")
      (setq prompts (list
                     :system gptel--system-message
                     :prompt
                     (if (prop-match-p prop)
                         (string-trim (buffer-substring-no-properties (prop-match-beginning prop)
                                                                      (prop-match-end prop))
                                      "[*# \t\n\r]+")
                       ""))))))

;;;###autoload
(cl-defun gptel-make-ollama
    (name &key host header key models stream
          (protocol "http")
          (endpoint "/api/generate"))
  "Register an Ollama backend for gptel with NAME.

Keyword arguments:

HOST is where Ollama runs (with port), typically localhost:11434

MODELS is a list of available model names.

STREAM is a boolean to toggle streaming responses, defaults to
false.

PROTOCOL (optional) specifies the protocol, http by default.

ENDPOINT (optional) is the API endpoint for completions, defaults to
\"/api/generate\".

HEADER (optional) is for additional headers to send with each
request. It should be an alist or a function that retuns an
alist, like:
((\"Content-Type\" . \"application/json\"))

KEY (optional) is a variable whose value is the API key, or
function that returns the key. This is typically not required for
local models like Ollama.

Example:
-------

(gptel-make-ollama
  \"Ollama\"
  :host \"localhost:11434\"
  :models \\='(\"mistral:latest\")
  :stream t)"
  (let ((backend (gptel--make-ollama
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


