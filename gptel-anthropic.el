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
(declare-function json-read "json")

;;; Anthropic (Messages API)
(cl-defstruct (gptel-anthropic (:constructor gptel--make-anthropic)
                               (:copier nil)
                               (:include gptel-backend)))

(cl-defmethod gptel-curl--parse-stream ((_backend gptel-anthropic) _info)
  (let* ((json-object-type 'plist)
         (content-strs)
         (pt (point)))
    (condition-case nil
        (while (re-search-forward "^event: " nil t)
          (setq pt (match-beginning 0))
          (cond
           ((looking-at "content_block_\\(?:start\\|delta\\|stop\\)")
            (save-match-data
              (forward-line 1) (forward-char 5)
              (when-let* ((response (json-read))
                          (content (map-nested-elt
                                    response '(:delta :text))))
                (push content content-strs))))))
      (error (goto-char pt)))
    (apply #'concat (nreverse content-strs))))

(cl-defmethod gptel--parse-response ((_backend gptel-anthropic) response _info)
  (with-current-buffer (get-buffer "*gptel-log*")
    (princ response))
  (map-nested-elt response '(:content 0 :text)))

(cl-defmethod gptel--request-data ((_backend gptel-anthropic) prompts)
  "JSON encode PROMPTS for sending to ChatGPT."
  (let ((prompts-plist
         `(:model ,gptel-model
           :messages [,@prompts]
           :system ,gptel--system-message
           :stream ,(or (and gptel-stream gptel-use-curl
                         (gptel-backend-stream gptel-backend))
                     :json-false)
           :max_tokens ,(or gptel-max-tokens 1024))))
    (when gptel-temperature
      (plist-put prompts-plist :temperature gptel-temperature))
    prompts-plist))

(cl-defmethod gptel--parse-buffer ((_backend gptel-anthropic) &optional max-entries)
  (let ((prompts) (prop))
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
    prompts))

;;;###autoload
(cl-defun gptel-make-anthropic
    (name &key curl-args stream key
          (header
           (lambda () (when-let (key (gptel--get-api-key))
                        `(("x-api-key" . ,key)
                          ("anthropic-version" . "2023-06-01")))))
          (models '("claude-3-sonnet-20240229" "claude-3-opus-20240229"))
          (host "api.anthropic.com")
          (protocol "https")
          (endpoint "/v1/messages"))
  "Register an Anthropic API-compatible backend for gptel with NAME.

Keyword arguments:

CURL-ARGS (optional) is a list of additional Curl arguments.

HOST (optional) is the API host, \"api.anthropic.com\" by default.

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
returns the key."
  (declare (indent 1))
  (let ((backend (gptel--make-anthropic
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

(provide 'gptel-anthropic)
;;; gptel-backends.el ends here
