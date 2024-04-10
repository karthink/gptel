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
    prompts-plist))

(cl-defmethod gptel--parse-buffer ((_backend gptel-gemini) &optional max-entries)
  (let ((prompts) (prop))
    (while (and
            (or (not max-entries) (>= max-entries 0))
            (setq prop (text-property-search-backward
                        'gptel 'response
                        (when (get-char-property (max (point-min) (1- (point)))
                                                 'gptel)
                          t))))
      (push (list :role (if (prop-match-value prop) "model" "user")
                  :parts
                  (list :text (string-trim
                               (buffer-substring-no-properties (prop-match-beginning prop)
                                                               (prop-match-end prop))
                               (format "[\t\r\n ]*\\(?:%s\\)?[\t\r\n ]*"
                                       (regexp-quote (gptel-prompt-prefix-string)))
                               (format "[\t\r\n ]*\\(?:%s\\)?[\t\r\n ]*"
                                       (regexp-quote (gptel-response-prefix-string))))))
            prompts)
      (and max-entries (cl-decf max-entries)))
    (cl-callf (lambda (msg) (concat gptel--system-message "\n\n" msg))
        (thread-first (car prompts)
                      (plist-get :parts)
                      (plist-get :text)))
    prompts))

;;;###autoload
(cl-defun gptel-make-gemini
    (name &key curl-args header key (stream nil)
          (host "generativelanguage.googleapis.com")
          (protocol "https")
          (models '("gemini-pro"
                    "gemini-1.5-pro-latest"))
          (endpoint "/v1beta/models"))

  "Register a Gemini backend for gptel with NAME.

Keyword arguments:

CURL-ARGS (optional) is a list of additional Curl arguments.

HOST (optional) is the API host, defaults to
\"generativelanguage.googleapis.com\".

MODELS is a list of available model names.

STREAM is a boolean to enable streaming responses, defaults to
false.

PROTOCOL (optional) specifies the protocol, \"https\" by default.

ENDPOINT (optional) is the API endpoint for completions, defaults to
\"/v1beta/models\".

HEADER (optional) is for additional headers to send with each
request. It should be an alist or a function that retuns an
alist, like:
((\"Content-Type\" . \"application/json\"))

KEY (optional) is a variable whose value is the API key, or
function that returns the key."
  (declare (indent 1))
  (let ((backend (gptel--make-gemini
                  :curl-args curl-args
                  :name name
                  :host host
                  :header header
                  :models models
                  :protocol protocol
                  :endpoint endpoint
                  :stream stream
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
