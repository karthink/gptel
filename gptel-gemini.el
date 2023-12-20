;;; gptel-gemini.el ---  Gemini suppport for gptel  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Keywords:

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

;;; Gemini
(cl-defstruct
    (gptel-gemini (:constructor gptel--make-gemini)
                  (:copier nil)
                  (:include gptel-backend)))

(cl-defmethod gptel--parse-response ((_backend gptel-gemini) response _info)
  (map-nested-elt response '(:candidates 0 :content :parts 0 :text)))

(cl-defmethod gptel--request-data ((_backend gptel-gemini) prompts)
  "JSON encode PROMPTS for sending to Gemini."
  (let ((prompts-plist
         `(:contents [,@prompts]
           )))
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
                               (format "[\t\r\n ]*%s[\t\r\n ]*" (regexp-quote (gptel-prompt-prefix-string)))
                               (format "[\t\r\n ]*%s[\t\r\n ]*" (regexp-quote (gptel-response-prefix-string)))))
                  )
            prompts)
      (and max-entries (cl-decf max-entries)))
    prompts))

;;;###autoload
(cl-defun gptel-make-gemini
    (name &key header key
          (host "generativelanguage.googleapis.com")
          (protocol "https")
          (models "gemini-pro")
          (endpoint "/v1beta/models/gemini-pro:generateContent"))

  "Register a Gemini backend for gptel with NAME.

Keyword arguments:

HOST (optional) is the API host, typically \"generativelanguage.googleapis.com\".

MODELS is a list of available model names.

STREAM is a boolean to toggle streaming responses, defaults to
false.

PROTOCOL (optional) specifies the protocol, https by default.

ENDPOINT (optional) is the API endpoint for completions, defaults to
\"/v1beta/models/gemini-pro:generateContent\".

HEADER (optional) is for additional headers to send with each
request. It should be an alist or a function that retuns an
alist, like:
((\"Content-Type\" . \"application/json\"))

KEY (optional) is a variable whose value is the API key, or
function that returns the key."
  (let ((backend (gptel--make-gemini
                  :name name
                  :host host
                  :header header
                  :models models
                  :protocol protocol
                  :endpoint endpoint
                  :stream nil
                  :url (if protocol
                           (concat protocol "://" host endpoint "?key=" key)
                         (concat host endpoint "?key=" key)))))
    (prog1 backend
      (setf (alist-get name gptel--known-backends
                       nil nil #'equal)
            backend))))

(provide 'gptel-gemini)
;;; gptel-gemini.el ends here
