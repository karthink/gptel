;;; gptel-kagi.el --- Kagi support for gptel     -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025  Karthik Chikmagalur

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

;; This file adds support for the Kagi FastGPT LLM API to gptel

;;; Code:
(require 'gptel)
(require 'cl-generic)
(eval-when-compile
  (require 'cl-lib))

(declare-function gptel-context--wrap "gptel-context")

;;; Kagi
(cl-defstruct (gptel-kagi (:constructor gptel--make-kagi)
                            (:copier nil)
                            (:include gptel-backend)))

(cl-defmethod gptel--parse-response ((_backend gptel-kagi) response info)
  (let* ((data (plist-get response :data))
         (output (plist-get data :output))
         (references (plist-get data :references)))
    (if (eq references :null) (setq references nil))
    (if (eq output :null) (setq output nil))
    (when references
      (setq references
            (cl-loop with linker =
                     (pcase (buffer-local-value 'major-mode
                                                (plist-get info :buffer))
                       ('org-mode
                        (lambda (text url)
                          (format "[[%s][%s]]" url text)))
                       ('markdown-mode
                        (lambda (text url)
                          (format "[%s](%s)" text url)))
                       (_ (lambda (text url)
                            (buttonize
                             text (lambda (data) (browse-url data))
                             url))))
                     for ref across references
                     for title = (plist-get ref :title)
                     for snippet = (plist-get ref :snippet)
                     for url = (plist-get ref :url)
                     for n upfrom 1
                     collect
                     (concat (format "[%d] " n)
                             (funcall linker title url) ": "
                             (replace-regexp-in-string
                              "</?b>" "*" snippet))
                     into ref-strings
                     finally return
                     (concat "\n\n" (mapconcat #'identity ref-strings "\n")))))
    (concat output references)))

;; TODO: Add model and backend-specific request-params support
(cl-defmethod gptel--request-data ((_backend gptel-kagi) prompts)
  "JSON encode PROMPTS for Kagi."
  (pcase-exhaustive (gptel--model-name gptel-model)
    ("fastgpt"
     `(,@prompts :web_search t :cache t))
    ((and model (guard (string-prefix-p "summarize" model)))
     `(,@prompts :engine ,(substring model 10)))))

(cl-defmethod gptel--parse-buffer ((_backend gptel-kagi) &optional _max-entries)
  (let ((url (or (thing-at-point 'url)
                 (get-text-property (point) 'shr-url)
                 (get-text-property (point) 'image-url)))
        ;; (filename (thing-at-point 'existing-filename)) ;no file upload support yet
        (prop (text-property-search-backward
               'gptel 'response
               (when (get-char-property (max (point-min) (1- (point)))
                                        'gptel)
                 t))))
    (if (and url (string-prefix-p "summarize" (gptel--model-name gptel-model)))
        (list :url url)
      (if (and (or gptel-mode gptel-track-response)
               (prop-match-p prop)
               (prop-match-value prop))
          (user-error "No user prompt found!")
        (let ((prompts
               (if (or gptel-mode gptel-track-response)
                   (or (gptel--trim-prefixes
                        (buffer-substring-no-properties (prop-match-beginning prop)
                                                        (prop-match-end prop)))
                       "")
                 (string-trim (buffer-substring-no-properties (point-min) (point-max))))))
          (pcase-exhaustive (gptel--model-name gptel-model)
            ("fastgpt" (setq prompts (list :query (if (prop-match-p prop) prompts ""))))
            ((and model (guard (string-prefix-p "summarize" model)))
             ;; If the entire contents of the prompt looks like a url, send the url
             ;; Else send the text of the region
             (setq prompts
                   (if-let* (((prop-match-p prop))
                             (engine (substring model 10)))
                       ;; It's a region of text
                       (list :text prompts)
                     ""))))
          prompts)))))

(cl-defmethod gptel--wrap-user-prompt ((_backend gptel-kagi) prompts)
  (cond
   ((plist-get prompts :url)
    (message "Ignoring gptel context for URL summary request."))
   ((plist-get prompts :query)
    (cl-callf gptel-context--wrap (plist-get prompts :query)))
   ((plist-get prompts :text)
    (cl-callf gptel-context--wrap (plist-get prompts :text)))))

;;;###autoload
(cl-defun gptel-make-kagi
    (name &key curl-args stream key
          (host "kagi.com")
          (header (lambda () `(("Authorization" . ,(concat "Bot " (gptel--get-api-key))))))
          (models '((fastgpt :capabilities (nosystem))
                    (summarize:cecil :capabilities (nosystem))
                    (summarize:agnes :capabilities (nosystem))
                    (summarize:daphne :capabilities (nosystem))
                    (summarize:muriel :capabilities (nosystem))))
          (protocol "https")
          (endpoint "/api/v0/"))
  "Register a Kagi FastGPT backend for gptel with NAME.

Keyword arguments:

CURL-ARGS (optional) is a list of additional Curl arguments.

HOST is the Kagi host (with port), defaults to \"kagi.com\".

MODELS is a list of available Kagi models: only fastgpt is supported.

STREAM is a boolean to toggle streaming responses, defaults to
false.  Kagi does not support a streaming API yet.

PROTOCOL (optional) specifies the protocol, https by default.

ENDPOINT (optional) is the API endpoint for completions, defaults to
\"/api/v0/fastgpt\".

HEADER (optional) is for additional headers to send with each
request.  It should be an alist or a function that retuns an
alist, like:
 ((\"Content-Type\" . \"application/json\"))

KEY (optional) is a variable whose value is the API key, or
function that returns the key.

Example:
-------

 (gptel-make-kagi \"Kagi\" :key my-kagi-key)"
  (declare (indent 1))
  stream                                ;Silence byte-compiler
  (let ((backend (gptel--make-kagi
                  :curl-args curl-args
                  :name name
                  :host host
                  :header header
                  :key key
                  :models (gptel--process-models models)
                  :protocol protocol
                  :endpoint endpoint
                  :url
                  (lambda ()
                    (concat protocol "://" host endpoint
                            (if (equal gptel-model 'fastgpt)
                                "fastgpt" "summarize"))))))
    (prog1 backend
      (setf (alist-get name gptel--known-backends
                       nil nil #'equal)
                  backend))))

(provide 'gptel-kagi)
;;; gptel-kagi.el ends here

;; Local Variables:
;; byte-compile-warnings: (not docstrings)
;; End:
