;;; gptel-deepseek.el --- DeepSeek support for gptel  -*- lexical-binding: t; -*-

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
;; This file adds support for DeepSeek's API to gptel.

;;; Code:

(require 'gptel)
(require 'gptel-openai)
(require 'cl-generic)
(eval-when-compile
  (require 'cl-lib))

(cl-defstruct (gptel-deepseek (:include gptel-openai)
                              (:copier nil)
                              (:constructor gptel--make-deepseek)))

(cl-defmethod gptel--parse-response ((_backend gptel-deepseek) response info)
  "Parse a DeepSeek non-streaming RESPONSE and return response text."
  (let* ((choice0 (map-nested-elt response '(:choices 0)))
         (message (plist-get choice0 :message))
         (reasoning (plist-get message :reasoning_content))
         (content (plist-get message :content))
         (show-reasoning gptel-include-reasoning))
    (plist-put info :stop-reason (plist-get choice0 :finish_reason))
    (plist-put info :output-tokens (map-nested-elt response '(:usage :completion_tokens)))
    (cond
     ((or content reasoning)
      (concat (when (and show-reasoning reasoning (not (string-empty-p reasoning)))
                (concat "*Chain of Thought*\n\n" reasoning "\n*Chain of Thought Complete*\n\n"))
              content))
     (t
      (when-let ((tool-calls (plist-get message :tool_calls)))
        (gptel--inject-prompt (plist-get info :backend) (plist-get info :data) message)
        (cl-loop for tool-call across tool-calls
                 collect (list :id (plist-get tool-call :id)
                               :name (plist-get (plist-get tool-call :function) :name)
                               :args (gptel--json-read-string
                                      (plist-get (plist-get tool-call :function) :arguments)))
                 into tool-use
                 finally (plist-put info :tool-use tool-use)))
      nil))))

(cl-defmethod gptel-curl--parse-stream ((_backend gptel-openai) info)
  "Parse a DeepSeek API data stream.

Return the text response accumulated since the last call to this
function.  Additionally, mutate state INFO to add reasoning formatting,
tool-use information if the stream contains it."
  (let* ((content-strs))
    (condition-case nil
        (while (re-search-forward "^data:" nil t)
          (save-match-data
            (if (looking-at " *\\[DONE\\]")
                ;; The stream has ended, handle tool calls if any
                (when-let* ((tool-use (plist-get info :tool-use))
                            (args (apply #'concat (nreverse (plist-get info :partial_json))))
                            (func (plist-get (car tool-use) :function)))
                  (plist-put func :arguments args)
                  (gptel--inject-prompt
                   (plist-get info :backend) (plist-get info :data)
                   `(:role "assistant" :content :null :tool_calls ,(vconcat tool-use)))
                  (cl-loop for tool-call in tool-use
                           for spec = (plist-get tool-call :function)
                           collect (list :id (plist-get tool-call :id)
                                         :name (plist-get spec :name)
                                         :args (ignore-errors (gptel--json-read-string
                                                               (plist-get spec :arguments))))
                            into call-specs
                            finally (plist-put info :tool-use call-specs)))
              (when-let* ((response (gptel--json-read))
                          (delta (map-nested-elt response '(:choices 0 :delta))))
                ;; Handle reasoning content and main content
                (let ((reasoning-content (plist-get delta :reasoning_content))
                      (main-content (plist-get delta :content)))
                  ;; Add reasoning content if available
                  (when (and reasoning-content
                             (not (equal reasoning-content :null))
                             gptel-include-reasoning)
                    (unless (plist-get info :header-printed)
                      (push "*Chain of Thought*\n\n" content-strs)
                      (plist-put info :header-printed t))
                    (plist-put info :has-reasoning t)
                    (push reasoning-content content-strs))
                  ;; Add main content if available
                  (when (and main-content (not (equal main-content :null)))
                    (when (and (plist-get info :has-reasoning)
                               (not (plist-get info :separator-added))
                               gptel-include-reasoning)
                      (push "\n*Chain of Thought Complete*\n\n" content-strs)
                      (plist-put info :separator-added t))
                    (push main-content content-strs))
                  ;; Tool calls
                  (when-let* ((tool-call (map-nested-elt delta '(:tool_calls 0)))
                              (func (plist-get tool-call :function)))
                    (if (plist-get func :name)
                        (progn
                          (when-let* ((partial (plist-get info :partial_json)))
                            (let* ((prev-tool-call (car (plist-get info :tool-use)))
                                   (prev-func (plist-get prev-tool-call :function)))
                              (plist-put prev-func :arguments
                                         (apply #'concat (nreverse (plist-get info :partial_json)))))
                            (plist-put info :partial_json nil))
                          (plist-put info :partial_json (list (plist-get func :arguments)))
                          (plist-put info :tool-use (cons tool-call (plist-get info :tool-use))))
                      (push (plist-get func :arguments) (plist-get info :partial_json)))))))))
      (error (goto-char (match-beginning 0))))
    (apply #'concat (nreverse content-strs))))

;;;###autoload
(cl-defun gptel-make-deepseek
    (name &key curl-args models stream key request-params
          (header
           (lambda () (when-let (key (gptel--get-api-key))
                        `(("Authorization" . ,(concat "Bearer " key))))))
          (host "api.deepseek.com")
          (protocol "https")
          (endpoint "/v1/chat/completions"))
  "Register a DeepSeek backend for gptel with NAME."
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

(provide 'gptel-deepseek)
;;; gptel-deepseek.el ends here
