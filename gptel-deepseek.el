;;; gptel-deepseek.el --- DeepSeek support for gptel  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Your Name

;; Author: Your Name <your@email>
;; Version: 0.1
;; SPDX-License-Identifier: GPL-3.0-or-later
;; URL: https://github.com/your-repo/gptel
;; Package-Requires: ((emacs "28.1") (gptel "0.x"))

;;; Commentary:
;; This file adds support for DeepSeek's API to gptel.

;;; Code:

(require 'gptel-openai)

(defgroup gptel-deepseek nil
  "DeepSeek backend for gptel."
  :group 'gptel)

(defcustom gptel-deepseek-show-reasoning t
  "Whether to include DeepSeek's reasoning_content in responses."
  :type 'boolean
  :group 'gptel-deepseek)

(cl-defstruct (gptel-deepseek (:include gptel-openai)
                              (:copier nil)
                              (:constructor gptel--make-deepseek)))

(cl-defmethod gptel-curl--parse-stream ((_backend gptel-openai) info)
  "Parse an OpenAI API data stream.

Return the text response accumulated since the last call to this
function.  Additionally, mutate state INFO to add tool-use
information if the stream contains it."
  (let* ((content-strs))
    (condition-case nil
        (while (re-search-forward "^data:" nil t)
          (save-match-data
            (if (looking-at " *\\[DONE\\]")
                ;; The stream has ended, so we do the following thing (if we found tool calls)
                ;; - pack tool calls into the messages prompts list to send (INFO -> :data -> :messages)
                ;; - collect tool calls (formatted differently) into (INFO -> :tool-use)
                (when-let* ((tool-use (plist-get info :tool-use))
                            (args (apply #'concat (nreverse (plist-get info :partial_json))))
                            (func (plist-get (car tool-use) :function)))
                  (plist-put func :arguments args) ;Update arguments for last recorded tool
                  (gptel--inject-prompt
                   (plist-get info :backend) (plist-get info :data)
                   `(:role "assistant" :content :null :tool_calls ,(vconcat tool-use))) ; :refusal :null
                  (cl-loop
                   for tool-call in tool-use ; Construct the call specs for running the function calls
                   for spec = (plist-get tool-call :function)
                   collect (list :id (plist-get tool-call :id)
                                 :name (plist-get spec :name)
                                 :args (ignore-errors (gptel--json-read-string
                                                       (plist-get spec :arguments))))
                   into call-specs
                   finally (plist-put info :tool-use call-specs)))
              (when-let* ((response (gptel--json-read))
                          (delta (map-nested-elt response '(:choices 0 :delta))))
                (message "Delta: %s" delta)
                (if-let* ((reasoning_content (plist-get delta :reasoning_content)))
                    (if (and (equal reasoning_content :null)
                             (not (equal (plist-get delta :content) :null)))
                        (push "\n\n*Chain of Thought Complete" content-strs)
                        (push reasoning_content content-strs)))
                (if-let* ((content (plist-get delta :content))
                          ((not (eq content :null))))
                    (push content content-strs)
                  ;; No text content, so look for tool calls
                  (when-let* ((tool-call (map-nested-elt delta '(:tool_calls 0)))
                              (func (plist-get tool-call :function)))
                    (if (plist-get func :name) ;new tool block begins
                        (progn
                          (when-let* ((partial (plist-get info :partial_json)))
                            (let* ((prev-tool-call (car (plist-get info :tool-use)))
                                   (prev-func (plist-get prev-tool-call :function)))
                              (plist-put prev-func :arguments ;update args for old tool block
                                         (apply #'concat (nreverse (plist-get info :partial_json)))))
                            (plist-put info :partial_json nil)) ;clear out finished chain of partial args
                          ;; Start new chain of partial argument strings
                          (plist-put info :partial_json (list (plist-get func :arguments)))
                          ;; NOTE: Do NOT use `push' for this, it prepends and we lose the reference
                          (plist-put info :tool-use (cons tool-call (plist-get info :tool-use))))
                      ;; old tool block continues, so continue collecting arguments in :partial_json
                      (push (plist-get func :arguments) (plist-get info :partial_json)))))))))
      (error (goto-char (match-beginning 0))))
    (apply #'concat (nreverse content-strs))))

;;;###autoload
(cl-defun gptel-make-deepseek
    (name &key curl-args models stream key request-params
          (header
           (lambda () (when-let (key (gptel--get-api-key)))
                   `(("Authorization" . ,(concat "Bearer " key)))))
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
