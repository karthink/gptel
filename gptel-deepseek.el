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
                                                               (plist-get spec :arguments)))
                                         into call-specs
                                         finally (plist-put info :tool-use call-specs))))
              (when-let* ((response (gptel--json-read))
                          (delta (map-nested-elt response '(:choices 0 :delta))))
                ;; Handle reasoning content and main content
                (let ((reasoning-content (plist-get delta :reasoning_content))
                      (main-content (plist-get delta :content)))
                  (when (and reasoning-content (not (equal reasoning-content :null)))
                    (plist-put info :has-reasoning t)
                    (push reasoning-content content-strs))
                  (when (and main-content (not (equal main-content :null)))
                    (when (and (plist-get info :has-reasoning)
                               (not (plist-get info :separator-added)))
                      (push "\n\n*Chain of Thought Complete*" content-strs)
                      (plist-put info :separator-added t))
                    (push main-content content-strs))
                  (cond
                   ;; Main content present
                   (main-content
                     (unless (eq main-content :null)
                       (push main-content content-strs)
                       (plist-put info :has-reasoning nil)))
                   ((and main-content (not (equal main-content :null)))
                    (progn (when (and (plist-get info :has-reasoning)
                                      (not (plist-get info :separator-added)))
                             (push "\n\n*Chain of Thought Complete*" content-strs)
                             (plist-put info :separator-added t))
                           (push main-content content-strs)))
                   ;; Tool calls
                   (t
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
                        (push (plist-get func :arguments) (plist-get info :partial_json)))))))))))
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
