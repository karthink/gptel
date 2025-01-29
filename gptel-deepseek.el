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

(cl-defmethod gptel--parse-response ((_backend gptel-deepseek) response info)
  "Parse a DeepSeek API response."
  (let* ((choice0 (map-nested-elt response '(:choices 0)))
         (message (plist-get choice0 :message))
         (content (plist-get message :content))
         (reasoning (plist-get message :reasoning_content)))
    (plist-put info :stop-reason (plist-get choice0 :finish_reason))
    (plist-put info :output-tokens (map-nested-elt response '(:usage :completion_tokens)))
    (if (and content (not (or (eq content :null) (string-empty-p content))))
        (if (and gptel-deepseek-show-reasoning reasoning (not (string-empty-p reasoning)))
            (concat content "\n\n---\n**Reasoning**\n" reasoning)
          content)
      nil)))

(cl-defmethod gptel-curl--parse-stream ((backend gptel-deepseek) info)
  "Parse a streaming DeepSeek API response."
  (let* ((content-acc)
         (reasoning-acc)
         (in-tool-call nil)
         (partial-json))
    (condition-case nil
        (while (re-search-forward "^data:" nil t)
          (save-match-data
            (if (looking-at " *\\[DONE\\]")
                (progn
                  (when-let ((tool-use (plist-get info :tool-use)))
                    (gptel--inject-prompt
                     backend (plist-get info :data)
                     `(:role "assistant" :content :null :tool_calls ,(vconcat tool-use))))
                  (setq content-acc nil reasoning-acc nil))
              (when-let* ((response (gptel--json-read))
                          (delta (map-nested-elt response '(:choices 0 :delta))))
                (cond
                 ((plist-get delta :tool_calls)
                  (setq in-tool-call t)
                  (let ((tool-call (map-nested-elt delta '(:tool_calls 0))))
                    (if (plist-get tool-call :id)
                        (progn
                          (when partial-json
                            (let* ((prev-tool (car (plist-get info :tool-use)))
                                   (prev-func (plist-get prev-tool :function)))
                              (plist-put prev-func :arguments
                                         (apply #'concat (nreverse partial-json)))))
                          (plist-put info :tool-use (cons tool-call (plist-get info :tool-use)))
                          (setq partial-json nil))
                      (when-let ((func (plist-get tool-call :function)))
                        (push (plist-get func :arguments) partial-json)))))

                 ((plist-get delta :content)
                  (push (plist-get delta :content) content-acc))

                 ((plist-get delta :reasoning_content)
                  (push (plist-get delta :reasoning_content) reasoning-acc)))))))
      (error (goto-char (match-beginning 0))))

    (let* ((content (apply #'concat (nreverse content-acc)))
           (reasoning (and gptel-deepseek-show-reasoning
                           (apply #'concat (nreverse reasoning-acc)))))
      (if (and reasoning (not (string-empty-p reasoning)))
          (concat content "\n\n---\n**Reasoning**\n" reasoning)
        content))))

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
