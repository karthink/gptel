;;; gptel-chatbotui.el --- ChatbotUI support for gptel     -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Damon Chan

;; Author: Damon Chan <elecming@gmail.com>
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

;; This file adds support for the ChatbotUI LLM API to gptel

;;; Code:
(require 'gptel)
(require 'cl-generic)

;;; ChatbotUI backend
(cl-defstruct (gptel-chatbotui (:constructor gptel--make-chatbotui)
                               (:copier nil)
                               (:include gptel-backend)))

(defvar-local gptel--chatbotui-context nil
  "Context for chatbotui conversations.
This variable holds the context array for conversations with
ChatbotUI models.")

;; Define parsing response methods here
(cl-defmethod gptel-curl--parse-stream ((_backend gptel-chatbotui) _info)
  "Parse streaming response from ChatbotUI."
  (when (bobp)
    (re-search-forward "\r\n\r\n")
    (forward-line 0))
  (let ((content "")
        (content-end (point-max)))
    (while (re-search-forward "\\(.+\\)$" content-end t) ; (re-search-forward "([a-f0-9]+ \\. [0-9]+)" nil t)
      (setq content (concat content "\n" (match-string 0))))
    ;; remove the final (4cfc131035f65027ad89312027fcb3d9 . 990)
    (replace-regexp-in-string "([a-f0-9]+ \\. [0-9]+)" "" content)))

(cl-defmethod gptel--parse-response ((_backend gptel-chatbotui) response _)
  "Parse response from ChatbotUI."
  (let ((content "")
        (content-end (point-max)))
    (while (re-search-forward "\\(.+\\)$" content-end t)
      (setq content (concat content "\n" (match-string 0))))
    (string-trim (replace-regexp-in-string "([a-f0-9]+ \\. [0-9]+)" "" content))))

(cl-defmethod gptel--request-data ((_backend gptel-chatbotui) prompts)
  "JSON encode PROMPTS for ChatbotUI."
  (let* ((model-plist
          `(:id "gpt-4-32k"
            :name "GPT-4-32K"
            :maxLength 96000
            :tokenLimit 32000)))
    `(:model ,model-plist
      :messages ,(plist-get prompts :messages)
      :key ""
      :prompt ,(plist-get prompts :system)
      :temperature ,(or (plist-get prompts :temperature) 1)
      :info ,(plist-get prompts :info))))

(cl-defmethod gptel--parse-buffer ((_backend gptel-chatbotui) max-entries)
  "Parse current buffer backwards from point and return a list of prompts."
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
    (list
     :system gptel--system-message
     :messages (vconcat prompts))))

;;;###autoload
(cl-defun gptel-make-chatbotui
    (name &key (curl-args nil)
                (header nil)
                (key nil)
                (models nil)
                (stream nil)
                (host nil)
                (protocol nil)
                (endpoint nil))
  "Register an ChatbotUI backend for gptel with NAME.

Keyword arguments:

Same as gptel-ollama but defaults are suited for ChatbotUI."
  (declare (indent 1))
  (let ((backend (gptel--make-chatbotui
                  :curl-args curl-args
                  :name name
                  :host host
                  :header header
                  :key key
                  :models models
                  :protocol protocol
                  :endpoint endpoint
                  :stream stream
                  :url (concat protocol "://" host endpoint))))
    (prog1 backend
      (setf (alist-get name gptel--known-backends nil nil #'equal)
            backend))))

(provide 'gptel-chatbotui)
;;; gptel-chatbotui.el ends here
