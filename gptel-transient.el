;;; gptel-transient.el --- Transient menu for GPTel  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Keywords: convenience

;; SPDX-License-Identifier: GPL-3.0-or-later

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

;;

;;; Code:
(eval-when-compile (require 'cl-lib))
(require 'gptel)
(require 'transient)

;;;###autoload (autoload 'gptel-send-menu "gptel-transient" nil t)
(transient-define-prefix gptel-send-menu ()
     "Change parameters of prompt to send ChatGPT."
     [:description
      (lambda () (format "Directive:  %s"
                    (truncate-string-to-width
                     gptel--system-message (max (- (window-width) 14) 20) nil nil t)))
      ("h" "Set directives for chat" gptel-system-prompt)]
     [["Session Parameters"
       (gptel--infix-max-tokens)
       (gptel--infix-num-messages-to-send)
       (gptel--infix-temperature)
       (gptel--infix-model)]
      ["Send"
       (gptel--suffix-send-existing)
       (gptel--suffix-send-new)
       ("RET" "Send prompt" gptel-send)]])

(transient-define-prefix gptel-system-prompt ()
  "Change the system prompt to send ChatGPT.

The \"system\" prompt establishes directives for the chat
session. Some examples of system prompts are:

You are a helpful assistant. Answer as concisely as possible.
Reply only with shell commands and no prose.
You are a poet. Reply only in verse.

Customize `gptel-directives' for task-specific prompts."
  [:description
   (lambda () (format "Directive: %s"
                 (truncate-string-to-width gptel--system-message (max (- (window-width) 14) 20) nil nil t)))
   :class transient-column
   :pad-keys t
   (gptel--suffix-system-message)
   ("p" "Programming"
    (lambda () (interactive)
      (setq gptel--system-message
            (alist-get 'programming gptel-directives)))
    :transient t)
   ("d" "Default"
    (lambda () (interactive)
      (setq gptel--system-message
            (alist-get 'default gptel-directives)))
    :transient t)
   ("w" "Writing"
    (lambda () (interactive)
      (setq gptel--system-message
            (alist-get 'writing gptel-directives)))
    :transient t)
   ("c" "Chat"
    (lambda () (interactive)
      (setq gptel--system-message
            (alist-get 'chat gptel-directives)))
    :transient t)])

;; TODO: Switch to dynamic Transient menus (below) once there's a new Transient release
;; (transient-define-prefix gptel-system-prompt ()
;;   "Change the system prompt to send ChatGPT."
;;   [:description (lambda () (format "Current directive: %s"
;;                                 (truncate-string-to-width gptel--system-message 100 nil nil t)))
;;    :class transient-column
;;    :setup-children gptel-system-prompt--setup
;;    :pad-keys t])

;; (defun gptel-system-prompt--setup (_)
;;   "Set up suffixes for system prompt."
;;   (transient-parse-suffixes
;;    'gptel-system-prompt
;;    (cl-loop for (type . prompt) in gptel-directives
;;        for name = (symbol-name type)
;;        for key = (substring name 0 1)
;;        collect (list (key-description key) (capitalize name)
;;                 `(lambda () (interactive)
;;                   (message "Directive: %s" ,prompt)
;;                   (setq gptel--system-message ,prompt))
;;                 :transient t)
;;        into prompt-suffixes
;;        finally return (cons (list 'gptel--suffix-system-message)
;;                             prompt-suffixes))))

(transient-define-infix gptel--infix-num-messages-to-send ()
  "Number of recent messages to send with each exchange.

By default, the full conversation history is sent with every new
prompt. This retains the full context of the conversation, but
can be expensive in token size. Set how many recent messages to
include."
  :description "Number of past messages to send"
  :class 'transient-lisp-variable
  :variable 'gptel--num-messages-to-send
  :key "n"
  :prompt "Number of past messages to include for context (leave empty for all): "
  :reader 'transient-read-number-N0)

(transient-define-infix gptel--infix-max-tokens ()
  "Max tokens per response.

This is roughly the number of words in the response. 100-300 is a
reasonable range for short answers, 400 or more for longer
responses.

If left unset, ChatGPT will target about 40% of the total token
count of the conversation so far in each message, so messages
will get progressively longer!"
  :description "Response length (tokens)"
  :class 'transient-lisp-variable
  :variable 'gptel-max-tokens
  :key "<"
  :prompt "Response length in tokens (leave empty: default, 80-200: short, 200-500: long): "
  :reader 'transient-read-number-N+)

(transient-define-infix gptel--infix-model ()
  "AI Model for Chat."
  :description "GPT Model: "
  :class 'transient-lisp-variable
  :variable 'gptel-model
  :key "m"
  :choices '("gpt-3.5-turbo-0301" "gpt-3.5-turbo" "gpt-4")
  :reader (lambda (prompt &rest _)
            (completing-read
             prompt
             '("gpt-3.5-turbo-0301" "gpt-3.5-turbo" "gpt-4"))))

(transient-define-infix gptel--infix-temperature ()
  "Temperature of request."
  :description "Randomness (0 - 2.0)"
  :class 'transient-lisp-variable
  :variable 'gptel-temperature
  :key "t"
  :reader (lambda (&rest _)
            (read-from-minibuffer "Set temperature (0.0-2.0, leave empty for default): "
                                  (number-to-string gptel-temperature))))

(transient-define-suffix gptel--suffix-send-existing ()
  "Send query in existing chat session."
  :if #'use-region-p
  :key "E"
  :description "Send in existing session"
  (interactive)
  (when-let* ((this (buffer-name))
              (prompt (buffer-substring (region-beginning)
                                       (region-end)))
              (buf
               (completing-read
                "Send query in buffer: " (mapcar #'buffer-name (buffer-list))
                (lambda (buf) (and (buffer-local-value 'gptel-mode (get-buffer buf))
                              (not (equal this buf)))))))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert prompt)
      (gptel-send))
    (pop-to-buffer buf)))

(transient-define-suffix gptel--suffix-send-new ()
  "Send query in new session."
  :if #'use-region-p
  :description "Send in new session"
  :key "N"
  (interactive)
  (let* ((current-prefix-arg t)
         (buf (call-interactively #'gptel)))
    (and (bufferp buf)
         (with-current-buffer buf (gptel-send)))))

(transient-define-suffix gptel--suffix-system-message ()
  "Set directives sent to ChatGPT."
  :transient nil
  :description "Set custom directives"
  :key "h"
  (interactive)
  (let ((orig-buf (current-buffer))
        (msg-start (make-marker)))
    (with-current-buffer (get-buffer-create "*gptel-system*")
      (erase-buffer)
      (text-mode)
      (insert
       "# Insert your system message below and press "
       (propertize "C-c C-c" 'face 'help-key-binding)
       " when ready.\n"
       "# Example: You are a helpful assistant. Answer as concisely as possible.\n"
       "# Example: Reply only with shell commands and no prose.\n"
       "# Example: You are a poet. Reply only in verse.\n\n")
      (set-marker msg-start (point))
      (insert (buffer-local-value 'gptel--system-message orig-buf))
      (beginning-of-line)
      (push-mark)
      (end-of-line)
      (activate-mark)
      (display-buffer (current-buffer)
                      `((display-buffer-below-selected)
                        (body-function . ,#'select-window)
                        (window-height . ,#'fit-window-to-buffer)))
      (local-set-key (kbd "C-c C-c")
                     (lambda ()
                       (interactive)
                       (setf (buffer-local-value 'gptel--system-message orig-buf)
                             (buffer-substring msg-start (point-max)))
                       (quit-window)
                       (display-buffer
                        orig-buf
                        `((display-buffer-reuse-window
                           display-buffer-use-some-window)
                          (body-function . ,#'select-window)))
                       (call-interactively #'gptel-send-menu))))))

(provide 'gptel-transient)
;;; gptel-transient.el ends here
