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

(declare-function ediff-regions-internal "ediff")
(declare-function ediff-make-cloned-buffer "ediff-utils")

;; * Helper functions
(defun gptel--refactor-or-rewrite ()
  "Rewrite should be refactored into refactor.

Or is it the other way around?"
  (if (derived-mode-p 'prog-mode)
      "Refactor" "Rewrite"))

(defvar-local gptel--rewrite-message nil)
(defun gptel--rewrite-message ()
  "Set a generic refactor/rewrite message for the buffer."
  (if (derived-mode-p 'prog-mode)
      (format "You are a %s programmer. Refactor the following code. Generate only code, no explanation."
              (substring (symbol-name major-mode) nil -5))
    (format "You are a prose editor. Rewrite the following text to be more professional.")))

(defvar gptel--crowdsourced-prompts-url
  "https://github.com/f/awesome-chatgpt-prompts/raw/main/prompts.csv"
  "URL for crowdsourced ChatGPT system prompts.")

(defvar gptel--crowdsourced-prompts
  (make-hash-table :test #'equal)
  "Crowdsourced system prompts for ChatGPT.")

(defun gptel--crowdsourced-prompts ()
  "Acquire and read crowdsourced system prompts for ChatGPT.

These are stored in the variable `gptel--crowdsourced-prompts',
which see."
  (when (hash-table-p gptel--crowdsourced-prompts)
    (when (hash-table-empty-p gptel--crowdsourced-prompts)
      (unless gptel-crowdsourced-prompts-file
        (run-at-time 0 nil #'gptel-system-prompt)
        (user-error "No crowdsourced prompts available"))
      (unless (and (file-exists-p gptel-crowdsourced-prompts-file)
                   (time-less-p
                    (time-subtract (current-time) (days-to-time 14))
                    (file-attribute-modification-time
                     (file-attributes gptel-crowdsourced-prompts-file))))
        (when (y-or-n-p
               (concat
                "Fetch crowdsourced system prompts from "
                (propertize "https://github.com/f/awesome-chatgpt-prompts" 'face 'link)
                "?"))
          ;; Fetch file
          (message "Fetching prompts...")
          (if (url-copy-file gptel--crowdsourced-prompts-url
                             gptel-crowdsourced-prompts-file
                             'ok-if-already-exists)
              (message "Fetching prompts... done.")
            (message "Could not retrieve new prompts."))))
      (if (not (file-readable-p gptel-crowdsourced-prompts-file))
          (progn (message "No crowdsourced prompts available")
                 (call-interactively #'gptel-system-prompt))
        (with-temp-buffer
          (insert-file-contents gptel-crowdsourced-prompts-file)
          (goto-char (point-min))
          (forward-line 1)
          (while (not (eobp))
            (when-let ((act (read (current-buffer))))
              (forward-char)
              (save-excursion
                (while (re-search-forward "\"\"" (line-end-position) t)
                  (replace-match "\\\\\"")))
              (when-let ((prompt (read (current-buffer))))
                (puthash act prompt gptel--crowdsourced-prompts)))
            (forward-line 1)))))
    gptel--crowdsourced-prompts))

;; * Transient Prefixes

(define-obsolete-function-alias 'gptel-send-menu 'gptel-menu "0.3.2")

;; BUG: The `:incompatible' spec doesn't work if there's a `:description' below it.
;;;###autoload (autoload 'gptel-menu "gptel-transient" nil t)
(transient-define-prefix gptel-menu ()
  "Change parameters of prompt to send ChatGPT."
  ;; :incompatible '(("-m" "-n" "-k" "-e"))
  [:description
   (lambda () (format "Directive:  %s"
                 (truncate-string-to-width
                  gptel--system-message (max (- (window-width) 14) 20) nil nil t)))
   ("h" "Set directives for chat" gptel-system-prompt :transient t)]
  [["Session Parameters"
    (gptel--infix-provider)
    ;; (gptel--infix-model)
    (gptel--infix-max-tokens)
    (gptel--infix-num-messages-to-send)
    (gptel--infix-temperature)]
   ["Prompt:"
    ("p" "From minibuffer instead" "p")
    ("i" "Replace/Delete prompt" "i")
    "Response to:"
    ("m" "Minibuffer instead" "m")
    ("n" "New session" "n"
     :class transient-option
     :prompt "Name for new session: "
     :reader
     (lambda (prompt _ history)
       (read-string
        prompt (generate-new-buffer-name "*ChatGPT*") history)))
    ("e" "Existing session" "e"
     :class transient-option
     :prompt "Existing session: "
     :reader
     (lambda (prompt _ history)
       (completing-read
        prompt (mapcar #'buffer-name (buffer-list))
        (lambda (buf) (and (buffer-local-value 'gptel-mode (get-buffer buf))
                      (not (equal (current-buffer) buf))))
        t nil history)))
    ("k" "Kill-ring" "k")]
   [:description gptel--refactor-or-rewrite
    :if use-region-p
    ("r"
     ;;FIXME: Transient complains if I use `gptel--refactor-or-rewrite' here. It
     ;;reads this function as a suffix instead of a function that returns the
     ;;description.
     (lambda () (if (derived-mode-p 'prog-mode)
               "Refactor" "Rewrite"))
     gptel-rewrite-menu)]
   ["Send" (gptel--suffix-send)]])


;; ** Prefix for setting the system prompt.
(defun gptel-system-prompt--setup (_)
  "Set up suffixes for system prompt."
  (transient-parse-suffixes
   'gptel-system-prompt
   (cl-loop for (type . prompt) in gptel-directives
       with taken
       for name = (symbol-name type)
       for key =
       (let ((idx 0) pos)
         (while (or (not pos) (member pos taken))
           (setq pos (substring name idx (1+ idx)))
           (cl-incf idx))
         (push pos taken)
         pos)
       collect (list (key-description key) (capitalize name)
                `(lambda () (interactive)
                  (message "Directive: %s" ,prompt)
                  (setq gptel--system-message ,prompt)))
       into prompt-suffixes
       finally return
       (nconc
        (list (list 'gptel--suffix-system-message))
        prompt-suffixes
        (list (list "SPC" "Pick crowdsourced prompt"
                    'gptel--read-crowdsourced-prompt :transient nil))))))

(transient-define-prefix gptel-system-prompt ()
  "Change the system prompt to send ChatGPT.

The \"system\" prompt establishes directives for the chat
session. Some examples of system prompts are:

You are a helpful assistant. Answer as concisely as possible.
Reply only with shell commands and no prose.
You are a poet. Reply only in verse.

Customize `gptel-directives' for task-specific prompts."
  [:description
   (lambda () (format "Current directive: %s"
                 (truncate-string-to-width
                  gptel--system-message 100 nil nil t)))
   :class transient-column
   :setup-children gptel-system-prompt--setup
   :pad-keys t])

;; ** Prefix for rewriting/refactoring

(transient-define-prefix gptel-rewrite-menu ()
  "Rewrite or refactor text region using ChatGPT."
  [:description
   (lambda ()
     (format "Directive:  %s"
             (truncate-string-to-width
              (or gptel--rewrite-message (gptel--rewrite-message))
              (max (- (window-width) 14) 20) nil nil t)))
   (gptel--infix-rewrite-prompt)]
  [[:description "Diff Options"
   ("-w" "Wordwise diff" "-w")]
   [:description
    (lambda () (if (derived-mode-p 'prog-mode)
              "Refactor" "Rewrite"))
    (gptel--suffix-rewrite)
    (gptel--suffix-rewrite-and-replace)
    (gptel--suffix-rewrite-and-ediff)]]
  (interactive)
  (unless gptel--rewrite-message
    (setq gptel--rewrite-message (gptel--rewrite-message)))
  (transient-setup 'gptel-rewrite-menu))

;; * Transient Infixes

;; ** Infixes for model parameters

(defun gptel--transient-read-variable (prompt initial-input history)
  "Read value from minibuffer and interpret the result as a Lisp object."
  (condition-case nil
      (read-from-minibuffer prompt initial-input read-expression-map t history)
    ('error nil)))

(transient-define-infix gptel--infix-num-messages-to-send ()
  "Number of recent messages to send with each exchange.

By default, the full conversation history is sent with every new
prompt. This retains the full context of the conversation, but
can be expensive in token size. Set how many recent messages to
include."
  :description "Number of past messages to send"
  :class 'transient-lisp-variable
  :variable 'gptel--num-messages-to-send
  :key "-n"
  :prompt "Number of past messages to include for context (leave empty for all): "
  :reader 'gptel--transient-read-variable)

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
  :key "-c"
  :prompt "Response length in tokens (leave empty: default, 80-200: short, 200-500: long): "
  :reader 'gptel--transient-read-variable)

(defclass gptel-provider-variable (transient-lisp-variable)
  ((model       :initarg :model)
   (model-value :initarg :model-value)
   (always-read :initform t)
   (set-value :initarg :set-value :initform #'set))
  "Class used for gptel-backends.")

(cl-defmethod transient-format-value ((obj gptel-provider-variable))
  (propertize (concat (gptel-backend-name (oref obj value)) ":"
                      (buffer-local-value (oref obj model) transient--original-buffer))
              'face 'transient-value))

(cl-defmethod transient-infix-set ((obj gptel-provider-variable) value)
  (pcase-let ((`(,backend-value ,model-value) value))
    (funcall (oref obj set-value)
             (oref obj variable)
             (oset obj value backend-value))
    (funcall (oref obj set-value)
             (oref obj model)
             (oset obj model-value model-value))))

(transient-define-infix gptel--infix-provider ()
  "AI Provider for Chat."
  :description "GPT Model: "
  :class 'gptel-provider-variable
  :prompt "Model provider: "
  :variable 'gptel-backend
  :model 'gptel-model
  :key "-m"
  :reader (lambda (prompt &rest _)
            (let* ((backend-name 
                    (if (<= (length gptel--known-backends) 1)
                        (caar gptel--known-backends)
                      (completing-read
                       prompt
                       (mapcar #'car gptel--known-backends))))
                   (backend (alist-get backend-name gptel--known-backends
                                nil nil #'equal))
                   (backend-models (gptel-backend-models backend))
                   (model-name (if (= (length backend-models) 1)
                                   (car backend-models)
                                 (completing-read
                                  "Model: " backend-models))))
              (list backend model-name))))

(transient-define-infix gptel--infix-model ()
  "AI Model for Chat."
  :description "GPT Model: "
  :class 'transient-lisp-variable
  :variable 'gptel-model
  :key "-m"
  :choices '("gpt-3.5-turbo" "gpt-3.5-turbo-16k" "gpt-4" "gpt-4-32k")
  :reader (lambda (prompt &rest _)
            (completing-read
             prompt
             '("gpt-3.5-turbo" "gpt-3.5-turbo-16k" "gpt-4" "gpt-4-32k"))))

(transient-define-infix gptel--infix-temperature ()
  "Temperature of request."
  :description "Randomness (0 - 2.0)"
  :class 'transient-lisp-variable
  :variable 'gptel-temperature
  :key "-t"
  :prompt "Set temperature (0.0-2.0, leave empty for default): "
  :reader 'gptel--transient-read-variable)

;; ** Infix for the refactor/rewrite system message

(transient-define-infix gptel--infix-rewrite-prompt ()
  "Chat directive (system message) to use for rewriting or refactoring."
  :description (lambda () (if (derived-mode-p 'prog-mode)
                         "Set directives for refactor"
                       "Set directives for rewrite"))
  :format "%k %d"
  :class 'transient-lisp-variable
  :variable 'gptel--rewrite-message
  :key "h"
  :prompt "Set directive for rewrite: "
  :reader (lambda (prompt _ history)
            (read-string
             prompt (gptel--rewrite-message) history)))

;; * Transient Suffixes

;; ** Suffix to send prompt

(transient-define-suffix gptel--suffix-send (args)
  "Send ARGS."
  :key "RET"
  :description "Send prompt"
  (interactive (list (transient-args transient-current-command)))
  (let ((stream gptel-stream)
        (in-place (and (member "i" args) t))
        (output-to-other-buffer-p)
        (backend gptel-backend)
        (backend-name (gptel-backend-name gptel-backend))
        (buffer) (position)
        (callback) (gptel-buffer-name)
        (prompt
         (and (member "p" args)
              (read-string
               (format "Ask %s: " (gptel-backend-name gptel-backend))
               (apply #'buffer-substring-no-properties
                      (if (use-region-p)
                          (list (region-beginning) (region-end))
                        (list (line-beginning-position) (line-end-position))))))))
    (cond
     ((member "m" args)
      (setq stream nil)
      (setq callback
            (lambda (resp info)
              (if resp
                  (message "%s response: %s" backend-name resp)
                (message "%s response error: %s" backend-name (plist-get info :status))))))
     ((member "k" args)
      (setq stream nil)
      (setq callback
            (lambda (resp info)
              (if (not resp)
                  (message "%s response error: %s" backend-name (plist-get info :status))
                (kill-new resp)
                (message "%s response: copied to kill-ring." backend-name)))))
     ((setq gptel-buffer-name
            (cl-some (lambda (s) (and (string-prefix-p "n" s)
                                 (substring s 1)))
                     args))
      (setq buffer
            (gptel gptel-buffer-name
                   (condition-case nil
                       (gptel--get-api-key)
                     ((error user-error)
                      (setq gptel-api-key
                            (read-passwd
                             (format "%s API key: "
                                     (gptel-backend-name
                                      gptel-backend))))))
                   (or prompt
                       (if (use-region-p)
                           (buffer-substring-no-properties (region-beginning)
                                                           (region-end))
                         (buffer-substring-no-properties
                          (save-excursion
                            (text-property-search-backward
                             'gptel 'response
                             (when (get-char-property (max (point-min) (1- (point)))
                                                      'gptel)
                               t))
                            (point))
                          (gptel--at-word-end (point)))))))
      (with-current-buffer buffer
        (setq gptel-backend backend)
        (gptel--update-header-line " Waiting..." 'warning)
        (setq position (point)))
      (setq output-to-other-buffer-p t))
     ((setq gptel-buffer-name
            (cl-some (lambda (s) (and (string-prefix-p "e" s)
                                 (substring s 1)))
                     args))
      (setq buffer (get-buffer gptel-buffer-name))
      (setq output-to-other-buffer-p t)
      (let ((reduced-prompt
             (or prompt
                 (if (use-region-p)
                     (buffer-substring-no-properties (region-beginning)
                                                     (region-end))
                   (buffer-substring-no-properties
                    (save-excursion
                      (text-property-search-backward
                       'gptel 'response
                       (when (get-char-property (max (point-min) (1- (point)))
                                                'gptel)
                         t))
                      (point))
                    (gptel--at-word-end (point)))))))
        (with-current-buffer buffer
          (goto-char (point-max))
          (if (or buffer-read-only
                  (get-char-property (point) 'read-only))
              (setq prompt reduced-prompt)
            (insert reduced-prompt))
          (setq position (point))
          (when gptel-mode
            (gptel--update-header-line " Waiting..." 'warning))))))

    (when in-place
      (setq prompt (gptel--create-prompt (point)))
      (let ((beg
             (if (use-region-p)
                 (region-beginning)
               (save-excursion
                 (text-property-search-backward
                  'gptel 'response
                  (when (get-char-property (max (point-min) (1- (point)))
                                           'gptel)
                    t))
                 (point))))
            (end (if (use-region-p) (region-end) (point))))
        (kill-region beg end)))

    (gptel-request
     prompt
     :buffer (or buffer (current-buffer))
     :position position
     :in-place (and in-place (not output-to-other-buffer-p))
     :stream stream
     :callback callback)
    (when output-to-other-buffer-p
      (message (concat "Prompt sent to buffer: "
                       (propertize gptel-buffer-name 'face 'help-key-binding)))
      (display-buffer
       buffer '((display-buffer-reuse-window
                 display-buffer-pop-up-window)
                (reusable-frames . visible))))))

;; ** Set system message
(defun gptel--read-crowdsourced-prompt ()
  "Pick a crowdsourced system prompt for gptel.

This uses the prompts in the variable
`gptel--crowdsourced-prompts', which see."
  (interactive)
  (if (not (hash-table-empty-p (gptel--crowdsourced-prompts)))
      (let ((choice
             (completing-read
              "Pick and edit prompt: "
              (lambda (str pred action)
                (if (eq action 'metadata)
                    `(metadata
                      (affixation-function .
                       (lambda (cands)
                         (mapcar
                          (lambda (c)
                            (list c ""
                             (concat (propertize " " 'display '(space :align-to 22))
                              " " (propertize (gethash c gptel--crowdsourced-prompts)
                               'face 'completions-annotations))))
                          cands))))
                  (complete-with-action action gptel--crowdsourced-prompts str pred)))
              nil t)))
        (when-let ((prompt (gethash choice gptel--crowdsourced-prompts)))
            (setq gptel--system-message prompt)
            (gptel--suffix-system-message)))
    (message "No prompts available.")))

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
       " when ready, or "
       (propertize "C-c C-k" 'face 'help-key-binding)
       " to abort.\n"
       "# Example: You are a helpful assistant. Answer as concisely as possible.\n"
       "# Example: Reply only with shell commands and no prose.\n"
       "# Example: You are a poet. Reply only in verse.\n\n")
      (set-marker msg-start (point))
      (insert (buffer-local-value 'gptel--system-message orig-buf))
      (beginning-of-line)
      (push-mark)
      (end-of-line)
      (exchange-point-and-mark)
      (activate-mark)
      (display-buffer (current-buffer)
                      `((display-buffer-below-selected)
                        (body-function . ,#'select-window)
                        (window-height . ,#'fit-window-to-buffer)))
      (let ((quit-to-menu
             (lambda ()
               (interactive)
               (quit-window)
               (display-buffer
                orig-buf
                `((display-buffer-reuse-window
                   display-buffer-use-some-window)
                  (body-function . ,#'select-window)))
               (call-interactively #'gptel-menu))))
        (local-set-key (kbd "C-c C-c")
                       (lambda ()
                         (interactive)
                         (let ((system-message
                                (buffer-substring msg-start (point-max))))
                           (with-current-buffer orig-buf
                             (setq gptel--system-message system-message)))
                         (funcall quit-to-menu)))
        (local-set-key (kbd "C-c C-k") quit-to-menu)))))

;; ** Suffixes for rewriting/refactoring

(transient-define-suffix gptel--suffix-rewrite ()
  "Rewrite or refactor region contents."
  :key "r"
  :description #'gptel--refactor-or-rewrite
  (interactive)
  (let* ((prompt (buffer-substring-no-properties
                  (region-beginning) (region-end)))
         (stream gptel-stream)
         (gptel--system-message gptel--rewrite-message))
    (gptel-request prompt :stream stream)))

(transient-define-suffix gptel--suffix-rewrite-and-replace ()
  "Refactor region contents and replace it."
  :key "R"
  :description (lambda () (concat (gptel--refactor-or-rewrite) " in place"))
  (interactive)
  (let* ((prompt (buffer-substring-no-properties
                  (region-beginning) (region-end)))
         (stream gptel-stream)
         (gptel--system-message gptel--rewrite-message))
    (kill-region (region-beginning) (region-end))
    (message "Original text saved to kill-ring.")
    (gptel-request prompt :stream stream :in-place t)))

(transient-define-suffix gptel--suffix-rewrite-and-ediff (args)
  "Refactoring or rewrite region contents and run Ediff."
  :key "E"
  :description (lambda () (concat (gptel--refactor-or-rewrite) " and Ediff"))
  (interactive (list (transient-args transient-current-command)))
  (let* ((prompt (buffer-substring-no-properties
                  (region-beginning) (region-end)))
         (gptel--system-message gptel--rewrite-message))
    (message "Waiting for response... ")
    (gptel-request
     prompt
     :context (cons (region-beginning) (region-end))
     :callback
     (lambda (response info)
       (if (not response)
           (message "ChatGPT response error: %s" (plist-get info :status))
         (let* ((gptel-buffer (plist-get info :buffer))
                (gptel-bounds (plist-get info :context))
                (buffer-mode
                 (buffer-local-value 'major-mode gptel-buffer)))
           (pcase-let ((`(,new-buf ,new-beg ,new-end)
                        (with-current-buffer (get-buffer-create "*gptel-rewrite-Region.B-*")
                          (erase-buffer)
                          (funcall buffer-mode)
                          (insert response)
                          (goto-char (point-min))
                          (list (current-buffer) (point-min) (point-max)))))
             (require 'ediff)
             (apply
              #'ediff-regions-internal
              (get-buffer (ediff-make-cloned-buffer gptel-buffer "-Region.A-"))
              (car gptel-bounds) (cdr gptel-bounds)
              new-buf new-beg new-end
              nil
              (if (transient-arg-value "-w" args)
                  (list 'ediff-regions-wordwise 'word-wise nil)
                (list 'ediff-regions-linewise nil nil))))))))))

(provide 'gptel-transient)
;;; gptel-transient.el ends here

;; Local Variables:
;; outline-regexp: "^;; \\*+"
;; eval: (outline-minor-mode 1)
;; End:
