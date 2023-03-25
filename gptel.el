;;; gptel.el --- A simple ChatGPT client      -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Karthik Chikmagalur

;; Author: Karthik Chikmagalur
;; Version: 0.10
;; Package-Requires: ((emacs "27.1") (transient "0.3.7"))
;; Keywords: convenience
;; URL: https://github.com/karthink/gptel

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

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; A simple ChatGPT client for Emacs.
;;
;; Requirements:
;; - You need an OpenAI API key. Set the variable `gptel-api-key' to the key or to
;;   a function of no arguments that returns the key.
;;
;; - Not required but recommended: Install `markdown-mode'.
;;
;; Usage:
;; gptel can be used in any buffer or in a dedicated chat buffer.
;;
;; To use this in a dedicated buffer:
;; - M-x gptel: Start a ChatGPT session
;; - C-u M-x gptel: Start another session or multiple independent ChatGPT sessions
;;
;; - In the chat session: Press `C-c RET' (`gptel-send') to send
;;   your prompt. Use a prefix argument (`C-u C-c RET') to set chat parameters.
;;
;; To use this in any buffer:
;;
;; - Select a region of text and call `gptel-send'. Call with a prefix argument
;;   to set chat parameters.
;; - You can select previous prompts and responses to continue the conversation.

;;; Code:
(declare-function markdown-mode "markdown-mode")
(declare-function gptel-curl-get-response "gptel-curl")
(declare-function gptel-send-menu "gptel-transient")
(declare-function pulse-momentary-highlight-region "pulse")

(eval-when-compile
  (require 'subr-x)
  (require 'cl-lib))

(require 'url)
(require 'json)
(require 'map)
(require 'text-property-search)

(defcustom gptel-api-key #'gptel-api-key-from-auth-source
  "An OpenAI API key (string).

Can also be a function of no arguments that returns an API
key (more secure)."
  :group 'gptel
  :type '(choice
          (string :tag "API key")
          (function :tag "Function that returns the API key")))

(defcustom gptel-playback nil
  "Whether responses from ChatGPT be played back in chunks.

When set to nil, it is inserted all at once.

'tis a bit silly."
  :group 'gptel
  :type 'boolean)

(defcustom gptel-use-curl (and (executable-find "curl") t)
  "Whether gptel should prefer Curl when available."
  :group 'gptel
  :type 'boolean)

(defcustom gptel-response-filter-functions
  '(gptel--convert-org)
  "Abnormal hook for transforming the response from ChatGPT.

This is useful if you want to format the response in some way,
such as filling paragraphs, adding annotations or recording
information in the response like links.

Each function in this hook receives two arguments, the response
string to transform and the ChatGPT interaction buffer. It should
return the transformed string."
  :group 'gptel
  :type 'hook)

(defvar gptel-default-session "*ChatGPT*")
(defvar gptel-default-mode (if (featurep 'markdown-mode)
                               'markdown-mode
                             'text-mode))

;; TODO: Handle `prog-mode' using the `comment-start' variable
(defcustom gptel-prompt-prefix-alist
  '((markdown-mode . "### ")
    (org-mode . "*** ")
    (text-mode . "### "))
  "String inserted after the response from ChatGPT.

This is an alist mapping major modes to the prefix strings. This
is only inserted in dedicated gptel buffers."
  :group 'gptel
  :type '(alist :key-type symbol :value-type string))

;; Model and interaction parameters
(defvar-local gptel--system-message
  "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")

(defcustom gptel-directives
  `((default . ,gptel--system-message)
    (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
    (writing . "You are a large language model and a writing assistant. Respond concisely.")
    (chat . "You are a large language model and a conversation partner. Respond concisely."))
  "System prompts (directives) for ChatGPT.

These are system instructions sent at the beginning of each
request to ChatGPT.

Each entry in this alist maps a symbol naming the directive to
the string that is sent. To set the directive for a chat session
interactively call `gptel-send' with a prefix argument.

Note: Currently the names (default, programming, writing and
chat) are hard-coded and only their values may be customized.
This will be fixed in an upcoming release."
  :group 'gptel
  :type '(alist :key-type symbol :value-type string))

(defcustom gptel-max-tokens nil
  "Max tokens per response.

This is roughly the number of words in the response. 100-300 is a
reasonable range for short answers, 400 or more for longer
responses.

To set the target token count for a chat session interactively
call `gptel-send' with a prefix argument.

If left unset, ChatGPT will target about 40% of the total token
count of the conversation so far in each message, so messages
will get progressively longer!"
  :local t
  :group 'gptel
  :type '(choice (integer :tag "Specify Token count")
                 (const :tag "Default" nil)))

(defcustom gptel-model "gpt-3.5-turbo"
  "GPT Model for chat.

The current options are
- \"gpt-3.5-turbo\"
- \"gpt-3.5-turbo-0301\"
- \"gpt-4\" (experimental)

To set the model for a chat session interactively call
`gptel-send' with a prefix argument."
  :local t
  :group 'gptel
  :type '(choice
          (const :tag "GPT 3.5 turbo" "gpt-3.5-turbo")
          (const :tag "GPT 3.5 turbo 0301" "gpt-3.5-turbo-0301")
          (const :tag "GPT 4 (experimental)" "gpt-4")))

(defcustom gptel-temperature 1.0
  "\"Temperature\" of ChatGPT response.

This is a number between 0.0 and 2.0 that controls the randomness
of the response, with 2.0 being the most random.

To set the temperature for a chat session interactively call
`gptel-send' with a prefix argument."
  :local t
  :group 'gptel
  :type 'number)

(defvar-local gptel--num-messages-to-send nil)
(defvar gptel--debug nil)

(defun gptel-api-key-from-auth-source (&optional host user)
  "Lookup api key in the auth source.
By default, \"openai.com\" is used as HOST and \"apikey\" as USER."
  (if-let ((secret (plist-get (car (auth-source-search
                                    :host (or host "openai.com")
                                    :user (or user "apikey")))
                              :secret)))
      (if (functionp secret) (funcall secret) secret)
    (user-error "No `gptel-api-key' found in the auth source")))

(defun gptel--api-key ()
  "Get api key from `gptel-api-key'."
  (pcase gptel-api-key
    ((pred stringp) gptel-api-key)
    ((pred functionp) (funcall gptel-api-key))
    (_ (error "`gptel-api-key' is not set"))))

(defun gptel--update-header-line (msg face)
  "Update header line with status MSG in FACE."
  (and header-line-format
    (setf (nth 1 header-line-format)
          (propertize msg 'face face))
    (force-mode-line-update)))

(defsubst gptel--numberize (val)
  "Ensure VAL is a number."
  (if (stringp val) (string-to-number val) val))

(defun gptel-prompt-string ()
  (or (alist-get major-mode gptel-prompt-prefix-alist) ""))

(defvar-local gptel--old-header-line nil)
(define-minor-mode gptel-mode
  "Minor mode for interacting with ChatGPT."
  :glboal nil
  :lighter " GPT"
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c RET") #'gptel-send)
    map)
  (if gptel-mode
      (setq gptel--old-header-line header-line-format
            header-line-format
            (list (concat (propertize " " 'display '(space :align-to 0))
                          (format "%s" (buffer-name)))
                  (propertize " Ready" 'face 'success)))
    (setq header-line-format gptel--old-header-line)))

;; TODO: Handle read-only buffers. Should we spawn a new buffer automatically?
;; TODO: Handle multiple requests(#15). (Only one request from one buffer at a time?)
;; TODO: Since we capture a marker for the insertion location, `gptel-buffer' no
;; longer needs to be recorded
;;;###autoload
(defun gptel-send (&optional arg)
  "Submit this prompt to ChatGPT.

With prefix arg ARG activate a transient menu with more options
instead."
  (interactive "P")
  (if (and arg (require 'gptel-transient nil t))
      (call-interactively #'gptel-send-menu)
  (message "Querying ChatGPT...")
  (let* ((response-pt
          (if (use-region-p)
              (set-marker (make-marker) (region-end))
            (point-marker)))
         (gptel-buffer (current-buffer))
         (full-prompt (gptel--create-prompt response-pt)))
    (funcall
     (if gptel-use-curl
         #'gptel-curl-get-response #'gptel--url-get-response)
     (list :prompt full-prompt
           :gptel-buffer gptel-buffer
           :insert-marker response-pt)))
    (gptel--update-header-line " Waiting..." 'warning)))

(defun gptel--insert-response (response info)
  "Insert RESPONSE from ChatGPT into the gptel buffer.

INFO is a plist containing information relevant to this buffer.
See `gptel--url-get-response' for details."
  (let* ((content-str (plist-get response :content))
         (status-str  (plist-get response :status))
         (gptel-buffer (plist-get info :gptel-buffer))
         (response-pt (plist-get info :insert-marker)))
    (if content-str
        (with-current-buffer gptel-buffer
          (setq content-str (gptel--transform-response
                             content-str gptel-buffer))
          (save-excursion
            (put-text-property 0 (length content-str) 'gptel 'response content-str)
            (message "Querying ChatGPT... done.")
            (goto-char response-pt)
            (unless (bobp) (insert-before-markers-and-inherit "\n\n"))
            (if gptel-playback
                (gptel--playback gptel-buffer content-str response-pt)
              (let ((p (point)))
                (insert content-str)
                (pulse-momentary-highlight-region p (point)))
              (when gptel-mode
                (insert "\n\n" (gptel-prompt-string))
                (gptel--update-header-line " Ready" 'success))))
          (goto-char (- (point) 2)))
      (gptel--update-header-line
       (format " Response Error: %s" status-str) 'error))))

(defun gptel--create-prompt (&optional prompt-end)
  "Return a full conversation prompt from the contents of this buffer.

If `gptel--num-messages-to-send' is set, limit to that many
recent exchanges.

If the region is active limit the prompt to the region contents
instead.

If PROMPT-END (a marker) is provided, end the prompt contents
there."
  (save-excursion
    (save-restriction
      (if (use-region-p)
          (progn (narrow-to-region (region-beginning) (region-end))
                 (goto-char (point-max)))
        (goto-char (or prompt-end (point-max))))
      (let ((max-entries (and gptel--num-messages-to-send
                              (* 2 (gptel--numberize
                                    gptel--num-messages-to-send))))
            (prop) (prompts))
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
                       "[*# \t\n\r]+"))
                prompts)
          (and max-entries (cl-decf max-entries)))
        (cons (list :role "system"
                    :content gptel--system-message)
              prompts)))))

(defun gptel--request-data (prompts)
  "JSON encode PROMPTS for sending to ChatGPT."
  (let ((prompts-plist
         `(:model ,gptel-model
           :messages [,@prompts])))
    (when gptel-temperature
      (plist-put prompts-plist :temperature (gptel--numberize gptel-temperature)))
    (when gptel-max-tokens
      (plist-put prompts-plist :max_tokens (gptel--numberize gptel-max-tokens)))
    prompts-plist))

;; TODO: Use `run-hook-wrapped' with an accumulator instead to handle
;; buffer-local hooks, etc.
(defun gptel--transform-response (content-str buffer)
  "Filter CONTENT-STR through `gptel-response-filter-functions`.

BUFFER is passed along with CONTENT-STR to each function in this
hook."
  (let ((filtered-str content-str))
    (dolist (filter-func gptel-response-filter-functions filtered-str)
      (condition-case nil
          (when (functionp filter-func)
            (setq filtered-str
                  (funcall filter-func filtered-str buffer)))
        (error
         (display-warning '(gptel filter-functions)
                          (format "Function %S returned an error"
                                  filter-func)))))))

(defun gptel--convert-org (content buffer)
  "Transform CONTENT according to required major-mode.

Currently only `org-mode' is handled.

BUFFER is the interaction buffer for ChatGPT."
  (pcase (buffer-local-value 'major-mode buffer)
    ('org-mode (gptel--convert-markdown->org content))
    (_ content)))

(defun gptel--url-get-response (info)
  "Fetch response to prompt in INFO from ChatGPT.

INFO is a plist with the following keys:
- :prompt (the prompt being sent)
- :gptel-buffer (the gptel buffer)
- :insert-marker (marker at which to insert the response)."
  (let* ((inhibit-message t)
         (message-log-max nil)
         (url-request-method "POST")
         (url-request-extra-headers
         `(("Content-Type" . "application/json")
           ("Authorization" . ,(concat "Bearer " (gptel--api-key)))))
        (url-request-data
         (encode-coding-string
          (json-encode (gptel--request-data (plist-get info :prompt)))
          'utf-8)))
    (url-retrieve "https://api.openai.com/v1/chat/completions"
                  (lambda (_)
                    (let ((response
                           (gptel--url-parse-response (current-buffer))))
                      (gptel--insert-response response info)
                      (kill-buffer)))
                  nil t nil)))

(defun gptel--url-parse-response (response-buffer)
  "Parse response in RESPONSE-BUFFER."
  (when (buffer-live-p response-buffer)
    (when gptel--debug
      (with-current-buffer response-buffer
        (clone-buffer "*gptel-error*" 'show)))
    (with-current-buffer response-buffer
      (if-let* ((status (buffer-substring (line-beginning-position) (line-end-position)))
                (json-object-type 'plist)
                (response (progn (forward-paragraph)
                                 (condition-case nil
                                         (json-read)
                                       (json-readtable-error 'json-read-error)))))
          (cond
           ((string-match-p "200 OK" status)
            (list :content (string-trim
                            (decode-coding-string
                             (map-nested-elt
                              response '(:choices 0 :message :content))
                             'utf-8))
                  :status status))
           ((plist-get response :error)
            (let* ((error-plist (plist-get response :error))
                   (error-msg (plist-get error-plist :message))
                   (error-type (plist-get error-plist :type)))
              (message "ChatGPT error: %s" error-msg)
              (list :content nil :status (concat status ": " error-type))))
           ((eq response 'json-read-error)
            (message "ChatGPT error: Malformed JSON in response.")
            (list :content nil :status (concat status ": Malformed JSON in response.")))
           (t (message "ChatGPT error: Could not parse HTTP response.")
              (list :content nil :status (concat status ": Could not parse HTTP response."))))
        (message "ChatGPT error: Could not parse HTTP response.")
        (list :content nil
              :status (concat status ": Could not parse HTTP response."))))))

;;;###autoload
(defun gptel (name &optional api-key initial)
  "Switch to or start ChatGPT session with NAME.

With a prefix arg, query for a (new) session name.

Ask for API-KEY if `gptel-api-key' is unset.

If region is active, use it as the INITIAL prompt. Returns the
buffer created or switched to."
  (interactive (list (if current-prefix-arg
                         (read-string "Session name: " (generate-new-buffer-name gptel-default-session))
                       gptel-default-session)
                     (condition-case nil
                         (gptel--api-key)
                       ((error user-error)
                        (setq gptel-api-key
                              (read-passwd "OpenAI API key: "))))
                     (and (use-region-p)
                          (buffer-substring (region-beginning)
                                            (region-end)))))
  (unless api-key
    (user-error "No API key available"))
  (with-current-buffer (get-buffer-create name)
    (cond ;Set major mode
     ((eq major-mode gptel-default-mode))
     ((eq gptel-default-mode 'text-mode)
      (text-mode)
      (visual-line-mode 1))
     (t (funcall gptel-default-mode)))
    (unless gptel-mode (gptel-mode 1))
    (if (bobp) (insert (or initial (gptel-prompt-string))))
    (pop-to-buffer (current-buffer))
    (goto-char (point-max))
    (skip-chars-backward "\t\r\n")
    (message "Send your query with %s!"
             (substitute-command-keys "\\[gptel-send]"))))

(defun gptel--convert-markdown->org (str)
  "Convert string STR from markdown to org markup.

This is a very basic converter that handles only a few markup
elements."
  (interactive)
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (while (re-search-forward "`\\|\\*\\{1,2\\}\\|_" nil t)
      (pcase (match-string 0)
        ("`" (if (looking-at "``")
                 (progn (backward-char)
                        (delete-char 3)
                        (insert "#+begin_src ")
                        (when (re-search-forward "^```" nil t)
                          (replace-match "#+end_src")))
               (replace-match "=")))
        ("**" (cond
               ((looking-at "\\*\\(?:[[:word:]]\\|\s\\)")
                (delete-char 1))
               ((looking-back "\\(?:[[:word:]]\\|\s\\)\\*\\{2\\}"
                              (max (- (point) 3) (point-min)))
                (backward-delete-char 1))))
        ((or "_" "*")
         (if (save-match-data
               (and (looking-back "\\(?:[[:space:]]\\|\s\\)\\(?:_\\|\\*\\)"
                                  (max (- (point) 2) (point-min)))
                    (not (looking-at "[[:space:]]\\|\s"))))
             ;; Possible beginning of italics
             (and
              (save-excursion
                (when (and (re-search-forward (regexp-quote (match-string 0)) nil t)
                           (looking-at "[[:space]]\\|\s")
                           (not (looking-back "\\(?:[[:space]]\\|\s\\)\\(?:_\\|\\*\\)"
                                              (max (- (point) 2) (point-min)))))
                  (backward-delete-char 1)
                  (insert "/") t))
              (progn (backward-delete-char 1)
                     (insert "/")))))))
    (buffer-string)))

(defun gptel--playback (buf content-str start-pt)
  "Playback CONTENT-STR in BUF.

Begin at START-PT."
  (let ((handle (gensym "gptel-change-group-handle--"))
        (playback-timer (gensym "gptel--playback-"))
        (content-length (length content-str))
        (idx 0) (pt (copy-marker start-pt t)))
    (setf (symbol-value handle) (prepare-change-group buf))
    (activate-change-group (symbol-value handle))
    (setf (symbol-value playback-timer)
          (run-at-time
          0 0.15
           (lambda ()
             (with-current-buffer buf
               (if (>= content-length idx)
                   (progn
                     (goto-char pt)
                     (insert
                      (seq-subseq
                       content-str idx
                       (min content-length (+ idx 16))))
                     (setq idx (+ idx 16)))
                 (when gptel-mode
                   (insert "\n\n" (gptel-prompt-string))
                   (gptel--update-header-line " Ready" 'success))
                 (when start-pt (goto-char (marker-position start-pt)))
                 (accept-change-group (symbol-value handle))
                 (undo-amalgamate-change-group (symbol-value handle))
                 (cancel-timer (symbol-value playback-timer)))))))
    nil))

(provide 'gptel)
;;; gptel.el ends here
