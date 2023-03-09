;;; gptel.el --- A simple ChatGPT client      -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Karthik Chikmagalur

;; Author: Karthik Chikmagalur
;; Version: 0.05
;; Package-Requires: ((emacs "27.1") (aio "1.0"))
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

;; A ChatGPT client for Emacs.
;;
;; Requirements:
;; - You need an OpenAI API key. Set the variable `gptel-api-key' to the key or to
;;   a function of no arguments that returns the key.
;;
;; - If installing manually: Install the package `emacs-aio' using `M-x package-install'
;;   or however you install packages.
;;
;; - Not required but recommended: Install `markdown-mode'.
;;
;; Usage:
;; - M-x gptel: Start a ChatGPT session
;; - C-u M-x gptel: Start another or multiple independent ChatGPT sessions
;;
;; - In the GPT session: Press `C-c RET' (control + c, followed by return) to send
;;   your prompt.
;; - To jump between prompts, use `C-c C-n' and `C-c C-p'.

;;; Code:
(declare-function markdown-mode "markdown-mode")
(declare-function gptel-curl-get-response "gptel-curl")

(eval-when-compile
  (require 'subr-x)
  (require 'cl-lib))

(require 'aio)
(require 'json)
(require 'map)
(require 'text-property-search)

(defcustom gptel-api-key nil
  "An OpenAI API key (string).

Can also be a function of no arguments that returns an API
key (more secure)."
  :group 'gptel
  :type '(choice
          (string :tag "API key")
          (function :tag "Function that retuns the API key")))

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

(defvar gptel-default-session "*ChatGPT*")
(defvar gptel-default-mode (if (featurep 'markdown-mode)
                               'markdown-mode
                             'text-mode))
(defvar gptel-prompt-string "### ")

(aio-defun gptel-send ()
(defvar-local gptel--num-messages-to-send nil)

(defsubst gptel--numberize (val)
  "Ensure VAL is a number."
  (if (stringp val) (string-to-number val) val))

  "Submit this prompt to ChatGPT."
  (interactive)
  (message "Querying ChatGPT...")
  (setf (nth 1 header-line-format)
        (propertize " Waiting..." 'face 'warning))
  (let* ((gptel-buffer (current-buffer))
         (full-prompt (gptel--create-prompt))
         (response (aio-await
                    (funcall
                     (if gptel-use-curl
                         #'gptel-curl-get-response #'gptel--get-response)
                     full-prompt)))
         (content-str (plist-get response :content))
         (status-str  (plist-get response :status)))
    (if content-str
            (with-current-buffer gptel-buffer
              (save-excursion
                (put-text-property 0 (length content-str) 'gptel 'response content-str)
                (message "Querying ChatGPT... done.")
                (goto-char (point-max))
                (display-buffer (current-buffer)
                                '((display-buffer-reuse-window
                                   display-buffer-use-some-window)))
                (unless (bobp) (insert "\n\n"))
                (if gptel-playback
                    (gptel--playback (current-buffer) content-str (point))
                  (insert content-str))
                (insert "\n\n" gptel-prompt-string)
                (unless gptel-playback
                  (setf (nth 1 header-line-format)
                        (propertize " Ready" 'face 'success)))))
          (setf (nth 1 header-line-format)
                (propertize (format " Response Error: %s" status-str)
                            'face 'error))))))

(defun gptel--create-prompt ()
  "Return a full conversation prompt from the contents of this buffer.

If `gptel--num-messages-to-send' is set, limit to that many
recent exchanges.

If the region is active limit the prompt to the region contents
instead."
  (save-excursion
    (save-restriction
      (when (use-region-p)
        (narrow-to-region (region-beginning) (region-end)))
      (goto-char (point-max))
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
                    :content
                    (concat
                     (when (eq major-mode 'org-mode)
                       (concat
                        "In this conversation, format your responses as in an org-mode buffer in Emacs."
                        " Do NOT use Markdown. I repeat, use org-mode markup and not markdown.\n"))
                     gptel--system-message))
              prompts)))))


(aio-defun gptel--get-response (prompts)
  "Fetch response for PROMPTS from ChatGPT.

Return the message received."
  (let* ((inhibit-message t)
         (message-log-max nil)
         (api-key
          (cond
           ((stringp gptel-api-key) gptel-api-key)
           ((functionp gptel-api-key) (funcall gptel-api-key))))
         (url-request-method "POST")
         (url-request-extra-headers
         `(("Content-Type" . "application/json")
           ("Authorization" . ,(concat "Bearer " api-key))))
        (url-request-data
         (encode-coding-string
          (json-encode
          `(:model "gpt-3.5-turbo"
            ;; :temperature 1.0
            ;; :top_p 1.0
            :messages [,@prompts]))
          'utf-8)))
    (pcase-let ((`(,_ . ,response-buffer)
                 (aio-await
                  (aio-url-retrieve "https://api.openai.com/v1/chat/completions"))))
      (prog1
          (gptel--parse-response response-buffer)
        (kill-buffer response-buffer)))))

(defun gptel--parse-response (response-buffer)
  "Parse response in RESPONSE-BUFFER."
  (when (buffer-live-p response-buffer)
    (with-current-buffer response-buffer
      (if-let* ((status (buffer-substring (line-beginning-position) (line-end-position)))
                ((string-match-p "200 OK" status))
                (response (progn (forward-paragraph)
                                 (json-read)))
                (content (map-nested-elt
                          response '(:choices 0 :message :content))))
          (list :content (string-trim content)
                :status status)
        (list :content nil :status status)))))

(define-minor-mode gptel-mode
  "Minor mode for interacting with ChatGPT."
  :glboal nil
  :lighter " GPT"
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c RET") #'gptel-send)
    map))

;;;###autoload
(defun gptel (name &optional api-key)
  "Switch to or start ChatGPT session with NAME.

With a prefix arg, query for a (new) session name.

Ask for API-KEY if `gptel-api-key' is unset."
  (interactive (list (if current-prefix-arg
                         (read-string "Session name: " (generate-new-buffer-name gptel-default-session))
                       gptel-default-session)
                     (or gptel-api-key
                         (setq gptel-api-key
                               (read-passwd "OpenAI API key: ")))))
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
    (if (bobp) (insert gptel-prompt-string))
    (pop-to-buffer (current-buffer))
    (goto-char (point-max))
    (skip-chars-backward "\t\r\n")
    (or header-line-format
      (setq header-line-format
            (list (concat (propertize " " 'display '(space :align-to 0))
                          (format "%s" (buffer-name)))
                  (propertize " Ready" 'face 'success))))
    (message "Send your query with %s!"
             (substitute-command-keys "\\[gptel-send]"))))

(defun gptel--playback (buf content-str start-pt)
  "Playback CONTENT-STR in BUF.

Begin at START-PT."
  (let ((handle (gensym "gptel-change-group-handle--"))
        (playback-timer (gensym "gptel--playback-"))
        (content-length (length content-str))
        (idx 0) (pt (make-marker)))
    (setf (symbol-value handle) (prepare-change-group buf))
    (activate-change-group (symbol-value handle))
    (setf (symbol-value playback-timer)
          (run-at-time
          0 0.15
           (lambda ()
             (with-current-buffer buf
               (if (>= content-length idx)
                   (progn
                     (when (= idx 0) (set-marker pt start-pt))
                     (goto-char pt)
                     (insert-before-markers-and-inherit
                      (cl-subseq
                       content-str idx
                       (min content-length (+ idx 16))))
                     (setq idx (+ idx 16)))
                 (when start-pt (goto-char (- start-pt 2)))
                 (setf (nth 1 header-line-format)
                      (propertize " Ready" 'face 'success))
                 (force-mode-line-update)
                 (accept-change-group (symbol-value handle))
                 (undo-amalgamate-change-group (symbol-value handle))
                 (cancel-timer (symbol-value playback-timer)))))))
    nil))

(provide 'gptel)
;;; gptel.el ends here
