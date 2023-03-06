;;; gptel.el --- A simple ChatGPT client for Emacs      -*- lexical-binding: t; -*-

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
(eval-when-compile
  (require 'subr-x))

(require 'aio)
(require 'json)
(require 'map)

(defcustom gptel-api-key nil
  "An OpenAI API key (string).

Can also be a function of no arguments that returns an API
key (more secure)."
  :group 'gptel
  :type '(choice
          (string :tag "API key")
          (function :tag "Function that retuns the API key")))

(defvar-local gptel--prompt-markers nil)
(defvar gptel-default-session "*ChatGPT*")
(defvar gptel-default-mode (if (featurep 'markdown-mode)
                               'markdown-mode
                             'text-mode))
(defvar gptel-prompt-string "### ")

(aio-defun gptel-send ()
  "Submit this prompt to ChatGPT."
  (interactive)
  (message "Querying ChatGPT...")
  (unless (and gptel--prompt-markers
               (equal (marker-position (car gptel--prompt-markers))
                      (point-max)))
    (push (set-marker (make-marker) (point-max))
          gptel--prompt-markers))
  (let* ((gptel-buffer (current-buffer))
         (full-prompt
          (save-excursion
            (goto-char (point-min))
            (cl-loop with role = "user"
                     for (pm rm . _) on gptel--prompt-markers
                     collect
                     (list :role role
                           :content
                           (string-trim (buffer-substring-no-properties (or rm (point-min)) pm)
                                        "[*# \t\n\r]+"))
                     into prompts
                     do (setq role (if (equal role "user") "assistant" "user"))
                     finally return (nreverse prompts))))
         (response-buffer (aio-await (gptel-get-response full-prompt)))
         (json-object-type 'plist))
    (unwind-protect
        (when-let* ((content-str (gptel-parse-response response-buffer)))
          (with-current-buffer gptel-buffer
            (save-excursion
              (message "Querying ChatGPT... done.")
              (goto-char (point-max))
              (display-buffer (current-buffer)
                              '((display-buffer-reuse-window
                                 display-buffer-use-some-window)))
              (unless (bobp) (insert "\n\n"))
              ;; (if gptel-playback-response
              ;;     (aio-await (gptel--playback-print content-str))
              ;;   (insert content-str))
              (insert content-str)
              (push (set-marker (make-marker) (point))
                    gptel--prompt-markers)
              (insert "\n\n" gptel-prompt-string))))
      (kill-buffer response-buffer))))

(aio-defun gptel-get-response (prompts)
  "Fetch response for PROMPTS from ChatGPT.

Return the response buffer."
  (let* ((api-key
          (cond
           ((stringp gptel-api-key) gptel-api-key)
           ((functionp gptel-api-key) (funcall gptel-api-key))))
         (url-request-method "POST")
         (url-request-extra-headers
         `(("Content-Type" . "application/json")
           ("Authorization" . ,(concat "Bearer " api-key))))
        (url-request-data
         (json-encode
          `(:model "gpt-3.5-turbo"
            ;; :temperature 1.0
            ;; :top_p 1.0
            :messages [,@prompts]))))
    (pcase-let ((`(,_ . ,buffer)
                  (aio-await
                   (aio-url-retrieve "https://api.openai.com/v1/chat/completions"))))
      buffer)))

;;;###autoload
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
                         (read-string "OpenAI API key: "))))
  (unless api-key
    (user-error "No API key available"))
  (with-current-buffer (get-buffer-create name)
    (unless (eq major-mode gptel-default-mode) (funcall gptel-default-mode))
    (unless gptel-mode (gptel-mode 1))
    (if (bobp) (insert gptel-prompt-string))
    (pop-to-buffer (current-buffer))
    (goto-char (point-max))
    (skip-chars-backward "\t\r\n")
    (setq header-line-format
          (concat (propertize " " 'display '(space :align-to 0))
                  (format "ChatGPT session (%s)" (buffer-name))))
    (message "Send your query with %s!"
             (substitute-command-keys "\\[gptel-send]"))))

(defun gptel-parse-response (response-buffer)
  "Parse response in RESPONSE-BUFFER."
  (when (buffer-live-p response-buffer)
    (with-current-buffer response-buffer
      (if-let* ((status (buffer-substring (line-beginning-position) (line-end-position)))
                ((string-match "200 OK" status))
                (response (progn (forward-paragraph)
                                 (json-read))))
          (map-nested-elt response '(:choices 0 :message :content))
        (user-error "Chat failed with status: %S" status)))))

(defvar gptel-playback-response t)

(aio-defun gptel--playback-print (response)
  (when response
    (dolist (line (split-string response "\n" nil))
      (insert line "\n")
      (aio-await (aio-sleep 0.3)))))

(provide 'gptel)
;;; gptel.el ends here
