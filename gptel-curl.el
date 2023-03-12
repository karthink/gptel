;;; gptel-curl.el --- Curl support for GPTel         -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Karthik Chikmagalur

;; Author: Karthik Chikmagalur;; <karthikchikmagalur@gmail.com>
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

;; Curl support for GPTel.  Utility functions.

;;; Code:

(require 'gptel)

(eval-when-compile
  (require 'subr-x))
(require 'map)
(require 'json)
(require 'aio)

(defvar gptel-curl--process-alist nil
  "Alist of active GPTel curl requests.")

(defun gptel-curl--get-args (prompts token)
  "Produce list of arguments for calling Curl.

PROMPTS is the data to send, TOKEN is a unique identifier."
  (let* ((args
          (list "--location" "--silent" "--compressed" "--disable"))
         (url "https://api.openai.com/v1/chat/completions")
         (data (encode-coding-string
                (json-encode (gptel--request-data prompts))
                'utf-8))
         (api-key
          (cond
           ((stringp gptel-api-key) gptel-api-key)
           ((functionp gptel-api-key) (funcall gptel-api-key))
           (t (setq gptel-api-key (read-passwd "OpenAI API key: ")))))
         (headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(concat "Bearer " api-key)))))

    (push (format "-X%s" "POST") args)
    (push (format "-w(%s . %%{size_header})" token) args)
    ;; (push (format "--keepalive-time %s" 240) args)
    (push (format "-m%s" 60) args)
    (push "-D-" args)
    (pcase-dolist (`(,key . ,val) headers)
      (push (format "-H%s: %s" key val) args))
    (push (format "-d%s" data) args)
    (nreverse (cons url args))))

;;;###autoload
(defun gptel-curl-get-response (prompts)
  "Retrieve response to PROMPTS."
  (with-current-buffer (generate-new-buffer "*gptel-curl*")
    (let* ((token (md5 (format "%s%s%s%s"
                               (random) (emacs-pid) (user-full-name)
                               (recent-keys))))
           (args (gptel-curl--get-args prompts token))
           (process (apply #'start-process "gptel-curl" (current-buffer)
                           "curl" args))
           (promise (aio-promise))
           (cb (lambda (result)
                 (aio-resolve promise (lambda () result))
                 (setf (alist-get process
                                  gptel-curl--process-alist nil 'remove)
                       nil))))
      (prog1 promise
        (set-process-query-on-exit-flag process nil)
        (setf (alist-get process gptel-curl--process-alist)
              (list :callback cb :token token))
        (set-process-sentinel process #'gptel-curl--sentinel)))))

(defun gptel-curl--sentinel (process status)
  "Process sentinel for GPTel curl requests.

PROCESS and STATUS are process parameters."
  (let ((proc-buf (process-buffer process)))
    (when gptel--debug
      (with-current-buffer proc-buf
        (clone-buffer "*gptel-error*" 'show)))
    (if-let* ((ok-p (equal status "finished\n"))
              (proc-info (alist-get process gptel-curl--process-alist))
              (proc-token (plist-get proc-info :token))
              (content (gptel-curl--parse-response proc-buf proc-token)))
        (funcall (plist-get proc-info :callback) content)
      ;; Failed
      (funcall (plist-get proc-info :callback) nil))
    (kill-buffer proc-buf)))

(defun gptel-curl--parse-response (buf token)
  "Parse the buffer BUF with curl's response.

TOKEN is used to disambiguate multiple requests in a single
buffer."
  (with-current-buffer buf
    (progn
      (goto-char (point-max))
      (search-backward token)
      (backward-char)
      (pcase-let* ((`(,_ . ,header-size) (read (current-buffer))))
          ;; (if (search-backward token nil t)
          ;;     (search-forward ")" nil t)
          ;;   (goto-char (point-min)))
          (goto-char (point-min))

          (if-let* ((http-msg (buffer-substring (line-beginning-position)
                                                (line-end-position)))
                    (http-status
                     (save-match-data
                       (and (string-match "HTTP/[.0-9]+ +\\([0-9]+\\)" http-msg)
                            (match-string 1 http-msg))))
                    (json-object-type 'plist)
                    (response (progn (goto-char header-size)
                                     (json-read)))
                    (content (map-nested-elt
                              response '(:choices 0 :message :content))))
              (cond
               ((not (equal http-status "200"))
                (message "GPTChat request failed with code %s" http-status)
                (list :content nil :status http-msg))
               (content
                (list :content (string-trim content) :status http-msg))
               (t (message "Could not parse response from ChatGPT.")
                  (list :content nil :status http-msg))))))))

(provide 'gptel-curl)
;;; gptel-curl.el ends here
