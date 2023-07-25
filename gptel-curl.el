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
  (require 'cl-lib)
  (require 'subr-x))
(require 'map)
(require 'json)

(defvar gptel-curl--process-alist nil
  "Alist of active GPTel curl requests.")

(defun gptel-curl--get-args (prompts token)
  "Produce list of arguments for calling Curl.

PROMPTS is the data to send, TOKEN is a unique identifier."
  (let* ((url (format "https://%s/v1/chat/completions" gptel-host))
         (data (encode-coding-string
                (json-encode (gptel--request-data prompts))
                'utf-8))
         (headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(concat "Bearer " (gptel--api-key))))))
    (append
     (list "--location" "--silent" "--compressed" "--disable"
           (format "-X%s" "POST")
           (format "-w(%s . %%{size_header})" token)
           (format "-m%s" 60)
           "-D-"
           (format "-d%s" data))
     (when (not (string-empty-p gptel-proxy))
       (list "--proxy" gptel-proxy
             "--proxy-negotiate"
             "--proxy-user" ":"))
     (cl-loop for (key . val) in headers
              collect (format "-H%s: %s" key val))
     (list url))))

;;TODO: The :transformer argument here is an alternate implementation of
;;`gptel-response-filter-functions'. The two need to be unified.
;;;###autoload
(defun gptel-curl-get-response (info &optional callback)
  "Retrieve response to prompt in INFO.

INFO is a plist with the following keys:
- :prompt (the prompt being sent)
- :buffer (the gptel buffer)
- :position (marker at which to insert the response).

Call CALLBACK with the response and INFO afterwards. If omitted
the response is inserted into the current buffer after point."
  (let* ((token (md5 (format "%s%s%s%s"
                             (random) (emacs-pid) (user-full-name)
                             (recent-keys))))
         (args (gptel-curl--get-args (plist-get info :prompt) token))
         (process (apply #'start-process "gptel-curl"
                         (generate-new-buffer "*gptel-curl*") "curl" args)))
    (with-current-buffer (process-buffer process)
      (set-process-query-on-exit-flag process nil)
      (setf (alist-get process gptel-curl--process-alist)
            (nconc (list :token token
                         :callback (or callback
                                       (if gptel-stream
                                           #'gptel-curl--stream-insert-response
                                         #'gptel--insert-response))
                         :transformer (when (eq (buffer-local-value
                                                 'major-mode
                                                 (plist-get info :buffer))
                                                'org-mode)
                                        (gptel--stream-convert-markdown->org)))
                   info))
      (if gptel-stream
          (progn (set-process-sentinel process #'gptel-curl--stream-cleanup)
                 (set-process-filter process #'gptel-curl--stream-filter))
        (set-process-sentinel process #'gptel-curl--sentinel)))))

(defun gptel-abort (buf)
  "Stop any active gptel process associated with the current buffer."
  (interactive (list (current-buffer)))
  (unless gptel-use-curl
    (user-error "Cannot stop a `url-retrieve' request!"))
  (if-let* ((proc-attrs
            (cl-find-if
             (lambda (proc-list)
               (eq (plist-get (cdr proc-list) :buffer) buf))
             gptel-curl--process-alist))
            (proc (car proc-attrs)))
      (progn
        (setf (alist-get proc gptel-curl--process-alist nil 'remove) nil)
        (set-process-sentinel proc #'ignore)
        (delete-process proc)
        (kill-buffer (process-buffer proc))
        (with-current-buffer buf
          (when gptel-mode (gptel--update-header-line  " Ready" 'success)))
        (message "Stopped gptel request in buffer %S" (buffer-name buf)))
    (message "No gptel request associated with buffer %S" (buffer-name buf))))

;; TODO: Separate user-messaging from this function
(defun gptel-curl--stream-cleanup (process _status)
  "Process sentinel for GPTel curl requests.

PROCESS and _STATUS are process parameters."
  (let ((proc-buf (process-buffer process)))
    (when gptel--debug
      (with-current-buffer proc-buf
        (clone-buffer "*gptel-error*" 'show)))
    (let* ((info (alist-get process gptel-curl--process-alist))
           (gptel-buffer (plist-get info :buffer))
           (tracking-marker (plist-get info :tracking-marker))
           (start-marker (plist-get info :position))
           (http-status (plist-get info :http-status))
           (http-msg (plist-get info :status)))
      (if (equal http-status "200")
          (progn
            ;; Finish handling response
            (with-current-buffer (marker-buffer start-marker)
              (pulse-momentary-highlight-region (+ start-marker 2) tracking-marker)
              (when gptel-mode (save-excursion (goto-char tracking-marker)
                                               (insert "\n\n" (gptel-prompt-string)))))
            (with-current-buffer gptel-buffer
              (when gptel-mode (gptel--update-header-line  " Ready" 'success))))
        ;; Or Capture error message
        (with-current-buffer proc-buf
          (goto-char (point-max))
          (search-backward (plist-get info :token))
          (backward-char)
          (pcase-let* ((`(,_ . ,header-size) (read (current-buffer)))
                       (json-object-type 'plist)
                       (response (progn (goto-char header-size)
                                        (condition-case nil (json-read)
                                          (json-readtable-error 'json-read-error)))))
            (cond
             ((plist-get response :error)
              (let* ((error-plist (plist-get response :error))
                     (error-msg (plist-get error-plist :message))
                     (error-type (plist-get error-plist :type)))
                (message "ChatGPT error: (%s) %s" http-msg error-msg)
                (setq http-msg (concat "("  http-msg ") " (string-trim error-type)))))
             ((eq response 'json-read-error)
              (message "ChatGPT error (%s): Malformed JSON in response." http-msg))
             (t (message "ChatGPT error (%s): Could not parse HTTP response." http-msg)))))
        (with-current-buffer gptel-buffer
          (when gptel-mode
            (gptel--update-header-line
             (format " Response Error: %s" http-msg) 'error))))
      (with-current-buffer gptel-buffer
        (run-hooks 'gptel-post-response-hook)))
    (setf (alist-get process gptel-curl--process-alist nil 'remove) nil)
    (kill-buffer proc-buf)))

(defun gptel-curl--stream-insert-response (response info)
  "Insert streaming RESPONSE from ChatGPT into the gptel buffer.

INFO is a mutable plist containing information relevant to this buffer.
See `gptel--url-get-response' for details."
  (let ((start-marker (plist-get info :position))
        (tracking-marker (plist-get info :tracking-marker))
        (transformer (plist-get info :transformer)))
    (when response
        (with-current-buffer (marker-buffer start-marker)
          (save-excursion
            (unless tracking-marker
              (gptel--update-header-line " Typing..." 'success)
              (goto-char start-marker)
              (unless (or (bobp) (plist-get info :in-place))
                (insert "\n\n"))
              (setq tracking-marker (set-marker (make-marker) (point)))
              (set-marker-insertion-type tracking-marker t)
              (plist-put info :tracking-marker tracking-marker))
            
            (when transformer
              (setq response (funcall transformer response)))
            
            (put-text-property 0 (length response) 'gptel 'response response)
            (goto-char tracking-marker)
            (insert response))))))

(defun gptel-curl--stream-filter (process output)
  (let* ((proc-info (alist-get process gptel-curl--process-alist)))
    (with-current-buffer (process-buffer process)
      ;; Insert output
      (save-excursion
        (goto-char (process-mark process))
        (insert output)
        (set-marker (process-mark process) (point)))
      
      ;; Find HTTP status
      (unless (plist-get proc-info :http-status)
        (save-excursion
          (goto-char (point-min))
          (when-let* (((not (= (line-end-position) (point-max))))
                      (http-msg (buffer-substring (line-beginning-position)
                                                  (line-end-position)))
                      (http-status
                       (save-match-data
                         (and (string-match "HTTP/[.0-9]+ +\\([0-9]+\\)" http-msg)
                              (match-string 1 http-msg)))))
            (plist-put proc-info :http-status http-status)
            (plist-put proc-info :status (string-trim http-msg))))
        ;; Handle read-only gptel buffer
        (when (with-current-buffer (plist-get proc-info :buffer)
                (or buffer-read-only
                    (get-char-property (plist-get proc-info :position) 'read-only)))
          (message "Buffer is read only, displaying reply in buffer \"*ChatGPT response*\"")
          (display-buffer
           (with-current-buffer (get-buffer-create "*ChatGPT response*")
             (goto-char (point-max))
             (move-marker (plist-get proc-info :position) (point) (current-buffer))
             (current-buffer))
           '((display-buffer-reuse-window
              display-buffer-pop-up-window)
             (reusable-frames . visible))))
        ;; Run pre-response hook
        (when (and (equal (plist-get proc-info :http-status) "200")
                   gptel-pre-response-hook)
          (with-current-buffer (marker-buffer (plist-get proc-info :position))
            (run-hooks 'gptel-pre-response-hook))))
      
      (when-let ((http-msg (plist-get proc-info :status))
                 (http-status (plist-get proc-info :http-status)))
        ;; Find data chunk(s) and run callback
        (when (equal http-status "200")
          (funcall (or (plist-get proc-info :callback)
                       #'gptel-curl--stream-insert-response)
                   (let* ((json-object-type 'plist)
                          (content-strs))
                     (condition-case nil
                         (while (re-search-forward "^data:" nil t)
                           (save-match-data
                             (unless (looking-at " *\\[DONE\\]")
                               (when-let* ((response (json-read))
                                           (delta (map-nested-elt
                                                   response '(:choices 0 :delta)))
                                           (content (plist-get delta :content)))
                                 (push content content-strs)))))
                       (error
                        (goto-char (match-beginning 0))))
                     (apply #'concat (nreverse content-strs)))
                   proc-info))))))

(defun gptel-curl--sentinel (process _status)
  "Process sentinel for GPTel curl requests.

PROCESS and _STATUS are process parameters."
  (let ((proc-buf (process-buffer process)))
    (when gptel--debug
      (with-current-buffer proc-buf
        (clone-buffer "*gptel-error*" 'show)))
    (when-let* (((eq (process-status process) 'exit))
                (proc-info (alist-get process gptel-curl--process-alist))
                (proc-token (plist-get proc-info :token))
                (proc-callback (plist-get proc-info :callback)))
      (pcase-let ((`(,response ,http-msg ,error)
                   (gptel-curl--parse-response proc-buf proc-token)))
        (plist-put proc-info :status http-msg)
        (when error (plist-put proc-info :error error))
        (funcall proc-callback response proc-info)))
    (setf (alist-get process gptel-curl--process-alist nil 'remove) nil)
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

          (if-let* ((http-msg (string-trim
                               (buffer-substring (line-beginning-position)
                                                 (line-end-position))))
                    (http-status
                     (save-match-data
                       (and (string-match "HTTP/[.0-9]+ +\\([0-9]+\\)" http-msg)
                            (match-string 1 http-msg))))
                    (json-object-type 'plist)
                    (response (progn (goto-char header-size)
                                     (condition-case nil
                                         (json-read)
                                       (json-readtable-error 'json-read-error)))))
              (cond
               ((equal http-status "200")
                (list (string-trim
                       (map-nested-elt response '(:choices 0 :message :content)))
                      http-msg))
                ((plist-get response :error)
                 (let* ((error-plist (plist-get response :error))
                        (error-msg (plist-get error-plist :message))
                        (error-type (plist-get error-plist :type)))
                   (list nil (concat "(" http-msg ") " (string-trim error-type)) error-msg)))
                ((eq response 'json-read-error)
                 (list nil (concat "(" http-msg ") Malformed JSON in response.")
                       "Malformed JSON in response"))
                (t (list nil (concat "(" http-msg ") Could not parse HTTP response.")
                         "Could not parse HTTP response.")))
            (list nil (concat "(" http-msg ") Could not parse HTTP response.")
                  "Could not parse HTTP response."))))))

(provide 'gptel-curl)
;;; gptel-curl.el ends here
