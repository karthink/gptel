;;; gptel-curl.el --- Curl support for gptel         -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025  Karthik Chikmagalur

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

;; Curl support for gptel.  Utility functions.

;;; Code:

(require 'gptel)

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))
(require 'map)

(declare-function json-read "json" ())
(defvar json-object-type)

(declare-function gptel--stream-convert-markdown->org "gptel-org")

(defcustom gptel-curl-extra-args nil
  "Extra arguments to pass to Curl when sending queries.

This should be a list of strings, each one a Curl command line
argument.  Note that these should not conflict with the options
in `gptel-curl--common-args', which gptel requires for correct
functioning.

If you want to specify extra arguments only when using a specific
gptel backend, use the `:curl-args' slot of the backend instead.
See `gptel-backend'."
  :group 'gptel
  :type '(repeat string))

(defconst gptel-curl--common-args
  (if (memq system-type '(windows-nt ms-dos))
      '("--disable" "--location" "--silent" "-XPOST"
        "-y7200" "-Y1" "-D-")
    '("--disable" "--location" "--silent" "--compressed"
      "-XPOST" "-y7200" "-Y1" "-D-"))
  "Arguments always passed to Curl for gptel queries.")

(defun gptel-curl--get-args (info token)
  "Produce list of arguments for calling Curl.

REQUEST-DATA is the data to send, TOKEN is a unique identifier."
  (let* ((data (plist-get info :data))
         ;; We have to let-bind the following two variables since their dynamic
         ;; values are used for key lookup and url resoloution
         (gptel-backend (plist-get info :backend))
         (gptel-stream (plist-get info :stream))
         (url (let ((backend-url (gptel-backend-url gptel-backend)))
                (if (functionp backend-url)
                    (with-current-buffer (plist-get info :buffer)
                      (funcall backend-url))
                  backend-url)))
         (data-json (encode-coding-string (gptel--json-encode data) 'utf-8))
         (headers
          (append '(("Content-Type" . "application/json"))
                  (when-let* ((header (gptel-backend-header gptel-backend)))
                    (if (functionp header)
                        (funcall header) header)))))
    (when gptel-log-level
      (when (eq gptel-log-level 'debug)
        (gptel--log (gptel--json-encode
                     (mapcar (lambda (pair) (cons (intern (car pair)) (cdr pair)))
                             headers))
                    "request headers"))
      (gptel--log data-json "request body"))
    (append
     gptel-curl--common-args
     gptel-curl-extra-args
     (and-let* ((curl-args (gptel-backend-curl-args gptel-backend)))
       (if (functionp curl-args) (funcall curl-args) curl-args))
     (list (format "-w(%s . %%{size_header})" token))
     (if (length< data-json gptel-curl-file-size-threshold)
         (list (format "-d%s" data-json))
       (letrec
           ((temp-filename (make-temp-file "gptel-curl-data" nil ".json" data-json))
            (cleanup-fn (lambda (&rest _)
                          (when (file-exists-p temp-filename)
                            (delete-file temp-filename)
                            (remove-hook 'gptel-post-response-functions cleanup-fn)))))
         (add-hook 'gptel-post-response-functions cleanup-fn)
         (list "--data-binary"
               (format "@%s" temp-filename))))
     (when (not (string-empty-p gptel-proxy))
       (list "--proxy" gptel-proxy
             "--proxy-negotiate"
             "--proxy-user" ":"))
     (cl-loop for (key . val) in headers
              collect (format "-H%s: %s" key val))
     (list url))))

;;;###autoload
(defun gptel-curl-get-response (fsm)
  "Fetch response to prompt in state FSM from the LLM using Curl.

FSM is the state machine driving this request.

FSM is the state machine driving this request.  Its INFO slot
contains the data required for setting up the request.  INFO is a
plist with the following keys, among others:
- :data     (the data being sent)
- :buffer   (the gptel buffer)
- :position (marker at which to insert the response).
- :callback (optional, the request callback)

Call CALLBACK with the response and INFO afterwards.  If omitted
the response is inserted into the current buffer after point."
  (let* ((token (md5 (format "%s%s%s%s"
                             (random) (emacs-pid) (user-full-name)
                             (recent-keys))))
         (info (gptel-fsm-info fsm))
         (backend (plist-get info :backend))
         (args (gptel-curl--get-args info token))
         (stream (plist-get info :stream))
         (process (apply #'start-process "gptel-curl"
                         (gptel--temp-buffer " *gptel-curl*") (gptel--curl-path) args)))
    (when (eq gptel-log-level 'debug)
      (gptel--log (mapconcat #'shell-quote-argument (cons (gptel--curl-path) args) " \\\n")
                  "request Curl command" 'no-json))
    (with-current-buffer (process-buffer process)
      (cond
       ((eq (gptel-backend-coding-system backend) 'binary)
        ;; set-buffer-file-coding-system is not needed since we don't save this buffer
        (set-buffer-multibyte nil)
        (set-process-coding-system process 'binary 'binary))
       (t
	;; Don't try to convert cr-lf to cr on Windows so that curl's "header size
	;; in bytes" stays correct. Explicitly set utf-8 for non-win systems too,
	;; for cases when buffer coding system is not set to utf-8.
	(set-process-coding-system process 'utf-8-unix 'utf-8-unix)))
      (set-process-query-on-exit-flag process nil)
      (if (plist-get info :token)       ;not the first run, set only the token
          (plist-put info :token token)
        (setf (gptel-fsm-info fsm)      ;fist run, set all process parameters
              (nconc (list :token token
                           :transformer
                           (when (with-current-buffer (plist-get info :buffer)
                                   (and (derived-mode-p 'org-mode)
                                        gptel-org-convert-response))
                             (gptel--stream-convert-markdown->org
                              (plist-get info :position))))
                     (unless (plist-get info :callback)
                       (list :callback (if stream
                                           #'gptel-curl--stream-insert-response
                                         #'gptel--insert-response)))
                     info)))
      (if stream
          (progn (set-process-sentinel process #'gptel-curl--stream-cleanup)
                 (set-process-filter process #'gptel-curl--stream-filter))
        (set-process-sentinel process #'gptel-curl--sentinel))
      (setf (alist-get process gptel--request-alist)
            (cons fsm
                  #'(lambda ()
                      ;; Clean up Curl process
                      (set-process-sentinel process #'ignore)
                      (delete-process process)
                      (kill-buffer (process-buffer process))))))))

;; ;; Ahead-Of-Time dispatch code for the parsers
;; :parser ; FIXME `cl--generic-*' are internal functions
;; (cl--generic-method-function
;;  (if stream
;;      (cl-loop
;;       for type in
;;       (cl--class-allparents (get (type-of backend) 'cl--class))
;;       with methods = (cl--generic-method-table
;;                       (cl--generic 'gptel-curl--parse-stream))
;;       when (cl--generic-member-method `(,type t) nil methods)
;;       return (car it))
;;    (cl-loop
;;     for type in
;;     (cl--class-allparents (get (type-of backend) 'cl--class))
;;     with methods = (cl--generic-method-table
;;                     (cl--generic 'gptel--parse-response))
;;     when (cl--generic-member-method `(,type t t) nil methods)
;;     return (car it))))

(defun gptel-curl--log-response (proc-buf proc-info)
  "Parse response buffer PROC-BUF and log response.

PROC-INFO is the plist containing process metadata."
  (with-current-buffer proc-buf
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "?\n?\n" nil t)
        (when (eq gptel-log-level 'debug)
          (gptel--log (gptel--json-encode
                       (buffer-substring-no-properties
                        (point-min) (1- (point))))
                      "response headers"))
        (let ((p (point)))
          (when (search-forward (plist-get proc-info :token) nil t)
            (goto-char (1- (match-beginning 0)))
            (gptel--log (buffer-substring-no-properties p (point))
                        "response body")))))))

;; TODO: Separate user-messaging from this function
(defun gptel-curl--stream-cleanup (process _status)
  "Process sentinel for gptel curl requests.

PROCESS and _STATUS are process parameters."
  (let ((proc-buf (process-buffer process)))
    (let* ((fsm (car (alist-get process gptel--request-alist)))
           (info (gptel-fsm-info fsm))
           (http-status (plist-get info :http-status)))
      (when gptel-log-level (gptel-curl--log-response proc-buf info)) ;logging
      (if (member http-status '("200" "100")) ;Finish handling response
          ;; Run the callback one last time to signal that the process has ended
          (with-demoted-errors "gptel callback error: %S"
            (funcall (plist-get info :callback) t info))
        (with-current-buffer proc-buf   ; Or Capture error message
          (goto-char (point-max))
          (search-backward (plist-get info :token))
          (backward-char)
          (pcase-let* ((`(,_ . ,header-size) (read (current-buffer)))
                       (response (progn (goto-char header-size)
                                        (condition-case nil (gptel--json-read)
                                          (error 'json-read-error))))
                       (error-data
                        (cond ((plistp response) (plist-get response :error))
                              ((arrayp response)
                               (cl-some (lambda (el) (plist-get el :error)) response)))))
            (cond
             (error-data
              (plist-put info :error error-data))
             ((eq response 'json-read-error)
              (plist-put info :error "Malformed JSON in response."))
             (t (plist-put info :error "Could not parse HTTP response.")))))
        (with-demoted-errors "gptel callback error: %S"
          (funcall (plist-get info :callback) nil info)))
      (gptel--fsm-transition fsm))      ; Move to next state
    (setf (alist-get process gptel--request-alist nil 'remove) nil)
    (kill-buffer proc-buf)))

(defun gptel-curl--stream-insert-response (response info &optional raw)
  "Insert streaming RESPONSE from an LLM into the gptel buffer.

INFO is a mutable plist containing information relevant to this buffer.
See `gptel--url-get-response' for details.

Optional RAW disables text properties and transformation."
  (pcase response
    ((pred stringp)
     (let ((start-marker (plist-get info :position))
           (tracking-marker (plist-get info :tracking-marker))
           (transformer (plist-get info :transformer)))
       (with-current-buffer (marker-buffer start-marker)
         (save-excursion
           (unless tracking-marker
             (goto-char start-marker)
             (unless (or (bobp) (plist-get info :in-place))
               (insert gptel-response-separator)
               (when gptel-mode
                 ;; Put prefix before AI response.
                 (insert (gptel-response-prefix-string)))
               (move-marker start-marker (point)))
             (setq tracking-marker (set-marker (make-marker) (point)))
             (set-marker-insertion-type tracking-marker t)
             (plist-put info :tracking-marker tracking-marker))
           (goto-char tracking-marker)
           (unless raw
             (when transformer
               (setq response (funcall transformer response)))
             (add-text-properties
              0 (length response) '(gptel response front-sticky (gptel))
              response))
           ;; (run-hooks 'gptel-pre-stream-hook)
           (insert response)
           (run-hooks 'gptel-post-stream-hook)))))
    (`(reasoning . ,text)
     (gptel--display-reasoning-stream text info))
    (`(tool-call . ,tool-calls)
     (gptel--display-tool-calls tool-calls info))
    (`(tool-result . ,tool-results)
     (gptel--display-tool-results tool-results info))))

(defun gptel-curl--stream-filter (process output)
  (let* ((fsm (car (alist-get process gptel--request-alist)))
         (proc-info (gptel-fsm-info fsm))
         (callback (or (plist-get proc-info :callback)
                       #'gptel-curl--stream-insert-response)))
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
            (plist-put proc-info :status (string-trim http-msg))
            (gptel--fsm-transition fsm))))
      
      (when-let* ((http-msg (plist-get proc-info :status))
                  (http-status (plist-get proc-info :http-status)))
        ;; Find data chunk(s) and run callback
        ;; FIXME Handle the case where HTTP 100 is followed by HTTP (not 200) BUG #194
        (when (member http-status '("200" "100"))
          (let ((response (gptel-curl--parse-stream
                           (plist-get proc-info :backend) proc-info))
                (reasoning-block (plist-get proc-info :reasoning-block)))
            ;; Depending on the API, there are two modes that reasoning or
            ;; chain-of-thought content appears: as part of the main response
            ;; but surrounded by <think>...</think> tags, or as a separate
            ;; JSON field in the response stream.
            ;;
            ;; These cases are handled using two PROC-INFO keys:
            ;;
            ;; :reasoning-block is nil before checking for reasoning, 'in when
            ;; in a reasoning block, t when we reach the end of the block, and
            ;; 'done afterwards or if no reasoning block is found.  This
            ;; applies to both the modes above.
            ;;
            ;; :reasoning contains the reasoning text parsed from the separate
            ;; JSON field.
            ;;
            ;; NOTE: We assume here that the reasoning block always
            ;; precedes the main response block.
            (unless (eq reasoning-block 'done)
              (let ((reasoning (plist-get proc-info :reasoning)))
                (cond
                 ((stringp reasoning)
                  ;; Obtained from separate JSON field in response
                  (funcall callback (cons 'reasoning reasoning) proc-info)
                  (unless reasoning-block ;Record that we're in a reasoning block (#709)
                    (plist-put proc-info :reasoning-block 'in))
                  (plist-put proc-info :reasoning nil)) ;Reset for next parsing round
                 ((and (null reasoning-block) (length> response 0))
                  (if (string-match-p "^ *<think>" response)
                      ;; Obtained from main response stream
                      (progn (setq response (cons 'reasoning response))
                             (plist-put proc-info :reasoning-block 'in))
                    (plist-put proc-info :reasoning-block 'done)))
                 ((and (not (eq reasoning-block t)) (length> response 0))
                  (if-let* ((idx (string-match-p "</think>" response)))
                      (progn
                        (funcall callback
                                 (cons 'reasoning ;last reasoning chunk
                                       (string-trim-left
                                        (substring response nil (+ idx 8))))
                                 proc-info)
                        (setq reasoning-block t) ;Signal end of reasoning stream
                        (plist-put proc-info :reasoning-block t)
                        (setq response (substring response (+ idx 8))))
                    (setq response (cons 'reasoning response)))))
                (when (eq reasoning-block t) ;End of reasoning block
                  (funcall callback '(reasoning . t) proc-info)
                  (plist-put proc-info :reasoning-block 'done))))
            (unless (equal response "") ;Response callback
              (funcall callback response proc-info))))))))

(cl-defgeneric gptel-curl--parse-stream (backend proc-info)
  "Stream parser for gptel-curl.

Implementations of this function run as part of the process
filter for the active query, and return partial responses from
the LLM.

BACKEND is the LLM backend in use.

PROC-INFO is a plist with process information and other context.
See `gptel-curl--get-response' for its contents.")

(defun gptel-curl--sentinel (process _status)
  "Process sentinel for gptel curl requests.

PROCESS and _STATUS are process parameters."
  (let ((proc-buf (process-buffer process)))
    (when-let* (((eq (process-status process) 'exit))
                (fsm (car (alist-get process gptel--request-alist)))
                (proc-info (gptel-fsm-info fsm))
                (proc-callback (plist-get proc-info :callback)))
      (when gptel-log-level (gptel-curl--log-response proc-buf proc-info)) ;logging
      (pcase-let ((`(,response ,http-status ,http-msg ,error)
                   (with-current-buffer proc-buf
                     (gptel-curl--parse-response proc-info))))
        (plist-put proc-info :http-status http-status)
        (plist-put proc-info :status http-msg)
        (gptel--fsm-transition fsm)     ;WAIT -> TYPE
        (when error (plist-put proc-info :error error))
        (when response                  ;Look for a reasoning block
          (if (string-match-p "^ *<think>\n" response)
              (when-let* ((idx (string-search "</think>\n" response)))
                (with-demoted-errors "gptel callback error: %S"
                  (funcall proc-callback
                           (cons 'reasoning (substring response nil (+ idx 8)))
                           proc-info))
                (setq response
                      (string-trim-left (substring response (+ idx 8)))))
            (when-let* ((reasoning (plist-get proc-info :reasoning))
                        ((stringp reasoning)))
              (funcall proc-callback (cons 'reasoning reasoning) proc-info))))
        (when (or response (not (member http-status '("200" "100"))))
          (with-demoted-errors "gptel callback error: %S"
            (funcall proc-callback response proc-info))))
      (gptel--fsm-transition fsm))      ;TYPE -> next
    (setf (alist-get process gptel--request-alist nil 'remove) nil)
    (kill-buffer proc-buf)))

(defun gptel-curl--parse-response (proc-info)
  "Parse the buffer BUF with curl's response.

PROC-INFO is a plist with contextual information."
  (let ((token (plist-get proc-info :token)))
    (goto-char (point-max))
    (search-backward token)
    (backward-char)
    (pcase-let* ((`(,_ . ,header-size) (read (current-buffer))))
      (goto-char (point-min))

      (if-let* ((http-msg (string-trim
                           (buffer-substring (line-beginning-position)
                                             (line-end-position))))
                (http-status
                 (save-match-data
                   (and (string-match "HTTP/[.0-9]+ +\\([0-9]+\\)" http-msg)
                        (match-string 1 http-msg))))
                (response (progn (goto-char header-size)
                                 (condition-case nil
                                     (gptel--json-read)
                                   (error 'json-read-error)))))
          (cond
           ;; FIXME Handle the case where HTTP 100 is followed by HTTP (not 200) BUG #194
           ((member http-status '("200" "100"))
            (list (and-let* ((resp (gptel--parse-response
                                    (plist-get proc-info :backend) response proc-info))
                             ((not (string-blank-p resp))))
                    (string-trim resp))
                  http-status http-msg))
           ((and-let* ((error-data
                        (cond ((plistp response) (plist-get response :error))
                              ((arrayp response)
                               (cl-some (lambda (el) (plist-get el :error)) response)))))
              (list nil http-status http-msg error-data)))
           ((eq response 'json-read-error)
            (list nil http-status (concat "(" http-msg ") Malformed JSON in response.")
                  "Malformed JSON in response"))
           (t (list nil http-status (concat "(" http-msg ") Could not parse HTTP response.")
                    "Could not parse HTTP response.")))
        (list nil http-status (concat "(" http-msg ") Could not parse HTTP response.")
              "Could not parse HTTP response.")))))

(provide 'gptel-curl)
;;; gptel-curl.el ends here
