;;; test-user-config.el --- Description -*- lexical-binding: t; -*-

;;; Example functions
(cl-defun my-cowsay (&key term)
  (let* ((bubble-top (concat " " (make-string (+ (length term) 2) ?_) "\n"))
         (bubble-middle (format "< %s >\n" term))
         (bubble-bottom (concat " " (make-string (+ (length term) 2) ?-) "\n"))
         (cow "        \\   ^__^\n         \\  (oo)\\_______\n            (__)\\       )\\/\\\n                ||----w |\n                ||     ||"))
    (format "%s%s%s%s" bubble-top bubble-middle bubble-bottom cow)))

(cl-defun my-create-file (&key filename contents)
  "Create a file with FILENAME and CONTENTS, and open it in a new buffer."
  (interactive "sEnter filename: \nMEnter contents: ")
  (if (not (file-exists-p filename))
      (with-temp-buffer
        (insert contents)
        (write-file filename)))
  (split-window)
  (other-window 1)
  (find-file filename))

;;; Callable function schema
(setq! gptel-callable-functions
       ;; Hard coded variable specifying callable functions. This could be defined in a user's configuration
       (vector
        (list
         :type "function"
         :function (list
                    :name "my-cowsay"
                    :description "Have a cow say something"
                    :parameters (list
                                 :type "object"
                                 :properties (list
                                              :term (list
                                                     :type "string"
                                                     :description "term to say"))
                                 :required ["term"])))
        (list
         :type "function"
         :function (list
                    :name "my-create-file"
                    :description "Create a new file"
                    :parameters (list
                                 :type "object"
                                 :properties (list
                                              :filename (list
                                                         :type "string"
                                                         :description "local path to file including file extension")
                                              :contents (list
                                                         :type "string"
                                                         :description "file contents")))
                    :required ["filename" "contents"]))))

;;; Backend to use for testing. Stream must be turned off (for now)
(setq gptel-backend (gptel-make-openai
                        "OpenAI with function calls"
                      :key #'gptel-api-key
                      :models '("gpt-3.5-turbo" "gpt-3.5-turbo-16k" "gpt-4" "gpt-4-turbo-preview" "gpt-4-32k" "gpt-4-1106-preview")
                      :stream nil))

;;; Hook to call function after gptel query
(defun cons-list-to-plist (cons-list)
  (let ((plist '()))
    (dolist (item cons-list)
      (setq plist (plist-put plist (if (keywordp (car item))
                                       (car item)
                                     (intern (concat ":" (symbol-name (car item)))))
                             (cdr item))))
    plist))

(cl-defun gptel-run-function-on-region (beg end)
  (when (and beg end)
    (save-excursion
      (let* ((contents (buffer-substring-no-properties beg end))
             (parsed (ignore-errors (read contents)))
             (is-vector (vectorp parsed)))
        (if is-vector
            ;; Function call data is in a vector. If the contents are just a string, do nothing
            (let* ((plist (aref parsed 0))
                   (is-function-call (string= (plist-get plist :type) "function")))
              (if is-function-call
                  (let* ((function-data (plist-get plist :function))
                         (function-name (plist-get function-data :name))
                         (arguments-json (plist-get function-data :arguments))
                         (arguments (json-read-from-string arguments-json)))
                    (when (yes-or-no-p (format "Call function `%s` with arguments %s?" function-name arguments-json))
                      (when (fboundp (intern function-name))
                        ;; Call the function and insert return value
                        (goto-char end)
                        (insert "\n")
                        (insert (apply (intern function-name) (cons-list-to-plist arguments)))))))))))))

(add-hook 'gptel-post-response-functions #'gptel-run-function-on-region)
