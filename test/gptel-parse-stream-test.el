;; -*- lexical-binding: t; -*-
(require 'ert)
(require 'gptel)
(require 'gptel-request)
(require 'gptel-test-backends)

;;; Reasoning stream tests
(defun gptel-test-stream-filter (backend-name stream-output-file &optional increment)
  "Parse a stream output for BACKEND-NAME.

The keys of `gptel-test-backends' are possible values of BACKEND-NAME.

STREAM-OUTPUT-FILE is the output (txt) file containing the streaming
response contents.

INCREMENT is the size of chunks to feed the process filter."
  (declare (indent 1))
  (let* ((fsm (gptel-make-fsm))
         (backend (alist-get backend-name gptel-test-backends))
         (increment (or increment 800))
         info reasoning-list response-list
         (proc (make-process :name "gptel-test" :command nil))
         (callback (lambda (resp info)
                     (pcase resp
                       (`(reasoning . ,text) (push text reasoning-list))
                       ((pred stringp)       (push resp response-list)))))
         (outputs
          (with-temp-buffer
            (insert-file-contents stream-output-file)
            (seq-partition (buffer-string) increment))))
    (setq info (list :backend backend
                     :data (list :contents [(:role "user" :parts [(:text "Test")])])
                     :include-reasoning t
                     :http-status "200"
                     :status "HTTP/2 200"
                     :callback callback))
    (setf (gptel-fsm-info fsm) info
          (alist-get proc gptel--request-alist) (list fsm))
    (unwind-protect
        (with-temp-buffer
          (setf (process-buffer proc) (current-buffer))
          (while outputs (gptel-curl--stream-filter proc (pop outputs))))
      (setf (alist-get proc gptel--request-alist nil t) nil)
      (delete-process proc))
    (list (car-safe reasoning-list)     ;Did the reasoning stream end with `t'?
          (string-join (nreverse (cdr-safe reasoning-list))) ;Correct reasoning content?
          (string-join (nreverse response-list)))))

(defmacro gptel-test-stream-parsing
    (name backend-name stream-output-file result-file &optional increment)
  "Create test for stream parsing that covers reasoning and response.

Tool use parsing is not covered in this test.

NAME is the name of the test, any string.
BACKEND-NAME is a key from `gptel-test-backends'.
STREAM-OUTPUT-FILE is the output (txt) file containing the streaming.
INCREMENT is the size of chunks to feed the process filter, defaults to 800.

RESULT-FILE is the corresponding (eld) file containing the parsed
output.  It should contain a list of three items:
 (<t or nil>           ; depending on whether a reasoning stream was found
  \"reasoning text\"
  \"response text\")"
  (declare (indent 2))
  `(ert-deftest ,(intern (concat "gptel-test-stream-parsing-" name)) ()
     (should
      (equal
       (with-temp-buffer
         (save-excursion (insert-file-contents ,result-file))
         (read (current-buffer)))
       (gptel-test-stream-filter
           ,backend-name ,stream-output-file ,increment)))))

;;;; OpenAI-compatible

;;;;; llama.cpp
(gptel-test-stream-parsing "llamacpp-01a" 'openai
  "examples-responses/openai/thinking_stream_llamacpp_glm4.5_01.txt"
  "examples-responses/openai/thinking_stream_llamacpp_glm4.5_01.eld"
  800)

(gptel-test-stream-parsing "llamacpp-01b" 'openai
  "examples-responses/openai/thinking_stream_llamacpp_glm4.5_01.txt"
  "examples-responses/openai/thinking_stream_llamacpp_glm4.5_01.eld"
  400)

(gptel-test-stream-parsing "llamacpp-01c" 'openai
  "examples-responses/openai/thinking_stream_llamacpp_glm4.5_01.txt"
  "examples-responses/openai/thinking_stream_llamacpp_glm4.5_01.eld"
  40)

;;;;; vLLM
(gptel-test-stream-parsing "vllm-01a" 'openai
  "examples-responses/openai/thinking_stream_vllm_glm4.5_01.txt"
  "examples-responses/openai/thinking_stream_vllm_glm4.5_01.eld"
  800)

(gptel-test-stream-parsing "vllm-01b" 'openai
  "examples-responses/openai/thinking_stream_vllm_glm4.5_01.txt"
  "examples-responses/openai/thinking_stream_vllm_glm4.5_01.eld"
  400)

(gptel-test-stream-parsing "vllm-01c" 'openai
  "examples-responses/openai/thinking_stream_vllm_glm4.5_01.txt"
  "examples-responses/openai/thinking_stream_vllm_glm4.5_01.eld"
  40)


;;;;; Openrouter
(gptel-test-stream-parsing "openrouter-01a" 'openai
  "examples-responses/openai/thinking_stream_openrouter_01.txt"
  "examples-responses/openai/thinking_stream_openrouter_01.eld"
  800)

(gptel-test-stream-parsing "openrouter-01b" 'openai
  "examples-responses/openai/thinking_stream_openrouter_01.txt"
  "examples-responses/openai/thinking_stream_openrouter_01.eld"
  400)

(gptel-test-stream-parsing "openrouter-02a" 'openai
  "examples-responses/openai/thinking_stream_openrouter_02.txt"
  "examples-responses/openai/thinking_stream_openrouter_02.eld"
  800)

(gptel-test-stream-parsing "openrouter-02b" 'openai
  "examples-responses/openai/thinking_stream_openrouter_02.txt"
  "examples-responses/openai/thinking_stream_openrouter_02.eld"
  400)

;;;;; Deepseek
(gptel-test-stream-parsing "deepseek-01a" 'deepseek
  "examples-responses/openai/thinking_stream_deepseek_01.txt"
  "examples-responses/openai/thinking_stream_deepseek_01.eld"
  800)

;; ;; TODO: This test file is too large, generate a smaller one
;; (gptel-test-stream-parsing "deepseek-01b" 'deepseek
;;   "examples-responses/openai/thinking_stream_deepseek_02.txt"
;;   "examples-responses/openai/thinking_stream_deepseek_02.eld"
;;   400)

;; ;; MAYBE The test file for this next text is very large, ignoring for now

;; (gptel-test-stream-parsing "deepseek-02a" 'deepseek
;;   "examples-responses/openai/thinking_stream_deepseek_01.txt"
;;   "examples-responses/openai/thinking_stream_deepseek_01.eld"
;;   800)

;; (gptel-test-stream-parsing "deepseek-02b" 'deepseek
;;   "examples-responses/openai/thinking_stream_deepseek_02.txt"
;;   "examples-responses/openai/thinking_stream_deepseek_02.eld"
;;   400)

;;;; Anthropic
(gptel-test-stream-parsing "anthropic-01a" 'anthropic
  "examples-responses/anthropic/thinking_stream_01.txt"
  "examples-responses/anthropic/thinking_stream_01.eld"
  800)

(gptel-test-stream-parsing "anthropic-01b" 'anthropic
  "examples-responses/anthropic/thinking_stream_01.txt"
  "examples-responses/anthropic/thinking_stream_01.eld"
  400)

(gptel-test-stream-parsing "anthropic-02a" 'anthropic
  "examples-responses/anthropic/thinking_stream_02.txt"
  "examples-responses/anthropic/thinking_stream_02.eld"
  800)

(gptel-test-stream-parsing "anthropic-02b" 'anthropic
  "examples-responses/anthropic/thinking_stream_02.txt"
  "examples-responses/anthropic/thinking_stream_02.eld"
  400)

(gptel-test-stream-parsing "anthropic-03a" 'anthropic
  "examples-responses/anthropic/thinking_stream_03.txt"
  "examples-responses/anthropic/thinking_stream_03.eld"
  800)

(gptel-test-stream-parsing "anthropic-03b" 'anthropic
  "examples-responses/anthropic/thinking_stream_03.txt"
  "examples-responses/anthropic/thinking_stream_03.eld"
  400)

(gptel-test-stream-parsing "anthropic-04a" 'anthropic
  "examples-responses/anthropic/thinking_stream_04.txt"
  "examples-responses/anthropic/thinking_stream_04.eld"
  800)

(gptel-test-stream-parsing "anthropic-04b" 'anthropic
  "examples-responses/anthropic/thinking_stream_04.txt"
  "examples-responses/anthropic/thinking_stream_04.eld"
  400)

;;;; Gemini

(gptel-test-stream-parsing "gemini-01a" 'gemini
  "examples-responses/gemini/thinking_stream_01.txt"
  "examples-responses/gemini/thinking_stream_01.eld"
  800)

(gptel-test-stream-parsing "gemini-01b" 'gemini
  "examples-responses/gemini/thinking_stream_01.txt"
  "examples-responses/gemini/thinking_stream_01.eld"
  400)

(gptel-test-stream-parsing "gemini-02a" 'gemini
  "examples-responses/gemini/thinking_stream_02.txt"
  "examples-responses/gemini/thinking_stream_02.eld"
  800)

(gptel-test-stream-parsing "gemini-02b" 'gemini
  "examples-responses/gemini/thinking_stream_02.txt"
  "examples-responses/gemini/thinking_stream_02.eld"
  400)

(gptel-test-stream-parsing "gemini-03a" 'gemini
  "examples-responses/gemini/thinking_stream_03.txt"
  "examples-responses/gemini/thinking_stream_03.eld"
  800)

(gptel-test-stream-parsing "gemini-03b" 'gemini
  "examples-responses/gemini/thinking_stream_03.txt"
  "examples-responses/gemini/thinking_stream_03.eld"
  400)
