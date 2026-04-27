;;; gptel-deepseek-reasoning-test.el --- DeepSeek reasoning_content round-trip tests -*- lexical-binding: t; -*-

;; DeepSeek's thinking-mode API rule: when an assistant turn performs a
;; tool call, the assistant's `reasoning_content' must travel with that
;; assistant message in every later request, otherwise DeepSeek returns
;;   400 invalid_request_error
;;   "The `reasoning_content' in the thinking mode must be passed back to the API."
;;
;; gptel persists that reasoning into the buffer-side tool-call sexp
;; (see `gptel--display-tool-results' in gptel.el) so it survives across
;; conversation turns.  These tests exercise the parse half of the
;; round-trip:
;;
;; - `gptel--parse-buffer' (gptel-openai.el) extracts :reasoning_content
;;   from the (tool . ID) sexp and re-emits it on the rebuilt assistant
;;   message.
;; - The deepseek `:around' merger (gptel-openai-extras.el) does NOT
;;   merge consecutive same-role messages that carry :tool_calls or
;;   :reasoning_content -- those must remain distinct.

(require 'ert)
(require 'gptel)
(require 'gptel-openai)
(require 'gptel-openai-extras)
(require 'gptel-test-backends)

(defun gptel-deepseek-reasoning-test--insert-tool-block (id name args
                                                           reasoning result)
  "Insert a buffer chunk simulating a tool call/result block.

ID is the tool-call id (string), NAME the tool name (string), ARGS the
args plist, REASONING the reasoning_content string (or nil), and RESULT
the tool-result string.

Uses the same shape `gptel--display-tool-results' writes:
  - a 1-line heading with text-property gptel='ignore
  - body: the printed sexp + newline + result, with text-property
    gptel=(tool . ID)."
  (let* ((sexp (if reasoning
                   `(:name ,name :args ,args :reasoning_content ,reasoning)
                 `(:name ,name :args ,args)))
         (heading (propertize (format "* TOOL %s\n" name)
                              'gptel 'ignore 'front-sticky '(gptel)))
         (body (propertize (concat (prin1-to-string sexp) "\n" result "\n")
                           'gptel `(tool . ,id))))
    (insert heading body)))

(ert-deftest gptel-deepseek-reasoning-parse-buffer-roundtrip ()
  "`gptel--parse-buffer' restores :reasoning_content from the tool sexp.

The reconstructed assistant message must carry both :tool_calls and
:reasoning_content so the subsequent DeepSeek request includes the
reasoning bound to that tool-call turn."
  (skip-unless (fboundp 'gptel--make-deepseek))
  (let* ((backend (alist-get 'deepseek gptel-test-backends))
         (gptel-backend backend)
         (gptel-track-response t)
         (gptel-mode t))
    (with-temp-buffer
      (insert "Initial user prompt asking for a calculation.\n")
      (gptel-deepseek-reasoning-test--insert-tool-block
       "call_abc" "calc" '(:expr "2+2")
       "I should call the calc tool with the expression 2+2."
       "4")
      (insert (propertize "Follow-up user message.\n"
                          ;; gptel default = no property
                          ))
      (goto-char (point-max))
      (let* ((messages (gptel--parse-buffer backend nil))
             (asst (cl-find-if
                    (lambda (m)
                      (and (equal (plist-get m :role) "assistant")
                           (plist-get m :tool_calls)))
                    messages)))
        (should asst)
        (should (plist-get asst :tool_calls))
        (should (equal (plist-get asst :reasoning_content)
                       "I should call the calc tool with the expression 2+2."))))))

(ert-deftest gptel-deepseek-reasoning-parse-buffer-no-reasoning ()
  "`gptel--parse-buffer' tolerates tool sexps without :reasoning_content.

Old buffers and non-DeepSeek backends produce sexps of the form
(:name ... :args ...) with no reasoning field; the reconstructed
assistant message must omit :reasoning_content rather than emit nil."
  (skip-unless (fboundp 'gptel--make-deepseek))
  (let* ((backend (alist-get 'deepseek gptel-test-backends))
         (gptel-backend backend)
         (gptel-track-response t)
         (gptel-mode t))
    (with-temp-buffer
      (insert "User asks something.\n")
      (gptel-deepseek-reasoning-test--insert-tool-block
       "call_xyz" "calc" '(:expr "1+1")
       nil
       "2")
      (insert "Follow-up.\n")
      (goto-char (point-max))
      (let* ((messages (gptel--parse-buffer backend nil))
             (asst (cl-find-if
                    (lambda (m)
                      (and (equal (plist-get m :role) "assistant")
                           (plist-get m :tool_calls)))
                    messages)))
        (should asst)
        (should-not (plist-member asst :reasoning_content))
        (should-not (plist-member asst :reasoning))))))

(ert-deftest gptel-deepseek-reasoning-merger-preserves-tool-calls ()
  "DeepSeek's same-role merger must NOT merge messages carrying tool_calls.

If two consecutive assistant messages both have :tool_calls or
:reasoning_content, the second's structural fields would be silently
dropped by a naive content-only merge.  DeepSeek's API rejects that:
reasoning_content has to travel with the tool-call assistant message
that produced it.

Two adjacent (tool . ID) blocks (e.g. one assistant turn with parallel
tool calls) parse into two assistant messages and must remain distinct."
  (skip-unless (fboundp 'gptel--make-deepseek))
  (let* ((backend (alist-get 'deepseek gptel-test-backends))
         (gptel-backend backend)
         (gptel-track-response t)
         (gptel-mode t))
    (with-temp-buffer
      (insert "Please run two tools.\n")
      (gptel-deepseek-reasoning-test--insert-tool-block
       "call_1" "calc" '(:expr "2+2")
       "First reasoning."
       "4")
      (gptel-deepseek-reasoning-test--insert-tool-block
       "call_2" "calc" '(:expr "3+3")
       "Second reasoning."
       "6")
      (insert "Now what's next?\n")
      (goto-char (point-max))
      (let* ((messages (gptel--parse-buffer backend nil))
             (assts (cl-remove-if-not
                     (lambda (m)
                       (and (equal (plist-get m :role) "assistant")
                            (plist-get m :tool_calls)))
                     messages)))
        (should (= (length assts) 2))
        ;; Both must carry their own reasoning_content; the merger must
        ;; not have collapsed them and dropped fields from the second.
        (should (equal (plist-get (nth 0 assts) :reasoning_content)
                       "First reasoning."))
        (should (equal (plist-get (nth 1 assts) :reasoning_content)
                       "Second reasoning."))))))

(provide 'gptel-deepseek-reasoning-test)
;;; gptel-deepseek-reasoning-test.el ends here
