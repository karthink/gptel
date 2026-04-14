;;; gptel-org-bounds-test.el --- Tests for gptel-org bounds restoration -*- lexical-binding: t; -*-

;; Tests for gptel-org--restore-bounds-from-tags which marks text properties
;; based on heading tags (:assistant:/:user:) or TODO keywords (AI/FEEDBACK).

(require 'ert)
(require 'gptel)
(require 'gptel-org)
(require 'gptel-openai)
(require 'gptel-test-backends)

;;; Helper macro

(defmacro gptel-org-bounds-test-with-buffer (content &rest body)
  "Create a temp org buffer with CONTENT and execute BODY.
Sets up org-mode with TODO keywords for keyword-based role detection.
A leading newline is prepended to CONTENT so that `outline-next-heading'
from `point-min' reaches the first heading, matching the traversal in
`gptel-org--restore-bounds-from-tags'."
  (declare (indent 1))
  `(let ((org-inhibit-startup t)
         (inhibit-message t)
         ;; Register TODO keywords so `org-get-todo-state' recognizes them.
         ;; AI is a done-state so headings like "* AI Answer" parse correctly.
         (org-todo-keywords
          '((sequence "FEEDBACK" "AI-DOING" "|" "AI-DONE" "AI"))))
     (with-temp-buffer
       (delay-mode-hooks (org-mode))
       ;; Activate the TODO keyword definitions in this buffer.
       (org-set-regexps-and-options)
       ;; Prepend newline so point-min is NOT on a heading.  This ensures
       ;; both `gptel-org--restore-bounds-from-tags' (which does
       ;; goto-char point-min then outline-next-heading in a loop) and
       ;; test navigation see every heading.
       (insert "\n" ,content)
       (goto-char (point-min))
       ,@body)))

;;; Tag-based detection tests

(ert-deftest gptel-test-bounds-tag-assistant-heading ()
  "Test that :assistant: tagged headings get gptel response property."
  (gptel-org-bounds-test-with-buffer
      "* Question :user:\n\nWhat is 2+2?\n\n* Answer :assistant:\n\nIt is 4.\n"
    (let ((gptel-org-infer-bounds-from-tags t)
          (gptel-org-use-todo-keywords nil))
      (gptel-org--restore-bounds-from-tags)
      ;; User heading should not have gptel property
      (goto-char (point-min))
      (outline-next-heading)
      (should (null (get-text-property (point) 'gptel)))
      ;; Assistant heading should have response property
      (outline-next-heading)
      (should (eq (get-text-property (point) 'gptel) 'response)))))

(ert-deftest gptel-test-bounds-tag-user-within-assistant ()
  "Test that :user: headings within :assistant: subtree lose gptel property."
  (gptel-org-bounds-test-with-buffer
      "* Response :assistant:\n\nAssistant text.\n\n** Follow-up :user:\n\nUser text.\n"
    (let ((gptel-org-infer-bounds-from-tags t)
          (gptel-org-use-todo-keywords nil))
      (gptel-org--restore-bounds-from-tags)
      ;; Assistant heading should have response
      (goto-char (point-min))
      (outline-next-heading)
      (should (eq (get-text-property (point) 'gptel) 'response))
      ;; User sub-heading should NOT have response
      (outline-next-heading)
      (should (null (get-text-property (point) 'gptel))))))

;;; Keyword-based detection tests

(ert-deftest gptel-test-bounds-keyword-ai-heading ()
  "Test that AI keyword headings get gptel response property."
  (gptel-org-bounds-test-with-buffer
      "* FEEDBACK Question\n\nWhat is 2+2?\n\n* AI Answer\n\nIt is 4.\n"
    (let ((gptel-org-use-todo-keywords t)
          (gptel-org-assistant-keyword "AI")
          (gptel-org-user-keyword "FEEDBACK")
          (gptel-org-infer-bounds-from-tags t))
      (gptel-org--restore-bounds-from-tags)
      ;; FEEDBACK heading: user
      (goto-char (point-min))
      (outline-next-heading)
      (should (null (get-text-property (point) 'gptel)))
      ;; AI heading: assistant
      (outline-next-heading)
      (should (eq (get-text-property (point) 'gptel) 'response)))))

(ert-deftest gptel-test-bounds-keyword-ai-done-nested ()
  "Test that AI-DONE headings nested under FEEDBACK get response property.
This is the core bug: the user heading handler was skipping its entire
subtree without marking assistant sub-headings."
  (gptel-org-bounds-test-with-buffer
      (concat "** FEEDBACK Calculate 2 + 2\n"
              "*** AI-DONE Calculate 2 + 2\n\n"
              "#+begin_src gptel-reasoning\n"
              "The answer is 4.\n"
              "#+end_src\n\n"
              "**** Result\n\n"
              "2 + 2 = 4\n\n"
              "*** FEEDBACK\n")
    (let ((gptel-org-use-todo-keywords t)
          (gptel-org-assistant-keyword "AI")
          (gptel-org-user-keyword "FEEDBACK")
          (gptel-org-infer-bounds-from-tags t)
          (gptel-org-tasks-done-keyword "AI-DONE"))
      (gptel-org--restore-bounds-from-tags)
      ;; ** FEEDBACK: user heading, no gptel property
      (goto-char (point-min))
      (outline-next-heading)
      (should (string-equal (org-get-todo-state) "FEEDBACK"))
      (should (null (get-text-property (point) 'gptel)))
      ;; *** AI-DONE: assistant heading, should have response property
      (outline-next-heading)
      (should (string-equal (org-get-todo-state) "AI-DONE"))
      (should (eq (get-text-property (point) 'gptel) 'response))
      ;; **** Result: within AI-DONE subtree, should also have response
      (outline-next-heading)
      (should (eq (get-text-property (point) 'gptel) 'response))
      ;; *** FEEDBACK (at end): user heading, no gptel property
      (re-search-forward "^\\*\\*\\* FEEDBACK$")
      (beginning-of-line)
      (should (null (get-text-property (point) 'gptel))))))

(ert-deftest gptel-test-bounds-keyword-ai-doing-nested ()
  "Test that AI-DOING headings nested under FEEDBACK get response property."
  (gptel-org-bounds-test-with-buffer
      (concat "** FEEDBACK Task\n"
              "*** AI-DOING Task\n\n"
              "In-progress response.\n\n"
              "*** FEEDBACK\n")
    (let ((gptel-org-use-todo-keywords t)
          (gptel-org-assistant-keyword "AI")
          (gptel-org-user-keyword "FEEDBACK")
          (gptel-org-infer-bounds-from-tags t)
          (gptel-org-tasks-doing-keyword "AI-DOING"))
      (gptel-org--restore-bounds-from-tags)
      ;; ** FEEDBACK: user
      (goto-char (point-min))
      (outline-next-heading)
      (should (null (get-text-property (point) 'gptel)))
      ;; *** AI-DOING: assistant
      (outline-next-heading)
      (should (eq (get-text-property (point) 'gptel) 'response)))))

(ert-deftest gptel-test-bounds-keyword-multiple-cycles ()
  "Test multiple FEEDBACK/AI-DONE cycles parse correctly."
  (gptel-org-bounds-test-with-buffer
      (concat "** FEEDBACK First question\n"
              "*** AI-DONE First answer\n\n"
              "Answer to first.\n\n"
              "*** FEEDBACK Second question\n"
              "**** AI-DONE Second answer\n\n"
              "Answer to second.\n\n"
              "**** FEEDBACK\n")
    (let ((gptel-org-use-todo-keywords t)
          (gptel-org-assistant-keyword "AI")
          (gptel-org-user-keyword "FEEDBACK")
          (gptel-org-infer-bounds-from-tags t)
          (gptel-org-tasks-done-keyword "AI-DONE"))
      (gptel-org--restore-bounds-from-tags)
      ;; ** FEEDBACK: user
      (goto-char (point-min))
      (outline-next-heading)
      (should (string-equal (org-get-todo-state) "FEEDBACK"))
      (should (null (get-text-property (point) 'gptel)))
      ;; *** AI-DONE First: assistant
      (outline-next-heading)
      (should (string-equal (org-get-todo-state) "AI-DONE"))
      (should (eq (get-text-property (point) 'gptel) 'response))
      ;; *** FEEDBACK Second: user
      (outline-next-heading)
      (should (string-equal (org-get-todo-state) "FEEDBACK"))
      (should (null (get-text-property (point) 'gptel)))
      ;; **** AI-DONE Second: assistant
      (outline-next-heading)
      (should (string-equal (org-get-todo-state) "AI-DONE"))
      (should (eq (get-text-property (point) 'gptel) 'response)))))

(ert-deftest gptel-test-bounds-keyword-without-infer-bounds ()
  "Test that keyword mode works even when gptel-org-infer-bounds-from-tags is nil."
  (gptel-org-bounds-test-with-buffer
      "* FEEDBACK Question\n\n* AI Answer\n\nResponse.\n"
    (let ((gptel-org-use-todo-keywords t)
          (gptel-org-assistant-keyword "AI")
          (gptel-org-user-keyword "FEEDBACK")
          (gptel-org-infer-bounds-from-tags nil))
      ;; Should still work via gptel-org-use-todo-keywords
      (gptel-org--restore-bounds-from-tags)
      (goto-char (point-min))
      (outline-next-heading) ; FEEDBACK
      (should (null (get-text-property (point) 'gptel)))
      (outline-next-heading) ; AI
      (should (eq (get-text-property (point) 'gptel) 'response)))))

;;; Edge cases

(ert-deftest gptel-test-bounds-no-role-headings ()
  "Test that headings without role indicators clear stale properties."
  (gptel-org-bounds-test-with-buffer
      "* Plain heading\n\nSome text.\n"
    (let ((gptel-org-infer-bounds-from-tags t)
          (gptel-org-use-todo-keywords nil))
      ;; Add stale property
      (goto-char (point-min))
      (outline-next-heading)
      (add-text-properties (point) (point-max) '(gptel response))
      ;; Restore should clear it
      (gptel-org--restore-bounds-from-tags)
      (goto-char (point-min))
      (outline-next-heading)
      (should (null (get-text-property (point) 'gptel))))))

(ert-deftest gptel-test-bounds-assistant-within-user-tag ()
  "Test that :assistant: sub-headings within :user: get response property."
  (gptel-org-bounds-test-with-buffer
      (concat "* Topic :user:\n\n"
              "User question.\n\n"
              "** Response :assistant:\n\n"
              "Assistant answer.\n\n"
              "** Follow-up :user:\n\n"
              "Another question.\n")
    (let ((gptel-org-infer-bounds-from-tags t)
          (gptel-org-use-todo-keywords nil))
      (gptel-org--restore-bounds-from-tags)
      ;; * Topic :user: — no gptel
      (goto-char (point-min))
      (outline-next-heading)
      (should (null (get-text-property (point) 'gptel)))
      ;; ** Response :assistant: — response
      (outline-next-heading)
      (should (eq (get-text-property (point) 'gptel) 'response))
      ;; ** Follow-up :user: — no gptel
      (outline-next-heading)
      (should (null (get-text-property (point) 'gptel))))))

;;; Integration: full parse-buffer tests

(ert-deftest gptel-test-bounds-parse-feedback-ai-done-feedback ()
  "Test full parsing of FEEDBACK parent with AI-DONE and FEEDBACK children.
This is the exact scenario from the bug report:
  ** FEEDBACK Calculate 2 + 2
  *** AI-DONE Calculate 2 + 2
      ...assistant content...
  *** FEEDBACK Add 11 to the result
Should produce: user, assistant, user messages."
  (gptel-org-bounds-test-with-buffer
      (concat "** FEEDBACK Calculate 2 + 2\n"
              "*** AI-DONE Calculate 2 + 2\n\n"
              "The answer is 4.\n\n"
              "*** FEEDBACK Add 11 to the result\n")
    (let ((gptel-org-use-todo-keywords t)
          (gptel-org-assistant-keyword "AI")
          (gptel-org-user-keyword "FEEDBACK")
          (gptel-org-infer-bounds-from-tags t)
          (gptel-org-tasks-done-keyword "AI-DONE")
          (gptel-org-tasks-doing-keyword "AI-DOING"))
      (gptel-org--restore-bounds-from-tags)
      ;; Verify text properties at each heading
      (goto-char (point-min))
      ;; ** FEEDBACK Calculate 2 + 2 — user, nil
      (outline-next-heading)
      (should (string-equal (org-get-todo-state) "FEEDBACK"))
      (should (null (get-text-property (point) 'gptel)))
      ;; *** AI-DONE Calculate 2 + 2 — assistant, response
      (outline-next-heading)
      (should (string-equal (org-get-todo-state) "AI-DONE"))
      (should (eq (get-text-property (point) 'gptel) 'response))
      ;; *** FEEDBACK Add 11 — user, nil
      (outline-next-heading)
      (should (string-equal (org-get-todo-state) "FEEDBACK"))
      (should (null (get-text-property (point) 'gptel))))))

(ert-deftest gptel-test-bounds-parse-buffer-feedback-cycle ()
  "Integration test: gptel--parse-buffer produces correct user/assistant messages.
Mirrors the real conversation from siro-ai.org:
  ** FEEDBACK Calculate 2 + A where you pick A value from 0-9
  *** AI-DONE Calculate 2 + A where you pick A value from 0-9
  **** Calculation result
  I'll pick A = 5.  Result: 2 + 5 = 7
  *** FEEDBACK Add 11 to the result
Verifies that gptel--parse-buffer (which reads gptel text properties)
produces three messages: user, assistant, user."
  (gptel-org-bounds-test-with-buffer
      (concat "** FEEDBACK Calculate 2 + A where you pick A value from 0-9\n"
              "*** AI-DONE Calculate 2 + A where you pick A value from 0-9\n\n"
              "**** Calculation result\n\n"
              "I'll pick A = 5 from the range 0-9.\n\n"
              "Result: *2 + 5 = 7*\n\n"
              "*** FEEDBACK Add 11 to the result\n")
    (let ((gptel-org-use-todo-keywords t)
          (gptel-org-assistant-keyword "AI")
          (gptel-org-user-keyword "FEEDBACK")
          (gptel-org-infer-bounds-from-tags t)
          (gptel-org-tasks-done-keyword "AI-DONE")
          (gptel-org-tasks-doing-keyword "AI-DOING")
          (gptel-track-response t)
          (gptel-backend (alist-get 'openai gptel-test-backends))
          (gptel-model 'gpt-4o-mini))
      ;; Mark bounds from keywords
      (gptel-org--restore-bounds-from-tags)
      ;; Parse from the end of buffer (where user would be typing)
      (goto-char (point-max))
      (let ((messages (gptel--parse-buffer gptel-backend)))
        ;; Should produce 3 messages: user, assistant, user
        (should (= (length messages) 3))
        ;; First message: user asking "Calculate 2 + A..."
        (should (string-equal (plist-get (nth 0 messages) :role) "user"))
        (should (string-match-p "Calculate 2 \\+ A"
                                (plist-get (nth 0 messages) :content)))
        ;; Second message: assistant with the calculation
        (should (string-equal (plist-get (nth 1 messages) :role) "assistant"))
        (should (string-match-p "pick A = 5"
                                (plist-get (nth 1 messages) :content)))
        (should (string-match-p "2 \\+ 5 = 7"
                                (plist-get (nth 1 messages) :content)))
        ;; Third message: user follow-up "Add 11 to the result"
        (should (string-equal (plist-get (nth 2 messages) :role) "user"))
        (should (string-match-p "Add 11 to the result"
                                (plist-get (nth 2 messages) :content)))))))

(ert-deftest gptel-test-bounds-parse-ai-doing-parent ()
  "Test parsing when top-level is AI-DOING (works in original code).
  * AI-DOING Task
  *** AI-DONE Task
  *** FEEDBACK Next"
  (gptel-org-bounds-test-with-buffer
      (concat "* AI-DOING Task\n"
              "*** AI-DONE Task\n\n"
              "The answer is 4.\n\n"
              "*** FEEDBACK Next\n")
    (let ((gptel-org-use-todo-keywords t)
          (gptel-org-assistant-keyword "AI")
          (gptel-org-user-keyword "FEEDBACK")
          (gptel-org-infer-bounds-from-tags t)
          (gptel-org-tasks-done-keyword "AI-DONE")
          (gptel-org-tasks-doing-keyword "AI-DOING"))
      (gptel-org--restore-bounds-from-tags)
      ;; * AI-DOING — assistant, response
      (goto-char (point-min))
      (outline-next-heading)
      (should (string-equal (org-get-todo-state) "AI-DOING"))
      (should (eq (get-text-property (point) 'gptel) 'response))
      ;; *** AI-DONE — still within assistant subtree, response
      (outline-next-heading)
      (should (eq (get-text-property (point) 'gptel) 'response))
      ;; *** FEEDBACK — user within assistant, should be nil
      (outline-next-heading)
      (should (string-equal (org-get-todo-state) "FEEDBACK"))
      (should (null (get-text-property (point) 'gptel))))))

(provide 'gptel-org-bounds-test)
;;; gptel-org-bounds-test.el ends here
