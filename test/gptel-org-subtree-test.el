;;; gptel-org-subtree-test.el --- Tests for gptel-org subtree context feature -*- lexical-binding: t; -*-

;; Tests for the gptel-org-subtree-context feature which enables
;; task-oriented conversation workflows under TODO headings via
;; agent indirect buffers.
;;
;; NOTE: Classical subtree mode (sibling scanning via
;; gptel-org--get-parent-heading-level and gptel-org--get-chat-siblings)
;; has been removed.  gptel-org-subtree-context now controls the
;; indirect buffer approach (previously gptel-org-agent-subtrees).
;;
;; Features tested:
;; - gptel-org--chat-heading-p: Check if heading is a chat entry (at-point)
;; - gptel-org--adjust-response-headings: Demote response headings
;; - gptel-org--escape-example-blocks: Escape blocks in responses
;; - Tag-based features (gptel-org-infer-bounds-from-tags)
;; - Hybrid branching context with agent subtrees
;; - Auto-correct stream in agent indirect buffers

(require 'ert)
(require 'gptel)
(require 'gptel-org)
(require 'gptel-org-agent)

;;; Helper macro for org buffer tests

(defmacro gptel-org-test-with-temp-buffer (content &rest body)
  "Create a temp org buffer with CONTENT and execute BODY.
Point is placed at the first @ character if present, otherwise at end.
Does NOT bind gptel-org-subtree-context - caller must do that."
  (declare (indent 1))
  `(let ((org-inhibit-startup t))
     (with-temp-buffer
       (delay-mode-hooks (org-mode))
       (insert ,content)
       (goto-char (point-min))
       (unless (search-forward "@" nil t)
         (goto-char (point-max)))
       (when (looking-back "@" 1)
         (backward-char))
       ,@body)))

(defmacro gptel-org-test-with-buffer (content &rest body)
  "Create a temp org buffer with CONTENT, enable subtree context, and execute BODY.
Point is placed at the first @ character if present, otherwise at end.
Disables `gptel-org-infer-bounds-from-tags' to test marker-based behavior."
  (declare (indent 1))
  `(let ((gptel-org-subtree-context t)
         (gptel-org-infer-bounds-from-tags nil))
     (gptel-org-test-with-temp-buffer ,content ,@body)))

;;; Tests for gptel-org--chat-heading-p

(ert-deftest gptel-org-subtree-test-chat-heading-p-at-heading ()
  "Test gptel-org--chat-heading-p when point is at a heading with :user: tag."
  (gptel-org-test-with-tags-buffer
   "* Task\n** Question :user:\nSome question"
   (goto-char (point-min))
   (search-forward "** Question")
   (beginning-of-line)
   (should (gptel-org--chat-heading-p))))

;;; Tests for gptel-org--get-parent-heading-level — REMOVED
;;; The function gptel-org--get-parent-heading-level has been removed
;;; (classical subtree mode no longer exists).

;;; Tests for gptel-org--adjust-response-headings

(ert-deftest gptel-org-subtree-test-adjust-headings-basic ()
  "Test that response headings are demoted correctly in agent indirect buffer."
  (gptel-org-test-with-agent-indirect-buffer
   "* H1\n** TODO Task\n*** :main@agent:                                       :main@agent:\n**** @assistant\n* Summary\nContent\n** Details\nMore\n"
   "* Summary"
   (let ((end (point-max)))
     (gptel-org--adjust-response-headings beg end)
     (goto-char beg)
     ;; @assistant at level 4, so * Summary (level 1) -> ***** Summary (level 5)
     ;; level-diff = (4 + 1) - 1 = 4
     (should (looking-at "\\*\\*\\*\\*\\* Summary"))
     (search-forward "Details")
     (beginning-of-line)
     ;; ** Details (level 2) -> ****** Details (level 6)
     (should (looking-at "\\*\\*\\*\\*\\*\\* Details")))))

(ert-deftest gptel-org-subtree-test-adjust-headings-no-change-needed ()
  "Test that headings at correct level are not changed in agent indirect buffer."
  (gptel-org-test-with-agent-indirect-buffer
   "* H1\n** TODO Task\n*** :main@agent:                                       :main@agent:\n**** @assistant\n***** Summary\nContent\n****** Details\nMore\n"
   "***** Summary"
   (let ((end (point-max)))
     (gptel-org--adjust-response-headings beg end)
     (goto-char beg)
     ;; Already at level 5+ (children of level-4 @assistant), should stay unchanged
     (should (looking-at "\\*\\*\\*\\*\\* Summary"))
     (search-forward "Details")
     (beginning-of-line)
     (should (looking-at "\\*\\*\\*\\*\\*\\* Details")))))

(ert-deftest gptel-org-subtree-test-adjust-headings-mixed-levels ()
  "Test adjustment with mixed heading levels in response in agent indirect buffer."
  (gptel-org-test-with-agent-indirect-buffer
   "* H1\n** TODO Task\n*** :main@agent:                                       :main@agent:\n**** @assistant\n* Top\nContent\n** Middle\n*** Deep\n* Another top\n"
   "* Top"
   (let ((end (point-max)))
     (gptel-org--adjust-response-headings beg end)
     (goto-char beg)
     ;; All headings shifted by same amount: level-diff = (4+1) - 1 = 4
     ;; Level 1 -> Level 5
     (should (looking-at "\\*\\*\\*\\*\\* Top"))
     (search-forward "Middle")
     (beginning-of-line)
     ;; Level 2 -> Level 6
     (should (looking-at "\\*\\*\\*\\*\\*\\* Middle"))
     (search-forward "Deep")
     (beginning-of-line)
     ;; Level 3 -> Level 7
     (should (looking-at "\\*\\*\\*\\*\\*\\*\\* Deep")))))

(ert-deftest gptel-org-subtree-test-adjust-headings-deeply-nested ()
  "Test adjustment with deeply nested assistant heading in agent indirect buffer."
  (gptel-org-test-with-agent-indirect-buffer
   "* A\n** B\n*** TODO Task\n**** :main@agent:                                       :main@agent:\n***** @assistant\n* Summary\nContent\n** Details\nMore\n"
   "* Summary"
   (let ((end (point-max)))
     (gptel-org--adjust-response-headings beg end)
     (goto-char beg)
     ;; @assistant at level 5, so level-diff = (5+1) - 1 = 5
     ;; * Summary (level 1) -> ****** Summary (level 6)
     (should (looking-at "\\*\\*\\*\\*\\*\\* Summary"))
     (search-forward "Details")
     (beginning-of-line)
     ;; ** Details (level 2) -> ******* Details (level 7)
     (should (looking-at "\\*\\*\\*\\*\\*\\*\\* Details")))))

(ert-deftest gptel-org-subtree-test-adjust-headings-no-headings ()
  "Test that text without headings is not affected in agent indirect buffer."
  (gptel-org-test-with-agent-indirect-buffer
   "* H1\n** TODO Task\n*** :main@agent:                                       :main@agent:\n**** @assistant\nJust some text\nWith multiple lines\nNo headings here\n"
   "Just some text"
   (let ((end (point-max))
         (original (buffer-substring beg (point-max))))
     (gptel-org--adjust-response-headings beg end)
     ;; Content should be unchanged
     (should (string= original (buffer-substring beg (point-max)))))))

;;; Tests for heading adjustment in agent indirect buffers

(defmacro gptel-org-test-with-agent-indirect-buffer (content beg-marker &rest body)
  "Create an agent indirect buffer simulating the agent subtree scenario.
CONTENT is the full org content for the base buffer.  BEG-MARKER is a
string to search for to position BEG (the start of AI response).
The indirect buffer is narrowed to include all content from the
:main@agent: heading to the end of the buffer, simulating how the
real agent indirect buffer auto-expands its narrowing via a marker
with insertion-type t as the AI streams text.
BODY receives `beg' bound to the position of BEG-MARKER."
  (declare (indent 2))
  `(let ((org-inhibit-startup t)
         (gptel-org-subtree-context t))
     (with-temp-buffer
       (delay-mode-hooks (org-mode))
       (insert ,content)
       ;; Find the :main@agent: heading
       (goto-char (point-min))
       (let ((agent-start nil))
         (when (re-search-forward "^\\(\\*+\\) .*:main@agent:" nil t)
           (beginning-of-line)
           (setq agent-start (point)))
         (should agent-start)
         ;; Create indirect buffer narrowed from agent heading to end
         ;; of buffer.  This simulates the real scenario where the
         ;; narrowing end-marker has insertion-type t and expands as
         ;; the AI streams incorrectly-leveled headings into the buffer.
         (let* ((indirect-buf (make-indirect-buffer (current-buffer)
                                                     " *gptel-test-agent*" t)))
           (with-current-buffer indirect-buf
             (delay-mode-hooks (org-mode))
             (narrow-to-region agent-start (point-max))
             ;; Find beg marker
             (goto-char (point-min))
             (search-forward ,beg-marker)
             (beginning-of-line)
             (let ((beg (point)))
               ,@body))
           (kill-buffer indirect-buf))))))

(ert-deftest gptel-org-subtree-test-adjust-headings-agent-indirect-basic ()
  "Test heading demotion in agent indirect buffer.
AI response with top-level headings should be demoted to be children
of the @assistant heading in the agent subtree."
  (gptel-org-test-with-agent-indirect-buffer
   "* H1\n** TODO Task 1\n*** :main@agent:                                       :main@agent:\n**** @user\nDo task 1\n**** @assistant\n* Summary\n** Phase 1\n** Phase 2\n*** Detail\n"
   "* Summary"
   (let ((end (point-max)))
     (gptel-org--adjust-response-headings beg end)
     (goto-char beg)
     ;; @assistant is at level 4, so * Summary (level 1) should become ***** Summary (level 5)
     ;; level-diff = (4 + 1) - 1 = 4
     (should (looking-at "\\*\\*\\*\\*\\* Summary"))
     (search-forward "Phase 1")
     (beginning-of-line)
     ;; ** Phase 1 -> ****** Phase 1
     (should (looking-at "\\*\\*\\*\\*\\*\\* Phase 1"))
     (search-forward "Detail")
     (beginning-of-line)
     ;; *** Detail -> ******* Detail
     (should (looking-at "\\*\\*\\*\\*\\*\\*\\* Detail")))))

(ert-deftest gptel-org-subtree-test-adjust-headings-agent-indirect-no-subtree-context ()
  "Test that agent indirect buffer heading fix works without gptel-org-subtree-context.
The fix should trigger based on being in an agent indirect buffer,
even when gptel-org-subtree-context is nil."
  (gptel-org-test-with-agent-indirect-buffer
   "* H1\n** TODO Task 1\n*** :main@agent:                                       :main@agent:\n**** @assistant\n* Summary\nContent\n"
   "* Summary"
   (let ((gptel-org-subtree-context nil))
     (let ((end (point-max)))
       ;; gptel-org-subtree-context is nil (overridden above)
       (should (not gptel-org-subtree-context))
       (gptel-org--adjust-response-headings beg end)
       (goto-char beg)
       ;; Should still be demoted because we're in an agent indirect buffer
       (should (looking-at "\\*\\*\\*\\*\\* Summary"))))))

(ert-deftest gptel-org-subtree-test-adjust-headings-agent-indirect-correct-level ()
  "Test heading levels are correct when response is already at child level."
  (gptel-org-test-with-agent-indirect-buffer
   "* H1\n** TODO Task 1\n*** :main@agent:                                       :main@agent:\n**** @assistant\n***** Already correct\nContent\n"
   "***** Already"
   (let ((end (point-max)))
     (gptel-org--adjust-response-headings beg end)
     (goto-char beg)
     ;; Already at level 5 (child of level-4 @assistant), should stay
     (should (looking-at "\\*\\*\\*\\*\\* Already correct")))))

(ert-deftest gptel-org-subtree-test-adjust-headings-agent-indirect-example-blocks ()
  "Test that headings inside example blocks are not modified in agent indirect buffers."
  (gptel-org-test-with-agent-indirect-buffer
   "* H1\n** TODO Task 1\n*** :main@agent:                                       :main@agent:\n**** @assistant\n* Summary\n#+begin_example\n* Not a heading\n#+end_example\n"
   "* Summary"
   (let ((end (point-max)))
     (gptel-org--adjust-response-headings beg end)
     (goto-char beg)
     ;; * Summary should be demoted
     (should (looking-at "\\*\\*\\*\\*\\* Summary"))
     ;; * inside example should be escaped with comma, not demoted
     (search-forward "begin_example")
     (forward-line 1)
     (should (looking-at ",\\* Not a heading")))))

(ert-deftest gptel-org-subtree-test-adjust-headings-agent-indirect-no-change-correct-level ()
  "Test that correctly-leveled headings are not adjusted when trailing
empty boundary heading is at the reference level.
Reproduces the bug where a trailing empty heading like \"*** \" at the
reference level triggers unnecessary demotion of headings that are
already children of the reference heading."
  (gptel-org-test-with-agent-indirect-buffer
   "** AI-DOING Only print simple message with heading\n***                                                            :main@agent:\n\n\n#+begin_src gptel-reasoning\nThe user is testing AI responsiveness.\n#+end_src\n\n**** AI-DONE Testing complete\n\nTest message successful! AI is responding correctly to your request.\n\n*** \n"
   "#+begin_src gptel-reasoning"
   (let ((end (point-max))
         (original (buffer-substring beg (point-max))))
     (gptel-org--adjust-response-headings beg end)
     ;; Nothing should change: **** AI-DONE is already a child of *** (level 3)
     ;; and the trailing *** boundary should not trigger demotion
     (should (string= original (buffer-substring beg (point-max))))
     (goto-char beg)
     ;; Verify the heading is still at level 4
     (search-forward "AI-DONE")
     (beginning-of-line)
     (should (looking-at "\\*\\*\\*\\* AI-DONE Testing complete")))))

(ert-deftest gptel-org-subtree-test-in-agent-indirect-buffer-p ()
  "Test gptel-org--in-agent-indirect-buffer-p detection."
  ;; Not in an indirect buffer
  (let ((org-inhibit-startup t))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert "* Heading\n")
      (should (not (gptel-org--in-agent-indirect-buffer-p)))))
  ;; In an agent indirect buffer
  (gptel-org-test-with-agent-indirect-buffer
   "* H1\n** TODO Task\n*** :main@agent:                                       :main@agent:\n**** @assistant\nContent\n"
   "Content"
   (should (gptel-org--in-agent-indirect-buffer-p))))

;;; Tests for nested example block escaping

(ert-deftest gptel-org-subtree-test-escape-nested-example-blocks ()
  "Test that nested example blocks are properly escaped."
  (gptel-org-test-with-buffer
   "* Task\n** @assistant\n"
   (goto-char (point-max))
   (let ((beg (point)))
     (insert "Here's an example:\n#+begin_example\n* Outer heading\n#+begin_example\n* Inner heading\n#+end_example\nMore content\n* Another outer heading\n#+end_example\n")
     (let ((end (point)))
       (gptel-org--escape-example-blocks beg end)
       (goto-char beg)
       (search-forward "#+begin_example" nil t)
       (forward-line 1)
       ;; * Outer heading should be escaped
       (should (looking-at ",\\* Outer heading"))
       (forward-line 1)
       ;; #+begin_example (nested) should be escaped
       (should (looking-at ",#\\+begin_example"))
       (forward-line 1)
       ;; * Inner heading should be escaped
       (should (looking-at ",\\* Inner heading"))
       (forward-line 1)
       ;; #+end_example (inner) should be escaped
       (should (looking-at ",#\\+end_example"))
       (forward-line 1)
       ;; More content - not special, unchanged
       (should (looking-at "More content"))
       (forward-line 1)
       ;; * Another outer heading should be escaped
       (should (looking-at ",\\* Another outer heading"))))))

(ert-deftest gptel-org-subtree-test-escape-example-block-already-escaped ()
  "Test that already-escaped lines are not double-escaped."
  (gptel-org-test-with-buffer
   "* Task\n** @assistant\n"
   (goto-char (point-max))
   (let ((beg (point)))
     (insert "#+begin_example\n,* Already escaped\n* Not yet escaped\n#+end_example\n")
     (let ((end (point)))
       (gptel-org--escape-example-blocks beg end)
       (goto-char beg)
       (search-forward "#+begin_example" nil t)
       (forward-line 1)
       ;; ,* should remain as ,* (not become ,,*)
       (should (looking-at ",\\* Already escaped"))
       (forward-line 1)
       ;; * should become ,*
       (should (looking-at ",\\* Not yet escaped"))))))

(ert-deftest gptel-org-subtree-test-escape-src-blocks ()
  "Test that src blocks are also escaped."
  (gptel-org-test-with-buffer
   "* Task\n** @assistant\n"
   (goto-char (point-max))
   (let ((beg (point)))
     (insert "#+begin_src org\n* Heading in src\n#+end_src\n")
     (let ((end (point)))
       (gptel-org--escape-example-blocks beg end)
       (goto-char beg)
       (search-forward "#+begin_src" nil t)
       (forward-line 1)
       (should (looking-at ",\\* Heading in src"))))))

;;; Integration test for full classical subtree workflow — REMOVED
;;; The test gptel-org-subtree-test-full-workflow used
;;; gptel-org--get-parent-heading-level and gptel-org--get-chat-siblings,
;;; both of which have been removed with classical subtree mode.

;;; Tests for tag-based features (gptel-org-infer-bounds-from-tags)

(defmacro gptel-org-test-with-tags-buffer (content &rest body)
  "Create a temp org buffer with CONTENT, enable tag inference, and execute BODY.
Point is placed at the first : character (tag marker) if present, otherwise at end."
  (declare (indent 1))
  `(let ((gptel-org-subtree-context t)
         (gptel-org-infer-bounds-from-tags t)
         (gptel-org-assistant-tag "assistant")
         (gptel-org-user-tag "user")
         (org-inhibit-startup t))
     (with-temp-buffer
       (delay-mode-hooks (org-mode))
       (insert ,content)
       (goto-char (point-min))
       ,@body)))

(ert-deftest gptel-org-subtree-test-chat-heading-p-tag-assistant ()
  "Test that heading with :assistant: tag is recognized as chat heading."
  (gptel-org-test-with-tags-buffer
   "* Task\n** Response :assistant:\nAnswer"
   (search-forward "** Response")
   (beginning-of-line)
   (should (gptel-org--chat-heading-p))))

(ert-deftest gptel-org-subtree-test-chat-heading-p-tag-user ()
  "Test that heading with :user: tag is recognized as chat heading."
  (gptel-org-test-with-tags-buffer
   "* Task\n** Question :user:\nMy question"
   (search-forward "** Question")
   (beginning-of-line)
   (should (gptel-org--chat-heading-p))))

(ert-deftest gptel-org-subtree-test-chat-heading-p-tag-case-insensitive ()
  "Test that tag matching is case-insensitive."
  (gptel-org-test-with-tags-buffer
   "* Task\n** Response :ASSISTANT:\nAnswer"
   (search-forward "** Response")
   (beginning-of-line)
   (should (gptel-org--chat-heading-p))))

(ert-deftest gptel-org-subtree-test-chat-heading-p-tag-no-tag ()
  "Test that heading without chat tags is not recognized."
  (gptel-org-test-with-tags-buffer
   "* Task\n** Details :notes:\nSome notes"
   (search-forward "** Details")
   (beginning-of-line)
   (should-not (gptel-org--chat-heading-p))))

(ert-deftest gptel-org-subtree-test-heading-has-tag-p ()
  "Test gptel-org--heading-has-tag-p function."
  (gptel-org-test-with-tags-buffer
   "* Task\n** Response :assistant:work:\nAnswer"
   (search-forward "** Response")
   (beginning-of-line)
   (should (gptel-org--heading-has-tag-p "assistant"))
   (should (gptel-org--heading-has-tag-p "work"))
   (should (gptel-org--heading-has-tag-p "ASSISTANT"))  ; case-insensitive
   (should-not (gptel-org--heading-has-tag-p "user"))))

(ert-deftest gptel-org-subtree-test-restore-bounds-assistant-children ()
  "Test that assistant subtree children retain gptel response property.
When an :assistant: tagged heading has child headings (without tags),
`gptel-org--restore-bounds-from-tags' must mark the entire subtree as
response, including the children.  Previously the outer scan loop
would re-visit untagged children and clear their properties."
  (gptel-org-test-with-tags-buffer
   (concat "* Debug\n"
           "** AI-DOING Task 1\n"
           "*** Feasibility study              :assistant:\n"
           "**** Report section\n"
           "Report content\n"
           "***** Requirement 3\n"
           "Requirement 3 content\n"
           "***** Requirement 4\n"
           "Requirement 4 content\n"
           "*** User message                   :user:\n"
           "User message content\n"
           "*** Clean recursive pattern        :assistant:\n"
           "Assistant reply\n"
           "*** Sub-agent returns              :user:\n"
           "Final user message\n")
   (gptel-org--restore-bounds-from-tags)
   ;; Assistant heading should be marked as response
   (goto-char (point-min))
   (search-forward "*** Feasibility") (beginning-of-line)
   (should (eq (get-text-property (point) 'gptel) 'response))
   ;; Child headings inside assistant subtree should ALSO be response
   (search-forward "**** Report") (beginning-of-line)
   (should (eq (get-text-property (point) 'gptel) 'response))
   (search-forward "***** Requirement 3") (beginning-of-line)
   (should (eq (get-text-property (point) 'gptel) 'response))
   (search-forward "***** Requirement 4") (beginning-of-line)
   (should (eq (get-text-property (point) 'gptel) 'response))
   ;; User heading should NOT be response
   (search-forward "*** User message") (beginning-of-line)
   (should-not (get-text-property (point) 'gptel))
   ;; Second assistant heading should be response
   (search-forward "*** Clean recursive") (beginning-of-line)
   (should (eq (get-text-property (point) 'gptel) 'response))
   ;; Second user heading should NOT be response
   (search-forward "*** Sub-agent") (beginning-of-line)
   (should-not (get-text-property (point) 'gptel))))


;;; gptel-org-subtree-test-branching-sibling-order — REMOVED
;;; Tested classical subtree mode's sibling scanning in prompt buffers,
;;; which no longer exists.  The agent indirect buffer approach replaces it.

(ert-deftest gptel-org-subtree-test-branching-sibling-order-agent-subtrees ()
  "Test branching context with agent subtrees preserves sibling order.
When `gptel-org-subtree-context' is enabled, the prompt buffer
includes a `:main@agent:' heading whose children are the chat
entries.  `gptel-org-agent--strip-agent-subtrees' must not delete
the chat content that lives under the @agent heading in the
prompt copy."
  (let ((gptel-org-branching-context t)
        (gptel-org-subtree-context t)
        (gptel-org-infer-bounds-from-tags t)
        (gptel-org-assistant-tag "assistant")
        (gptel-org-user-tag "user")
        (gptel-org-ignore-elements nil)
        (gptel-prompt-filter-hook nil)
        (gptel--num-messages-to-send nil)
        (gptel-track-response t)
        (gptel-track-media nil)
        (org-inhibit-startup t))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert "* AI-DOING Only print simple message\n"
              "**                                              :main@agent:\n"
              "*** First response                              :assistant:\n"
              "Test message received.\n"
              "*** Give second message                         :user:\n"
              "Second message here.\n"
              "*** Second response                             :assistant:\n"
              "AI response confirmed.\n"
              "*** Give third message                          :user:\n"
              "Third message here.\n")
      (goto-char (point-max))
      (let* ((backend (gptel-make-openai "test" :key "fake" :models '("gpt-4")))
             (prompt-buf (gptel-org--create-prompt-buffer)))
        (unwind-protect
            (with-current-buffer prompt-buf
              (setq gptel-mode t)
              (goto-char (point-max))
              (let ((messages (gptel--parse-buffer backend nil)))
                ;; Should have 5 messages:
                ;; 1. user: context (TODO heading)
                ;; 2. assistant: First response
                ;; 3. user: Give second message
                ;; 4. assistant: Second response
                ;; 5. user: Give third message (current)
                (should (= (length messages) 5))
                ;; First message should be user with TODO heading content
                (should (string= (plist-get (nth 0 messages) :role) "user"))
                (should (string-match-p "simple message"
                                        (plist-get (nth 0 messages) :content)))
                ;; Second should be assistant
                (should (string= (plist-get (nth 1 messages) :role) "assistant"))
                (should (string-match-p "Test message"
                                        (plist-get (nth 1 messages) :content)))
                ;; Third should be user
                (should (string= (plist-get (nth 2 messages) :role) "user"))
                (should (string-match-p "Second message"
                                        (plist-get (nth 2 messages) :content)))
                ;; Fourth should be assistant
                (should (string= (plist-get (nth 3 messages) :role) "assistant"))
                (should (string-match-p "AI response"
                                        (plist-get (nth 3 messages) :content)))
                ;; Last message should be the current user message
                (should (string= (plist-get (nth 4 messages) :role) "user"))
                (should (string-match-p "Third message"
                                        (plist-get (nth 4 messages) :content)))
                ;; TODO heading should NOT appear in last message
                (should-not (string-match-p "AI-DOING"
                                            (plist-get (nth 4 messages) :content)))))
          (when (buffer-live-p prompt-buf)
            (kill-buffer prompt-buf)))))))
(ert-deftest gptel-org-subtree-test-hybrid-context-includes-siblings ()
  "Test that hybrid context includes sibling sub-headings of TODO heading.
When `gptel-org-branching-context' and `gptel-org-subtree-context' are
both enabled, content within a TODO heading's subtree should use
non-branching context (include all siblings up to cursor), while
ancestors above the TODO use branching context (heading lines only)."
  (let ((gptel-org-branching-context t)
        (gptel-org-subtree-context t)
        (gptel-org-infer-bounds-from-tags nil)
        (gptel-org-ignore-elements nil)
        (gptel-org-todo-keywords '("TODO" "AI-DO" "AI-DOING"))
        (gptel-prompt-filter-hook nil)
        (gptel--num-messages-to-send nil)
        (gptel-track-response t)
        (gptel-track-media nil)
        (org-inhibit-startup t))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert "* Heading 1\n"
              "** Heading 2\n"
              "Some text under heading 2.\n"
              "*** TODO Task 1\n"
              "**** Task 1 sub-topic 1\n"
              "***** Task 1 subsub-topic\n"
              "Sub-sub detail here.\n"
              "**** Task 1 sub-topic 2\n"
              "- Plaa plaa\n")
      (goto-char (point-max))
      (let ((prompt-buf (gptel-org--create-prompt-buffer)))
        (unwind-protect
            (with-current-buffer prompt-buf
              (let ((content (buffer-string)))
                ;; Should contain parent headings (as heading lines)
                (should (string-match-p "Heading 1" content))
                (should (string-match-p "Heading 2" content))
                ;; Parent heading body text should NOT be included
                ;; (branching context above TODO = heading lines only)
                (should-not (string-match-p "Some text under heading 2" content))
                ;; Should contain TODO heading
                (should (string-match-p "TODO Task 1" content))
                ;; CRITICAL: Should contain sibling sub-topic 1 and its children
                (should (string-match-p "Task 1 sub-topic 1" content))
                (should (string-match-p "Task 1 subsub-topic" content))
                (should (string-match-p "Sub-sub detail here" content))
                ;; Should contain sub-topic 2 and cursor text
                (should (string-match-p "Task 1 sub-topic 2" content))
                (should (string-match-p "Plaa plaa" content))))
          (when (buffer-live-p prompt-buf)
            (kill-buffer prompt-buf)))))))

(ert-deftest gptel-org-subtree-test-hybrid-context-excludes-after-cursor ()
  "Test that hybrid context excludes content after cursor position."
  (let ((gptel-org-branching-context t)
        (gptel-org-subtree-context t)
        (gptel-org-infer-bounds-from-tags nil)
        (gptel-org-ignore-elements nil)
        (gptel-org-todo-keywords '("TODO"))
        (gptel-prompt-filter-hook nil)
        (gptel--num-messages-to-send nil)
        (gptel-track-response t)
        (gptel-track-media nil)
        (org-inhibit-startup t))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert "* H1\n"
              "** TODO Task\n"
              "*** Sub1\nSub1 content.\n"
              "*** Sub2\n- cursor here\n"
              "*** Sub3\nAfter cursor content.\n")
      ;; Place cursor at "cursor here"
      (goto-char (point-min))
      (search-forward "cursor here")
      (let ((prompt-buf (gptel-org--create-prompt-buffer)))
        (unwind-protect
            (with-current-buffer prompt-buf
              (let ((content (buffer-string)))
                ;; Should include Sub1 and Sub2 (before cursor)
                (should (string-match-p "Sub1" content))
                (should (string-match-p "Sub1 content" content))
                (should (string-match-p "Sub2" content))
                ;; Should NOT include Sub3 (after cursor)
                (should-not (string-match-p "Sub3" content))
                (should-not (string-match-p "After cursor content" content))))
          (when (buffer-live-p prompt-buf)
            (kill-buffer prompt-buf)))))))

(ert-deftest gptel-org-subtree-test-hybrid-context-pure-branching-without-subtree-context ()
  "Without gptel-org-subtree-context, pure branching context is used."
  (let ((gptel-org-branching-context t)
        (gptel-org-subtree-context nil)
        (gptel-org-infer-bounds-from-tags nil)
        (gptel-org-ignore-elements nil)
        (gptel-org-todo-keywords '("TODO"))
        (gptel-prompt-filter-hook nil)
        (gptel--num-messages-to-send nil)
        (gptel-track-response t)
        (gptel-track-media nil)
        (org-inhibit-startup t))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert "* H1\n"
              "** TODO Task\n"
              "*** Sub1\nSub1 content.\n"
              "*** Sub2\n- cursor here\n")
      (goto-char (point-min))
      (search-forward "cursor here")
      (let ((prompt-buf (gptel-org--create-prompt-buffer)))
        (unwind-protect
            (with-current-buffer prompt-buf
              (let ((content (buffer-string)))
                ;; Pure branching: Sub1 should NOT be included (sibling, not lineage)
                (should-not (string-match-p "Sub1" content))
                ;; Sub2 and cursor text should be included (in lineage)
                (should (string-match-p "Sub2" content))
                (should (string-match-p "cursor here" content))))
          (when (buffer-live-p prompt-buf)
            (kill-buffer prompt-buf)))))))

(ert-deftest gptel-org-subtree-test-hybrid-context-cursor-on-todo ()
  "Hybrid context works when cursor is on the TODO heading itself."
  (let ((gptel-org-branching-context t)
        (gptel-org-subtree-context t)
        (gptel-org-infer-bounds-from-tags nil)
        (gptel-org-ignore-elements nil)
        (gptel-org-todo-keywords '("TODO"))
        (gptel-prompt-filter-hook nil)
        (gptel--num-messages-to-send nil)
        (gptel-track-response t)
        (gptel-track-media nil)
        (org-inhibit-startup t))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert "* H1\n"
              "** TODO Task heading text\n"
              "*** Sub1\nSub1 content.\n")
      ;; Place cursor at end of TODO heading line
      (goto-char (point-min))
      (search-forward "Task heading text")
      (let ((prompt-buf (gptel-org--create-prompt-buffer)))
        (unwind-protect
            (with-current-buffer prompt-buf
              (let ((content (buffer-string)))
                ;; Should contain parent and TODO heading
                (should (string-match-p "H1" content))
                (should (string-match-p "TODO Task heading text" content))
                ;; Sub1 is AFTER cursor, should not be included
                (should-not (string-match-p "Sub1" content))))
          (when (buffer-live-p prompt-buf)
            (kill-buffer prompt-buf)))))))

(ert-deftest gptel-org-subtree-test-hybrid-context-with-assistant-tags ()
  "Hybrid context correctly handles :assistant: and :user: tagged headings."
  (let ((gptel-org-branching-context t)
        (gptel-org-subtree-context t)
        (gptel-org-infer-bounds-from-tags t)
        (gptel-org-assistant-tag "assistant")
        (gptel-org-user-tag "user")
        (gptel-org-ignore-elements nil)
        (gptel-org-todo-keywords '("TODO" "AI-DO" "AI-DOING"))
        (gptel-prompt-filter-hook nil)
        (gptel--num-messages-to-send nil)
        (gptel-track-response t)
        (gptel-track-media nil)
        (org-inhibit-startup t))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert "* Heading 1\n"
              "** Heading 2\n"
              "*** TODO Task 1\n"
              "**** Task 1 sub-topic 1\n"
              "Topic 1 details.\n"
              "**** Task 1 sub-topic 2\n"
              "- Plaa plaa\n"
              "**** AI response                                :assistant:\n"
              "Here is my analysis.\n"
              "**** User reply                                 :user:\n"
              "Thanks, continue.\n")
      (goto-char (point-max))
      (let* ((backend (gptel-make-openai "test" :key "fake" :models '("gpt-4")))
             (prompt-buf (gptel-org--create-prompt-buffer)))
        (unwind-protect
            (with-current-buffer prompt-buf
              (setq gptel-mode t)
              (goto-char (point-max))
              (let ((messages (gptel--parse-buffer backend nil)))
                ;; Should have messages: user context, assistant, user reply
                (should (>= (length messages) 3))
                ;; First message should be user with TODO + siblings
                (should (string= (plist-get (nth 0 messages) :role) "user"))
                (should (string-match-p "Task 1 sub-topic 1"
                                        (plist-get (nth 0 messages) :content)))
                (should (string-match-p "Topic 1 details"
                                        (plist-get (nth 0 messages) :content)))
                (should (string-match-p "Plaa plaa"
                                        (plist-get (nth 0 messages) :content)))
                ;; Should have assistant message
                (let ((asst-msg (cl-find "assistant" messages
                                         :key (lambda (m) (plist-get m :role))
                                         :test #'string=)))
                  (should asst-msg)
                  (should (string-match-p "my analysis"
                                          (plist-get asst-msg :content))))
                ;; Last message should be user
                (should (string= (plist-get (car (last messages)) :role) "user"))
                (should (string-match-p "continue"
                                        (plist-get (car (last messages)) :content)))))
          (when (buffer-live-p prompt-buf)
            (kill-buffer prompt-buf)))))))

(ert-deftest gptel-org-subtree-test-hybrid-no-todo-in-lineage ()
  "When no TODO heading in lineage, pure branching context is used."
  (let ((gptel-org-branching-context t)
        (gptel-org-subtree-context t)
        (gptel-org-infer-bounds-from-tags nil)
        (gptel-org-ignore-elements nil)
        (gptel-org-todo-keywords '("TODO"))
        (gptel-prompt-filter-hook nil)
        (gptel--num-messages-to-send nil)
        (gptel-track-response t)
        (gptel-track-media nil)
        (org-inhibit-startup t))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert "* H1\n"
              "** Task\n"
              "*** Sub1\nSub1 content.\n"
              "*** Sub2\n- cursor here\n")
      (goto-char (point-min))
      (search-forward "cursor here")
      (let ((prompt-buf (gptel-org--create-prompt-buffer)))
        (unwind-protect
            (with-current-buffer prompt-buf
              (let ((content (buffer-string)))
                ;; No TODO heading, so pure branching = Sub1 excluded
                (should-not (string-match-p "Sub1" content))
                (should (string-match-p "Sub2" content))))
          (when (buffer-live-p prompt-buf)
            (kill-buffer prompt-buf)))))))

;;; Tests for gptel-org--auto-correct-on-change (idempotent corrector)

(ert-deftest gptel-org-subtree-test-auto-correct-agent-indirect-preserves-agent-heading ()
  "Test that auto-corrector does not rebase the agent heading at point-min.
The agent heading (e.g. *** AI-DOING ... :main@agent:) is at point-min
in the indirect buffer.  The corrector must skip it and only rebase
the AI response content that follows."
  (gptel-org-test-with-agent-indirect-buffer
   "* H1\n** DOING What is 2 + 2\n*** AI-DOING What is 2 + 2              :main@agent:\n"
   "AI-DOING"
   (goto-char (point-max))
   (let ((response-start (point))
         (gptel-org-use-todo-keywords t)
         (gptel-org-assistant-keyword "AI"))
     ;; Enable the idempotent auto-corrector
     (gptel-org--enable-auto-correct)
     ;; Simulate AI streaming — the after-change hook fires automatically
     (insert "* AI Simple Arithmetic\n\n2 + 2 = *4*.\n\n** AI Details\nMore info.\n")
     ;; Verify the agent heading was NOT modified
     (goto-char (point-min))
     (should (looking-at "\\*\\*\\* AI-DOING What is 2 \\+ 2"))
     ;; Verify AI response headings WERE rebased correctly
     ;; ref-level = (1+ 3) = 4, so AI's "* AI" (level 1) → "**** AI" (level 4)
     (goto-char response-start)
     (should (looking-at "\\*\\*\\*\\* AI Simple Arithmetic"))
     (search-forward "Details")
     (beginning-of-line)
     ;; AI's "** AI Details" (level 2) → "***** AI Details" (level 5)
     (should (looking-at "\\*\\*\\*\\*\\* AI Details")))))

(ert-deftest gptel-org-subtree-test-auto-correct-agent-indirect-level-2-task ()
  "Test auto-corrector with level-2 task heading produces correct levels.
Reproduces the original bug where ** DOING → ********* headings."
  (gptel-org-test-with-agent-indirect-buffer
   "* Project\n** DOING What is 2 + 2\n*** AI-DOING What is 2 + 2              :main@agent:\n"
   "AI-DOING"
   (goto-char (point-max))
   (let ((response-start (point))
         (gptel-org-use-todo-keywords t)
         (gptel-org-assistant-keyword "AI"))
     (gptel-org--enable-auto-correct)
     (insert "* AI Answer\n\n2 + 2 = 4.\n")
     ;; Agent heading must remain at level 3
     (goto-char (point-min))
     (should (looking-at "\\*\\*\\* AI-DOING"))
     ;; AI heading must be at level 4 (= agent level 3 + 1)
     (goto-char response-start)
     (should (looking-at "\\*\\*\\*\\* AI Answer")))))

(ert-deftest gptel-org-subtree-test-auto-correct-multi-chunk-no-double-correction ()
  "Test that heading correction is not applied twice across stream chunks.
The idempotent corrector should skip already-rebased headings, so
inserting more text after a corrected heading must not re-correct it."
  (gptel-org-test-with-agent-indirect-buffer
   "* H1\n** DOING What is 2 + 2\n*** AI-DOING What is 2 + 2              :main@agent:\n"
   "AI-DOING"
   (let ((gptel-org-use-todo-keywords t)
         (gptel-org-assistant-keyword "AI"))
     (gptel-org--enable-auto-correct)
     ;; Chunk 1: stream just the heading line
     (goto-char (point-max))
     (let ((response-start (point)))
       (insert "* AI Simple arithmetic\n")
       ;; Verify heading was correctly rebased to level 4
       (goto-char response-start)
       (should (looking-at "\\*\\*\\*\\* AI Simple arithmetic"))
       ;; Chunk 2: stream body text
       (goto-char (point-max))
       (insert "\n2 + 2 = 4.\n")
       ;; Verify heading is STILL at level 4, not double-corrected to level 7
       (goto-char response-start)
       (should (looking-at "\\*\\*\\*\\* AI Simple arithmetic"))))))

(ert-deftest gptel-org-subtree-test-auto-correct-multi-chunk-multiple-headings ()
  "Test multi-chunk correction with headings split across chunks."
  (gptel-org-test-with-agent-indirect-buffer
   "* H1\n** DOING Task\n*** AI-DOING Task              :main@agent:\n"
   "AI-DOING"
   (let ((gptel-org-use-todo-keywords t)
         (gptel-org-assistant-keyword "AI"))
     (gptel-org--enable-auto-correct)
     ;; Chunk 1: first heading
     (goto-char (point-max))
     (let ((response-start (point)))
       (insert "* AI Overview\nSome text.\n")
       ;; Chunk 2: second heading
       (goto-char (point-max))
       (insert "** AI Details\nMore text.\n")
       ;; Verify both headings at correct levels
       (goto-char response-start)
       ;; * AI Overview → **** AI Overview (level 4)
       (should (looking-at "\\*\\*\\*\\* AI Overview"))
       (search-forward "Details")
       (beginning-of-line)
       ;; ** AI Details → ***** AI Details (level 5)
       (should (looking-at "\\*\\*\\*\\*\\* AI Details"))))))


(ert-deftest gptel-org-subtree-test-auto-correct-example-block-closing-delimiter ()
  "Test that outer #+end_src is NOT comma-escaped by auto-corrector.
Reproduces the bug where `gptel-org--in-example-block-p' treats the
closing delimiter of the outermost block as \"inside\" the block,
causing it to be comma-escaped and permanently breaking the block."
  (gptel-org-test-with-agent-indirect-buffer
   "* H1\n** DOING Task\n*** AI-DOING Task              :main@agent:\n"
   "AI-DOING"
   (goto-char (point-max))
   (let ((response-start (point))
         (gptel-org-use-todo-keywords t)
         (gptel-org-assistant-keyword "AI"))
     (gptel-org--enable-auto-correct)
     ;; Simulate AI writing a src org block containing nested org syntax
     (insert "#+begin_src org\n* Heading\n#+begin_src elisp\n(+ 2 2)\n#+end_src\n#+begin_example\n4\n#+end_example\n#+end_src\n")
     ;; The outer #+end_src must NOT be comma-escaped
     (goto-char response-start)
     ;; Inner content should be comma-escaped
     (search-forward "#+begin_src org")
     (forward-line 1)
     (should (looking-at ",\\* Heading"))
     (forward-line 1)
     (should (looking-at ",#\\+begin_src elisp"))
     (forward-line 1)
     ;; (+ 2 2) doesn't start with * or #+, so no comma
     (should (looking-at "(\\+ 2 2)"))
     (forward-line 1)
     ;; Inner #+end_src should be comma-escaped
     (should (looking-at ",#\\+end_src"))
     (forward-line 1)
     ;; #+begin_example should be comma-escaped
     (should (looking-at ",#\\+begin_example"))
     (forward-line 1)
     ;; 4 doesn't need escaping
     (should (looking-at "4"))
     (forward-line 1)
     ;; #+end_example should be comma-escaped
     (should (looking-at ",#\\+end_example"))
     (forward-line 1)
     ;; CRITICAL: outer #+end_src must NOT be comma-escaped
     (should (looking-at "#\\+end_src"))
     (should (not (looking-at ","))))))

(ert-deftest gptel-org-subtree-test-auto-correct-simple-src-block-closing ()
  "Test that #+end_src of a simple src block is NOT comma-escaped.
Reproduces the case where #+begin_src org contains only headings
and text (no nested blocks), and the outer #+end_src still gets
incorrectly comma-escaped."
  (gptel-org-test-with-agent-indirect-buffer
   "* H1\n** DOING Task\n*** AI-DOING Task              :main@agent:\n"
   "AI-DOING"
   (goto-char (point-max))
   (let ((response-start (point))
         (gptel-org-use-todo-keywords t)
         (gptel-org-assistant-keyword "AI"))
     (gptel-org--enable-auto-correct)
     ;; Simulate AI writing a src org block with headings (no nested blocks)
     (insert "#+begin_src org\n**** Result\n2 + 2 = /4/\n#+end_src\n")
     (goto-char response-start)
     (search-forward "#+begin_src org")
     (forward-line 1)
     ;; Heading inside block should be comma-escaped
     (should (looking-at ",\\*\\*\\*\\* Result"))
     (forward-line 1)
     ;; Normal text - no escaping needed
     (should (looking-at "2 \\+ 2"))
     (forward-line 1)
     ;; CRITICAL: outer #+end_src must NOT be comma-escaped
     (should (looking-at "#\\+end_src"))
     (should (not (looking-at ","))))))

(ert-deftest gptel-org-subtree-test-auto-correct-src-block-after-reasoning ()
  "Test that #+end_src of src org block is not escaped after reasoning block.
Reproduces the real-world case where a gptel-reasoning block precedes
a #+begin_src org block.  The reasoning block's #+end_src must not
confuse the block-depth tracking for the subsequent org block."
  (gptel-org-test-with-agent-indirect-buffer
   "* H1\n** DOING Task\n*** AI-DOING Task              :main@agent:\n"
   "AI-DOING"
   (goto-char (point-max))
   (let ((response-start (point))
         (gptel-org-use-todo-keywords t)
         (gptel-org-assistant-keyword "AI"))
     (gptel-org--enable-auto-correct)
     ;; Simulate AI writing reasoning block then src org block
     (insert "#+begin_src gptel-reasoning\nThinking...\n#+end_src\n\n#+begin_src org\n**** Result\n2 + 2 = /4/\n#+end_src\n")
     (goto-char response-start)
     ;; Reasoning block should be untouched (no * or #+ content inside)
     (search-forward "#+begin_src gptel-reasoning")
     (forward-line 1)
     (should (looking-at "Thinking\\.\\.\\."))
     (forward-line 1)
     ;; Reasoning block's #+end_src must NOT be escaped
     (should (looking-at "#\\+end_src"))
     ;; Now check the org block
     (search-forward "#+begin_src org")
     (forward-line 1)
     ;; Heading inside block should be comma-escaped
     (should (looking-at ",\\*\\*\\*\\* Result"))
     (forward-line 1)
     (should (looking-at "2 \\+ 2"))
     (forward-line 1)
     ;; CRITICAL: org block's #+end_src must NOT be comma-escaped
     (should (looking-at "#\\+end_src"))
     (should (not (looking-at ","))))))

(ert-deftest gptel-org-subtree-test-auto-correct-streaming-src-block ()
  "Test that streaming #+end_src is not escaped when arriving in a later chunk.
Simulates the real streaming scenario where #+begin_src org arrives
in one chunk and #+end_src arrives in a subsequent chunk."
  (gptel-org-test-with-agent-indirect-buffer
   "* H1\n** DOING Task\n*** AI-DOING Task              :main@agent:\n"
   "AI-DOING"
   (goto-char (point-max))
   (let ((response-start (point))
         (gptel-org-use-todo-keywords t)
         (gptel-org-assistant-keyword "AI"))
     (gptel-org--enable-auto-correct)
     ;; Chunk 1: reasoning block
     (insert "#+begin_src gptel-reasoning\nThinking...\n#+end_src\n\n")
     ;; Chunk 2: src org opener and content
     (insert "#+begin_src org\n**** Result\n2 + 2 = /4/\n")
     ;; Chunk 3: closing delimiter arrives
     (insert "#+end_src\n")
     ;; Verify the org block
     (goto-char response-start)
     (search-forward "#+begin_src org")
     (forward-line 1)
     (should (looking-at ",\\*\\*\\*\\* Result"))
     (forward-line 1)
     (should (looking-at "2 \\+ 2"))
     (forward-line 1)
     ;; CRITICAL: #+end_src must NOT be comma-escaped
     (should (looking-at "#\\+end_src"))
     (should (not (looking-at ","))))))

(ert-deftest gptel-org-subtree-test-auto-correct-remove-comma-from-outer-closer ()
  "Test that a comma-escaped outer #+end_example is corrected.
When the AI writes ,#+end_example as the outer block closer, the
comma must be removed because org does not strip commas from
delimiter lines — only from content lines inside the block.
A ,#+end_example is never recognised as a block closer by org,
so the block runs to end-of-file."
  (gptel-org-test-with-agent-indirect-buffer
   "* H1\n** DOING Task\n*** AI-DOING Task              :main@agent:\n"
   "AI-DOING"
   (goto-char (point-max))
   (let ((response-start (point))
         (gptel-org-use-todo-keywords t)
         (gptel-org-assistant-keyword "AI"))
     (gptel-org--enable-auto-correct)
     ;; Simulate AI writing an example block where the closer is
     ;; incorrectly comma-escaped
     (insert "#+begin_example\n* Top heading\n- List item\n,#+end_example\n")
     (goto-char response-start)
     (search-forward "#+begin_example")
     (forward-line 1)
     ;; * heading inside block should be comma-escaped
     (should (looking-at ",\\* Top heading"))
     (forward-line 1)
     (should (looking-at "- List item"))
     (forward-line 1)
     ;; CRITICAL: comma must have been REMOVED from #+end_example
     (should (looking-at "#\\+end_example"))
     (should (not (looking-at ","))))))

(ert-deftest gptel-org-subtree-test-auto-correct-remove-comma-from-outer-end-src ()
  "Test that a comma-escaped outer #+end_src is corrected.
Same as the end_example test but for src blocks."
  (gptel-org-test-with-agent-indirect-buffer
   "* H1\n** DOING Task\n*** AI-DOING Task              :main@agent:\n"
   "AI-DOING"
   (goto-char (point-max))
   (let ((response-start (point))
         (gptel-org-use-todo-keywords t)
         (gptel-org-assistant-keyword "AI"))
     (gptel-org--enable-auto-correct)
     ;; Simulate AI writing a src block where the closer is
     ;; incorrectly comma-escaped
     (insert "#+begin_src org\n* Heading\n,#+end_src\n")
     (goto-char response-start)
     (search-forward "#+begin_src org")
     (forward-line 1)
     ;; * heading inside block should be comma-escaped
     (should (looking-at ",\\* Heading"))
     (forward-line 1)
     ;; CRITICAL: comma must have been REMOVED from #+end_src
     (should (looking-at "#\\+end_src"))
     (should (not (looking-at ","))))))

(ert-deftest gptel-org-subtree-test-auto-correct-keep-comma-on-inner-closer ()
  "Test that comma on inner #+end_example is preserved.
When a block closer is inside an outer block (nested), it is content
and its comma prefix must be kept."
  (gptel-org-test-with-agent-indirect-buffer
   "* H1\n** DOING Task\n*** AI-DOING Task              :main@agent:\n"
   "AI-DOING"
   (goto-char (point-max))
   (let ((response-start (point))
         (gptel-org-use-todo-keywords t)
         (gptel-org-assistant-keyword "AI"))
     (gptel-org--enable-auto-correct)
     ;; Outer src block containing an inner example block
     ;; AI correctly writes content but incorrectly commas the outer closer
     (insert "#+begin_src org\n#+begin_example\nContent\n#+end_example\n,#+end_src\n")
     (goto-char response-start)
     (search-forward "#+begin_src org")
     (forward-line 1)
     ;; Inner #+begin_example should be comma-escaped (content of outer block)
     (should (looking-at ",#\\+begin_example"))
     (forward-line 1)
     (should (looking-at "Content"))
     (forward-line 1)
     ;; Inner #+end_example should be comma-escaped (content of outer block)
     (should (looking-at ",#\\+end_example"))
     (forward-line 1)
     ;; CRITICAL: outer #+end_src must have comma REMOVED
     (should (looking-at "#\\+end_src"))
     (should (not (looking-at ","))))))


(ert-deftest gptel-org-subtree-test-branching-includes-assistant-sibling ()
  "Branching context includes assistant-response sibling headings.

When a user heading has an assistant-response sibling before it (e.g.
AI-DONE), the branching context should include that assistant content
in the prompt, preserving the conversation history."
  (let ((gptel-org-branching-context t)
        (gptel-org-subtree-context nil)
        (gptel-org-infer-bounds-from-tags nil)
        (gptel-org-use-todo-keywords t)
        (gptel-org-ignore-elements nil)
        (gptel-org-todo-keywords '("AI-DO" "AI-DOING"))
        (gptel-prompt-filter-hook nil)
        (gptel--num-messages-to-send nil)
        (gptel-track-response t)
        (gptel-track-media nil)
        (org-inhibit-startup t)
        ;; Register TODO keywords so org-get-todo-state recognizes them
        (org-todo-keywords '((sequence "AI-DO" "AI-DOING" "DOING" "FEEDBACK"
                                       "|" "AI-DONE" "DONE"))))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      ;; Activate the TODO keyword definitions in this buffer
      (org-set-regexps-and-options)
      (insert "* H1\n"
              "** FEEDBACK Calculate 2 + A where you pick A value from 0-9\n"
              "*** AI-DONE Calculate 2 + A where you pick A value from 0-9\n"
              "I'll pick A = 7. So 2 + 7 = 9.\n"
              "*** FEEDBACK Add 10 to the result\n"
              "- cursor here\n")
      (goto-char (point-min))
      (search-forward "cursor here")
      (let* ((backend (gptel-make-openai "test" :key "fake" :models '("gpt-4")))
             (prompt-buf (gptel-org--create-prompt-buffer)))
        (unwind-protect
            (with-current-buffer prompt-buf
              (setq gptel-mode t)
              (goto-char (point-max))
              (let ((messages (gptel--parse-buffer backend nil))
                    (content (buffer-string)))
                ;; Should have at least 2 messages: user context + assistant + user follow-up
                (should (>= (length messages) 2))
                ;; Should contain assistant response
                (let ((asst-msg (cl-find "assistant" messages
                                         :key (lambda (m) (plist-get m :role))
                                         :test #'string=)))
                  (should asst-msg)
                  (should (string-match-p "2 \\+ 7 = 9"
                                          (plist-get asst-msg :content))))
                ;; Last message should be user with follow-up
                (should (string= (plist-get (car (last messages)) :role) "user"))
                (should (string-match-p "Add 10"
                                        (plist-get (car (last messages)) :content)))))
          (when (buffer-live-p prompt-buf)
            (kill-buffer prompt-buf)))))))
(provide 'gptel-org-subtree-test)
;;; gptel-org-subtree-test.el ends here
