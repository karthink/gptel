;;; gptel-org-subtree-test.el --- Tests for gptel-org subtree context feature -*- lexical-binding: t; -*-

;; Tests for the gptel-org-subtree-context feature which enables
;; task-oriented conversation workflows under TODO headings.
;;
;; Features tested:
;; - gptel-org--chat-heading-p: Check if heading is a chat entry
;; - gptel-org--get-parent-heading-level: Find task heading level
;; - gptel-org--get-chat-siblings: Collect sibling chat headings
;; - gptel-org--dynamic-prefix-string: Adjust prefix heading levels
;; - gptel-org--adjust-response-headings: Demote response headings

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
  `(let ((org-inhibit-startup t)
         (gptel-org-chat-heading-markers '("@user" "@assistant")))
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

(ert-deftest gptel-org-subtree-test-chat-heading-p-user ()
  "Test that @user heading is recognized as chat heading."
  (gptel-org-test-with-buffer
   "* Task\n** @user\nQuestion"
   (should (gptel-org--chat-heading-p "@user"))))

(ert-deftest gptel-org-subtree-test-chat-heading-p-assistant ()
  "Test that @assistant heading is recognized as chat heading."
  (gptel-org-test-with-buffer
   "* Task\n** @assistant\nResponse"
   (should (gptel-org--chat-heading-p "@assistant"))))

(ert-deftest gptel-org-subtree-test-chat-heading-p-non-chat ()
  "Test that regular headings are not recognized as chat headings."
  (gptel-org-test-with-buffer
   "* Task\n** Details\nSome text"
   (should-not (gptel-org--chat-heading-p "Details"))
   (should-not (gptel-org--chat-heading-p "TODO Something"))
   (should-not (gptel-org--chat-heading-p "Summary"))))

(ert-deftest gptel-org-subtree-test-chat-heading-p-embedded ()
  "Test chat heading detection with embedded marker."
  (gptel-org-test-with-buffer
   "* Task"
   ;; Marker embedded in larger text should still match
   (should (gptel-org--chat-heading-p "My @user prompt"))
   (should (gptel-org--chat-heading-p "@assistant response here"))))

(ert-deftest gptel-org-subtree-test-chat-heading-p-at-heading ()
  "Test gptel-org--chat-heading-p when point is at a heading."
  (gptel-org-test-with-buffer
   "* Task\n** @user\nQuestion"
   (goto-char (point-min))
   (search-forward "** @user")
   (beginning-of-line)
   (should (gptel-org--chat-heading-p))))

;;; Tests for gptel-org--get-parent-heading-level

(ert-deftest gptel-org-subtree-test-parent-level-simple ()
  "Test finding parent level in simple task structure."
  (gptel-org-test-with-buffer
   "* Task\n** @user\nQuestion"
   (search-forward "Question")
   (should (= 1 (gptel-org--get-parent-heading-level)))))

(ert-deftest gptel-org-subtree-test-parent-level-nested ()
  "Test finding parent level with nested task headings."
  (gptel-org-test-with-buffer
   "* Project\n** Task\n*** @user\nQuestion"
   (search-forward "Question")
   (should (= 2 (gptel-org--get-parent-heading-level)))))

(ert-deftest gptel-org-subtree-test-parent-level-deeply-nested ()
  "Test finding parent level with deeply nested structure."
  (gptel-org-test-with-buffer
   "* Level 1\n** Level 2\n*** TODO Task\n**** @user\nQuestion\n**** @assistant\nAnswer"
   (search-forward "Question")
   (should (= 3 (gptel-org--get-parent-heading-level)))))

(ert-deftest gptel-org-subtree-test-parent-level-from-assistant ()
  "Test finding parent level when inside @assistant subtree."
  (gptel-org-test-with-buffer
   "* Task\n** @user\nQuestion\n** @assistant\nAnswer\n*** Details\nMore info"
   (search-forward "More info")
   (should (= 1 (gptel-org--get-parent-heading-level)))))

(ert-deftest gptel-org-subtree-test-parent-level-at-chat-heading ()
  "Test finding parent level when at a chat heading."
  (gptel-org-test-with-buffer
   "* Task\n** @user\nQuestion"
   (goto-char (point-min))
   (search-forward "** @user")
   (beginning-of-line)
   (should (= 1 (gptel-org--get-parent-heading-level)))))

;;; Tests for gptel-org--get-chat-siblings

(ert-deftest gptel-org-subtree-test-get-siblings-basic ()
  "Test collecting chat siblings in basic structure."
  (gptel-org-test-with-buffer
   "* Task\n** @user\nQuestion\n** @assistant\nAnswer"
   (search-forward "Answer")
   (let ((siblings (gptel-org--get-chat-siblings)))
     (should (= 2 (length siblings)))
     ;; Each sibling should be a cons of (beg . end)
     (should (consp (car siblings)))
     (should (consp (cadr siblings))))))

(ert-deftest gptel-org-subtree-test-get-siblings-multiple-turns ()
  "Test collecting chat siblings with multiple conversation turns."
  (gptel-org-test-with-buffer
   "* Task\n** @user\nQ1\n** @assistant\nA1\n** @user\nQ2\n** @assistant\nA2"
   (goto-char (point-max))
   (let ((siblings (gptel-org--get-chat-siblings)))
     (should (= 4 (length siblings))))))

(ert-deftest gptel-org-subtree-test-get-siblings-with-non-chat ()
  "Test that non-chat siblings are excluded."
  (gptel-org-test-with-buffer
   "* Task\n** Notes\nSome notes\n** @user\nQuestion\n** @assistant\nAnswer\n** References\nLinks"
   (search-forward "Answer")
   (let ((siblings (gptel-org--get-chat-siblings)))
     ;; Should only collect @user and @assistant, not Notes or References
     (should (= 2 (length siblings))))))

(ert-deftest gptel-org-subtree-test-get-siblings-nested-content ()
  "Test that siblings with nested subheadings are fully captured."
  (gptel-org-test-with-buffer
   "* Task\n** @user\nQuestion\n** @assistant\nAnswer\n*** Details\nMore\n*** Code\nExample"
   (search-forward "Question")
   (let ((siblings (gptel-org--get-chat-siblings)))
     (should (= 2 (length siblings)))
     ;; The @assistant sibling should include its subheadings
     (let* ((assistant-sibling (cadr siblings))
            (content (buffer-substring (car assistant-sibling) (cdr assistant-sibling))))
       (should (string-match-p "Details" content))
       (should (string-match-p "Code" content))))))

(ert-deftest gptel-org-subtree-test-get-siblings-disabled ()
  "Test that get-chat-siblings returns nil when feature is disabled."
  (let ((gptel-org-subtree-context nil))
    (gptel-org-test-with-temp-buffer
     "* Task\n** @user\nQuestion\n** @assistant\nAnswer"
     (search-forward "Answer")
     (should-not (gptel-org--get-chat-siblings)))))

;;; Tests for gptel-org--dynamic-prefix-string

(ert-deftest gptel-org-subtree-test-dynamic-prefix-basic ()
  "Test dynamic prefix adjustment with starred prefix."
  (gptel-org-test-with-buffer
   "* Task\n** @user\nQuestion"
   (search-forward "Question")
   (let ((result (gptel-org--dynamic-prefix-string "** @assistant\n")))
     ;; Parent level is 1, so target level is 2
     (should (string= "** @assistant\n" result)))))

(ert-deftest gptel-org-subtree-test-dynamic-prefix-nested ()
  "Test dynamic prefix adjustment with nested task."
  (gptel-org-test-with-buffer
   "* Project\n** Task\n*** @user\nQuestion"
   (search-forward "Question")
   (let ((result (gptel-org--dynamic-prefix-string "** @assistant\n")))
     ;; Parent level is 2, so target level is 3
     (should (string= "*** @assistant\n" result)))))

(ert-deftest gptel-org-subtree-test-dynamic-prefix-no-stars ()
  "Test dynamic prefix when base prefix has no stars."
  (gptel-org-test-with-buffer
   "* Task\n** @user\nQuestion"
   (search-forward "Question")
   (let ((result (gptel-org--dynamic-prefix-string "@assistant\n")))
     ;; Should prepend stars: level 1 parent -> level 2 for chat
     (should (string= "** @assistant\n" result)))))

(ert-deftest gptel-org-subtree-test-dynamic-prefix-empty ()
  "Test dynamic prefix when base prefix is empty."
  (gptel-org-test-with-buffer
   "* Task\n** @user\nQuestion"
   (search-forward "Question")
   (let ((result (gptel-org--dynamic-prefix-string "")))
     ;; Should create stars with space and newline
     (should (string= "** \n" result)))))

(ert-deftest gptel-org-subtree-test-dynamic-prefix-disabled ()
  "Test that dynamic prefix returns base prefix when feature is disabled."
  (let ((gptel-org-subtree-context nil))
    (gptel-org-test-with-temp-buffer
     "* Task\n** @user\nQuestion"
     (search-forward "Question")
     (let ((result (gptel-org--dynamic-prefix-string "** @assistant\n")))
       ;; Should return unchanged
       (should (string= "** @assistant\n" result))))))

(ert-deftest gptel-org-subtree-test-dynamic-prefix-level-4 ()
  "Test dynamic prefix at level 4 task."
  (gptel-org-test-with-buffer
   "* A\n** B\n*** C\n**** TODO Task\n***** @user\nQuestion"
   (search-forward "Question")
   (let ((result (gptel-org--dynamic-prefix-string "** @assistant\n")))
     ;; Parent level is 4, so target level is 5
     (should (string= "***** @assistant\n" result)))))

;;; Tests for gptel-org--adjust-response-headings

(ert-deftest gptel-org-subtree-test-adjust-headings-basic ()
  "Test that response headings are demoted correctly."
  (gptel-org-test-with-buffer
   "* Task\n** @assistant\n"
   (goto-char (point-max))
   (let ((beg (point)))
     (insert "* Summary\nContent\n** Details\nMore")
     (let ((end (point)))
       (gptel-org--adjust-response-headings beg end)
       (goto-char beg)
       ;; * Summary should become *** Summary (level 2 + 1 = 3)
       (should (looking-at "\\*\\*\\* Summary"))
       (search-forward "Details")
       (beginning-of-line)
       ;; ** Details should become **** Details
       (should (looking-at "\\*\\*\\*\\* Details"))))))

(ert-deftest gptel-org-subtree-test-adjust-headings-no-change-needed ()
  "Test that headings at correct level are not changed."
  (gptel-org-test-with-buffer
   "* Task\n** @assistant\n"
   (goto-char (point-max))
   (let ((beg (point)))
     (insert "*** Summary\nContent\n**** Details\nMore")
     (let ((end (point)))
       (gptel-org--adjust-response-headings beg end)
       (goto-char beg)
       ;; Already at level 3+, should stay unchanged
       (should (looking-at "\\*\\*\\* Summary"))
       (search-forward "Details")
       (beginning-of-line)
       (should (looking-at "\\*\\*\\*\\* Details"))))))

(ert-deftest gptel-org-subtree-test-adjust-headings-mixed-levels ()
  "Test adjustment with mixed heading levels in response."
  (gptel-org-test-with-buffer
   "* Task\n** @assistant\n"
   (goto-char (point-max))
   (let ((beg (point)))
     (insert "* Top\nContent\n** Middle\n*** Deep\n* Another top")
     (let ((end (point)))
       (gptel-org--adjust-response-headings beg end)
       (goto-char beg)
       ;; All headings should be shifted by the same amount
       ;; Level 1 -> Level 3 (diff of 2)
       (should (looking-at "\\*\\*\\* Top"))
       (search-forward "Middle")
       (beginning-of-line)
       ;; Level 2 -> Level 4
       (should (looking-at "\\*\\*\\*\\* Middle"))
       (search-forward "Deep")
       (beginning-of-line)
       ;; Level 3 -> Level 5
       (should (looking-at "\\*\\*\\*\\*\\* Deep"))))))

(ert-deftest gptel-org-subtree-test-adjust-headings-disabled ()
  "Test that heading adjustment is skipped when feature is disabled."
  (let ((gptel-org-subtree-context nil))
    (gptel-org-test-with-temp-buffer
     "* Task\n** @assistant\n"
     (goto-char (point-max))
     (let ((beg (point)))
       (insert "* Summary\nContent")
       (let ((end (point)))
         (gptel-org--adjust-response-headings beg end)
         (goto-char beg)
         ;; Should remain unchanged
         (should (looking-at "\\* Summary")))))))

(ert-deftest gptel-org-subtree-test-adjust-headings-deeply-nested ()
  "Test adjustment with deeply nested assistant heading."
  (gptel-org-test-with-buffer
   "* A\n** B\n*** TODO Task\n**** @assistant\n"
   (goto-char (point-max))
   (let ((beg (point)))
     (insert "* Summary\nContent\n** Details\nMore")
     (let ((end (point)))
       (gptel-org--adjust-response-headings beg end)
       (goto-char beg)
       ;; Assistant at level 4, so min response level (1) should go to level 5
       ;; Level diff = 5 - 1 = 4
       (should (looking-at "\\*\\*\\*\\*\\* Summary"))
       (search-forward "Details")
       (beginning-of-line)
       (should (looking-at "\\*\\*\\*\\*\\*\\* Details"))))))

(ert-deftest gptel-org-subtree-test-adjust-headings-no-headings ()
  "Test that text without headings is not affected."
  (gptel-org-test-with-buffer
   "* Task\n** @assistant\n"
   (goto-char (point-max))
   (let ((beg (point)))
     (insert "Just some text\nWith multiple lines\nNo headings here")
     (let ((end (point))
           (original (buffer-substring beg (point))))
       (gptel-org--adjust-response-headings beg end)
       ;; Content should be unchanged
       (should (string= original (buffer-substring beg end)))))))

;;; Integration tests

(ert-deftest gptel-org-subtree-test-full-workflow ()
  "Test the complete subtree context workflow."
  (gptel-org-test-with-buffer
   "* Project\n** TODO Implement feature\n*** @user\nHow do I implement X?\n*** @assistant\nHere's how:\n**** Step 1\nDo this\n**** Step 2\nDo that\n*** @user\nThanks, what about Y?\n"
   ;; Position at the last @user entry
   (goto-char (point-max))
   (search-backward "@user")
   (forward-line 1)
   (end-of-line)
   
   ;; Verify parent level detection
   (should (= 2 (gptel-org--get-parent-heading-level)))
   
   ;; Verify sibling collection
   (let ((siblings (gptel-org--get-chat-siblings)))
     (should (= 3 (length siblings))))
   
   ;; Verify prefix adjustment
   (let ((prefix (gptel-org--dynamic-prefix-string "** @assistant\n")))
     (should (string= "*** @assistant\n" prefix)))))

;;; Tests for tag-based features (gptel-org-infer-bounds-from-tags)

(defmacro gptel-org-test-with-tags-buffer (content &rest body)
  "Create a temp org buffer with CONTENT, enable tag inference, and execute BODY.
Point is placed at the first : character (tag marker) if present, otherwise at end."
  (declare (indent 1))
  `(let ((gptel-org-subtree-context t)
         (gptel-org-infer-bounds-from-tags t)
         (gptel-org-assistant-tag "assistant")
         (gptel-org-user-tag "user")
         (gptel-org-chat-heading-markers '("@user" "@assistant"))
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

(ert-deftest gptel-org-subtree-test-dynamic-prefix-tag-mode ()
  "Test dynamic prefix in tag-based mode."
  (gptel-org-test-with-tags-buffer
   "* Task\n** Question :user:\nQuestion text"
   (search-forward "Question text")
   (let ((result (gptel-org--dynamic-prefix-string "** @assistant\n")))
     ;; In tag mode, should generate tag-style heading
     (should (string= "** :assistant:\n" result)))))

(ert-deftest gptel-org-subtree-test-dynamic-prefix-tag-nested ()
  "Test dynamic prefix in tag-based mode with nested task."
  (gptel-org-test-with-tags-buffer
   "* Project\n** Task\n*** Question :user:\nQuestion text"
   (search-forward "Question text")
   (let ((result (gptel-org--dynamic-prefix-string "** @assistant\n")))
     ;; Parent level is 2, so target level is 3
     (should (string= "*** :assistant:\n" result)))))

(ert-deftest gptel-org-subtree-test-dynamic-prefix-tag-for-prompt ()
  "Test dynamic prefix for user prompt in tag mode."
  (gptel-org-test-with-tags-buffer
   "* Task\n** Response :assistant:\nAnswer"
   (search-forward "Answer")
   (let ((result (gptel-org--dynamic-prefix-string "** @user\n" t)))
     ;; for-prompt=t should generate user tag
     (should (string= "** :user:\n" result)))))

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

(ert-deftest gptel-org-subtree-test-apply-pending-tag-correct-heading ()
  "Test that pending tag is applied to newly inserted heading, not previous one.
This tests the fix for a bug where `gptel-org--apply-pending-tag-on-change'
would incorrectly apply the tag to the heading *before* the insertion point
rather than to the newly inserted heading."
  (gptel-org-test-with-tags-buffer
   "* Project\n** AI-DOING Task\nSome content\n"
   (goto-char (point-max))
   ;; Simulate what happens when gptel inserts a response:
   ;; 1. gptel-org--dynamic-prefix-string sets up pending tag
   ;; 2. Heading is inserted
   ;; 3. after-change-functions triggers apply-pending-tag-on-change
   (let ((prefix (gptel-org--dynamic-prefix-string "** @assistant\n")))
     ;; Verify prefix was generated with tag
     (should (string-match-p ":assistant:" prefix))
     ;; Now insert the prefix (this triggers after-change-functions)
     (let ((insert-pos (point)))
       (insert prefix)
       ;; Manually trigger the hook since temp buffers may not have it active
       (when gptel-org--pending-heading-tag
         (gptel-org--apply-pending-tag-on-change insert-pos (point) 0))))
   ;; Now verify the tags are on the correct headings
   (goto-char (point-min))
   (search-forward "AI-DOING")
   (beginning-of-line)
   ;; AI-DOING should NOT have the assistant tag
   (should-not (gptel-org--heading-has-tag-p "assistant"))
   ;; Find the new heading
   (outline-next-heading)
   ;; The new heading SHOULD have the assistant tag
   (should (gptel-org--heading-has-tag-p "assistant"))))

(ert-deftest gptel-org-subtree-test-pending-tag-no-leak-to-sibling ()
  "Test that pending tag does not leak to a nearby sibling heading.
This reproduces the real scenario: gptel-response-separator (\\n\\n) is
inserted together with the prompt prefix in a single `insert' call.
The `after-change-functions' hook fires with beg at the separator (not
the heading), so `org-at-heading-p' fails and `re-search-forward' must
find the correct heading rather than a nearby sibling."
  (gptel-org-test-with-tags-buffer
   ;; Buffer: two sibling tasks, cursor between them.
   ;; The second heading is within 200 chars of the first.
   (concat "* Project\n"
           "** AI-DOING Print hello world\n"
           "*** Response content :assistant:\n"
           "Done.\n"
           "** AI-DOING Run remote command\n")
   ;; Go to end of "Done." line — where a user prompt would be inserted
   (goto-char (point-min))
   (search-forward "Done.\n")
   ;; Simulate the real insertion: gptel-prompt-prefix-string is evaluated
   ;; first (setting up the pending tag), then insert is called with both
   ;; the separator and prefix together.
   (let ((prefix (gptel-org--dynamic-prefix-string "*** @user\n" 'for-prompt))
         (separator "\n\n"))
     (should (string-match-p ":user:" prefix))
     ;; This mirrors: (insert gptel-response-separator (gptel-prompt-prefix-string))
     ;; The after-change-functions hook fires with beg at the separator position.
     (let ((insert-pos (point)))
       (insert separator prefix)
       ;; Manually trigger since temp buffers may not have the hook active
       (when gptel-org--pending-heading-tag
         (gptel-org--apply-pending-tag-on-change insert-pos (point) 0))))
   ;; The user tag should be on the newly inserted heading only
   (goto-char (point-min))
   (search-forward "Run remote command")
   (beginning-of-line)
   ;; The sibling heading MUST NOT have the :user: tag
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


(ert-deftest gptel-org-subtree-test-branching-sibling-order ()
  "Test that branching context with chat siblings preserves message order.
When `gptel-org-branching-context' and `gptel-org-subtree-context' are
both enabled, chat siblings must be inserted AFTER all lineage headings
in the prompt buffer.  Previously, the siblings were inserted before
intermediate lineage headings, causing the TODO heading to appear as
the last user message instead of being part of the first (context)
message.  This also caused the current user message to merge with the
TODO heading."
  (let ((gptel-org-branching-context t)
        (gptel-org-subtree-context t)
        (gptel-org-infer-bounds-from-tags t)
        (gptel-org-assistant-tag "assistant")
        (gptel-org-user-tag "user")
        (gptel-org-chat-heading-markers '("@user" "@assistant"))
        (gptel-org-ignore-elements nil)
        (gptel-org-agent-subtrees nil)
        (gptel-prompt-filter-hook nil)
        (gptel--num-messages-to-send nil)
        (gptel-track-response t)
        (gptel-track-media nil)
        (org-inhibit-startup t))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert "* Project\n"
              "** AI-DOING Fix the widget\n"
              "*** First response                              :assistant:\n"
              "Done fixing.\n"
              "*** Follow-up question                          :user:\n"
              "What about tests?\n"
              "*** Second response                             :assistant:\n"
              "Tests added.\n"
              "*** Another question                            :user:\n"
              "Looks good, commit it.\n")
      (goto-char (point-max))
      (let* ((backend (gptel-make-openai "test" :key "fake" :models '("gpt-4")))
             (prompt-buf (gptel-org--create-prompt-buffer)))
        (unwind-protect
            (with-current-buffer prompt-buf
              (setq gptel-mode t)
              (goto-char (point-max))
              (let ((messages (gptel--parse-buffer backend nil)))
                ;; Should have 5 messages: context, asst, user, asst, user
                (should (= (length messages) 5))
                ;; First message should be user with BOTH parent AND TODO heading
                (should (string= (plist-get (nth 0 messages) :role) "user"))
                (should (string-match-p "Project" (plist-get (nth 0 messages) :content)))
                (should (string-match-p "Fix the widget"
                                        (plist-get (nth 0 messages) :content)))
                ;; Last message should be just the current user message
                (should (string= (plist-get (nth 4 messages) :role) "user"))
                (should (string-match-p "Looks good"
                                        (plist-get (nth 4 messages) :content)))
                ;; TODO heading should NOT appear in last message
                (should-not (string-match-p "AI-DOING"
                                            (plist-get (nth 4 messages) :content)))))
          (when (buffer-live-p prompt-buf)
            (kill-buffer prompt-buf)))))))

(ert-deftest gptel-org-subtree-test-branching-sibling-order-agent-subtrees ()
  "Test branching context with agent subtrees preserves sibling order.
When `gptel-org-agent-subtrees' is enabled, the prompt buffer
includes a `:main@agent:' heading whose children are the chat
entries.  `gptel-org-agent--strip-agent-subtrees' must not delete
the chat content that lives under the @agent heading in the
prompt copy."
  (let ((gptel-org-branching-context t)
        (gptel-org-subtree-context t)
        (gptel-org-infer-bounds-from-tags t)
        (gptel-org-assistant-tag "assistant")
        (gptel-org-user-tag "user")
        (gptel-org-chat-heading-markers '("@user" "@assistant"))
        (gptel-org-ignore-elements nil)
        (gptel-org-agent-subtrees t)
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
When `gptel-org-branching-context' and `gptel-org-agent-subtrees' are
both enabled, content within a TODO heading's subtree should use
non-branching context (include all siblings up to cursor), while
ancestors above the TODO use branching context (heading lines only)."
  (let ((gptel-org-branching-context t)
        (gptel-org-subtree-context nil)
        (gptel-org-infer-bounds-from-tags nil)
        (gptel-org-ignore-elements nil)
        (gptel-org-agent-subtrees t)
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
        (gptel-org-subtree-context nil)
        (gptel-org-infer-bounds-from-tags nil)
        (gptel-org-ignore-elements nil)
        (gptel-org-agent-subtrees t)
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

(ert-deftest gptel-org-subtree-test-hybrid-context-pure-branching-without-agent ()
  "Without gptel-org-agent-subtrees, pure branching context is used."
  (let ((gptel-org-branching-context t)
        (gptel-org-subtree-context nil)
        (gptel-org-infer-bounds-from-tags nil)
        (gptel-org-ignore-elements nil)
        (gptel-org-agent-subtrees nil)
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
        (gptel-org-subtree-context nil)
        (gptel-org-infer-bounds-from-tags nil)
        (gptel-org-ignore-elements nil)
        (gptel-org-agent-subtrees t)
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
        (gptel-org-subtree-context nil)
        (gptel-org-infer-bounds-from-tags t)
        (gptel-org-assistant-tag "assistant")
        (gptel-org-user-tag "user")
        (gptel-org-ignore-elements nil)
        (gptel-org-agent-subtrees t)
        (gptel-org-todo-keywords '("TODO" "AI-DO" "AI-DOING"))
        (gptel-org-chat-heading-markers '("@user" "@assistant"))
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
        (gptel-org-subtree-context nil)
        (gptel-org-infer-bounds-from-tags nil)
        (gptel-org-ignore-elements nil)
        (gptel-org-agent-subtrees t)
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

(provide 'gptel-org-subtree-test)
;;; gptel-org-subtree-test.el ends here
