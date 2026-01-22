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
Point is placed at the first @ character if present, otherwise at end."
  (declare (indent 1))
  `(let ((gptel-org-subtree-context t))
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

(provide 'gptel-org-subtree-test)
;;; gptel-org-subtree-test.el ends here
