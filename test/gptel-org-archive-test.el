;;; gptel-org-archive-test.el --- Tests for gptel-org-archive -*- lexical-binding: t; -*-

;; Tests for the gptel-org-archive module which provides:
;; - Archive location determination
;; - Subtree content extraction
;; - Git metadata extraction and parsing
;; - Summary formatting

(require 'ert)
(require 'gptel-org-archive)

;;; Helper macro for org buffer tests

(defmacro gptel-org-archive-test-with-buffer (content &rest body)
  "Create a temp org buffer with CONTENT and execute BODY."
  (declare (indent 1))
  `(let ((org-inhibit-startup t)
         (org-todo-keywords '((sequence "TODO" "|" "DONE")))
         (org-done-keywords '("DONE")))
     (with-temp-buffer
       (delay-mode-hooks (org-mode))
       (insert ,content)
       (goto-char (point-min))
       ,@body)))

;;; Tests for archive location

(ert-deftest gptel-org-archive-test-default-location-ai-file ()
  "Test archive location for *-ai.org files."
  (let ((result (gptel-org-archive--default-location "/path/to/task-ai.org")))
    (should (equal result "/path/to/task-ai-archive.org"))))

(ert-deftest gptel-org-archive-test-default-location-regular-file ()
  "Test archive location for non-ai.org files."
  (let ((result (gptel-org-archive--default-location "/path/to/notes.org")))
    (should (equal result "/path/to/notes_archive.org"))))

(ert-deftest gptel-org-archive-test-default-location-nested-ai ()
  "Test archive location for nested -ai.org files."
  (let ((result (gptel-org-archive--default-location "/my-ai-project/task-ai.org")))
    (should (equal result "/my-ai-project/task-ai-archive.org"))))

;;; Tests for subtree content extraction

(ert-deftest gptel-org-archive-test-get-subtree-content ()
  "Test extracting subtree content."
  (gptel-org-archive-test-with-buffer
   "* DONE Completed task\nSome content\n** Sub-heading\nMore content"
   (org-back-to-heading t)
   (let ((info (gptel-org-archive--get-subtree-content)))
     (should (equal (plist-get info :heading) "Completed task"))
     (should (equal (plist-get info :level) 1))
     (should (equal (plist-get info :todo-state) "DONE"))
     (should (string-match-p "Sub-heading" (plist-get info :content))))))

(ert-deftest gptel-org-archive-test-get-subtree-with-properties ()
  "Test extracting subtree content with properties."
  (gptel-org-archive-test-with-buffer
   "* DONE Task with props
:PROPERTIES:
:GPTEL_BACKEND: Claude
:GPTEL_MODEL: sonnet
:END:
Task content"
   (org-back-to-heading t)
   (let ((info (gptel-org-archive--get-subtree-content)))
     (should (equal (plist-get info :backend) "Claude"))
     (should (equal (plist-get info :model) "sonnet")))))

;;; Tests for conversation extraction

(ert-deftest gptel-org-archive-test-extract-conversation ()
  "Test extracting conversation from content."
  (let ((content "* DONE Task
:PROPERTIES:
:GPTEL_BACKEND: Claude
:END:
** @user
Question here
** @assistant
Answer here"))
    (let ((result (gptel-org-archive--extract-conversation content)))
      ;; Should remove property drawer
      (should-not (string-match-p ":PROPERTIES:" result))
      (should-not (string-match-p ":END:" result))
      ;; Should keep conversation
      (should (string-match-p "@user" result))
      (should (string-match-p "Question here" result)))))

(ert-deftest gptel-org-archive-test-extract-conversation-multiple-blanks ()
  "Test that excessive blank lines are removed."
  (let ((content "* Task\n\n\n\n\nContent with gaps\n\n\n\nMore content"))
    (let ((result (gptel-org-archive--extract-conversation content)))
      ;; Should not have more than 2 consecutive newlines
      (should-not (string-match-p "\n\\{3,\\}" result)))))

;;; Tests for git changes parsing

(ert-deftest gptel-org-archive-test-parse-git-changes ()
  "Test parsing git changes section from summary."
  (let ((summary "Task summary here.

Git Changes:
- REPO_PATH: quelpa/build/gptel
  COMMITS: abc1234, def5678
- REPO_PATH: .
  COMMITS: 123abcd"))
    (let* ((parsed (gptel-org-archive--parse-git-changes summary))
           (clean-summary (car parsed))
           (git-changes (cdr parsed)))
      ;; Clean summary should not contain Git Changes section
      (should-not (string-match-p "Git Changes:" clean-summary))
      (should (string-match-p "Task summary" clean-summary))
      ;; Should parse git changes
      (should (= 2 (length git-changes)))
      (let ((first (car git-changes)))
        (should (equal (plist-get first :path) "quelpa/build/gptel"))
        (should (equal (plist-get first :commits) "abc1234, def5678"))))))

(ert-deftest gptel-org-archive-test-parse-git-changes-no-section ()
  "Test parsing when no git changes section exists."
  (let ((summary "Just a regular summary without git info."))
    (let* ((parsed (gptel-org-archive--parse-git-changes summary))
           (clean-summary (car parsed))
           (git-changes (cdr parsed)))
      (should (equal clean-summary summary))
      (should-not git-changes))))

(ert-deftest gptel-org-archive-test-parse-git-changes-path-only ()
  "Test parsing git changes with path but no commits."
  ;; The parser requires a newline after the REPO_PATH line
  (let ((summary "Summary.

Git Changes:
- REPO_PATH: some/path
"))
    (let* ((parsed (gptel-org-archive--parse-git-changes summary))
           (git-changes (cdr parsed)))
      (should (= 1 (length git-changes)))
      (let ((first (car git-changes)))
        (should (equal (plist-get first :path) "some/path"))
        (should-not (plist-get first :commits))))))

;;; Tests for git changes formatting

(ert-deftest gptel-org-archive-test-format-git-changes ()
  "Test formatting git changes as org properties."
  (let ((changes (list (list :path "repo/path" :commits "abc, def")
                       (list :path "other/repo" :commits "123"))))
    (let ((result (gptel-org-archive--format-git-changes changes)))
      (should (string-match-p ":GIT_PATH: repo/path" result))
      (should (string-match-p ":GIT_COMMITS: abc, def" result))
      (should (string-match-p ":GIT_PATH_1: other/repo" result))
      (should (string-match-p ":GIT_COMMITS_1: 123" result)))))

(ert-deftest gptel-org-archive-test-format-git-changes-empty ()
  "Test formatting empty git changes."
  (should-not (gptel-org-archive--format-git-changes nil)))

;;; Tests for persistent task support

(ert-deftest gptel-org-archive-test-persistent-task-p ()
  "Test detecting persistent tag on heading."
  (gptel-org-archive-test-with-buffer
   "* TODO Task with persistent tag          :persistent:
Description here
"
   (org-back-to-heading t)
   (should (gptel-org-archive--persistent-task-p))))

(ert-deftest gptel-org-archive-test-persistent-task-p-negative ()
  "Test that headings without persistent tag return nil."
  (gptel-org-archive-test-with-buffer
   "* TODO Regular task                      :haiku:
Description here
"
   (org-back-to-heading t)
   (should-not (gptel-org-archive--persistent-task-p))))

(ert-deftest gptel-org-archive-test-persistent-task-p-case-insensitive ()
  "Test case-insensitive persistent tag detection."
  (gptel-org-archive-test-with-buffer
   "* TODO Task                              :Persistent:
Description here
"
   (org-back-to-heading t)
   (should (gptel-org-archive--persistent-task-p))))

(ert-deftest gptel-org-archive-test-persistent-task-p-multiple-tags ()
  "Test persistent tag detection with multiple tags."
  (gptel-org-archive-test-with-buffer
   "* TODO Task                              :opus:persistent:
Description here
"
   (org-back-to-heading t)
   (should (gptel-org-archive--persistent-task-p))))

(ert-deftest gptel-org-archive-test-find-conversation-start ()
  "Test finding the first user/assistant sub-heading."
  (gptel-org-archive-test-with-buffer
   "* TODO Task                              :persistent:
Description text here
- bullet point
** First response                        :assistant:
Some response
** Follow up                             :user:
More text
"
   (org-back-to-heading t)
   (let ((conv-start (gptel-org-archive--find-conversation-start)))
     (should conv-start)
     (save-excursion
       (goto-char conv-start)
       (should (org-at-heading-p))
       (should (string-match-p "First response" (org-get-heading t t t t)))))))

(ert-deftest gptel-org-archive-test-find-conversation-start-none ()
  "Test finding conversation start when no user/assistant headings exist."
  (gptel-org-archive-test-with-buffer
   "* TODO Task                              :persistent:
Description text
** Regular sub-heading
Some text
"
   (org-back-to-heading t)
   (should-not (gptel-org-archive--find-conversation-start))))

(ert-deftest gptel-org-archive-test-find-conversation-start-user-first ()
  "Test finding conversation start when user heading comes first."
  (gptel-org-archive-test-with-buffer
   "* TODO Task                              :persistent:
Description text
**                                       :user:
User's question
** Response                              :assistant:
AI response
"
   (org-back-to-heading t)
   (let ((conv-start (gptel-org-archive--find-conversation-start)))
     (should conv-start)
     (save-excursion
       (goto-char conv-start)
       (should (let ((tags (org-get-tags nil t)))
                 (cl-some (lambda (tag) (string-equal-ignore-case tag "user")) tags)))))))

(ert-deftest gptel-org-archive-test-strip-state-log ()
  "Test stripping state log entries."
  (let ((text "- State \"AI-DOING\"   from \"AI-DO\"      [2026-03-18 Wed 08:39]
- The widget crashes when clicking submit
- Only happens with empty form fields
"))
    (let ((result (gptel-org-archive--strip-state-log text)))
      (should-not (string-match-p "State \"AI-DOING\"" result))
      (should (string-match-p "widget crashes" result))
      (should (string-match-p "empty form fields" result)))))

(ert-deftest gptel-org-archive-test-strip-state-log-multiple ()
  "Test stripping multiple state log entries."
  (let ((text "- State \"AI-DOING\"   from \"AI-DO\"      [2026-03-18 Wed 08:39]
- State \"AI-DO\"      from              [2026-03-17 Tue 10:00]
- Keep this description line
"))
    (let ((result (gptel-org-archive--strip-state-log text)))
      (should-not (string-match-p "State" result))
      (should (string-match-p "Keep this" result)))))

(ert-deftest gptel-org-archive-test-strip-state-log-none ()
  "Test that text without state log is preserved."
  (let ((text "- Description line 1
- Description line 2
"))
    (let ((result (gptel-org-archive--strip-state-log text)))
      (should (string-match-p "Description line 1" result))
      (should (string-match-p "Description line 2" result)))))

(ert-deftest gptel-org-archive-test-format-persistent-summary ()
  "Test formatting summary for persistent task."
  (let ((summary "Fixed the widget crash. Root cause was null check."))
    (let ((result (gptel-org-archive--format-persistent-summary
                   summary
                   '(:heading "Fix widget" :level 2))))
      ;; Should have a Summary sub-heading at level 3 (2+1)
      (should (string-match-p "^\\*\\*\\* Summary$" result))
      ;; Should contain the summary text
      (should (string-match-p "Fixed the widget crash" result)))))

(ert-deftest gptel-org-archive-test-format-persistent-summary-strips-git ()
  "Test that git changes are stripped from persistent summary."
  (let ((summary "Fixed bug.\n\nGit Changes:\n- REPO_PATH: .\n  COMMITS: abc1234\n"))
    (let ((result (gptel-org-archive--format-persistent-summary
                   summary
                   '(:heading "Fix" :level 1))))
      (should (string-match-p "Fixed bug" result))
      (should-not (string-match-p "Git Changes" result))
      (should-not (string-match-p "REPO_PATH" result)))))

(ert-deftest gptel-org-archive-test-get-subtree-content-persistent ()
  "Test that get-subtree-content captures persistent flag."
  (gptel-org-archive-test-with-buffer
   "* TODO Persistent task                   :persistent:
Description
** Response                              :assistant:
AI text
"
   (org-back-to-heading t)
   (let ((info (gptel-org-archive--get-subtree-content)))
     (should (plist-get info :persistent))
     (should (plist-get info :conversation-start)))))

(ert-deftest gptel-org-archive-test-get-subtree-content-not-persistent ()
  "Test that get-subtree-content returns nil for non-persistent tasks."
  (gptel-org-archive-test-with-buffer
   "* DONE Regular task
Description
** Response                              :assistant:
AI text
"
   (org-back-to-heading t)
   (let ((info (gptel-org-archive--get-subtree-content)))
     (should-not (plist-get info :persistent))
     (should (plist-get info :conversation-start)))))

(ert-deftest gptel-org-archive-test-replace-persistent ()
  "Test that replace-persistent preserves heading and description."
  (gptel-org-archive-test-with-buffer
   "* TODO Fix widget issue                  :persistent:
- State \"AI-DOING\"   from \"AI-DO\"      [2026-03-18 Wed 08:39]
- The widget crashes on submit
- Only with empty fields
** Investigation                         :assistant:
Long conversation about the fix...
** Follow up                             :user:
More questions
** Final answer                          :assistant:
The solution was...
"
   (org-back-to-heading t)
   (let ((task-info (gptel-org-archive--get-subtree-content)))
     ;; Mock the archive write (no actual file)
     (cl-letf (((symbol-function 'gptel-org-archive--write-to-archive)
                (lambda (_summary _task-info) nil)))
       (gptel-org-archive--replace-persistent
        "Fixed widget crash by adding null check in validate()."
        task-info))
     ;; Verify heading is preserved
     (goto-char (point-min))
     (should (string-match-p "Fix widget issue" (buffer-string)))
     (should (string-match-p ":persistent:" (buffer-string)))
     ;; Verify description is preserved (without state log)
     (should (string-match-p "widget crashes on submit" (buffer-string)))
     (should (string-match-p "Only with empty fields" (buffer-string)))
     ;; Verify state log is stripped
     (should-not (string-match-p "State \"AI-DOING\"" (buffer-string)))
     ;; Verify conversation is removed
     (should-not (string-match-p "Investigation" (buffer-string)))
     (should-not (string-match-p "Long conversation" (buffer-string)))
     (should-not (string-match-p "Follow up" (buffer-string)))
     (should-not (string-match-p "Final answer" (buffer-string)))
     ;; Verify summary sub-heading is added
     (should (string-match-p "Summary" (buffer-string)))
     (should (string-match-p "null check" (buffer-string))))))

(ert-deftest gptel-org-archive-test-replace-persistent-no-conversation ()
  "Test persistent replace when there's no conversation."
  (gptel-org-archive-test-with-buffer
   "* TODO Fix widget issue                  :persistent:
- The widget crashes on submit
"
   (org-back-to-heading t)
   (let ((task-info (gptel-org-archive--get-subtree-content)))
     (cl-letf (((symbol-function 'gptel-org-archive--write-to-archive)
                (lambda (_summary _task-info) nil)))
       (gptel-org-archive--replace-persistent
        "Initial analysis summary."
        task-info))
     ;; Heading and description preserved
     (should (string-match-p "Fix widget issue" (buffer-string)))
     (should (string-match-p "widget crashes" (buffer-string)))
     ;; Summary added
     (should (string-match-p "Summary" (buffer-string)))
     (should (string-match-p "Initial analysis" (buffer-string))))))

(ert-deftest gptel-org-archive-test-replace-persistent-with-property-drawer ()
  "Test persistent replace preserves property drawer."
  (gptel-org-archive-test-with-buffer
   "* TODO Fix widget                        :persistent:
:PROPERTIES:
:GPTEL_BACKEND: Claude
:END:
- Description text
** Response                              :assistant:
AI response text
"
   (org-back-to-heading t)
   (let ((task-info (gptel-org-archive--get-subtree-content)))
     (cl-letf (((symbol-function 'gptel-org-archive--write-to-archive)
                (lambda (_summary _task-info) nil)))
       (gptel-org-archive--replace-persistent "Summary text." task-info))
     ;; Property drawer preserved
     (should (string-match-p ":PROPERTIES:" (buffer-string)))
     (should (string-match-p "GPTEL_BACKEND" (buffer-string)))
     ;; Description preserved
     (should (string-match-p "Description text" (buffer-string)))
     ;; Conversation removed
     (should-not (string-match-p "AI response text" (buffer-string)))
     ;; Summary added
     (should (string-match-p "Summary text" (buffer-string))))))

(provide 'gptel-org-archive-test)
;;; gptel-org-archive-test.el ends here
