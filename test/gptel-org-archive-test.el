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

(provide 'gptel-org-archive-test)
;;; gptel-org-archive-test.el ends here
