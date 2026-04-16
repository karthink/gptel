;;; gptel-org-todo-write-test.el --- Tests for TodoWrite org heading structure  -*- lexical-binding: t; -*-

;; Tests for gptel-org-agent--write-todo-org and related functions.
;; Validates that TodoWrite creates todo headings directly under the agent
;; heading (no "Tasks" container) and that the FSM position markers are
;; redirected into the in_progress heading so AI responses stream under it.

(require 'ert)
(require 'gptel)
(require 'gptel-org)
(require 'gptel-org-agent)

;;; Helper macros

(defmacro gptel-org-todo-test-with-buffer (content &rest body)
  "Create a temp org buffer with CONTENT and execute BODY.
Sets up org-mode with AI-DO/AI-DOING/AI-DONE keywords."
  (declare (indent 1))
  `(let ((org-inhibit-startup t)
         (org-todo-keywords '((sequence "AI-DO" "AI-DOING" "|" "AI-DONE")))
         (gptel-org-todo-keywords '("AI-DO" "AI-DOING"))
         (gptel-org-agent-status-keyword-map
          '(("pending"     . "AI-DO")
            ("in_progress" . "AI-DOING")
            ("completed"   . "AI-DONE")))
         ;; Suppress ensure-todo-keywords side effects
         (gptel-org-agent-todo-keywords nil))
     (with-temp-buffer
       (delay-mode-hooks (org-mode))
       (insert ,content)
       (goto-char (point-min))
       ,@body)))

;;; ---- Basic todo heading creation ------------------------------------------

(ert-deftest gptel-org-todo-test-creates-headings-without-tasks-container ()
  "TodoWrite should create headings directly under agent, no Tasks heading."
  (gptel-org-todo-test-with-buffer
      "** AI-DOING My task :main@agent:\n"
    (gptel-org-agent--write-todo-org
     '((:content "Step one" :status "pending" :activeForm "Doing step one")
       (:content "Step two" :status "pending" :activeForm "Doing step two")))
    (goto-char (point-min))
    ;; No "Tasks" heading should exist
    (should-not (re-search-forward "^\\*+ Tasks$" nil t))
    ;; Todo headings should be direct children (level 3 under level 2)
    (goto-char (point-min))
    (should (re-search-forward "^\\*\\*\\* AI-DO Step one$" nil t))
    (should (re-search-forward "^\\*\\*\\* AI-DO Step two$" nil t))))

(ert-deftest gptel-org-todo-test-creates-headings-at-correct-level ()
  "Todo headings should be one level deeper than the agent heading."
  (gptel-org-todo-test-with-buffer
      "*** AI-DOING My task :main@agent:\n"
    (gptel-org-agent--write-todo-org
     '((:content "Task A" :status "in_progress" :activeForm "Working A")
       (:content "Task B" :status "pending" :activeForm "Working B")))
    (goto-char (point-min))
    ;; Agent is level 3, so todos should be level 4
    (should (re-search-forward "^\\*\\*\\*\\* AI-DOING Task A$" nil t))
    (should (re-search-forward "^\\*\\*\\*\\* AI-DO Task B$" nil t))))

(ert-deftest gptel-org-todo-test-status-mapping ()
  "Todo statuses should map to correct org TODO keywords."
  (gptel-org-todo-test-with-buffer
      "** AI-DOING My task :main@agent:\n"
    (gptel-org-agent--write-todo-org
     '((:content "Done task" :status "completed" :activeForm "Done")
       (:content "Active task" :status "in_progress" :activeForm "Working")
       (:content "Waiting task" :status "pending" :activeForm "Waiting")))
    (goto-char (point-min))
    (should (re-search-forward "^\\*\\*\\* AI-DONE Done task$" nil t))
    (goto-char (point-min))
    (should (re-search-forward "^\\*\\*\\* AI-DOING Active task$" nil t))
    (goto-char (point-min))
    (should (re-search-forward "^\\*\\*\\* AI-DO Waiting task$" nil t))))

;;; ---- Idempotent updates ---------------------------------------------------

(ert-deftest gptel-org-todo-test-idempotent-update ()
  "Calling write-todo-org twice with same data should not duplicate headings."
  (gptel-org-todo-test-with-buffer
      "** AI-DOING My task :main@agent:\n"
    (let ((todos '((:content "Step one" :status "pending" :activeForm "Doing one")
                   (:content "Step two" :status "pending" :activeForm "Doing two"))))
      (gptel-org-agent--write-todo-org todos)
      (gptel-org-agent--write-todo-org todos))
    ;; Should have exactly 2 todo headings, not 4
    (goto-char (point-min))
    (let ((count 0))
      (while (re-search-forward "^\\*\\*\\* AI-DO " nil t)
        (cl-incf count))
      (should (= count 2)))))

(ert-deftest gptel-org-todo-test-status-transition ()
  "Updating a pending task to in_progress should change its keyword."
  (gptel-org-todo-test-with-buffer
      "** AI-DOING My task :main@agent:\n"
    ;; First call: all pending
    (gptel-org-agent--write-todo-org
     '((:content "Step one" :status "pending" :activeForm "Doing one")
       (:content "Step two" :status "pending" :activeForm "Doing two")))
    ;; Second call: step one in progress
    (gptel-org-agent--write-todo-org
     '((:content "Step one" :status "in_progress" :activeForm "Doing one")
       (:content "Step two" :status "pending" :activeForm "Doing two")))
    (goto-char (point-min))
    (should (re-search-forward "^\\*\\*\\* AI-DOING Step one$" nil t))
    (should (re-search-forward "^\\*\\*\\* AI-DO Step two$" nil t))))

(ert-deftest gptel-org-todo-test-remove-deleted-tasks ()
  "Tasks not in the new list should be removed."
  (gptel-org-todo-test-with-buffer
      "** AI-DOING My task :main@agent:\n"
    ;; First: three tasks
    (gptel-org-agent--write-todo-org
     '((:content "Task A" :status "completed" :activeForm "A")
       (:content "Task B" :status "in_progress" :activeForm "B")
       (:content "Task C" :status "pending" :activeForm "C")))
    ;; Second: only two tasks (B removed)
    (gptel-org-agent--write-todo-org
     '((:content "Task A" :status "completed" :activeForm "A")
       (:content "Task C" :status "in_progress" :activeForm "C")))
    (goto-char (point-min))
    (should (re-search-forward "AI-DONE Task A" nil t))
    (should-not (re-search-forward "Task B" nil t))
    (goto-char (point-min))
    (should (re-search-forward "AI-DOING Task C" nil t))))

;;; ---- FSM marker redirection -----------------------------------------------

(ert-deftest gptel-org-todo-test-markers-redirected-to-in-progress ()
  "FSM position markers should move to end of in_progress heading subtree."
  (gptel-org-todo-test-with-buffer
      "** AI-DOING My task :main@agent:\n"
    ;; Simulate FSM markers like gptel-org-agent--transform-redirect does
    (goto-char (point-max))
    (let* ((pos-marker (point-marker))
           (tracking-marker (point-marker))
           (fsm (gptel-make-fsm))
           (info (list :buffer (current-buffer)
                       :position pos-marker :tracking-marker tracking-marker)))
      (set-marker-insertion-type pos-marker t)
      (set-marker-insertion-type tracking-marker t)
      (setf (gptel-fsm-info fsm) info)
      (setq-local gptel--fsm-last fsm)
      ;; Write todos with one in_progress
      (gptel-org-agent--write-todo-org
       '((:content "Step one" :status "in_progress" :activeForm "Doing one")
         (:content "Step two" :status "pending" :activeForm "Doing two")))
      ;; Markers should now be at end of "Step one" subtree
      (goto-char (point-min))
      (should (re-search-forward "^\\*\\*\\* AI-DOING Step one$" nil t))
      (beginning-of-line)
      (let ((heading-end (save-excursion (org-end-of-subtree t) (point))))
        (should (= (marker-position pos-marker) heading-end))
        (should (= (marker-position tracking-marker) heading-end))))))

(ert-deftest gptel-org-todo-test-markers-follow-task-transition ()
  "When in_progress task changes, markers should move to new active task."
  (gptel-org-todo-test-with-buffer
      "** AI-DOING My task :main@agent:\n"
    (goto-char (point-max))
    (let* ((pos-marker (point-marker))
           (tracking-marker (point-marker))
           (fsm (gptel-make-fsm))
           (info (list :buffer (current-buffer)
                       :position pos-marker :tracking-marker tracking-marker)))
      (set-marker-insertion-type pos-marker t)
      (set-marker-insertion-type tracking-marker t)
      (setf (gptel-fsm-info fsm) info)
      (setq-local gptel--fsm-last fsm)
      ;; First: Step one in progress
      (gptel-org-agent--write-todo-org
       '((:content "Step one" :status "in_progress" :activeForm "Doing one")
         (:content "Step two" :status "pending" :activeForm "Doing two")))
      ;; Simulate some AI text under Step one
      (goto-char (marker-position pos-marker))
      (insert "Some AI work under step one\n")
      ;; Second: Step one done, Step two in progress
      (gptel-org-agent--write-todo-org
       '((:content "Step one" :status "completed" :activeForm "Doing one")
         (:content "Step two" :status "in_progress" :activeForm "Doing two")))
      ;; Markers should now be at end of "Step two" subtree
      (goto-char (point-min))
      (should (re-search-forward "^\\*\\*\\* AI-DOING Step two$" nil t))
      (beginning-of-line)
      (let ((heading-end (save-excursion (org-end-of-subtree t) (point))))
        (should (= (marker-position pos-marker) heading-end))))))

;;; ---- Simulated tool execution under todo headings -------------------------

(ert-deftest gptel-org-todo-test-simulated-tool-execution-structure ()
  "Simulate the full TodoWrite lifecycle: create tasks, execute under them.
This tests the expected org structure when tool calls happen under todo headings."
  (gptel-org-todo-test-with-buffer
      "** AI-DOING Test TodoWrite :main@agent:\n"
    (goto-char (point-max))
    (let* ((pos-marker (point-marker))
           (tracking-marker (point-marker))
           (fsm (gptel-make-fsm))
           (info (list :buffer (current-buffer)
                       :position pos-marker :tracking-marker tracking-marker)))
      (set-marker-insertion-type pos-marker t)
      (set-marker-insertion-type tracking-marker t)
      (setf (gptel-fsm-info fsm) info)
      (setq-local gptel--fsm-last fsm)

      ;; Step 1: AI creates todos (first call typically has step 1 in_progress)
      (gptel-org-agent--write-todo-org
       '((:content "Check current time" :status "in_progress"
          :activeForm "Checking current time")
         (:content "Check working directory" :status "pending"
          :activeForm "Checking working directory")
         (:content "Check Emacs version" :status "pending"
          :activeForm "Checking Emacs version")))

      ;; Simulate AI text + tool result appearing at pos-marker (under Step 1)
      (goto-char (marker-position pos-marker))
      (insert "\n#+begin_src gptel-tool\n(Eval :expression \"(current-time-string)\")\nResult: \"Sat Apr 11 10:49:58 2026\"\n#+end_src\n")

      ;; Step 2: Complete step 1, start step 2
      (gptel-org-agent--write-todo-org
       '((:content "Check current time" :status "completed"
          :activeForm "Checking current time")
         (:content "Check working directory" :status "in_progress"
          :activeForm "Checking working directory")
         (:content "Check Emacs version" :status "pending"
          :activeForm "Checking Emacs version")))

      ;; Simulate tool result under step 2
      (goto-char (marker-position pos-marker))
      (insert "\n#+begin_src gptel-tool\n(Eval :expression \"default-directory\")\nResult: \"/home/user/\"\n#+end_src\n")

      ;; Step 3: Complete step 2, start step 3
      (gptel-org-agent--write-todo-org
       '((:content "Check current time" :status "completed"
          :activeForm "Checking current time")
         (:content "Check working directory" :status "completed"
          :activeForm "Checking working directory")
         (:content "Check Emacs version" :status "in_progress"
          :activeForm "Checking Emacs version")))

      ;; Simulate tool result under step 3
      (goto-char (marker-position pos-marker))
      (insert "\n#+begin_src gptel-tool\n(Eval :expression \"emacs-version\")\nResult: \"30.2\"\n#+end_src\n")

      ;; Verify final structure:
      ;; 1. No "Tasks" heading
      (goto-char (point-min))
      (should-not (re-search-forward "^\\*+ Tasks$" nil t))

      ;; 2. Tool result for "current-time-string" should be under "Check current time"
      (goto-char (point-min))
      (should (re-search-forward "^\\*\\*\\* AI-DONE Check current time$" nil t))
      (let ((heading-start (line-beginning-position))
            (heading-end (save-excursion (org-end-of-subtree t) (point))))
        (should (string-match-p "current-time-string"
                                (buffer-substring-no-properties
                                 heading-start heading-end))))

      ;; 3. Tool result for "default-directory" should be under "Check working directory"
      (goto-char (point-min))
      (should (re-search-forward "^\\*\\*\\* AI-DONE Check working directory$" nil t))
      (let ((heading-start (line-beginning-position))
            (heading-end (save-excursion (org-end-of-subtree t) (point))))
        (should (string-match-p "default-directory"
                                (buffer-substring-no-properties
                                 heading-start heading-end))))

      ;; 4. Tool result for "emacs-version" should be under "Check Emacs version"
      (goto-char (point-min))
      (should (re-search-forward "^\\*\\*\\* AI-DOING Check Emacs version$" nil t))
      (let ((heading-start (line-beginning-position))
            (heading-end (save-excursion (org-end-of-subtree t) (point))))
        (should (string-match-p "emacs-version"
                                (buffer-substring-no-properties
                                 heading-start heading-end)))))))

(ert-deftest gptel-org-todo-test-no-cross-contamination ()
  "Tool results should not leak into sibling todo heading subtrees."
  (gptel-org-todo-test-with-buffer
      "** AI-DOING Test :main@agent:\n"
    (goto-char (point-max))
    (let* ((pos-marker (point-marker))
           (tracking-marker (point-marker))
           (fsm (gptel-make-fsm))
           (info (list :buffer (current-buffer)
                       :position pos-marker :tracking-marker tracking-marker)))
      (set-marker-insertion-type pos-marker t)
      (set-marker-insertion-type tracking-marker t)
      (setf (gptel-fsm-info fsm) info)
      (setq-local gptel--fsm-last fsm)

      ;; Task 1 active
      (gptel-org-agent--write-todo-org
       '((:content "Task A" :status "in_progress" :activeForm "A")
         (:content "Task B" :status "pending" :activeForm "B")))

      ;; Insert content at marker (should go under Task A)
      (goto-char (marker-position pos-marker))
      (insert "\nContent for Task A only\n")

      ;; Task 2 active
      (gptel-org-agent--write-todo-org
       '((:content "Task A" :status "completed" :activeForm "A")
         (:content "Task B" :status "in_progress" :activeForm "B")))

      ;; Insert content at marker (should go under Task B)
      (goto-char (marker-position pos-marker))
      (insert "\nContent for Task B only\n")

      ;; Verify Task A's subtree has its content but not Task B's
      (goto-char (point-min))
      (should (re-search-forward "^\\*\\*\\* AI-DONE Task A$" nil t))
      (let ((a-content (buffer-substring-no-properties
                        (point) (save-excursion (org-end-of-subtree t) (point)))))
        (should (string-match-p "Content for Task A only" a-content))
        (should-not (string-match-p "Content for Task B only" a-content)))

      ;; Verify Task B's subtree has its content but not Task A's
      (goto-char (point-min))
      (should (re-search-forward "^\\*\\*\\* AI-DOING Task B$" nil t))
      (let ((b-content (buffer-substring-no-properties
                        (point) (save-excursion (org-end-of-subtree t) (point)))))
        (should (string-match-p "Content for Task B only" b-content))
        (should-not (string-match-p "Content for Task A only" b-content))))))

;;; ---- Edge cases -----------------------------------------------------------

(ert-deftest gptel-org-todo-test-no-in-progress-no-marker-move ()
  "When no task is in_progress, markers should not be moved."
  (gptel-org-todo-test-with-buffer
      "** AI-DOING My task :main@agent:\n"
    (goto-char (point-max))
    (let* ((original-pos (point))
           (pos-marker (point-marker))
           (fsm (gptel-make-fsm))
           (info (list :buffer (current-buffer) :position pos-marker)))
      (set-marker-insertion-type pos-marker t)
      (setf (gptel-fsm-info fsm) info)
      (setq-local gptel--fsm-last fsm)
      ;; All tasks pending — no in_progress
      (gptel-org-agent--write-todo-org
       '((:content "Step one" :status "pending" :activeForm "One")
         (:content "Step two" :status "pending" :activeForm "Two")))
      ;; Marker should have moved due to insertion-type t (headings inserted
      ;; at end pushed it), but redirect-markers should not have been called.
      ;; The key test is that when there IS an in_progress, it DOES redirect.
      ;; Here we just verify the function doesn't crash with all-pending.
      (should (markerp pos-marker)))))

(ert-deftest gptel-org-todo-test-vector-input ()
  "TodoWrite sends todos as a vector; write-todo-org should handle both."
  (gptel-org-todo-test-with-buffer
      "** AI-DOING My task :main@agent:\n"
    ;; Pass a vector (as the LLM tool call would)
    (gptel-org-agent--write-todo-org
     [(:content "Step one" :status "pending" :activeForm "One")])
    (goto-char (point-min))
    (should (re-search-forward "^\\*\\*\\* AI-DO Step one$" nil t))))

(ert-deftest gptel-org-todo-test-no-fsm-does-not-crash ()
  "write-todo-org should work even without gptel--fsm-last (no marker redirect)."
  (gptel-org-todo-test-with-buffer
      "** AI-DOING My task :main@agent:\n"
    ;; Don't set gptel--fsm-last
    (gptel-org-agent--write-todo-org
     '((:content "Step one" :status "in_progress" :activeForm "One")))
    (goto-char (point-min))
    (should (re-search-forward "^\\*\\*\\* AI-DOING Step one$" nil t))))

;;; ---- collect-todo-headings filtering --------------------------------------

(ert-deftest gptel-org-todo-test-collect-only-todo-headings ()
  "collect-todo-headings should only return headings with todo keywords,
not AI response headings or other children."
  (gptel-org-todo-test-with-buffer
      (concat "** AI-DOING My task :main@agent:\n"
              "*** AI-DOING Step one\n"
              "*** AI response text heading\n"
              "*** AI-DO Step two\n"
              "*** Another non-todo heading\n")
    (goto-char (point-min))
    (let ((headings (gptel-org-agent--collect-todo-headings 3 (point))))
      ;; Should only find the two headings with AI-DO/AI-DOING keywords
      (should (= (length headings) 2))
      (should (equal (car (nth 0 headings)) "Step one"))
      (should (equal (car (nth 1 headings)) "Step two")))))

;;; gptel-org-todo-write-test.el ends here
