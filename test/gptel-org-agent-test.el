;;; gptel-org-agent-test.el --- Tests for gptel-org-agent  -*- lexical-binding: t; -*-

;; Tests for the gptel-org-agent module which provides:
;; - Agent tag predicates and constructors
;; - Agent subtree creation and discovery
;; - Indirect buffer management (open/close/narrow)
;; - Integration: maybe-setup-subtree for TODO headings

(require 'ert)
(require 'gptel)
(require 'gptel-org)
(require 'gptel-org-agent)

;;; Helper macros

(defmacro gptel-org-agent-test-with-buffer (content &rest body)
  "Create a temp org buffer with CONTENT and execute BODY.
Sets up org-mode with AI-DO/AI-DOING/AI-DONE keywords and
`gptel-org-todo-keywords'.  Point is placed at the first @
character if present, otherwise at end."
  (declare (indent 1))
  `(let ((org-inhibit-startup t)
         (org-todo-keywords '((sequence "AI-DO" "AI-DOING" "|" "AI-DONE")))
         (gptel-org-todo-keywords '("AI-DO" "AI-DOING")))
     (with-temp-buffer
       (delay-mode-hooks (org-mode))
       (insert ,content)
       (goto-char (point-min))
       (unless (search-forward "@" nil t)
         (goto-char (point-max)))
       (when (looking-back "@" 1)
         (backward-char))
       ,@body)))


;;; ---- Helper function tests ------------------------------------------------

(ert-deftest gptel-org-agent-test-agent-tag-p ()
  "Test that agent-tag-p recognizes the *@agent pattern."
  ;; Valid agent tags
  (should (gptel-org-agent--agent-tag-p "main@agent"))
  (should (gptel-org-agent--agent-tag-p "researcher@main@agent"))
  (should (gptel-org-agent--agent-tag-p "gatherer@researcher@main@agent"))
  ;; Invalid inputs
  (should-not (gptel-org-agent--agent-tag-p "agent"))
  (should-not (gptel-org-agent--agent-tag-p "main"))
  (should-not (gptel-org-agent--agent-tag-p ""))
  (should-not (gptel-org-agent--agent-tag-p nil))
  ;; Edge: must *end* with @agent
  (should-not (gptel-org-agent--agent-tag-p "main@agent@extra")))

(ert-deftest gptel-org-agent-test-construct-tag ()
  "Test tag construction with and without parent-tag."
  ;; Without parent-tag: agent-type@agent
  (should (equal "main@agent"
                 (gptel-org-agent--construct-tag "main")))
  (should (equal "researcher@agent"
                 (gptel-org-agent--construct-tag "researcher")))
  ;; With parent-tag: agent-type@parent-tag
  (should (equal "researcher@main@agent"
                 (gptel-org-agent--construct-tag "researcher" "main@agent")))
  (should (equal "gatherer@main@agent"
                 (gptel-org-agent--construct-tag "gatherer" "main@agent"))))

(ert-deftest gptel-org-agent-test-construct-tag-nested ()
  "Test deeply nested tag construction."
  ;; Level 1: main@agent
  (let* ((tag1 (gptel-org-agent--construct-tag "main"))
         ;; Level 2: researcher@main@agent
         (tag2 (gptel-org-agent--construct-tag "researcher" tag1))
         ;; Level 3: researcher@researcher@main@agent
         (tag3 (gptel-org-agent--construct-tag "researcher" tag2)))
    (should (equal "main@agent" tag1))
    (should (equal "researcher@main@agent" tag2))
    (should (equal "researcher@researcher@main@agent" tag3))
    ;; All should be valid agent tags
    (should (gptel-org-agent--agent-tag-p tag1))
    (should (gptel-org-agent--agent-tag-p tag2))
    (should (gptel-org-agent--agent-tag-p tag3))))


;;; ---- Create subtree tests ------------------------------------------------

(ert-deftest gptel-org-agent-test-create-subtree-basic ()
  "Create a main@agent subtree under a TODO heading.
Verify the child heading exists at the correct level with the right tag."
  (gptel-org-agent-test-with-buffer
   "* AI-DO Implement feature\nDescription\n"
   (goto-char (point-min))
   (org-back-to-heading t)
   (let ((marker (gptel-org-agent--create-subtree "main")))
     (should (markerp marker))
     ;; Go to the created heading
     (goto-char marker)
     (should (org-at-heading-p))
     ;; Should be level 2 (child of level 1)
     (should (= 2 (org-current-level)))
     ;; Should have the main@agent tag
     (let ((tags (org-get-tags nil t)))
       (should (cl-some (lambda (tg)
                          (string-equal-ignore-case tg "main@agent"))
                        tags))))))

(ert-deftest gptel-org-agent-test-create-subtree-nested ()
  "Create a sub-agent subtree (researcher) under an existing main@agent heading."
  (gptel-org-agent-test-with-buffer
   "* AI-DO Task\n** :main@agent:\nSome agent content\n"
   (goto-char (point-min))
   ;; Position on the main@agent heading
   (search-forward "** ")
   (beginning-of-line)
   (should (org-at-heading-p))
   (let ((marker (gptel-org-agent--create-subtree "researcher" "main@agent")))
     (should (markerp marker))
     (goto-char marker)
     (should (org-at-heading-p))
     ;; Should be level 3 (child of level 2)
     (should (= 3 (org-current-level)))
     ;; Should have the researcher@main@agent tag
     (let ((tags (org-get-tags nil t)))
       (should (cl-some (lambda (tg)
                          (string-equal-ignore-case tg "researcher@main@agent"))
                        tags))))))

(ert-deftest gptel-org-agent-test-create-subtree-preserves-point ()
  "Verify that create-subtree uses save-excursion — point should not move."
  (gptel-org-agent-test-with-buffer
   "* AI-DO Task\nDescription\n"
   (goto-char (point-min))
   (org-back-to-heading t)
   (let ((original-point (point)))
     (gptel-org-agent--create-subtree "main")
     (should (= original-point (point))))))


;;; ---- Find subtree tests ---------------------------------------------------

(ert-deftest gptel-org-agent-test-find-existing ()
  "Create a subtree then find it via find-agent-subtree."
  (gptel-org-agent-test-with-buffer
   "* AI-DO Task\nDescription\n"
   (goto-char (point-min))
   (org-back-to-heading t)
   ;; Create the subtree first
   (let ((created-marker (gptel-org-agent--create-subtree "main")))
     ;; Go back to parent heading and search
     (goto-char (point-min))
     (org-back-to-heading t)
     (let ((found-marker (gptel-org-agent--find-agent-subtree "main@agent")))
       (should (markerp found-marker))
       (should (= (marker-position created-marker)
                  (marker-position found-marker)))))))

(ert-deftest gptel-org-agent-test-find-missing ()
  "Search for a non-existent agent subtree returns nil."
  (gptel-org-agent-test-with-buffer
   "* AI-DO Task\n** @user\nQuestion\n"
   (goto-char (point-min))
   (org-back-to-heading t)
   (should-not (gptel-org-agent--find-agent-subtree "main@agent"))))

(ert-deftest gptel-org-agent-test-find-ignores-deeper ()
  "Only direct children match, not grandchildren."
  (gptel-org-agent-test-with-buffer
   "* AI-DO Task\n** Some child\n*** :main@agent:\nDeep content\n"
   (goto-char (point-min))
   (org-back-to-heading t)
   ;; main@agent is at level 3 (grandchild of level 1), should not match
   (should-not (gptel-org-agent--find-agent-subtree "main@agent"))))


;;; ---- Indirect buffer tests ------------------------------------------------

(ert-deftest gptel-org-agent-test-open-indirect-buffer ()
  "Open an indirect buffer and verify it is narrowed to the subtree."
  (gptel-org-agent-test-with-buffer
   "* AI-DO Task\nDescription\n"
   (goto-char (point-min))
   (org-back-to-heading t)
   (let* ((base-buf (current-buffer))
          (marker (gptel-org-agent--create-subtree "main"))
          (indirect-buf nil))
     (unwind-protect
         (progn
           (setq indirect-buf
                 (gptel-org-agent--open-indirect-buffer base-buf marker))
           ;; Should be a live buffer
           (should (buffer-live-p indirect-buf))
           ;; Should be an indirect buffer of the base
           (should (eq (buffer-base-buffer indirect-buf) base-buf))
           ;; Content of the narrowed region should contain main@agent
           (with-current-buffer indirect-buf
             (let ((content (buffer-substring-no-properties
                             (point-min) (point-max))))
               (should (string-match-p "main@agent" content))))
           ;; Buffer should NOT contain "AI-DO Task" heading text within
           ;; the narrowed region — that's the parent
           (with-current-buffer indirect-buf
             (let ((content (buffer-substring-no-properties
                             (point-min) (point-max))))
               (should-not (string-match-p "AI-DO Task" content)))))
       ;; Cleanup
       (when (and indirect-buf (buffer-live-p indirect-buf))
         (kill-buffer indirect-buf))))))

(ert-deftest gptel-org-agent-test-open-indirect-buffer-seeds-termine ()
  "`gptel-org-agent--open-indirect-buffer' produces an IB with TERMINE.

TERMINE seeding is the responsibility of the generic factory
`gptel-org-ib-create' (universal invariant — every IB has TERMINE
as its last child, see gptel-indirect-buffer-ai.org *** TERMINE).
Agent IBs inherit TERMINE from the factory so streaming insertion
lands BEFORE it, preserving the \"TERMINE is last child\" invariant
and sibling-IB isolation."
  (gptel-org-agent-test-with-buffer
   "* AI-DO Task\nDescription\n"
   (goto-char (point-min))
   (org-back-to-heading t)
   (let* ((base-buf (current-buffer))
          (marker (gptel-org-agent--create-subtree "main"))
          (indirect-buf nil))
     (unwind-protect
         (progn
           (setq indirect-buf
                 (gptel-org-agent--open-indirect-buffer base-buf marker))
           (should (buffer-live-p indirect-buf))
           (with-current-buffer indirect-buf
             ;; TERMINE must exist — seeded by the agent factory.
             (should (gptel-org-ib-find-terminator "TERMINE"))
             ;; Streaming marker must take the TERMINE branch:
             ;; insertion-type=nil pinned at TERMINE line start.
             (let ((m (gptel-org-ib-streaming-marker "TERMINE")))
               (should (markerp m))
               (should-not (marker-insertion-type m)))))
       ;; Cleanup
       (when (and indirect-buf (buffer-live-p indirect-buf))
         (kill-buffer indirect-buf))))))

(ert-deftest gptel-org-agent-test-indirect-buffer-grows ()
  "Insert text at end of indirect buffer and verify narrow region expands."
  (gptel-org-agent-test-with-buffer
   "* AI-DO Task\nDescription\n"
   (goto-char (point-min))
   (org-back-to-heading t)
   (let* ((base-buf (current-buffer))
          (marker (gptel-org-agent--create-subtree "main"))
          (indirect-buf nil))
     (unwind-protect
         (progn
           (setq indirect-buf
                 (gptel-org-agent--open-indirect-buffer base-buf marker))
           (let ((initial-size (with-current-buffer indirect-buf
                                 (- (point-max) (point-min)))))
             ;; Insert text at the end of the indirect buffer
             (with-current-buffer indirect-buf
               (goto-char (point-max))
               (insert "New agent response content\n"))
             ;; The narrow region should have grown
             (let ((new-size (with-current-buffer indirect-buf
                               (- (point-max) (point-min)))))
               (should (> new-size initial-size)))
             ;; The inserted text should be visible in the indirect buffer
             (with-current-buffer indirect-buf
               (let ((content (buffer-substring-no-properties
                               (point-min) (point-max))))
                 (should (string-match-p "New agent response content"
                                         content))))))
       ;; Cleanup
       (when (and indirect-buf (buffer-live-p indirect-buf))
         (kill-buffer indirect-buf))))))

(ert-deftest gptel-org-agent-test-close-indirect-buffer ()
  "Close indirect buffer and verify it is killed and markers cleaned up."
  (gptel-org-agent-test-with-buffer
   "* AI-DO Task\nDescription\n"
   (goto-char (point-min))
   (org-back-to-heading t)
   (let* ((base-buf (current-buffer))
          (marker (gptel-org-agent--create-subtree "main"))
          (indirect-buf (gptel-org-agent--open-indirect-buffer base-buf marker))
          (end-marker (buffer-local-value
                       'gptel-org-agent--narrow-end-marker
                       indirect-buf)))
     ;; Pre-condition: buffer is alive
     (should (buffer-live-p indirect-buf))
     (should (markerp end-marker))
     ;; Close the buffer
     (gptel-org-agent--close-indirect-buffer indirect-buf)
     ;; Buffer should be killed
     (should-not (buffer-live-p indirect-buf))
     ;; End marker should be cleaned up (position set to nil)
     (should-not (marker-position end-marker)))))

(ert-deftest gptel-org-agent-test-close-with-fold ()
  "Close with fold=t and verify the subtree is folded in the base buffer."
  (gptel-org-agent-test-with-buffer
   "* AI-DO Task\nDescription\n"
   (goto-char (point-min))
   (org-back-to-heading t)
   (let* ((base-buf (current-buffer))
          (marker (gptel-org-agent--create-subtree "main"))
          (indirect-buf nil))
     ;; Insert some content so folding is meaningful
     (goto-char marker)
     (org-end-of-subtree t)
     (unless (bolp) (insert "\n"))
     (insert "Agent response line 1\nAgent response line 2\n")
     ;; Open and close with fold
     (setq indirect-buf
           (gptel-org-agent--open-indirect-buffer base-buf marker))
     (gptel-org-agent--close-indirect-buffer indirect-buf t)
     ;; The subtree at the marker should be folded in the base buffer
     (with-current-buffer base-buf
       (goto-char (marker-position marker))
       (should (org-at-heading-p))
       ;; Check that the subtree body is invisible (folded)
       (end-of-line)
       (should (org-fold-folded-p (point)))))))


;;; ---- Integration tests ----------------------------------------------------

(ert-deftest gptel-org-agent-test-maybe-setup-on-todo ()
  "Test that maybe-setup-subtree works on an AI-DO heading."
  (gptel-org-agent-test-with-buffer
   "* AI-DO Implement feature\nDescription\n"
   (let ((gptel-org-subtree-context t)
         (indirect-buf nil))
     (goto-char (point-min))
     (org-back-to-heading t)
     (unwind-protect
         (progn
           (setq indirect-buf (gptel-org-agent--maybe-setup-subtree))
           ;; Should return an indirect buffer
           (should indirect-buf)
           (should (buffer-live-p indirect-buf))
           (should (buffer-base-buffer indirect-buf))
           ;; The indirect buffer should contain main@agent
           (with-current-buffer indirect-buf
             (let ((content (buffer-substring-no-properties
                             (point-min) (point-max))))
               (should (string-match-p "main@agent" content)))))
       ;; Cleanup
       (when (and indirect-buf (buffer-live-p indirect-buf))
         (kill-buffer indirect-buf))))))

(ert-deftest gptel-org-agent-test-maybe-setup-disabled ()
  "Test that nil gptel-org-subtree-context returns nil."
  (gptel-org-agent-test-with-buffer
   "* AI-DO Implement feature\nDescription\n"
   (let ((gptel-org-subtree-context nil))
     (goto-char (point-min))
     (org-back-to-heading t)
     (should-not (gptel-org-agent--maybe-setup-subtree)))))

(ert-deftest gptel-org-agent-test-maybe-setup-non-todo ()
  "Test that a regular heading (no TODO keyword) returns nil."
  (gptel-org-agent-test-with-buffer
   "* Regular heading\nDescription\n"
   (let ((gptel-org-subtree-context t))
     (goto-char (point-min))
     (org-back-to-heading t)
     (should-not (gptel-org-agent--maybe-setup-subtree)))))

(ert-deftest gptel-org-agent-test-maybe-setup-reuses ()
  "Test that calling maybe-setup-subtree twice reuses the existing subtree."
  (gptel-org-agent-test-with-buffer
   "* AI-DO Implement feature\nDescription\n"
   (let ((gptel-org-subtree-context t)
         (indirect-buf-1 nil)
         (indirect-buf-2 nil))
     (unwind-protect
         (progn
           ;; First call: creates subtree
           (goto-char (point-min))
           (org-back-to-heading t)
           (setq indirect-buf-1 (gptel-org-agent--maybe-setup-subtree))
           (should indirect-buf-1)
           ;; Count how many main@agent headings exist
           (let ((count 0))
             (goto-char (point-min))
             (while (re-search-forward "main@agent" nil t)
               (setq count (1+ count)))
             ;; Should have exactly 1 main@agent tag in the buffer
             ;; (appearing once in the heading tag)
             (should (>= count 1)))
           ;; Kill the first indirect buffer to avoid name clash
           (kill-buffer indirect-buf-1)
           (setq indirect-buf-1 nil)
           ;; Second call: should reuse existing subtree, not create a new one
           (goto-char (point-min))
           (org-back-to-heading t)
           (setq indirect-buf-2 (gptel-org-agent--maybe-setup-subtree))
           (should indirect-buf-2)
           ;; Still should have exactly the same number of main@agent headings
           (let ((count 0))
             (goto-char (point-min))
             (while (re-search-forward "main@agent" nil t)
               (setq count (1+ count)))
             ;; If reused, count should not increase.  The heading's tag
             ;; appears once.
             (should (>= count 1))
             ;; Crucially: should NOT have 2 separate agent headings
             (goto-char (point-min))
             (let ((heading-count 0))
               (while (re-search-forward "^\\*+ .+:main@agent:" nil t)
                 (setq heading-count (1+ heading-count)))
               (should (= 1 heading-count)))))
       ;; Cleanup
       (when (and indirect-buf-1 (buffer-live-p indirect-buf-1))
         (kill-buffer indirect-buf-1))
       (when (and indirect-buf-2 (buffer-live-p indirect-buf-2))
         (kill-buffer indirect-buf-2))))))


;;; ---- Base buffer visibility test ------------------------------------------

(ert-deftest gptel-org-agent-test-base-buffer-has-full-tree ()
  "After creating subtree and inserting text via indirect buffer,
verify the base buffer shows the full tree structure."
  (gptel-org-agent-test-with-buffer
   "* AI-DO Implement feature\nDescription\n"
   (goto-char (point-min))
   (org-back-to-heading t)
   (let* ((base-buf (current-buffer))
          (marker (gptel-org-agent--create-subtree "main"))
          (indirect-buf nil))
     (unwind-protect
         (progn
           (setq indirect-buf
                 (gptel-org-agent--open-indirect-buffer base-buf marker))
           ;; Insert content via the indirect buffer (simulating LLM response)
           (with-current-buffer indirect-buf
             (goto-char (point-max))
             (insert "** @user\nHow do I implement X?\n")
             (insert "** @assistant\nHere is the answer.\n"))
           ;; Now verify the base buffer contains the full structure
           (with-current-buffer base-buf
             (let ((content (buffer-substring-no-properties
                             (point-min) (point-max))))
               ;; Original heading
               (should (string-match-p "AI-DO Implement feature" content))
               ;; Agent subtree heading
               (should (string-match-p "main@agent" content))
               ;; Content inserted via indirect buffer
               (should (string-match-p "@user" content))
               (should (string-match-p "How do I implement X?" content))
               (should (string-match-p "@assistant" content))
               (should (string-match-p "Here is the answer." content)))))
       ;; Cleanup
       (when (and indirect-buf (buffer-live-p indirect-buf))
         (kill-buffer indirect-buf))))))


;;; Phase 2 tests

;;; ---- current-agent-tag tests ----------------------------------------------

(ert-deftest gptel-org-agent-test-current-agent-tag-in-agent-buffer ()
  "In an agent indirect buffer, current-agent-tag returns the agent tag."
  (gptel-org-agent-test-with-buffer
   "* AI-DO Implement feature\nDescription\n"
   (goto-char (point-min))
   (org-back-to-heading t)
   (let* ((gptel-org-subtree-context t)
          (base-buf (current-buffer))
          (marker (gptel-org-agent--create-subtree "main"))
          (indirect-buf nil))
     (unwind-protect
         (progn
           (setq indirect-buf
                 (gptel-org-agent--open-indirect-buffer base-buf marker))
           (with-current-buffer indirect-buf
             (should (equal "main@agent"
                            (gptel-org-agent--current-agent-tag)))))
       (when (and indirect-buf (buffer-live-p indirect-buf))
         (kill-buffer indirect-buf))))))

(ert-deftest gptel-org-agent-test-current-agent-tag-in-base-buffer ()
  "In a regular org buffer, current-agent-tag returns nil."
  (gptel-org-agent-test-with-buffer
   "* AI-DO Implement feature\nDescription\n"
   (goto-char (point-min))
   (should-not (gptel-org-agent--current-agent-tag))))


;;; ---- setup-task-subtree tests ---------------------------------------------

(ert-deftest gptel-org-agent-test-setup-task-subtree-basic ()
  "Setup-task-subtree creates a sub-agent subtree and returns a plist."
  (gptel-org-agent-test-with-buffer
   "* AI-DO Implement feature\nDescription\n"
   (goto-char (point-min))
   (org-back-to-heading t)
   (let* ((gptel-org-subtree-context t)
          (base-buf (current-buffer))
          (main-marker (gptel-org-agent--create-subtree "main"))
          (main-indirect nil)
          (result nil)
          (sub-indirect nil))
     (unwind-protect
         (progn
           (setq main-indirect
                 (gptel-org-agent--open-indirect-buffer base-buf main-marker))
           ;; Call setup-task-subtree from within the main@agent indirect buffer
           (with-current-buffer main-indirect
             (setq result
                   (gptel-org-agent--setup-task-subtree "researcher" "Find code")))
           ;; Should return a plist
           (should result)
           (setq sub-indirect (plist-get result :indirect-buffer))
           ;; :indirect-buffer should be live
           (should (buffer-live-p sub-indirect))
           ;; :heading-marker should point to a heading with researcher@main@agent
           (let ((hm (plist-get result :heading-marker)))
             (should (markerp hm))
             (with-current-buffer base-buf
               (save-excursion
                 (goto-char hm)
                 (should (org-at-heading-p))
                 (let ((tags (org-get-tags nil t)))
                   (should (cl-some
                            (lambda (tg)
                              (string-equal-ignore-case
                               tg "researcher@main@agent"))
                            tags))))))
           ;; :position-marker should be in the sub-agent indirect buffer
           (let ((pm (plist-get result :position-marker)))
             (should (markerp pm))
             (should (eq (marker-buffer pm) sub-indirect)))
           ;; :parent-tag should be "main@agent"
           (should (equal "main@agent" (plist-get result :parent-tag))))
       ;; Cleanup
       (when (and sub-indirect (buffer-live-p sub-indirect))
         (kill-buffer sub-indirect))
       (when (and main-indirect (buffer-live-p main-indirect))
         (kill-buffer main-indirect))))))

(ert-deftest gptel-org-agent-test-setup-task-subtree-disabled ()
  "With gptel-org-subtree-context nil, setup-task-subtree returns nil."
  (gptel-org-agent-test-with-buffer
   "* AI-DO Implement feature\nDescription\n"
   (goto-char (point-min))
   (org-back-to-heading t)
   (let* ((gptel-org-subtree-context nil)
          (base-buf (current-buffer))
          (main-marker (gptel-org-agent--create-subtree "main"))
          (main-indirect nil))
     (unwind-protect
         (progn
           (setq main-indirect
                 (gptel-org-agent--open-indirect-buffer base-buf main-marker))
           (with-current-buffer main-indirect
             (should-not
              (gptel-org-agent--setup-task-subtree "researcher" "Find code"))))
       (when (and main-indirect (buffer-live-p main-indirect))
         (kill-buffer main-indirect))))))

(ert-deftest gptel-org-agent-test-setup-task-subtree-reuses ()
  "Setup-task-subtree reuses an existing sub-agent subtree."
  (gptel-org-agent-test-with-buffer
   "* AI-DO Implement feature\nDescription\n"
   (goto-char (point-min))
   (org-back-to-heading t)
   (let* ((gptel-org-subtree-context t)
          (base-buf (current-buffer))
          (main-marker (gptel-org-agent--create-subtree "main"))
          (main-indirect nil)
          (result-1 nil)
          (result-2 nil)
          (sub-indirect-1 nil)
          (sub-indirect-2 nil))
     (unwind-protect
         (progn
           (setq main-indirect
                 (gptel-org-agent--open-indirect-buffer base-buf main-marker))
           ;; First call: creates the researcher subtree
           (with-current-buffer main-indirect
             (setq result-1
                   (gptel-org-agent--setup-task-subtree "researcher" "Find code")))
           (should result-1)
           (setq sub-indirect-1 (plist-get result-1 :indirect-buffer))
           ;; Kill the sub-indirect buffer so the second call can reopen it
           (when (buffer-live-p sub-indirect-1)
             (kill-buffer sub-indirect-1))
           (setq sub-indirect-1 nil)
           ;; Second call: should reuse the existing researcher subtree
           (with-current-buffer main-indirect
             (setq result-2
                   (gptel-org-agent--setup-task-subtree "researcher" "Find code")))
           (should result-2)
           (setq sub-indirect-2 (plist-get result-2 :indirect-buffer))
           ;; Verify only one researcher@main@agent heading exists
           (with-current-buffer base-buf
             (let ((count 0))
               (save-excursion
                 (goto-char (point-min))
                 (while (re-search-forward
                         "^\\*+ .+:researcher@main@agent:" nil t)
                   (setq count (1+ count))))
               (should (= 1 count)))))
       ;; Cleanup
       (when (and sub-indirect-1 (buffer-live-p sub-indirect-1))
         (kill-buffer sub-indirect-1))
       (when (and sub-indirect-2 (buffer-live-p sub-indirect-2))
         (kill-buffer sub-indirect-2))
       (when (and main-indirect (buffer-live-p main-indirect))
         (kill-buffer main-indirect))))))


;;; ---- extract-final-text tests ---------------------------------------------

(ert-deftest gptel-org-agent-test-extract-final-text-with-properties ()
  "Extract text that has the gptel response text property."
  (gptel-org-agent-test-with-buffer
   "* AI-DO Implement feature\nDescription\n"
   (goto-char (point-min))
   (org-back-to-heading t)
   (let* ((base-buf (current-buffer))
          (marker (gptel-org-agent--create-subtree "main"))
          (indirect-buf nil))
     (unwind-protect
         (progn
           (setq indirect-buf
                 (gptel-org-agent--open-indirect-buffer base-buf marker))
           (with-current-buffer indirect-buf
             (goto-char (point-max))
             ;; Insert some non-response text
             (insert "User prompt text\n")
             ;; Insert response text with gptel response property
             (let ((start (point)))
               (insert "Here is the LLM response.\n")
               (add-text-properties start (point)
                                    '(gptel response front-sticky (gptel)))))
           (let ((result (gptel-org-agent--extract-final-text indirect-buf)))
             (should result)
             (should (string-match-p "Here is the LLM response" result))))
       (when (and indirect-buf (buffer-live-p indirect-buf))
         (kill-buffer indirect-buf))))))

(ert-deftest gptel-org-agent-test-extract-final-text-fallback ()
  "Extract plain text (no gptel properties) as fallback."
  (gptel-org-agent-test-with-buffer
   "* AI-DO Implement feature\nDescription\n"
   (goto-char (point-min))
   (org-back-to-heading t)
   (let* ((base-buf (current-buffer))
          (marker (gptel-org-agent--create-subtree "main"))
          (indirect-buf nil))
     (unwind-protect
         (progn
           (setq indirect-buf
                 (gptel-org-agent--open-indirect-buffer base-buf marker))
           (with-current-buffer indirect-buf
             ;; Insert before the pre-seeded TERMINE terminator so
             ;; the fallback strategy picks up the body text (it stops
             ;; at TERMINE, which is not response content).
             (goto-char (gptel-org-ib-streaming-marker "TERMINE"))
             ;; Insert a newline to start body text on a new line after
             ;; the heading, then insert plain text without gptel properties
             (insert "\nSome fallback text without properties\n"))
           (let ((result (gptel-org-agent--extract-final-text indirect-buf)))
             (should result)
             (should (string-match-p "Some fallback text without properties"
                                     result))))
       (when (and indirect-buf (buffer-live-p indirect-buf))
         (kill-buffer indirect-buf))))))

(ert-deftest gptel-org-agent-test-extract-final-text-empty ()
  "Extract returns nil when the buffer has only the heading and no body."
  (gptel-org-agent-test-with-buffer
   "* AI-DO Implement feature\nDescription\n"
   (goto-char (point-min))
   (org-back-to-heading t)
   (let* ((base-buf (current-buffer))
          (marker (gptel-org-agent--create-subtree "main"))
          (indirect-buf nil))
     (unwind-protect
         (progn
           (setq indirect-buf
                 (gptel-org-agent--open-indirect-buffer base-buf marker))
           ;; Don't insert anything — buffer has only the heading + newline
           (should-not (gptel-org-agent--extract-final-text indirect-buf)))
       (when (and indirect-buf (buffer-live-p indirect-buf))
         (kill-buffer indirect-buf))))))


;;; ---- Phase 5: Advisor agent integration tests -----------------------------

(ert-deftest gptel-org-agent-test-strip-agent-subtrees ()
  "Strip @agent subtrees from a buffer while preserving regular content."
  (gptel-org-agent-test-with-buffer
   "* AI-DO Implement feature
Some description
** :main@agent:
Agent conversation content
*** @user
User prompt here
*** @assistant
Agent response here
** Regular heading
Keep this content
"
   (goto-char (point-min))
   (gptel-org-agent--strip-agent-subtrees)
   (let ((content (buffer-substring-no-properties (point-min) (point-max))))
     ;; The @agent subtree should be gone
     (should-not (string-match-p "main@agent" content))
     (should-not (string-match-p "Agent conversation content" content))
     (should-not (string-match-p "User prompt here" content))
     (should-not (string-match-p "Agent response here" content))
     ;; Regular content should remain
     (should (string-match-p "AI-DO Implement feature" content))
     (should (string-match-p "Some description" content))
     (should (string-match-p "Regular heading" content))
     (should (string-match-p "Keep this content" content)))))

(ert-deftest gptel-org-agent-test-strip-preserves-regular ()
  "Stripping does nothing when there are no @agent subtrees."
  (gptel-org-agent-test-with-buffer
   "* AI-DO Implement feature
Some description
** @user
User question here
** @assistant
Assistant answer here
** Notes
Additional notes
"
   (goto-char (point-min))
   (let ((content-before (buffer-substring-no-properties
                          (point-min) (point-max))))
     (gptel-org-agent--strip-agent-subtrees)
     (let ((content-after (buffer-substring-no-properties
                           (point-min) (point-max))))
       (should (equal content-before content-after))))))

(ert-deftest gptel-org-agent-test-strip-nested-agents ()
  "Strip nested @agent subtrees (parent contains child agent)."
  (gptel-org-agent-test-with-buffer
   "* AI-DO Implement feature
Some description
** :main@agent:
Main agent content
*** :researcher@main@agent:
Researcher found this
*** :gatherer@main@agent:
Gatherer collected that
** Summary
Final summary here
"
   (goto-char (point-min))
   (gptel-org-agent--strip-agent-subtrees)
   (let ((content (buffer-substring-no-properties (point-min) (point-max))))
     ;; All agent subtrees should be removed
     (should-not (string-match-p "main@agent" content))
     (should-not (string-match-p "researcher@main@agent" content))
     (should-not (string-match-p "gatherer@main@agent" content))
     (should-not (string-match-p "Main agent content" content))
     (should-not (string-match-p "Researcher found this" content))
     (should-not (string-match-p "Gatherer collected that" content))
     ;; Non-agent content survives
     (should (string-match-p "AI-DO Implement feature" content))
     (should (string-match-p "Some description" content))
     (should (string-match-p "Summary" content))
     (should (string-match-p "Final summary here" content)))))

(ert-deftest gptel-org-agent-test-include-subtrees-flag ()
  "With `gptel-org-agent-include-subtrees' set, stripping is skipped.
The flag is checked by the caller, not by the strip function itself.
This test verifies the guard logic that would appear in
`gptel-org--create-prompt-buffer'."
  (gptel-org-agent-test-with-buffer
   "* AI-DO Implement feature
** :main@agent:
Agent content that should be preserved
"
   (goto-char (point-min))
   (let ((gptel-org-agent-include-subtrees t))
     ;; Simulate the guard: when include-subtrees is nil, call strip
     (unless gptel-org-agent-include-subtrees
       (gptel-org-agent--strip-agent-subtrees))
     ;; Agent content should still be present because strip was not called
     (let ((content (buffer-substring-no-properties (point-min) (point-max))))
       (should (string-match-p "main@agent" content))
       (should (string-match-p "Agent content that should be preserved"
                                content))))))

(ert-deftest gptel-org-agent-test-advisor-preset ()
  "Test that the advisor preset returns expected keys."
  (let ((preset (gptel-org-agent--advisor-preset)))
    (should (plist-get preset :include-agent-subtrees))
    (should (stringp (plist-get preset :system)))
    (should (string-match-p "Advisor" (plist-get preset :system)))))

(ert-deftest gptel-org-agent-test-strip-multiple-agent-subtrees ()
  "Strip multiple separate @agent subtrees at the same level."
  (gptel-org-agent-test-with-buffer
   "* AI-DO Complex task
Description
** :main@agent:
First agent run
** Regular notes
Some notes
** :main@agent:
Second agent run
** Conclusion
Final thoughts
"
   (goto-char (point-min))
   (gptel-org-agent--strip-agent-subtrees)
   (let ((content (buffer-substring-no-properties (point-min) (point-max))))
     ;; Both agent subtrees should be gone
     (should-not (string-match-p "main@agent" content))
     (should-not (string-match-p "First agent run" content))
     (should-not (string-match-p "Second agent run" content))
     ;; Non-agent content survives
     (should (string-match-p "AI-DO Complex task" content))
     (should (string-match-p "Regular notes" content))
     (should (string-match-p "Some notes" content))
     (should (string-match-p "Conclusion" content))
     (should (string-match-p "Final thoughts" content)))))

;;; ---- Transform redirect tests (Phase 1 integration) -----------------------

(ert-deftest gptel-org-agent-test-transform-redirect-on-todo ()
  "Transform redirects FSM to agent indirect buffer on TODO heading."
  (gptel-org-agent-test-with-buffer
   "* AI-DO Implement feature\nDescription\n"
   (goto-char (point-min))
   (org-back-to-heading t)
   (let* ((gptel-org-subtree-context t)
          (base-buf (current-buffer))
          (fsm (gptel-make-fsm))
          (info (list :buffer base-buf
                      :position (point-marker))))
     (setf (gptel-fsm-info fsm) info)
     (unwind-protect
         (progn
           (gptel-org-agent--transform-redirect fsm)
           (let ((new-buf (plist-get info :buffer))
                 (new-pos (plist-get info :position)))
             ;; Should have been redirected to an indirect buffer
             (should-not (eq new-buf base-buf))
             (should (buffer-live-p new-buf))
             (should (eq (buffer-base-buffer new-buf) base-buf))
             ;; Position marker should be in the indirect buffer
             (should (markerp new-pos))
             (should (eq (marker-buffer new-pos) new-buf))
             ;; Indirect buffer should contain main@agent
             (with-current-buffer new-buf
               (let ((content (buffer-substring-no-properties
                               (point-min) (point-max))))
                 (should (string-match-p "main@agent" content))))))
       ;; Cleanup
       (when-let* ((indirect (plist-get info :agent-indirect-buffer)))
         (when (buffer-live-p indirect)
           (kill-buffer indirect)))))))

(ert-deftest gptel-org-agent-test-transform-redirect-disabled ()
  "Transform does nothing when gptel-org-subtree-context is nil."
  (gptel-org-agent-test-with-buffer
   "* AI-DO Implement feature\nDescription\n"
   (goto-char (point-min))
   (org-back-to-heading t)
   (let* ((gptel-org-subtree-context nil)
          (base-buf (current-buffer))
          (fsm (gptel-make-fsm))
          (info (list :buffer base-buf
                      :position (point-marker))))
     (setf (gptel-fsm-info fsm) info)
     (gptel-org-agent--transform-redirect fsm)
     ;; Buffer should be unchanged
     (should (eq (plist-get info :buffer) base-buf)))))

(ert-deftest gptel-org-agent-test-transform-redirect-non-todo ()
  "Transform does nothing on a regular (non-TODO) heading."
  (gptel-org-agent-test-with-buffer
   "* Regular heading\nDescription\n"
   (goto-char (point-min))
   (org-back-to-heading t)
   (let* ((gptel-org-subtree-context t)
          (base-buf (current-buffer))
          (fsm (gptel-make-fsm))
          (info (list :buffer base-buf
                      :position (point-marker))))
     (setf (gptel-fsm-info fsm) info)
     (gptel-org-agent--transform-redirect fsm)
     ;; Buffer should be unchanged
     (should (eq (plist-get info :buffer) base-buf)))))

(ert-deftest gptel-org-agent-test-transform-redirect-skips-indirect ()
  "Transform does nothing when request already comes from an indirect buffer."
  (gptel-org-agent-test-with-buffer
   "* AI-DO Implement feature\nDescription\n"
   (goto-char (point-min))
   (org-back-to-heading t)
   (let* ((gptel-org-subtree-context t)
          (base-buf (current-buffer))
          ;; First create an indirect buffer to simulate being in one
          (marker (gptel-org-agent--create-subtree "main"))
          (indirect-buf (gptel-org-agent--open-indirect-buffer
                         base-buf marker))
          (fsm (gptel-make-fsm))
          (info (list :buffer indirect-buf
                      :position (with-current-buffer indirect-buf
                                  (goto-char (point-max))
                                  (point-marker)))))
     (setf (gptel-fsm-info fsm) info)
     (unwind-protect
         (progn
           (gptel-org-agent--transform-redirect fsm)
           ;; Buffer should still be the indirect buffer (not double-redirected)
           (should (eq (plist-get info :buffer) indirect-buf)))
       ;; Cleanup
       (when (buffer-live-p indirect-buf)
         (kill-buffer indirect-buf))))))

(ert-deftest gptel-org-agent-test-transform-redirect-reuses-subtree ()
  "Transform reuses existing main@agent subtree on repeated calls."
  (gptel-org-agent-test-with-buffer
   "* AI-DO Implement feature\nDescription\n"
   (goto-char (point-min))
   (org-back-to-heading t)
   (let* ((gptel-org-subtree-context t)
          (base-buf (current-buffer))
          (fsm1 (gptel-make-fsm))
          (info1 (list :buffer base-buf
                       :position (point-marker)))
          (fsm2 (gptel-make-fsm))
          (info2 (list :buffer base-buf
                       :position (point-marker))))
     (setf (gptel-fsm-info fsm1) info1)
     (setf (gptel-fsm-info fsm2) info2)
     (unwind-protect
         (progn
           ;; First redirect
           (gptel-org-agent--transform-redirect fsm1)
           (let ((indirect-1 (plist-get info1 :agent-indirect-buffer)))
             (should (buffer-live-p indirect-1))
             ;; Kill the indirect buffer (simulating it being closed after
             ;; first request completes)
             (kill-buffer indirect-1))
           ;; Second redirect - should reuse the same subtree heading
           (gptel-org-agent--transform-redirect fsm2)
           (let ((indirect-2 (plist-get info2 :agent-indirect-buffer)))
             (should (buffer-live-p indirect-2))
             ;; Verify only one main@agent heading exists in the base buffer
             (with-current-buffer base-buf
               (let ((count 0))
                 (save-excursion
                   (goto-char (point-min))
                   (while (re-search-forward
                           "^\\*+ .+:main@agent:" nil t)
                     (setq count (1+ count))))
                 (should (= 1 count))))))
       ;; Cleanup
       (when-let* ((indirect (plist-get info2 :agent-indirect-buffer)))
         (when (buffer-live-p indirect)
           (kill-buffer indirect)))))))

(ert-deftest ib-fatal-during-fsm-aborts-cleanly ()
  "On user-error from the IB API during an FSM transition, the
transform wrapper must transition the user's TODO to DENIED and
insert a TERMINE child heading whose body is an example block
containing the error message, then re-signal the user-error.

This exercises the IB-3.3 recovery hook installed on
`gptel-org-agent--transform-redirect'."
  (let ((org-inhibit-startup t)
        (org-todo-keywords
         '((sequence "AI-DO" "AI-DOING" "|" "AI-DONE" "DENIED")))
        (gptel-org-todo-keywords '("AI-DO" "AI-DOING"))
        (test-buf (generate-new-buffer "*ib-fatal-test*")))
    (unwind-protect
        (progn
          (with-current-buffer test-buf
            (delay-mode-hooks (org-mode))
            (insert "* AI-DO User task\nbody\n")
            (goto-char (point-min))
            (org-back-to-heading t)
            (cl-letf (((symbol-function 'gptel-org-agent--maybe-setup-subtree)
                       (lambda (&rest _)
                         (gptel-org-ib-fatal "test fatal %s" "boom"))))
              (let* ((gptel-org-subtree-context t)
                     (pos (point))
                     (info (list :buffer test-buf
                                 :position (copy-marker pos)))
                     (fsm (gptel-make-fsm)))
                (setf (gptel-fsm-info fsm) info)
                (should-error
                 (gptel-org-agent--transform-redirect fsm)
                 :type 'user-error))))
          ;; Verify DENIED keyword and TERMINE heading with error body.
          (with-current-buffer test-buf
            (goto-char (point-min))
            (should (re-search-forward "^\\* DENIED User task" nil t))
            (goto-char (point-min))
            (should (re-search-forward "^\\*\\* TERMINE\\b" nil t))
            (goto-char (point-min))
            (should (re-search-forward "^#\\+begin_example$" nil t))
            (goto-char (point-min))
            (should (re-search-forward "test fatal boom" nil t))
            (goto-char (point-min))
            (should (re-search-forward "^#\\+end_example$" nil t))))
      (when (buffer-live-p test-buf)
        (kill-buffer test-buf)))))

(ert-deftest gptel-org-agent-test-enable-disable ()
  "Test that enable/disable add/remove the transform from the hook."
  (let ((gptel-prompt-transform-functions '()))
    ;; Enable should add the transform
    (gptel-org-agent--enable)
    (should (memq #'gptel-org-agent--transform-redirect
                  gptel-prompt-transform-functions))
    ;; Disable should remove it
    (gptel-org-agent--disable)
    (should-not (memq #'gptel-org-agent--transform-redirect
                      gptel-prompt-transform-functions))))

(ert-deftest gptel-org-agent-test-response-insertion-in-indirect ()
  "Simulate response insertion into the redirected indirect buffer.
Verifies that text inserted at the FSM's :position marker
appears in both the indirect and base buffers."
  (gptel-org-agent-test-with-buffer
   "* AI-DO Implement feature\nDescription\n"
   (goto-char (point-min))
   (org-back-to-heading t)
   (let* ((gptel-org-subtree-context t)
          (base-buf (current-buffer))
          (fsm (gptel-make-fsm))
          (info (list :buffer base-buf
                      :position (point-marker))))
     (setf (gptel-fsm-info fsm) info)
     (unwind-protect
         (progn
           (gptel-org-agent--transform-redirect fsm)
           (let ((indirect-buf (plist-get info :buffer))
                 (pos-marker (plist-get info :position)))
             ;; Simulate what gptel--insert-response does:
             ;; insert text at the position marker in the buffer
             (with-current-buffer (marker-buffer pos-marker)
               (goto-char pos-marker)
               (let ((response "Here is the AI response.\n"))
                 (insert response)))
             ;; Response should be visible in the indirect buffer
             (with-current-buffer indirect-buf
               (let ((content (buffer-substring-no-properties
                               (point-min) (point-max))))
                 (should (string-match-p "Here is the AI response"
                                         content))))
             ;; Response should also be visible in the base buffer
             ;; (since indirect buffers share text)
             (with-current-buffer base-buf
               (let ((content (buffer-substring-no-properties
                               (point-min) (point-max))))
                 (should (string-match-p "Here is the AI response"
                                         content))
                 ;; Base buffer should have the full structure
                 (should (string-match-p "AI-DO Implement feature"
                                         content))
                 (should (string-match-p "main@agent" content))))))
       ;; Cleanup
       (when-let* ((indirect (plist-get info :agent-indirect-buffer)))
         (when (buffer-live-p indirect)
           (kill-buffer indirect)))))))


;;; ---- insert-user-heading tests (indirect buffer post-response) ------------

(ert-deftest gptel-org-agent-test-insert-user-heading-after-indirect-response ()
  "Insert exactly one FEEDBACK heading as a sibling of the agent subtree.
Verifies the heading is created in the base buffer at agent-level
\(same level as the agent heading), and that the agent heading transitions
to AI-DONE with its tag removed."
  (let ((org-inhibit-startup t)
        (org-todo-keywords '((sequence "AI-DO" "AI-DOING" "FEEDBACK" "|" "AI-DONE")))
        (gptel-org-todo-keywords '("AI-DO" "AI-DOING")))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert "* AI-DO Implement feature\nDescription\n")
      (goto-char (point-min))
      (org-back-to-heading t)
      (let* ((gptel-org-subtree-context t)
             (gptel-org-use-todo-keywords t)
             (gptel-org-user-keyword "FEEDBACK")
             (gptel-org-tasks-done-keyword "AI-DONE")
             (base-buf (current-buffer))
             (marker (gptel-org-agent--create-subtree "main"))
             (indirect-buf nil))
        (unwind-protect
            (progn
              (setq indirect-buf
                    (gptel-org-agent--open-indirect-buffer base-buf marker))
              ;; Simulate AI response inside the agent subtree
              (with-current-buffer indirect-buf
                (goto-char (point-max))
                (insert "*** AI Simple Arithmetic\n2 + 2 =\n\n*4*.\n"))
              ;; Call insert-user-heading from indirect buffer context
              (with-current-buffer indirect-buf
                (gptel-org-agent--insert-user-heading nil nil))
              ;; Verify FEEDBACK heading in base buffer
              (with-current-buffer base-buf
                (goto-char (point-min))
                ;; Should have exactly one FEEDBACK heading at agent level (2)
                (should (= 1
                           (how-many "^\\*\\* FEEDBACK"
                                     (point-min) (point-max))))
                ;; Agent heading should have transitioned to AI-DONE with tag removed
                (should (re-search-forward "^\\*\\* AI-DONE Implement feature" nil t))
                (goto-char (match-beginning 0))
                (let ((agent-level (org-current-level)))
                  ;; Agent should NOT have its tag anymore
                  (should-not (org-get-tags nil t))
                  ;; Find the FEEDBACK heading — should be sibling at agent-level
                  (should (re-search-forward "^\\*\\* FEEDBACK" nil t))
                  (goto-char (match-beginning 0))
                  ;; FEEDBACK heading should be at agent-level (sibling of agent)
                  (should (= agent-level (org-current-level))))))
          (when (and indirect-buf (buffer-live-p indirect-buf))
            (kill-buffer indirect-buf)))))))

(ert-deftest gptel-org-agent-test-insert-user-heading-not-duplicated ()
  "Repeated insert-user-heading calls should not create duplicate FEEDBACK headings."
  (let ((org-inhibit-startup t)
        (org-todo-keywords '((sequence "AI-DO" "AI-DOING" "FEEDBACK" "|" "AI-DONE")))
        (gptel-org-todo-keywords '("AI-DO" "AI-DOING")))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert "* AI-DO Implement feature\nDescription\n")
      (goto-char (point-min))
      (org-back-to-heading t)
      (let* ((gptel-org-subtree-context t)
             (gptel-org-use-todo-keywords t)
             (gptel-org-user-keyword "FEEDBACK")
             (gptel-org-tasks-done-keyword "AI-DONE")
             (base-buf (current-buffer))
             (marker (gptel-org-agent--create-subtree "main"))
             (indirect-buf nil))
        (unwind-protect
            (progn
              (setq indirect-buf
                    (gptel-org-agent--open-indirect-buffer base-buf marker))
              ;; Simulate AI response.  Insert at the streaming marker
              ;; (before the pre-seeded TERMINE child) so TERMINE's body
              ;; stays empty — matching the real code path that uses
              ;; `gptel-org-ib-streaming-marker' with insertion-type=nil.
              (with-current-buffer indirect-buf
                (goto-char (gptel-org-ib-streaming-marker "TERMINE"))
                (insert "Answer\n"))
              ;; Call insert-user-heading twice
              (with-current-buffer indirect-buf
                (gptel-org-agent--insert-user-heading nil nil)
                (gptel-org-agent--insert-user-heading nil nil))
              ;; Should have exactly 1 FEEDBACK heading at agent level, not 2
              (with-current-buffer base-buf
                (should (= 1
                           (how-many "^\\*\\* FEEDBACK"
                                     (point-min) (point-max))))))
          (when (and indirect-buf (buffer-live-p indirect-buf))
            (kill-buffer indirect-buf)))))))

(ert-deftest gptel-org-agent-test-insert-user-heading-level-is-agent-sibling ()
  "FEEDBACK heading should be at agent-level (sibling of agent subtree).
In keyword mode, the FEEDBACK heading is created as a sibling of the agent
subtree at the same level, NOT as a child at agent-level + 1.

Expected structure:
  ** DOING Task              <- level 2 (parent)
  *** AI-DONE Task           <- level 3 (agent heading, transitioned)
  **** AI Response           <- level 4 (AI response = agent + 1)
  *** FEEDBACK               <- level 3 (user = agent-level, CORRECT)"
  (let ((org-inhibit-startup t)
        (org-todo-keywords '((sequence "AI-DO" "AI-DOING" "DOING" "FEEDBACK" "|" "AI-DONE" "DONE")))
        (gptel-org-todo-keywords '("AI-DO" "AI-DOING")))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      ;; Use level 2 parent to match the structure
      (insert "** DOING Run simple gatherer task\nDescription\n")
      (goto-char (point-min))
      (org-back-to-heading t)
      (let* ((gptel-org-subtree-context t)
             (gptel-org-use-todo-keywords t)
             (gptel-org-user-keyword "FEEDBACK")
             (gptel-org-tasks-done-keyword "AI-DONE")
             (base-buf (current-buffer))
             (parent-level (org-current-level))
             (marker (gptel-org-agent--create-subtree "main"))
             (indirect-buf nil))
        (unwind-protect
            (progn
              (setq indirect-buf
                    (gptel-org-agent--open-indirect-buffer base-buf marker))
              ;; Simulate AI response at agent-level + 1
              (with-current-buffer indirect-buf
                (goto-char (point-max))
                (insert "**** AI Getting current time\n\n=2026-04-10 21:28:45 EEST=\n"))
              ;; Call insert-user-heading from indirect buffer context
              (with-current-buffer indirect-buf
                (gptel-org-agent--insert-user-heading nil nil))
              ;; Verify structure in base buffer
              (with-current-buffer base-buf
                (goto-char (point-min))
                ;; Parent task should be level 2
                (should (= 2 parent-level))
                ;; Find the agent heading — should be level 3, now AI-DONE
                (should (re-search-forward "^\\*\\*\\* AI-DONE" nil t))
                (goto-char (match-beginning 0))
                (let ((agent-level (org-current-level)))
                  (should (= 3 agent-level))
                  ;; Find the AI response heading — should be level 4
                  (should (re-search-forward "^\\*\\*\\*\\* AI Getting current time" nil t))
                  (goto-char (match-beginning 0))
                  (should (= 4 (org-current-level)))
                  ;; Find the FEEDBACK heading — should be level 3 (agent-level)
                  (should (re-search-forward "^\\*\\*\\* FEEDBACK" nil t))
                  (goto-char (match-beginning 0))
                  (should (= 3 (org-current-level)))
                  ;; FEEDBACK should be at agent-level (sibling)
                  (should (= agent-level (org-current-level))))))
          (when (and indirect-buf (buffer-live-p indirect-buf))
            (kill-buffer indirect-buf)))))))

(ert-deftest gptel-org-agent-test-insert-user-heading-level-after-auto-correct ()
  "FEEDBACK heading level should be correct after auto-corrector rebases AI headings.
Reproduces the real post-response sequence: auto-correct processes the AI
response (rebasing level-1 headings to agent-level+1), then cleanup runs,
then insert-user-heading runs.

Expected structure:
  ,** DOING Calculate 2 + 2            <- level 2 (parent)
  ,*** AI-DONE Calculate 2 + 2         <- level 3 (agent heading, transitioned)
  ,**** AI Calculate 2 + 2             <- level 4 (rebased from * AI)
  ,*** FEEDBACK                        <- level 3 (user = agent-level, CORRECT)"
  (let ((org-inhibit-startup t)
        (org-todo-keywords '((sequence "AI-DO" "AI-DOING" "DOING" "FEEDBACK"
                                       "|" "AI-DONE" "DONE")))
        (gptel-org-todo-keywords '("AI-DO" "AI-DOING")))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert "** DOING Calculate 2 + 2\nDescription\n")
      (goto-char (point-min))
      (org-back-to-heading t)
      (let* ((gptel-org-subtree-context t)
             (gptel-org-use-todo-keywords t)
             (gptel-org-user-keyword "FEEDBACK")
             (gptel-org-tasks-done-keyword "AI-DONE")
             (gptel-org-assistant-keyword "AI")
             (base-buf (current-buffer))
             (marker (gptel-org-agent--create-subtree "main"))
             (indirect-buf nil))
        (unwind-protect
            (progn
              (setq indirect-buf
                    (gptel-org-agent--open-indirect-buffer base-buf marker))
              (with-current-buffer indirect-buf
                ;; Simulate AI streaming: the AI writes at level 1 as instructed.
                ;; The idempotent auto-corrector rebases via after-change-functions.
                ;; open-indirect-buffer already called enable-auto-correct, but
                ;; the test macro doesn't, so enable it here.
                (gptel-org--enable-auto-correct)
                (goto-char (point-max))
                (insert "* AI Calculate 2 + 2\n4.\n")
                ;; Simulate post-response hook: insert-user-heading
                (gptel-org-agent--insert-user-heading nil nil))
              ;; Verify structure in base buffer
              (with-current-buffer base-buf
                (goto-char (point-min))
                ;; Agent heading must still be level 3, now AI-DONE
                (should (re-search-forward "AI-DONE Calculate 2 \\+ 2" nil t))
                (goto-char (match-beginning 0))
                (beginning-of-line)
                (should (= 3 (org-current-level)))
                ;; AI response heading should be level 4 (rebased from 1)
                (should (re-search-forward "AI Calculate 2 \\+ 2" nil t))
                (goto-char (match-beginning 0))
                (beginning-of-line)
                (should (= 4 (org-current-level)))
                ;; FEEDBACK heading should be level 3 (agent-level)
                (should (re-search-forward "^\\*+ FEEDBACK" nil t))
                (goto-char (match-beginning 0))
                (should (= 3 (org-current-level)))))
          (when (and indirect-buf (buffer-live-p indirect-buf))
            (kill-buffer indirect-buf)))))))

(ert-deftest gptel-org-agent-test-insert-user-heading-correct-after-second-send ()
  "FEEDBACK heading level is correct on second send cycle with mutated FEEDBACK.
Tests the full two-cycle scenario where the user sends feedback and the
FEEDBACK heading is mutated into a new AI-DOING agent subtree.

Scenario:
1. First cycle completed: *** AI-DONE (tag removed), *** FEEDBACK + 5 created
2. User sends from *** FEEDBACK + 5
3. maybe-setup-subtree mutates FEEDBACK to *** AI-DOING + 5 with :main@agent:
4. Agent responds with `9.'
5. insert-user-heading should create *** FEEDBACK (level 3, sibling)

Expected final structure:
  ** DOING Calculate 2 + 2
  *** AI-DONE Calculate 2 + 2              <- first agent, AI-DONE, no tag
  **** AI Calculate 2 + 2
       4.
  *** AI-DONE + 5                          <- mutated from FEEDBACK, now AI-DONE
  **** AI Result                           <- new AI response
       9.
  *** FEEDBACK                             <- new FEEDBACK heading"
  (let ((org-inhibit-startup t)
        (org-todo-keywords '((sequence "AI-DO" "AI-DOING" "DOING" "FEEDBACK"
                                       "|" "AI-DONE" "DONE")))
        (gptel-org-todo-keywords '("AI-DO" "AI-DOING")))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      ;; Start with the state AFTER the first response cycle completed and the
      ;; user typed feedback.  The FEEDBACK heading has been mutated to AI-DOING
      ;; by maybe-setup-subtree (simulating what happens when user sends from
      ;; FEEDBACK heading).
      (insert "\
** DOING Calculate 2 + 2
*** AI-DONE Calculate 2 + 2
**** AI Calculate 2 + 2
4.
*** AI-DOING + 5                                              :main@agent:
")
      (let* ((gptel-org-subtree-context t)
             (gptel-org-use-todo-keywords t)
             (gptel-org-user-keyword "FEEDBACK")
             (gptel-org-tasks-done-keyword "AI-DONE")
             (gptel-org-assistant-keyword "AI")
             (base-buf (current-buffer))
             (indirect-buf nil))
        (unwind-protect
            (progn
              ;; Open indirect buffer on the mutated FEEDBACK->AI-DOING subtree
              (goto-char (point-min))
              (re-search-forward "^\\*\\*\\* AI-DOING \\+ 5")
              (beginning-of-line)
              (let ((heading-marker (point-marker)))
                (setq indirect-buf
                      (gptel-org-agent--open-indirect-buffer base-buf heading-marker)))
              ;; In the indirect buffer, simulate AI response streaming
              (with-current-buffer indirect-buf
                ;; open-indirect-buffer already called enable-auto-correct,
                ;; so the after-change hook is active.
                (goto-char (point-max))
                (insert "* AI Result\n9.\n")
                ;; Simulate post-response hook: insert-user-heading
                (gptel-org-agent--insert-user-heading nil nil))
              ;; Verify in base buffer
              (with-current-buffer base-buf
                (goto-char (point-min))
                ;; The first agent heading should be AI-DONE at level 3
                (should (re-search-forward "AI-DONE Calculate 2 \\+ 2" nil t))
                (goto-char (match-beginning 0))
                (beginning-of-line)
                (should (= 3 (org-current-level)))
                ;; The FIRST AI response heading must still be level 4
                (should (re-search-forward "AI Calculate 2 \\+ 2" nil t))
                (goto-char (match-beginning 0))
                (beginning-of-line)
                (should (= 4 (org-current-level)))
                ;; The second agent heading (mutated FEEDBACK) should be
                ;; AI-DONE at level 3, with tag removed
                (should (re-search-forward "AI-DONE \\+ 5" nil t))
                (goto-char (match-beginning 0))
                (beginning-of-line)
                (should (= 3 (org-current-level)))
                ;; The new AI response heading should be level 4
                (should (re-search-forward "AI Result" nil t))
                (goto-char (match-beginning 0))
                (beginning-of-line)
                (should (= 4 (org-current-level)))
                ;; The NEW FEEDBACK heading should be level 3 (agent-level).
                ;; Find the LAST FEEDBACK heading in the buffer.
                (goto-char (point-max))
                (should (re-search-backward "^\\*+ FEEDBACK" nil t))
                (should (= 3 (org-current-level)))))
          (when (and indirect-buf (buffer-live-p indirect-buf))
            (kill-buffer indirect-buf)))))))

;;; ---- maybe-setup-subtree mutation from FEEDBACK heading -------------------

(ert-deftest gptel-org-agent-test-maybe-setup-subtree-mutates-feedback-heading ()
  "Sending from a FEEDBACK heading should mutate it into an AI-DOING agent subtree.
When `gptel-org-use-todo-keywords' is enabled, a FEEDBACK heading is a user
prompt heading created as a sibling of the agent subtree.
`maybe-setup-subtree' must:
1. Mutate the FEEDBACK heading in-place (change to AI-DOING + add agent tag)
2. Transition the parent heading to AI-DOING
3. Return an indirect buffer narrowed to the mutated heading

Expected:
  ** DOING Task           -> ** AI-DOING Task (parent transitioned)
  *** AI-DONE Task        <- previous agent, completed
  *** FEEDBACK Add 6      -> *** AI-DOING Add 6 :main@agent: (mutated)"
  (let ((org-inhibit-startup t)
        (org-todo-keywords '((sequence "AI-DO" "AI-DOING" "DOING" "FEEDBACK"
                                       "|" "AI-DONE" "DONE")))
        (gptel-org-todo-keywords '("AI-DO" "AI-DOING")))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert "** DOING Calculate 2 + 2
*** AI-DONE Calculate 2 + 2
**** AI Calculate 2 + 2
2 + 2 = 4
*** FEEDBACK Add 6
")
      (let* ((gptel-org-subtree-context t)
             (gptel-org-use-todo-keywords t)
             (gptel-org-user-keyword "FEEDBACK")
             (gptel-org-tasks-doing-keyword "AI-DOING")
             (gptel-org-tasks--active-task-marker nil)
             (indirect-buf nil))
        (unwind-protect
            (progn
              ;; Move to the FEEDBACK heading
              (goto-char (point-min))
              (re-search-forward "^\\*\\*\\* FEEDBACK")
              (beginning-of-line)
              (should (equal (org-get-todo-state) "FEEDBACK"))
              ;; Call maybe-setup-subtree - should mutate FEEDBACK heading
              (setq indirect-buf (gptel-org-agent--maybe-setup-subtree))
              ;; Should have returned an indirect buffer
              (should indirect-buf)
              (should (buffer-live-p indirect-buf))
              ;; The indirect buffer should be narrowed to the mutated heading
              (with-current-buffer indirect-buf
                (goto-char (point-min))
                (should (org-at-heading-p))
                ;; Should be the mutated FEEDBACK heading, now AI-DOING
                (should (equal (org-get-todo-state) "AI-DOING"))
                (should (= 3 (org-current-level)))
                ;; Should have the agent tag
                (should (cl-some #'gptel-org-agent--agent-tag-p
                                 (org-get-tags nil t))))
              ;; Verify the parent heading was transitioned to AI-DOING
              (goto-char (point-min))
              (re-search-forward "^\\*\\* ")
              (beginning-of-line)
              (should (equal (org-get-todo-state) "AI-DOING"))
              ;; Verify the active task marker was set
              (should gptel-org-tasks--active-task-marker))
          (when (and indirect-buf (buffer-live-p indirect-buf))
            (kill-buffer indirect-buf)))))))

;;; ---- Auto-corrector heading level stability --------------------------------

(ert-deftest gptel-org-agent-test-auto-correct-does-not-rebase-agent-heading ()
  "Auto-corrector must NOT rebase the agent heading at point-min.
Reproduces a bug where the coordinator agent heading at level 3 gets
rebased to level 6 by the auto-corrector (ref-level=4, offset=3,
3+3=6).

The auto-corrector rebases headings with stars < ref-level.  The agent
heading at point-min of the indirect buffer IS at a level < ref-level
\(e.g. level 3 < ref-level 4), but it must be excluded because it is
the structural heading that owns the subtree, not AI response content.

Setup: level-2 parent -> level-3 coordinator agent heading.
The coordinator writes a level-1 AI heading which gets rebased to 4.
Verify the agent heading stays at level 3."
  (let ((org-inhibit-startup t)
        (org-todo-keywords '((sequence "AI-DO" "AI-DOING" "DOING" "FEEDBACK"
                                       "|" "AI-DONE" "DONE")))
        (gptel-org-todo-keywords '("AI-DO" "AI-DOING")))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert "** DOING Delegate to researcher\nDescription\n")
      (goto-char (point-min))
      (org-back-to-heading t)
      (let* ((gptel-org-subtree-context t)
             (gptel-org-use-todo-keywords t)
             (gptel-org-user-keyword "FEEDBACK")
             (gptel-org-tasks-done-keyword "AI-DONE")
             (gptel-org-assistant-keyword "AI")
             (base-buf (current-buffer))
             (marker (gptel-org-agent--create-subtree "coordinator"))
             (indirect-buf nil))
        (unwind-protect
            (progn
              (setq indirect-buf
                    (gptel-org-agent--open-indirect-buffer base-buf marker))
              ;; Verify the agent heading is at level 3 before any AI response
              (with-current-buffer indirect-buf
                (goto-char (point-min))
                (should (org-at-heading-p))
                (should (= 3 (org-current-level))))
              ;; Simulate AI streaming: the coordinator writes level-1 heading
              ;; The auto-corrector (enabled by open-indirect-buffer) should
              ;; rebase it to level 4 but leave the agent heading at level 3.
              (with-current-buffer indirect-buf
                (goto-char (point-max))
                (insert "* AI Delegation result\nThe chain worked.\n"))
              ;; THE BUG: the agent heading at point-min was rebased from 3 to 6
              (with-current-buffer indirect-buf
                (goto-char (point-min))
                (should (org-at-heading-p))
                ;; Agent heading MUST still be level 3, NOT 6
                (should (= 3 (org-current-level))))
              ;; The AI response heading should have been rebased to level 4
              (with-current-buffer base-buf
                (goto-char (point-min))
                (should (re-search-forward "AI Delegation result" nil t))
                (beginning-of-line)
                (should (= 4 (org-current-level)))))
          (when (and indirect-buf (buffer-live-p indirect-buf))
            (kill-buffer indirect-buf)))))))

(ert-deftest gptel-org-agent-test-auto-correct-preserves-agent-heading-with-sub-agents ()
  "Agent heading levels preserved during 3-level delegation chain.
Reproduces the real bug from coordinator -> researcher -> gatherer chain.

The coordinator indirect buffer has auto-correct with ref-level=4.
When the researcher sub-agent is created inside that buffer via
setup-task-subtree, the coordinator heading must remain at level 3.

Expected structure:
  ** DOING Delegate task                              <- level 2 (parent)
  *** AI-DOING Delegate task  :coordinator@agent:     <- level 3 (coordinator)
  **** AI-DOING Research...   :researcher@coord@agent: <- level 4 (researcher)
  ***** AI-DOING Run date     :gatherer@res@coord@agent: <- level 5 (gatherer)

The bug: coordinator heading ends up at level 6 (3 + offset 3)."
  (let ((org-inhibit-startup t)
        (org-todo-keywords '((sequence "AI-DO" "AI-DOING" "DOING" "FEEDBACK"
                                       "|" "AI-DONE" "DONE")))
        (gptel-org-todo-keywords '("AI-DO" "AI-DOING")))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert "** DOING Delegate to researcher agent\nDescription\n")
      (goto-char (point-min))
      (org-back-to-heading t)
      (let* ((gptel-org-subtree-context t)
             (gptel-org-use-todo-keywords t)
             (gptel-org-user-keyword "FEEDBACK")
             (gptel-org-tasks-done-keyword "AI-DONE")
             (gptel-org-tasks-doing-keyword "AI-DOING")
             (gptel-org-assistant-keyword "AI")
             (base-buf (current-buffer))
             ;; Step 1: Create coordinator subtree (level 3 under level 2)
             (coord-marker (gptel-org-agent--create-subtree "coordinator"))
             (coord-indirect nil)
             (researcher-result nil)
             (researcher-indirect nil)
             (gatherer-result nil)
             (gatherer-indirect nil))
        (unwind-protect
            (progn
              ;; Step 2: Open coordinator indirect buffer (enables auto-correct)
              (setq coord-indirect
                    (gptel-org-agent--open-indirect-buffer base-buf coord-marker))
              ;; Verify coordinator heading at level 3
              (with-current-buffer coord-indirect
                (goto-char (point-min))
                (should (= 3 (org-current-level))))

              ;; Step 3: Create researcher sub-agent inside coordinator buffer
              ;; This is what happens when the coordinator LLM calls Agent tool
              (with-current-buffer coord-indirect
                (setq researcher-result
                      (gptel-org-agent--setup-task-subtree
                       "researcher" "Research with date delegation")))
              (should researcher-result)
              (setq researcher-indirect
                    (plist-get researcher-result :indirect-buffer))

              ;; Verify coordinator heading still at level 3
              (with-current-buffer coord-indirect
                (goto-char (point-min))
                (should (= 3 (org-current-level))))

              ;; Verify researcher heading at level 4
              (with-current-buffer researcher-indirect
                (goto-char (point-min))
                (should (= 4 (org-current-level))))

              ;; Step 4: Create gatherer sub-agent inside researcher buffer
              (with-current-buffer researcher-indirect
                (setq gatherer-result
                      (gptel-org-agent--setup-task-subtree
                       "gatherer" "Run date command")))
              (should gatherer-result)
              (setq gatherer-indirect
                    (plist-get gatherer-result :indirect-buffer))

              ;; Verify all heading levels are still correct
              ;; Coordinator: level 3
              (with-current-buffer coord-indirect
                (goto-char (point-min))
                (should (= 3 (org-current-level))))
              ;; Researcher: level 4
              (with-current-buffer researcher-indirect
                (goto-char (point-min))
                (should (= 4 (org-current-level))))
              ;; Gatherer: level 5
              (with-current-buffer gatherer-indirect
                (goto-char (point-min))
                (should (= 5 (org-current-level))))

              ;; Step 5: Simulate gatherer response (writes at level 1, auto-corrected)
              (with-current-buffer gatherer-indirect
                (goto-char (point-max))
                (insert "#+begin_example\nTue Apr 14 13:06:37 EEST 2026\n#+end_example\n"))

              ;; Step 6: Simulate gatherer completion and cleanup
              ;; (In real code, subtree--handle-done extracts text and closes buffer)
              (when (buffer-live-p gatherer-indirect)
                (gptel-org-agent--close-indirect-buffer gatherer-indirect t)
                (setq gatherer-indirect nil))

              ;; Step 7: Simulate researcher writing its final response.
              ;; IB-4.3: must stream BEFORE the TERMINE terminator so it
              ;; remains empty-body and eligible for removal at AI-DONE.
              (with-current-buffer researcher-indirect
                (save-excursion
                  (goto-char (point-min))
                  (goto-char (gptel-org-ib-streaming-marker "TERMINE"))
                  (insert "* AI Gatherer Output\nThe date is Tue Apr 14.\n")))

              ;; Verify coordinator heading STILL at level 3
              (with-current-buffer coord-indirect
                (goto-char (point-min))
                (should (= 3 (org-current-level))))

              ;; Step 8: Simulate researcher completion
              (when (buffer-live-p researcher-indirect)
                (gptel-org-agent--close-indirect-buffer researcher-indirect t)
                (setq researcher-indirect nil))

              ;; Step 9: Simulate coordinator writing final response.
              ;; IB-4.3: stream BEFORE the TERMINE so the placeholder
              ;; can be removed when the agent completes.
              (with-current-buffer coord-indirect
                (save-excursion
                  (goto-char (point-min))
                  (goto-char (gptel-org-ib-streaming-marker "TERMINE"))
                  (insert "The chain worked: coordinator -> researcher -> gatherer.\nResult: Tue Apr 14 13:06:37 EEST 2026\n")))

              ;; Step 10: Verify the coordinator heading is STILL level 3
              (with-current-buffer coord-indirect
                (goto-char (point-min))
                (should (= 3 (org-current-level))))

              ;; Step 11: insert-user-heading should create FEEDBACK at level 3
              (with-current-buffer coord-indirect
                (gptel-org-agent--insert-user-heading nil nil))

              ;; Step 12: Verify final structure in base buffer
              (with-current-buffer base-buf
                (goto-char (point-min))
                ;; Parent at level 2
                (should (re-search-forward "^\\*\\* " nil t))
                (beginning-of-line)
                (should (= 2 (org-current-level)))
                ;; Agent heading (now AI-DONE) at level 3, NOT level 6
                (should (re-search-forward "AI-DONE" nil t))
                (beginning-of-line)
                (should (= 3 (org-current-level)))
                ;; FEEDBACK heading at level 3 (sibling of agent)
                (goto-char (point-max))
                (should (re-search-backward "^\\*+ FEEDBACK" nil t))
                (should (= 3 (org-current-level)))))
          ;; Cleanup
          (when (and gatherer-indirect (buffer-live-p gatherer-indirect))
            (kill-buffer gatherer-indirect))
          (when (and researcher-indirect (buffer-live-p researcher-indirect))
            (kill-buffer researcher-indirect))
          (when (and coord-indirect (buffer-live-p coord-indirect))
            (kill-buffer coord-indirect)))))))

(ert-deftest gptel-org-agent-test-heading-titles-use-description-not-parent ()
  "Sub-agent headings should use the task description, not the parent heading title.
When setup-task-subtree creates a researcher sub-agent, the heading title
should be the description parameter (e.g. 'Research with date delegation'),
not the parent heading text (e.g. 'Delegate to researcher agent')."
  (let ((org-inhibit-startup t)
        (org-todo-keywords '((sequence "AI-DO" "AI-DOING" "DOING" "FEEDBACK"
                                       "|" "AI-DONE" "DONE")))
        (gptel-org-todo-keywords '("AI-DO" "AI-DOING")))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert "** DOING Delegate to researcher agent\nDescription\n")
      (goto-char (point-min))
      (org-back-to-heading t)
      (let* ((gptel-org-subtree-context t)
             (gptel-org-use-todo-keywords t)
             (gptel-org-tasks-doing-keyword "AI-DOING")
             (base-buf (current-buffer))
             (coord-marker (gptel-org-agent--create-subtree "coordinator"))
             (coord-indirect nil)
             (researcher-result nil)
             (researcher-indirect nil))
        (unwind-protect
            (progn
              (setq coord-indirect
                    (gptel-org-agent--open-indirect-buffer base-buf coord-marker))
              ;; Create researcher sub-agent with a specific description
              (with-current-buffer coord-indirect
                (setq researcher-result
                      (gptel-org-agent--setup-task-subtree
                       "researcher" "Research with date delegation")))
              (should researcher-result)
              (setq researcher-indirect
                    (plist-get researcher-result :indirect-buffer))
              ;; The researcher heading should use the description, not parent title
              (with-current-buffer researcher-indirect
                (goto-char (point-min))
                (should (org-at-heading-p))
                (let ((heading-text (org-get-heading t t t t)))
                  ;; Should contain the description
                  (should (string-match-p "Research with date delegation"
                                          heading-text))
                  ;; Should NOT be the parent's heading text
                  (should-not (string-match-p "Delegate to researcher agent"
                                              heading-text)))))
          (when (and researcher-indirect (buffer-live-p researcher-indirect))
            (kill-buffer researcher-indirect))
          (when (and coord-indirect (buffer-live-p coord-indirect))
            (kill-buffer coord-indirect)))))))

(ert-deftest gptel-org-agent-test-insert-user-heading-transitions-allowed-to-done ()
  "ALLOWED tool confirmation headings should transition to AI-DONE.
When insert-user-heading runs after a tool call response completes,
any ALLOWED (or DENIED) tool confirmation headings within the agent
subtree should be transitioned to AI-DONE along with the agent heading."
  (let ((org-inhibit-startup t)
        (org-todo-keywords '((sequence "AI-DO" "AI-DOING" "PENDING" "ALLOWED"
                                       "FEEDBACK" "|" "AI-DONE" "DENIED")))
        (gptel-org-todo-keywords '("AI-DO" "AI-DOING")))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert "* AI-DO Calculate 2 + A\nPick A from 0-9\n")
      (goto-char (point-min))
      (org-back-to-heading t)
      (let* ((gptel-org-subtree-context t)
             (gptel-org-use-todo-keywords t)
             (gptel-org-user-keyword "FEEDBACK")
             (gptel-org-tasks-done-keyword "AI-DONE")
             (gptel-org-agent-tool-confirm-keywords '("PENDING" "ALLOWED" "DENIED"))
             (base-buf (current-buffer))
             (marker (gptel-org-agent--create-subtree "main"))
             (indirect-buf nil))
        (unwind-protect
            (progn
              (setq indirect-buf
                    (gptel-org-agent--open-indirect-buffer base-buf marker))
              ;; Simulate AI response with a tool call that was ALLOWED
              (with-current-buffer indirect-buf
                (goto-char (point-max))
                (insert "*** AI Simple Arithmetic\n\n"
                        "Let me calculate 2 + 5.\n\n"
                        "*** ALLOWED Requesting permission to run: Eval\n"
                        "(Eval \"(+ 2 5)\")\n\n"
                        "#+begin_src gptel-tool\n"
                        "(Eval :expression \"(+ 2 5)\")\n"
                        "(:name \"Eval\" :args (:expression \"(+ 2 5)\"))\n\n"
                        "Result:\n7\n"
                        "#+end_src\n"
                        "*** Testing result\n\n"
                        "The answer is *7*.\n"))
              ;; Call insert-user-heading from indirect buffer context
              (with-current-buffer indirect-buf
                (gptel-org-agent--insert-user-heading nil nil))
              ;; Verify in base buffer
              (with-current-buffer base-buf
                (goto-char (point-min))
                ;; Agent heading should be AI-DONE
                (should (re-search-forward "^\\*\\* AI-DONE" nil t))
                ;; ALLOWED heading should have been transitioned to AI-DONE
                (should (= 0 (how-many "^\\*\\*\\* ALLOWED"
                                        (point-min) (point-max))))
                (goto-char (point-min))
                ;; The tool confirm heading should now be AI-DONE
                (should (re-search-forward
                         "^\\*\\*\\* AI-DONE Requesting permission to run: Eval"
                         nil t))
                ;; FEEDBACK heading should exist
                (should (re-search-forward "^\\*\\* FEEDBACK" nil t))))
          (when (and indirect-buf (buffer-live-p indirect-buf))
            (kill-buffer indirect-buf)))))))

(ert-deftest gptel-org-agent-test-insert-user-heading-transitions-denied-to-done ()
  "DENIED tool confirmation headings should also transition to AI-DONE.
After the LLM processes a denied tool call and produces a final response,
the DENIED heading should transition to AI-DONE."
  (let ((org-inhibit-startup t)
        (org-todo-keywords '((sequence "AI-DO" "AI-DOING" "PENDING" "ALLOWED"
                                       "FEEDBACK" "|" "AI-DONE" "DENIED")))
        (gptel-org-todo-keywords '("AI-DO" "AI-DOING")))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert "* AI-DO Dangerous task\nDo something risky\n")
      (goto-char (point-min))
      (org-back-to-heading t)
      (let* ((gptel-org-subtree-context t)
             (gptel-org-use-todo-keywords t)
             (gptel-org-user-keyword "FEEDBACK")
             (gptel-org-tasks-done-keyword "AI-DONE")
             (gptel-org-agent-tool-confirm-keywords '("PENDING" "ALLOWED" "DENIED"))
             (base-buf (current-buffer))
             (marker (gptel-org-agent--create-subtree "main"))
             (indirect-buf nil))
        (unwind-protect
            (progn
              (setq indirect-buf
                    (gptel-org-agent--open-indirect-buffer base-buf marker))
              ;; Simulate a denied tool call followed by LLM response
              (with-current-buffer indirect-buf
                (goto-char (point-max))
                (insert "*** AI Attempting risky operation\n\n"
                        "*** DENIED Requesting permission to run: Bash\n"
                        "(Bash \"rm -rf /\")\n\n"
                        "#+begin_src gptel-tool\n"
                        "Error: denied\n"
                        "#+end_src\n"
                        "*** Adjusted approach\n\n"
                        "I'll take a safer approach.\n"))
              (with-current-buffer indirect-buf
                (gptel-org-agent--insert-user-heading nil nil))
              (with-current-buffer base-buf
                (goto-char (point-min))
                ;; DENIED heading should be transitioned to AI-DONE
                (should (= 0 (how-many "^\\*\\*\\* DENIED"
                                        (point-min) (point-max))))
                (goto-char (point-min))
                (should (re-search-forward
                         "^\\*\\*\\* AI-DONE Requesting permission to run: Bash"
                         nil t))))
          (when (and indirect-buf (buffer-live-p indirect-buf))
            (kill-buffer indirect-buf)))))))

(ert-deftest gptel-org-agent-test-tool-heading-rebased-in-indirect-buffer ()
  "TOOL heading inserted in indirect buffer should be rebased by auto-correct.
When `gptel--display-tool-results' inserts a `* TOOL ...' heading in an
agent indirect buffer (with ref-level set and auto-correct enabled), the
auto-corrector must rebase it to the correct depth.

This test covers the normal path: insertion happens in the indirect buffer
and auto-correct fires via `after-change-functions'."
  (let ((org-inhibit-startup t)
        (org-todo-keywords '((sequence "AI-DO" "AI-DOING" "DOING" "FEEDBACK"
                                       "|" "AI-DONE" "DONE")))
        (gptel-org-todo-keywords '("AI-DO" "AI-DOING")))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert "** DOING Run a tool\nDescription\n")
      (goto-char (point-min))
      (org-back-to-heading t)
      (let* ((gptel-org-subtree-context t)
             (gptel-org-use-todo-keywords t)
             (gptel-org-tasks-doing-keyword "AI-DOING")
             (base-buf (current-buffer))
             (coord-marker (gptel-org-agent--create-subtree "coordinator"))
             (coord-indirect nil))
        (unwind-protect
            (progn
              (setq coord-indirect
                    (gptel-org-agent--open-indirect-buffer base-buf coord-marker))
              ;; Verify auto-correct is enabled with correct ref-level
              (with-current-buffer coord-indirect
                (should (= 4 gptel-org--ref-level))
                (should (memq #'gptel-org--auto-correct-on-change
                              after-change-functions)))
              ;; Simulate tool result insertion as display-tool-results does:
              ;; always writes "* TOOL ..." (level 1), relying on auto-correct
              ;; to rebase it.
              (with-current-buffer coord-indirect
                (goto-char (point-max))
                (let ((tool-heading "* TOOL (Bash :command \"date\")\n")
                      (tool-body "(:name \"Bash\" :args (:command \"date\"))\n#+begin_tool\nThu Apr 16 19:01:57 EEST 2026\n#+end_tool\n"))
                  ;; Mark heading as gptel ignore (as display-tool-results does)
                  (add-text-properties
                   0 (length tool-heading)
                   '(gptel ignore front-sticky (gptel)) tool-heading)
                  (insert tool-heading tool-body)))
              ;; Verify: the TOOL heading should be rebased to ref-level (4)
              ;; by the auto-corrector that fires on after-change-functions
              (with-current-buffer coord-indirect
                (goto-char (point-min))
                (should (re-search-forward "^\\(\\*+\\) TOOL " nil t))
                (should (= 4 (length (match-string 1)))))
              ;; Also verify in base buffer: heading should be at level 4
              (with-current-buffer base-buf
                (goto-char (point-min))
                (should (re-search-forward "^\\(\\*+\\) TOOL " nil t))
                (should (= 4 (length (match-string 1))))))
          (when (and coord-indirect (buffer-live-p coord-indirect))
            (kill-buffer coord-indirect)))))))

(ert-deftest gptel-org-agent-test-tool-heading-at-root-when-inserted-in-base-buffer ()
  "TOOL heading inserted in base buffer is placed at correct ref-level.

Verifies the fix for a bug where `display-tool-results' inserted a
root-level `* TOOL' heading when its start-marker resolved to the base
buffer (e.g., after a sub-agent IB was closed before the result
arrived).  `gptel--display-tool-results' now computes the correct
heading level up front from an agent IB registered on the same base
buffer, so the TOOL heading is written at the right depth directly.

The sequence the fix must handle:
1. Coordinator runs in indirect buffer (ref-level=4)
2. Coordinator calls sub-agent tool (gatherer)
3. Gatherer completes → its indirect buffer may be closed
4. display-tool-results inserts in the base buffer because the
   start-marker resolves to the base buffer
5. With the fix, the TOOL heading level is computed from the
   still-live coordinator IB's ref-level (=4) via the IB registry,
   so the heading is written directly as `**** TOOL ...'."
  (let ((org-inhibit-startup t)
        (org-todo-keywords '((sequence "AI-DO" "AI-DOING" "DOING" "FEEDBACK"
                                       "|" "AI-DONE" "DONE")))
        (gptel-org-todo-keywords '("AI-DO" "AI-DOING")))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert "** DOING Delegate task\nDescription\n")
      (goto-char (point-min))
      (org-back-to-heading t)
      (let* ((gptel-org-subtree-context t)
             (gptel-org-use-todo-keywords t)
             (gptel-org-tasks-doing-keyword "AI-DOING")
             (base-buf (current-buffer))
             (coord-marker (gptel-org-agent--create-subtree "coordinator"))
             (coord-indirect nil))
        (unwind-protect
            (progn
              (setq coord-indirect
                    (gptel-org-agent--open-indirect-buffer base-buf coord-marker))
              ;; Verify that ref-level IS set in indirect buffer
              (with-current-buffer coord-indirect
                (should (= 4 gptel-org--ref-level)))
              ;; Verify that ref-level is NOT set in base buffer
              (with-current-buffer base-buf
                (should-not (bound-and-true-p gptel-org--ref-level)))
              ;; Exercise the real code path: call
              ;; `gptel--display-tool-results' with an `info' plist whose
              ;; :buffer and :position resolve to the BASE buffer (as if
              ;; the sub-agent IB was closed and the marker association
              ;; staled to the base).  The coordinator IB remains open
              ;; and registered on base-buf, so the fix's priority-3
              ;; lookup should find ref-level=4.
              (let* ((tool-spec (gptel-make-tool
                                 :name "Agent"
                                 :function #'ignore
                                 :description "Delegate"
                                 :args '((:name "subagent_type"
                                          :type "string"
                                          :description "t"))
                                 :include t))
                     (tool-results
                      (list (list tool-spec
                                  '(:subagent_type "gatherer")
                                  "Gatherer result")))
                     ;; Position inside the coordinator subtree (same
                     ;; position the agent IB is narrowed to).  In the
                     ;; real bug, the tool result is written inside the
                     ;; agent subtree via a marker whose buffer resolved
                     ;; to the base (not the IB).
                     (position-in-subtree
                      (with-current-buffer coord-indirect
                        (save-excursion
                          (goto-char (point-max))
                          (point))))
                     (position-marker
                      (with-current-buffer base-buf
                        (copy-marker position-in-subtree nil)))
                     (info (list :buffer base-buf
                                 :position position-marker
                                 :callback #'gptel--insert-response
                                 :tools (list tool-spec)
                                 :tool-use (list (list :name "Agent"
                                                       :id "call_1"))))
                     ;; display-tool-results honors gptel-include-tool-results
                     (gptel-include-tool-results t))
                (gptel--display-tool-results tool-results info))
              ;; With the fix: the per-tool-name state (derived from
              ;; :subagent_type, so GATHERER, not AGENT) heading is at
              ;; level 4 in base buffer, inferred from the registered
              ;; coordinator IB.  The :subagent_type key is excluded
              ;; from the args-title, so with no other args the title
              ;; is just "GATHERER" with no trailing args.
              (with-current-buffer base-buf
                (goto-char (point-min))
                (should (re-search-forward "^\\(\\*+\\) GATHERER\\(?:[ \t]\\|$\\)" nil t))
                (should (= 4 (length (match-string 1)))))
              ;; CONTRAST: direct insertion in the indirect buffer also yields
              ;; level 4 (via auto-correct rebasing — unchanged path)
              (with-current-buffer coord-indirect
                (goto-char (point-max))
                (let ((tool-heading "* BASH :command \"echo hi\"\n")
                      (tool-body "(:name \"Bash\" :args (:command \"echo hi\"))\n#+begin_tool\nhi\n#+end_tool\n"))
                  (add-text-properties
                   0 (length tool-heading)
                   '(gptel ignore front-sticky (gptel)) tool-heading)
                  (insert tool-heading tool-body)))
              (with-current-buffer coord-indirect
                (goto-char (point-min))
                (should (re-search-forward "^\\(\\*+\\) BASH " nil t))
                ;; Correctly rebased to level 4 in indirect buffer
                (should (= 4 (length (match-string 1))))))
          (when (and coord-indirect (buffer-live-p coord-indirect))
            (kill-buffer coord-indirect)))))))

(ert-deftest gptel-org-agent-test-tool-heading-uses-indirect-buffer ()
  "TOOL body insertion goes through a transient indirect buffer.
Verifies that `gptel--display-tool-results' creates a TOOL IB via
`gptel-org--tool-create-indirect-buffer' and closes it afterwards
(i.e., `gptel-org--tool-indirect-buffer' is set transiently during
insertion and nil after)."
  (let ((org-inhibit-startup t)
        (org-todo-keywords '((sequence "AI-DO" "AI-DOING" "DOING" "FEEDBACK"
                                       "|" "AI-DONE" "DONE")))
        (gptel-org-todo-keywords '("AI-DO" "AI-DOING")))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert "** DOING Run a tool\nDescription\n")
      (goto-char (point-min))
      (org-back-to-heading t)
      (let* ((gptel-org-subtree-context t)
             (gptel-org-use-todo-keywords t)
             (gptel-org-tasks-doing-keyword "AI-DOING")
             (base-buf (current-buffer))
             (coord-marker (gptel-org-agent--create-subtree "coordinator"))
             (coord-indirect nil)
             (ib-seen nil))
        (unwind-protect
            (progn
              (setq coord-indirect
                    (gptel-org-agent--open-indirect-buffer base-buf coord-marker))
              ;; Spy on tool IB creation: advice around
              ;; `gptel-org--tool-create-indirect-buffer' captures the
              ;; buffer created during the call.
              (let ((spy (lambda (orig &rest args)
                           (let ((ib (apply orig args)))
                             (when ib (setq ib-seen ib))
                             ib))))
                (advice-add 'gptel-org--tool-create-indirect-buffer
                            :around spy)
                (unwind-protect
                    (let* ((tool-spec (gptel-make-tool
                                       :name "Bash"
                                       :function #'ignore
                                       :description "Run"
                                       :args '((:name "command"
                                                :type "string"
                                                :description "c"))
                                       :include t))
                           (tool-results
                            (list (list tool-spec
                                        '(:command "date")
                                        "Thu Apr 16 19:01:57 EEST 2026")))
                           (position-marker
                            (with-current-buffer coord-indirect
                              (save-excursion
                                (goto-char (point-max))
                                (copy-marker (point) nil))))
                           (info (list :buffer coord-indirect
                                       :position position-marker
                                       :callback #'gptel--insert-response
                                       :tools (list tool-spec)
                                       :tool-use (list (list :name "Bash"
                                                             :id "call_b1"))))
                           (gptel-include-tool-results t))
                      (gptel--display-tool-results tool-results info))
                  (advice-remove 'gptel-org--tool-create-indirect-buffer spy)))
              ;; An IB was created during the insertion
              (should ib-seen)
              ;; And it was closed afterwards
              (should-not (buffer-live-p ib-seen))
              ;; Tracking vars are cleared in both IB and base
              (with-current-buffer coord-indirect
                (should-not gptel-org--tool-indirect-buffer))
              (with-current-buffer base-buf
                (should-not gptel-org--tool-indirect-buffer))
              ;; The BASH (per-tool-name) heading landed at
              ;; ref-level=4 inside coord-indirect
              (with-current-buffer coord-indirect
                (goto-char (point-min))
                (should (re-search-forward "^\\(\\*+\\) BASH " nil t))
                (should (= 4 (length (match-string 1))))))
          (when (and coord-indirect (buffer-live-p coord-indirect))
            (kill-buffer coord-indirect)))))))

(ert-deftest gptel-org-agent-test-tool-heading-nested-agent-chain ()
  "TOOL headings at each level of a coordinator->gatherer chain are rebased.
When the coordinator calls a gatherer sub-agent, the gatherer's TOOL
heading should be rebased in the gatherer's indirect buffer, and the
coordinator's TOOL heading (for the Agent tool result) should be rebased
in the coordinator's indirect buffer."
  (let ((org-inhibit-startup t)
        (org-todo-keywords '((sequence "AI-DO" "AI-DOING" "DOING" "FEEDBACK"
                                       "|" "AI-DONE" "DONE")))
        (gptel-org-todo-keywords '("AI-DO" "AI-DOING")))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert "** DOING Delegate task\nDescription\n")
      (goto-char (point-min))
      (org-back-to-heading t)
      (let* ((gptel-org-subtree-context t)
             (gptel-org-use-todo-keywords t)
             (gptel-org-tasks-doing-keyword "AI-DOING")
             (base-buf (current-buffer))
             (coord-marker (gptel-org-agent--create-subtree "coordinator"))
             (coord-indirect nil)
             (gatherer-result nil)
             (gatherer-indirect nil))
        (unwind-protect
            (progn
              ;; Create coordinator indirect buffer (ref-level=4)
              (setq coord-indirect
                    (gptel-org-agent--open-indirect-buffer base-buf coord-marker))
              (with-current-buffer coord-indirect
                (should (= 4 gptel-org--ref-level)))

              ;; Create gatherer sub-agent (ref-level=5)
              (with-current-buffer coord-indirect
                (setq gatherer-result
                      (gptel-org-agent--setup-task-subtree
                       "gatherer" "Run date command")))
              (should gatherer-result)
              (setq gatherer-indirect
                    (plist-get gatherer-result :indirect-buffer))
              (with-current-buffer gatherer-indirect
                (should (= 5 gptel-org--ref-level)))

              ;; Step 1: Insert TOOL heading in gatherer's buffer (simulates
              ;; Bash tool result returned to gatherer)
              (with-current-buffer gatherer-indirect
                (goto-char (point-max))
                (let ((tool-text "* TOOL (Bash :command \"date\")\n(:name \"Bash\" :args (:command \"date\"))\n#+begin_tool\nThu Apr 16 19:01:57 EEST 2026\n#+end_tool\n"))
                  (insert tool-text)))

              ;; Verify gatherer's TOOL heading is at level 5
              (with-current-buffer gatherer-indirect
                (goto-char (point-min))
                (should (re-search-forward "^\\(\\*+\\) TOOL (Bash" nil t))
                (should (= 5 (length (match-string 1)))))

              ;; Step 2: Simulate gatherer completion — close its buffer
              (gptel-org-agent--close-indirect-buffer gatherer-indirect t)
              (setq gatherer-indirect nil)

              ;; Step 3: Insert TOOL heading in coordinator's buffer (simulates
              ;; Agent tool result returned to coordinator after gatherer completed)
              (with-current-buffer coord-indirect
                (goto-char (point-max))
                (let ((tool-text "* TOOL (Agent :subagent_type \"gatherer\" :description \"Run date command\")\n(:name \"Agent\" :args (:subagent_type \"gatherer\"))\n#+begin_tool\nGatherer result for task: Run date command\n\nThu Apr 16 19:01:57 EEST 2026\n#+end_tool\n"))
                  (insert tool-text)))

              ;; Verify coordinator's TOOL heading is at level 4
              (with-current-buffer coord-indirect
                (goto-char (point-min))
                (should (re-search-forward "^\\(\\*+\\) TOOL (Agent" nil t))
                (should (= 4 (length (match-string 1)))))

              ;; Verify full structure in base buffer
              (with-current-buffer base-buf
                (goto-char (point-min))
                ;; Parent at level 2
                (should (re-search-forward "^\\*\\* " nil t))
                ;; Agent heading at level 3
                (should (re-search-forward "^\\*\\*\\* AI-DOING" nil t))
                ;; Gatherer's TOOL at level 5
                (should (re-search-forward "^\\*\\*\\*\\*\\* TOOL (Bash" nil t))
                ;; Coordinator's TOOL at level 4
                (should (re-search-forward "^\\*\\*\\*\\* TOOL (Agent" nil t))))
          (when (and gatherer-indirect (buffer-live-p gatherer-indirect))
            (kill-buffer gatherer-indirect))
          (when (and coord-indirect (buffer-live-p coord-indirect))
            (kill-buffer coord-indirect)))))))

;;; ---- IB-4.7: AI-DO depth validator ---------------------------------------

(ert-deftest gptel-org-agent-test-ai-do-rejects-nesting ()
  "Validator signals `user-error' when an AI-DO-family ancestor exists.
Attempting to place an AI-DO heading two levels under an existing
AI-DOING heading violates the Phase IB-4 heading-state grammar."
  (gptel-org-agent-test-with-buffer
      "* User Task          :user-task:
** AI-DOING Existing Agent   :main@agent:
Agent body.
*** Nested child
body
"
    (goto-char (point-min))
    (should (search-forward "Nested child" nil t))
    (should-error (gptel-org-agent--validate-ai-do-depth)
                  :type 'user-error)))

(ert-deftest gptel-org-agent-test-ai-do-accepts-top-level-task ()
  "Validator returns t when the ancestor is a plain user TODO.
An AI-DO heading placed directly under a non-AI-DO parent
satisfies the Phase IB-4 heading-state grammar."
  (gptel-org-agent-test-with-buffer
      "* User Task          :user-task:
** AI-DO New Task
"
    (goto-char (point-min))
    (should (search-forward "AI-DO New Task" nil t))
    (should (eq t (gptel-org-agent--validate-ai-do-depth)))))

(ert-deftest gptel-org-agent-test-ai-do-parent-rejects-ai-do-family ()
  "`gptel-org-agent--validate-ai-do-parent' rejects AI-DO-family parents.
Simulates the pre-insertion check done by
`gptel-org-agent--create-handover-heading' and `gptel-org-handoff'."
  (gptel-org-agent-test-with-buffer
      "* AI-DOING Mistaken user task   :user-task:
body
"
    (goto-char (point-min))
    (should (search-forward "Mistaken user task" nil t))
    (org-back-to-heading t)
    (should-error (gptel-org-agent--validate-ai-do-parent)
                  :type 'user-error)))

(ert-deftest gptel-org-agent-test-ai-do-parent-accepts-top-level ()
  "`gptel-org-agent--validate-ai-do-parent' accepts a plain user heading."
  (gptel-org-agent-test-with-buffer
      "* User Task          :user-task:
body
"
    (goto-char (point-min))
    (should (search-forward "User Task" nil t))
    (org-back-to-heading t)
    (should (eq t (gptel-org-agent--validate-ai-do-parent)))))

(ert-deftest gptel-org-agent-handover-heading-uses-terminator ()
  "`gptel-org-agent--create-handover-heading' inserts BEFORE TERMINE.

Regression test for the unified parent-aware insertion API: the
hand-rolled `org-end-of-subtree' path used to append the handover
heading AFTER the user task's TERMINE child.  After migration to
`gptel-org-ib-insert-child' with `:terminator-keyword \"TERMINE\"',
the heading must land BEFORE the TERMINE marker, on its own line."
  (let ((org-inhibit-startup t)
        (org-todo-keywords
         '((sequence "TODO" "AI-DO" "AI-DOING" "|" "DONE" "AI-DONE")))
        (gptel-org-todo-keywords '("AI-DO" "AI-DOING")))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert "*** TODO User task\n**** TERMINE\n")
      (goto-char (point-min))
      (search-forward "User task")
      (org-back-to-heading t)
      (let* ((test-buf (current-buffer))
             (user-task-pos (point))
             result)
        ;; Stub the IB resolver chain so the function operates on
        ;; the test buffer with point at the user task heading.
        ;; This drives `--create-handover-heading' through the same
        ;; code path as the live IB flow without requiring a real IB.
        (cl-letf (((symbol-function 'gptel-org-ib-registered-p)
                   (lambda (_buf) t))
                  ((symbol-function 'gptel-org-ib-base)
                   (lambda (_buf) test-buf))
                  ((symbol-function 'gptel-org-ib-resolve-agent-heading)
                   (lambda (_buf) user-task-pos)))
          (setq result
                (gptel-org-agent--create-handover-heading
                 "task body" "Sub task")))
        ;; Return value is the heading text (level 4 = parent + 1).
        (should (equal "**** AI-DO Sub task" result))
        (let ((content (buffer-substring-no-properties (point-min) (point-max))))
          ;; The handover heading is present.
          (should (string-match-p "^\\*\\*\\*\\* AI-DO Sub task$" content))
          ;; Body content was inserted.
          (should (string-match-p "^task body$" content))
          ;; The handover heading appears BEFORE TERMINE (regression).
          (let ((handover-pos
                 (string-match "^\\*\\*\\*\\* AI-DO Sub task" content))
                (termine-pos
                 (string-match "^\\*\\*\\*\\* TERMINE" content)))
            (should handover-pos)
            (should termine-pos)
            (should (< handover-pos termine-pos))))
        ;; Leading newline guard: the new heading must start at BOL,
        ;; not be merged into a prior content line.
        (goto-char (point-min))
        (re-search-forward "^\\*\\*\\*\\* AI-DO Sub task")
        (beginning-of-line)
        (should (or (= (point) (point-min))
                    (= (char-before) ?\n)))))))


;;; ---- IB-4.8: REQ-prefix invariant for tool headings ---------------------

(ert-deftest gptel-org-agent-test-tool-heading-req-prefix-p ()
  "Pure helper recognises `<REQ> <STATE> <summary>' form."
  (let ((gptel-org-agent-tool-confirm-keywords '("PENDING" "ALLOWED" "DENIED")))
    ;; With stars
    (should (equal "PENDING"
                   (gptel-org-agent--tool-heading-req-prefix-p
                    "*** PENDING BASH :command \"date\"")))
    (should (equal "ALLOWED"
                   (gptel-org-agent--tool-heading-req-prefix-p
                    "**** ALLOWED EVAL :expression \"(+ 2 3)\"")))
    (should (equal "DENIED"
                   (gptel-org-agent--tool-heading-req-prefix-p
                    "** DENIED BASH :command \"rm -rf /\"")))
    ;; Without stars (still a single heading line)
    (should (equal "PENDING"
                   (gptel-org-agent--tool-heading-req-prefix-p
                    "PENDING BASH :command \"date\"")))
    ;; No REQ prefix — bare STATE
    (should-not (gptel-org-agent--tool-heading-req-prefix-p
                 "*** BASH :command \"date\""))
    (should-not (gptel-org-agent--tool-heading-req-prefix-p
                 "*** AI-DOING main agent"))
    ;; REQ word alone (no STATE) is not a valid prefix
    (should-not (gptel-org-agent--tool-heading-req-prefix-p "*** PENDING"))
    ;; Non-REQ first word
    (should-not (gptel-org-agent--tool-heading-req-prefix-p
                 "*** TODO something"))
    ;; nil / empty
    (should-not (gptel-org-agent--tool-heading-req-prefix-p nil))
    (should-not (gptel-org-agent--tool-heading-req-prefix-p ""))))

(ert-deftest gptel-org-agent-test-tool-heading-strip-req-prefix ()
  "Pure helper removes a REQ prefix while preserving stars."
  (let ((gptel-org-agent-tool-confirm-keywords '("PENDING" "ALLOWED" "DENIED")))
    (should (equal "*** BASH :command \"date\""
                   (gptel-org-agent--tool-heading-strip-req-prefix
                    "*** PENDING BASH :command \"date\"")))
    (should (equal "**** EVAL :expression \"(+ 2 3)\""
                   (gptel-org-agent--tool-heading-strip-req-prefix
                    "**** ALLOWED EVAL :expression \"(+ 2 3)\"")))
    (should (equal "** BASH :command \"rm -rf /\""
                   (gptel-org-agent--tool-heading-strip-req-prefix
                    "** DENIED BASH :command \"rm -rf /\"")))
    ;; Already bare — unchanged
    (should (equal "*** BASH :command \"date\""
                   (gptel-org-agent--tool-heading-strip-req-prefix
                    "*** BASH :command \"date\"")))
    ;; Without stars
    (should (equal "BASH :command \"date\""
                   (gptel-org-agent--tool-heading-strip-req-prefix
                    "PENDING BASH :command \"date\"")))))

(ert-deftest gptel-org-agent-test-tool-heading-keeps-req-prefix-until-run ()
  "REQ prefix survives until `--update-tool-heading' runs.

Codifies the IB-4.8 invariant: a tool heading is written as
`<REQ> <STATE> <summary>' during the request phase (REQ ∈ PENDING /
ALLOWED / DENIED) and transitions to bare `<STATE> <summary>' only
after execution.

Flow:
  1. `--display-tool-calls' creates a PENDING heading  → REQ prefix present.
  2. Swap PENDING → ALLOWED via a raw heading rewrite (simulates the
     hook firing without the tool actually running yet) → REQ prefix
     still present (now \"ALLOWED\").
  3. `--update-tool-heading' runs with the tool result → REQ prefix
     gone."
  (let ((org-inhibit-startup t)
        (org-todo-keywords '((sequence "AI-DO" "AI-DOING" "DOING"
                                       "PENDING" "ALLOWED" "BASH"
                                       "FEEDBACK"
                                       "|" "AI-DONE" "DENIED")))
        (gptel-org-todo-keywords '("AI-DO" "AI-DOING")))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert "** DOING Run a tool\nDescription\n")
      (goto-char (point-min))
      (org-back-to-heading t)
      (let* ((gptel-org-subtree-context t)
             (gptel-org-use-todo-keywords t)
             (gptel-org-tasks-doing-keyword "AI-DOING")
             (gptel-org-agent-tool-confirm-keywords
              '("PENDING" "ALLOWED" "DENIED"))
             (base-buf (current-buffer))
             (marker (gptel-org-agent--create-subtree "main"))
             (indirect-buf nil))
        (unwind-protect
            (progn
              (setq indirect-buf
                    (gptel-org-agent--open-indirect-buffer base-buf marker))
              (let* ((tool-spec (gptel-make-tool
                                 :name "Bash"
                                 :function #'ignore
                                 :description "Run shell"
                                 :args '((:name "command"
                                          :type "string"
                                          :description "cmd"))
                                 :confirm t
                                 :include t))
                     (arg-plist '(:command "date"))
                     (tool-calls (list (list tool-spec arg-plist #'ignore)))
                     (position-marker
                      (with-current-buffer indirect-buf
                        (save-excursion
                          (goto-char (point-max))
                          (copy-marker (point) nil))))
                     (info (list :buffer indirect-buf
                                 :position position-marker
                                 :tracking-marker position-marker
                                 :callback #'gptel--insert-response
                                 :tools (list tool-spec))))
                ;; 1. Request phase: PENDING heading is created.
                (with-current-buffer indirect-buf
                  (gptel-org-agent--display-tool-calls tool-calls info))
                ;; Assert: heading has `<REQ=PENDING> <STATE=BASH> <summary>'.
                (with-current-buffer base-buf
                  (goto-char (point-min))
                  (should (re-search-forward
                           "^\\(\\*+ \\(?:PENDING\\) \\(?:BASH\\) .*\\)$"
                           nil t))
                  (let ((heading-line (match-string 1)))
                    (should (equal "PENDING"
                                   (gptel-org-agent--tool-heading-req-prefix-p
                                    heading-line)))
                    ;; Strip returns bare STATE form.
                    (should (string-match-p
                             "\\`\\*+ BASH "
                             (gptel-org-agent--tool-heading-strip-req-prefix
                              heading-line)))))
                ;; 2. Transition PENDING → ALLOWED at the raw heading
                ;;    level (no tool execution).  The REQ prefix must
                ;;    remain — just with a different REQ keyword.
                (with-current-buffer base-buf
                  (goto-char (point-min))
                  (should (re-search-forward "^\\(\\*+\\) PENDING " nil t))
                  (let ((inhibit-read-only t))
                    (replace-match (concat (match-string 1) " ALLOWED ")
                                   t t))
                  (goto-char (point-min))
                  (should (re-search-forward
                           "^\\(\\*+ ALLOWED BASH .*\\)$" nil t))
                  (let ((heading-line (match-string 1)))
                    (should (equal "ALLOWED"
                                   (gptel-org-agent--tool-heading-req-prefix-p
                                    heading-line)))))
                ;; 3. Execution: `--update-tool-heading' drops REQ.
                (with-current-buffer indirect-buf
                  (goto-char (point-max))
                  (should (gptel-org-agent--update-tool-heading
                           tool-spec arg-plist "Thu Apr 16 2026" "call_b1")))
                ;; Assert: heading is now bare `<STATE=BASH> <summary>'.
                (with-current-buffer base-buf
                  (goto-char (point-min))
                  ;; No PENDING/ALLOWED/DENIED prefix for this tool heading.
                  (should (= 0 (how-many "^\\*+ \\(?:PENDING\\|ALLOWED\\|DENIED\\) BASH "
                                         (point-min) (point-max))))
                  (goto-char (point-min))
                  (should (re-search-forward "^\\(\\*+ BASH .*\\)$" nil t))
                  (let ((heading-line (match-string 1)))
                    (should-not (gptel-org-agent--tool-heading-req-prefix-p
                                 heading-line))))))
          (when (and indirect-buf (buffer-live-p indirect-buf))
            (kill-buffer indirect-buf)))))))

(ert-deftest gptel-org-agent-test-non-agent-tool-triad-collapses ()
  "Non-Agent tools collapse their triad to three copies of the state keyword.
Codifies the IB-5.3 backward-compat invariant: only the Agent tool
resolves per-agent triads; every other tool keeps its single state
keyword so existing behaviour is unchanged."
  (should (equal '("BASH" "BASH" "BASH")
                 (gptel-org-agent--tool-state-triad "Bash" '(:command "date"))))
  (should (equal '("GREP" "GREP" "GREP")
                 (gptel-org-agent--tool-state-triad "Grep" '(:pattern "foo"))))
  ;; No args plist: still collapses.
  (should (equal '("EVAL" "EVAL" "EVAL")
                 (gptel-org-agent--tool-state-triad "Eval" nil)))
  ;; Agent tool without resolvable :subagent_type: collapses to
  ;; three copies of "AGENT" (the uppercased fallback).
  (should (equal '("AGENT" "AGENT" "AGENT")
                 (gptel-org-agent--tool-state-triad "Agent" nil))))

(ert-deftest gptel-org-agent-test-gatherer-transitions-gather-gathering-gathered ()
  "Agent-tool heading for the gatherer progresses GATHER → GATHERING → GATHERED.

Codifies the IB-5.3 invariant: when the `Agent' tool is dispatched
to an agent with a :state-words triad, the heading lifecycle uses
the triad words instead of the generic subagent_type:

  1. `--display-tool-calls' creates `PENDING GATHER ...'
  2. User flips PENDING → ALLOWED   → `ALLOWED GATHER ...'
  3. `--update-tool-heading' drops REQ and rewrites STATE to
     `GATHERING'
  4. Sub-agent cleanup transitions the heading to `GATHERED'
     (simulated by `gptel-org-agent--set-todo-keyword')."
  (let ((org-inhibit-startup t)
        (org-todo-keywords '((sequence "AI-DO" "AI-DOING" "DOING"
                                       "PENDING" "ALLOWED"
                                       "GATHER" "GATHERING"
                                       "FEEDBACK"
                                       "|" "AI-DONE" "DENIED"
                                       "GATHERED")))
        (gptel-org-todo-keywords '("AI-DO" "AI-DOING")))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert "** DOING Dispatch sub-agent\nDescription\n")
      (goto-char (point-min))
      (org-back-to-heading t)
      (let* ((gptel-org-subtree-context t)
             (gptel-org-use-todo-keywords t)
             (gptel-org-tasks-doing-keyword "AI-DOING")
             (gptel-org-tasks-done-keyword "AI-DONE")
             (gptel-org-agent-tool-confirm-keywords
              '("PENDING" "ALLOWED" "DENIED"))
             (base-buf (current-buffer))
             (marker (gptel-org-agent--create-subtree "main"))
             (indirect-buf nil))
        (unwind-protect
            ;; Stub `gptel-agent-state-words' to return a fixed triad
            ;; for "gatherer" so this test does not require
            ;; gptel-agent to be loaded.
            (cl-letf (((symbol-function 'gptel-agent-state-words)
                       (lambda (agent-name)
                         (if (equal agent-name "gatherer")
                             '("GATHER" "GATHERING" "GATHERED")
                           '("PENDING" "RUNNING" "DONE")))))
              (setq indirect-buf
                    (gptel-org-agent--open-indirect-buffer base-buf marker))
              (let* ((tool-spec (gptel-make-tool
                                 :name "Agent"
                                 :function #'ignore
                                 :description "Dispatch sub-agent"
                                 :args '((:name "subagent_type"
                                          :type "string"
                                          :description "agent")
                                         (:name "prompt"
                                          :type "string"
                                          :description "prompt"))
                                 :confirm t
                                 :include t))
                     (arg-plist '(:subagent_type "gatherer"
                                  :prompt "Find the thing"))
                     (tool-calls (list (list tool-spec arg-plist #'ignore)))
                     (position-marker
                      (with-current-buffer indirect-buf
                        (save-excursion
                          (goto-char (point-max))
                          (copy-marker (point) nil))))
                     (info (list :buffer indirect-buf
                                 :position position-marker
                                 :tracking-marker position-marker
                                 :callback #'gptel--insert-response
                                 :tools (list tool-spec))))
                ;; 1. Request phase: heading is `PENDING GATHER ...'.
                (with-current-buffer indirect-buf
                  (gptel-org-agent--display-tool-calls tool-calls info))
                (with-current-buffer base-buf
                  (goto-char (point-min))
                  (should (re-search-forward
                           "^\\*+ PENDING GATHER " nil t))
                  ;; Not GATHERER (the uppercased subagent_type) —
                  ;; triad[0] is GATHER.
                  (goto-char (point-min))
                  (should-not (re-search-forward
                               "^\\*+ PENDING GATHERER\\b" nil t)))
                ;; 2. User flips PENDING → ALLOWED.
                (with-current-buffer base-buf
                  (goto-char (point-min))
                  (should (re-search-forward "^\\(\\*+\\) PENDING " nil t))
                  (let ((inhibit-read-only t))
                    (replace-match (concat (match-string 1) " ALLOWED ")
                                   t t))
                  (goto-char (point-min))
                  (should (re-search-forward
                           "^\\*+ ALLOWED GATHER " nil t)))
                ;; 3. Execution: `--update-tool-heading' drops REQ and
                ;;    rewrites STATE to `GATHERING' (triad[1]).
                (with-current-buffer indirect-buf
                  (goto-char (point-max))
                  (should (gptel-org-agent--update-tool-heading
                           tool-spec arg-plist "sub-agent result" "call_g1")))
                (with-current-buffer base-buf
                  (goto-char (point-min))
                  ;; REQ dropped.
                  (should (= 0 (how-many
                                "^\\*+ \\(?:PENDING\\|ALLOWED\\|DENIED\\) GATHER "
                                (point-min) (point-max))))
                  ;; State is GATHERING.
                  (should (re-search-forward "^\\*+ GATHERING " nil t))
                  ;; Not plain GATHER (request word) anymore.
                  (goto-char (point-min))
                  (should (= 0 (how-many "^\\*+ GATHER " (point-min) (point-max)))))
                ;; 4. Sub-agent cleanup: triad[2] is `GATHERED'.
                ;;    Simulate by calling --set-todo-keyword on the
                ;;    heading directly (mirrors what
                ;;    `gptel-agent-subtree--cleanup' does on DONE).
                (with-current-buffer base-buf
                  (goto-char (point-min))
                  (should (re-search-forward "^\\*+ GATHERING " nil t))
                  (beginning-of-line)
                  ;; Register the DONE word so --set-todo-keyword
                  ;; accepts it.
                  (gptel-org--ensure-todo-state
                   "GATHERED" gptel-org--agent-state-face t)
                  (gptel-org-agent--set-todo-keyword "GATHERED")
                  (goto-char (point-min))
                  (should (re-search-forward "^\\*+ GATHERED " nil t))
                  (should (= 0 (how-many "^\\*+ GATHERING "
                                         (point-min) (point-max)))))))
          (when (and indirect-buf (buffer-live-p indirect-buf))
            (kill-buffer indirect-buf)))))))

(ert-deftest gptel-org-agent-test-pending-entry-stores-ib-and-marker ()
  "PENDING tool-call entry stores :pending-ib and :heading-marker.

Codifies the IB-7 invariant: each pending tool-call heading lives
in its own dedicated indirect buffer so that subsequent state
transitions can be performed via real `org-todo' calls instead of
raw text rewriting.

The hash-table entry must therefore include:
  - :pending-ib    a live indirect buffer holding the heading
  - :heading-marker a marker pointing to the heading line"
  (let ((org-inhibit-startup t)
        (org-todo-keywords '((sequence "AI-DO" "AI-DOING" "DOING"
                                       "PENDING" "ALLOWED" "BASH"
                                       "FEEDBACK"
                                       "|" "AI-DONE" "DENIED")))
        (gptel-org-todo-keywords '("AI-DO" "AI-DOING")))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert "** DOING Run a tool\nDescription\n")
      (goto-char (point-min))
      (org-back-to-heading t)
      (let* ((gptel-org-subtree-context t)
             (gptel-org-use-todo-keywords t)
             (gptel-org-tasks-doing-keyword "AI-DOING")
             (gptel-org-agent-tool-confirm-keywords
              '("PENDING" "ALLOWED" "DENIED"))
             (base-buf (current-buffer))
             (marker (gptel-org-agent--create-subtree "main"))
             (indirect-buf nil)
             (pending-ib nil))
        (clrhash gptel-org-agent--pending-tool-calls)
        (unwind-protect
            (progn
              (setq indirect-buf
                    (gptel-org-agent--open-indirect-buffer base-buf marker))
              (let* ((tool-spec (gptel-make-tool
                                 :name "Bash"
                                 :function #'ignore
                                 :description "Run shell"
                                 :args '((:name "command"
                                          :type "string"
                                          :description "cmd"))
                                 :confirm t
                                 :include t))
                     (arg-plist '(:command "date"))
                     (tool-calls (list (list tool-spec arg-plist #'ignore)))
                     (position-marker
                      (with-current-buffer indirect-buf
                        (save-excursion
                          (goto-char (point-max))
                          (copy-marker (point) nil))))
                     (info (list :buffer indirect-buf
                                 :position position-marker
                                 :tracking-marker position-marker
                                 :callback #'gptel--insert-response
                                 :tools (list tool-spec))))
                (with-current-buffer indirect-buf
                  (gptel-org-agent--display-tool-calls tool-calls info))
                ;; Exactly one entry was registered.
                (should (= 1 (hash-table-count
                              gptel-org-agent--pending-tool-calls)))
                (maphash
                 (lambda (_id entry)
                   ;; New keys present and live.
                   (should (plist-member entry :pending-ib))
                   (should (plist-member entry :heading-marker))
                   (let ((ib (plist-get entry :pending-ib))
                         (m (plist-get entry :heading-marker)))
                     (setq pending-ib ib)
                     (should (bufferp ib))
                     (should (buffer-live-p ib))
                     (should (markerp m))
                     (should (marker-buffer m))
                     ;; The pending IB is registered with the IB
                     ;; framework.
                     (should (gptel-org-ib-registered-p ib))
                     ;; Point-min in the pending IB is the heading.
                     (with-current-buffer ib
                       (save-excursion
                         (goto-char (point-min))
                         (should (org-at-heading-p))))))
                 gptel-org-agent--pending-tool-calls)))
          (clrhash gptel-org-agent--pending-tool-calls)
          (when (and pending-ib (buffer-live-p pending-ib))
            (kill-buffer pending-ib))
          (when (and indirect-buf (buffer-live-p indirect-buf))
            (kill-buffer indirect-buf)))))))

(ert-deftest gptel-org-agent-test-update-tool-heading-uses-org-todo ()
  "`--update-tool-heading' transitions state via real `org-todo' on the IB.

Architectural invariant (IB-7): the PENDING tool-call heading lives
in its own indirect buffer.  PENDING → ALLOWED → DOING is performed
via `org-todo' calls on that single heading, not by deleting and
re-inserting heading lines.

Concretely:
  1. `--display-tool-calls' creates `PENDING GATHER ...' in a
     dedicated pending-ib.
  2. User flips PENDING → ALLOWED (simulated by raw text rewrite
     in the base buffer).
  3. `--update-tool-heading' rewrites the heading IN the pending-ib
     via `org-todo' to GATHERING.

Asserts:
  - Heading line count in the base buffer is preserved across the
    transition (no NEW heading is appended after :tracking-marker).
  - The heading's TODO state, as read by `org-get-todo-state',
    becomes the DOING-state keyword.
  - The pending-ib remains alive after the transition (IB lifecycle
    survives until a separate cleanup step)."
  (let ((org-inhibit-startup t)
        (org-todo-keywords '((sequence "AI-DO" "AI-DOING" "DOING"
                                       "PENDING" "ALLOWED"
                                       "GATHER" "GATHERING"
                                       "FEEDBACK"
                                       "|" "AI-DONE" "DENIED"
                                       "GATHERED")))
        (gptel-org-todo-keywords '("AI-DO" "AI-DOING")))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert "** DOING Dispatch sub-agent\nDescription\n")
      (goto-char (point-min))
      (org-back-to-heading t)
      (let* ((gptel-org-subtree-context t)
             (gptel-org-use-todo-keywords t)
             (gptel-org-tasks-doing-keyword "AI-DOING")
             (gptel-org-tasks-done-keyword "AI-DONE")
             (gptel-org-agent-tool-confirm-keywords
              '("PENDING" "ALLOWED" "DENIED"))
             (base-buf (current-buffer))
             (marker (gptel-org-agent--create-subtree "main"))
             (indirect-buf nil)
             (pending-ib nil))
        (clrhash gptel-org-agent--pending-tool-calls)
        (unwind-protect
            (cl-letf (((symbol-function 'gptel-agent-state-words)
                       (lambda (agent-name)
                         (if (equal agent-name "gatherer")
                             '("GATHER" "GATHERING" "GATHERED")
                           '("PENDING" "RUNNING" "DONE")))))
              (setq indirect-buf
                    (gptel-org-agent--open-indirect-buffer base-buf marker))
              (let* ((tool-spec (gptel-make-tool
                                 :name "Agent"
                                 :function #'ignore
                                 :description "Dispatch sub-agent"
                                 :args '((:name "subagent_type"
                                          :type "string"
                                          :description "agent")
                                         (:name "prompt"
                                          :type "string"
                                          :description "prompt"))
                                 :confirm t
                                 :include t))
                     (arg-plist '(:subagent_type "gatherer"
                                  :prompt "Find the thing"))
                     (tool-calls (list (list tool-spec arg-plist #'ignore)))
                     ;; Insert response text BEFORE the PENDING heading
                     ;; would be created, so :tracking-marker sits in
                     ;; the agent body — mirroring the production
                     ;; scenario where the bug used to manifest.
                     (position-marker
                      (with-current-buffer indirect-buf
                        (save-excursion
                          (goto-char (point-min))
                          (org-end-of-meta-data t)
                          (insert "Some streamed AI text.\n")
                          (copy-marker (point) nil))))
                     (info (list :buffer indirect-buf
                                 :position position-marker
                                 :tracking-marker position-marker
                                 :callback #'gptel--insert-response
                                 :tools (list tool-spec))))
                ;; 1. Request phase.
                (with-current-buffer indirect-buf
                  (gptel-org-agent--display-tool-calls tool-calls info))
                ;; Capture the pending IB for later assertions.
                (maphash (lambda (_id e)
                           (setq pending-ib (plist-get e :pending-ib)))
                         gptel-org-agent--pending-tool-calls)
                (should (bufferp pending-ib))
                (should (buffer-live-p pending-ib))
                ;; 2. User flips PENDING → ALLOWED via raw text
                ;;    (simulating the hook firing without execution).
                (with-current-buffer base-buf
                  (goto-char (point-min))
                  (should (re-search-forward "^\\(\\*+\\) PENDING " nil t))
                  (let ((inhibit-read-only t))
                    (replace-match (concat (match-string 1) " ALLOWED ")
                                   t t)))
                ;; 3. Execution: --update-tool-heading.  Critically,
                ;;    we invoke it via the FULL advice path, which is
                ;;    what the FSM does in production — this is what
                ;;    used to fall through to orig-fn (creating a NEW
                ;;    EXECUTING heading) when the backward search
                ;;    failed.
                (let ((advice-orig-called nil)
                      (orig-fn (lambda (&rest _)
                                 (setq advice-orig-called t))))
                  (with-current-buffer indirect-buf
                    (gptel-org-agent--display-tool-results-advice
                     orig-fn
                     (list (list tool-spec arg-plist "sub-agent result"))
                     (plist-put info :tool-use
                                (list (list :name "Agent"
                                            :id "call_g1")))))
                  ;; The advice MUST NOT have fallen through to
                  ;; orig-fn — the IB-based lookup found the entry.
                  (should-not advice-orig-called))
                ;; The heading is rewritten in-place (no duplicate
                ;; GATHERING heading appended).  Counting GATHERING
                ;; headings yields exactly 1.
                (with-current-buffer base-buf
                  (should (= 1 (how-many "^\\*+ GATHERING "
                                         (point-min) (point-max))))
                  ;; The PENDING/ALLOWED tool heading is gone — no
                  ;; orphan request-state line for this tool call.
                  (should (= 0 (how-many
                                "^\\*+ \\(?:PENDING\\|ALLOWED\\|DENIED\\) GATHER "
                                (point-min) (point-max)))))
                ;; The TODO state on the heading is GATHERING — set
                ;; via real `org-todo', readable via
                ;; `org-get-todo-state'.
                (with-current-buffer pending-ib
                  (save-excursion
                    (goto-char (point-min))
                    (should (org-at-heading-p))
                    (should (equal "GATHERING"
                                   (org-get-todo-state)))))
                ;; The dedicated IB is still alive after the
                ;; transition (lifecycle survives the DOING phase).
                (should (buffer-live-p pending-ib))))
          (clrhash gptel-org-agent--pending-tool-calls)
          (when (and pending-ib (buffer-live-p pending-ib))
            (kill-buffer pending-ib))
          (when (and indirect-buf (buffer-live-p indirect-buf))
            (kill-buffer indirect-buf)))))))

(ert-deftest gptel-org-agent-test-update-tool-heading-executor-triad ()
  "Executor agent heading transitions EXECUTE → EXECUTING via `org-todo'.

Mirrors `gptel-org-agent-test-update-tool-heading-uses-org-todo'
with the executor triad (\"EXECUTE\" \"EXECUTING\" \"EXECUTED\")."
  (let ((org-inhibit-startup t)
        (org-todo-keywords '((sequence "AI-DO" "AI-DOING" "DOING"
                                       "PENDING" "ALLOWED"
                                       "EXECUTE" "EXECUTING"
                                       "FEEDBACK"
                                       "|" "AI-DONE" "DENIED"
                                       "EXECUTED")))
        (gptel-org-todo-keywords '("AI-DO" "AI-DOING")))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert "** DOING Dispatch executor\nDescription\n")
      (goto-char (point-min))
      (org-back-to-heading t)
      (let* ((gptel-org-subtree-context t)
             (gptel-org-use-todo-keywords t)
             (gptel-org-tasks-doing-keyword "AI-DOING")
             (gptel-org-tasks-done-keyword "AI-DONE")
             (gptel-org-agent-tool-confirm-keywords
              '("PENDING" "ALLOWED" "DENIED"))
             (base-buf (current-buffer))
             (marker (gptel-org-agent--create-subtree "main"))
             (indirect-buf nil)
             (pending-ib nil))
        (clrhash gptel-org-agent--pending-tool-calls)
        (unwind-protect
            (cl-letf (((symbol-function 'gptel-agent-state-words)
                       (lambda (agent-name)
                         (if (equal agent-name "executor")
                             '("EXECUTE" "EXECUTING" "EXECUTED")
                           '("PENDING" "RUNNING" "DONE")))))
              (setq indirect-buf
                    (gptel-org-agent--open-indirect-buffer base-buf marker))
              (let* ((tool-spec (gptel-make-tool
                                 :name "Agent"
                                 :function #'ignore
                                 :description "Dispatch sub-agent"
                                 :args '((:name "subagent_type"
                                          :type "string"
                                          :description "agent")
                                         (:name "prompt"
                                          :type "string"
                                          :description "prompt"))
                                 :confirm t
                                 :include t))
                     (arg-plist '(:subagent_type "executor"
                                  :prompt "Run the thing"))
                     (tool-calls (list (list tool-spec arg-plist #'ignore)))
                     (position-marker
                      (with-current-buffer indirect-buf
                        (save-excursion
                          (goto-char (point-min))
                          (org-end-of-meta-data t)
                          (insert "Some streamed AI text.\n")
                          (copy-marker (point) nil))))
                     (info (list :buffer indirect-buf
                                 :position position-marker
                                 :tracking-marker position-marker
                                 :callback #'gptel--insert-response
                                 :tools (list tool-spec))))
                ;; 1. Request phase.
                (with-current-buffer indirect-buf
                  (gptel-org-agent--display-tool-calls tool-calls info))
                (maphash (lambda (_id e)
                           (setq pending-ib (plist-get e :pending-ib)))
                         gptel-org-agent--pending-tool-calls)
                (should (bufferp pending-ib))
                ;; Heading is `<stars> PENDING EXECUTE ...'
                (with-current-buffer base-buf
                  (goto-char (point-min))
                  (should (re-search-forward "^\\*+ PENDING EXECUTE "
                                             nil t)))
                ;; 2. ALLOWED via raw text rewrite.
                (with-current-buffer base-buf
                  (goto-char (point-min))
                  (should (re-search-forward "^\\(\\*+\\) PENDING " nil t))
                  (let ((inhibit-read-only t))
                    (replace-match (concat (match-string 1) " ALLOWED ")
                                   t t)))
                ;; 3. Execution: heading transitions EXECUTING.
                (with-current-buffer indirect-buf
                  (should (gptel-org-agent--update-tool-heading
                           tool-spec arg-plist "ok" "call_e1")))
                ;; Heading rewritten in-place (no duplicate EXECUTING).
                (with-current-buffer base-buf
                  (should (= 1 (how-many "^\\*+ EXECUTING "
                                         (point-min) (point-max))))
                  (should (= 0 (how-many
                                "^\\*+ \\(?:PENDING\\|ALLOWED\\|DENIED\\) EXECUTE "
                                (point-min) (point-max)))))
                ;; State is EXECUTING via real `org-todo'.
                (with-current-buffer pending-ib
                  (save-excursion
                    (goto-char (point-min))
                    (should (equal "EXECUTING"
                                   (org-get-todo-state)))))
                (should (buffer-live-p pending-ib))))
          (clrhash gptel-org-agent--pending-tool-calls)
          (when (and pending-ib (buffer-live-p pending-ib))
            (kill-buffer pending-ib))
          (when (and indirect-buf (buffer-live-p indirect-buf))
            (kill-buffer indirect-buf)))))))

;;; ---- IB-7 PENDING-IB reuse for sub-agent dispatch ------------------------
;;
;; These tests codify the unified-pattern decisions in the AI-DO
;; "EXECUTING should be state transition of ALLOWED EXECUTE heading"
;; TODO (gptel-ai.org).  Rationale: a single PENDING tool-call IB is
;; created at request-arrival time and re-used for the entire
;; lifecycle PENDING → ALLOWED → EXECUTING → EXECUTED.  The Agent
;; tool path (sub-agent dispatch) MUST reuse that IB instead of
;; creating a fresh sub-agent subtree.

(ert-deftest gptel-org-agent-test-pending-tool-subtree-info-reuses-ib ()
  "`--pending-tool-subtree-info' returns the PENDING entry's IB+marker.

Codifies IB-7 reuse: when a pending-id exists in
`gptel-org-agent--pending-tool-calls', the helper returns a
subtree-info plist whose `:indirect-buffer' is the entry's
`:pending-ib' and `:heading-marker' is the entry's heading marker.
The position-marker is inside the IB before its TERMINE child."
  (let ((org-inhibit-startup t)
        (org-todo-keywords '((sequence "AI-DO" "AI-DOING" "DOING"
                                       "PENDING" "ALLOWED"
                                       "EXECUTE" "EXECUTING"
                                       "FEEDBACK"
                                       "|" "AI-DONE" "DENIED"
                                       "EXECUTED")))
        (gptel-org-todo-keywords '("AI-DO" "AI-DOING")))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert "** DOING Dispatch executor\nDescription\n")
      (goto-char (point-min))
      (org-back-to-heading t)
      (let* ((gptel-org-subtree-context t)
             (gptel-org-use-todo-keywords t)
             (gptel-org-tasks-doing-keyword "AI-DOING")
             (gptel-org-agent-tool-confirm-keywords
              '("PENDING" "ALLOWED" "DENIED"))
             (base-buf (current-buffer))
             (marker (gptel-org-agent--create-subtree "main"))
             (indirect-buf nil)
             (pending-ib nil))
        (clrhash gptel-org-agent--pending-tool-calls)
        (unwind-protect
            (cl-letf (((symbol-function 'gptel-agent-state-words)
                       (lambda (agent-name)
                         (if (equal agent-name "executor")
                             '("EXECUTE" "EXECUTING" "EXECUTED")
                           '("PENDING" "RUNNING" "DONE")))))
              (setq indirect-buf
                    (gptel-org-agent--open-indirect-buffer base-buf marker))
              (let* ((tool-spec (gptel-make-tool
                                 :name "Agent"
                                 :function #'ignore
                                 :description "Dispatch sub-agent"
                                 :args '((:name "subagent_type"
                                          :type "string"
                                          :description "agent")
                                         (:name "prompt"
                                          :type "string"
                                          :description "prompt"))
                                 :confirm t
                                 :include t))
                     (arg-plist '(:subagent_type "executor"
                                  :prompt "Run the thing"))
                     (tool-calls (list (list tool-spec arg-plist #'ignore)))
                     (position-marker
                      (with-current-buffer indirect-buf
                        (save-excursion
                          (goto-char (point-max))
                          (copy-marker (point) nil))))
                     (info (list :buffer indirect-buf
                                 :position position-marker
                                 :tracking-marker position-marker
                                 :callback #'gptel--insert-response
                                 :tools (list tool-spec)))
                     (pending-id nil)
                     (entry-pending-ib nil)
                     (entry-heading-marker nil))
                ;; Create the PENDING entry.
                (with-current-buffer indirect-buf
                  (gptel-org-agent--display-tool-calls tool-calls info))
                ;; Look up the entry data.
                (maphash (lambda (id e)
                           (setq pending-id id)
                           (setq entry-pending-ib (plist-get e :pending-ib))
                           (setq entry-heading-marker
                                 (plist-get e :heading-marker)))
                         gptel-org-agent--pending-tool-calls)
                (setq pending-ib entry-pending-ib)
                (should (stringp pending-id))
                (should (bufferp entry-pending-ib))
                (should (markerp entry-heading-marker))
                ;; Call the helper.  It must return the same IB + marker.
                (let ((subtree-info
                       (gptel-org-agent--pending-tool-subtree-info
                        pending-id)))
                  (should subtree-info)
                  (should (eq (plist-get subtree-info :indirect-buffer)
                              entry-pending-ib))
                  (should (eq (plist-get subtree-info :heading-marker)
                              entry-heading-marker))
                  ;; Position marker is inside the PENDING IB, before
                  ;; TERMINE (which is the IB's last child per the
                  ;; universal TERMINE invariant).
                  (let ((pm (plist-get subtree-info :position-marker)))
                    (should (markerp pm))
                    (should (eq (marker-buffer pm) entry-pending-ib))
                    (with-current-buffer entry-pending-ib
                      (save-excursion
                        (goto-char (point-min))
                        ;; TERMINE child must exist.
                        (should (gptel-org-ib-find-terminator "TERMINE"))
                        ;; Position marker must be at-or-before TERMINE.
                        (let ((termine-pos
                               (gptel-org-ib-find-terminator "TERMINE")))
                          (should (<= (marker-position pm)
                                      termine-pos)))))))
                ;; Returns nil for unknown pending-id.
                (should-not
                 (gptel-org-agent--pending-tool-subtree-info
                  "nonexistent-id"))
                ;; Returns nil for nil pending-id.
                (should-not (gptel-org-agent--pending-tool-subtree-info nil))))
          (clrhash gptel-org-agent--pending-tool-calls)
          (when (and pending-ib (buffer-live-p pending-ib))
            (kill-buffer pending-ib))
          (when (and indirect-buf (buffer-live-p indirect-buf))
            (kill-buffer indirect-buf)))))))

(ert-deftest gptel-org-agent-test-accept-tool-calls-binds-dispatching-id ()
  "`--accept-tool-calls' binds `--dispatching-pending-id' for tool dispatch.

Codifies the dynbind contract: when `--accept-tool-calls' is called
with a non-nil PENDING-ID argument, `gptel-org-agent--dispatching-pending-id'
is bound to that value while the tool function is invoked.  This is
the mechanism by which `gptel-agent--task' locates the PENDING IB."
  (let ((org-inhibit-startup t)
        (org-todo-keywords '((sequence "AI-DO" "AI-DOING" "DOING"
                                       "PENDING" "ALLOWED" "BASH"
                                       "FEEDBACK"
                                       "|" "AI-DONE" "DENIED")))
        (gptel-org-todo-keywords '("AI-DO" "AI-DOING")))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert "** DOING Run a tool\nDescription\n")
      (let* ((gptel-org-subtree-context t)
             (observed-pending-id 'unset)
             (tool-spec (gptel-make-tool
                         :name "Bash"
                         :function (lambda (_command)
                                     (setq observed-pending-id
                                           gptel-org-agent--dispatching-pending-id)
                                     "result")
                         :description "Run shell"
                         :args '((:name "command"
                                  :type "string"
                                  :description "cmd"))
                         :confirm t
                         :include t))
             (arg-plist '(:command "date"))
             (tool-calls (list (list tool-spec arg-plist
                                     (lambda (_) nil))))
             (info (list :buffer (current-buffer)
                         :tools (list tool-spec))))
        ;; With pending-id supplied: dynbind reflects it during the
        ;; tool function call.
        (gptel-org-agent--accept-tool-calls tool-calls info "test-pending-42")
        (should (equal observed-pending-id "test-pending-42"))
        ;; After return: the dynbind is unbound (let-scope ended).
        (should-not gptel-org-agent--dispatching-pending-id)
        ;; Without pending-id: dynbind is nil during the tool call too.
        (setq observed-pending-id 'unset)
        (gptel-org-agent--accept-tool-calls tool-calls info)
        (should (eq observed-pending-id nil))))))

(ert-deftest gptel-org-agent-test-subagent-dispatch-no-duplicate-heading ()
  "Sub-agent dispatch via PENDING IB does not create a duplicate heading.

Codifies the load-bearing decision in the AI-DO \"EXECUTING\" TODO:
when the Agent tool is dispatched from a PENDING tool-call entry,
the sub-agent FSM re-uses the existing PENDING IB instead of
creating a fresh sub-agent subtree.  The base buffer must therefore
contain exactly ONE heading for the dispatch (the original PENDING
heading), not two.

This test simulates the dispatch by directly invoking the
`gptel-org-agent--pending-tool-subtree-info' helper (the mechanism
`gptel-agent--task' calls when `--dispatching-pending-id' is
bound).  An end-to-end test that exercises `gptel-agent--task'
itself lives in the gptel-agent test suite (which has gptel-agent
on its load path).

Asserts:
  - The helper returns the EXISTING PENDING IB, not a new IB.
  - No fresh sub-agent subtree heading is created in the base
    buffer (no `:executor@...:' tag, no bare EXECUTING heading).
  - The base buffer's heading count is unchanged across the call."
  (let ((org-inhibit-startup t)
        (org-todo-keywords '((sequence "AI-DO" "AI-DOING" "DOING"
                                       "PENDING" "ALLOWED"
                                       "EXECUTE" "EXECUTING"
                                       "FEEDBACK"
                                       "|" "AI-DONE" "DENIED"
                                       "EXECUTED")))
        (gptel-org-todo-keywords '("AI-DO" "AI-DOING")))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert "** DOING Dispatch executor\nDescription\n")
      (goto-char (point-min))
      (org-back-to-heading t)
      (let* ((gptel-org-subtree-context t)
             (gptel-org-use-todo-keywords t)
             (gptel-org-tasks-doing-keyword "AI-DOING")
             (gptel-org-agent-tool-confirm-keywords
              '("PENDING" "ALLOWED" "DENIED"))
             (base-buf (current-buffer))
             (marker (gptel-org-agent--create-subtree "main"))
             (indirect-buf nil)
             (pending-ib nil)
             )
        (clrhash gptel-org-agent--pending-tool-calls)
        (unwind-protect
            (cl-letf
                (((symbol-function 'gptel-agent-state-words)
                  (lambda (agent-name)
                    (if (equal agent-name "executor")
                        '("EXECUTE" "EXECUTING" "EXECUTED")
                      '("PENDING" "RUNNING" "DONE")))))
              (setq indirect-buf
                    (gptel-org-agent--open-indirect-buffer base-buf marker))
              (let* ((tool-spec (gptel-make-tool
                                 :name "Agent"
                                 :function #'ignore
                                 :description "Dispatch sub-agent"
                                 :args '((:name "subagent_type"
                                          :type "string"
                                          :description "agent")
                                         (:name "prompt"
                                          :type "string"
                                          :description "prompt"))
                                 :confirm t
                                 :include t))
                     (arg-plist '(:subagent_type "executor"
                                  :prompt "date"))
                     (tool-calls (list (list tool-spec arg-plist
                                             (lambda (_) nil))))
                     (position-marker
                      (with-current-buffer indirect-buf
                        (save-excursion
                          (goto-char (point-max))
                          (copy-marker (point) nil))))
                     (info (list :buffer indirect-buf
                                 :position position-marker
                                 :tracking-marker position-marker
                                 :callback #'gptel--insert-response
                                 :tools (list tool-spec)))
                     (captured-pending-id nil))
                ;; 1. Request phase: create PENDING entry + heading + IB.
                (with-current-buffer indirect-buf
                  (gptel-org-agent--display-tool-calls tool-calls info))
                (maphash (lambda (id e)
                           (setq captured-pending-id id)
                           (setq pending-ib (plist-get e :pending-ib)))
                         gptel-org-agent--pending-tool-calls)
                (should (stringp captured-pending-id))
                (should (bufferp pending-ib))
                ;; Snapshot the base-buffer heading count before
                ;; "dispatch" (PENDING heading already created).
                (let ((heading-count-before
                       (with-current-buffer base-buf
                         (how-many "^\\*+ " (point-min) (point-max)))))
                  ;; 2. ALLOWED via raw text rewrite.
                  (with-current-buffer base-buf
                    (goto-char (point-min))
                    (should (re-search-forward "^\\(\\*+\\) PENDING " nil t))
                    (let ((inhibit-read-only t))
                      (replace-match (concat (match-string 1) " ALLOWED ")
                                     t t)))
                  ;; 3. Simulate dispatch: bind dynbind and call helper.
                  ;; This is what `gptel-agent--task' does internally
                  ;; when the dynbind is set.
                  (let* ((gptel-org-agent--dispatching-pending-id
                          captured-pending-id)
                         (subtree-info
                          (gptel-org-agent--pending-tool-subtree-info
                           captured-pending-id)))
                    ;; ASSERTION 1: helper returned the EXISTING IB.
                    (should subtree-info)
                    (should (eq (plist-get subtree-info :indirect-buffer)
                                pending-ib)))
                  ;; ASSERTION 2: no new heading was inserted into
                  ;; the base buffer.
                  (with-current-buffer base-buf
                    (should (= heading-count-before
                               (how-many "^\\*+ "
                                         (point-min) (point-max))))
                    ;; No bare EXECUTING heading.
                    (goto-char (point-min))
                    (should (= 0 (how-many "^\\*+ EXECUTING "
                                           (point-min) (point-max))))
                    ;; No `:executor@' tag (would imply setup-task-subtree).
                    (goto-char (point-min))
                    (should-not (re-search-forward ":executor@[^:]+:"
                                                   nil t))))))
          (clrhash gptel-org-agent--pending-tool-calls)
          (when (and pending-ib (buffer-live-p pending-ib))
            (kill-buffer pending-ib))
          (when (and indirect-buf (buffer-live-p indirect-buf))
            (kill-buffer indirect-buf)))))))

(provide 'gptel-org-agent-test)
;;; gptel-org-agent-test.el ends here