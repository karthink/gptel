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
             (goto-char (point-max))
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
              ;; Simulate AI response
              (with-current-buffer indirect-buf
                (goto-char (point-max))
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

              ;; Step 7: Simulate researcher writing its final response
              (with-current-buffer researcher-indirect
                (goto-char (point-max))
                (insert "* AI Gatherer Output\nThe date is Tue Apr 14.\n"))

              ;; Verify coordinator heading STILL at level 3
              (with-current-buffer coord-indirect
                (goto-char (point-min))
                (should (= 3 (org-current-level))))

              ;; Step 8: Simulate researcher completion
              (when (buffer-live-p researcher-indirect)
                (gptel-org-agent--close-indirect-buffer researcher-indirect t)
                (setq researcher-indirect nil))

              ;; Step 9: Simulate coordinator writing final response
              (with-current-buffer coord-indirect
                (goto-char (point-max))
                (insert "The chain worked: coordinator -> researcher -> gatherer.\nResult: Tue Apr 14 13:06:37 EEST 2026\n"))

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

(provide 'gptel-org-agent-test)
;;; gptel-org-agent-test.el ends here
