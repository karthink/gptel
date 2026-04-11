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
  "Insert exactly one HI heading as a child of the agent subtree.
Verifies the heading is created in the base buffer at agent-level + 1
\(same level as AI response headings), as a child of the agent heading."
  (let ((org-inhibit-startup t)
        (org-todo-keywords '((sequence "AI-DO" "AI-DOING" "HI" "|" "AI-DONE")))
        (gptel-org-todo-keywords '("AI-DO" "AI-DOING")))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert "* AI-DO Implement feature\nDescription\n")
      (goto-char (point-min))
      (org-back-to-heading t)
      (let* ((gptel-org-subtree-context t)
             (gptel-org-use-todo-keywords t)
             (gptel-org-user-keyword "HI")
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
              ;; Verify HI heading in base buffer
              (with-current-buffer base-buf
                (goto-char (point-min))
                ;; Should have exactly one HI heading
                (should (= 1
                           (how-many "^\\*\\*\\* HI"
                                     (point-min) (point-max))))
                ;; Find the agent heading
                (should (re-search-forward "^\\*\\* AI-DOING Implement feature" nil t))
                (goto-char (match-beginning 0))
                (let ((agent-level (org-current-level)))
                  ;; Find the HI heading inside the agent subtree
                  (should (re-search-forward "^\\*\\*\\* HI" nil t))
                  (goto-char (match-beginning 0))
                  ;; HI heading should be at agent-level + 1 (child of agent)
                  (should (= (1+ agent-level) (org-current-level))))))
          (when (and indirect-buf (buffer-live-p indirect-buf))
            (kill-buffer indirect-buf)))))))

(ert-deftest gptel-org-agent-test-insert-user-heading-not-duplicated ()
  "Repeated insert-user-heading calls should not create duplicate HI headings."
  (let ((org-inhibit-startup t)
        (org-todo-keywords '((sequence "AI-DO" "AI-DOING" "HI" "|" "AI-DONE")))
        (gptel-org-todo-keywords '("AI-DO" "AI-DOING")))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert "* AI-DO Implement feature\nDescription\n")
      (goto-char (point-min))
      (org-back-to-heading t)
      (let* ((gptel-org-subtree-context t)
             (gptel-org-use-todo-keywords t)
             (gptel-org-user-keyword "HI")
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
              ;; Should have exactly 1 HI heading, not 2
              (with-current-buffer base-buf
                (should (= 1
                           (how-many "^\\*\\*\\* HI"
                                     (point-min) (point-max))))))
          (when (and indirect-buf (buffer-live-p indirect-buf))
            (kill-buffer indirect-buf)))))))

(ert-deftest gptel-org-agent-test-insert-user-heading-level-matches-ai-response ()
  "User heading should be at same level as AI response headings (agent-level + 1).
Reproduces a bug where user heading appeared at agent-level (sibling of agent)
instead of agent-level + 1 (child of agent, same level as AI responses).

Expected structure:
  ** DOING Task              <- level 2 (parent)
  *** AI-DOING Task          <- level 3 (agent heading)
  **** AI Response           <- level 4 (AI response = agent + 1)
  **** HI                    <- level 4 (user = agent + 1, CORRECT)

Bug produced:
  *** HI                     <- level 3 (user = agent-level, WRONG)"
  (let ((org-inhibit-startup t)
        (org-todo-keywords '((sequence "AI-DO" "AI-DOING" "DOING" "HI" "|" "AI-DONE" "DONE")))
        (gptel-org-todo-keywords '("AI-DO" "AI-DOING")))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      ;; Use level 2 parent to match the bug report structure
      (insert "** DOING Run simple gatherer task\nDescription\n")
      (goto-char (point-min))
      (org-back-to-heading t)
      (let* ((gptel-org-subtree-context t)
             (gptel-org-use-todo-keywords t)
             (gptel-org-user-keyword "HI")
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
                ;; Find the agent heading — should be level 3
                (should (re-search-forward "^\\*\\*\\* AI-DOING" nil t))
                (goto-char (match-beginning 0))
                (let ((agent-level (org-current-level)))
                  (should (= 3 agent-level))
                  ;; Find the AI response heading — should be level 4
                  (should (re-search-forward "^\\*\\*\\*\\* AI Getting current time" nil t))
                  (goto-char (match-beginning 0))
                  (should (= 4 (org-current-level)))
                  ;; Find the HI heading — should ALSO be level 4 (not 3!)
                  (should (re-search-forward "^\\*\\*\\*\\* HI" nil t))
                  (goto-char (match-beginning 0))
                  (should (= 4 (org-current-level)))
                  ;; HI should be at agent-level + 1
                  (should (= (1+ agent-level) (org-current-level))))))
          (when (and indirect-buf (buffer-live-p indirect-buf))
            (kill-buffer indirect-buf)))))))

(ert-deftest gptel-org-agent-test-insert-user-heading-level-after-auto-correct ()
  "HI heading level should be correct after auto-corrector rebases AI headings.
Reproduces the real post-response sequence: auto-correct processes the AI
response (rebasing level-1 headings to agent-level+1), then cleanup runs,
then insert-user-heading runs.

Expected structure:
  ,** DOING Calculate 2 + 2            <- level 2 (parent)
  ,*** AI-DOING Calculate 2 + 2        <- level 3 (agent heading, UNCHANGED)
  ,**** AI Calculate 2 + 2             <- level 4 (rebased from * AI)
  ,**** HI                             <- level 4 (user = agent + 1, CORRECT)"
  (let ((org-inhibit-startup t)
        (org-todo-keywords '((sequence "AI-DO" "AI-DOING" "DOING" "HI"
                                       "|" "AI-DONE" "DONE")))
        (gptel-org-todo-keywords '("AI-DO" "AI-DOING")))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert "** DOING Calculate 2 + 2\nDescription\n")
      (goto-char (point-min))
      (org-back-to-heading t)
      (let* ((gptel-org-subtree-context t)
             (gptel-org-use-todo-keywords t)
             (gptel-org-user-keyword "HI")
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
                ;; The auto-corrector rebases these in real-time.
                (goto-char (point-max))
                (insert "* AI Calculate 2 + 2\n4.\n")
                ;; Run auto-corrector on first chunk (initializes + processes)
                (let ((gptel-org--corrector-state nil))
                  (gptel-org--auto-correct-stream)
                  ;; Simulate post-response hook sequence:
                  ;; Priority 2: auto-correct-cleanup (processes remaining text)
                  (gptel-org--auto-correct-cleanup nil nil)
                  ;; Priority 95: insert-user-heading
                  (gptel-org-agent--insert-user-heading nil nil)))
              ;; Verify structure in base buffer
              (with-current-buffer base-buf
                (goto-char (point-min))
                ;; Agent heading must still be level 3 (NOT rebased)
                (should (re-search-forward "AI-DOING Calculate 2 \\+ 2" nil t))
                (goto-char (match-beginning 0))
                (beginning-of-line)
                (should (= 3 (org-current-level)))
                ;; AI response heading should be level 4 (rebased from 1)
                (should (re-search-forward "AI Calculate 2 \\+ 2" nil t))
                (goto-char (match-beginning 0))
                (beginning-of-line)
                (should (= 4 (org-current-level)))
                ;; HI heading should be level 4 (agent-level + 1)
                (should (re-search-forward "^\\*+ HI" nil t))
                (goto-char (match-beginning 0))
                (should (= 4 (org-current-level)))))
          (when (and indirect-buf (buffer-live-p indirect-buf))
            (kill-buffer indirect-buf)))))))

(ert-deftest gptel-org-agent-test-insert-user-heading-correct-after-second-send ()
  "HI heading level is correct when user sends again from existing HI heading.
Reproduces the exact bug scenario: after the first response creates a correct
HI heading and the user types into it and sends again, the SECOND response
cycle should also produce a correct-level HI heading.

Scenario:
1. User sends from ** DOING Calculate 2 + 2
2. Agent creates *** AI-DOING, AI responds with `4.', HI heading created at level 4
3. User types `+ 5' in the HI heading -> **** HI + 5
4. User sends again from **** HI + 5
5. maybe-setup-subtree reuses existing *** AI-DOING subtree
6. Agent responds with `9.'
7. insert-user-heading should create **** HI (level 4)

Bug: the second HI heading appeared at level 7 (******* HI) instead of 4."
  (let ((org-inhibit-startup t)
        (org-todo-keywords '((sequence "AI-DO" "AI-DOING" "DOING" "HI"
                                       "|" "AI-DONE" "DONE")))
        (gptel-org-todo-keywords '("AI-DO" "AI-DOING")))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      ;; Start with the state AFTER the first response + HI heading + user edit.
      ;; The AI response heading was rebased from * AI to **** AI by the corrector.
      (insert "\
** DOING Calculate 2 + 2
*** AI-DOING Calculate 2 + 2                                  :main@agent:
**** AI Calculate 2 + 2
4.
**** HI + 5
")
      (let* ((gptel-org-subtree-context t)
             (gptel-org-use-todo-keywords t)
             (gptel-org-user-keyword "HI")
             (gptel-org-assistant-keyword "AI")
             (base-buf (current-buffer))
             (indirect-buf nil))
        (unwind-protect
            (progn
              ;; Simulate maybe-setup-subtree: find the existing agent subtree
              ;; and open a fresh indirect buffer for it.
              (goto-char (point-min))
              (re-search-forward "^\\*\\*\\* AI-DOING")
              (beginning-of-line)
              (let ((heading-marker (point-marker)))
                (setq indirect-buf
                      (gptel-org-agent--open-indirect-buffer base-buf heading-marker)))
              ;; Now the indirect buffer is narrowed to the *** AI-DOING subtree.
              ;; The corrector must NOT re-process the already-rebased
              ;; **** AI heading from the first response.
              ;;
              ;; In real usage, transform-redirect sets :position to
              ;; (point-max) in the indirect buffer.  The AI response is
              ;; streamed at that position (after **** HI + 5).
              ;; The auto-corrector initializes with start = forward-line 1
              ;; from point-min (the agent heading), which means it would
              ;; try to re-process **** AI (already at level 4) and rebase
              ;; it to level 7, corrupting the subtree.
              (with-current-buffer indirect-buf
                ;; Set response-start before inserting, just as
                ;; transform-redirect does before streaming begins.
                (goto-char (point-max))
                (setq-local gptel-org--response-start (point-marker))
                (insert "* AI Result\n9.\n")
                ;; Run auto-corrector (initializes + processes)
                (let ((gptel-org--corrector-state nil))
                  (gptel-org--auto-correct-stream)
                  ;; Simulate post-response hook sequence:
                  ;; Priority 2: cleanup
                  (gptel-org--auto-correct-cleanup nil nil)
                  ;; Priority 95: insert-user-heading
                  (gptel-org-agent--insert-user-heading nil nil)))
              ;; Verify in base buffer
              (with-current-buffer base-buf
                (goto-char (point-min))
                ;; Agent heading must STILL be level 3
                (should (re-search-forward "AI-DOING Calculate 2 \\+ 2" nil t))
                (goto-char (match-beginning 0))
                (beginning-of-line)
                (should (= 3 (org-current-level)))
                ;; The FIRST AI response heading (from previous cycle)
                ;; must still be level 4, NOT re-rebased to level 7.
                (should (re-search-forward "AI Calculate 2 \\+ 2" nil t))
                (goto-char (match-beginning 0))
                (beginning-of-line)
                (should (= 4 (org-current-level)))
                ;; The original HI + 5 heading must still be level 4
                (should (re-search-forward "HI \\+ 5" nil t))
                (goto-char (match-beginning 0))
                (beginning-of-line)
                (should (= 4 (org-current-level)))
                ;; The new AI response heading should be level 4
                (should (re-search-forward "AI Result" nil t))
                (goto-char (match-beginning 0))
                (beginning-of-line)
                (should (= 4 (org-current-level)))
                ;; The NEW HI heading (second one) should also be level 4.
                ;; Find the LAST HI heading in the buffer.
                (goto-char (point-max))
                (should (re-search-backward "^\\*+ HI " nil t))
                (should (= 4 (org-current-level)))))
          (when (and indirect-buf (buffer-live-p indirect-buf))
            (kill-buffer indirect-buf)))))))

;;; ---- maybe-setup-subtree walk-up from HI heading -------------------------

(ert-deftest gptel-org-agent-test-maybe-setup-subtree-walks-up-from-hi-heading ()
  "Sending from a HI heading should walk up to parent and reuse agent subtree.
When `gptel-org-use-todo-keywords' is enabled, a HI heading is a user prompt
heading.  `maybe-setup-subtree' must walk up to the parent DOING heading
and reuse the existing agent subtree instead of creating a new one under HI.

Reproduces a bug where HI (having a non-nil TODO state) was treated as a
parent task heading, causing a new agent subtree to be created underneath it
at the wrong level.

Expected: reuse *** AI-DOING subtree under ** DOING
Bug produced: new agent subtree created under **** HI"
  (let ((org-inhibit-startup t)
        (org-todo-keywords '((sequence "AI-DO" "AI-DOING" "DOING" "HI"
                                       "|" "AI-DONE" "DONE")))
        (gptel-org-todo-keywords '("AI-DO" "AI-DOING")))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert "** DOING Calculate 2 + 2
*** AI-DOING Calculate 2 + 2                                  :main@agent:
**** AI Calculate 2 + 2
2 + 2 = 4
**** HI Add 6
")
      (let* ((gptel-org-subtree-context t)
             (gptel-org-use-todo-keywords t)
             (gptel-org-user-keyword "HI")
             (indirect-buf nil))
        (unwind-protect
            (progn
              ;; Move to the HI heading
              (goto-char (point-min))
              (re-search-forward "^\\*\\*\\*\\* HI")
              (beginning-of-line)
              (should (equal (org-get-todo-state) "HI"))
              ;; Call maybe-setup-subtree - should walk up and reuse existing
              (setq indirect-buf (gptel-org-agent--maybe-setup-subtree))
              ;; Should have returned an indirect buffer (reusing existing subtree)
              (should indirect-buf)
              (should (buffer-live-p indirect-buf))
              ;; The indirect buffer should be narrowed to the existing agent subtree
              (with-current-buffer indirect-buf
                (goto-char (point-min))
                (should (org-at-heading-p))
                ;; Should be the existing AI-DOING heading, not a new one
                (should (equal (org-get-todo-state) "AI-DOING"))
                (should (= 3 (org-current-level)))))
          (when (and indirect-buf (buffer-live-p indirect-buf))
            (kill-buffer indirect-buf)))))))

(provide 'gptel-org-agent-test)
;;; gptel-org-agent-test.el ends here
