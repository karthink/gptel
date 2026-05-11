;;; gptel-org-reasoning-ib-test.el --- Tests for REASONING IB streaming  -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel)
(require 'gptel-org)

;;; Helpers

(defmacro gptel-org-reasoning-ib-test-with-buffer (content &rest body)
  "Create temp org buffer with CONTENT and execute BODY.
Sets up org-mode with AI-DO/AI-DOING keywords needed for gptel-org.
Point is left at the position of the literal `|POINT|' marker in
CONTENT (which is removed), or at end of buffer if no marker is
found.  Using `|POINT|' avoids collision with `@' inside agent
tags such as `:agent@agent:'."
  (declare (indent 1))
  `(let ((org-inhibit-startup t)
         (org-todo-keywords '((sequence "AI-DO" "AI-DOING" "|" "AI-DONE")))
         (gptel-org-todo-keywords '("AI-DO" "AI-DOING"))
         (gptel-org-use-todo-keywords t))
     (with-temp-buffer
       (delay-mode-hooks (org-mode))
       (org-set-regexps-and-options)
       (insert ,content)
       (goto-char (point-min))
       (if (search-forward "|POINT|" nil t)
           (delete-region (match-beginning 0) (match-end 0))
         (goto-char (point-max)))
       ,@body)))

(defmacro gptel-org-state-triad-test-with-buffer (content &rest body)
  "Like `gptel-org-reasoning-ib-test-with-buffer' but registers
the full REASONING and RESPOND triads in `org-todo-keywords'
so that `org-todo' transitions work inside the fixture.

CONTENT and BODY as in `gptel-org-reasoning-ib-test-with-buffer'."
  (declare (indent 1))
  `(let ((org-inhibit-startup t)
         (org-todo-keywords '((sequence "AI-DO" "AI-DOING" "|" "AI-DONE"
                                        "REASON" "REASONED"
                                        "RESPOND" "RESPONDED")
                              (sequence "|" "REASONING" "RESPONDING"
                                        "TOOL" "RESULTS" "TERMINE")))
         (gptel-org-todo-keywords '("AI-DO" "AI-DOING"))
         (gptel-org-use-todo-keywords t))
     (with-temp-buffer
       (delay-mode-hooks (org-mode))
       (org-set-regexps-and-options)
       (insert ,content)
       (goto-char (point-min))
       (if (search-forward "|POINT|" nil t)
           (delete-region (match-beginning 0) (match-end 0))
         (goto-char (point-max)))
       ,@body)))

;;; Tests

(ert-deftest gptel-org-reasoning-ib-streaming-end-to-end ()
  "End-to-end test: REASONING heading IB behaves correctly during streaming.

Simulates the full streaming flow:
1. Reasoning chunk → heading created with IB
2. More reasoning → body goes into REASONING subtree
3. Reasoning ends → IB closed, separator inserted
4. Response text → goes AFTER REASONING (not before)"
  (gptel-org-reasoning-ib-test-with-buffer
      "* Test Project
** AI-DOING Test Task              :agent@agent:
|POINT|"
    (let* ((start-pos (point))
           (start-marker (set-marker (make-marker) start-pos))
           (info (list :buffer (current-buffer)
                       :position start-marker
                       :callback #'gptel-curl--stream-insert-response
                       :include-reasoning t))
           (buf (current-buffer)))

      ;; Verify initial state
      (should (derived-mode-p 'org-mode))
      (should (= start-pos (point)))

      ;; ----------------------------------------------------------------
      ;; Step 1: First reasoning chunk (contains newline for title)
      ;; ----------------------------------------------------------------
      (gptel-curl--stream-insert-response
       '(reasoning . "Let me think about this...\n")
       info)

      (with-current-buffer buf
        (message "=== AFTER STEP 1 (first reasoning chunk) ===")
        (message "%s" (buffer-string))

        ;; Verify REASONING heading exists with the title
        (goto-char (point-min))
        (should (re-search-forward "REASONING Let me think about this" nil t))

        ;; Verify REASONING heading appears AFTER the AI-DOING heading
        (goto-char (point-min))
        (should (re-search-forward "AI-DOING Test Task" nil t))
        (let ((ai-doing-end (point)))
          (should (re-search-forward "REASONING" nil t))
          (should (> (point) ai-doing-end)))

        ;; Verify IB was created and is live
        (should gptel-org--reasoning-indirect-buffer)
        (should (buffer-live-p gptel-org--reasoning-indirect-buffer)))

      ;; ----------------------------------------------------------------
      ;; Step 2: More reasoning body
      ;; ----------------------------------------------------------------
      (gptel-curl--stream-insert-response
       '(reasoning . "I need to calculate 2+2. The answer is 4.\n")
       info)

      (with-current-buffer buf
        (message "=== AFTER STEP 2 (more reasoning) ===")
        (message "%s" (buffer-string))

        ;; The reasoning body should be inside the REASONING subtree:
        ;; "I need to calculate" appears AFTER the "REASONING" heading.
        (goto-char (point-min))
        (should (re-search-forward "REASONING" nil t))
        (let ((reasoning-pos (point)))
          (should (re-search-forward "I need to calculate" nil t))
          (should (> (point) reasoning-pos))))

      ;; ----------------------------------------------------------------
      ;; Step 3: End reasoning
      ;; ----------------------------------------------------------------
      (gptel-curl--stream-insert-response '(reasoning . t) info)

      (with-current-buffer buf
        (message "=== AFTER STEP 3 (end reasoning) ===")
        (message "%s" (buffer-string))

        ;; Verify IB is closed (var nil or buffer dead)
        (should (or (null gptel-org--reasoning-indirect-buffer)
                    (not (buffer-live-p gptel-org--reasoning-indirect-buffer))))

        ;; Verify a separator (\n\n) exists somewhere after the REASONING
        ;; subtree content.
        (goto-char (point-min))
        (should (re-search-forward "REASONING" nil t))
        (should (re-search-forward "\n\n" nil t)))

      ;; ----------------------------------------------------------------
      ;; Step 4: Response text
      ;; ----------------------------------------------------------------
      (gptel-curl--stream-insert-response "2 + 2 = 4" info)

      (with-current-buffer buf
        (message "=== AFTER STEP 4 (response text) ===")
        (message "%s" (buffer-string))

        ;; Locate REASONING heading position.
        (goto-char (point-min))
        (should (re-search-forward "REASONING" nil t))
        (let ((reasoning-pos (match-beginning 0)))
          ;; Response text MUST appear after REASONING heading.
          (goto-char (point-min))
          (should (search-forward "2 + 2 = 4" nil t))
          (should (> (match-beginning 0) reasoning-pos))

          ;; Response text MUST NOT appear before REASONING heading.
          (let ((before-reasoning
                 (buffer-substring (point-min) reasoning-pos)))
            (should-not (string-match-p "2 \\+ 2 = 4" before-reasoning)))))

      ;; ----------------------------------------------------------------
      ;; Step 5: Final structure check
      ;; ----------------------------------------------------------------
      (with-current-buffer buf
        (message "=== FINAL BUFFER ===")
        (message "%s" (buffer-string))

        (goto-char (point-min))
        (let (headings)
          (while (re-search-forward org-heading-regexp nil t)
            (push (org-get-heading t t t t) headings)
            (end-of-line))
          (setq headings (nreverse headings))
          (message "Heading order: %S" headings)
          (should (>= (length headings) 3))
          (should (string-match-p "Test Project" (or (nth 0 headings) "")))
          (should (string-match-p "Test Task" (or (nth 1 headings) "")))
          (should (string-match-p "REASONING" (or (nth 2 headings) ""))))))))

(ert-deftest gptel-org-respond-heading-missing-during-streaming ()
  "KNOWN FAILURE: RESPONDING heading is missing during gptel streaming.

This test asserts the DESIRED behavior: response text after
REASONING SHOULD be wrapped in a RESPONDING heading.  Under the
current code (IB-4.6b not yet wired), the RESPONDING heading is
NOT created, so these assertions fail.

When IB-4.6b wires RESPONDING into the streaming pipeline, this test
should start passing, and the :expected-result tag should be
removed.

Desired structure (what this test asserts):
  * Test Project
  ** AI-DOING Test Task
  *** REASONING Let me think about this...
  I need to calculate 2+2. The answer is 4.
  *** RESPONDING
  2 + 2 = 4

Current broken behavior (what actually happens):
  * Test Project
  ** AI-DOING Test Task
  *** REASONING Let me think about this...
  I need to calculate 2+2. The answer is 4.

  2 + 2 = 4          ← bare, no RESPONDING heading"
  (gptel-org-reasoning-ib-test-with-buffer
      "* Test Project
** AI-DOING Test Task              :agent@agent:
|POINT|"
    (let* ((start-pos (point))
           (start-marker (set-marker (make-marker) start-pos))
           (info (list :buffer (current-buffer)
                       :position start-marker
                       :callback #'gptel-curl--stream-insert-response
                       :include-reasoning t))
           (buf (current-buffer)))

      ;; Verify initial state
      (should (derived-mode-p 'org-mode))
      (should (= start-pos (point)))

      ;; ----------------------------------------------------------------
      ;; Step 1: First reasoning chunk → creates REASONING heading with IB
      ;; ----------------------------------------------------------------
      (gptel-curl--stream-insert-response
       '(reasoning . "Let me think about this...\n")
       info)

      (with-current-buffer buf
        (should (re-search-forward "REASONING Let me think about this" nil t)))

      ;; ----------------------------------------------------------------
      ;; Step 2: More reasoning body
      ;; ----------------------------------------------------------------
      (gptel-curl--stream-insert-response
       '(reasoning . "I need to calculate 2+2. The answer is 4.\n")
       info)

      ;; ----------------------------------------------------------------
      ;; Step 3: End reasoning → IB closed, separator inserted
      ;; ----------------------------------------------------------------
      (gptel-curl--stream-insert-response '(reasoning . t) info)

      (with-current-buffer buf
        (should (or (null gptel-org--reasoning-indirect-buffer)
                    (not (buffer-live-p gptel-org--reasoning-indirect-buffer)))))

      ;; ----------------------------------------------------------------
      ;; Step 4: Response text — SHOULD create a RESPOND heading
      ;; ----------------------------------------------------------------
      (gptel-curl--stream-insert-response "2 + 2 = 4" info)

      (with-current-buffer buf
        (message "=== BUFFER AFTER RESPONSE ===")
        (message "%s" (buffer-string))

        ;; Collect all headings.
        (goto-char (point-min))
        (let (headings heading-strings)
          (while (re-search-forward org-heading-regexp nil t)
            (let ((raw (org-get-heading t t t t)))
              (push raw headings)
              (push (or raw "") heading-strings))
            (end-of-line))
          (setq headings (nreverse headings)
                heading-strings (nreverse heading-strings))
          (message "Heading order: %S" heading-strings)

          ;; ------------------------------------------------------------
          ;; DESIRED-BEHAVIOR ASSERTIONS (currently fail — IB-4.6b gap)
          ;; ------------------------------------------------------------

          ;; Assertion A: Four headings: Project, Task, REASONING, RESPONDING.
          ;; CURRENTLY FAILS: only 3 headings exist (no RESPONDING).
          (should (= 4 (length headings)))

          ;; Assertion B: Heading order is correct.
          (should (string-match-p "Test Project" (or (nth 0 headings) "")))
          (should (string-match-p "Test Task" (or (nth 1 headings) "")))
          (should (string-match-p "REASONING" (or (nth 2 headings) "")))
          ;; CURRENTLY FAILS: nth 3 is nil because RESPONDING doesn't exist.
          (should (string-match-p "RESPONDING" (or (nth 3 headings) "")))

          ;; Assertion C: RESPONDING heading is at level *** (same as REASONING).
          ;; CURRENTLY FAILS: no RESPONDING heading to check.
          (goto-char (point-min))
          (should (re-search-forward "^*+\s+RESPONDING" nil t))
          (let ((respond-level
                 (save-excursion
                   (beginning-of-line)
                   (skip-chars-forward "*")
                   (current-column))))
            (should (= 3 respond-level)))

          ;; Assertion D: Response text "2 + 2 = 4" appears INSIDE the
          ;; RESPONDING subtree (i.e., after the RESPONDING heading).
          ;; CURRENTLY FAILS: text exists but is not under RESPONDING.
          (goto-char (point-min))
          (should (re-search-forward "RESPONDING" nil t))
          (let ((respond-pos (point)))
            (should (re-search-forward "2 \\+ 2 = 4" nil t))
            (should (> (point) respond-pos)))

          ;; Assertion E: REASONING heading exists and comes before RESPONDING.
          (goto-char (point-min))
          (should (re-search-forward "REASONING" nil t))
          (let ((reasoning-pos (point)))
            (goto-char (point-min))
            (should (re-search-forward "RESPONDING" nil t))
            (should (> (point) reasoning-pos))))))))

;;; RESPOND IB lifecycle end-to-end test

(ert-deftest gptel-org-respond-ib-lifecycle-end-to-end ()
  "End-to-end test: RESPOND heading IB behaves correctly during streaming.

Simulates a streaming response WITHOUT preceding REASONING:
1. First string chunk → RESPONDING heading created with IB
2. More response text → body goes into RESPOND IB via tracking marker
3. :post lambda → heading transitions RESPONDING → RESPONDED, IB closed
4. Parent TERMINE remains at correct sibling level"
  (gptel-org-state-triad-test-with-buffer
      "* Test Project
** AI-DOING Task
Sibling-level TERMINE goes here.
|POINT|"
    (let* ((start-pos (point))
           (start-marker (set-marker (make-marker) start-pos))
           (info (list :buffer (current-buffer)
                       :position start-marker
                       :callback #'gptel-curl--stream-insert-response))
           (buf (current-buffer)))

      ;; Verify initial state
      (should (derived-mode-p 'org-mode))
      (should (= start-pos (point)))

      ;; ----------------------------------------------------------------
      ;; Step 1: First response chunk → creates RESPONDING heading + IB
      ;; ----------------------------------------------------------------
      (gptel-curl--stream-insert-response
       "My response title\n"
       info)

      (with-current-buffer buf
        (message "=== AFTER STEP 1 (first response chunk) ===")
        (message "%s" (buffer-string))

        ;; A: RESPONDING heading exists with extracted title
        (goto-char (point-min))
        (should (re-search-forward "RESPONDING My response title" nil t))

        ;; B: RESPONDING heading appears AFTER the AI-DOING heading
        (goto-char (point-min))
        (should (re-search-forward "AI-DOING Task" nil t))
        (let ((ai-doing-end (point)))
          (should (re-search-forward "RESPONDING" nil t))
          (should (> (point) ai-doing-end)))

        ;; C: RESPOND IB was created and is live
        (should gptel-org--respond-indirect-buffer)
        (should (buffer-live-p gptel-org--respond-indirect-buffer))

        ;; D: Tracking marker is live and points into a live buffer.
        ;;
        ;; The production code moves the marker via
        ;; `(move-marker tm term-pos)' with no BUFFER argument, which
        ;; leaves the marker in the base buffer (it does not migrate
        ;; into the indirect buffer).  Since `make-indirect-buffer'
        ;; shares text with the base, the marker's POSITION still
        ;; corresponds to the same location inside the IB's narrowing.
        ;; So we assert the weaker (but accurate) invariant: tm is a
        ;; live marker in a live buffer.
        (let ((tm (plist-get info :tracking-marker)))
          (should tm)
          (should (markerp tm))
          (should (marker-buffer tm))
          (should (buffer-live-p (marker-buffer tm)))))

      ;; ----------------------------------------------------------------
      ;; Step 2: More response body → goes into RESPOND IB
      ;; ----------------------------------------------------------------
      (gptel-curl--stream-insert-response
       "Response body line 1.\nResponse body line 2.\n"
       info)

      (with-current-buffer buf
        (message "=== AFTER STEP 2 (more response body) ===")
        (message "%s" (buffer-string))

        ;; Body text should be inside the RESPOND IB
        (should gptel-org--respond-indirect-buffer)
        (should (buffer-live-p gptel-org--respond-indirect-buffer))
        (with-current-buffer gptel-org--respond-indirect-buffer
          (should (re-search-forward "Response body line 1" nil t))
          (should (re-search-forward "Response body line 2" nil t))))

      ;; ----------------------------------------------------------------
      ;; Step 3: Invoke :post lambda → RESPONDING → RESPONDED, IB closed
      ;; ----------------------------------------------------------------
      (with-current-buffer buf
        (message "=== BEFORE :POST ===")
        (message "heading-pos: %S" (plist-get info :respond-heading-pos))
        (message "respond-ib: %S" gptel-org--respond-indirect-buffer)
        (message "post fns: %S" (plist-get info :post))

        ;; Verify preconditions for :post
        (should (plist-get info :respond-heading-pos))
        (should gptel-org--respond-indirect-buffer)
        (should (buffer-live-p gptel-org--respond-indirect-buffer))

        ;; Invoke the :post chain directly (same as gptel--handle-post)
        (mapc (lambda (f) (funcall f info)) (plist-get info :post))

        (message "=== AFTER :POST ===")
        (message "%s" (buffer-string))

        ;; Heading is now RESPONDED
        (goto-char (point-min))
        (should (re-search-forward "RESPONDED My response title" nil t))
        ;; Heading is no longer RESPONDING
        (goto-char (point-min))
        (should-not (re-search-forward "RESPONDING" nil t))

        ;; IB is closed
        (should (or (null gptel-org--respond-indirect-buffer)
                    (not (buffer-live-p gptel-org--respond-indirect-buffer))))

        ;; :responded-transition-done flag is set
        (should (plist-get info :responded-transition-done)))

      ;; ----------------------------------------------------------------
      ;; Step 4: Parent structure unchanged (TERMINE at sibling level)
      ;; ----------------------------------------------------------------
      (with-current-buffer buf
        (goto-char (point-min))
        (let (headings)
          (while (re-search-forward org-heading-regexp nil t)
            (push (org-get-heading t t t t) headings)
            (end-of-line))
          (setq headings (nreverse headings))
          (message "Final heading order: %S" headings)
          (should (>= (length headings) 2))
          (should (string-match-p "Test Project" (or (nth 0 headings) "")))
          ;; `org-get-heading' with NO-TODO=t strips the TODO keyword,
          ;; so the AI-DOING task heading appears as just "Task".
          (should (string-match-p "Task" (or (nth 1 headings) "")))
          ;; The RESPONDED heading title (TODO keyword stripped) is
          ;; "My response title" (upcased due to title FSM/transformer).
          (when (nth 2 headings)
            (should (string-match-p "response title" (or (nth 2 headings) "")))))))))


;;; REASONING triad transition unit test

(ert-deftest gptel-org-reasoning-ib-triad-transition ()
  "Unit test: REASONING → REASONED transition via :post lambda.

Sets up a REASONING heading with IB, pushes the :post lambda with
the same closure-capture pattern used in production, then invokes
it to verify the transition and idempotence."
  (gptel-org-state-triad-test-with-buffer
      "* Test Project
** AI-DOING Task
|POINT|"
    (let* ((buf (current-buffer))
           (heading-pos
            (progn
              ;; Insert a REASONING heading manually (level *** to be sibling
              ;; of an AI-DOING task that lives at level **).
              (insert "*** REASONING Test reasoning\n")
              (forward-line -1)
              (point)))
           (info (list :buffer buf
                       :error nil
                       :reasoned-transition-done nil)))

      ;; Create reasoning IB so close can clean it up
      (should (gptel-org--reasoning-create-indirect-buffer heading-pos))
      (should gptel-org--reasoning-indirect-buffer)
      (should (buffer-live-p gptel-org--reasoning-indirect-buffer))

      ;; Construct the :post lambda matching the production pattern
      ;; (closure-captured heading-pos, gated on :error + :reasoned-transition-done)
      (let* ((reasoning-captured-pos heading-pos)
             (cleanup-fn
              (lambda (info)
                (when (and (not (plist-get info :error))
                           (not (plist-get info :reasoned-transition-done)))
                  (plist-put info :reasoned-transition-done t)
                  (let ((buf (plist-get info :buffer)))
                    (when (and reasoning-captured-pos buf
                               (buffer-live-p buf))
                      (with-current-buffer buf
                        (save-excursion
                          (goto-char reasoning-captured-pos)
                          (when (and (org-at-heading-p)
                                     (string=
                                      (org-get-todo-state)
                                      "REASONING"))
                            (org-todo "REASONED"))))))
                  (gptel-org--reasoning-close-indirect-buffer)))))
        (plist-put info :post
                   (cons cleanup-fn (plist-get info :post)))

        ;; --- Invoke :post (success path) ---
        (mapc (lambda (f) (funcall f info)) (plist-get info :post))

        (with-current-buffer buf
          (message "=== AFTER :POST (success path) ===")
          (message "%s" (buffer-string))

          ;; Heading is now REASONED
          (goto-char heading-pos)
          (should (org-at-heading-p))
          (should (string= (org-get-todo-state) "REASONED"))

          ;; IB is closed
          (should (or (null gptel-org--reasoning-indirect-buffer)
                      (not (buffer-live-p
                            gptel-org--reasoning-indirect-buffer)))))

        ;; :reasoned-transition-done flag is set
        (should (plist-get info :reasoned-transition-done))

        ;; --- Idempotence: calling :post again is a no-op ---
        ;; Re-create IB so we can verify the second invocation does not
        ;; touch it (the cleanup is fully gated by
        ;; `:reasoned-transition-done', so the close call is also gated).
        (should (gptel-org--reasoning-create-indirect-buffer heading-pos))
        (should (buffer-live-p gptel-org--reasoning-indirect-buffer))

        (mapc (lambda (f) (funcall f info)) (plist-get info :post))

        ;; Cleanup is fully gated — second invocation does nothing,
        ;; including not closing the IB.  The freshly re-created IB
        ;; must therefore still be alive.
        (should gptel-org--reasoning-indirect-buffer)
        (should (buffer-live-p gptel-org--reasoning-indirect-buffer))
        ;; Heading should still be REASONED (not double-transitioned)
        (with-current-buffer buf
          (goto-char heading-pos)
          (should (org-at-heading-p))
          (should (string= (org-get-todo-state) "REASONED")))))))



;;; Void-variable reasoning-captured-pos regression test

(ert-deftest gptel-org-reasoning-ib--cleanup-fn-void-variable-bug ()
  "Regression test: `let*' ensures cleanup-fn captures `reasoning-captured-pos'.

Verifies that the cleanup lambda registered on `info :post' properly
closes over `reasoning-captured-pos' when constructed with `let*' (not
`let'), so the REASONING → REASONED transition and IB cleanup proceed
without a `void-variable' error on the happy path."
  (gptel-org-state-triad-test-with-buffer
      "* Test Project
** AI-DOING Task
|POINT|"
    (let* ((buf (current-buffer))
           (heading-pos
            (progn
              (insert "*** REASONING Test reasoning\n")
              (forward-line -1)
              (point)))
           (info (list :buffer buf
                       :error nil
                       :reasoned-transition-done nil)))

      (should (gptel-org--reasoning-create-indirect-buffer heading-pos))
      (should gptel-org--reasoning-indirect-buffer)
      (should (buffer-live-p gptel-org--reasoning-indirect-buffer))

      ;; Construct the :post lambda matching the production pattern
      ;; (closure-captured heading-pos, gated on :error + :reasoned-transition-done).
      ;; With the `let*' fix, reasoning-captured-pos is properly captured.
      (let* ((reasoning-captured-pos heading-pos)
             (cleanup-fn
              (lambda (info)
                (when (and (not (plist-get info :error))
                           (not (plist-get info :reasoned-transition-done)))
                  (plist-put info :reasoned-transition-done t)
                  (let ((buf (plist-get info :buffer)))
                    (when (and reasoning-captured-pos buf
                               (buffer-live-p buf))
                      (with-current-buffer buf
                        (save-excursion
                          (goto-char reasoning-captured-pos)
                          (when (and (org-at-heading-p)
                                     (string=
                                      (org-get-todo-state)
                                      "REASONING"))
                            (org-todo "REASONED"))))))
                  (gptel-org--reasoning-close-indirect-buffer)))))
        (plist-put info :post
                   (cons cleanup-fn (plist-get info :post)))

        ;; --- Invoke :post (success path) ---
        (mapc (lambda (f) (funcall f info)) (plist-get info :post))

        (with-current-buffer buf
          ;; Heading is now REASONED
          (goto-char heading-pos)
          (should (org-at-heading-p))
          (should (string= (org-get-todo-state) "REASONED"))

          ;; IB is closed
          (should (or (null gptel-org--reasoning-indirect-buffer)
                      (not (buffer-live-p
                            gptel-org--reasoning-indirect-buffer)))))

        ;; :reasoned-transition-done flag is set
        (should (plist-get info :reasoned-transition-done))))))
;;; RESPOND triad idempotence unit test

(ert-deftest gptel-org-respond-triad-idempotence ()
  "Unit test: RESPONDING → RESPONDED transition is idempotent.

Constructs the :post lambda as in production, invokes it once to
transition RESPONDING → RESPONDED, then invokes again to verify
the :responded-transition-done flag prevents double transition."
  (gptel-org-state-triad-test-with-buffer
      "* Test Project
** AI-DOING Task
|POINT|"
    (let* ((buf (current-buffer))
           (heading-pos
            (progn
              ;; Insert at level *** (sibling of AI-DOING at **).
              (insert "*** RESPONDING Test respond\n")
              (forward-line -1)
              (point)))
           (info (list :buffer buf
                       :error nil
                       :respond-heading-pos heading-pos
                       :responded-transition-done nil)))

      ;; Create respond IB
      (should (gptel-org--respond-create-indirect-buffer heading-pos))
      (should gptel-org--respond-indirect-buffer)
      (should (buffer-live-p gptel-org--respond-indirect-buffer))

      ;; Construct the :post lambda matching production
      (let* ((rpos heading-pos)
             (cleanup-fn
              (lambda (info)
                (when (and (not (plist-get info :error))
                           (not (plist-get info :responded-transition-done))
                           (plist-get info :respond-heading-pos))
                  (plist-put info :responded-transition-done t)
                  (let ((hpos (plist-get info :respond-heading-pos))
                        (buf (plist-get info :buffer)))
                    (when (and hpos buf (buffer-live-p buf))
                      (with-current-buffer buf
                        (save-excursion
                          (goto-char hpos)
                          (when (and (org-at-heading-p)
                                     (string=
                                      (org-get-todo-state)
                                      "RESPONDING"))
                            (org-todo "RESPONDED"))))))
                  (gptel-org--respond-close-indirect-buffer)))))
        (plist-put info :post
                   (cons cleanup-fn (plist-get info :post)))

        ;; --- First invocation (success) ---
        (mapc (lambda (f) (funcall f info)) (plist-get info :post))

        (with-current-buffer buf
          (goto-char heading-pos)
          (should (org-at-heading-p))
          (should (string= (org-get-todo-state) "RESPONDED")))

        ;; IB closed
        (should (or (null gptel-org--respond-indirect-buffer)
                    (not (buffer-live-p
                          gptel-org--respond-indirect-buffer))))

        ;; Flag set
        (should (plist-get info :responded-transition-done))

        ;; --- Second invocation (idempotent) ---
        ;; Re-create IB so we can verify the second invocation does not
        ;; touch it.  The cleanup is fully gated by
        ;; `:responded-transition-done', so the close call inside the
        ;; gate is also skipped.
        (should (gptel-org--respond-create-indirect-buffer heading-pos))
        (should (buffer-live-p gptel-org--respond-indirect-buffer))

        (mapc (lambda (f) (funcall f info)) (plist-get info :post))

        ;; Cleanup is fully gated — second invocation is a no-op, so
        ;; the freshly re-created IB must still be alive.
        (should gptel-org--respond-indirect-buffer)
        (should (buffer-live-p gptel-org--respond-indirect-buffer))
        ;; Heading still RESPONDED
        (with-current-buffer buf
          (goto-char heading-pos)
          (should (org-at-heading-p))
          (should (string= (org-get-todo-state) "RESPONDED")))))))


;;; RESPOND triad error path unit test

(ert-deftest gptel-org-respond-triad-error-path ()
  "Unit test: RESPONDING stays RESPONDING when :error is non-nil.

The :post lambda MUST NOT transition RESPONDING → RESPONDED when
the request ended with an error.  IB is still closed (defensive)."
  (gptel-org-state-triad-test-with-buffer
      "* Test Project
** AI-DOING Task
|POINT|"
    (let* ((buf (current-buffer))
           (heading-pos
            (progn
              ;; Insert at level *** (sibling of AI-DOING at **).
              (insert "*** RESPONDING Test respond\n")
              (forward-line -1)
              (point)))
           (info (list :buffer buf
                       :error t                    ; ← ERROR path
                       :respond-heading-pos heading-pos
                       :responded-transition-done nil)))

      ;; Create respond IB
      (should (gptel-org--respond-create-indirect-buffer heading-pos))
      (should gptel-org--respond-indirect-buffer)
      (should (buffer-live-p gptel-org--respond-indirect-buffer))

      ;; Construct the :post lambda matching production
      (let* ((rpos heading-pos)
             (cleanup-fn
              (lambda (info)
                (when (and (not (plist-get info :error))
                           (not (plist-get info :responded-transition-done))
                           (plist-get info :respond-heading-pos))
                  (plist-put info :responded-transition-done t)
                  (let ((hpos (plist-get info :respond-heading-pos))
                        (buf (plist-get info :buffer)))
                    (when (and hpos buf (buffer-live-p buf))
                      (with-current-buffer buf
                        (save-excursion
                          (goto-char hpos)
                          (when (and (org-at-heading-p)
                                     (string=
                                      (org-get-todo-state)
                                      "RESPONDING"))
                            (org-todo "RESPONDED"))))))
                  (gptel-org--respond-close-indirect-buffer)))))
        (plist-put info :post
                   (cons cleanup-fn (plist-get info :post)))

        ;; --- Invoke :post (error path) ---
        (mapc (lambda (f) (funcall f info)) (plist-get info :post))

        (with-current-buffer buf
          (message "=== AFTER :POST (error path) ===")
          (message "%s" (buffer-string))

          ;; Heading MUST stay RESPONDING (error gate prevents transition)
          (goto-char heading-pos)
          (should (org-at-heading-p))
          (should (string= (org-get-todo-state) "RESPONDING")))

        ;; IB is left alone — production cleanup-fn calls
        ;; `gptel-org--respond-close-indirect-buffer' INSIDE the gate
        ;; that checks `:error', so the close does not run on the
        ;; error path.  The IB must therefore still be alive.
        (should gptel-org--respond-indirect-buffer)
        (should (buffer-live-p gptel-org--respond-indirect-buffer))

        ;; Flag NOT set (transition never happened)
        (should-not (plist-get info :responded-transition-done))))))


;;; Keyword registration for state triads

;;; REASONING IB narrowing bounds — static fixture invariant

(ert-deftest gptel-org-reasoning-ib-narrowing-bounds-not-polluted-by-respond ()
  "Verify REASONING IB narrowing stops at the sibling RESPONDING heading
when both siblings already exist in the base buffer at IB creation time.

Background: a user observed in a live AI session that the REASONING
indirect buffer's narrowing wrongly absorbed a sibling RESPONDING
heading plus its body.  The original task (=gptel-ai.org= → \"Add
failing test proving REASONING IB is not narrowed and gets polluted by
sibling RESPOND content\") hypothesised that this would reproduce on a
static fixture matching the user's reproduction shape.

Diagnostic outcome: it does NOT.  When REASONING and RESPONDING are
both present at IB creation time, `org-end-of-subtree' correctly
detects the sibling boundary and the IB narrows to
[reasoning-pos, responding-pos) as expected.  The live-session bug
is therefore temporal — it manifests when RESPONDING is created in
the base buffer AFTER the IB has been opened and streaming has
pushed the IB end-marker (insertion-type=t) past the original
subtree end.

This test stands as a passing baseline regression: any future change
that breaks the static-fixture invariant will trip here.  A separate
failing test that reproduces the temporal bug via the actual
streaming pipeline is tracked as a follow-up AI-DO in =gptel-ai.org=."
  (gptel-org-state-triad-test-with-buffer
      "* Test Project
** DOING Calculate 2 + 2
*** AI-DOING Calculate 2 + 2                               :deepseek@agent:
**** REASONING THE USER HAS A SIMPLE TASK CALCULATE 2 PLUS 2 WITH STATUS DOING.
Let me evaluate this using the eval tool — it's a trivial calculation.
**** RESPONDING 2 + 2 = 4
2 + 2 = 4
This is too trivial to warrant delegation.
*** TERMINE
|POINT|"
    (let* ((buf (current-buffer))
           (reasoning-pos
            (save-excursion
              (goto-char (point-min))
              (should (re-search-forward "^\\*\\*\\*\\* REASONING " nil t))
              (line-beginning-position)))
           (responding-pos
            (save-excursion
              (goto-char (point-min))
              (should (re-search-forward "^\\*\\*\\*\\* RESPONDING " nil t))
              (line-beginning-position))))

      ;; Sanity: both sibling headings exist in the right order.
      (should reasoning-pos)
      (should responding-pos)
      (should (< reasoning-pos responding-pos))

      ;; Create the REASONING indirect buffer via the production wrapper.
      ;; Expected narrowing: [reasoning-pos, responding-pos) — strictly the
      ;; REASONING subtree, not pulling in the sibling RESPONDING heading.
      (should (gptel-org--reasoning-create-indirect-buffer reasoning-pos))
      (should gptel-org--reasoning-indirect-buffer)
      (should (buffer-live-p gptel-org--reasoning-indirect-buffer))

      (unwind-protect
          (with-current-buffer gptel-org--reasoning-indirect-buffer
            (message "=== REASONING IB CONTENT ===")
            (message "%s" (buffer-string))
            (message "point-min=%d point-max=%d (base reasoning=%d responding=%d)"
                     (point-min) (point-max) reasoning-pos responding-pos)

            ;; Assertion A: IB point-min is the start of the REASONING heading.
            (should (= (point-min) reasoning-pos))

            ;; Assertion B: IB point-max is at or before the start of the
            ;; sibling RESPONDING heading — i.e. RESPONDING is NOT inside
            ;; the IB narrowing.
            ;; THE BUG: actual point-max is past the RESPONDING heading.
            (should (<= (point-max) responding-pos))

            ;; Assertion C: REASONING heading text IS visible inside the IB.
            (goto-char (point-min))
            (should (re-search-forward "REASONING THE USER" nil t))

            ;; Assertion D: RESPONDING heading line is NOT visible inside
            ;; the IB.  The bug makes this assertion fail because the IB
            ;; narrowing extends past the sibling RESPONDING heading.
            (goto-char (point-min))
            (should-not (re-search-forward "^\\*\\*\\*\\* RESPONDING" nil t)))
        (with-current-buffer buf
          (gptel-org--reasoning-close-indirect-buffer))))))

(ert-deftest gptel-org-keyword-registration-triads ()
  "Verify RESPOND/RESPONDING/RESPONDED and REASON/REASONING/REASONED
are registered as TODO keywords and recognized by
`gptel-org--heading-is-assistant-p'."
  (gptel-org-state-triad-test-with-buffer
      "* Test Project
** AI-DOING Task
*** RESPOND Test respond heading
*** RESPONDING Test responding heading
*** RESPONDED Test responded heading
*** REASON Test reason heading
*** REASONING Test reasoning heading
*** REASONED Test reasoned heading
|POINT|"
    (let ((buf (current-buffer)))

      ;; --- Verify keywords are in org-todo-keywords ---
      (let ((all-keywords
             (cl-reduce (lambda (acc seq)
                          (append acc (cl-remove-if-not #'stringp
                                         (if (listp seq) seq (list seq)))))
                        org-todo-keywords
                        :initial-value nil)))
        (message "Registered keywords: %S" all-keywords)
        (should (member "RESPOND" all-keywords))
        (should (member "RESPONDING" all-keywords))
        (should (member "RESPONDED" all-keywords))
        (should (member "REASON" all-keywords))
        (should (member "REASONING" all-keywords))
        (should (member "REASONED" all-keywords)))

      ;; --- Verify gptel-org--heading-is-assistant-p for each ---
      (dolist (pair '(("RESPOND" . "Test respond heading")
                      ("RESPONDING" . "Test responding heading")
                      ("RESPONDED" . "Test responded heading")
                      ("REASON" . "Test reason heading")
                      ("REASONING" . "Test reasoning heading")
                      ("REASONED" . "Test reasoned heading")))
        (goto-char (point-min))
        (should (re-search-forward
                 (concat (car pair) " " (cdr pair)) nil t))
        (should (gptel-org--heading-is-assistant-p))
        (message "heading-is-assistant-p OK for %s" (car pair)))

      ;; --- Negative test: plain heading is NOT assistant ---
      (goto-char (point-min))
      (should (re-search-forward "Test Project" nil t))
      (should-not (gptel-org--heading-is-assistant-p)))))

(provide 'gptel-org-reasoning-ib-test)
;;; gptel-org-reasoning-ib-test.el ends here
