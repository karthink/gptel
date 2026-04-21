;;; gptel-org-ib-test.el --- Tests for gptel-indirect-buffer  -*- lexical-binding: t; -*-

;; Tests for the `gptel-indirect-buffer' module which provides:
;; - A registry of tracked indirect buffers keyed by buffer name.
;; - Utilities for resolving root base buffers and reading heading info.
;; - Heading navigation past agent tags.
;; - Terminator heading management (find / create / ensure).
;; - Safe sibling heading creation that preserves concurrent IBs.
;; - Indirect buffer lifecycle (create / close / valid-p).
;; - Recreation after a subtree has been moved (name-based lookup).
;; - Auto-correction: validate-and-fix, renarrow.
;; - High-level safe-insert-sibling: the core invariant of the module.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gptel)
(require 'gptel-org)
(require 'gptel-org-agent)
(require 'gptel-indirect-buffer)

;;; Helper macros

(defmacro gptel-org-ib-test-with-buffer (content &rest body)
  "Create a temp org buffer with CONTENT and execute BODY.
Sets up org-mode with AI-DO/AI-DOING/FEEDBACK/AI-DONE keywords,
unsets `gptel-org-debug', and binds `gptel-org-ib--registry' to a
fresh hash table so tests never contaminate one another."
  (declare (indent 1))
  `(let ((org-inhibit-startup t)
         (inhibit-message t)
         (gptel-org-debug nil)
         (gptel-org-ib--registry (make-hash-table :test 'equal))
         (org-todo-keywords
          '((sequence "AI-DO" "AI-DOING" "FEEDBACK" "|" "AI-DONE"))))
     (with-temp-buffer
       (delay-mode-hooks (org-mode))
       (org-set-regexps-and-options)
       (insert ,content)
       (goto-char (point-min))
       ,@body)))

(defmacro gptel-org-ib-test-with-cleanup (&rest body)
  "Run BODY, then kill every indirect buffer registered in the registry.
Protects tests against leaks from registered IBs they created."
  `(unwind-protect (progn ,@body)
     (dolist (name (hash-table-keys gptel-org-ib--registry))
       (let ((buf (plist-get (gethash name gptel-org-ib--registry) :buffer)))
         (when (buffer-live-p buf) (kill-buffer buf))))))


;;; ---- Tracking Registry ----------------------------------------------------

(ert-deftest gptel-org-ib-test-registry-register-and-get ()
  "Register an entry and verify `gptel-org-ib-get' returns its plist."
  (gptel-org-ib-test-with-buffer
      "* Heading\nBody\n"
    (let* ((base (current-buffer))
           (hm (point-marker))
           (em (point-marker))
           (name "*gptel:test-abcdef*"))
      (gptel-org-ib-register name base base hm em "test")
      (let ((entry (gptel-org-ib-get name)))
        (should entry)
        (should (eq (plist-get entry :buffer) base))
        (should (eq (plist-get entry :base) base))
        (should (eq (plist-get entry :heading-marker) hm))
        (should (eq (plist-get entry :end-marker) em))
        (should (equal (plist-get entry :tag) "test"))))))

(ert-deftest gptel-org-ib-test-registry-unregister-clears-markers ()
  "Unregistering clears marker positions (marker-buffer -> nil)."
  (gptel-org-ib-test-with-buffer
      "* Heading\nBody\n"
    (let* ((base (current-buffer))
           (hm (copy-marker (point-min)))
           (em (copy-marker (point-max)))
           (name "*gptel:x-abcdef*"))
      (gptel-org-ib-register name base base hm em "x")
      (should (marker-buffer hm))
      (should (marker-buffer em))
      (gptel-org-ib-unregister name)
      (should-not (marker-buffer hm))
      (should-not (marker-buffer em))
      (should-not (gptel-org-ib-get name)))))

(ert-deftest gptel-org-ib-test-registry-unregister-missing-name ()
  "Unregistering a non-existent name is a no-op and must not error."
  (gptel-org-ib-test-with-buffer
      "* Heading\n"
    ;; Should not raise
    (gptel-org-ib-unregister "*gptel:nonexistent-000000*")
    (should-not (gptel-org-ib-get "*gptel:nonexistent-000000*"))))

(ert-deftest gptel-org-ib-test-registry-all-for-base-filters ()
  "`gptel-org-ib-all-for-base' returns names only for the matching base."
  (gptel-org-ib-test-with-buffer
      "* H\n"
    (let ((base-a (current-buffer))
          (base-b (generate-new-buffer " *gptel-ib-test-base-b*")))
      (unwind-protect
          (progn
            (gptel-org-ib-register "n1" base-a base-a nil nil "t")
            (gptel-org-ib-register "n2" base-a base-a nil nil "t")
            (gptel-org-ib-register "n3" base-b base-b nil nil "t")
            (should (= 2 (length (gptel-org-ib-all-for-base base-a))))
            (should (= 1 (length (gptel-org-ib-all-for-base base-b)))))
        (kill-buffer base-b)))))

(ert-deftest gptel-org-ib-test-registry-cleanup-dead ()
  "`cleanup-dead' removes entries whose buffer was killed."
  (gptel-org-ib-test-with-buffer
      "* H\n"
    (let* ((base (current-buffer))
           (dead-buf (generate-new-buffer " *gptel-ib-test-dead*")))
      (gptel-org-ib-register "dead" dead-buf base nil nil "t")
      (kill-buffer dead-buf)
      (should (= 1 (gptel-org-ib-cleanup-dead)))
      (should-not (gptel-org-ib-get "dead")))))

(ert-deftest gptel-org-ib-test-registry-cleanup-dead-none ()
  "`cleanup-dead' returns 0 when all buffers are alive."
  (gptel-org-ib-test-with-buffer
      "* H\n"
    (let ((base (current-buffer)))
      (gptel-org-ib-register "alive" base base nil nil "t")
      (should (= 0 (gptel-org-ib-cleanup-dead)))
      (should (gptel-org-ib-get "alive")))))


;;; ---- Utilities ------------------------------------------------------------

(ert-deftest gptel-org-ib-test-utils-base-buffer-of-base ()
  "`base-buffer' of a non-indirect buffer returns that buffer itself."
  (gptel-org-ib-test-with-buffer
      "* H\n"
    (should (eq (current-buffer) (gptel-org-ib-base-buffer (current-buffer))))))

(ert-deftest gptel-org-ib-test-utils-base-buffer-of-indirect ()
  "`base-buffer' of an indirect buffer returns the root base."
  (gptel-org-ib-test-with-buffer
      "* H\n"
    (let* ((base (current-buffer))
           (ib (make-indirect-buffer base " *gptel-ib-test-ib*" t)))
      (unwind-protect
          (should (eq base (gptel-org-ib-base-buffer ib)))
        (kill-buffer ib)))))

(ert-deftest gptel-org-ib-test-utils-base-buffer-of-indirect-of-indirect ()
  "`base-buffer' walks the chain even though Emacs flattens it."
  (gptel-org-ib-test-with-buffer
      "* H\n"
    (let* ((base (current-buffer))
           (ib1 (make-indirect-buffer base " *gptel-ib-test-ib1*" t))
           ;; Emacs flattens: ib2's base is `base', not ib1.  Regardless,
           ;; the function must walk up to the root.
           (ib2 (make-indirect-buffer ib1 " *gptel-ib-test-ib2*" t)))
      (unwind-protect
          (should (eq base (gptel-org-ib-base-buffer ib2)))
        (kill-buffer ib2)
        (kill-buffer ib1)))))

(ert-deftest gptel-org-ib-test-utils-heading-level-and-text ()
  "`heading-level' and `heading-text' return the level/title at point-min."
  (gptel-org-ib-test-with-buffer
      "* Parent\n** Child\n*** Foo\nBody\n"
    (gptel-org-ib-test-with-cleanup
     (goto-char (point-min))
     (re-search-forward "^\\*\\*\\* Foo")
     (beginning-of-line)
     (let ((ib (gptel-org-ib-create (current-buffer) (point))))
       (should (= 3 (gptel-org-ib-heading-level ib)))
       (should (equal "Foo" (gptel-org-ib-heading-text ib)))))))

(ert-deftest gptel-org-ib-test-utils-heading-level-and-text-on-dead-buffer ()
  "Both utilities return nil on a killed buffer."
  (gptel-org-ib-test-with-buffer
      "* Foo\n"
    (let* ((base (current-buffer))
           (ib (make-indirect-buffer base " *gptel-ib-test-dead*" t)))
      (kill-buffer ib)
      (should-not (gptel-org-ib-heading-level ib))
      (should-not (gptel-org-ib-heading-text ib)))))


;;; ---- Terminator Heading Management ----------------------------------------

(ert-deftest gptel-org-ib-test-terminator-regexp-matches ()
  "The terminator regexp matches the exact keyword at the exact level."
  (let ((re (gptel-org-ib--terminator-regexp "FEEDBACK" 5)))
    (should (string-match-p re "***** FEEDBACK "))
    (should (string-match-p re "***** FEEDBACK\n"))
    (should-not (string-match-p re "**** FEEDBACK "))
    (should-not (string-match-p re "****** FEEDBACK "))
    (should-not (string-match-p re "***** FEEDBACKX "))))

(ert-deftest gptel-org-ib-test-terminator-find-missing ()
  "Finding a terminator in a subtree that has none returns nil."
  (gptel-org-ib-test-with-buffer
      "* Parent\n** Child\nbody\n"
    (goto-char (point-min))
    (org-back-to-heading t)
    (should-not (gptel-org-ib-find-terminator "FEEDBACK"))))

(ert-deftest gptel-org-ib-test-terminator-find-present ()
  "Finding a terminator at the child level succeeds."
  (gptel-org-ib-test-with-buffer
      "* Parent\n** Child\n** FEEDBACK\n"
    (goto-char (point-min))
    (org-back-to-heading t)
    (let ((pos (gptel-org-ib-find-terminator "FEEDBACK")))
      (should pos)
      (save-excursion
        (goto-char pos)
        (should (org-at-heading-p))
        (should (= 2 (org-current-level)))
        ;; FEEDBACK is an org TODO keyword in the test fixture, so the
        ;; heading's "title" (after stripping TODO state) is empty and
        ;; FEEDBACK is reported as the TODO state.
        (should (equal "FEEDBACK" (org-get-todo-state)))))))

(ert-deftest gptel-org-ib-test-terminator-find-deeper-ignored ()
  "A deeper heading with the same keyword is NOT matched."
  (gptel-org-ib-test-with-buffer
      "* Parent\n** Child\n*** FEEDBACK\n"
    (goto-char (point-min))
    (org-back-to-heading t)
    (should-not (gptel-org-ib-find-terminator "FEEDBACK"))))

(ert-deftest gptel-org-ib-test-terminator-create-appends ()
  "`create-terminator' inserts at child level at end of subtree."
  (gptel-org-ib-test-with-buffer
      "* Parent\n** Child\nbody\n"
    (goto-char (point-min))
    (org-back-to-heading t)
    (let ((m (gptel-org-ib-create-terminator "FEEDBACK")))
      (should (markerp m))
      (save-excursion
        (goto-char m)
        (should (org-at-heading-p))
        (should (= 2 (org-current-level)))
        ;; FEEDBACK is an org TODO keyword in the test fixture.
        (should (equal "FEEDBACK" (org-get-todo-state))))
      ;; Re-find should locate the same heading.
      (goto-char (point-min))
      (org-back-to-heading t)
      (let ((found (gptel-org-ib-find-terminator "FEEDBACK")))
        (should found)
        (should (= (marker-position m) found))))))

(ert-deftest gptel-org-ib-test-terminator-ensure-idempotent ()
  "`ensure-terminator' is idempotent: two calls => one heading, same line."
  (gptel-org-ib-test-with-buffer
      "* Parent\n** Child\nbody\n"
    (goto-char (point-min))
    (org-back-to-heading t)
    (let ((m1 (gptel-org-ib-ensure-terminator "FEEDBACK")))
      (goto-char (point-min))
      (org-back-to-heading t)
      (let ((m2 (gptel-org-ib-ensure-terminator "FEEDBACK")))
        (should (= (marker-position m1) (marker-position m2)))
        ;; Exactly one FEEDBACK heading in the buffer
        (goto-char (point-min))
        (should (= 1 (how-many "^\\*+ FEEDBACK\\b" (point-min) (point-max))))))))


;;; ---- Streaming Marker ----------------------------------------------------

(ert-deftest gptel-org-ib-test-streaming-marker-with-terminator ()
  "With a FEEDBACK terminator, marker lands at start of FEEDBACK line."
  (gptel-org-ib-test-with-buffer
      "* Parent\n** Child\nbody\n** FEEDBACK\n"
    (goto-char (point-min))
    (org-back-to-heading t)
    (let ((m (gptel-org-ib-streaming-marker "FEEDBACK")))
      (should (markerp m))
      (should-not (marker-insertion-type m))
      (save-excursion
        (goto-char m)
        (should (bolp))
        (should (org-at-heading-p))
        (should (equal "FEEDBACK" (org-get-todo-state)))))))

(ert-deftest gptel-org-ib-test-streaming-marker-without-terminator ()
  "Without terminator, marker is at point-max with insertion-type t."
  (gptel-org-ib-test-with-buffer
      "* Parent\nbody\n"
    (goto-char (point-min))
    (org-back-to-heading t)
    (let ((m (gptel-org-ib-streaming-marker "FEEDBACK")))
      (should (markerp m))
      (should (marker-insertion-type m))
      ;; Marker at end-of-buffer (modulo trailing newline handling).
      (should (= (marker-position m)
                 (save-excursion
                   (goto-char (point-max))
                   (skip-chars-backward "\n")
                   (end-of-line)
                   (point)))))))

(ert-deftest gptel-org-ib-test-streaming-marker-insertion-before-terminator ()
  "Inserting at the streaming marker places content BEFORE the terminator."
  (gptel-org-ib-test-with-buffer
      "* Parent\n** Child\nbody\n** FEEDBACK\n"
    (goto-char (point-min))
    (org-back-to-heading t)
    (let ((m (gptel-org-ib-streaming-marker "FEEDBACK")))
      (save-excursion
        (goto-char m)
        (insert "*** TOOL streamed\n"))
      ;; FEEDBACK should still exist and be after the inserted heading.
      (goto-char (point-min))
      (let (order)
        (while (re-search-forward org-heading-regexp nil t)
          (push (or (org-get-todo-state)
                    (org-get-heading t t t t))
                order))
        (setq order (nreverse order))
        (should (equal "Parent" (nth 0 order)))
        (should (equal "Child" (nth 1 order)))
        (should (equal "TOOL streamed" (nth 2 order)))
        (should (equal "FEEDBACK" (nth 3 order)))))))

(ert-deftest gptel-org-ib-test-streaming-marker-no-terminator-arg ()
  "Passing nil for terminator falls back to point-max with insertion-type t."
  (gptel-org-ib-test-with-buffer
      "* Parent\n** Child\n** FEEDBACK\n"
    (goto-char (point-min))
    (org-back-to-heading t)
    (let ((m (gptel-org-ib-streaming-marker nil)))
      (should (markerp m))
      (should (marker-insertion-type m)))))


;;; ---- Sibling Terminator --------------------------------------------------

(ert-deftest gptel-org-ib-test-ensure-sibling-terminator-creates-new ()
  "`ensure-sibling-terminator' creates a sibling heading at LEVEL when missing."
  (gptel-org-ib-test-with-buffer
      "* Top\n** Agent\nbody\n"
    (goto-char (point-min))
    (re-search-forward "^\\*\\* Agent")
    (beginning-of-line)
    (let ((m (gptel-org-ib-ensure-sibling-terminator "FEEDBACK" 2)))
      (should (markerp m))
      (save-excursion
        (goto-char m)
        (should (org-at-heading-p))
        (should (= 2 (org-current-level)))
        (should (equal "FEEDBACK" (org-get-todo-state)))))))

(ert-deftest gptel-org-ib-test-ensure-sibling-terminator-reuses-existing ()
  "`ensure-sibling-terminator' returns an existing sibling without duplicating."
  (gptel-org-ib-test-with-buffer
      "* Top\n** Agent\nbody\n** FEEDBACK\n"
    (goto-char (point-min))
    (re-search-forward "^\\*\\* Agent")
    (beginning-of-line)
    (let ((m1 (gptel-org-ib-ensure-sibling-terminator "FEEDBACK" 2)))
      (goto-char (point-min))
      (re-search-forward "^\\*\\* Agent")
      (beginning-of-line)
      (let ((m2 (gptel-org-ib-ensure-sibling-terminator "FEEDBACK" 2)))
        (should (= (marker-position m1) (marker-position m2)))
        (goto-char (point-min))
        (should (= 1 (how-many "^\\*+ FEEDBACK\\b" (point-min) (point-max))))))))

(ert-deftest gptel-org-ib-test-ensure-sibling-terminator-stops-at-parent-boundary ()
  "Sibling search stops at a heading of shallower level (leaves parent scope)."
  (gptel-org-ib-test-with-buffer
      "* Top\n** Agent\nbody\n* Other Top\n** FEEDBACK\n"
    (goto-char (point-min))
    (re-search-forward "^\\*\\* Agent")
    (beginning-of-line)
    (let ((m (gptel-org-ib-ensure-sibling-terminator "FEEDBACK" 2)))
      (should (markerp m))
      ;; Should have created a NEW feedback heading inside Top, not reused
      ;; the one inside Other Top.
      (goto-char (point-min))
      (should (= 2 (how-many "^\\*+ FEEDBACK\\b" (point-min) (point-max))))
      ;; The returned marker must point to the new FEEDBACK inside Top.
      (save-excursion
        (goto-char m)
        (should (org-at-heading-p))
        (should (equal "FEEDBACK" (org-get-todo-state)))
        ;; It must be BEFORE "Other Top".
        (should (< (point)
                   (save-excursion
                     (goto-char (point-min))
                     (re-search-forward "^\\* Other Top")
                     (match-beginning 0))))))))


;;; ---- Safe Heading Creation (Insert Before Terminator) --------------------

(ert-deftest gptel-org-ib-test-create-heading-before-terminator ()
  "`create-heading' inserts BEFORE an existing terminator.
Order of headings: parent, existing child, new heading, FEEDBACK."
  (gptel-org-ib-test-with-buffer
      "* Parent\n** Existing child\nbody\n** FEEDBACK\n"
    (goto-char (point-min))
    (org-back-to-heading t)
    (gptel-org-ib-create-heading "AI-DO" "New task" nil "FEEDBACK")
    (goto-char (point-min))
    (let (order)
      (while (re-search-forward org-heading-regexp nil t)
        ;; Record (title . todo-state) so FEEDBACK (a TODO keyword)
        ;; is represented even though its title is empty.
        (push (or (org-get-todo-state)
                  (org-get-heading t t t t))
              order))
      (setq order (nreverse order))
      (should (equal "Parent" (nth 0 order)))
      (should (equal "Existing child" (nth 1 order)))
      ;; New heading is created with AI-DO TODO keyword.
      (should (equal "AI-DO" (nth 2 order)))
      (should (equal "FEEDBACK" (nth 3 order))))))

(ert-deftest gptel-org-ib-test-create-heading-without-terminator-appends ()
  "With no terminator-keyword, new heading appears at end of subtree."
  (gptel-org-ib-test-with-buffer
      "* Parent\n** Existing child\nbody\n"
    (goto-char (point-min))
    (org-back-to-heading t)
    (gptel-org-ib-create-heading "AI-DO" "Tail task" nil nil)
    (goto-char (point-min))
    (let (order)
      (while (re-search-forward org-heading-regexp nil t)
        (push (org-get-heading t t t t) order))
      (setq order (nreverse order))
      (should (equal "Parent" (nth 0 order)))
      (should (equal "Existing child" (nth 1 order)))
      (should (equal "Tail task" (nth 2 order))))))

(ert-deftest gptel-org-ib-test-create-heading-with-tags ()
  "`create-heading' applies tags via `org-set-tags'."
  (gptel-org-ib-test-with-buffer
      "* Parent\n"
    (goto-char (point-min))
    (org-back-to-heading t)
    (let ((org-auto-align-tags nil)
          (m (gptel-org-ib-create-heading
              "AI-DO" "Tagged" '("main@agent" "custom") nil)))
      (save-excursion
        (goto-char m)
        (should (org-at-heading-p))
        (let ((tags (org-get-tags nil t)))
          (should (member "main@agent" tags))
          (should (member "custom" tags)))))))

(ert-deftest gptel-org-ib-test-create-heading-with-missing-terminator-keyword ()
  "If the specified terminator keyword is absent, fall back to end-of-subtree."
  (gptel-org-ib-test-with-buffer
      "* Parent\n** Existing child\n"
    (goto-char (point-min))
    (org-back-to-heading t)
    (gptel-org-ib-create-heading "AI-DO" "Tail" nil "NOT-PRESENT")
    (goto-char (point-min))
    (let (order)
      (while (re-search-forward org-heading-regexp nil t)
        (push (org-get-heading t t t t) order))
      (setq order (nreverse order))
      (should (equal "Parent" (nth 0 order)))
      (should (equal "Existing child" (nth 1 order)))
      (should (equal "Tail" (nth 2 order))))))


;;; ---- Indirect Buffer Lifecycle --------------------------------------------

(ert-deftest gptel-org-ib-test-compute-subtree-region-includes-trailing-newline ()
  "`compute-subtree-region' advances past trailing newline when possible."
  (gptel-org-ib-test-with-buffer
      "* A\nbody\n* B\n"
    (let* ((base (current-buffer))
           (heading-pos (with-current-buffer base
                          (goto-char (point-min))
                          (org-back-to-heading t)
                          (point)))
           (region (gptel-org-ib--compute-subtree-region base heading-pos))
           (end (cdr region)))
      ;; End should point at the start of "* B" (past the "\n" that
      ;; terminates "body").
      (with-current-buffer base
        (goto-char end)
        (should (looking-at-p "^\\* B"))))))

(ert-deftest gptel-org-ib-test-extract-tag-at-with-agent-tag ()
  "`extract-tag-at' returns the first *@agent tag."
  (gptel-org-ib-test-with-buffer
      "* Heading  :main@agent:\nbody\n"
    (let ((pos (progn (goto-char (point-min)) (org-back-to-heading t) (point))))
      (should (equal "main@agent"
                     (gptel-org-ib--extract-tag-at (current-buffer) pos))))))

(ert-deftest gptel-org-ib-test-extract-tag-at-no-agent-tag ()
  "`extract-tag-at' returns \"agent\" when no *@agent tag is present."
  (gptel-org-ib-test-with-buffer
      "* Heading  :random:\nbody\n"
    (let ((pos (progn (goto-char (point-min)) (org-back-to-heading t) (point))))
      (should (equal "agent"
                     (gptel-org-ib--extract-tag-at (current-buffer) pos))))))

(ert-deftest gptel-org-ib-test-create-narrows-to-subtree ()
  "`gptel-org-ib-create' narrows IB to subtree; siblings not visible."
  (gptel-org-ib-test-with-buffer
      "* A  :main@agent:\nA body\n* B\nB body\n"
    (gptel-org-ib-test-with-cleanup
     (goto-char (point-min))
     (org-back-to-heading t)
     (let ((ib (gptel-org-ib-create (current-buffer) (point))))
       (with-current-buffer ib
         ;; point-min is the heading
         (goto-char (point-min))
         (should (org-at-heading-p))
         (let ((content (buffer-substring-no-properties (point-min) (point-max))))
           (should (string-match-p "A body" content))
           (should-not (string-match-p "^\\* B" content))
           (should-not (string-match-p "B body" content))))))))

(ert-deftest gptel-org-ib-test-create-end-marker-expands ()
  "Text inserted at point-max of the IB is visible in IB and base."
  (gptel-org-ib-test-with-buffer
      "* A  :main@agent:\nA body\n* B\nB body\n"
    (gptel-org-ib-test-with-cleanup
     (goto-char (point-min))
     (org-back-to-heading t)
     (let ((ib (gptel-org-ib-create (current-buffer) (point)))
           (marker "UNIQUE-MARKER-42"))
       (with-current-buffer ib
         (let ((size-before (- (point-max) (point-min))))
           (goto-char (point-max))
           (insert marker "\n")
           (should (> (- (point-max) (point-min)) size-before))
           (should (string-match-p
                    marker
                    (buffer-substring-no-properties (point-min) (point-max))))))
       ;; Visible in base too.
       (should (string-match-p
                marker
                (buffer-substring-no-properties (point-min) (point-max))))))))

(ert-deftest gptel-org-ib-test-create-replaces-existing ()
  "Calling create twice with the same name kills the old IB and replaces it."
  (gptel-org-ib-test-with-buffer
      "* Heading\nbody\n"
    (gptel-org-ib-test-with-cleanup
     (goto-char (point-min))
     (org-back-to-heading t)
     (let* ((name "*gptel-ib-test-replace*")
            (ib1 (gptel-org-ib-create (current-buffer) (point) name))
            (ib2 (gptel-org-ib-create (current-buffer) (point) name)))
       (should-not (eq ib1 ib2))
       (should-not (buffer-live-p ib1))
       (should (buffer-live-p ib2))
       ;; Registry has exactly one entry with this name.
       (should (gptel-org-ib-get name))
       (should (= 1 (length (gptel-org-ib-all-for-base (current-buffer)))))))))

(ert-deftest gptel-org-ib-test-create-from-indirect-base-flattens ()
  "Passing an indirect buffer as base still resolves root for the new IB."
  (gptel-org-ib-test-with-buffer
      "* Heading\nbody\n"
    (gptel-org-ib-test-with-cleanup
     (let* ((base (current-buffer))
            (intermediate (make-indirect-buffer
                           base " *gptel-ib-test-intermediate*" t)))
       (unwind-protect
           (let ((ib (with-current-buffer intermediate
                       (goto-char (point-min))
                       (org-back-to-heading t)
                       (gptel-org-ib-create intermediate (point)))))
             (should (buffer-live-p ib))
             (should (eq base (buffer-base-buffer ib))))
         (kill-buffer intermediate))))))

(ert-deftest gptel-org-ib-test-close-kills-and-unregisters ()
  "`close' kills the IB and removes it from the registry."
  (gptel-org-ib-test-with-buffer
      "* Heading\nbody\n"
    (goto-char (point-min))
    (org-back-to-heading t)
    (let* ((ib (gptel-org-ib-create (current-buffer) (point)))
           (name (buffer-name ib)))
      (should (buffer-live-p ib))
      (should (gptel-org-ib-get name))
      (gptel-org-ib-close ib)
      (should-not (buffer-live-p ib))
      (should-not (gptel-org-ib-get name)))))

(ert-deftest gptel-org-ib-test-close-with-fold-true ()
  "Closing with fold=t folds the subtree in the base buffer."
  (gptel-org-ib-test-with-buffer
      "* Heading\nLine 1\nLine 2\n"
    (let* ((base (current-buffer))
           (marker (progn (goto-char (point-min))
                          (org-back-to-heading t)
                          (point-marker)))
           (ib (gptel-org-ib-create base (marker-position marker))))
      (gptel-org-ib-close ib t)
      (with-current-buffer base
        (goto-char (marker-position marker))
        (should (org-at-heading-p))
        (end-of-line)
        (should (org-fold-folded-p (point)))))))

(ert-deftest gptel-org-ib-test-close-dead-buffer-noop ()
  "Closing a buffer that was already killed is a no-op."
  (gptel-org-ib-test-with-buffer
      "* Heading\n"
    (goto-char (point-min))
    (org-back-to-heading t)
    (let ((ib (gptel-org-ib-create (current-buffer) (point))))
      (kill-buffer ib)
      ;; Must not raise.
      (gptel-org-ib-close ib)
      (should-not (buffer-live-p ib)))))

(ert-deftest gptel-org-ib-test-valid-p-true-for-fresh-ib ()
  "A newly created IB is valid-p."
  (gptel-org-ib-test-with-buffer
      "* Heading\nbody\n"
    (gptel-org-ib-test-with-cleanup
     (goto-char (point-min))
     (org-back-to-heading t)
     (let ((ib (gptel-org-ib-create (current-buffer) (point))))
       (should (gptel-org-ib-valid-p ib))))))

(ert-deftest gptel-org-ib-test-valid-p-false-for-dead ()
  "A killed IB is not valid-p."
  (gptel-org-ib-test-with-buffer
      "* Heading\nbody\n"
    (goto-char (point-min))
    (org-back-to-heading t)
    (let ((ib (gptel-org-ib-create (current-buffer) (point))))
      (kill-buffer ib)
      (should-not (gptel-org-ib-valid-p ib)))))

(ert-deftest gptel-org-ib-test-valid-p-false-when-collapsed ()
  "An IB whose narrowing has collapsed is not valid-p."
  (gptel-org-ib-test-with-buffer
      "* Heading\nbody\n"
    (gptel-org-ib-test-with-cleanup
     (goto-char (point-min))
     (org-back-to-heading t)
     (let ((ib (gptel-org-ib-create (current-buffer) (point))))
       (with-current-buffer ib
         (widen)
         (narrow-to-region 1 1))
       (should-not (gptel-org-ib-valid-p ib))))))


;;; ---- Heading Navigation ---------------------------------------------------

(ert-deftest gptel-org-ib-test-find-user-task-heading-walks-up ()
  "`find-user-task-heading' walks up past *@agent-tagged headings."
  (gptel-org-ib-test-with-buffer
      "* User task\n** Agent  :main@agent:\n*** Sub agent  :researcher@main@agent:\n"
    (goto-char (point-min))
    (re-search-forward "^\\*\\*\\* Sub agent")
    (beginning-of-line)
    (let ((pos (gptel-org-ib-find-user-task-heading)))
      (should pos)
      (save-excursion
        (goto-char pos)
        (should (org-at-heading-p))
        (should (equal "User task" (org-get-heading t t t t)))))))

(ert-deftest gptel-org-ib-test-resolve-agent-heading-nil-for-non-indirect ()
  "`resolve-agent-heading' on a non-indirect buffer returns nil."
  (gptel-org-ib-test-with-buffer
      "* H\n"
    (should-not (gptel-org-ib-resolve-agent-heading (current-buffer)))))

(ert-deftest gptel-org-ib-test-resolve-agent-heading-in-indirect ()
  "In an IB, `resolve-agent-heading' returns point-min when heading there."
  (gptel-org-ib-test-with-buffer
      "* Heading\nbody\n"
    (gptel-org-ib-test-with-cleanup
     (goto-char (point-min))
     (org-back-to-heading t)
     (let ((ib (gptel-org-ib-create (current-buffer) (point))))
       (let ((pos (gptel-org-ib-resolve-agent-heading ib)))
         (should pos)
         (should (= pos (with-current-buffer ib (point-min)))))))))


;;; ---- Recreation After Reorder --------------------------------------------

(ert-deftest gptel-org-ib-test-parse-buffer-name-valid ()
  "Valid buffer names parse into (TAG . HASH)."
  (let ((r (gptel-org-ib--parse-buffer-name "*gptel:main@agent-abc123*")))
    (should r)
    (should (equal (car r) "main@agent"))
    (should (equal (cdr r) "abc123"))))

(ert-deftest gptel-org-ib-test-parse-buffer-name-invalid ()
  "Malformed buffer names return nil."
  ;; Missing hash
  (should-not (gptel-org-ib--parse-buffer-name "*gptel:main@agent*"))
  ;; Wrong prefix
  (should-not (gptel-org-ib--parse-buffer-name "*other:tag-abc123*"))
  ;; Non-hex hash
  (should-not (gptel-org-ib--parse-buffer-name "*gptel:tag-xyz123*"))
  ;; Too-long hash (regex requires exactly 6 hex chars)
  (should-not (gptel-org-ib--parse-buffer-name "*gptel:tag-abcdef0*")))

(ert-deftest gptel-org-ib-test-find-heading-by-tag-and-hash ()
  "`find-heading-by-tag-and-hash' locates the right heading by tag+hash."
  (gptel-org-ib-test-with-buffer
      "* Parent\n** Heading A  :main@agent:\n** Heading B  :main@agent:\n"
    (let* ((base (current-buffer))
           (pos-b (progn (goto-char (point-min))
                         (re-search-forward "^\\*\\* Heading B")
                         (beginning-of-line)
                         (point)))
           (name-b (gptel-org-agent--indirect-buffer-name
                    base pos-b "main@agent"))
           (parsed (gptel-org-ib--parse-buffer-name name-b))
           (tag (car parsed))
           (hash (cdr parsed))
           (found (gptel-org-ib--find-heading-by-tag-and-hash base tag hash)))
      (should found)
      (should (= found pos-b)))))

(ert-deftest gptel-org-ib-test-recreate-after-move ()
  "After moving a subtree past a sibling, recreate rewires to new pos."
  (gptel-org-ib-test-with-buffer
      "* Parent\n** A  :main@agent:\nA body\n** B  :main@agent:\nB body\n"
    (gptel-org-ib-test-with-cleanup
     (let* ((base (current-buffer))
            (pos-a (progn (goto-char (point-min))
                          (re-search-forward "^\\*\\* A ")
                          (beginning-of-line)
                          (point)))
            (ib (gptel-org-ib-create base pos-a))
            (name (buffer-name ib)))
       ;; Move A down past B.
       (with-current-buffer base
         (goto-char (point-min))
         (re-search-forward "^\\*\\* A ")
         (beginning-of-line)
         (org-move-subtree-down))
       ;; Narrowing in IB should have collapsed.
       (should (with-current-buffer ib (>= (point-min) (point-max))))
       ;; Recreate.
       (let ((new-ib (gptel-org-ib-recreate ib)))
         (should new-ib)
         (should (buffer-live-p new-ib))
         (should (equal name (buffer-name new-ib)))
         (should-not (buffer-live-p ib))
         ;; New IB content is A's subtree.
         (with-current-buffer new-ib
           (goto-char (point-min))
           (should (org-at-heading-p))
           (should (equal "A" (org-get-heading t t t t)))
           (should (string-match-p
                    "A body"
                    (buffer-substring-no-properties
                     (point-min) (point-max))))))))))

(ert-deftest gptel-org-ib-test-recreate-deleted-heading-returns-nil ()
  "If the heading was deleted, recreate returns nil."
  (gptel-org-ib-test-with-buffer
      "* Parent\n** A  :main@agent:\nA body\n** B\n"
    (gptel-org-ib-test-with-cleanup
     (let* ((base (current-buffer))
            (pos-a (progn (goto-char (point-min))
                          (re-search-forward "^\\*\\* A ")
                          (beginning-of-line)
                          (point)))
            (ib (gptel-org-ib-create base pos-a)))
       ;; Delete A's subtree entirely.
       (with-current-buffer base
         (goto-char pos-a)
         (let ((beg (point))
               (end (save-excursion (org-end-of-subtree t) (point))))
           (delete-region beg end)))
       (should-not (gptel-org-ib-recreate ib))))))


;;; ---- Auto-correction ------------------------------------------------------

(ert-deftest gptel-org-ib-test-validate-and-fix-valid-returns-t ()
  "A fresh IB validates as t."
  (gptel-org-ib-test-with-buffer
      "* H\nbody\n"
    (gptel-org-ib-test-with-cleanup
     (goto-char (point-min))
     (org-back-to-heading t)
     (let ((ib (gptel-org-ib-create (current-buffer) (point))))
       (should (eq t (gptel-org-ib-validate-and-fix ib)))))))

(ert-deftest gptel-org-ib-test-validate-and-fix-dead-returns-nil ()
  "A killed IB returns nil."
  (gptel-org-ib-test-with-buffer
      "* H\nbody\n"
    (goto-char (point-min))
    (org-back-to-heading t)
    (let ((ib (gptel-org-ib-create (current-buffer) (point))))
      (kill-buffer ib)
      (should-not (gptel-org-ib-validate-and-fix ib)))))

(ert-deftest gptel-org-ib-test-validate-and-fix-collapsed-recreates ()
  "When narrowing collapses, validate-and-fix recreates and returns t."
  (gptel-org-ib-test-with-buffer
      "* Parent\n** A  :main@agent:\nA body\n** B  :main@agent:\nB body\n"
    (gptel-org-ib-test-with-cleanup
     (let* ((base (current-buffer))
            (pos-a (progn (goto-char (point-min))
                          (re-search-forward "^\\*\\* A ")
                          (beginning-of-line)
                          (point)))
            (ib (gptel-org-ib-create base pos-a)))
       ;; Move A down past B to collapse narrowing.
       (with-current-buffer base
         (goto-char (point-min))
         (re-search-forward "^\\*\\* A ")
         (beginning-of-line)
         (org-move-subtree-down))
       (should (eq t (gptel-org-ib-validate-and-fix ib)))))))

(ert-deftest gptel-org-ib-test-renarrow-expands-to-new-subtree-end ()
  "After adding body text at end of subtree, renarrow updates end-marker."
  (gptel-org-ib-test-with-buffer
      "* Heading\nbody\n* Next\nnext body\n"
    (gptel-org-ib-test-with-cleanup
     (let* ((base (current-buffer))
            (heading-pos (progn (goto-char (point-min))
                                (org-back-to-heading t)
                                (point)))
            (ib (gptel-org-ib-create base heading-pos))
            (before-size (with-current-buffer ib (- (point-max) (point-min)))))
       ;; Append body text at end of first subtree in the base buffer,
       ;; before "* Next".  This extends the subtree; the IB end-marker
       ;; position needs to be recomputed.
       (with-current-buffer base
         (goto-char (point-min))
         (re-search-forward "^\\* Next")
         (beginning-of-line)
         (insert "more body line\n"))
       ;; Now renarrow should pick up the new text.
       (should (eq t (gptel-org-ib-renarrow ib)))
       (let ((after-size (with-current-buffer ib
                           (- (point-max) (point-min)))))
         (should (> after-size before-size))
         (should (string-match-p
                  "more body line"
                  (with-current-buffer ib
                    (buffer-substring-no-properties
                     (point-min) (point-max))))))))))


;;; ---- High-level -----------------------------------------------------------

(ert-deftest gptel-org-ib-test-safe-insert-sibling-creates-all ()
  "safe-insert-sibling creates terminator, heading, and IB."
  (gptel-org-ib-test-with-buffer
      "* Parent\nbody\n"
    (gptel-org-ib-test-with-cleanup
     (goto-char (point-min))
     (org-back-to-heading t)
     (let ((ib (gptel-org-ib-safe-insert-sibling
                "AI-DO" "Task" nil "FEEDBACK")))
       (should (buffer-live-p ib))
       (should (gptel-org-ib-valid-p ib))
       ;; Registered
       (should (gptel-org-ib-get (buffer-name ib)))
       ;; IB narrowed to new heading
       (with-current-buffer ib
         (goto-char (point-min))
         (should (org-at-heading-p))
         (let ((title (org-get-heading t t t t)))
           (should (equal "Task" title))))
       ;; In base: FEEDBACK comes after the new heading.
       (with-current-buffer (gptel-org-ib-base-buffer ib)
         (goto-char (point-min))
         (let ((task-pos (progn (re-search-forward "^\\*+ AI-DO Task") (point)))
               (fb-pos  (progn (re-search-forward "^\\*+ FEEDBACK") (point))))
           (should (< task-pos fb-pos))))))))

(ert-deftest gptel-org-ib-test-safe-insert-sibling-multiple-undisturbed ()
  "CORE INVARIANT: concurrent IBs are not disturbed by new siblings."
  (gptel-org-ib-test-with-buffer
      "* Parent\nbody\n"
    (gptel-org-ib-test-with-cleanup
     (goto-char (point-min))
     (org-back-to-heading t)
     (let* ((ib1 (gptel-org-ib-safe-insert-sibling
                  "AI-DO" "T1" nil "FEEDBACK")))
       ;; Put some unique content in ib1 so we can verify it survives
       (with-current-buffer ib1
         (goto-char (point-max))
         (insert "UNIQUE-T1-CONTENT\n"))
       (goto-char (point-min))
       (org-back-to-heading t)
       (let* ((ib2 (gptel-org-ib-safe-insert-sibling
                    "AI-DO" "T2" nil "FEEDBACK")))
         (goto-char (point-min))
         (org-back-to-heading t)
         (let ((ib3 (gptel-org-ib-safe-insert-sibling
                     "AI-DO" "T3" nil "FEEDBACK")))
           ;; All 3 live and valid
           (should (gptel-org-ib-valid-p ib1))
           (should (gptel-org-ib-valid-p ib2))
           (should (gptel-org-ib-valid-p ib3))
           ;; 3 distinct buffers
           (should-not (eq ib1 ib2))
           (should-not (eq ib2 ib3))
           (should-not (eq ib1 ib3))
           ;; T1's IB still shows only T1 (its unique content, not T2/T3)
           (with-current-buffer ib1
             (let ((content (buffer-substring-no-properties
                             (point-min) (point-max))))
               (should (string-match-p "T1" content))
               (should (string-match-p "UNIQUE-T1-CONTENT" content))
               (should-not (string-match-p "\\bT2\\b" content))
               (should-not (string-match-p "\\bT3\\b" content))))
           ;; FEEDBACK terminator appears last in base buffer
           (with-current-buffer (gptel-org-ib-base-buffer ib1)
             (goto-char (point-min))
             (let ((t1p (progn (re-search-forward "^\\*+ AI-DO T1") (point)))
                   (t2p (progn (re-search-forward "^\\*+ AI-DO T2") (point)))
                   (t3p (progn (re-search-forward "^\\*+ AI-DO T3") (point)))
                   (fbp (progn (re-search-forward "^\\*+ FEEDBACK") (point))))
               (should (< t1p t2p))
               (should (< t2p t3p))
               (should (< t3p fbp))))))))))


(provide 'gptel-org-ib-test)
;;; gptel-org-ib-test.el ends here
