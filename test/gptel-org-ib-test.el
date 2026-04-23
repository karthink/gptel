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
Sets up org-mode with AI-DO/AI-DOING/FEEDBACK/AI-DONE/TERMINE
keywords (TERMINE added in IB-4.2 so tests can exercise the new
unified terminator without per-test override), unsets
`gptel-org-debug', and binds `gptel-org-ib--registry' to a fresh
hash table so tests never contaminate one another."
  (declare (indent 1))
  `(let ((org-inhibit-startup t)
         (inhibit-message t)
         (gptel-org-debug nil)
         (gptel-org-ib--registry (make-hash-table :test 'equal))
         (org-todo-keywords
          '((sequence "AI-DO" "AI-DOING" "FEEDBACK" "|" "AI-DONE" "TERMINE"))))
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


;;; ---- Canonical Node Struct ------------------------------------------------

(ert-deftest ib-node-struct-roundtrip ()
  "Construct a `gptel-org-ib-node' and read each slot back.
Also verify the type predicate accepts the instance and rejects
non-nodes."
  (let* ((base-buf    (generate-new-buffer " *ib-node-rt-base*"))
         (ind-buf     (generate-new-buffer " *ib-node-rt-ind*"))
         (parent-node (make-gptel-org-ib-node :tag "parent@agent"))
         (child-a     (make-gptel-org-ib-node :tag "child-a@agent"))
         (child-b     (make-gptel-org-ib-node :tag "child-b@agent"))
         (hm          (with-current-buffer base-buf
                        (copy-marker (point-min))))
         (em          (with-current-buffer base-buf
                        (let ((m (copy-marker (point-max))))
                          (set-marker-insertion-type m t)
                          m)))
         (node (make-gptel-org-ib-node
                :buffer         ind-buf
                :base           base-buf
                :parent         parent-node
                :children       (list child-a child-b)
                :heading-marker hm
                :end-marker     em
                :tag            "main@agent"
                :hash           "deadbeef")))
    (unwind-protect
        (progn
          ;; Predicate
          (should (gptel-org-ib-node-p node))
          (should (gptel-org-ib-node-p parent-node))
          (should-not (gptel-org-ib-node-p nil))
          (should-not (gptel-org-ib-node-p '(:buffer foo)))
          (should-not (gptel-org-ib-node-p "not-a-node"))
          ;; Accessors
          (should (eq  (gptel-org-ib-node-buffer         node) ind-buf))
          (should (eq  (gptel-org-ib-node-base           node) base-buf))
          (should (eq  (gptel-org-ib-node-parent         node) parent-node))
          (should (equal (gptel-org-ib-node-children     node)
                         (list child-a child-b)))
          (should (eq  (gptel-org-ib-node-heading-marker node) hm))
          (should (eq  (gptel-org-ib-node-end-marker     node) em))
          (should (equal (gptel-org-ib-node-tag          node) "main@agent"))
          (should (equal (gptel-org-ib-node-hash         node) "deadbeef"))
          ;; parent=nil convention: a freshly-constructed node with no
          ;; :parent has nil parent, meaning "parent is the base buffer".
          (should-not (gptel-org-ib-node-parent
                       (make-gptel-org-ib-node :tag "top"))))
      (when (marker-buffer hm) (set-marker hm nil))
      (when (marker-buffer em) (set-marker em nil))
      (when (buffer-live-p ind-buf)  (kill-buffer ind-buf))
      (when (buffer-live-p base-buf) (kill-buffer base-buf)))))


;;; ---- Buffer Name Generator ------------------------------------------------

(ert-deftest ib-compute-name-unique ()
  "`gptel-org-ib-compute-name' yields unique names for 100 distinct headings.

Inserts 100 sibling headings into a temp org buffer, then computes a
name for each heading position.  The resulting list of names must
contain 100 unique strings (no collisions from the 6-char hash prefix
for this small distinct input)."
  (gptel-org-ib-test-with-buffer
      ""
    (let ((base (current-buffer))
          (positions nil))
      (dotimes (i 100)
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (let ((start (point)))
          (insert (format "* Heading %d\nbody %d\n" i i))
          (push start positions)))
      (setq positions (nreverse positions))
      (let ((names (mapcar (lambda (pos)
                             (gptel-org-ib-compute-name base pos "main@agent"))
                           positions)))
        (should (= (length names) 100))
        (should (= (length (delete-dups (copy-sequence names))) 100))
        ;; Every name should match the expected format.
        (dolist (n names)
          (should (string-match-p "\\`\\*gptel:main@agent-[0-9a-f]\\{6\\}\\*\\'"
                                  n)))))))


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


(ert-deftest gptel-org-ib-test-terminator-recognises-termine ()
  "TERMINE is recognised by the heading-state grammar (IB-4.1).

Two assertions:
- `gptel-org--heading-is-assistant-p' returns non-nil for a
  heading whose TODO state is TERMINE (with
  `gptel-org-use-todo-keywords' enabled).
- A heading with keyword TERMINE at the child level is found via
  `gptel-org-ib-find-terminator' (round-trip recognition).

RESULTS/FEEDBACK recognition is NOT altered by IB-4.1; those will
be removed in later sub-phases."
  (let ((org-inhibit-startup t)
        (inhibit-message t)
        (gptel-org-debug nil)
        (gptel-org-ib--registry (make-hash-table :test 'equal))
        (gptel-org-use-todo-keywords t)
        (org-todo-keywords
         '((sequence "AI-DO" "AI-DOING" "FEEDBACK"
                     "|" "AI-DONE" "TERMINE"))))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (org-set-regexps-and-options)
      (insert "* Parent\n** Child\n** TERMINE\n")
      ;; (1) heading-is-assistant-p recognises TERMINE.
      (goto-char (point-min))
      (re-search-forward "^\\*\\* TERMINE")
      (beginning-of-line)
      (should (org-at-heading-p))
      (should (equal "TERMINE" (org-get-todo-state)))
      (should (gptel-org--heading-is-assistant-p))
      ;; (2) find-terminator locates the TERMINE heading at child
      ;; level via basic round-trip.
      (goto-char (point-min))
      (org-back-to-heading t)
      (let ((pos (gptel-org-ib-find-terminator "TERMINE")))
        (should pos)
        (save-excursion
          (goto-char pos)
          (should (org-at-heading-p))
          (should (= 2 (org-current-level)))
          (should (equal "TERMINE" (org-get-todo-state))))))))

;;; ---- Remove Terminator (IB-4.3) ------------------------------------------

(ert-deftest gptel-org-ib-test-remove-terminator-removes-existing ()
  "`remove-terminator' deletes an existing TERMINE child and returns t."
  (gptel-org-ib-test-with-buffer
      "* Parent\n** Child\nbody\n** TERMINE\n"
    (goto-char (point-min))
    (org-back-to-heading t)
    ;; Sanity: TERMINE present before removal.
    (should (gptel-org-ib-find-terminator "TERMINE"))
    (let ((result (gptel-org-ib-remove-terminator "TERMINE")))
      (should (eq result t)))
    ;; TERMINE gone; Parent and Child still present.
    (goto-char (point-min))
    (org-back-to-heading t)
    (should-not (gptel-org-ib-find-terminator "TERMINE"))
    (should (= 0 (how-many "^\\*+ TERMINE\\b" (point-min) (point-max))))
    (should (= 1 (how-many "^\\* Parent" (point-min) (point-max))))
    (should (= 1 (how-many "^\\*\\* Child" (point-min) (point-max))))))

(ert-deftest gptel-org-ib-test-remove-terminator-idempotent ()
  "`remove-terminator' is a no-op when no terminator exists; returns nil."
  (gptel-org-ib-test-with-buffer
      "* Parent\n** Child\nbody\n"
    (goto-char (point-min))
    (org-back-to-heading t)
    (let ((before (buffer-string))
          (result (gptel-org-ib-remove-terminator "TERMINE")))
      (should (null result))
      ;; Buffer content unchanged.
      (should (equal before (buffer-string))))))

(ert-deftest gptel-org-ib-test-remove-terminator-fatal-on-non-empty-body ()
  "`remove-terminator' raises `user-error' when the terminator has body content."
  (gptel-org-ib-test-with-buffer
      "* Parent\n** Child\n** TERMINE\ngarbage\n"
    (goto-char (point-min))
    (org-back-to-heading t)
    (should-error (gptel-org-ib-remove-terminator "TERMINE")
                  :type 'user-error)
    ;; TERMINE should still be present since removal aborted.
    (goto-char (point-min))
    (org-back-to-heading t)
    (should (gptel-org-ib-find-terminator "TERMINE"))))

(ert-deftest gptel-org-ib-test-remove-terminator-removes-duplicates ()
  "`remove-terminator' removes all TERMINE siblings when multiple exist.
This shouldn't happen in valid data but the API defends against it."
  (gptel-org-ib-test-with-buffer
      "* Parent\n** Child\n** TERMINE\n** TERMINE\n"
    (goto-char (point-min))
    (org-back-to-heading t)
    (let ((result (gptel-org-ib-remove-terminator "TERMINE")))
      (should (eq result t)))
    (goto-char (point-min))
    (should (= 0 (how-many "^\\*+ TERMINE\\b" (point-min) (point-max))))))

(ert-deftest gptel-org-ib-test-remove-terminator-legacy-keywords ()
  "`remove-terminator' works for legacy RESULTS/FEEDBACK empty terminators."
  (gptel-org-ib-test-with-buffer
      "* Parent\n** Child\n** RESULTS\n"
    (goto-char (point-min))
    (org-back-to-heading t)
    (should (eq t (gptel-org-ib-remove-terminator "RESULTS")))
    (should (= 0 (how-many "^\\*+ RESULTS\\b" (point-min) (point-max)))))
  (gptel-org-ib-test-with-buffer
      "* Parent\n** Child\n** FEEDBACK\n"
    (goto-char (point-min))
    (org-back-to-heading t)
    (should (eq t (gptel-org-ib-remove-terminator "FEEDBACK")))
    (should (= 0 (how-many "^\\*+ FEEDBACK\\b" (point-min) (point-max))))))

(ert-deftest gptel-org-ib-test-remove-terminator-fatal-when-not-at-heading ()
  "`remove-terminator' is fatal when point is not on a heading."
  (gptel-org-ib-test-with-buffer
      "* Parent\nbody line\n** Child\n** TERMINE\n"
    ;; Move into Parent's body text (line 2), not on a heading.
    (goto-char (point-min))
    (forward-line 1)
    (should-not (org-at-heading-p))
    (should-error (gptel-org-ib-remove-terminator "TERMINE")
                  :type 'user-error)))

(ert-deftest gptel-org-ib-test-termine-removed-on-ai-done ()
  "After simulating agent completion (AI-DONE transition), TERMINE is gone.

Integration-flavoured test covering the IB-4.3 acceptance criterion:
when an agent subtree transitions to a terminal state, the TERMINE
placeholder child is removed from the subtree.

Since `gptel-org-agent--insert-user-heading' is tightly coupled to
FSM/hook state that is hard to reproduce in isolation, this test
exercises the primitive directly (the code path in
`gptel-org-agent--insert-user-heading' after the agent heading
transitions to AI-DONE) and asserts the observable effect."
  (gptel-org-ib-test-with-buffer
      "* AI-DOING Task\n** AI-DOING Agent :main@agent:\n*** CONTENT\nbody\n*** TERMINE\n"
    (goto-char (point-min))
    (re-search-forward "^\\*\\* AI-DOING Agent")
    (beginning-of-line)
    ;; Sanity: TERMINE exists under the agent subtree.
    (should (gptel-org-ib-find-terminator "TERMINE"))
    ;; Simulate the agent-completion transition: AI-DOING -> AI-DONE
    ;; plus tag removal (mirrors the code in
    ;; gptel-org-agent--insert-user-heading).
    (let ((inhibit-message t))
      (org-todo "AI-DONE"))
    (org-set-tags nil)
    ;; Now remove the TERMINE child, as IB-4.3 does.
    (org-back-to-heading t)
    (should (eq t (gptel-org-ib-remove-terminator "TERMINE")))
    ;; TERMINE is gone; CONTENT child remains.
    (goto-char (point-min))
    (should (= 0 (how-many "^\\*+ TERMINE\\b" (point-min) (point-max))))
    (should (= 1 (how-many "^\\*\\*\\* CONTENT\\b" (point-min) (point-max))))
    ;; Agent heading is AI-DONE with no tags.
    (goto-char (point-min))
    (re-search-forward "^\\*\\* AI-DONE Agent")
    (beginning-of-line)
    (should (org-at-heading-p))
    (should (equal "AI-DONE" (org-get-todo-state)))
    (should (null (org-get-tags nil t)))))

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


(ert-deftest ib-parent-child-links ()
  "Creating nested IBs wires parent/children; closing child unwires them.

Scenario:
  - Base buffer with nested headings A (outer) and B (inner, under A).
  - Create IB A from the base buffer (parent-node is nil, since the
    current buffer at call time is the base).
  - Switch INTO IB A and create IB B from there (parent-node is node-A).

Assertions on create:
  - node-A has nil parent (top-level).
  - node-B has parent eq node-A.
  - node-A.children contains node-B.

Assertions on close (of B):
  - node-A.children no longer contains node-B.
  - node-B is no longer in the registry."
  (gptel-org-ib-test-with-buffer
      "* A  :main@agent:\n** B  :researcher@main@agent:\nb body\n"
    (gptel-org-ib-test-with-cleanup
     (let* ((base (current-buffer))
            (pos-a (progn (goto-char (point-min))
                          (re-search-forward "^\\* A")
                          (beginning-of-line)
                          (point)))
            (pos-b (progn (goto-char (point-min))
                          (re-search-forward "^\\*\\* B")
                          (beginning-of-line)
                          (point)))
            (ib-a (gptel-org-ib-create base pos-a))
            ;; Create IB B from inside IB A so parent-node is node-A.
            (ib-b (with-current-buffer ib-a
                    (gptel-org-ib-create base pos-b)))
            (node-a (gptel-org-ib--get-node (buffer-name ib-a)))
            (node-b (gptel-org-ib--get-node (buffer-name ib-b))))
       ;; Sanity
       (should (gptel-org-ib-node-p node-a))
       (should (gptel-org-ib-node-p node-b))
       ;; Parent/children wiring
       (should (null  (gptel-org-ib-node-parent node-a)))
       (should (eq    (gptel-org-ib-node-parent node-b) node-a))
       (should (memq  node-b (gptel-org-ib-node-children node-a)))
       ;; Close B — it should splice itself out of A's children.
       (gptel-org-ib-close ib-b)
       (should-not (gptel-org-ib--get-node (buffer-name ib-b)))
       (should-not (memq node-b (gptel-org-ib-node-children node-a)))))))


(ert-deftest ib-parent-returns-node-or-base ()
  "`gptel-org-ib-parent' returns parent node, or base buffer when parent is nil.

- For a top-level IB (node.parent is nil) it returns the base buffer.
- For a nested IB it returns the parent node struct."
  (gptel-org-ib-test-with-buffer
      "* A  :main@agent:\n** B  :researcher@main@agent:\nb body\n"
    (gptel-org-ib-test-with-cleanup
     (let* ((base (current-buffer))
            (pos-a (progn (goto-char (point-min))
                          (re-search-forward "^\\* A")
                          (beginning-of-line)
                          (point)))
            (pos-b (progn (goto-char (point-min))
                          (re-search-forward "^\\*\\* B")
                          (beginning-of-line)
                          (point)))
            (ib-a (gptel-org-ib-create base pos-a))
            (ib-b (with-current-buffer ib-a
                    (gptel-org-ib-create base pos-b)))
            (node-a (gptel-org-ib--get-node (buffer-name ib-a))))
       ;; Top-level IB: parent-slot is nil, resolver returns BASE buffer.
       (should (eq (gptel-org-ib-parent ib-a) base))
       ;; Nested IB: resolver returns the parent node struct.
       (should (eq (gptel-org-ib-parent ib-b) node-a))
       ;; Accepts a buffer-name string too.
       (should (eq (gptel-org-ib-parent (buffer-name ib-a)) base))
       ;; Unregistered buffer → user-error.
       (should-error (gptel-org-ib-parent "*nonexistent-gptel-ib*")
                     :type 'user-error)))))

(ert-deftest ib-children-returns-registered-list ()
  "`gptel-org-ib-children' returns the list of child nodes of IB.

- Top-level IB with no children returns nil.
- After nesting an IB beneath it, the child node appears in the list.
- After closing the child, the list is emptied again.
- Accepts a buffer-name string as well as a buffer object.
- Unregistered buffers raise `user-error'."
  (gptel-org-ib-test-with-buffer
      "* A  :main@agent:\n** B  :researcher@main@agent:\nb body\n"
    (gptel-org-ib-test-with-cleanup
     (let* ((base (current-buffer))
            (pos-a (progn (goto-char (point-min))
                          (re-search-forward "^\\* A")
                          (beginning-of-line)
                          (point)))
            (pos-b (progn (goto-char (point-min))
                          (re-search-forward "^\\*\\* B")
                          (beginning-of-line)
                          (point)))
            (ib-a (gptel-org-ib-create base pos-a)))
       ;; No children registered yet.
       (should (null (gptel-org-ib-children ib-a)))
       ;; Create nested IB B under A.
       (let* ((ib-b (with-current-buffer ib-a
                      (gptel-org-ib-create base pos-b)))
              (node-b (gptel-org-ib--get-node (buffer-name ib-b)))
              (children (gptel-org-ib-children ib-a)))
         (should (listp children))
         (should (memq node-b children))
         ;; Accepts a buffer-name string.
         (should (memq node-b (gptel-org-ib-children (buffer-name ib-a))))
         ;; Closing the child splices it out of the list.
         (gptel-org-ib-close ib-b)
         (should (null (gptel-org-ib-children ib-a))))
       ;; Unregistered buffer → user-error.
       (should-error (gptel-org-ib-children "*nonexistent-gptel-ib*")
                     :type 'user-error)))))

(ert-deftest ib-base-walks-chain ()
  "`gptel-org-ib-base' walks the registry parent chain to the base buffer.

Creates a nested IB-of-IB (B inside A, both backed by the same base
file buffer) and verifies that `gptel-org-ib-base' returns the base
buffer for both the outer IB and the nested IB — by traversing the
node.parent chain in the registry, not by calling `buffer-base-buffer'
on the indirect buffer.

Also verifies:
- Accepts both a buffer object and a buffer-name string.
- Signals `user-error' for buffers that are not registered gptel IBs."
  (gptel-org-ib-test-with-buffer
      "* A  :main@agent:\n** B  :researcher@main@agent:\nb body\n"
    (gptel-org-ib-test-with-cleanup
     (let* ((base (current-buffer))
            (pos-a (progn (goto-char (point-min))
                          (re-search-forward "^\\* A")
                          (beginning-of-line)
                          (point)))
            (pos-b (progn (goto-char (point-min))
                          (re-search-forward "^\\*\\* B")
                          (beginning-of-line)
                          (point)))
            (ib-a (gptel-org-ib-create base pos-a))
            (ib-b (with-current-buffer ib-a
                    (gptel-org-ib-create base pos-b))))
       ;; Sanity check: nested IB's node.parent points at the outer
       ;; node (not at the base buffer), so the resolver must walk up.
       (let ((node-a (gptel-org-ib--get-node (buffer-name ib-a)))
             (node-b (gptel-org-ib--get-node (buffer-name ib-b))))
         (should (eq (gptel-org-ib-node-parent node-b) node-a))
         (should (null (gptel-org-ib-node-parent node-a))))
       ;; Top-level IB resolves to base directly.
       (should (eq base (gptel-org-ib-base ib-a)))
       ;; Nested IB walks the chain to the same base.
       (should (eq base (gptel-org-ib-base ib-b)))
       ;; Accepts a buffer-name string.
       (should (eq base (gptel-org-ib-base (buffer-name ib-b))))
       ;; The resolver must NOT rely on `buffer-base-buffer' — shadow
       ;; it with a signalling stub and confirm the result is unchanged.
       (cl-letf (((symbol-function 'buffer-base-buffer)
                  (lambda (&rest _)
                    (error "buffer-base-buffer must not be called"))))
         (should (eq base (gptel-org-ib-base ib-b)))
         (should (eq base (gptel-org-ib-base ib-a))))
       ;; Unregistered buffer → user-error.
       (should-error (gptel-org-ib-base "*nonexistent-gptel-ib*")
                     :type 'user-error)
       ;; Invalid IB argument → user-error.
       (should-error (gptel-org-ib-base 42) :type 'user-error)))))


(ert-deftest ib-bounds-matches-narrowing ()
  "`gptel-org-ib-bounds' reads only from the node's heading/end markers.

For every registered IB the returned (START . END) must equal the
IB's current narrowing — i.e. `point-min' and `point-max' observed
inside the indirect buffer — and the positions must be expressed in
the IB's parent buffer (the base file buffer for a top-level IB, the
same base buffer for a nested IB).

Also verifies:
- Accepts both a buffer object and a buffer-name string.
- The resolver reads from the node's `:heading-marker'/`:end-marker'
  slots only: shadowing `buffer-base-buffer', `point-min' and
  `point-max' with signalling stubs must not change the result.
- Works for nested IB-of-IB (both bounds resolvable, independent).
- Expanding the `:end-marker' (insertion-type=t behaviour) is
  reflected in the bounds, matching the updated narrowing.
- Signals `user-error' for unregistered buffers, invalid arguments,
  and for nodes whose markers have been cleared."
  (gptel-org-ib-test-with-buffer
      "* A  :main@agent:\nA body\n** B  :researcher@main@agent:\nb body\n* C\nc body\n"
    (gptel-org-ib-test-with-cleanup
     (let* ((base (current-buffer))
            (pos-a (progn (goto-char (point-min))
                          (re-search-forward "^\\* A")
                          (beginning-of-line)
                          (point)))
            (pos-b (progn (goto-char (point-min))
                          (re-search-forward "^\\*\\* B")
                          (beginning-of-line)
                          (point)))
            (ib-a (gptel-org-ib-create base pos-a))
            (ib-b (with-current-buffer ib-a
                    (gptel-org-ib-create base pos-b))))
       ;; --- Top-level IB: bounds match its narrowing in the base buffer.
       (let ((bounds (gptel-org-ib-bounds ib-a)))
         (should (consp bounds))
         (should (integerp (car bounds)))
         (should (integerp (cdr bounds)))
         (should (= (car bounds)
                    (with-current-buffer ib-a (point-min))))
         (should (= (cdr bounds)
                    (with-current-buffer ib-a (point-max))))
         ;; START is the heading position in the parent (= base) buffer.
         (should (= (car bounds) pos-a)))
       ;; --- Nested IB: same bounds convention; positions are in the
       ;; shared base buffer (which is ib-b's parent buffer in the
       ;; Emacs sense — indirect-of-indirect is flattened to root).
       (let ((bounds (gptel-org-ib-bounds ib-b)))
         (should (= (car bounds)
                    (with-current-buffer ib-b (point-min))))
         (should (= (cdr bounds)
                    (with-current-buffer ib-b (point-max))))
         (should (= (car bounds) pos-b)))
       ;; --- Accepts a buffer-name string.
       (should (equal (gptel-org-ib-bounds ib-a)
                      (gptel-org-ib-bounds (buffer-name ib-a))))
       ;; --- Resolver must read markers ONLY.  Shadow
       ;; `buffer-base-buffer' with a signalling stub and confirm the
       ;; result is unchanged — the resolver cannot be consulting the
       ;; indirect buffer's base chain to reconstruct bounds.
       (let ((expected-a (gptel-org-ib-bounds ib-a))
             (expected-b (gptel-org-ib-bounds ib-b)))
         (cl-letf (((symbol-function 'buffer-base-buffer)
                    (lambda (&rest _)
                      (error "buffer-base-buffer must not be called"))))
           (should (equal expected-a (gptel-org-ib-bounds ib-a)))
           (should (equal expected-b (gptel-org-ib-bounds ib-b)))))
       ;; --- Resolver must read the registry markers directly.
       ;; Mutating the IB's narrowing (widen) must NOT change the
       ;; bounds, since bounds come from `:heading-marker'/`:end-marker'
       ;; and not from `point-min'/`point-max' of the indirect buffer.
       (let ((expected (gptel-org-ib-bounds ib-a)))
         (with-current-buffer ib-a
           (save-restriction
             (widen)
             (should (equal expected (gptel-org-ib-bounds ib-a))))))
       ;; --- Expanding end-marker: inserting text inside the IB (at
       ;; its point-max) advances the end-marker (insertion-type=t),
       ;; and the bounds reflect that, continuing to match narrowing.
       (let ((before (gptel-org-ib-bounds ib-a)))
         (with-current-buffer ib-a
           (goto-char (point-max))
           (insert "extra streamed text\n"))
         (let ((after (gptel-org-ib-bounds ib-a)))
           (should (= (car after) (car before)))
           (should (> (cdr after) (cdr before)))
           (should (= (car after)
                      (with-current-buffer ib-a (point-min))))
           (should (= (cdr after)
                      (with-current-buffer ib-a (point-max))))))
       ;; --- Cleared markers: unregister ib-b and confirm a lookup
       ;; by its (now stale) buffer name signals a user-error rather
       ;; than returning silently stale bounds.
       (let ((name-b (buffer-name ib-b)))
         (gptel-org-ib-unregister name-b)
         (should-error (gptel-org-ib-bounds name-b) :type 'user-error))
       ;; --- Unregistered buffer → user-error.
       (should-error (gptel-org-ib-bounds "*nonexistent-gptel-ib*")
                     :type 'user-error)
       ;; --- Invalid IB argument → user-error.
       (should-error (gptel-org-ib-bounds 42) :type 'user-error)))))


(ert-deftest ib-point-respects-narrowing ()
  "`gptel-org-ib-point' returns the IB's own `point', clamped to its narrowing.

Each indirect buffer has an independent `point' from its base buffer.
The resolver must return that per-IB point, and the value must always
lie within the IB's accessible region (`point-min' .. `point-max').

Also verifies:
- Returns an integer that equals `(with-current-buffer ib (point))'.
- Moving point inside the IB is reflected on the next call.
- Accepts both a buffer object and a buffer-name string.
- Signals `user-error' for unregistered buffers and invalid arguments."
  (gptel-org-ib-test-with-buffer
      "* A  :main@agent:\nA body line 1\nA body line 2\n** B  :researcher@main@agent:\nb body\n* C\n"
    (gptel-org-ib-test-with-cleanup
     (let* ((base (current-buffer))
            (pos-a (progn (goto-char (point-min))
                          (re-search-forward "^\\* A")
                          (beginning-of-line)
                          (point)))
            (pos-b (progn (goto-char (point-min))
                          (re-search-forward "^\\*\\* B")
                          (beginning-of-line)
                          (point)))
            (ib-a (gptel-org-ib-create base pos-a))
            (ib-b (with-current-buffer ib-a
                    (gptel-org-ib-create base pos-b))))
       (dolist (ib (list ib-a ib-b))
         ;; Returns an integer.
         (should (integerp (gptel-org-ib-point ib)))
         ;; Value equals IB's own point.
         (should (= (gptel-org-ib-point ib)
                    (with-current-buffer ib (point))))
         ;; Value lies within IB's narrowing.
         (let ((p (gptel-org-ib-point ib)))
           (should (<= (with-current-buffer ib (point-min)) p))
           (should (<= p (with-current-buffer ib (point-max)))))
         ;; Moving point inside the IB is reflected on the next call.
         (let ((new-pos (with-current-buffer ib
                          (goto-char (point-min))
                          (forward-line 1)
                          (point))))
           (should (= new-pos (gptel-org-ib-point ib)))
           ;; Still within narrowing.
           (should (<= (with-current-buffer ib (point-min)) new-pos))
           (should (<= new-pos (with-current-buffer ib (point-max)))))
         ;; Accepts a buffer-name string.
         (should (= (gptel-org-ib-point ib)
                    (gptel-org-ib-point (buffer-name ib)))))
       ;; Unregistered buffer → user-error.
       (should-error (gptel-org-ib-point "*nonexistent-gptel-ib*")
                     :type 'user-error)
       ;; Invalid IB argument → user-error.
       (should-error (gptel-org-ib-point 42) :type 'user-error)))))


(ert-deftest ib-absolute-position-nested ()
  "`gptel-org-ib-absolute-position' returns (BASE . POS) via the logical chain.

Builds a 3-level chain of indirect buffers (A ⊃ B ⊃ C) all backed by
the same base file buffer, and verifies that the resolver returns the
base buffer and IB's current point (already an absolute position in
the shared base buffer) at every level.

Also verifies:
- The parent chain in the registry is logical (node.parent links), so
  node-c's parent is node-b, node-b's parent is node-a, node-a's
  parent is nil.
- The result's car is the base file buffer.
- The result's cdr is an integer equal to `(with-current-buffer ib (point))'.
- Moving point inside an IB is reflected on the next call.
- Accepts both a buffer object and a buffer-name string.
- The resolver walks the registry, not Emacs's indirect chain:
  shadowing `buffer-base-buffer' with a signalling stub does not
  change the result for any level.
- Signals `user-error' for unregistered buffers and invalid arguments."
  (gptel-org-ib-test-with-buffer
      "* A  :main@agent:\nA body\n** B  :researcher@main@agent:\nb body\n*** C  :nested@researcher@main@agent:\nc body\n"
    (gptel-org-ib-test-with-cleanup
     (let* ((base (current-buffer))
            (pos-a (progn (goto-char (point-min))
                          (re-search-forward "^\\* A")
                          (beginning-of-line)
                          (point)))
            (pos-b (progn (goto-char (point-min))
                          (re-search-forward "^\\*\\* B")
                          (beginning-of-line)
                          (point)))
            (pos-c (progn (goto-char (point-min))
                          (re-search-forward "^\\*\\*\\* C")
                          (beginning-of-line)
                          (point)))
            (ib-a (gptel-org-ib-create base pos-a))
            (ib-b (with-current-buffer ib-a
                    (gptel-org-ib-create base pos-b)))
            (ib-c (with-current-buffer ib-b
                    (gptel-org-ib-create base pos-c))))
       ;; --- Sanity: logical parent chain in the registry is 3 levels.
       (let ((node-a (gptel-org-ib--get-node (buffer-name ib-a)))
             (node-b (gptel-org-ib--get-node (buffer-name ib-b)))
             (node-c (gptel-org-ib--get-node (buffer-name ib-c))))
         (should (eq (gptel-org-ib-node-parent node-c) node-b))
         (should (eq (gptel-org-ib-node-parent node-b) node-a))
         (should (null (gptel-org-ib-node-parent node-a))))
       ;; --- Per-IB: result is a cons (base . point-in-ib).
       (dolist (ib (list ib-a ib-b ib-c))
         (let ((result (gptel-org-ib-absolute-position ib)))
           (should (consp result))
           (should (eq base (car result)))
           (should (integerp (cdr result)))
           (should (= (cdr result) (with-current-buffer ib (point)))))
         ;; --- Accepts a buffer-name string.
         (should (equal (gptel-org-ib-absolute-position ib)
                        (gptel-org-ib-absolute-position (buffer-name ib)))))
       ;; --- Moving point inside an IB is reflected on the next call.
       (let ((new-pos (with-current-buffer ib-b
                        (goto-char (point-min))
                        (forward-line 1)
                        (point))))
         (let ((result (gptel-org-ib-absolute-position ib-b)))
           (should (eq base (car result)))
           (should (= new-pos (cdr result)))))
       ;; --- Resolver must NOT rely on `buffer-base-buffer' — shadow
       ;; it with a signalling stub and confirm results are unchanged
       ;; for every level of the chain.
       (let ((expected-a (gptel-org-ib-absolute-position ib-a))
             (expected-b (gptel-org-ib-absolute-position ib-b))
             (expected-c (gptel-org-ib-absolute-position ib-c)))
         (cl-letf (((symbol-function 'buffer-base-buffer)
                    (lambda (&rest _)
                      (error "buffer-base-buffer must not be called"))))
           (should (equal expected-a (gptel-org-ib-absolute-position ib-a)))
           (should (equal expected-b (gptel-org-ib-absolute-position ib-b)))
           (should (equal expected-c (gptel-org-ib-absolute-position ib-c)))))
       ;; --- Unregistered buffer → user-error.
       (should-error (gptel-org-ib-absolute-position "*nonexistent-gptel-ib*")
                     :type 'user-error)
       ;; --- Invalid IB argument → user-error.
       (should-error (gptel-org-ib-absolute-position 42) :type 'user-error)))))


(ert-deftest ib-api-is-exclusive-resolver ()
  "The canonical resolver API must never call `buffer-base-buffer'.

Phase IB-2.7: shadow `buffer-base-buffer' with a signalling stub and
confirm every public `gptel-org-ib-*' resolver still succeeds.  Any
silent fallback to Emacs's low-level indirect-buffer relationship
would surface as the stub's error and fail the test.

Covers `gptel-org-ib-registered-p', `gptel-org-ib-parent',
`gptel-org-ib-base', `gptel-org-ib-bounds', `gptel-org-ib-children',
`gptel-org-ib-point', and `gptel-org-ib-absolute-position' over a
2-level IB chain (base -> ib-a -> ib-b)."
  (gptel-org-ib-test-with-buffer
      "* A  :main@agent:\nA body\n** B  :researcher@main@agent:\nb body\n"
    (gptel-org-ib-test-with-cleanup
     (let* ((base (current-buffer))
            (pos-a (progn (goto-char (point-min))
                          (re-search-forward "^\\* A")
                          (beginning-of-line)
                          (point)))
            (pos-b (progn (goto-char (point-min))
                          (re-search-forward "^\\*\\* B")
                          (beginning-of-line)
                          (point)))
            (ib-a (gptel-org-ib-create base pos-a))
            (ib-b (with-current-buffer ib-a
                    (gptel-org-ib-create base pos-b)))
            (node-b (gptel-org-ib--get-node (buffer-name ib-b)))
            (node-a (gptel-org-ib--get-node (buffer-name ib-a))))
       ;; Sanity: both IBs created.
       (should (bufferp ib-a))
       (should (bufferp ib-b))
       ;; --- Shadow `buffer-base-buffer' with a signalling stub and
       ;; exercise every resolver.  Any silent fallback to Emacs's
       ;; low-level indirect-buffer chain would trigger the error.
       (cl-letf* (((symbol-function 'buffer-base-buffer)
                   (lambda (&rest args)
                     (error
                      "buffer-base-buffer called inside exclusive-resolver test: args=%S"
                      args))))
         ;; registered-p
         (should (gptel-org-ib-registered-p ib-a))
         (should (gptel-org-ib-registered-p ib-b))
         (should (gptel-org-ib-registered-p (buffer-name ib-a)))
         (should-not (gptel-org-ib-registered-p base))
         (should-not (gptel-org-ib-registered-p "*no-such-gptel-ib*"))
         ;; parent: top-level IB's parent is the base buffer object;
         ;; nested IB's parent is the outer node.
         (should (eq base (gptel-org-ib-parent ib-a)))
         (should (eq node-a (gptel-org-ib-parent ib-b)))
         ;; base: walks the logical registry chain to the file buffer.
         (should (eq base (gptel-org-ib-base ib-a)))
         (should (eq base (gptel-org-ib-base ib-b)))
         ;; bounds: (START . END) integers in the parent buffer.
         (let ((bounds-a (gptel-org-ib-bounds ib-a)))
           (should (consp bounds-a))
           (should (integerp (car bounds-a)))
           (should (integerp (cdr bounds-a)))
           (should (= (car bounds-a) pos-a)))
         (let ((bounds-b (gptel-org-ib-bounds ib-b)))
           (should (consp bounds-b))
           (should (integerp (car bounds-b)))
           (should (integerp (cdr bounds-b)))
           (should (= (car bounds-b) pos-b)))
         ;; children: ib-a has ib-b as its registered child; ib-b has none.
         (let ((kids-a (gptel-org-ib-children ib-a)))
           (should (listp kids-a))
           (should (memq node-b kids-a)))
         (should (null (gptel-org-ib-children ib-b)))
         ;; point: per-IB integer.
         (should (integerp (gptel-org-ib-point ib-a)))
         (should (integerp (gptel-org-ib-point ib-b)))
         ;; absolute-position: (BASE . INT) with BASE = file buffer.
         (let ((ap-a (gptel-org-ib-absolute-position ib-a)))
           (should (consp ap-a))
           (should (eq base (car ap-a)))
           (should (integerp (cdr ap-a))))
         (let ((ap-b (gptel-org-ib-absolute-position ib-b)))
           (should (consp ap-b))
           (should (eq base (car ap-b)))
           (should (integerp (cdr ap-b)))))
       ;; --- Cleanup outside the letf* block: teardown may legitimately
       ;; call `buffer-base-buffer' while killing indirect buffers.
       (when (buffer-live-p ib-b) (gptel-org-ib-close ib-b))
       (when (buffer-live-p ib-a) (gptel-org-ib-close ib-a))))))


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
           (name-b (gptel-org-ib-compute-name
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


(ert-deftest ib-fatal-signals-user-error ()
  "`gptel-org-ib-fatal' signals `user-error' with the formatted message."
  (let ((err (should-error (gptel-org-ib-fatal "boom %s %d" "x" 42)
                           :type 'user-error)))
    (should (string-match-p "boom x 42" (error-message-string err)))))


(ert-deftest ib-stale-registry-aborts-run ()
  "Canonical ops hard-fail with `user-error' on IB inconsistency.

Simulates a broken/collapsed IB state: an indirect buffer is
registered in the registry, but its visible region has been narrowed
such that `point-min' is no longer at a heading.  A canonical op
\(`gptel-org-ib-ensure-terminator') called on that buffer must abort
via `gptel-org-ib-fatal' (→ `user-error') rather than silently
coerce point via the old `ignore-errors'/`org-back-to-heading'
fallback.  This locks in the IB-3.2 hard-fail policy."
  (gptel-org-ib-test-with-buffer
      "* Parent\nbody line\n"
    (gptel-org-ib-test-with-cleanup
     (goto-char (point-min))
     (org-back-to-heading t)
     (let ((ib (gptel-org-ib-safe-insert-sibling
                "AI-DO" "Task" nil "FEEDBACK")))
       (should (buffer-live-p ib))
       (should (gptel-org-ib-get (buffer-name ib)))
       ;; Inject staleness: narrow the IB so point-min is no longer
       ;; anchored at a heading.  The registry still references this
       ;; IB, but its visible region is broken.
       (with-current-buffer ib
         (widen)
         (goto-char (point-max))
         (insert "free text line\n")
         (let ((body-start (save-excursion
                             (goto-char (point-min))
                             (forward-line 1)
                             (point))))
           (narrow-to-region body-start (point-max)))
         (goto-char (point-min))
         (should-not (org-at-heading-p))
         ;; Canonical op must hard-fail with `user-error', not silently
         ;; back-to-heading.
         (should-error (gptel-org-ib-ensure-terminator "FEEDBACK")
                       :type 'user-error))))))


(provide 'gptel-org-ib-test)
;;; gptel-org-ib-test.el ends here
