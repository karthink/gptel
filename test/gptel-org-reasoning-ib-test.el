;;; gptel-org-reasoning-ib-test.el --- Tests for REASONING IB streaming  -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel)
(require 'gptel-org)

;;; Helper
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
         (gptel-org-todo-keywords '("AI-DO" "AI-DOING")))
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

(provide 'gptel-org-reasoning-ib-test)
;;; gptel-org-reasoning-ib-test.el ends here
