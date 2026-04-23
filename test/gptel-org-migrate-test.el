;;; gptel-org-migrate-test.el --- Tests for legacy terminator migration  -*- lexical-binding: t; -*-

;; Tests for `gptel-org-migrate-terminators' (IB-4.4): the one-shot
;; interactive command that rewrites legacy empty-body RESULTS /
;; FEEDBACK terminator headings to the canonical TERMINE keyword.
;;
;; Non-empty RESULTS/FEEDBACK headings (content containers, user
;; prompts with content) must NOT be rewritten.  The command defaults
;; to a DRY-RUN; a prefix argument actually applies the rewrite.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gptel)
(require 'gptel-org)

(defmacro gptel-org-migrate-test-with-buffer (content &rest body)
  "Create an org-mode buffer with CONTENT and execute BODY.
Sets up org-mode with RESULTS and FEEDBACK as TODO keywords so
the legacy grammar the migration command rewrites is actually
parsed, plus TERMINE (required by the migration preflight)."
  (declare (indent 1))
  `(let ((org-inhibit-startup t)
         (inhibit-message t)
         (org-todo-keywords
          '((sequence "AI-DO" "AI-DOING" "FEEDBACK"
                      "|" "AI-DONE" "RESULTS" "TERMINE"))))
     (with-temp-buffer
       (delay-mode-hooks (org-mode))
       (org-set-regexps-and-options)
       (insert ,content)
       (goto-char (point-min))
       ,@body)))

(ert-deftest gptel-org-migrate-terminators-rewrites-headings ()
  "`gptel-org-migrate-terminators' rewrites empty RESULTS/FEEDBACK only.

Verifies the full matrix required by IB-4.4:
- empty-body RESULTS  => TERMINE (with apply flag)
- empty-body FEEDBACK => TERMINE (with apply flag)
- non-empty RESULTS   => left alone (content container)
- non-empty FEEDBACK  => left alone (user prompt with content)
- dry-run (no prefix) => no buffer change
- idempotency         => second run is a no-op (0 candidates)."
  ;; --- Errors in non-org buffer -------------------------------------------
  (with-temp-buffer
    (should-error (gptel-org-migrate-terminators)
                  :type 'user-error))

  ;; --- Errors when TERMINE not registered ---------------------------------
  (let ((org-inhibit-startup t)
        (inhibit-message t)
        (org-todo-keywords
         '((sequence "AI-DO" "|" "AI-DONE" "RESULTS" "FEEDBACK"))))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (org-set-regexps-and-options)
      (insert "* Parent\n** RESULTS\n")
      (should-error (gptel-org-migrate-terminators)
                    :type 'user-error)))

  ;; --- Dry-run on a fresh buffer leaves it untouched ----------------------
  (gptel-org-migrate-test-with-buffer
      "* AI-DOING Task
** AI-DOING Agent :main@agent:
*** TOOL call
some body
*** RESULTS
agent result stays
** RESULTS
** FEEDBACK
user content that must stay
** FEEDBACK
"
    (let ((before (buffer-string)))
      (should (= 2 (gptel-org-migrate-terminators)))
      (should (equal before (buffer-string)))
      ;; The dry-run report buffer mentions both candidate lines.
      (with-current-buffer (get-buffer "*gptel-org-migrate*")
        (let ((report (buffer-string)))
          (should (string-match-p "Would rewrite 2 heading" report))
          (should (string-match-p "1 RESULTS, 1 FEEDBACK" report))))
      (kill-buffer "*gptel-org-migrate*")))

  ;; --- Apply rewrites only empty-body terminators -------------------------
  (gptel-org-migrate-test-with-buffer
      "* AI-DOING Task
** AI-DOING Agent :main@agent:
*** TOOL call
some body
*** RESULTS
agent result stays
** RESULTS
** FEEDBACK
user content that must stay
** FEEDBACK
"
    (should (= 2 (gptel-org-migrate-terminators t)))
    ;; Empty-body RESULTS (line 7) and empty-body FEEDBACK (line 10)
    ;; are rewritten.
    (should (string-match-p "^\\*\\* TERMINE *$" (buffer-string)))
    ;; Count TERMINE occurrences.
    (goto-char (point-min))
    (should (= 2 (how-many "^\\*+ TERMINE\\(\\s-\\|$\\)"
                           (point-min) (point-max))))
    ;; The non-empty RESULTS content container must survive.
    (should (string-match-p "^\\*\\*\\* RESULTS$" (buffer-string)))
    (should (string-match-p "agent result stays" (buffer-string)))
    ;; The non-empty FEEDBACK user-prompt heading (with content) must
    ;; also survive — NOT rewritten.
    (should (= 1 (how-many "^\\*\\* FEEDBACK\\(\\s-\\|$\\)"
                           (point-min) (point-max))))
    (should (string-match-p "user content that must stay" (buffer-string)))

    ;; --- Idempotency: second apply sees 0 candidates ---------------------
    (let ((after-first-apply (buffer-string)))
      (should (= 0 (gptel-org-migrate-terminators t)))
      (should (equal after-first-apply (buffer-string))))
    ;; And a dry-run after migration also reports 0.
    (should (= 0 (gptel-org-migrate-terminators)))))

(ert-deftest gptel-org-migrate-terminators-preserves-tags-and-level ()
  "Rewrite preserves heading level, tags, and heading anatomy.

Only the TODO keyword portion is replaced; stars, tags, and any
priority cookie in the title stay put."
  ;; Two siblings so each terminator has a truly empty body.  If
  ;; FEEDBACK were nested under RESULTS the latter would have a
  ;; non-empty body (the child heading) and be correctly skipped —
  ;; we test heading-anatomy preservation, not nesting semantics.
  (gptel-org-migrate-test-with-buffer
      "* Top
** Parent A
**** RESULTS                                                      :legacy:
** Parent B
***** FEEDBACK :user:
"
    (should (= 2 (gptel-org-migrate-terminators t)))
    (goto-char (point-min))
    ;; Level-4 RESULTS => level-4 TERMINE, :legacy: tag preserved.
    (should (re-search-forward
             "^\\*\\*\\*\\* TERMINE[ \t]+.*:legacy:" nil t))
    (goto-char (point-min))
    ;; Level-5 FEEDBACK => level-5 TERMINE, :user: tag preserved.
    (should (re-search-forward
             "^\\*\\*\\*\\*\\* TERMINE[ \t]+:user:" nil t))))

(provide 'gptel-org-migrate-test)
;;; gptel-org-migrate-test.el ends here
