;;; gptel-org-mode-activation-test.el --- Tests for gptel-org-mode activation -*- lexical-binding: t; -*-

;; Regression tests for `gptel-org-mode' activation paths, especially
;; activation via file-local variables which previously caused a
;; recursive loop via `org-mode-restart' inside
;; `gptel-org--register-todo-keywords'.

(require 'ert)
(require 'cl-lib)
(require 'gptel)
(require 'gptel-org)
(require 'gptel-org-dashboard nil t)

;;; Tests

(ert-deftest gptel-test-org-mode-file-local-no-recursion ()
  "Activating `gptel-org-mode' via file-local variables must not recurse.

Regression test for the loop where `gptel-org--register-todo-keywords'
called `org-mode-restart', which ran `kill-all-local-variables' and then
`hack-local-variables' re-applied the file-local `gptel-org-mode'
activation, re-entering the activation chain in a tight recursive loop.

The fix replaced `org-mode-restart' with `org-set-regexps-and-options'
and marked `gptel-org-mode' as `permanent-local'.

The real bug trigger requires `hack-local-variables' to actually call
the mode function (not just set the variable), which only happens when
file-local vars use the `eval:' form.  Setting `gptel-org-mode: 1' as
a plain file-local variable only assigns the variable without invoking
the mode body, so it cannot reproduce the recursion.  This test uses
`# eval: (gptel-org-mode 1)' so the activation body actually runs end
to end through `hack-local-variables'.

We stub `gptel-org-dashboard-register' to count how many times the
activation chain executes: with the fix it runs exactly once; with the
buggy `org-mode-restart' it would re-enter via the eval applied by the
re-run of `hack-local-variables' inside `kill-all-local-variables' +
`org-mode'."
  (let* ((tmp (make-temp-file "gptel-org-flv-" nil ".org"))
         (register-call-count 0))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert "* Heading\n\nBody.\n\n"
                    "# Local Variables:\n"
                    "# eval: (gptel-org-mode 1)\n"
                    "# End:\n"))
          ;; Stub the dashboard-register side effect so we can count
          ;; how many times the activation chain runs.
          (cl-letf (((symbol-function 'gptel-org-dashboard-register)
                     (lambda (&rest _) (cl-incf register-call-count))))
            (let ((enable-local-variables :all)
                  (enable-local-eval t)
                  (org-inhibit-startup t)
                  (inhibit-message t)
                  ;; Bound `max-lisp-eval-depth' low-ish so a recursion
                  ;; bug fails loudly rather than silently appearing to
                  ;; pass.  800 leaves headroom for `find-file' itself.
                  (max-lisp-eval-depth 800))
              (let ((buf (find-file-noselect tmp)))
                (unwind-protect
                    (with-current-buffer buf
                      (should (derived-mode-p 'org-mode))
                      (should gptel-org-mode)
                      ;; The bug manifests as MANY register calls; the
                      ;; fix gives exactly one.
                      (should (= register-call-count 1)))
                  (kill-buffer buf))))))
      (when (file-exists-p tmp)
        (delete-file tmp)))))

(provide 'gptel-org-mode-activation-test)
;;; gptel-org-mode-activation-test.el ends here
