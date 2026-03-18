;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel)

;;; Test persistence of gptel state (bounds restoration)

(ert-deftest gptel-test-persist-restore-bounds ()
  "Test that gptel-mode correctly restores text properties from gptel--bounds."
  (skip-unless (fboundp 'markdown-mode))
  (let* ((gptel-track-response t)
         (inhibit-message t))
    (with-temp-buffer
      (insert-file-contents "examples/chat-restore.md" 'visit)
      ;; Set appropriate major mode before enabling gptel-mode
      (markdown-mode)
      ;; Enable gptel-mode, which triggers restoration
      (gptel-mode 1)

      ;; Verify that the bounds are correctly restored
      ;; Tool property at position 80 to 1346 with tool id
      (should (equal (get-text-property 80 'gptel)
                     '(tool . "toolu_01BUV8b8HrXnk2tVUKdZoBLj")))
      ;; Check a position inside the tool range
      (should (equal (get-text-property 1000 'gptel)
                     '(tool . "toolu_01BUV8b8HrXnk2tVUKdZoBLj")))

      ;; Ignore property at position 37 to 80
      (should (eq (get-text-property 40 'gptel) 'ignore))
      ;; Check a position inside the first ignore range
      (should (eq (get-text-property 60 'gptel) 'ignore))

      ;; Ignore property at position 1346 to 1351
      (should (eq (get-text-property 1348 'gptel) 'ignore))

      ;; Response property at position 1351 to 2903
      (should (eq (get-text-property 1352 'gptel) 'response))
      (should (eq (get-text-property 2000 'gptel) 'response))
      (should (eq (get-text-property 2900 'gptel) 'response))

      ;; Response property at position 2917 to 2971
      (should (eq (get-text-property 2918 'gptel) 'response))
      (should (eq (get-text-property 2950 'gptel) 'response))
      (should (eq (get-text-property 2970 'gptel) 'response))

      ;; Verify that positions outside bounds don't have gptel property
      (should (null (get-text-property 10 'gptel)))
      (should (null (get-text-property 3000 'gptel)))

      ;; Verify front-sticky for responses
      (should (get-text-property 1352 'front-sticky)))))

(provide 'gptel-persistence-test)
