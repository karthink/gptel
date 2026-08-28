;;; gptel-transient-memory-test.el --- Test for memory-report compatibility  -*- lexical-binding: t; -*-

;; This test verifies that the fix for GitHub issue #1032 works correctly.
;; The issue was that memory-report would fail when trying to compute the
;; size of gptel's transient history data containing backend structs.

;;; Code:

(require 'gptel-transient)
(require 'gptel-gemini)

;; Create a test backend
(defvar test-backend (gptel-make-gemini "Test-Gemini"
                                         :key "test-key"
                                         :stream t))

;; Create a provider object as used in gptel-menu
(defvar test-provider-obj (make-instance 'gptel-provider-variable
                                          :backend 'gptel-backend
                                          :variable 'gptel-model))

;; The fix: history-key and history-default should be nil to prevent
;; storing the backend struct in transient-history
(message "Testing memory-report compatibility fix...")
(message "history-key is nil: %s" (null (oref test-provider-obj history-key)))
(message "history-default is nil: %s" (null (oref test-provider-obj history-default)))

;; This prevents the backend struct (with its bytecode) from being
;; stored in transient-history, which was causing memory-report errors
(if (and (null (oref test-provider-obj history-key))
         (null (oref test-provider-obj history-default)))
    (message "✓ Fix verified: Backend structs will not be stored in transient history")
  (error "✗ Fix failed: Backend structs may still cause memory-report errors"))

(provide 'gptel-transient-memory-test)
;;; gptel-transient-memory-test.el ends here