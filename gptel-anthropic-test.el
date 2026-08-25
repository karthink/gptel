;;; gptel-anthropic-test.el --- Tests for Anthropic support -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest gptel-anthropic-models-are-processed-on-load ()
  "Loading `gptel-anthropic' applies properties to its model symbols."
  (unload-feature 'gptel-anthropic t)
  (setf (symbol-plist 'claude-sonnet-4-6) nil)
  (require 'gptel-anthropic)
  (should (memq 'media (gptel--model-capabilities 'claude-sonnet-4-6)))
  (should (member "image/jpeg" (gptel--model-mimes 'claude-sonnet-4-6))))

;;; gptel-anthropic-test.el ends here
