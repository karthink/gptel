;;; gptel-anthropic-test.el --- Tests for Anthropic support -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel-anthropic)

(ert-deftest gptel-anthropic-models-are-processed-on-load ()
  "Loading `gptel-anthropic' applies properties to its model symbols."
  (should (memq 'media (gptel--model-capabilities 'claude-sonnet-4-6)))
  (should (member "image/jpeg" (gptel--model-mime-types 'claude-sonnet-4-6))))

;;; gptel-anthropic-test.el ends here
