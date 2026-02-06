;;; gptel-test-backends.el --- Shared test backends for gptel tests  -*- lexical-binding: t; -*-

;; This file defines the test backends used across all gptel test files.
;; By centralizing the backend definitions here, we avoid issues with
;; multiple defvar forms being evaluated in undefined order when files
;; are loaded via `make test`.

(require 'gptel)
(require 'gptel-openai)
(require 'gptel-anthropic)
(require 'gptel-gemini)
(require 'gptel-ollama)
(require 'gptel-openai-extras)
(require 'gptel-kagi)

(defvar gptel-test-backends
  `((openai    . ,(gptel--make-openai
                   :name "OpenAI" :models '(gpt-4o-mini)))
    (anthropic . ,(gptel--make-anthropic
                   :name "Claude" :models '(claude-3-5-sonnet-20240620)))
    (gemini    . ,(gptel--make-gemini
                   :name "Gemini" :models '(gemini-1.5-pro-latest)))
    (ollama    . ,(gptel--make-ollama
                   :name "Ollama" :models '(testmodel)))
    (deepseek  . ,(gptel--make-deepseek
                   :name "Deepseek" :models '(deepseek-reasoner)))
    (kagi      . ,(gptel--make-kagi :name "Kagi" :models '(fastgpt summarize:agnes))))
  "Dummy models for testing gptel.")

(provide 'gptel-test-backends)
;;; gptel-test-backends.el ends here
