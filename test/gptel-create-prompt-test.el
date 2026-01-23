;; -*- lexical-binding: t; -*-
(require 'ert)
(require 'gptel)
(require 'gptel-org)
(require 'gptel-test-backends)

;; Go from a buffer with included metadata to a list of prompts.

;;; General

(defmacro gptel-test-prompt-creation (name result-file &rest body)
  (declare (indent 2))
  `(ert-deftest
       ,(intern (concat "gptel-test-prompt-" name))
       ()
     (should (equal
              (with-temp-buffer
                (save-excursion (insert-file-contents ,result-file))
                (read (current-buffer)))
              ,(macroexp-progn body)))))

(defmacro with-gptel-chat-file (filename backend-sym &optional model &rest body)
  `(let* ((gptel-track-response t)
         (org-inhibit-startup t)
         (buf (delay-mode-hooks (find-file-noselect ,filename)))
         (gptel--num-messages-to-send nil)
         (gptel-context nil)
         (gptel-cache nil)
         (gptel--system-message gptel-test-system-message)
         (inhibit-message t))
    (with-current-buffer buf
      (deactivate-mark)
      (gptel--restore-state)
      (prog1
          (let* ((gptel-backend (alist-get ',backend-sym gptel-test-backends))
                 (gptel-prompt-prefix-alist
                  '((markdown-mode . "#### ") (org-mode . "*Prompt*: ")
                    (text-mode . "### ") (fundamental-mode . "*Prompt*: ")))
                 (gptel-response-prefix-alist
                  '((markdown-mode . "") (org-mode . "*Response*:\n")
                    (text-mode . "### ") (fundamental-mode . "*Response*:\n")))
                 (gptel-response-separator "\n\n")
                 (gptel-model (or ,model (car (gptel-backend-models gptel-backend)))))
            ,(macroexp-progn body))
        (set-buffer-modified-p nil)))))

(defconst gptel-test-system-message
  "To assist: Be very terse.  Respond in under 100 words if possible.  Speak in specific, topic relevant terminology. Do NOT hedge or qualify. Speak directly and be willing to make creative guesses. Explain your reasoning. if you don’t know, say you don’t know. Never apologize.  Ask questions when unsure."
  "System message for prompt creation tests.  Do not edit, as this
is used in the test data.")

;;; Setup code

(cl-defun gptel-test-write-prompt-data (source-file dest-file &key backend branching-context scope)
  "Generate test prompts.

SOURCE-FILE is an Org or MD file

DEST-FILE is the .eld file to write the prompt to

Keys:

BACKEND is one of openai, gemini, anthropic, ollama or kagi, see
`gptel-test-backends'.

BRANCHING-CONTEXT is a boolean specifying if branching context
should be used in Org mode.

SCOPE is the extent of the buffer to create a prompt from.  It
can be nil (for the whole buffer), a point value or the symbol
`region' for a specific region.  (The choice of region is
preselected for now, see below.)"
  (declare (indent 1))
  (let ((buf (find-file-noselect source-file))
        (file-name-handler-alist nil)
        (coding-system-for-write 'utf-8)
        (gptel-prompt-prefix-alist
         '((markdown-mode . "#### ")
           (org-mode . "*Prompt*: ")
           (text-mode . "#### ")))
        (gptel-response-prefix-alist
         '((markdown-mode . "")
           (org-mode . "*Response*:
")
           (text-mode . "")))
        (gptel--system-message gptel-test-system-message)
        (write-region-annotate-functions nil)
        (write-region-post-annotation-function nil)
        (gptel-org-branching-context branching-context))
    (with-current-buffer buf
      (deactivate-mark)
      (gptel--restore-state)
      ;; ;; For region
      (when (eq scope 'region)
        (goto-char 1046)
        (push-mark)
        (goto-char 2383)
        (activate-mark))
      (setq-local gptel-backend (alist-get backend gptel-test-backends)
                  gptel-model (car (gptel-backend-models gptel-backend)))
      (write-region (prin1-to-string
                     (gptel--create-prompt
                      (and (integerp scope) scope)))
                    nil dest-file)
      (set-buffer-modified-p nil))))

;; MD file end of prompt: 1792
;; Org file region bounds: (1046 . 2383)
;; Org file end of prompt: (point-max)

;; (gptel-test-write-prompt-data (expand-file-name "examples/prompt-creation.md")
;;                               (expand-file-name "examples/gemini-prompt-md.eld")
;;                               :backend 'gemini)

;; (gptel-test-write-prompt-data (expand-file-name "examples/prompt-creation.md")
;;                               (expand-file-name "examples/openai-prompt-md.eld")
;;                               :backend 'openai)

;; (gptel-test-write-prompt-data (expand-file-name "examples/prompt-creation.md")
;;                               (expand-file-name "examples/anthropic-prompt-md.eld")
;;                               :backend 'anthropic)

;; (gptel-test-write-prompt-data (expand-file-name "examples/prompt-creation.md")
;;                               (expand-file-name "examples/ollama-prompt-md.eld")
;;                               :backend 'ollama)

;; (gptel-test-write-prompt-data (expand-file-name "examples/prompt-creation-branching.org")
;;                               (expand-file-name "examples/openai-prompt-branching-org.eld")
;;                               :backend 'openai
;;                               :branching-context t)

;; (gptel-test-write-prompt-data (expand-file-name "examples/prompt-creation-branching.org")
;;                               (expand-file-name "examples/anthropic-prompt-branching-org.eld")
;;                               :backend 'anthropic
;;                               :branching-context t)

;; (gptel-test-write-prompt-data (expand-file-name "examples/prompt-creation-branching.org")
;;                               (expand-file-name "examples/gemini-prompt-region-org.eld")
;;                               :backend 'gemini)

;; (gptel-test-write-prompt-data (expand-file-name "examples/prompt-creation-branching.org")
;;                               (expand-file-name "examples/ollama-prompt-branching-org.eld")
;;                              :backend 'ollama
;;                              :branching-context t)

;; (gptel-test-write-prompt-data (expand-file-name "examples/prompt-creation-tool-block.org")
;;   (expand-file-name "examples/gemini-prompt-tool-block-org.eld")
;;   :backend 'gemini)

;; (gptel-test-write-prompt-data (expand-file-name "examples/prompt-creation-tool-block.org")
;;   (expand-file-name "examples/anthropic-prompt-tool-block-org.eld")
;;   :backend 'anthropic)

;; (gptel-test-write-prompt-data (expand-file-name "examples/prompt-creation-tool-block.org")
;;   (expand-file-name "examples/ollama-prompt-tool-block-org.eld")
;;   :backend 'ollama)


;;; Markdown
;;;; OpenAI
(gptel-test-prompt-creation
    "openai-md" "examples/openai-prompt-md.eld"
  (with-gptel-chat-file
   "examples/prompt-creation.md" openai nil
   (setf (alist-get 'fundamental-mode gptel-prompt-prefix-alist) "#### ")
   (gptel--create-prompt 1792)))

;; Test with the cursor before point-max (#723):
(gptel-test-prompt-creation
    "openai-md-before-max" "examples/openai-prompt-md-before-max.eld"
  (with-gptel-chat-file
   "examples/prompt-creation.md" openai nil
   (setf (alist-get 'fundamental-mode gptel-prompt-prefix-alist) "#### ")
   (goto-char 1492)
   (gptel--create-prompt)))

;;;; Anthropic
(gptel-test-prompt-creation
    "anthropic-md" "examples/anthropic-prompt-md.eld"
  (with-gptel-chat-file
   "examples/prompt-creation.md" anthropic nil
   (setf (alist-get 'fundamental-mode gptel-prompt-prefix-alist) "#### ")
   (gptel--create-prompt 1792)))

;;;; Gemini
(gptel-test-prompt-creation
    "gemini-md" "examples/gemini-prompt-md.eld"
  (with-gptel-chat-file
   "examples/prompt-creation.md" gemini nil
   (setf (alist-get 'fundamental-mode gptel-prompt-prefix-alist) "#### ")
   (gptel--create-prompt 1792)))

;;;; Ollama
(gptel-test-prompt-creation
    "ollama-md" "examples/ollama-prompt-md.eld"
  (with-gptel-chat-file
   "examples/prompt-creation.md" ollama nil
   (setf (alist-get 'fundamental-mode gptel-prompt-prefix-alist) "#### ")
   (gptel--create-prompt 1792)))

;;;; Kagi
;; TODO: Test for Kagi backend

;;; Org mode with branching
;;;; OpenAI
(gptel-test-prompt-creation
    "openai-branching-org" "examples/openai-prompt-branching-org.eld"
  (with-gptel-chat-file
   "examples/prompt-creation-branching.org" openai nil
   (let ((gptel-org-branching-context t)
         (gptel-org-ignore-elements nil)
         (gptel-prompt-filter-hook nil))
     (gptel--create-prompt (point-max)))))

;;;; Anthropic
(gptel-test-prompt-creation
    "anthropic-branching-org" "examples/anthropic-prompt-branching-org.eld"
  (with-gptel-chat-file
   "examples/prompt-creation-branching.org" anthropic nil
   (let ((gptel-org-branching-context t)
         (gptel-org-ignore-elements nil)
         (gptel-prompt-filter-hook nil))
     (gptel--create-prompt (point-max)))))

;;;; Gemini
(gptel-test-prompt-creation
    "gemini-branching-org" "examples/gemini-prompt-branching-org.eld"
  (with-gptel-chat-file
   "examples/prompt-creation-branching.org" gemini nil
   (let ((gptel-org-branching-context t)
         (gptel-org-ignore-elements nil)
         (gptel-prompt-filter-hook nil))
     (gptel--create-prompt (point-max)))))

;;;; Ollama
(gptel-test-prompt-creation
    "ollama-branching-org" "examples/ollama-prompt-branching-org.eld"
  (with-gptel-chat-file
   "examples/prompt-creation-branching.org" ollama nil
   (let ((gptel-org-branching-context t)
         (gptel-org-ignore-elements nil)
         (gptel-prompt-filter-hook nil))
     (gptel--create-prompt (point-max)))))

;;;; Kagi
;;;;; FastGPT
;; TODO
;;;;; Summarizer
;; TODO

;;;; Branching context edge cases (#476)
(ert-deftest gptel-test-prompt-branching-heading-at-top ()
  "Test branching prompt creation when there is an Org heading at
the top of the buffer.

This case requires special logic because of the behavior of
`org-element-lineage', see `gptel-org--create-prompt' for
details."
    (let ((gptel-track-response t)
          (gptel-org-branching-context t)
          (gptel--num-messages-to-send nil)
          (gptel-context nil)
          (gptel-backend (alist-get 'openai gptel-test-backends))
          (gptel--system-message gptel-test-system-message)
          (gptel-org-ignore-elements nil)
          (gptel-prompt-filter-hook nil)
          (inhibit-message t)
          (org-inhibit-startup t))
      (pcase-dolist (`(,text ,result)
                     ;; Case 1: Heading at (point-min) should be excluded
                     '(("*** This is heading 1

Irrelevant text

*** This is heading 2

Some details

**** This is heading 2.1"
                        ((:role "user" :content "*** This is heading 2

Some details

**** This is heading 2.1")))
                       ;; Case 2: Heading at (point-min) should be included
                       ("*** This is heading 1

Relevant text"
                        ((:role "user" :content "*** This is heading 1

Relevant text")))
                       ;; Case 3: Non heading text at (point-min) should be included
                       ("Some text
*** This is heading 1

Irrelevant text

*** This is heading 2

Some details"
                        ((:role "user" :content "Some text
*** This is heading 2

Some details")))))
        (should (equal
                 (with-temp-buffer
                   (let ((gptel-prompt-prefix-alist))
                     (delay-mode-hooks
                       (insert text)
                       (org-mode)
                       (setf (alist-get 'org-mode gptel-prompt-prefix-alist) nil)
                       (gptel--create-prompt (point-max)))))
                 result)))))

;;; Org-mode without branching
;;;; OpenAI
(gptel-test-prompt-creation "openai-org" "examples/openai-prompt-org.eld"
  (with-gptel-chat-file
   "examples/prompt-creation-branching.org" openai nil
   (let ((gptel-org-branching-context nil)
         (gptel-org-ignore-elements nil)
         (gptel-prompt-filter-hook nil))
     (gptel--create-prompt (point-max)))))

;; Test with the cursor before point-max (#723):
(gptel-test-prompt-creation "openai-org-before-max" "examples/openai-prompt-org-before-max.eld"
  (with-gptel-chat-file
   "examples/prompt-creation-branching.org" openai nil
   (let ((gptel-org-branching-context nil)
         (gptel-org-ignore-elements nil)
         (gptel-prompt-filter-hook nil))
     (goto-char 4097)
     (gptel--create-prompt))))


;;;; Anthropic
(gptel-test-prompt-creation "anthropic-org" "examples/anthropic-prompt-org.eld"
  (with-gptel-chat-file
   "examples/prompt-creation-branching.org" anthropic nil
   (let ((gptel-org-branching-context nil)
         (gptel-org-ignore-elements nil)
         (gptel-prompt-filter-hook nil))
     (gptel--create-prompt (point-max)))))

;;;; Gemini
(gptel-test-prompt-creation "gemini-org" "examples/gemini-prompt-org.eld"
  (with-gptel-chat-file
   "examples/prompt-creation-branching.org" gemini nil
   (let ((gptel-org-branching-context nil)
         (gptel-org-ignore-elements nil)
         (gptel-prompt-filter-hook nil))
     (gptel--create-prompt (point-max)))))

;;;; Ollama
(gptel-test-prompt-creation "ollama-org" "examples/ollama-prompt-org.eld"
  (with-gptel-chat-file
   "examples/prompt-creation-branching.org" ollama nil
   (let ((gptel-org-branching-context nil)
         (gptel-org-ignore-elements nil)
         (gptel-prompt-filter-hook nil))
     (gptel--create-prompt (point-max)))))

;;; Org-mode with region

;;;; OpenAI
(gptel-test-prompt-creation "openai-region-org" "examples/openai-prompt-region-org.eld"
  (with-gptel-chat-file
   "examples/prompt-creation-branching.org" openai nil
   (let ((gptel-org-branching-context nil)
         (gptel-org-ignore-elements nil)
         (gptel-prompt-filter-hook nil))
     (goto-char 1046) (push-mark)
     (goto-char 2383) (activate-mark)
     (gptel--create-prompt))))

;;;; Anthropic
(gptel-test-prompt-creation "anthropic-region-org" "examples/anthropic-prompt-region-org.eld"
  (with-gptel-chat-file
   "examples/prompt-creation-branching.org" anthropic nil
   (let ((gptel-org-branching-context nil)
         (gptel-org-ignore-elements nil)
         (gptel-prompt-filter-hook nil))
     (goto-char 1046) (push-mark)
     (goto-char 2383) (activate-mark)
     (gptel--create-prompt))))

;;;; Gemini
(gptel-test-prompt-creation "gemini-region-org" "examples/gemini-prompt-region-org.eld"
  (with-gptel-chat-file
   "examples/prompt-creation-branching.org" gemini nil
   (let ((gptel-org-branching-context nil)
         (gptel-org-ignore-elements nil)
         (gptel-prompt-filter-hook nil))
     (goto-char 1046) (push-mark)
     (goto-char 2383) (activate-mark)
     (gptel--create-prompt))))

;;;; Ollama
(gptel-test-prompt-creation "ollama-region-org" "examples/ollama-prompt-region-org.eld"
  (with-gptel-chat-file
   "examples/prompt-creation-branching.org" ollama nil
   (let ((gptel-org-branching-context nil)
         (gptel-org-ignore-elements nil)
         (gptel-prompt-filter-hook nil))
     (goto-char 1046) (push-mark)
     (goto-char 2383) (activate-mark)
     (gptel--create-prompt))))

;;; Org-mode with region, set 2

;; Only testing OpenAI because the logic being tested is upstream of backend-stuff.

;;;; OpenAI
(gptel-test-prompt-creation "openai-region-2-org" "examples/openai-prompt-region-2-org.eld"
  (with-gptel-chat-file
   "examples/prompt-creation-branching.org" openai nil
   (let ((gptel-org-branching-context nil)
         (gptel-org-ignore-elements nil)
         (gptel-prompt-filter-hook nil))
     (goto-char 3013) (push-mark)
     (goto-char 3662) (activate-mark)
     (gptel--create-prompt))))

(gptel-test-prompt-creation "openai-region-branching-org" "examples/openai-prompt-region-branching-org.eld"
  (with-gptel-chat-file
   "examples/prompt-creation-branching.org" openai nil
   (let ((gptel-org-branching-context t)
         (gptel-org-ignore-elements nil)
         (gptel-prompt-filter-hook nil))
     (goto-char 1610) (push-mark)
     (goto-char 3716) (activate-mark)
     (gptel--create-prompt))))

;;; Org-mode with gptel-org-ignore-elements
;;;; OpenAI
(gptel-test-prompt-creation "openai-org-noprops" "examples/openai-prompt-noprops-org.eld"
  (with-gptel-chat-file
   "examples/prompt-creation-branching.org" openai nil
   (let ((gptel-org-branching-context nil)
         (gptel-org-ignore-elements '(property-drawer)))
     (gptel--create-prompt (point-max)))))

;;;; Anthropic
(gptel-test-prompt-creation "anthropic-org-noprops" "examples/anthropic-prompt-noprops-org.eld"
  (with-gptel-chat-file
   "examples/prompt-creation-branching.org" anthropic nil
   (let ((gptel-org-branching-context nil)
         (gptel-org-ignore-elements '(property-drawer)))
     (gptel--create-prompt (point-max)))))

;;;; Gemini
(gptel-test-prompt-creation "gemini-org-noprops" "examples/gemini-prompt-noprops-org.eld"
  (with-gptel-chat-file
   "examples/prompt-creation-branching.org" gemini nil
   (let ((gptel-org-branching-context nil)
         (gptel-org-ignore-elements '(property-drawer)))
     (gptel--create-prompt (point-max)))))

;;;; Ollama
(gptel-test-prompt-creation "ollama-org-noprops" "examples/ollama-prompt-noprops-org.eld"
  (with-gptel-chat-file
   "examples/prompt-creation-branching.org" ollama nil
   (let ((gptel-org-branching-context nil)
         (gptel-org-ignore-elements '(property-drawer)))
     (gptel--create-prompt (point-max)))))


;;; TODO(cache) With gptel-cache
;;; Bounds v2 Testing

;; Tests for the new bounds format: ((response (N1 N2) ...) (tool (N3 N4) ...))
;; TODO(persistence) all of the old bounds can be updated by round-tripping the test
;; files through save.  Whenever the new style becomes preferred, let's invert
;; this special cast test.

;;;; OpenAI
(gptel-test-prompt-creation
    "openai-bounds-v2-org" "examples/openai-prompt-bounds-v2-org.eld"
  (with-gptel-chat-file
   "examples/prompt-creation-bounds-v2.org" openai nil
   (let ((gptel-org-branching-context nil)
         (gptel-org-ignore-elements nil)
         (gptel-prompt-filter-hook nil))
     (gptel--create-prompt (point-max)))))

;;; Tool block parsing testing

;;;; OpenAI
(gptel-test-prompt-creation
    "openai-tool-block-org" "examples/openai-prompt-tool-block-org.eld"
  (with-gptel-chat-file
   "examples/prompt-creation-tool-block.org" openai nil
   (let ((gptel-org-branching-context nil)
         (gptel-org-ignore-elements nil)
         (gptel-prompt-filter-hook nil))
     (gptel--create-prompt (point-max)))))

;;;; Gemini
(gptel-test-prompt-creation
    "gemini-tool-block-org" "examples/gemini-prompt-tool-block-org.eld"
  (with-gptel-chat-file
   "examples/prompt-creation-tool-block.org" gemini nil
   (let ((gptel-org-branching-context nil)
         (gptel-org-ignore-elements nil)
         (gptel-prompt-filter-hook nil))
     (gptel--create-prompt (point-max)))))

;;;; Anthropic
(gptel-test-prompt-creation
    "anthropic-tool-block-org" "examples/anthropic-prompt-tool-block-org.eld"
  (with-gptel-chat-file
   "examples/prompt-creation-tool-block.org" anthropic nil
   (let ((gptel-org-branching-context nil)
         (gptel-org-ignore-elements nil)
         (gptel-prompt-filter-hook nil))
     (gptel--create-prompt (point-max)))))

;;;; Ollama
(gptel-test-prompt-creation
    "ollama-tool-block-org" "examples/ollama-prompt-tool-block-org.eld"
  (with-gptel-chat-file
   "examples/prompt-creation-tool-block.org" ollama nil
   (let ((gptel-org-branching-context nil)
         (gptel-org-ignore-elements nil)
         (gptel-prompt-filter-hook nil))
     (gptel--create-prompt (point-max)))))
