;;; gptel.el --- Interact with ChatGPT or other LLMs     -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthik.chikmagalur@gmail.com>
;; Version: 0.9.9.4
;; Package-Requires: ((emacs "27.1") (transient "0.7.4") (compat "30.1.0.0"))
;; Keywords: convenience, tools
;; URL: https://github.com/karthink/gptel

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; gptel is a simple Large Language Model chat client, with support for multiple
;; models and backends.
;;
;; It works in the spirit of Emacs, available at any time and in any buffer.
;;
;; gptel supports:
;;
;; - The services ChatGPT, Azure, Gemini, Anthropic AI, Together.ai, Perplexity,
;;   AI/ML API, Anyscale, OpenRouter, Groq, PrivateGPT, DeepSeek, Cerebras, Github Models,
;;   GitHub Copilot chat, AWS Bedrock, Novita AI, xAI, Sambanova, Mistral Le
;;   Chat and Kagi (FastGPT & Summarizer).
;; - Local models via Ollama, Llama.cpp, Llamafiles or GPT4All
;;
;; Additionally, any LLM service (local or remote) that provides an
;; OpenAI-compatible API is supported.
;;
;; Features:
;;
;; - Interact with LLMs from anywhere in Emacs (any buffer, shell, minibuffer,
;;   wherever).
;; - LLM responses are in Markdown or Org markup.
;; - Supports conversations and multiple independent sessions.
;; - Supports tool-use to equip LLMs with agentic capabilities.
;; - Supports Model Context Protocol (MCP) integration using the mcp.el package.
;; - Supports multi-modal models (send images, documents).
;; - Supports "reasoning" content in LLM responses.
;; - Save chats as regular Markdown/Org/Text files and resume them later.
;; - You can go back and edit your previous prompts or LLM responses when
;;   continuing a conversation.  These will be fed back to the model.
;; - Redirect prompts and responses easily
;; - Rewrite, refactor or fill in regions in buffers.
;; - Write your own commands for custom tasks with a simple API.
;;
;; Requirements for ChatGPT, Azure, Gemini or Kagi:
;;
;; - You need an appropriate API key.  Set the variable `gptel-api-key' to the
;;   key or to a function of no arguments that returns the key.  (It tries to
;;   use `auth-source' by default)
;;
;; ChatGPT is configured out of the box.  For the other sources:
;;
;; - For Azure: define a gptel-backend with `gptel-make-azure'.
;; - For Gemini: define a gptel-backend with `gptel-make-gemini'.
;; - For Anthropic (Claude): define a gptel-backend with `gptel-make-anthropic'.
;; - For AI/ML API, Together.ai, Anyscale, Groq, OpenRouter, DeepSeek, Cerebras
;;   or Github Models: define a gptel-backend with `gptel-make-openai'.
;; - For PrivateGPT: define a backend with `gptel-make-privategpt'.
;; - For Perplexity: define a backend with `gptel-make-perplexity'.
;; - For Deepseek: define a backend with `gptel-make-deepseek'.
;; - For Kagi: define a gptel-backend with `gptel-make-kagi'.
;;
;; For local models using Ollama, Llama.cpp or GPT4All:
;;
;; - The model has to be running on an accessible address (or localhost)
;; - Define a gptel-backend with `gptel-make-ollama' or `gptel-make-gpt4all'.
;; - Llama.cpp or Llamafiles: Define a gptel-backend with `gptel-make-openai'.
;;
;; Consult the package README for examples and more help with configuring
;; backends.
;;
;; Usage:
;;
;; gptel can be used in any buffer or in a dedicated chat buffer.  The
;; interaction model is simple: Type in a query and the response will be
;; inserted below.  You can continue the conversation by typing below the
;; response.
;;
;; To use this in any buffer:
;;
;; - Call `gptel-send' to send the buffer's text up to the cursor.  Select a
;;   region to send only the region.
;;
;; - You can select previous prompts and responses to continue the conversation.
;;
;; - Call `gptel-send' with a prefix argument to access a menu where you can set
;;   your backend, model and other parameters, or to redirect the
;;   prompt/response.
;;
;; To use this in a dedicated buffer:
;; 
;; - M-x gptel: Start a chat session.
;;
;; - In the chat session: Press `C-c RET' (`gptel-send') to send your prompt.
;;   Use a prefix argument (`C-u C-c RET') to access a menu.  In this menu you
;;   can set chat parameters like the system directives, active backend or
;;   model, or choose to redirect the input or output elsewhere (such as to the
;;   kill ring or the echo area).
;;
;; - You can save this buffer to a file.  When opening this file, turn on
;;   `gptel-mode' before editing it to restore the conversation state and
;;   continue chatting.
;;
;; - To include media files with your request, you can add them to the context
;;   (described next), or include them as links in Org or Markdown mode chat
;;   buffers.  Sending media is disabled by default, you can turn it on globally
;;   via `gptel-track-media', or locally in a chat buffer via the header line.
;; 
;; Include more context with requests:
;;
;; If you want to provide the LLM with more context, you can add arbitrary
;; regions, buffers, files or directories to the query with `gptel-add'.  To add
;; text or media files, call `gptel-add' in Dired or use the dedicated
;; `gptel-add-file'.
;;
;; You can also add context from gptel's menu instead (`gptel-send' with a
;; prefix arg), as well as examine or modify context.
;;
;; When context is available, gptel will include it with each LLM query.
;;
;; LLM Tool use:
;;
;; gptel supports "tool calling" behavior, where LLMs can specify arguments with
;; which to call provided "tools" (elisp functions).  The results of running the
;; tools are fed back to the LLM, giving it capabilities and knowledge beyond
;; what is available out of the box.  For example, tools can perform web
;; searches or API lookups, modify files and directories, and so on.
;;
;; Tools can be specified via `gptel-make-tool', or obtained from other
;; repositories, or from Model Context Protocol (MCP) servers using the mcp.el
;; package.  See the README for details.
;;
;; Tools can be included with LLM queries using gptel's menu, or from
;; `gptel-tools'.
;;
;; Rewrite interface
;;
;; In any buffer: with a region selected, you can rewrite prose, refactor code
;; or fill in the region.  This is accessible via `gptel-rewrite', and also from
;; the `gptel-send' menu.
;;
;; Presets
;;
;; Define a bundle of configuration (model, backend, system message, tools etc)
;; as a "preset" that can be applied together, making it easy to switch between
;; tasks in gptel.  Presets can be saved and applied from gptel's transient
;; menu.  You can also include a cookie of the form "@preset-name" in the prompt
;; to send a request with a preset applied.  This feature works everywhere, but
;; preset cookies are also fontified in chat buffers.
;;
;; gptel in Org mode:
;;
;; gptel offers a few extra conveniences in Org mode:
;;
;; - You can limit the conversation context to an Org heading with
;;   `gptel-org-set-topic'.
;;   
;; - You can have branching conversations in Org mode, where each hierarchical
;;   outline path through the document is a separate conversation branch.
;;   See the variable `gptel-org-branching-context'.
;;   
;; - You can declare the gptel model, backend, temperature, system message and
;;   other parameters as Org properties with the command
;;   `gptel-org-set-properties'.  gptel queries under the corresponding heading
;;   will always use these settings, allowing you to create mostly reproducible
;;   LLM chat notebooks.
;;
;; Finally, gptel offers a general purpose API for writing LLM ineractions that
;; suit your workflow.  See `gptel-request', and `gptel-fsm' for more advanced
;; usage.

;;; Code:
(defconst gptel-version "0.9.9.4")

(declare-function markdown-mode "markdown-mode")
(declare-function gptel-menu "gptel-transient")
(declare-function gptel-system-prompt "gptel-transient")
(declare-function gptel-tools "gptel-transient")
(declare-function gptel--vterm-pre-insert "gptel-integrations")
(declare-function pulse-momentary-highlight-region "pulse")

(declare-function ediff-make-cloned-buffer "ediff-util")
(declare-function ediff-regions-internal "ediff")
(declare-function hl-line-highlight "hl-line")

(declare-function org-escape-code-in-string "org-src")
(declare-function gptel-org-set-topic "gptel-org")
(declare-function gptel-org--save-state "gptel-org")
(declare-function gptel-org--restore-state "gptel-org")
(declare-function gptel-org--annotate-links "gptel-org")
(declare-function gptel-org--heading-has-tag-p "gptel-org")
(defvar gptel-org-assistant-tag)
(declare-function org-at-heading-p "org")
(declare-function org-get-tags "org")
(declare-function org-end-of-subtree "org")
(declare-function outline-next-heading "outline")
(define-obsolete-function-alias
  'gptel-set-topic 'gptel-org-set-topic "0.7.5")

(declare-function markdown-link-at-pos "markdown-mode")

(eval-when-compile
  (require 'subr-x))
(require 'cl-lib)
(require 'compat nil t)
(require 'url)
(require 'map)
(require 'text-property-search)
(require 'cl-generic)
(require 'gptel-request)


;;; User options
(defcustom gptel-pre-response-hook nil
  "Hook run before inserting the LLM response into the current buffer.

This hook is called in the buffer where the LLM response will be
inserted.

Note: this hook only runs if the request succeeds."
  :type 'hook
  :group 'gptel)

(define-obsolete-variable-alias
  'gptel-post-response-hook 'gptel-post-response-functions
  "0.6.0"
  "Post-response functions are now called with two arguments: the
start and end buffer positions of the response.")

(defcustom gptel-post-response-functions nil
  "Abnormal hook run after inserting the LLM response into the current buffer.

This hook is called in the buffer to which the LLM response is
sent, and after the full response has been inserted.  Each
function is called with two arguments: the response beginning and
end positions.

Note: this hook runs even if the request fails.  In this case the
response beginning and end positions are both the cursor position
at the time of the request."
  :type 'hook
  :group 'gptel)

(add-hook 'gptel-post-response-functions 'pulse-momentary-highlight-region 70)

(defcustom gptel-post-stream-hook nil
  "Hook run after each insertion of the LLM's streaming response.

This hook is called in the buffer from which the prompt was sent
to the LLM, and after a text insertion."
  :type 'hook
  :group 'gptel)

(defcustom gptel-abort-hook nil
  "Hook run in the gptel buffer after a request is aborted.

This hook is called in the buffer where the request was active,
after all other abort cleanup has completed.  Use this to perform
additional cleanup of UI elements or state."
  :type 'hook
  :group 'gptel)

(defcustom gptel-save-state-hook nil
  "Hook run before gptel saves model parameters to a file.

You can use this hook to store additional conversation state or
model parameters to the chat buffer, or to modify the buffer in
some other way."
  :type 'hook
  :group 'gptel)

(defcustom gptel-default-mode (if (fboundp 'markdown-mode)
				  'markdown-mode
				'text-mode)
  "The default major mode for dedicated chat buffers.

If `markdown-mode' is available, it is used.  Otherwise gptel
defaults to `text-mode'."
  :type 'function
  :group 'gptel)

(defcustom gptel-use-header-line t
  "Whether `gptel-mode' should use header-line for status information.

When set to nil, use the mode line for (minimal) status
information and the echo area for messages."
  :type 'boolean
  :group 'gptel)

;; Set minimally to avoid display-buffer action alist conflicts (#533)
(defcustom gptel-display-buffer-action `(nil (body-function . ,#'select-window))
  "The action used to display gptel chat buffers.

The gptel buffer is displayed in a window using

  (display-buffer BUFFER gptel-display-buffer-action)

The value of this option has the form (FUNCTION . ALIST),
where FUNCTION is a function or a list of functions.  Each such
function should accept two arguments: a buffer to display and an
alist of the same form as ALIST.  See info node `(elisp)Choosing
Window' for details."
  :type display-buffer--action-custom-type
  :group 'gptel)

(defcustom gptel-crowdsourced-prompts-file
  (let ((cache-dir (or (eval-when-compile
			 (require 'xdg)
			 (xdg-cache-home))
                       user-emacs-directory)))
    (expand-file-name "gptel-crowdsourced-prompts.csv" cache-dir))
  "File used to store crowdsourced system prompts.

These are prompts cached from an online source (see
`gptel--crowdsourced-prompts-url'), and can be set from the
transient menu interface provided by `gptel-menu'."
  :type 'file
  :group 'gptel)

(defvar gptel-refresh-buffer-hook '(jit-lock-refontify)
  "Hook run in gptel buffers after changing gptel's configuration.

This hook runs in gptel chat buffers after making a change to gptel's
configuration that might require a UI update.")

(defvar-local gptel--bounds nil)
(put 'gptel--bounds 'safe-local-variable #'always)

(defvar gptel--preset nil
  "Name of last applied gptel preset.

For internal use only.")
(put 'gptel--preset 'safe-local-variable #'symbolp)

(defvar-local gptel--tool-names nil
  "Store to persist tool names to file across Emacs sessions.

Note: Changing this variable does not affect gptel\\='s behavior
in any way.")
(put 'gptel--tool-names 'safe-local-variable #'always)

(defvar-local gptel--backend-name nil
  "Store to persist backend name across Emacs sessions.

Note: Changing this variable does not affect gptel\\='s behavior
in any way.")
(put 'gptel--backend-name 'safe-local-variable #'always)

(defvar-local gptel--old-header-line nil)

(defvar gptel--markdown-block-map
  (define-keymap
    "<tab>" 'gptel-markdown-cycle-block
    "TAB"   'gptel-markdown-cycle-block)
  "Keymap for folding and unfolding Markdown code blocks.")


;;; Utility functions
(defun gptel--modify-value (original new-spec)
  "Combine ORIGINAL with NEW-SPEC and return the new result.

This function is non-destructive, ORIGINAL is not modified.

NEW-SPEC is either a declarative action spec (plist) of the form
 (:key val ...), or a simple value.  Recognized spec keys are :append,
:prepend, :eval, :function and :merge.  If NEW-SPEC does not have this
form it is returned as is.

- :append and :prepend will append/prepend val (a list or string) to ORIGINAL.
  Actions on strings are idempotent, they will only be appended/prepended once.
- :eval will evaluate val and return the result, and
- :function will call val with ORIGINAL as its argument, and return the result.
- :merge will treat ORIGINAL and NEW-SPEC as plists and return a merged plist,
  with NEW-SPEC taking precedence."
  (if (not (and (consp new-spec) (keywordp (car new-spec))))
      new-spec
    (let ((current original) (tail new-spec))
      (while tail
        (let ((key (pop tail)) (form (pop tail)))
          (setq current
                (pcase key
                  (:append (if (stringp form)
                               (if (string-suffix-p form current t)
                                   current (concat current form))
                             (append current form)))
                  (:prepend (if (stringp form)
                                (if (string-prefix-p form current t)
                                    current (concat form current))
                              (append form current)))
                  (:eval (eval form t))
                  (:function (funcall form current))
                  (:merge (gptel--merge-plists (copy-sequence current) form))
                  (_ new-spec)))))
      current)))

(defun gptel-auto-scroll ()
  "Scroll window if LLM response continues below viewport.

Note: This will move the cursor."
  (when-let* ((win (get-buffer-window (current-buffer) 'visible))
              ((not (pos-visible-in-window-p (point) win)))
              (scroll-error-top-bottom t))
    (condition-case nil
        (with-selected-window win
          (scroll-up-command))
      (error nil))))

(defun gptel-beginning-of-response (&optional beg _end arg)
  "Move point to BEG, or to the beginning of the LLM response ARG times."
  (interactive (list nil nil
                     (prefix-numeric-value current-prefix-arg)))
  (gptel-end-of-response beg nil (- (or arg 1))))

(defun gptel-end-of-response (&optional beg end arg)
  "Move point to end of LLM response.

With BEG, start search from BEG when ARG is negative.
With END, start search from END when ARG is positive.
Otherwise move ARG times, defaulting to 1."
  (interactive (list nil nil
                     (prefix-numeric-value current-prefix-arg)))
  (unless arg (setq arg 1))
  (let* ((search (if (> arg 0)
                     #'text-property-search-forward
                   #'text-property-search-backward))
         (goto-prefix-end
          (lambda () (when-let* ((prefix (gptel-prompt-prefix-string))
                            ((not (string-empty-p prefix)))
                            ((looking-at (concat "\n\\{1,2\\}"
                                                 (regexp-quote prefix) "?"))))
                  (goto-char (match-end 0)))))
         (goto-prefix-beg
          (lambda () (when-let* ((prefix (gptel-response-prefix-string))
                            ((not (string-empty-p prefix)))
                            ((looking-back (concat (regexp-quote prefix) "?")
                                           (point-min))))
                  (goto-char (match-beginning 0))))))
    (cond
     ((and end (> arg 0)) (goto-char end) (cl-decf arg) (funcall goto-prefix-end))
     ((and beg (< arg 0)) (goto-char beg) (cl-incf arg) (funcall goto-prefix-beg)))
    (dotimes (_ (abs arg))
      (funcall search 'gptel 'response t)
      (if (> arg 0)
          (funcall goto-prefix-end)
        (funcall goto-prefix-beg)))))

(defun gptel-markdown-cycle-block ()
  "Cycle code blocks in Markdown."
  (interactive)
  (save-excursion
    (forward-line 0)
    (let (start end (parity 0))
      (cond            ;Find start and end of block, with possible nested blocks
       ((looking-at-p "^``` *\n")       ;end of block, find corresponding start
        (setq parity -1 end (line-end-position))
        (while (and (not (= parity 0)) (not (bobp)) (forward-line -1))
          (cond ((looking-at-p "^``` *\n") (cl-decf parity))
                ((looking-at-p "^``` ?[a-z]") (cl-incf parity))))
        (when (= parity 0) (setq start (point))))

       ((looking-at-p "^``` ?[a-z]") ;beginning of block, find corresponding end
        (setq parity 1 start (point))
        (while (and (not (= parity 0)) (not (eobp)) (forward-line 1))
          (cond ((looking-at-p "^``` *\n") (cl-decf parity))
                ((looking-at-p "^``` ?[a-z]") (cl-incf parity))))
        (when (= parity 0) (setq end (line-end-position)))))
      (when (and start end)
        (goto-char start)
        (end-of-line)
        (pcase-let* ((`(,value . ,hide-ov)
                      (get-char-property-and-overlay (point) 'invisible)))
          (if (and hide-ov (eq value t))
              (delete-overlay hide-ov)
            (unless hide-ov (setq hide-ov (make-overlay (point) end)))
            (overlay-put hide-ov 'evaporate t)
            (overlay-put hide-ov 'invisible t)
            (overlay-put hide-ov 'before-string
                         (propertize "..." 'face 'shadow))))))))

(defsubst gptel--annotate-link (ov link-status)
  "Annotate link overlay OV according to LINK-STATUS.

LINK-STATUS is a list of link properties relevant to gptel queries, of
the form (valid . REST).  See `gptel-markdown--validate-link' for
details.  Indicate the (in)validity of the link for inclusion with gptel
queries via OV."
  (cl-destructuring-bind
      (valid _ path resource-type user-check readablep mime-valid _mime)
      link-status
    (if valid
        (progn
          (overlay-put
           ov 'before-string
           (concat (propertize "SEND" 'face '(:inherit success :height 0.8))
                   (if (display-graphic-p)
                       (propertize " " 'display '(space :width 0.5)) " ")))
          (overlay-put ov 'help-echo
                       (format "Sending %s %s with gptel requests" resource-type path)))
      (overlay-put ov 'before-string
                   (concat (propertize "!" 'face '(:inherit error))
                           (propertize " " 'display '(space :width 0.3))))
      (overlay-put
       ov 'help-echo
       (concat
        "Sending only link text with gptel requests, "
        "this link will not be followed to its source.\n\nReason: "
        (cond
         ((not resource-type) "Not a supported link type\
 (Only \"file\" and \"attachment\" are supported)")
         ((not user-check)
          (concat
           "\nNot a standalone link -- separate link from text around it. \n           (OR)
Link failed to validate, see `gptel-markdown-validate-link' or `gptel-org-validate-link'."))
         ((not readablep) (format "File %s is not readable" path))
         ((not mime-valid)
          (pcase resource-type
            ('file (format "%s does not support binary file %s" gptel-model path))
            ('url (format "%s does not support fetching non-image URLs" gptel-model))))))))))

(defun gptel--annotate-link-clear (&optional beg end)
  "Delete all gptel org link annotations between BEG and END."
  (mapc #'delete-overlay
        (cl-delete-if-not
         (lambda (o) (overlay-get o 'gptel-track-media))
         (overlays-in (or beg (point-min)) (or end (point-max))))))

;;;; Response text recognition

(defun gptel--get-buffer-bounds ()
  "Return the gptel response boundaries in the buffer as an alist."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (let ((bounds) (prev-pt (point)))
        (while (and (/= prev-pt (point-min))
                    (goto-char (previous-single-property-change
                                (point) 'gptel nil (point-min))))
          (when-let* ((prop (get-char-property (point) 'gptel)))
            (let* ((prop-name (if (symbolp prop) prop (car prop)))
                   (val (when (consp prop) (cdr prop)))
                   (bound (if val
                              (list (point) prev-pt val)
                            (list (point) prev-pt))))
              (push bound (alist-get prop-name bounds))))
          (setq prev-pt (point)))
        bounds))))

(define-obsolete-function-alias
  'gptel--get-bounds 'gptel--get-response-bounds "0.9.8")

(defun gptel--get-response-bounds ()
  "Return the gptel response boundaries around point."
  (let (prop)
    (save-excursion
      (when (text-property-search-forward
                          'gptel 'response t)
        (when (setq prop (text-property-search-backward
                          'gptel 'response t))
          (cons (prop-match-beginning prop)
                (prop-match-end prop)))))))

(defun gptel--in-response-p (&optional pt)
  "Check if position PT is inside a gptel response."
  (eq (get-char-property (or pt (point)) 'gptel) 'response))

(defun gptel--at-response-history-p (&optional pt)
  "Check if gptel response at position PT has variants."
  (get-char-property (or pt (point)) 'gptel-history))

(defvar gptel--mode-description-alist
  '((js2-mode      . "Javascript")
    (sh-mode       . "Shell")
    (enh-ruby-mode . "Ruby")
    (yaml-mode     . "Yaml")
    (yaml-ts-mode  . "Yaml")
    (rustic-mode   . "Rust")
    (tuareg-mode   . "OCaml"))
  "Mapping from unconventionally named major modes to languages.

This is used when generating system prompts for rewriting and
when including context from these major modes.")


;;; Saving and restoring state

(defun gptel--restore-props (bounds-alist)
  "Restore text properties from BOUNDS-ALIST.
BOUNDS-ALIST is (PROP . BOUNDS).  BOUNDS is a list of BOUND.  Each BOUND
is either (BEG END VAL) or (BEG END).

For (BEG END VAL) forms, even if VAL is nil, the gptel property will be
set to (PROP . VAL).  For (BEG END) forms, except when PROP is response,
the gptel property is set to just PROP.

The legacy structure, a list of (BEG . END) is also supported and will be
applied before being re-persisted in the new structure."
  ;; Run silently to avoid `gptel--inherit-stickiness' and other hooks that
  ;; might modify the gptel text property.
  (with-silent-modifications
    (if (symbolp (caar bounds-alist))
        (mapc
         (lambda (bounds)
           (let* ((prop (pop bounds)))
             (mapc
              (lambda (bound)
                (let ((prop-has-val (> (length bound) 2)))
                  (add-text-properties
                   (pop bound) (pop bound)
                   (if (eq prop 'response)
                       '(gptel response front-sticky (gptel))
                     (list 'gptel
                           (if prop-has-val
                               (cons prop (pop bound))
                             prop))))))
              bounds)))
         bounds-alist)
      (mapc (lambda (bound)
              (add-text-properties
               (car bound) (cdr bound) '(gptel response front-sticky (gptel))))
            bounds-alist))))

(defun gptel--restore-state ()
  "Restore gptel state when turning on `gptel-mode'."
  (when (buffer-file-name)
    (if (derived-mode-p 'org-mode)
        (progn
          (require 'gptel-org)
          (gptel-org--restore-state))
      (when gptel--bounds
        (gptel--restore-props gptel--bounds)
        (message "gptel chat restored."))
      (when gptel--preset
        (if (gptel-get-preset gptel--preset)
            (gptel--apply-preset
             gptel--preset (lambda (sym val) (set (make-local-variable sym) val)))
          (display-warning
           '(gptel presets)
           (format "Could not activate gptel preset `%s' in buffer \"%s\""
                   gptel--preset (buffer-name)))))
      (when gptel--backend-name
        (if-let* ((backend (alist-get
                            gptel--backend-name gptel--known-backends
                            nil nil #'equal)))
            (setq-local gptel-backend backend)
          (message
           (substitute-command-keys
            (concat
             "Could not activate gptel backend \"%s\"!  "
             "Switch backends with \\[universal-argument] \\[gptel-send]"
             " before using gptel."))
           gptel--backend-name)))
      (when gptel--tool-names
        (if-let* ((tools (cl-loop
                          for tname in gptel--tool-names
                          for tool = (with-demoted-errors "gptel: %S"
                                       (gptel-get-tool tname))
                          if tool collect tool else do
                          (display-warning
                           '(gptel org tools)
                           (format "Tool %s not found, ignoring" tname)))))
            (setq-local gptel-tools tools))))))

(defun gptel--save-state ()
  "Write the gptel state to the buffer.

This saves chat metadata when writing the buffer to disk.  To
restore a chat session, turn on `gptel-mode' after opening the
file.

If a gptel preset has been applied in this buffer, a reference to it is
saved.

Additional metadata is stored only if no preset was applied or if it
differs from the preset specification.  This is limited to the active
gptel model and backend names, the system message, active tools, the
response temperature, max tokens and number of conversation turns to
send in queries.  (See `gptel--num-messages-to-send' for the last one.)"
  (run-hooks 'gptel-save-state-hook)
  (if (derived-mode-p 'org-mode)
      (progn
        (require 'gptel-org)
        (gptel-org--save-state))
    (let ((print-escape-newlines t)
          (preset-spec (and gptel--preset
                            (gptel-get-preset gptel--preset))))
      (save-excursion
        (save-restriction

          (if preset-spec
              (add-file-local-variable 'gptel--preset gptel--preset)
            (delete-file-local-variable 'gptel--preset))

          ;; Model and backend
          (if (gptel--preset-mismatch-value preset-spec :model gptel-model)
              (add-file-local-variable 'gptel-model gptel-model))
          (if (gptel--preset-mismatch-value preset-spec :backend gptel-backend)
              (add-file-local-variable 'gptel--backend-name
                                       (gptel-backend-name gptel-backend)))
          ;; System message
          (let ((parsed (car-safe (gptel--parse-directive gptel--system-message))))
            (if (gptel--preset-mismatch-value preset-spec :system parsed)
                (add-file-local-variable 'gptel--system-message parsed)
              (delete-file-local-variable 'gptel--system-message)))
          ;; Tools
          (let ((tool-names (mapcar #'gptel-tool-name gptel-tools)))
            (if (gptel--preset-mismatch-value preset-spec :tools tool-names)
                (add-file-local-variable 'gptel--tool-names tool-names)
              (delete-file-local-variable 'gptel--tool-names)))
          ;; Temperature, max tokens and cutoff
          (if (and (gptel--preset-mismatch-value preset-spec :temperature gptel-temperature)
                   (not (equal (default-value 'gptel-temperature) gptel-temperature)))
              (add-file-local-variable 'gptel-temperature gptel-temperature)
            (delete-file-local-variable 'gptel-temperature))
          (if (and (gptel--preset-mismatch-value preset-spec :max-tokens gptel-max-tokens)
                   gptel-max-tokens)
              (add-file-local-variable 'gptel-max-tokens gptel-max-tokens)
            (delete-file-local-variable 'gptel-max-tokens))
          (if (and (gptel--preset-mismatch-value
                    preset-spec :num-messages-to-send gptel--num-messages-to-send)
                   (natnump gptel--num-messages-to-send))
              (add-file-local-variable 'gptel--num-messages-to-send
                                       gptel--num-messages-to-send)
            (delete-file-local-variable 'gptel--num-messages-to-send))
          (add-file-local-variable 'gptel--bounds (gptel--get-buffer-bounds)))))))


;;; Minor modes and UI

;; NOTE: It's not clear that this is the best strategy:
(cl-pushnew '(gptel . t) (default-value 'text-property-default-nonsticky)
            :test #'equal)

(defun gptel--inherit-stickiness (beg end _pre)
  "Mark any change to an LLM response region as a response.

Intended to be added to `after-change-functions' in gptel chat buffers,
which see for BEG, END and PRE."
  (and (/= beg end) (< end (point-max))
       (and-let* ((val (get-text-property end 'gptel)))
         (add-text-properties
          beg end `(gptel ,val front-sticky (gptel))))))

(defun gptel-markdown--annotate-links (beg end)
  "Annotate Markdown links whose sources will be sent with `gptel-send'.

Search between BEG and END."
  (when gptel-track-media
    (save-excursion
      (goto-char beg) (forward-line -1)
      (let ((link-ovs (cl-loop for o in (overlays-in (point) end)
                               if (overlay-get o 'gptel-track-media)
                               collect o into os finally return os)))
        (while (re-search-forward gptel-markdown--link-regex end t)
          (unless (gptel--in-response-p (1- (point)))
            (let* ((link (markdown-link-at-pos (point)))
                   (from (car link)) (to (cadr link))
                   (link-status (gptel-markdown--validate-link link))
                   (ov (cl-loop for o in (overlays-in from to)
                                if (overlay-get o 'gptel-track-media)
                                return o)))
              (if ov                    ; Ensure overlay over each link
                  (progn (move-overlay ov from to)
                         (setq link-ovs (delq ov link-ovs)))
                (setq ov (make-overlay from to nil t))
                (overlay-put ov 'gptel-track-media t)
                (overlay-put ov 'evaporate t)
                (overlay-put ov 'priority -80))
              ;; Check if link will be sent, and annotate accordingly
              (gptel--annotate-link ov link-status))))
        (and link-ovs (mapc #'delete-overlay link-ovs))))
    `(jit-lock-bounds ,beg . ,end)))

(defvar gptel--header-line-info
  '(:eval
    (let* ((model (gptel--model-name gptel-model))
           (system
            (propertize
             (buttonize
              (format "[Prompt: %s]"
                      (or (car-safe (rassoc gptel--system-message gptel-directives))
                          (gptel--describe-directive gptel--system-message 15)))
              (lambda (&rest _) (gptel-system-prompt)))
             'mouse-face 'highlight
             'help-echo "System message for session"))
           (context
            (and gptel-context
                 (cl-loop
                  for entry in gptel-context
                  if (bufferp (or (car-safe entry) entry)) count it into bufs
                  else count (stringp (or (car-safe entry) entry)) into files
                  finally return
                  (propertize
                   (buttonize
                    (concat "[Context: "
                            (and (> bufs 0) (format "%d buf" bufs))
                            (and (> bufs 1) "s")
                            (and (> bufs 0) (> files 0) ", ")
                            (and (> files 0) (format "%d file" files))
                            (and (> files 1) "s")
                            "]")
                    (lambda (&rest _)
                      (require 'gptel-context)
                      (gptel-context--buffer-setup)))
                   'mouse-face 'highlight
                   'help-echo "Active gptel context"))))
           (toggle-track-media
            (lambda (&rest _)
              (setq-local gptel-track-media (not gptel-track-media))
              (if gptel-track-media
                  (progn
                    (run-hooks 'gptel-refresh-buffer-hook)
                    (message "Sending media from included links."))
                (without-restriction (gptel--annotate-link-clear))
                (message "Ignoring links.  Only link text will be sent."))
              (run-at-time 0 nil #'force-mode-line-update)))
           (track-media
            (and (gptel--model-capable-p 'media)
                 (if gptel-track-media
                     (propertize
                      (buttonize "[Sending media]" toggle-track-media)
                      'mouse-face 'highlight
                      'help-echo
                      "Sending media from links/urls when supported.\nClick to toggle")
                   (propertize
                    (buttonize "[Ignoring media]" toggle-track-media)
                    'mouse-face 'highlight
                    'help-echo
                    "Ignoring media from links/urls.\nClick to toggle"))))
           (toggle-tools (lambda (&rest _) (interactive)
                           (run-at-time 0 nil
                                        (lambda () (call-interactively #'gptel-tools)))))
           (tools (when (and gptel-use-tools gptel-tools)
                    (propertize
                     (buttonize (pcase (length gptel-tools)
                                  (0 "[No tools]") (1 "[1 tool]")
                                  (len (format "[%d tools]" len)))
                                toggle-tools)
                     'mouse-face 'highlight
                     'help-echo "Select tools"))))
      (concat
       (propertize
        " " 'display
        `(space :align-to (- right
                             ,(+ 5 (length model) (length system)
                                 (length track-media) (length context) (length tools)))))
       tools (and track-media " ") track-media (and context " ") context " " system " "
       (propertize
        (buttonize (concat "[" model "]")
                   (lambda (&rest _) (gptel-menu)))
        'mouse-face 'highlight
        'help-echo "Model in use"))))
  "Information segment for the header-line in gptel-mode.")

(defun gptel-use-header-line ()
  "Set up the header-line for a gptel buffer.

It is composed of three segments: the backend name, the
status (Ready/Waiting etc) and the info segment, showing the current
context, tools, system prompt, model and more."
  (setq
   header-line-format
   (list '(:eval (concat (propertize " " 'display '(space :align-to 0))
                         (format "%s" (gptel-backend-name gptel-backend))))
         (propertize " Ready" 'face 'success)
         gptel--header-line-info)))

;;;###autoload
(define-minor-mode gptel-mode
  "Minor mode for interacting with LLMs."
  :lighter " GPT"
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c RET") #'gptel-send)
    map)
  (if gptel-mode
      (progn
        (unless (derived-mode-p 'org-mode 'markdown-mode 'text-mode)
          (gptel-mode -1)
          (user-error (format "`gptel-mode' is not supported in `%s'." major-mode)))
        (add-hook 'before-save-hook #'gptel--save-state nil t)
        (add-hook 'after-change-functions 'gptel--inherit-stickiness nil t)
        (gptel--prettify-preset)
        (cond
         ((derived-mode-p 'org-mode)
          (require 'gptel-org)
          (jit-lock-register 'gptel-org--annotate-links)
          ;; Work around bug in `org-fontify-extend-region'.
          (add-hook 'gptel-post-response-functions #'font-lock-flush nil t))
         ((derived-mode-p 'markdown-mode)
          (font-lock-add-keywords ;keymap is a font-lock-managed property in markdown-mode
           nil '(("^```[ \t]*\\([[:alpha:]][^\n]*\\)?$" ;match code fences
                  0 (list 'face nil 'keymap gptel--markdown-block-map))))
          (jit-lock-register 'gptel-markdown--annotate-links)))
        (gptel--restore-state)
        (if gptel-use-header-line
            (progn (setq gptel--old-header-line header-line-format)
                   (gptel-use-header-line))
          (setq mode-line-process
                '(:eval (concat " " (buttonize (gptel--model-name gptel-model)
                                               (lambda (&rest _) (gptel-menu))))))))
    (remove-hook 'before-save-hook #'gptel--save-state t)
    (remove-hook 'after-change-functions 'gptel--inherit-stickiness t)
    (cond
     ((derived-mode-p 'org-mode)
      (jit-lock-unregister #'gptel-org--annotate-links)
      (without-restriction (gptel--annotate-link-clear)))
     ((derived-mode-p 'markdown-mode)
      (jit-lock-unregister #'gptel-markdown--annotate-links)
      (without-restriction (gptel--annotate-link-clear))))
    (gptel--prettify-preset)
    (if gptel-use-header-line
        (setq header-line-format gptel--old-header-line
              gptel--old-header-line nil)
      (setq mode-line-process nil))))

;; ;TODO(request-lib): Declaration no longer needed
(defvar gptel--fsm-last)                ;Defined further below
(defun gptel--update-status (&optional msg face)
  "Update status MSG in FACE."
  (when gptel-mode
    (if gptel-use-header-line
        (and (consp header-line-format)
             (setf (nth 1 header-line-format)
                   (thread-first
                     msg
                     (buttonize (lambda (_) (gptel--inspect-fsm)))
                     (propertize 'face face 'mouse-face 'highlight))))
      (if (member msg '(" Typing..." " Waiting..."))
          (setq mode-line-process (propertize msg 'face face))
        (setq mode-line-process
              '(:eval (concat " "
                       (buttonize (gptel--model-name gptel-model)
                            (lambda (&rest _) (gptel-menu))))))
        (message (propertize msg 'face face))))
    (force-mode-line-update)))


;;;; gptel-highlight-mode

(defcustom gptel-highlight-methods '(margin)
  "Types of LLM response highlighting used by `gptel-highlight-mode'.

This must be a list of symbols denoting types of highlighting for LLM responses:
- face: highlight LLM responses using face `gptel-response-highlight'.
- fringe: highlight using a (left) fringe marker.
- margin: highlight in the (left) display margin.

margin and fringe markings are mutually exclusive, and use the
`gptel-response-fringe-highlight' face."
  :type '(set (const :tag "Fringe marker" fringe)
              (const :tag "Face highlighting" face)
              (const :tag "Margin indicator" margin))
  :group 'gptel)

(defface gptel-response-highlight
  '((((background light) (min-colors 88)) :background "linen" :extend t)
    (((background dark)  (min-colors 88)) :background "gray14" :extend t)
    (t :inherit mode-line))
  "Face used to highlight LLM responses when using `gptel-highlight-mode'.

To enable this face for responses, `gptel-highlight-methods' must be set."
  :group 'gptel)

(defface gptel-response-fringe-highlight
  ;; NOTE: Remove conditional after we drop Emacs 28.1 (#1254)
  (if (< emacs-major-version 29)
      '((t :inherit outline-1 :height 1.0))
    '((t :inherit outline-1 :height reset)))
  "LLM response fringe/margin face when using `gptel-highlight-mode'.

To enable response highlights in the fringe, `gptel-highlight-methods'
must be set."
  :group 'gptel)

(define-fringe-bitmap 'gptel-highlight-fringe
  (make-vector 28 #b01100000)
  nil nil 'center)

;; Common options for margin indicator:
;; BOX DRAWINGS LIGHT VERTICAL  0x002502
;; LEFT ONE QUARTER BLOCK       0x00258E
;; LEFT THREE EIGHTHS BLOCK     0x00258D
;; BOX DRAWINGS HEAVY VERTICAL  0x002503
;; VERTICAL ONE EIGHTH BLOCK-2  0x01FB70

(defun gptel-highlight--margin-prefix (type)
  "Create margin prefix string for TYPE.

Supported TYPEs are response, ignore and tool calls."
  (propertize ">" 'display
              `( (margin left-margin)
                 ,(propertize "▎" 'face
                              (pcase type
                                ('response 'gptel-response-fringe-highlight)
                                ('ignore 'shadow)
                                (`(tool . ,_) 'shadow))))))

(defun gptel-highlight--fringe-prefix (type)
  "Create fringe prefix string for TYPE.

Supported TYPEs are response, ignore and tool calls."
  (propertize ">" 'display
              `( left-fringe gptel-highlight-fringe
                 ,(pcase type
                    ('response 'gptel-response-fringe-highlight)
                    ('ignore 'shadow)
                    (`(tool . ,_) 'shadow)))))

(defun gptel-highlight--decorate (ov &optional val)
  "Decorate gptel indicator overlay OV whose type is VAL."
  (overlay-put ov 'evaporate t)
  (overlay-put ov 'gptel-highlight t)
  (when (memq 'face gptel-highlight-methods)
    (overlay-put ov 'font-lock-face
                 (pcase val
                   ('response 'gptel-response-highlight)
                   ('ignore 'shadow)
                   (`(tool . ,_) 'shadow))))
  ;; Skip line-prefix in org-mode: it conflicts with org-indent-mode's
  ;; line-prefix text properties, causing assistant text to lose indentation.
  (unless (derived-mode-p 'org-mode)
    (when-let* ((prefix
                 (cond ((memq 'margin gptel-highlight-methods)
                        (gptel-highlight--margin-prefix (or val 'response)))
                       ((memq 'fringe gptel-highlight-methods)
                        (gptel-highlight--fringe-prefix (or val 'response))))))
      (overlay-put ov 'line-prefix prefix)
      (overlay-put ov 'wrap-prefix prefix))))

(defun gptel-highlight--update (beg end)
  "JIT-lock function: mark gptel response/reasoning regions.

BEG and END delimit the region to refresh."
  (save-excursion                ;Scan across region for the gptel text property
    (let ((prev-pt (goto-char end)))
      (while (and (goto-char (previous-single-property-change
                              (point) 'gptel nil beg))
                  (/= (point) prev-pt))
        (pcase (get-char-property (point) 'gptel)
          ((and (or 'response 'ignore `(tool . ,_)) val)
           (if-let* ((ov (or (cdr-safe (get-char-property-and-overlay
                                        (point) 'gptel-highlight))
                             (cdr-safe (get-char-property-and-overlay
                                        prev-pt 'gptel-highlight))))
                     (from (overlay-start ov)) (to (overlay-end ov)))
               (unless (<= from (point) prev-pt to)
                 (move-overlay ov (min from (point)) (max to prev-pt)))
             (gptel-highlight--decorate ;Or make new overlay covering just region
              (make-overlay (point) prev-pt nil t) val)))
          ('nil                     ;If there's an overlay, we need to split it.
           (when-let* ((ov (cdr-safe (get-char-property-and-overlay
                                      (point) 'gptel-highlight)))
                       (from (overlay-start ov)) (to (overlay-end ov)))
             (move-overlay ov from (point)) ;Move overlay to left side
             (gptel-highlight--decorate     ;Make a new one on the right
              (make-overlay prev-pt to nil t)
              (get-char-property prev-pt 'gptel)))))
        (setq prev-pt (point)))))
  `(jit-lock-bounds ,beg . ,end))

;; Org-mode specific highlighting based on heading tags
;; Uses `gptel-org--heading-has-tag-p' and `gptel-org-assistant-tag' from gptel-org.el

(defun gptel-highlight--org-update-overlays ()
  "Update highlight overlays based on Org heading tags.

Scans all headings for the assistant tag and creates/updates
overlays spanning the entire subtree.  Uses `gptel-org-assistant-tag'
for consistency with gptel-org."
  (when (derived-mode-p 'org-mode)
    (require 'gptel-org)
    ;; Remove existing org-based highlight overlays
    (remove-overlays (point-min) (point-max) 'gptel-highlight-org t)
    ;; Scan for tagged headings and create overlays
    (save-excursion
      (goto-char (point-min))
      (while (outline-next-heading)
        (when (gptel-org--heading-has-tag-p gptel-org-assistant-tag)
          (let* ((beg (point))
                 (end (save-excursion
                        (org-end-of-subtree t t)
                        (point)))
                 (ov (make-overlay beg end nil t)))
            (overlay-put ov 'evaporate t)
            (overlay-put ov 'gptel-highlight t)
            (overlay-put ov 'gptel-highlight-org t)
            (gptel-highlight--decorate ov 'response)))))))

(defun gptel-highlight--org-after-tags-change (&rest _)
  "Hook function to update highlights after Org tag changes."
  (when (bound-and-true-p gptel-highlight-mode)
    (gptel-highlight--org-update-overlays)))

(defvar-local gptel-highlight--org-change-idle-timer nil
  "Idle timer for debouncing highlight updates on buffer changes.")

(defun gptel-highlight--org-after-change (beg _end _len)
  "Handle buffer changes that might affect tags.
BEG is the start of the changed region."
  (when (bound-and-true-p gptel-highlight-mode)
    ;; Only update if change is on a heading line (where tags are)
    (save-excursion
      (goto-char beg)
      (when (org-at-heading-p)
        ;; Debounce updates using an idle timer
        (when gptel-highlight--org-change-idle-timer
          (cancel-timer gptel-highlight--org-change-idle-timer))
        (setq gptel-highlight--org-change-idle-timer
              (run-with-idle-timer
               0.2 nil
               (lambda (buf)
                 (when (buffer-live-p buf)
                   (with-current-buffer buf
                     (setq gptel-highlight--org-change-idle-timer nil)
                     (gptel-highlight--org-update-overlays))))
               (current-buffer)))))))

(defun gptel-highlight--org-setup-hooks ()
  "Install Org hooks for dynamic highlight updates."
  ;; Hook into tag changes (via org-set-tags-command)
  (add-hook 'org-after-tags-change-hook
            #'gptel-highlight--org-after-tags-change nil t)
  ;; Hook into visibility changes (folding/unfolding)
  (add-hook 'org-cycle-hook
            #'gptel-highlight--org-after-tags-change nil t)
  ;; Hook into buffer changes for manual tag edits
  (add-hook 'after-change-functions
            #'gptel-highlight--org-after-change nil t))

(defun gptel-highlight--org-remove-hooks ()
  "Remove Org hooks for highlight updates."
  (remove-hook 'org-after-tags-change-hook
               #'gptel-highlight--org-after-tags-change t)
  (remove-hook 'org-cycle-hook
               #'gptel-highlight--org-after-tags-change t)
  (remove-hook 'after-change-functions
               #'gptel-highlight--org-after-change t)
  (when gptel-highlight--org-change-idle-timer
    (cancel-timer gptel-highlight--org-change-idle-timer)
    (setq gptel-highlight--org-change-idle-timer nil)))

(define-minor-mode gptel-highlight-mode
  "Visually highlight LLM respones regions.

Highlighting is via fringe or margin markers, and optionally a response
face.  See `gptel-highlight-methods' for highlighting methods, and
`gptel-response-highlight' and `gptel-response-fringe-highlight' for the
faces.

This minor mode can be used anywhere in Emacs, and not just gptel chat
buffers.

In Org mode buffers, highlighting is based on heading tags rather
than text properties.  This integrates better with Org's structure
and folding.  The tag used is configured by `gptel-org-assistant-tag'
\(default \"assistant\")."
  :lighter nil
  :global nil
  (cond
   (gptel-highlight-mode
    (when (memq 'margin gptel-highlight-methods)
      (setq left-margin-width (1+ left-margin-width))
      (if-let* ((win (get-buffer-window (current-buffer))))
          (set-window-buffer win (current-buffer))))
    (if (derived-mode-p 'org-mode)
        ;; Org mode: use tag-based highlighting
        (progn
          (gptel-highlight--org-setup-hooks)
          (gptel-highlight--org-update-overlays))
      ;; Non-Org: use JIT-lock with text properties
      (jit-lock-register #'gptel-highlight--update)
      (gptel-highlight--update (point-min) (point-max))))
   (t (when (memq 'margin gptel-highlight-methods)
        (setq left-margin-width (max (1- left-margin-width) 0))
        (if-let* ((win (get-buffer-window (current-buffer))))
            (set-window-buffer win (current-buffer))))
      (if (derived-mode-p 'org-mode)
          ;; Org mode: remove hooks and overlays
          (progn
            (gptel-highlight--org-remove-hooks)
            (remove-overlays nil nil 'gptel-highlight-org t))
        ;; Non-Org: unregister JIT-lock
        (jit-lock-unregister #'gptel-highlight--update))
      (without-restriction
        (remove-overlays nil nil 'gptel-highlight t)))))


;;; State machine additions for `gptel-send'.

(defvar gptel-send--handlers
  `((WAIT ,#'gptel--handle-wait ,#'gptel--update-wait)
    (TYPE ,#'gptel--handle-pre-insert)
    (ERRS ,#'gptel--handle-error ,#'gptel--fsm-last)
    (TOOL ,#'gptel--update-tool-call ,#'gptel--handle-tool-use ,#'gptel--update-tool-ask)
    (DONE ,#'gptel--handle-post-insert ,#'gptel--fsm-last)
    (ABRT ,#'gptel--handle-abort))
  "Alist specifying handlers for `gptel-send' state transitions.

See `gptel-request--handlers' for details.")

(defvar-local gptel--fsm-last nil
  "State machine for latest request in the buffer.")

(defun gptel--fsm-last (fsm)
    "Capture the latest request state FSM for introspection."
    (let ((info (gptel-fsm-info fsm)))
      (unless gptel-log-level
        (let ((data (plist-get info :data)))
          (dolist (key '(:messages :contents :query))
            (setf (plist-get data key) nil))))
      (setf (gptel-fsm-info fsm)
            (plist-put info :end-time (current-time-string)))
      (with-current-buffer (plist-get info :buffer)
        (setq gptel--fsm-last fsm))))

(defun gptel--inspect-fsm (&optional fsm)
  "Inspect gptel request state FSM.

FSM defaults to the state of the last request in the current
buffer."
  (unless fsm
    (setq fsm (or (cdr-safe (cl-find-if
                             (lambda (proc-list)
                               (eq (thread-first (cadr proc-list)
                                                 (gptel-fsm-info)
                                                 (plist-get :buffer))
                                   (current-buffer)))
                             gptel--request-alist))
                  gptel--fsm-last)))
  (unless (cl-typep gptel--fsm-last 'gptel-fsm)
    (user-error "No gptel request log in this buffer yet!"))
  (require 'tabulated-list)
  (with-current-buffer (get-buffer-create "*gptel-diagnostic*")
    (setq tabulated-list-format [("Request attribute" 30 t) ("Value" 30)])
    (let* ((pb (lambda (s) (propertize s 'face 'font-lock-builtin-face)))
           (ps (lambda (s) (propertize s 'face 'font-lock-string-face)))
           (fmt (lambda (s) (cond ((memq (car-safe s) '(closure lambda))
                              (format "#<lambda %#x>" (sxhash s)))
                             ((byte-code-function-p s)
                              (format "#<compiled %#x>" (sxhash s)))
                             ((stringp s) (string-replace "\n" "⮐ " s))
                             (t (prin1-to-string s)))))
           (inhibit-read-only t)
           (info (gptel-fsm-info fsm))
           (entries-info
            (cl-loop
             for idx upfrom 3
             for (key val) on info by #'cddr
             unless (memq key '(:data :history :tools
                                :partial_text :partial_json))
             collect
             (list idx `[,(funcall pb (symbol-name key))
                         ,(funcall ps (funcall fmt val))])))
           (entries-data
            (cl-loop
             for idx upfrom 50
             for (key val) on (plist-get info :data) by #'cddr
             unless (memq key '(:messages :stream :contents :query))
             collect
             (list idx `[,(funcall pb (symbol-name key))
                         ,(funcall ps (funcall fmt val))]))))
      (setq tabulated-list-entries
            (nconc (list `(2 [,(funcall pb ":state")
                              ,(funcall ps
                                (mapconcat
                                 fmt (reverse (cons (gptel-fsm-state fsm)
                                               (plist-get info :history)))
                                 " → "))]))
                   entries-info
                   entries-data))
      (tabulated-list-print)
      (tabulated-list-mode)
      (tabulated-list-init-header)
      (hl-line-mode 1)
      (display-buffer
       (current-buffer)
       '((display-buffer-in-side-window)
         (side . bottom)
         (window-height . fit-window-to-buffer)
         (slot . 10)
         (body-function . select-window))))))

;;;; State machine handlers
;; The next few functions are default state handlers for gptel-send, see
;; `gptel-send--handlers'.

(defun gptel--handle-pre-insert (fsm)
  "Tasks before inserting the LLM response for state FSM.

Handle read-only buffers and run pre-response hooks (but only if
the request succeeded)."
  (let* ((info (gptel-fsm-info fsm))
         (start-marker (plist-get info :position)))
    (when (memq (plist-get info :callback)
                '(gptel--insert-response gptel-curl--stream-insert-response))
      (with-current-buffer (plist-get info :buffer)
        (when (or buffer-read-only (get-char-property start-marker 'read-only))
          (cond
           ((derived-mode-p 'vterm-mode)
            (require 'gptel-integrations)
            (gptel--vterm-pre-insert info))
           (t
            (message "Buffer is read only, displaying reply in buffer \"*LLM response*\"")
            (display-buffer
             (with-current-buffer (get-buffer-create "*LLM response*")
               (visual-line-mode 1)
               (goto-char (point-max))
               (move-marker start-marker (point) (current-buffer))
               (current-buffer))
             '((display-buffer-reuse-window
                display-buffer-pop-up-window)
               (reusable-frames . visible))))))))
    (with-current-buffer (marker-buffer start-marker)
      (when (plist-get info :stream)
        (gptel--update-status " Typing..." 'success))
      (save-excursion
        (goto-char start-marker)
        (when (and (member (plist-get info :http-status) '("200" "100"))
                   gptel-pre-response-hook)
          (run-hooks 'gptel-pre-response-hook))))))

(defun gptel--handle-post-insert (fsm)
  "Tasks after successfully inserting the LLM response with state FSM.

Indicate gptel status, pulse the inserted text and run post-response hooks.

No state transition here since that's handled by the process sentinels."
  (let* ((info (gptel-fsm-info fsm))
         (start-marker (plist-get info :position))
         (tracking-marker (or (plist-get info :tracking-marker)
                              start-marker))
         ;; start-marker may have been moved if :buffer was read-only
         (gptel-buffer (marker-buffer start-marker)))
    (with-current-buffer gptel-buffer
      (if (not tracking-marker)         ;Empty response
          (when gptel-mode (gptel--update-status " Empty response" 'success))
        (set-marker-insertion-type tracking-marker nil) ;Lock tracking-marker
        (when gptel-mode
          (gptel--update-status  " Ready" 'success))))
    ;; Run hook in visible window to set window-point, BUG #269
    (if-let* ((gptel-window (get-buffer-window gptel-buffer 'visible)))
        (with-selected-window gptel-window
          (mapc (lambda (f) (funcall f info)) (plist-get info :post))
          (run-hook-with-args
           'gptel-post-response-functions
           (marker-position start-marker) (marker-position tracking-marker)))
      (with-current-buffer gptel-buffer
        (mapc (lambda (f) (funcall f info)) (plist-get info :post))
        (run-hook-with-args
         'gptel-post-response-functions
         (marker-position start-marker) (marker-position tracking-marker))))
    ;; Insert prompt prefix AFTER post-response hooks have run
    ;; This ensures heading adjustments are complete before calculating prefix level
    (when (and gptel-mode tracking-marker (not (plist-get info :in-place)))
      (with-current-buffer gptel-buffer
        (save-excursion (goto-char tracking-marker)
                        (insert gptel-response-separator
                                (gptel-prompt-prefix-string)))))))

(defun gptel--handle-error (fsm)
  "Check for errors in request state FSM.

Perform UI updates and run post-response hooks."
  (when-let* ((info (gptel-fsm-info fsm))
              (error-data (plist-get info :error))
              (http-msg   (plist-get info :status))
              (gptel-buffer (plist-get info :buffer))
              (start-marker (plist-get info :position))
              (tracking-marker (or (plist-get info :tracking-marker)
                                   start-marker))
              (backend-name
               (gptel-backend-name
                (buffer-local-value 'gptel-backend gptel-buffer))))
    (if (stringp error-data)
        (message "%s error: (%s) %s" backend-name http-msg (string-trim error-data))
      (when-let* ((error-type (plist-get error-data :type)))
        (setq http-msg (concat "("  http-msg ") "
                               (string-trim (gptel--to-string error-type)))))
      (when-let* ((error-msg (plist-get error-data :message)))
        (message "%s error: (%s) %s" backend-name http-msg
                 (string-trim (gptel--to-string error-msg)))))
    (if-let* ((gptel-window (get-buffer-window gptel-buffer 'visible)))
        (with-selected-window gptel-window
          (mapc (lambda (f) (funcall f info)) (plist-get info :post))
          (run-hook-with-args
           'gptel-post-response-functions
           (marker-position start-marker) (marker-position tracking-marker)))
      (with-current-buffer gptel-buffer
        (mapc (lambda (f) (funcall f info)) (plist-get info :post))
        (run-hook-with-args
         'gptel-post-response-functions
         (marker-position start-marker) (marker-position tracking-marker))))
    (with-current-buffer gptel-buffer
      (when gptel-mode
        (gptel--update-status
         (format " Error: %s" http-msg) 'error)))))

(defun gptel--handle-abort (fsm)
  "Perform UI update on `gptel-abort' for FSM."
  (when-let* ((info (gptel-fsm-info fsm))
              (gptel-buffer (plist-get info :buffer))
              (start-marker (plist-get info :position))
              (tracking-marker (or (plist-get info :tracking-marker)
                                   start-marker)))
    ;; Clean up any tool call confirmation overlays in the buffer
    (gptel--clean-tool-overlays-in-buffer gptel-buffer)
    (if-let* ((gptel-window (get-buffer-window gptel-buffer 'visible)))
        (with-selected-window gptel-window
          (mapc (lambda (f) (funcall f info)) (plist-get info :post))
          (run-hook-with-args
           'gptel-post-response-functions
           (marker-position start-marker) (marker-position tracking-marker)))
      (with-current-buffer gptel-buffer
        (mapc (lambda (f) (funcall f info)) (plist-get info :post))
        (run-hook-with-args
         'gptel-post-response-functions
         (marker-position start-marker) (marker-position tracking-marker))))
    ;; Insert prompt prefix AFTER post-response hooks have run
    ;; This ensures heading adjustments are complete before calculating prefix level
    (when (and gptel-mode tracking-marker (not (plist-get info :in-place)))
      (with-current-buffer gptel-buffer
        (save-excursion (goto-char tracking-marker)
                        (insert gptel-response-separator
                                (gptel-prompt-prefix-string)))))
    (with-current-buffer gptel-buffer
      (when gptel-mode (gptel--update-status  " Abort" 'error))
      (run-hooks 'gptel-abort-hook))))

(defun gptel--update-wait (fsm)
  "Update gptel's status after sending a request."
  (with-current-buffer (plist-get (gptel-fsm-info fsm) :buffer)
    (when gptel-mode
      (gptel--update-status " Waiting..." 'warning))))

(defun gptel--update-tool-call (fsm)
  "Update gptel's status when calling a tool."
  (with-current-buffer (plist-get (gptel-fsm-info fsm) :buffer)
    (setq gptel--fsm-last fsm)
    (when gptel-mode
      (gptel--update-status " Calling tool..." 'mode-line-emphasis))))

(defun gptel--update-tool-ask (fsm)
  "Update gptel's status when there are pending tool calls."
  (when (plist-get (gptel-fsm-info fsm) :tool-pending)
    (plist-put (gptel-fsm-info fsm) :tool-pending nil)
    (when gptel-mode
      (gptel--update-status " Run tools?" 'mode-line-emphasis))))


;;; Send queries, handle responses
;;;###autoload
(defun gptel-send (&optional arg)
  "Submit this prompt to the current LLM backend.

By default, the contents of the buffer up to the cursor position
are sent.  If the region is active, its contents are sent
instead.

The response from the LLM is inserted below the cursor position
at the time of sending.  To change this behavior or model
parameters, use prefix arg ARG activate a transient menu with
more options instead.

This command is asynchronous, you can continue to use Emacs while
waiting for the response."
  (interactive "P")
  (if (and arg (require 'gptel-transient nil t))
      (call-interactively #'gptel-menu)
    (gptel--sanitize-model)
    (let ((fsm (gptel-make-fsm :handlers gptel-send--handlers)))
      (gptel-request nil
        :stream gptel-stream
        :transforms gptel-prompt-transform-functions
        :fsm fsm)
      (message "Querying %s..."
               (thread-first (gptel-fsm-info fsm)
                             (plist-get :backend)
                             (or gptel-backend)
                             (gptel-backend-name))))
    (gptel--update-status " Waiting..." 'warning)))

(declare-function json-pretty-print-buffer "json")
(defun gptel--inspect-query (&optional request-fsm format)
  "Show the full LLM query that will be sent in a buffer.

This functions as a dry run of `gptel-send'.  The request data
may be edited and the query continued from this buffer.

REQUEST-FSM is the state of the request, as returned by
`gptel-request'.  If FORMAT is the symbol json, show the encoded
JSON query instead of the Lisp structure gptel uses."
  (unless request-fsm (setq request-fsm gptel--fsm-last))
  (if (bufferp (plist-get (gptel-fsm-info request-fsm) :data))
      (letrec ((dry-run-poll
                (run-with-timer
                 0 1 (lambda (fsm form)
                       (unless (bufferp (plist-get (gptel-fsm-info fsm) :data))
                         (cancel-timer dry-run-poll)
                         (gptel--inspect-query fsm form)))
                 request-fsm format))))
    (with-current-buffer (plist-get (gptel-fsm-info request-fsm) :buffer)
      (gptel--update-status " Ready" 'success))
    (with-current-buffer (get-buffer-create "*gptel-query*")
      (let* ((standard-output (current-buffer))
             (inhibit-read-only t)
             (request-data
              (plist-get (gptel-fsm-info request-fsm) :data)))
        (buffer-disable-undo)
        (erase-buffer)
        (if (eq format 'json)
            (progn (fundamental-mode)
                   (insert (gptel--json-encode request-data))
                   (json-pretty-print-buffer))
          (lisp-data-mode)
          (prin1 request-data)
          (pp-buffer))
        (setq-local gptel--fsm-last request-fsm)
        (goto-char (point-min))
        (view-mode 1)
        (setq buffer-undo-list nil)
        (use-local-map
         (make-composed-keymap
          (define-keymap
            "C-c C-c" #'gptel--continue-query
            "C-c C-w" (lambda () "Copy Curl command for query."
                        (interactive) (gptel--continue-query 'copy))
            "C-c C-k" #'quit-window)
          (current-local-map)))
        (unless header-line-format
          (setq header-line-format
                (substitute-command-keys
                 (concat
                  "Edit request: \\[read-only-mode],"
                  " Send request: \\[gptel--continue-query],"
                  (format " Copy Curl: %s"
                          (propertize "C-c C-w" 'face 'help-key-binding))
                  " Quit: \\[quit-window]"))))
        (display-buffer (current-buffer) gptel-display-buffer-action)))))

(defun gptel--continue-query (&optional copy)
  "Continue sending the gptel query displayed in this buffer.

The request is continued with the same parameters as originally
specified.

With prefix arg COPY, copy the Curl command for the request to the
kill ring instead."
  (interactive "P" lisp-data-mode fundamental-mode)
  (unless (equal (buffer-name) "*gptel-query*")
    (user-error "This command is meant for use in a gptel dry-run buffer"))
  (save-excursion
    (goto-char (point-min))
    (condition-case-unless-debug nil
        (when-let* ((data (if (eq major-mode 'lisp-data-mode)
                              (read (current-buffer))
                            (gptel--json-read))))
          (cl-assert (cl-typep gptel--fsm-last 'gptel-fsm))
          (plist-put (gptel-fsm-info gptel--fsm-last) :data data)
          (if copy                 ;Copy Curl command instead of sending request
              (let ((args (gptel-curl--get-args (gptel-fsm-info gptel--fsm-last)
                                                (md5 (format "%s" (random))))))
                (kill-new
                 (mapconcat #'shell-quote-argument
                            (cons (gptel--curl-path) args) " \\\n"))
                (message "Curl command for request copied to kill-ring"))
            (gptel--fsm-transition gptel--fsm-last) ;INIT -> WAIT
            (quit-window)))
      (error
       (user-error "Can not resume request: could not read data from buffer!")))))

(defun gptel--insert-response (response info &optional raw)
  "Insert the LLM RESPONSE into the gptel buffer.

INFO is a plist containing information relevant to this buffer.
See `gptel--url-get-response' for details.

Optional RAW disables text properties and transformation."
  (let* ((gptel-buffer (plist-get info :buffer))
         (start-marker (plist-get info :position))
         (tracking-marker (plist-get info :tracking-marker)))
    (pcase response
      ((pred stringp)                ;Response text
       (with-current-buffer gptel-buffer
         (when tracking-marker           ;separate from previous response
           (setq response (concat gptel-response-separator response)))
         (save-excursion
           (with-current-buffer (marker-buffer start-marker)
             (goto-char (or tracking-marker start-marker))
             ;; (run-hooks 'gptel-pre-response-hook)
             (unless (or (bobp) (plist-get info :in-place)
                         tracking-marker)
               (insert gptel-response-separator)
               (when gptel-mode
                 (insert (gptel-response-prefix-string)))
               (move-marker start-marker (point)))
             (unless raw
               (when-let* ((transformer (plist-get info :transformer)))
                 (setq response (funcall transformer response)))
               (add-text-properties
                0 (length response) '(gptel response front-sticky (gptel)) response))
             (insert response)
             (plist-put info :tracking-marker (setq tracking-marker (point-marker)))
             ;; for uniformity with streaming responses
             (set-marker-insertion-type tracking-marker t)))))
      (`(reasoning . ,text)
       (when-let* ((include (plist-get info :include-reasoning)))
         (if (stringp include)
             (with-current-buffer (get-buffer-create
                                   (plist-get info :include-reasoning))
               (save-excursion (goto-char (point-max)) (insert text)))
           (with-current-buffer (marker-buffer start-marker)
             (let ((separator         ;Separate from response prefix if required
                    (and (not tracking-marker) gptel-mode
                         (not (string-suffix-p "\n" (gptel-response-prefix-string)))
                         "\n"))
                   (blocks (if (derived-mode-p 'org-mode)
                               `("#+begin_reasoning\n" . ,(concat "\n#+end_reasoning"
                                                           gptel-response-separator))
                             ;; TODO(reasoning) remove properties and strip instead
                             (cons (propertize "``` reasoning\n" 'gptel 'ignore
                                               'keymap gptel--markdown-block-map)
                                   (concat (propertize "\n```" 'gptel 'ignore
                                                       'keymap gptel--markdown-block-map)
                                           gptel-response-separator)))))
               (if (eq include 'ignore)
                   (progn
                     (add-text-properties
                      0 (length text) '(gptel ignore front-sticky (gptel)) text)
                     (gptel--insert-response
                      (concat (car blocks) text (cdr blocks)) info t))
                 (gptel--insert-response (concat separator (car blocks)) info t)
                 (gptel--insert-response text info)
                 (gptel--insert-response (cdr blocks) info t))
               (save-excursion
                 (goto-char (plist-get info :tracking-marker))
                 (if (derived-mode-p 'org-mode) ;fold block
                     (progn (search-backward "#+end_reasoning" start-marker t)
                            (when (looking-at "^#\\+end_reasoning")
                              (org-cycle)))
                   (when (re-search-backward "^```" start-marker t)
                     (gptel-markdown-cycle-block)))))))))
      (`(tool-call . ,tool-calls)
       (gptel--display-tool-calls tool-calls info))
      (`(tool-result . ,tool-results)
       (gptel--display-tool-results tool-results info)))))

(defun gptel-curl--stream-insert-response (response info &optional raw)
  "Insert streaming RESPONSE from an LLM into the gptel buffer.

INFO is a mutable plist containing information relevant to this buffer.
See `gptel--url-get-response' for details.

Optional RAW disables text properties and transformation."
  (pcase response
    ((pred stringp)
     (let ((start-marker (plist-get info :position))
           (tracking-marker (plist-get info :tracking-marker))
           (transformer (plist-get info :transformer)))
       (with-current-buffer (marker-buffer start-marker)
         (save-excursion
           (unless tracking-marker
             (goto-char start-marker)
             (unless (or (bobp) (plist-get info :in-place))
               (insert gptel-response-separator)
               (when gptel-mode
                 ;; Put prefix before AI response.
                 (insert (gptel-response-prefix-string)))
               (move-marker start-marker (point)))
             (setq tracking-marker (set-marker (make-marker) (point)))
             (set-marker-insertion-type tracking-marker t)
             (plist-put info :tracking-marker tracking-marker))
           (goto-char tracking-marker)
           (unless raw
             (when transformer
               (setq response (funcall transformer response)))
             (add-text-properties
              0 (length response) '(gptel response front-sticky (gptel))
              response))
           ;; (run-hooks 'gptel-pre-stream-hook)
           (insert response)
           (run-hooks 'gptel-post-stream-hook)))))
    (`(reasoning . ,text)
     (gptel--display-reasoning-stream text info))
    (`(tool-call . ,tool-calls)
     (gptel--display-tool-calls tool-calls info))
    (`(tool-result . ,tool-results)
     (gptel--display-tool-results tool-results info)
     ;; Adjust for tool calls inside reasoning blocks
     (when (eq (plist-get info :reasoning-block) 'in)
       (when-let* ((rm (plist-get info :reasoning-marker))
                   (tm (plist-get info :tracking-marker)))
         (move-marker rm tm (marker-buffer tm)))))))

;;;###autoload
(defun gptel (name &optional _ initial interactivep)
  "Switch to or start a chat session with NAME.

Ask for API-KEY if `gptel-api-key' is unset.

If region is active, use it as the INITIAL prompt.  Returns the
buffer created or switched to.

INTERACTIVEP is t when gptel is called interactively."
  (interactive
   (let* ((backend (default-value 'gptel-backend))
          (backend-name
           (format "*%s*" (gptel-backend-name backend))))
     (list (read-buffer
            "Create or choose gptel buffer: "
            backend-name nil                         ; DEFAULT and REQUIRE-MATCH
            (lambda (b)                                   ; PREDICATE
              ;; NOTE: buffer check is required (#450)
              (and-let* ((buf (get-buffer (or (car-safe b) b))))
                (buffer-local-value 'gptel-mode buf))))
           (condition-case nil
               (gptel--get-api-key
                (gptel-backend-key backend))
             ((error user-error)
              (setq gptel-api-key
                    (read-passwd
                     (format "%s API key: " backend-name)))))
           (and (use-region-p)
                (buffer-substring (region-beginning)
                                  (region-end)))
           t)))
  (with-current-buffer (get-buffer-create name)
    (cond                               ;Set major mode
     ((eq major-mode gptel-default-mode))
     ((eq gptel-default-mode 'text-mode)
      (text-mode)
      (visual-line-mode 1))
     (t (funcall gptel-default-mode)))
    (gptel--sanitize-model :backend (default-value 'gptel-backend)
                           :model (default-value 'gptel-model)
                           :shoosh nil)
    (unless gptel-mode (gptel-mode 1))
    (goto-char (point-max))
    (skip-chars-backward "\t\r\n")
    (if (bobp) (insert (or initial (gptel-prompt-prefix-string))))
    (when interactivep
      (display-buffer (current-buffer) gptel-display-buffer-action)
      (message "Send your query with %s!"
               (substitute-command-keys "\\[gptel-send]")))
    (current-buffer)))


;;; Reasoning content UI
(defun gptel--display-reasoning-stream (text info)
  "Show reasoning TEXT in an appropriate location.

INFO is the request INFO, see `gptel--url-get-response'.  This is
for streaming responses only."
  (when-let* ((include (plist-get info :include-reasoning)))
    (if (stringp include)
        (unless (eq text t)
          (with-current-buffer (get-buffer-create include)
            (save-excursion (goto-char (point-max))
                            (insert text))))
      (let* ((reasoning-marker (plist-get info :reasoning-marker))
             (tracking-marker (plist-get info :tracking-marker))
             (start-marker (plist-get info :position)))
        (with-current-buffer (marker-buffer start-marker)
          (if (eq text t)               ;end of stream
              (progn
                (gptel-curl--stream-insert-response
                 (concat (if (derived-mode-p 'org-mode)
                             "\n#+end_reasoning"
                           ;; TODO(reasoning) remove properties and strip instead
                           (propertize "\n```" 'gptel 'ignore
                                       'keymap gptel--markdown-block-map))
                         gptel-response-separator)
                 info t)
                (ignore-errors          ;fold block
                  (save-excursion
                    (goto-char tracking-marker)
                    (if (derived-mode-p 'org-mode)
                        (progn (search-backward "#+end_reasoning" start-marker t)
                               (when (looking-at "^#\\+end_reasoning")
                                 (org-cycle)))
                      (when (re-search-backward "^```" start-marker t)
                        (gptel-markdown-cycle-block))))))
            (unless (and reasoning-marker tracking-marker
                         (= reasoning-marker tracking-marker))
              (let ((separator        ;Separate from response prefix if required
                     (and (not tracking-marker) gptel-mode
                          (not (string-suffix-p
                                "\n" (gptel-response-prefix-string)))
                          "\n")))
                (gptel-curl--stream-insert-response
                 (concat separator
                         (if (derived-mode-p 'org-mode)
                             "#+begin_reasoning\n"
                           ;; TODO(reasoning) remove properties and strip instead
                           (propertize "``` reasoning\n" 'gptel 'ignore
                                       'keymap gptel--markdown-block-map)))
                 info t)))
            (if (eq include 'ignore)
                (progn
                  (add-text-properties
                   0 (length text) '(gptel ignore front-sticky (gptel)) text)
                  (gptel-curl--stream-insert-response text info t))
              (gptel-curl--stream-insert-response text info)))
          (setq tracking-marker (plist-get info :tracking-marker))
          (if reasoning-marker
              (move-marker reasoning-marker tracking-marker)
            (plist-put info :reasoning-marker
                       (copy-marker tracking-marker nil))))))))

(defvar gptel--tool-preview-alist nil
  "Alist mapping tool names to preview functions for tools.

Each key is a tool name (string) and value is a list of one or two
functions, for preview-setup and (optional) preview-teardown.

The preview-setup function is called with two arguments: a list of the
corresponding tool call arguments and the request INFO plist.  It must
set up the preview for the tool call and return a handle to the preview,
which can be any object, but typically an overlay or a buffer.

The preview-setup can integrate with gptel's default previewer by
inserting at point (and moving point), or use a different preview method
entirely.

The preview-teardown function, if provided, is called with this handle
when the tool call is accepted or rejected, and it must clear the
preview.

Note: This tool call preview API is currently experimental.")


;;; Tool use UI
(defvar-keymap gptel-tool-call-actions-map
  :doc "Keymap for actions on tool calls."
  "<mouse-1>" #'gptel--dispatch-tool-calls
  "C-c C-c" #'gptel--accept-tool-calls
  "C-c C-k" #'gptel--reject-tool-calls
  "C-c C-d" #'gptel--deny-tool-calls
  "C-c C-i" #'gptel--inspect-tool-calls)

(defun gptel--display-tool-calls (tool-calls info &optional use-minibuffer)
  "Handle tool call confirmation.

TOOL-CALLS should be a list of tool call specifications or results,
structured as:

 ((tool args callback) ...)

for tool call specifications to be confirmed.  INFO contains the
state of the request.  To prompt for tool call confirmation, use
either an overlay in the request buffer or the minibuffer (if
USE-MINIBUFFER is non-nil)."
  (let* ((start-marker (plist-get info :position))
         (tracking-marker (plist-get info :tracking-marker)))
    ;; pending tool calls look like ((tool callback args) ...)
    (with-current-buffer (plist-get info :buffer)
      (if (or use-minibuffer        ;prompt for confirmation from the minibuffer
              buffer-read-only ;TEMP(tool-preview) Handle read-only buffers better
              (get-char-property
               (max (point-min) (1- (or tracking-marker start-marker)))
               'read-only))
          (let* ((minibuffer-allow-text-properties t)
                 (backend-name (gptel-backend-name (plist-get info :backend)))
                 (prompt (format "%s wants to run " backend-name)))
            (map-y-or-n-p
             (lambda (tool-call-spec)
               (concat prompt (propertize (gptel-tool-name (car tool-call-spec))
                                          'face 'font-lock-keyword-face)
                       ": "))
             (lambda (tcs) (gptel--accept-tool-calls (list tcs) nil))
             tool-calls '("tool call" "tool calls" "run")
             `((?i ,(lambda (_) (save-window-excursion
                             (with-selected-window
                                 (gptel--inspect-fsm gptel--fsm-last)
                               (goto-char (point-min))
                               (when (search-forward-regexp "^:tool-use" nil t)
                                 (forward-line 0) (hl-line-highlight))
                               (use-local-map
                                (make-composed-keymap
                                 (define-keymap "q" (lambda () (interactive)
                                                      (quit-window)
                                                      (exit-recursive-edit)))
                                 (current-local-map)))
                               (recursive-edit) nil)))
                   "inspect call(s)"))))
        ;; Prompt for confirmation from the chat buffer
        (let* ((backend-name (gptel-backend-name (plist-get info :backend)))
               (actions-string
                (concat (propertize "Run tools: " 'face 'font-lock-string-face)
                        (propertize "C-c C-c" 'face 'help-key-binding)
                        (propertize ", Cancel: " 'face 'font-lock-string-face)
                        (propertize "C-c C-k" 'face 'help-key-binding)
                        (propertize ", Deny: " 'face 'font-lock-string-face)
                        (propertize "C-c C-d" 'face 'help-key-binding)
                        (propertize ", Inspect: " 'face 'font-lock-string-face)
                        (propertize "C-c C-i" 'face 'help-key-binding)))
               (confirm-strings)
               ;; FIXME(tool) use a wrapper instead of a manual text-property search,
               ;; this is fragile
               (ov-start (save-excursion
                           (goto-char start-marker)
                           (text-property-search-backward 'gptel 'response)
                           (point)))
               (preview-handlers)
               (ov (or (cdr-safe (get-char-property-and-overlay
                                  start-marker 'gptel-tool))
                       (make-overlay ov-start (or tracking-marker start-marker)
                                     nil nil nil)))
               (prompt-ov))
          ;; If the cursor is at the overlay-end, it ends up outside, so move it back
          (unless tracking-marker
            (when (= (point) start-marker) (ignore-errors (backward-char))))
          (save-excursion
            (goto-char (overlay-end ov))
            (pcase-dolist (`(,tool-spec ,arg-values _) tool-calls)
              ;; Call tool-specific confirmation prompt
              (if-let* ((funcs (cdr (assoc (gptel-tool-name tool-spec)
                                           gptel--tool-preview-alist)))
                        ((functionp (car-safe funcs))))
                  ;;preview-teardown func   preview-handle overlay/buffer
                  (push (list (cadr funcs) (funcall (car funcs) arg-values info))
                        preview-handlers)
                (push (gptel--format-tool-call (gptel-tool-name tool-spec) arg-values)
                      confirm-strings)))
            (and confirm-strings (apply #'insert (nreverse confirm-strings)))
            (add-text-properties (overlay-end ov) (1- (point))
                                 '(read-only t font-lock-fontified t))
            (setq prompt-ov (make-overlay (overlay-end ov) (point) nil t))
            (overlay-put
             prompt-ov 'before-string
             (concat "\n"
                     (propertize " " 'display `(space :align-to (- right ,(length actions-string) 2))
                                 'face '(:inherit font-lock-string-face :underline t :extend t))
                     actions-string
                     (format (propertize "\n%s wants to run:\n\n"
                                         'face 'font-lock-string-face)
                             backend-name)))
            (overlay-put
             prompt-ov 'after-string
             (concat (propertize "\n" 'face
                                 '(:inherit font-lock-string-face :underline t :extend t))))
            (overlay-put prompt-ov 'evaporate t)
            (overlay-put ov 'prompt prompt-ov)
            (move-overlay ov ov-start (point)))
          ;; Add confirmation prompt to the overlay
          (when preview-handlers (overlay-put ov 'previews preview-handlers))
          (overlay-put ov 'mouse-face 'highlight)
          (overlay-put ov 'gptel-tool tool-calls)
          (overlay-put ov 'help-echo
                       (concat "Tool call(s) requested: " actions-string))
          (overlay-put ov 'keymap gptel-tool-call-actions-map)
          prompt-ov)))))

(defun gptel--display-tool-results (tool-results info)
  "Insert TOOL-RESULTS into buffer.

TOOL-RESULTS is

 ((tool args result) ...)

for tool call results.  INFO contains the state of the request."
  (let* ((start-marker (plist-get info :position))
         (tool-marker (plist-get info :tool-marker))
         (tracking-marker (plist-get info :tracking-marker)))
    ;; Insert tool results
    (with-current-buffer (marker-buffer start-marker)
      (when gptel-include-tool-results
        (cl-loop
         for (tool args result) in tool-results
         with include-names =
         (mapcar #'gptel-tool-name
                 (cl-remove-if-not #'gptel-tool-include (plist-get info :tools)))
         if (or (eq gptel-include-tool-results t)
                (member (gptel-tool-name tool) include-names))
         do (funcall
             (plist-get info :callback)
             (let* ((name (gptel-tool-name tool))
                    (separator        ;Separate from response prefix if required
                     (cond ((not tracking-marker)
                            (and gptel-mode
                                 (not (string-suffix-p
                                       "\n" (gptel-response-prefix-string)))
                                 "\n"))           ;start of response
                           ((not (and tool-marker ;not consecutive tool result blocks
                                      (= tracking-marker tool-marker)))
                            gptel-response-separator)))
                    (tool-use
                     ;; TODO(tool) also check args since there may be more than
                     ;; one call/result for the same tool
                     (cl-find-if
                      (lambda (tu) (equal (plist-get tu :name) name))
                      (plist-get info :tool-use)))
                    (id (plist-get tool-use :id))
                    (display-call (format "(%s %s)" name
                                          (string-trim (prin1-to-string args) "(" ")")))
                    (call (prin1-to-string `(:name ,name :args ,args)))
                    (truncated-call
                     (string-replace "\n" " "
                                     (truncate-string-to-width
                                      display-call
                                      (floor (* (window-width) 0.6)) 0 nil " ...)"))))
               (if (derived-mode-p 'org-mode)
                   (concat
                    separator
                    "#+begin_src gptel-tool\n"
                    truncated-call
                    (propertize
                     (org-escape-code-in-string (concat "\n" call "\n\n" result))
                     'gptel `(tool . ,id))
                    "\n#+end_src\n")
                 ;; TODO(tool) else branch is handling all front-ends as markdown.
                 ;; At least escape markdown.
                 (concat
                  separator
                  ;; TODO(tool) remove properties and strip instead of ignoring
                  (propertize (format "``` tool %s" truncated-call)
                              'gptel 'ignore 'keymap gptel--markdown-block-map)
                  (propertize
                   ;; TODO(tool) escape markdown in result
                   (concat "\n" call "\n\n" result)
                   'gptel `(tool . ,id))
                  ;; TODO(tool) remove properties and strip instead of ignoring
                  (propertize "\n```\n" 'gptel 'ignore
                              'keymap gptel--markdown-block-map))))
             info
             'raw)
         ;; tool-result insertion has updated the tracking marker
         (unless tracking-marker
           (setq tracking-marker (plist-get info :tracking-marker)))
         (if tool-marker
             (move-marker tool-marker tracking-marker)
           (setq tool-marker (copy-marker tracking-marker nil))
           (plist-put info :tool-marker tool-marker))
         (ignore-errors                 ;fold drawer
           (save-excursion
             (goto-char tracking-marker)
             (forward-line -1)
             (if (derived-mode-p 'org-mode)
                 (when (looking-at-p "^#\\+end_src") (org-cycle))
               (when (looking-at-p "^```") (gptel-markdown-cycle-block))))))))))

(defun gptel--format-tool-call (name arg-values)
  "Format a tool call for display in the buffer.

NAME and ARG-VALUES are the name and arguments for the call."
  (format "(%s %s)\n"
          (propertize name 'font-lock-face 'font-lock-keyword-face)
          (propertize
           (mapconcat (lambda (arg)
                        (cond ((stringp arg)
                               (prin1-to-string
                                (replace-regexp-in-string
                                 "\n" "⮐" (truncate-string-to-width
                                           arg (floor (window-width) 2)
                                           nil nil t))))
                              (t (prin1-to-string arg))))
                      arg-values " ")
           'font-lock-face 'font-lock-constant-face)))

(defun gptel--inspect-tool-calls ()
  (interactive)
  (with-selected-window
      (gptel--inspect-fsm gptel--fsm-last)
    (goto-char (point-min))
    (when (search-forward-regexp "^:tool-use" nil t)
      (forward-line 0)
      (hl-line-highlight))))

(defun gptel--clean-tool-overlay (ov)
  "Clean up tool call overlay OV and its associated previews.

Tears down any preview handlers, deletes the prompt region, and
removes the overlay."
  (when (and (overlayp ov) (overlay-buffer ov))
    (with-current-buffer (overlay-buffer ov)
      (when-let* ((preview-handles (overlay-get ov 'previews)))
        (dolist (func-to-handle preview-handles)
          (when (car func-to-handle) (apply func-to-handle))))
      (when-let* ((prompt-ov (overlay-get ov 'prompt))
                  ((overlay-buffer prompt-ov))
                  (inhibit-read-only t))
        (delete-region (overlay-start prompt-ov)
                       (overlay-end prompt-ov))))
    (delete-overlay ov)))

(defun gptel--clean-tool-overlays-in-buffer (&optional buf)
  "Clean up all tool call overlays in BUF (defaults to current buffer).

This finds and removes all overlays with the `gptel-tool' property,
tearing down their previews and prompt regions."
  (with-current-buffer (or buf (current-buffer))
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'gptel-tool)
        (gptel--clean-tool-overlay ov)))))

(defun gptel--accept-tool-calls (&optional response ov)
  (interactive (pcase-let ((`(,resp . ,o) (get-char-property-and-overlay
                                           (point) 'gptel-tool)))
                 (list resp o)))
  (gptel--update-status " Calling tool..." 'mode-line-emphasis)
  (message "Continuing query...")
  (cl-loop for (tool-spec arg-values process-tool-result) in response
           do
           (if (gptel-tool-async tool-spec)
               (apply (gptel-tool-function tool-spec)
                      process-tool-result arg-values)
             (let ((result
                    (condition-case errdata
                        (apply (gptel-tool-function tool-spec) arg-values)
                      (error (mapconcat #'gptel--to-string errdata " ")))))
               (funcall process-tool-result result))))
  (gptel--clean-tool-overlay ov))

(defun gptel--reject-tool-calls (&optional _response ov)
  (interactive (pcase-let ((`(,resp . ,o) (get-char-property-and-overlay
                                           (point) 'gptel-tool)))
                 (list resp o)))
  (gptel--update-status " Tools cancelled" 'error)
  (message (substitute-command-keys
            "Tool calls canceled.  \\[gptel-menu] to continue them!"))
  (gptel--clean-tool-overlay ov)
  ;; Run post-response hooks and insert prompt prefix, like gptel--handle-abort
  (when-let* ((info (and gptel--fsm-last (gptel-fsm-info gptel--fsm-last)))
              (gptel-buffer (plist-get info :buffer))
              (start-marker (plist-get info :position))
              (tracking-marker (or (plist-get info :tracking-marker)
                                   start-marker)))
    (with-current-buffer gptel-buffer
      (run-hook-with-args
       'gptel-post-response-functions
       (marker-position start-marker) (marker-position tracking-marker))
      ;; Insert prompt prefix AFTER post-response hooks have run
      (when (and gptel-mode tracking-marker (not (plist-get info :in-place)))
        (save-excursion (goto-char tracking-marker)
                        (insert gptel-response-separator
                                (gptel-prompt-prefix-string)))))))

(defun gptel--deny-tool-calls (&optional response ov)
  "Deny tool calls and inform the LLM about the denial.

Unlike `gptel--reject-tool-calls' which freezes the FSM, this
sends a denial result back to the LLM for each tool call so it
can adjust its approach.  The user is prompted for a reason.
RESPONSE is the list of pending tool calls and OV is the tool
call overlay."
  (interactive (pcase-let ((`(,resp . ,o) (get-char-property-and-overlay
                                           (point) 'gptel-tool)))
                 (list resp o)))
  (let* ((reason (read-string "Reason for denying (empty for none): "))
         (denial-msg
          (if (string-empty-p reason)
              "Error: The user denied execution of this tool call.  \
Adjust your approach or ask the user for guidance."
            (format "Error: The user denied execution of this tool call.  \
Reason: %s" reason))))
    (gptel--update-status " Tools denied, informing LLM..." 'warning)
    (message "Denying tool calls and informing the LLM...")
    (cl-loop for (_tool-spec _arg-values process-tool-result) in response
             do (funcall process-tool-result denial-msg))
    (gptel--clean-tool-overlay ov)))

(defun gptel--dispatch-tool-calls (choice)
  (interactive
   (list
    (let ((choices '((?y "yes") (?n "do nothing")
                     (?k "cancel request") (?d "deny and inform LLM")
                     (?i "inspect call(s)"))))
      (read-multiple-choice "Run tool calls? " choices))))
  (pcase (car choice)
    (?y (call-interactively #'gptel--accept-tool-calls))
    (?k (call-interactively #'gptel--reject-tool-calls))
    (?d (call-interactively #'gptel--deny-tool-calls))
    (?i (gptel--inspect-fsm gptel--fsm-last))))


;;; Presets
;;;; Presets implementation
(defvar gptel--known-presets nil
  "Alist of presets for gptel.

Each entry maps a preset name (a symbol) to a plist of
specifications (see `gptel-make-preset').")

(defun gptel-make-preset (name &rest keys)
  "Define a gptel preset with NAME.

A preset is a combination of gptel options intended to be applied and
used together.  Presets can make it less tedious to change gptel
settings on the fly.

Typically this will include a model, backend, system message and perhaps
some tools, but any set of gptel options can be set this way.

NAME must be a symbol.  KEYS is a plist corresponding to the options
being set.  All KEYS are optional.

Recognized keys:

DESCRIPTION is a description of the preset, used when selecting a
preset.

PARENTS is a preset name (or list of preset names) to apply before this
one.

PRE and POST are functions to run before and after the preset is
applied.  They take no arguments.

BACKEND is the `gptel-backend' to set, or its name (like \"ChatGPT\").

MODEL is the `gptel-model', a symbol.

SYSTEM is the directive.  It can be
- the system message (a string),
- a list of strings (a conversation template)
- or a function (dynamic system message).
- It can also be a symbol naming a directive in `gptel-directives'.

TOOLS is a list of gptel tools or tool names, like
\\='(\"read_url\" \"read_buffer\" ...)

Recognized keys are not limited to the above.  Any other key (like
`:foo') corresponds to the value of either `gptel-foo' (preferred) or
`gptel--foo'.
- So TOOLS corresponds to option `gptel-tools',
- CONFIRM-TOOL-CALLS to `gptel-confirm-tool-calls',
- TEMPERATURE to `gptel-temperature' and so on.
See gptel's customization options for all available settings.

Specifying the value of a key will set the corresponding gptel option to
it.  For example,

  (gptel-make-preset \\='websearch
    :tools \\='(\"search_web\" \"read_url\")
    :system \"Use the provided tools to search the web
              for up-to-date information\")

will replace the currently active option `gptel-tools' and the system
message.

Alternatively,

- You can require that the value be appended or prepended to the
  existing value instead of replacing it.  This can be done by
  specifying the value as a plist instead with the keys `:prepend' or
  `:append'.

  (gptel-make-preset \\='websearch
    :tools  \\='(:append (\"search_web\" \"read_url\"))
    :system \\='(:prepend \"Use the provided tools to search the web
                        for up-to-date information.\"))

- You can dynamically compute the value for a key at the time the preset
  is applied with `:eval' or `:function'.  This is mostly useful when
  using presets in the prompt, as @preset-name.

  An `:eval' form is evaluated when the preset is applied:

  (gptel-make-preset \\='visible-buffers
    :description \"Include the full text of all buffers visible in the
                 frame.\"
    :context \\='(:eval (mapcar #\\='window-buffer (window-list))))
    ▲                ▲
    │                ╰╴evaluated when preset is applied
    ╰╴sets `gptel-context'

  `:function' should take the current value of the key as an input and
  return the new value.  Here we combine it with `:append' in the plist.

  (gptel-make-preset \\='github-read-only
    :description \"Provide read-only GitHub tools\"
    :pre (lambda () (gptel-mcp-connect \\='(\"github\") \\='sync))
    :tools
    \\='( :append (\"mcp-github\")       ;Adds all github MCP tools
       :function (lambda (tools)
                   (cl-delete-if    ;Remove \"write\" access to GitHub
                    (lambda (tool)
                      (string-match-p \"create_\" (gptel-tool-name tool)))
                    tools))))

  NOTE: `:eval' and `:function' are evaluated in a temporary buffer, and
  not the buffer from which the request is sent."
  (declare (indent 1))
  (if-let* ((p (assoc name gptel--known-presets)))
      (setcdr p keys)
    (setq gptel--known-presets          ;Add at end of presets for menu ordering
          (nconc gptel--known-presets (list (cons name keys))))))

(defun gptel-get-preset (name)
  "Get the gptel preset spec with NAME."
  (alist-get name gptel--known-presets nil nil #'equal))

(defun gptel--save-preset (name &optional description)
  "Save gptel's current settings as a preset with NAME.

NAME must be a symbol.  DESCRIPTION is added if provided.  In addition
to registering the preset, elisp code to do the same is copied to the
`kill-ring'."
  (interactive
   (list (intern (completing-read "Save gptel settings to (existing or new) preset: "
                                  gptel--known-presets))
         (read-string "Description (optional): ")))
  (let ((preset-code
         `(gptel-make-preset ',name
           :description ,(when (and description
                                (not (string-blank-p description)))
                          description)
           :backend ,(gptel-backend-name gptel-backend)
           :model ',gptel-model
           :system ,(if-let* ((directive (car-safe (rassoc gptel--system-message
                                                    gptel-directives))))
                         `',directive
                      gptel--system-message)
           :tools ',(mapcar #'gptel-tool-name gptel-tools)
           :stream ,gptel-stream
           :temperature ,gptel-temperature
           :max-tokens ,gptel-max-tokens
           :use-context ',gptel-use-context
           :track-media ,gptel-track-media
           :include-reasoning ,(let ((reasoning gptel-include-reasoning))
                                   (if (eq reasoning 'ignore)
                                       ''ignore reasoning)))))
    (kill-new (pp-to-string preset-code))
    (eval preset-code)
    (message "Preset %s saved. (Lisp expression for preset saved to kill-ring)"
             (propertize (symbol-name name) 'face 'highlight))))

(defvar gptel--rewrite-directive)
(defun gptel--apply-preset (preset &optional setter)
  "Apply gptel PRESET with SETTER.

PRESET is the name of a preset, or a spec (plist) of the form
 (:KEY1 VAL1 :KEY2 VAL2 ...).

SETTER is the function used to set the gptel options.  It must accept
two arguments, the symbol being set and the value to set it to.  It
defaults to `set', and can be set to a different function to (for
example) apply the preset buffer-locally."
  (unless setter (setq setter #'set))
  (when (memq (type-of preset) '(string symbol))
    (let ((spec (or (gptel-get-preset preset)
                    (user-error "gptel preset \"%s\": Cannot find preset"
                                preset))))
      (funcall setter 'gptel--preset preset)
      (setq preset spec)))
  (when-let* ((func (plist-get preset :pre))) (funcall func))
  (when-let* ((parents (plist-get preset :parents)))
    (mapc (lambda (parent) (gptel--apply-preset parent setter)) (ensure-list parents)))
  (map-do
   (lambda (key val)
     (pcase key
       ((or :parents :description :pre :post) nil)
       ((or :system :system-message :rewrite-directive)
        (let ((sym (if (eq key :rewrite-directive)
                       'gptel--rewrite-directive 'gptel--system-message)))
          (when (consp val)
            ;; Possibly complain about trying to compose a system message string
            ;; with a non-string
            ;; TODO(modify-list): Catch other incompatible combinations
            (and (or (plist-member val :append) (plist-member val :prepend))
                 (not (stringp (symbol-value sym)))
                 (user-error "Composing non-string system messages is not implemented"))
            (setq val (gptel--modify-value (symbol-value sym) val)))
          (if (and (symbolp val) (not (functionp val)))
              (if-let* ((directive (alist-get val gptel-directives)))
                  (funcall setter sym directive)
                (user-error "gptel preset: Cannot find directive %s" val))
            (funcall setter sym val))))
       (:backend
        (when (consp val) (setq val (gptel--modify-value 'gptel-backend val)))
        (setq val (cl-etypecase val
                    (gptel-backend val)
                    (string (gptel-get-backend val))))
        (unless val
          (user-error "gptel preset: Cannot find backend %s" val))
        (funcall setter 'gptel-backend val))
       (:tools                          ;TEMP Confirm this `:append' convention
        (setq val (gptel--modify-value gptel-tools val))
        (let* ((tools
                (flatten-list
                 (cl-loop for tool-name in (ensure-list val)
                          for tool = (cl-etypecase tool-name
                                       (gptel-tool tool-name)
                                       (string (ignore-errors
                                                 (gptel-get-tool tool-name))))
                          do (unless tool
                               (user-error "gptel preset: Cannot find tool %S"
                                           tool-name))
                          collect tool))))
          (funcall setter 'gptel-tools (cl-delete-duplicates tools :test #'eq))))
       ((and (let sym (or (intern-soft
                           (concat "gptel-" (substring (symbol-name key) 1)))
                          (intern-soft
                           (concat "gptel--" (substring (symbol-name key) 1)))))
             (guard (and sym (boundp sym))))
        (funcall setter sym (if (consp val)
                                (gptel--modify-value (symbol-value sym) val)
                              val)))
       (_ (display-warning
           '(gptel presets)
           (format "gptel preset: setting for %s not found, ignoring." key)))))
   preset)
  (when-let* ((func (plist-get preset :post))) (funcall func)))

(defun gptel--preset-syms (preset)
  "Return a list of gptel variables (symbols) set by PRESET.

PRESET is the name of a preset, or a spec (plist) of the form
 (:KEY1 VAL1 :KEY2 VAL2 ...)."
  (when (memq (type-of preset) '(string symbol))
    (let ((spec (or (gptel-get-preset preset)
                    (user-error "gptel preset \"%s\": Cannot find preset"
                                preset))))
      (setq preset spec)))
  (let* ((index preset)
         syms key val)
    (while index
      (setq key (pop index) val (pop index))
      (pcase key
        ((or :description :pre :post))
        (:parents
         (mapc (lambda (parent-preset)
                 (nconc syms (gptel--preset-syms parent-preset)))
               (ensure-list val)))
        (:system (push 'gptel--system-message syms))
        (_ (if-let* ((var (or (intern-soft
                               (concat "gptel-" (substring (symbol-name key) 1)))
                              (intern-soft
                               (concat "gptel--" (substring (symbol-name key) 1))))))
               (push var syms)
             (display-warning
              '(gptel presets)
              (format "gptel preset \"%s\": setting for %s not found, ignoring."
                      (car preset) key))))))
    (cl-delete-duplicates syms)))

;; This is identical to `cl-progv', only we let-bind symbols SYM from the preset
;; to their current values instead of evaluating the values explicitly. (#1005)
(defmacro gptel-with-preset (name &rest body)
  "Run BODY with gptel preset NAME applied.

This macro can be used to create `gptel-request' command with settings
from a gptel preset applied.

NAME is the name of a preset, or a spec (plist) of the form
 (:KEY1 VAL1 :KEY2 VAL2 ...).  It must be quoted."
  (declare (indent 1))
  (let ((syms (make-symbol "syms"))
        (binds (make-symbol "binds"))
        (bodyfun (make-symbol "body")))
    ;; Let-bind symbols that we want to modify with the presets.  Also include
    ;; `gptel--preset' in this list as we don't want to change its value outside
    ;; of this macro's scope.
    `(let* ((,syms (cons 'gptel--preset (gptel--preset-syms ,name)))
            (,bodyfun (lambda () (gptel--apply-preset ,name) ,@body))
            (,binds nil))
       (while ,syms (push (list (car ,syms) (pop ,syms)) ,binds))
       (eval (list 'let (nreverse ,binds) (list 'funcall (list 'quote ,bodyfun)))))))

(defun gptel--preset-mismatch-value (preset-spec key val)
  "Determine if the value of KEY in PRESET-SPEC matches VAL.

This is an imperfect check for whether the value corresponding to KEY (a
keyword) in PRESET-SPEC (a plist) matches VAL.  This is required
primarily to identify which gptel variable values have changed since
PRESET-SPEC was applied, which is relevant when writing gptel metadata
to a chat file.

See also `gptel--preset-mismatch-p'."
  ;; In all cases, assume a mismatch if the preset's value for KEY is a
  ;; modify-list spec, such as (:append ...)
  ;; Mismatches may not even be well-defined/determinable in these cases.
  (or (not preset-spec)
      (pcase key
        ;; special cases
        ((or :system :system-message)
         (let ((system (plist-get preset-spec :system)))
           (or (and (stringp system) (not (equal system val)))
               (functionp system)
               (and (consp system) (keywordp (car system)))
               (and (consp system)
                    (not (equal (car-safe (gptel--parse-directive system))
                                val))))))
        (:backend
         (let ((backend (plist-get preset-spec :backend)))
           (or (and (consp backend) (keywordp (car-safe backend)))
               (not (equal (or (and (gptel-backend-p val) (gptel-backend-name val))
                               val)
                           (or (and (gptel-backend-p backend) (gptel-backend-name backend))
                               backend))))))
        ;; FIXME: We're assuming that val is a list of tool names, not tools
        (:tools
         (and-let* ((preset-tools (plist-get preset-spec :tools)))
           (or (keywordp (car-safe preset-tools))
               (cl-loop
                for tool in preset-tools
                for tool-name =
                (or (and (stringp tool) tool)
                    (ignore-errors (gptel-tool-name tool)))
                if (not (member tool-name uniq-tool-names))
                collect tool-name into uniq-tool-names
                finally return
                (not (equal (sort uniq-tool-names #'string-lessp)
                            (sort (copy-sequence (ensure-list val)) #'string-lessp)))))))
        ;; Generic case
        (_ (let ((field-val (plist-get preset-spec key)))
             (or (and (consp field-val) (keywordp (car field-val)))
                 (not (equal field-val val))))))))

;;;; Presets in-buffer UI
(defun gptel--transform-apply-preset (_fsm)
  "Apply a gptel preset to the buffer depending on the prompt.

If the last user prompt includes @foo, the preset foo is applied.
Before applying the preset, \"@foo\" is removed from the prompt and
point is placed at its position."
  (when gptel--known-presets
    (text-property-search-backward 'gptel nil t)
    (while (re-search-forward "@\\([^[:space:]]+\\)\\_>" nil t)
      ;; The following convoluted check is because re-search is much faster if
      ;; the search pattern begins with a non-whitespace char.
      (when (or (= (match-beginning 0) (point-min))
                (memq (char-syntax (char-before (match-beginning 0))) '(32 62)))
        (when-let* ((name (match-string 1))
                    (preset (or (gptel-get-preset (intern-soft name))
                                (gptel-get-preset name))))
          (delete-region (match-beginning 0) (match-end 0))
          ;; Point must be after @foo when the preset is applied to allow for
          ;; more advanced transformations.
          (gptel--apply-preset preset
                               (lambda (sym val)
                                 (set (make-local-variable sym) val))))))))

;; ;; Alternative approach with string search
;; (search-forward "@" nil t)
;; (if (and (memq (char-syntax (char-before (1- (point)))) '(32 62))
;;          (looking-at "\\([^[:blank:]]+?\\)[[:punct:]]?\\s-+"))
;;     do-stuff)

(defun gptel--fontify-preset-keyword (end)
  "Font-lock function for preset indicators in chat buffers.

Return preset fontification info for text up to END."
  (and (re-search-forward "@\\([^[:space:]]+\\)\\_>" end t)
       (or (= (match-beginning 0) (point-min))
           (memq (char-syntax (char-before (match-beginning 0))) '(32 62)))
       (not (plist-get (text-properties-at (match-beginning 1)) 'gptel))))

(defun gptel-preset-capf ()
  "Completion at point for gptel presets in `gptel-mode'.

Add this to `completion-at-point-functions'."
  (and gptel--known-presets
       (save-excursion
         (let ((num (- (skip-syntax-backward "w_"))))
           (when (eql (char-before) ?@)
             (list (point) (+ (point) num)
                   gptel--known-presets
                   :exclusive 'no
                   :annotation-function
                   #'(lambda (c) (thread-first
                              (intern-soft c)
                              (assq gptel--known-presets) (cdr)
                              (plist-get :description)))))))))

(defun gptel--prettify-preset ()
  "Get visual and completion help with presets in gptel buffers.

Intended to be added to `gptel-mode-hook'."
  (let ((keyword '((gptel--fontify-preset-keyword
                    ;; subexp 0 here is not required, we retain it to make it
                    ;; easy to swtich to more complex patterns in the future
                    0 (when-let* ((comps (all-completions (match-string 1)
                                          gptel--known-presets))
                                  ((member (match-string 1) comps)))
                       '(:box -1 :inherit secondary-selection))
                    prepend))))
    (cond
     (gptel-mode
      (font-lock-add-keywords nil keyword t)
      (add-hook 'completion-at-point-functions #'gptel-preset-capf nil t))
     (t (font-lock-remove-keywords nil keyword)
        (remove-hook 'completion-at-point-functions #'gptel-preset-capf t)))))


;;; Response tweaking commands

(defun gptel--attach-response-history (history &optional buf)
  "Attach HISTORY to the next gptel response in buffer BUF.

HISTORY is a list of strings typically containing text replaced
by gptel.  BUF is the current buffer if not specified.

This is used to maintain variants of prompts or responses to diff
against if required."
  (with-current-buffer (or buf (current-buffer))
    (letrec ((gptel--attach-after
              (lambda (b e)
                (when (and b e)
                  (add-text-properties
                   b e `(gptel-history
                         ,(append (ensure-list history)
                           (get-char-property (1- e) 'gptel-history))
                         front-sticky (gptel gptel-history))))
                (remove-hook 'gptel-post-response-functions
                             gptel--attach-after 'local))))
      (add-hook 'gptel-post-response-functions gptel--attach-after
                nil 'local))))

(defun gptel--ediff (&optional arg bounds-func)
  "Ediff response at point against previous gptel responses.

If prefix ARG is non-nil, select the previous response to ediff
against interactively.

If specified, use BOUNDS-FUNC to compute the bounds of the
response at point.  This can be used to include additional
context for the ediff session."
  (interactive "P")
  (when (gptel--at-response-history-p)
    (pcase-let* ((`(,beg . ,end) (funcall (or bounds-func #'gptel--get-response-bounds)))
                 (prev-response
                  (if arg
                      (completing-read "Choose response variant to diff against: "
                                       (get-char-property (point) 'gptel-history)
                                       nil t)
                    (car-safe (get-char-property (point) 'gptel-history))))
                 (buffer-mode major-mode)
                 (bufname (buffer-name))
                 (`(,new-buf ,new-beg ,new-end)
                  (with-current-buffer
                      (get-buffer-create (concat bufname "-PREVIOUS-*"))
                    (let ((inhibit-read-only t))
                      (erase-buffer)
                      (delay-mode-hooks (funcall buffer-mode))
                      (visual-line-mode)
                      (insert prev-response)
                      (goto-char (point-min))
                      (list (current-buffer) (point-min) (point-max))))))
      (unless prev-response (user-error "gptel response is additive: no changes to ediff"))
      (require 'ediff)
      (letrec ((cwc (current-window-configuration))
               (gptel--ediff-restore
                (lambda ()
                  (when (window-configuration-p cwc)
                    (set-window-configuration cwc))
                  (kill-buffer (get-buffer (concat bufname "-PREVIOUS-*")))
                  (kill-buffer (get-buffer (concat bufname "-CURRENT-*")))
                  (remove-hook 'ediff-quit-hook gptel--ediff-restore))))
        (add-hook 'ediff-quit-hook gptel--ediff-restore)
        (apply
         #'ediff-regions-internal
         (get-buffer (ediff-make-cloned-buffer (current-buffer) "-CURRENT-*"))
         beg end new-buf new-beg new-end
         nil
         (list 'ediff-regions-wordwise 'word-wise nil)
         ;; (if (transient-arg-value "-w" args)
         ;;     (list 'ediff-regions-wordwise 'word-wise nil)
         ;;   (list 'ediff-regions-linewise nil nil))
         )))))

(defun gptel--mark-response ()
  "Mark gptel response at point, if any."
  (interactive)
  (unless (gptel--in-response-p) (user-error "No gptel response at point"))
  (pcase-let ((`(,beg . ,end) (gptel--get-response-bounds)))
    (goto-char beg) (push-mark) (goto-char end) (activate-mark)))

(defun gptel--previous-variant (&optional arg)
  "Switch to ARG previous gptel-response at this point, if it exists."
  (interactive "p")
  (pcase-let* ((`(,beg . ,end) (gptel--get-response-bounds))
               (history (get-char-property (point) 'gptel-history))
               (alt-response (car-safe history))
               (offset))
    (unless (and history alt-response)
      (user-error "No variant responses available"))
    (if (> arg 0)
        (setq history (append (cdr history)
                              (list (buffer-substring-no-properties beg end))))
      (setq
       alt-response (car (last history))
       history (cons (buffer-substring-no-properties beg end)
                     (nbutlast history))))
    (add-text-properties
             0 (length alt-response)
             `(gptel response gptel-history ,history)
             alt-response)
    (setq offset (min (- (point) beg) (1- (length alt-response))))
    (delete-region beg end)
    (insert alt-response)
    (goto-char (+ beg offset))
    (pulse-momentary-highlight-region beg (+ beg (length alt-response)))))

(defun gptel--next-variant (&optional arg)
  "Switch to ARG next gptel-response at this point, if it exists."
  (interactive "p")
  (gptel--previous-variant (- arg)))

(provide 'gptel)
;;; gptel.el ends here

;; Local Variables:
;; bug-reference-url-format: "https://github.com/karthink/gptel/issues/%s"
;; End:
