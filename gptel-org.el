;;; gptel-org.el --- Org functions for gptel         -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Keywords:

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

;;; Commentary:

;;

;;; Code:
(require 'cl-lib)
(require 'org-element)
(require 'outline)
(require 'mailcap)                    ;FIXME Avoid this somehow
(require 'org-src)
(eval-when-compile (require 'gptel-request))

;; Load gptel-org-agent when subtree context is enabled.
;; Use eval-after-load to handle the case where gptel-org-subtree-context
;; is set after gptel-org.el is loaded (the defcustom lives in
;; gptel-org.el itself).
(require 'gptel-org-agent nil t)
(require 'gptel-indirect-buffer nil t)

;; NOTE: gptel-tool and gptel-reasoning src block language registrations
;; have been removed.  Tool results and reasoning now use TOOL and
;; REASONING org headings instead of src blocks.

;; Functions used for saving/restoring gptel state in Org buffers
(defvar gptel--num-messages-to-send)
(defvar org-entry-property-inherited-from)
(defvar gptel-backend)
(defvar gptel--known-backends)
(defvar gptel--system-message)
(defvar gptel-model)
(defvar gptel-temperature)
(defvar gptel-max-tokens)
(defvar gptel--link-type-cache)
(defvar gptel--preset)

;; Recursive guards for advice functions
(defvar gptel-org--in-send-with-props nil
  "Non-nil when inside `gptel-org--send-with-props' to prevent recursion.")
(defvar gptel-org--in-prefix-advice nil
  "Non-nil when inside prefix advice functions to prevent recursion.")

(defvar-local gptel-org--org-format-response nil
  "Non-nil when the AI was instructed to respond in org format.

When set, the markdown-to-org converter is skipped since the
response is already in org format.  A lightweight post-response
sanitizer runs instead to fix common AI formatting mistakes.")

(defvar-local gptel-org--ref-level nil
  "Reference heading level for auto-correction in agent indirect buffers.
Set once when the indirect buffer is created.  This is the level at
which the AI's top-level headings (level 1) should appear — computed
as one more than the agent heading level at `point-min'.

The auto-corrector uses this to rebase any heading with fewer stars
than ref-level by adding an offset of (ref-level - 1).  Headings
already at or above ref-level are left untouched, making the
correction idempotent.")


(defvar-local gptel-org--base-ref-level nil
  "Original `gptel-org--ref-level' before TodoWrite sub-task redirect.
Saved by `gptel-org-agent--redirect-markers-to-heading' when it updates
`gptel-org--ref-level' for a sub-task.  Restored by
`gptel-org-agent--write-todo-org' when no sub-task is in_progress.")
(defvar-local gptel-org--auto-correcting nil
  "Non-nil while the auto-corrector is modifying the buffer.
Used as a re-entrancy guard to prevent the auto-corrector from
being triggered by the cache reset that follows its own edits.")

(defvar-local gptel-org--agent-indirect-buffer-p nil
  "Non-nil when this buffer is an agent indirect buffer.
Set by `gptel-org-agent--open-indirect-buffer'.  Unlike checking
the agent tag on the heading, this flag persists even after the
tag is removed (e.g. by `gptel-org-agent--insert-user-heading').")

(defvar-local gptel-org--reasoning-indirect-buffer nil
  "The indirect buffer currently showing reasoning content, if any.
Used to track and clean up the reasoning display buffer.")

;; Debug support
(defvar gptel-org-debug nil
  "When non-nil, output debug messages for subtree context operations.
Set to t to enable debug output to *Messages* buffer.
Useful for diagnosing heading level issues.")

(defun gptel-org--debug (format-string &rest args)
  "Output debug message for gptel-org operations.

When `gptel-org-debug' is non-nil, output to *Messages* buffer.
When `gptel-log-level' is \\='debug, also log to the structured
*gptel-log* buffer as a sub-entry under the current request.

FORMAT-STRING and ARGS are passed to `format'."
  (let ((msg (apply #'format format-string args)))
    (when gptel-org-debug
      (message "[gptel-org] %s" msg))
    (when (eq gptel-log-level 'debug)
      (gptel--log msg "gptel-org" 'no-json))))

(defvar org-link-angle-re)
(defvar org-link-bracket-re)
(declare-function mailcap-file-name-to-mime-type "mailcap")
(declare-function gptel--log "gptel-request")
(declare-function gptel--model-capable-p "gptel-request")
(declare-function gptel--model-mime-capable-p "gptel-request")
(declare-function gptel--model-name "gptel-request")
(declare-function gptel--to-string "gptel-request")
(declare-function gptel--to-number "gptel-request")
(declare-function gptel--intern "gptel-request")
(declare-function gptel-backend-name "gptel-request")
(declare-function gptel--parse-buffer "gptel-request")
(declare-function gptel--parse-directive "gptel-request")
(declare-function gptel--with-buffer-copy "gptel-request")
(declare-function gptel--file-binary-p "gptel-request")
(declare-function gptel--get-buffer-bounds "gptel")
(declare-function gptel--restore-props "gptel")
(declare-function org-entry-get "org")
(declare-function org-entry-put "org")
(declare-function org-entry-delete "org")
(declare-function org-with-wide-buffer "org-macs")
(declare-function org-set-property "org")
(declare-function org-property-values "org")
(declare-function org-open-line "org")
(declare-function org-at-heading-p "org")
(declare-function org-get-heading "org")
(declare-function org-get-tags "org")
(declare-function org-end-of-subtree "org")
(declare-function gptel-org-ib-create "gptel-indirect-buffer")
(declare-function gptel-org-ib-close "gptel-indirect-buffer")

;; Bundle `org-element-lineage-map' if it's not available (for Org 9.67 or older)
(eval-and-compile
  (if (fboundp 'org-element-lineage-map)
      (progn (declare-function org-element-lineage-map "org-element-ast")
             (defalias 'gptel-org--element-lineage-map 'org-element-lineage-map))
    (defun gptel-org--element-lineage-map (datum fun &optional types with-self first-match)
      "Map FUN across ancestors of DATUM, from closest to furthest.

DATUM is an object or element.  For TYPES, WITH-SELF and
FIRST-MATCH see `org-element-lineage-map'.

This function is provided for compatibility with older versions
of Org."
      (declare (indent 2))
      (setq fun (if (functionp fun) fun `(lambda (node) ,fun)))
      (let ((up (if with-self datum (org-element-parent datum)))
	    acc rtn)
        (catch :--first-match
          (while up
            (when (or (not types) (org-element-type-p up types))
              (setq rtn (funcall fun up))
              (if (and first-match rtn)
                  (throw :--first-match rtn)
                (when rtn (push rtn acc))))
            (setq up (org-element-parent up)))
          (nreverse acc)))))
  (if (fboundp 'org-element-begin)
      (progn (declare-function org-element-begin "org-element")
             (declare-function org-element-end "org-element")
             (declare-function org-element-parent "org-element")
             (defalias 'gptel-org--element-begin 'org-element-begin)
             (defalias 'gptel-org--element-end 'org-element-end)
             (defalias 'gptel-org--element-parent 'org-element-parent))
    (defsubst gptel-org--element-begin (node)
      "Get `:begin' property of NODE."
      (org-element-property :begin node))
    (defsubst gptel-org--element-end (node)
      "Get `:end' property of NODE."
      (org-element-property :end node))
    (defsubst gptel-org--element-parent (node)
      "Return `:parent' property of NODE."
      (org-element-property :parent node))))


;;; User options
(defcustom gptel-org-branching-context nil
  "Use the lineage of the current heading as the context for gptel in Org buffers.

This makes each same level heading a separate conversation
branch.

By default, gptel uses a linear context: all the text up to the
cursor is sent to the LLM.  Enabling this option makes the
context the hierarchical lineage of the current Org heading.  In
this example:

-----
Top level text

* Heading 1
heading 1 text

* Heading 2
heading 2 text

** Heading 2.1
heading 2.1 text
** Heading 2.2
heading 2.2 text
-----

With the cursor at the end of the buffer, the text sent to the
LLM will be limited to

-----
Top level text

* Heading 2
heading 2 text

** Heading 2.2
heading 2.2 text
-----

This makes it feasible to have multiple conversation branches."
  :type 'boolean
  :group 'gptel)

(defcustom gptel-org-ignore-elements '(property-drawer)
  "Types of Org elements to be stripped from the prompt before sending.

By default gptel will remove Org property drawers from the
prompt.  For the full list of available elements, please see
`org-element-all-elements'.

Please note: Removing property-drawer elements is fast, but
adding elements to this list can significantly slow down
`gptel-send'."
  :group 'gptel
  :type '(repeat symbol))

(defcustom gptel-org-validate-link #'always
  "Validate links to be sent as context with gptel queries.

When `gptel-track-media' is enabled, this option determines if a
supported link will be followed and its source included with gptel
queries from Org buffers.  Currently only \"file\" and \"attachment\"
link types are supported (along with web URLs if the model supports
them).

It should be a function that accepts an Org link object and return
non-nil if the link should be followed.

By default, all links are considered valid.

Set this to `gptel-org--link-standalone-p' to only follow links placed
on a line by themselves, separated from surrounding text."
  :group 'gptel
  :type '(choice
          (const :tag "All links" always)
          (const :tag "Standalone links" gptel-org--link-standalone-p)
          (function :tag "Function")))

(defconst gptel-org--link-regex
  (concat "\\(?:" org-link-bracket-re "\\|" org-link-angle-re "\\)")
  "Link regex for `gptel-mode' in Org mode.")

(defcustom gptel-org-subtree-context nil
  "Use indirect buffer mode for subtree-scoped conversations.

When non-nil, gptel will create an indirect buffer narrowed to the
current subtree for each conversation.  This keeps conversation
context scoped to the subtree and prevents responses from escaping
into sibling headings.

The indirect buffer approach provides proper isolation: each
conversation runs in its own narrowed view of the subtree, with
heading levels automatically adjusted relative to the subtree root.

This is used by the agent system (`gptel-org-agent') to manage
task-oriented workflows where conversations happen under TODO
headings."
  :type 'boolean
  :group 'gptel)

(defcustom gptel-org-save-state t
  "Whether to save gptel state as Org properties when saving the buffer.

When non-nil (the default), gptel will save model, backend, system
message, and other state as properties at the top of Org buffers
when they are saved.

Set to nil to disable automatic property saving for Org files.
This can be useful when you don't want gptel to modify your Org
files with GPTEL_* properties."
  :type 'boolean
  :group 'gptel)

;; Reasoning blocks in org-mode always use indirect buffers.
;; A side window shows reasoning content live during streaming,
;; and is automatically closed when the response completes.

(defcustom gptel-org-infer-bounds-from-tags t
  "Infer assistant/user message bounds from org heading tags.

When non-nil (the default), gptel will scan headings for :assistant:
and :user: tags to determine message roles, instead of relying on
the GPTEL_BOUNDS property.

This allows marking assistant and user parts of a conversation
using standard org tags on headings:

  ** AI-DOING Do something
  *** Title for assistant work                    :assistant:
  - Assistant talking
  *** User feedback                               :user:
  - User talking

The tags are case-insensitive.  When this option is enabled and
tagged headings are found, GPTEL_BOUNDS will not be written on save."
  :type 'boolean
  :group 'gptel)

(defcustom gptel-org-assistant-tag "assistant"
  "Tag name used to mark assistant message headings.

Headings with this tag will be treated as assistant responses.
The tag comparison is case-insensitive."
  :type 'string
  :group 'gptel)

(defcustom gptel-org-user-tag "user"
  "Tag name used to mark user message headings.

Headings with this tag will be treated as user messages.
The tag comparison is case-insensitive."
  :type 'string
  :group 'gptel)

(defcustom gptel-org-assistant-keyword "AI"
  "TODO keyword used to mark assistant message headings.

Headings with this TODO keyword will be treated as assistant responses.
When `gptel-org-use-todo-keywords' is enabled, this keyword replaces
the tag-based assistant detection."
  :type 'string
  :group 'gptel)

(defcustom gptel-org-user-keyword "FEEDBACK"
  "TODO keyword used to mark user message headings.

Headings with this TODO keyword will be treated as user messages
and serve as the prompt location for user feedback in the iterative
AI task cycle.  When `gptel-org-use-todo-keywords' is enabled, this
keyword replaces the tag-based user detection.

In keyword mode, FEEDBACK headings are created as siblings of agent
subtrees after each AI response cycle.  Sending from a FEEDBACK
heading mutates it into an AI-DOING agent subtree."
  :type 'string
  :group 'gptel)

(defcustom gptel-org-use-todo-keywords nil
  "When non-nil, use TODO keywords instead of tags for role detection.

When enabled, `gptel-org-assistant-keyword' (default \"AI\") and
`gptel-org-user-keyword' (default \"FEEDBACK\") TODO keywords are
used to mark assistant and user headings, replacing the tag-based
`:assistant:' and `:user:' approach.

The AI/FEEDBACK keywords must be registered in the org TODO keyword
sequence for proper fontification and cycling.  See
`gptel-org--register-todo-keywords'."
  :type 'boolean
  :group 'gptel)

(defcustom gptel-org-response-title-function
  #'gptel-org-response-title-from-first-line
  "Function to generate a title for assistant response headings.

When non-nil, this function is called after the response is complete
to generate a title for the response heading.  The function receives
three arguments:
- BEG: Start position of the response
- END: End position of the response
- HEADING-POS: Position of the response heading

The function should return a string to use as the heading title,
or nil to keep the heading without a title.

The heading is updated in place, preserving any existing tags.

Built-in options:
- `gptel-org-response-title-from-first-line': Use first line (default)

Example to generate title via LLM (requires separate request):

  (setq gptel-org-response-title-function
        (lambda (beg end heading-pos)
          (gptel-request
           (format \"Summarize in 5 words: %s\"
                   (buffer-substring-no-properties beg (min end (+ beg 500))))
           :callback (lambda (title _info)
                       (when title
                         (gptel-org--set-heading-title
                          heading-pos (string-trim title)))))
          nil))"
  :type '(choice (const :tag "No title" nil)
                 (const :tag "First line of response"
                        gptel-org-response-title-from-first-line)
                 (function :tag "Custom function"))
  :group 'gptel)

(defcustom gptel-org-model-from-user-tag t
  "When non-nil, detect model from tags on user headings.

When enabled, gptel will check the current user heading for tags
that match model aliases (like :haiku:, :sonnet:, :opus:) or model
names defined in any backend.  If found, the matching model will
be used for the request.

For example, with this heading:
  ** :user:haiku: My question here

The request will use the \\=`haiku\\=' model alias (Claude Haiku).

This only affects the current request and does not change the
buffer-local model setting."
  :type 'boolean
  :group 'gptel)

(defcustom gptel-org-model-from-todo-tag t
  "When non-nil, detect model from tags on TODO headings.

When enabled, gptel will check the current heading for TODO keywords
\(as defined in `gptel-org-todo-keywords') and if found, will search
the heading's tags for model aliases or model names.

For example, with this heading:
  ** AI-DO Implement feature :haiku:

The request will use the \\=`haiku\\=' model alias (Claude Haiku).

This allows using model tags on task headings without requiring
the :user: tag."
  :type 'boolean
  :group 'gptel)

(defcustom gptel-org-todo-keywords '("AI-DO" "AI-DOING")
  "TODO keywords that indicate AI task headings.

Headings with these TODO keywords will have their tags checked for
model specifications when `gptel-org-model-from-todo-tag' is enabled.

The check is case-sensitive to match org-mode's TODO keyword handling."
  :type '(repeat string)
  :group 'gptel)


;;; Subtree context helper functions

(defun gptel-org--compute-response-level ()
  "Compute the heading level for AI response content.
Returns the level at which the AI's top-level headings should appear."
  (when (gptel-org--in-agent-indirect-buffer-p)
    (save-excursion
      (goto-char (point-min))
      (when (org-at-heading-p)
        (1+ (org-current-level))))))

(defun gptel-org--chat-heading-p (&optional heading-text)
  "Check if HEADING-TEXT (or current heading) is a chat entry.

Returns non-nil if the heading has role tags/keywords
\(assistant/user or AI/HI).  When HEADING-TEXT is provided, only
keyword-based and tag-based detection is performed (requires point
to be at the heading)."
  (ignore heading-text)
  (let* ((keyword-match
          (and gptel-org-use-todo-keywords
               (org-at-heading-p)
               (or (gptel-org--heading-is-assistant-p)
                   (gptel-org--heading-is-user-p))))
         (tag-match
          (and (not gptel-org-use-todo-keywords)
               gptel-org-infer-bounds-from-tags
               (org-at-heading-p)
               (or (gptel-org--heading-has-tag-p gptel-org-assistant-tag)
                   (gptel-org--heading-has-tag-p gptel-org-user-tag))))
         (result (or keyword-match tag-match)))
    (gptel-org--debug "chat-heading-p: keyword-match=%s tag-match=%s result=%s"
                      keyword-match tag-match result)
    result))

(defvar-local gptel-org--pending-heading-tag nil
  "Tag to apply to the heading after prefix insertion.
A cons cell (MARKER . TAG) where MARKER is a marker and TAG is the
tag name to apply using `org-set-tags'.")

(defun gptel-org--apply-pending-tag-on-change (beg end _len)
  "Apply pending heading tag after insertion between BEG and END.
This is called from `after-change-functions' to apply tags using
`org-set-tags' after a heading prefix has been inserted.

When `insert' is called with multiple arguments (e.g., a separator
and a heading prefix), Emacs fires `after-change-functions' separately
for each argument.  We must only apply the tag when the heading is
actually present in the changed region [BEG, END), not when an
unrelated change (like a separator) triggers the hook."
  (when gptel-org--pending-heading-tag
    (let ((marker (car gptel-org--pending-heading-tag))
          (tag (cdr gptel-org--pending-heading-tag)))
      ;; Only consider changes near where we expect the heading
      (when (and (markerp marker)
                 (marker-buffer marker)
                 (<= (abs (- beg (marker-position marker))) 50))
        (save-excursion
          (goto-char beg)
          (let ((found-heading
                 (cond
                  ;; If beg is on a heading line AND beg is at the start
                  ;; of the line, this heading was just inserted (the
                  ;; insert placed us at the beginning of a new heading).
                  ;; If beg is NOT at the start of a heading line, we're
                  ;; on a pre-existing heading (e.g., the task heading)
                  ;; and should not apply the tag to it.
                  ((org-at-heading-p)
                   (= beg (line-beginning-position)))
                  ;; Search only within the changed region.  This prevents
                  ;; matching a pre-existing sibling heading when the change
                  ;; is just a separator (e.g., "\n\n") inserted before the
                  ;; heading prefix.
                  (t (and (re-search-forward org-heading-regexp end t)
                          (progn (beginning-of-line) t))))))
            (when (and found-heading (org-at-heading-p))
              (gptel-org--debug
               "apply-pending-tag-on-change: beg=%d end=%d marker=%d tag=%S heading=%d"
               beg end (marker-position marker) tag (point))
              ;; Clean up BEFORE calling org-set-tags to prevent re-entrancy.
              ;; org-set-tags modifies the buffer, which triggers
              ;; after-change-functions again.  If we don't clear the pending
              ;; state first, the re-entrant call finds the tag still pending
              ;; and may apply it to the wrong heading.
              (set-marker marker nil)
              (setq gptel-org--pending-heading-tag nil)
              (remove-hook 'after-change-functions
                           #'gptel-org--apply-pending-tag-on-change t)
              (org-set-tags (list tag)))))))))

(defun gptel-org--dynamic-prefix-string (base-prefix &optional _for-prompt)
  "Return BASE-PREFIX adjusted for current org heading context.

In agent indirect buffers, return an empty string since the agent
system manages its own heading structure for both response and
user headings.  Otherwise, return BASE-PREFIX unchanged."
  (if (gptel-org--in-agent-indirect-buffer-p)
      ;; Agent indirect buffers manage their own heading structure.
      ;; Return empty string to suppress prefix insertion entirely —
      ;; the agent system handles both response and user headings.
      ""
    base-prefix))

;;; Setting context and creating queries
(defun gptel-org--get-topic-start ()
  "If a conversation topic is set, return it."
  (when (org-entry-get (point) "GPTEL_TOPIC" 'inherit)
    (marker-position org-entry-property-inherited-from)))

(defun gptel-org-set-topic (topic)
  "Set a TOPIC and limit this conversation to the current heading.

This limits the context sent to the LLM to the text between the current
heading (i.e. the heading with the topic set) and the cursor position."
  (interactive
   (list
    (progn
      (or (derived-mode-p 'org-mode)
          (user-error "Support for multiple topics per buffer is only implemented for `org-mode'"))
      (completing-read "Set topic as: "
                       (org-property-values "GPTEL_TOPIC")
                       nil nil (downcase
                                (truncate-string-to-width
                                 (substring-no-properties
                                  (replace-regexp-in-string
                                   "\\s-+" "-"
                                   (org-entry-get nil "ITEM")))
                                 50))))))
  (when (stringp topic) (org-set-property "GPTEL_TOPIC" topic)))

;; NOTE: This can be converted to a cl-defmethod for
;; `gptel--create-prompt-buffer' (conceptually cleaner), but will cause
;; load-order issues in gptel.el and might be harder to debug.
(defun gptel-org--create-prompt-buffer (&optional prompt-end)
  "Return a buffer with the conversation prompt to be sent.

If the region is active limit the prompt text to the region contents.
Otherwise the prompt text is constructed from the contents of the
current buffer up to point, or PROMPT-END if provided.  Its contents
depend on the value of `gptel-org-branching-context', which see.

When `gptel-org-subtree-context' is enabled, conversations run in
indirect buffers narrowed to the relevant subtree."
  ;; Refresh bounds from tags/keywords before constructing prompt to ensure
  ;; text properties reflect current buffer state (avoids stale markers)
  (when (or gptel-org-infer-bounds-from-tags gptel-org-use-todo-keywords)
    (gptel-org--restore-bounds-from-tags))
  (when (use-region-p)
    (narrow-to-region (region-beginning) (region-end))
    (setq prompt-end (point-max)))
  (goto-char (or prompt-end (setq prompt-end (point))))
  (let ((topic-start (gptel-org--get-topic-start)))
    (when topic-start
      ;; narrow to GPTEL_TOPIC property scope
      (narrow-to-region topic-start prompt-end))
    (if (and gptel-org-branching-context
             (or (fboundp 'org-element-lineage-map)
                 (prog1 nil
                   (display-warning
                    '(gptel org)
                    "Using `gptel-org-branching-context' requires Org version 9.7 or higher, it will be ignored."))))
        ;; Create prompt from direct ancestors of point
        (save-excursion
          (let* ((org-buf (current-buffer))
                 ;; Collect all heading start positions in the lineage
                 (full-bounds (gptel-org--element-lineage-map
                                  (org-element-at-point) #'gptel-org--element-begin
                                '(headline) 'with-self) )
                 ;; lineage-map returns the full lineage in the unnarrowed
                 ;; buffer.  Remove heading start positions before (point-min)
                 ;; that are invalid due to narrowing, and add (point-min) if
                 ;; it's not already included in the lineage
                 (start-bounds
                  (nconc (cl-delete-if (lambda (p) (< p (point-min)))
                                       full-bounds)
                         (unless (save-excursion (goto-char (point-min))
                                                 (looking-at-p outline-regexp))
                           (list (point-min)))))
                 (end-bounds
                  (cl-loop
                   ;; For each ancestor, find the end of context to include.
                   ;; Start from the first sub-heading (branching context
                   ;; boundary), then extend forward to include any
                   ;; contiguous assistant-response sibling headings that
                   ;; precede the lineage child.  This preserves the
                   ;; conversation history (user→assistant→user flow) while
                   ;; still excluding unrelated sibling branches.
                   for child-start in start-bounds
                   for pos in (cdr start-bounds)
                   collect (save-excursion
                             (goto-char pos)
                             (if (outline-next-heading)
                                 ;; Scan forward past assistant siblings
                                 ;; that appear before the lineage child
                                 (let ((end (point)))
                                   (while (and (< (point) child-start)
                                               (get-text-property
                                                (point) 'gptel))
                                     (setq end
                                           (save-excursion
                                             (org-end-of-subtree t t)
                                             (point)))
                                     (goto-char end)
                                     ;; Skip whitespace to next heading
                                     (unless (outline-next-heading)
                                       (goto-char (point-max))))
                                   (min end child-start))
                               child-start))
                   into ends
                   finally return (cons prompt-end ends)))
                 ;; Save lineage depth before hybrid context may truncate
                 ;; start-bounds — needed for lineage position collection
                 ;; during agent subtree stripping.
                 (lineage-depth (length start-bounds)))
            ;; Hybrid context: when gptel-org-subtree-context is enabled
            ;; and a TODO heading exists in the lineage, include the full
            ;; TODO subtree content (non-branching) up to prompt-end,
            ;; while keeping branching context for ancestors above it.
            ;; This ensures sibling sub-headings within the TODO task
            ;; are included in the context.
            (when gptel-org-subtree-context
              (gptel-org--debug "hybrid-context: start-bounds=%S end-bounds=%S prompt-end=%d"
                                start-bounds end-bounds prompt-end)
              (when-let* ((todo-idx
                           (gptel-org--find-todo-in-lineage start-bounds)))
                (gptel-org--debug "hybrid-context: found TODO at index %d, pos %d"
                                  todo-idx (nth todo-idx start-bounds))
                (when (> todo-idx 0)
                  (setq start-bounds (nthcdr todo-idx start-bounds))
                  (setq end-bounds (nthcdr todo-idx end-bounds))
                  ;; TODO subtree: include full content up to cursor
                  (setcar end-bounds prompt-end)
                  ;; Ancestors above TODO: restrict to heading line only
                  ;; (branching context = heading lines, no body text)
                  (cl-loop for tail on (cdr start-bounds)
                           for etail on (cdr end-bounds)
                           do (setcar etail
                                      (save-excursion
                                        (goto-char (car tail))
                                        (line-end-position))))
                  (gptel-org--debug "hybrid-context: after truncation start-bounds=%S end-bounds=%S"
                                    start-bounds end-bounds))))
            (gptel--with-buffer-copy org-buf nil nil
              (cl-loop for start in start-bounds
                       for end in end-bounds
                       do (insert-buffer-substring org-buf start end)
                       (goto-char (point-min)))
              (goto-char (point-max))
              (gptel-org--unescape-tool-results)
              (gptel-org--strip-block-headers)
              ;; Strip @agent subtrees unless include-agent-subtrees is set.
              ;; In branching context, protect lineage headings — they are
              ;; structural ancestors of the current conversation and their
              ;; subtrees contain the chat messages being sent.
              (when (and gptel-org-subtree-context
                         (not (and (boundp 'gptel-org-agent-include-subtrees)
                                   (buffer-local-value
                                    'gptel-org-agent-include-subtrees org-buf)))
                         (fboundp 'gptel-org-agent--strip-agent-subtrees))
                (let (lineage-positions)
                  (save-excursion
                    (goto-char (point-min))
                    (dotimes (_ lineage-depth)
                      (when (org-at-heading-p)
                        (push (point) lineage-positions))
                      (unless (eobp) (outline-next-heading))))
                  (gptel-org-agent--strip-agent-subtrees lineage-positions)))
              (when-let* ((gptel-org-ignore-elements ;not copied by -with-buffer-copy
                           (buffer-local-value 'gptel-org-ignore-elements
                                               org-buf)))
                (gptel-org--strip-elements))
              (setq org-complex-heading-regexp ;For org-element-context to run
                    (buffer-local-value 'org-complex-heading-regexp org-buf))
              (current-buffer))))
      ;; Create prompt the usual way (non-branching context)
      (let ((org-buf (current-buffer))
            (beg (point-min)))
        (gptel--with-buffer-copy org-buf beg prompt-end
          (goto-char (point-max))
          (gptel-org--unescape-tool-results)
          (gptel-org--strip-block-headers)
          ;; Strip @agent subtrees unless include-agent-subtrees is set
          (when (and gptel-org-subtree-context
                     (not (and (boundp 'gptel-org-agent-include-subtrees)
                               (buffer-local-value
                                'gptel-org-agent-include-subtrees org-buf)))
                     (fboundp 'gptel-org-agent--strip-agent-subtrees))
            (gptel-org-agent--strip-agent-subtrees))
          (when-let* ((gptel-org-ignore-elements ;not copied by -with-buffer-copy
                       (buffer-local-value 'gptel-org-ignore-elements
                                           org-buf)))
                (gptel-org--strip-elements))
          (setq org-complex-heading-regexp ;For org-element-context to run
                (buffer-local-value 'org-complex-heading-regexp org-buf))
          (current-buffer))))))

(defun gptel-org--strip-elements ()
  "Remove all elements in `gptel-org-ignore-elements' from the prompt."
  (let ((major-mode 'org-mode) element-markers)
    (if (equal '(property-drawer) gptel-org-ignore-elements)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward org-property-drawer-re nil t)
            ;; ;; Slower but accurate
            ;; (let ((drawer (org-element-at-point)))
            ;;   (when (org-element-type-p drawer 'property-drawer)
            ;;     (delete-region (org-element-begin drawer) (org-element-end drawer))))

            ;; Fast but inexact, can have false positives
            (delete-region (match-beginning 0) (match-end 0))))
      ;; NOTE: Parsing the buffer is extremely slow.  Avoid this path unless
      ;; required.
      ;; NOTE: `org-element-map' takes a third KEEP-DEFERRED argument in newer
      ;; Org versions
      (org-element-map (org-element-parse-buffer 'element nil)
          gptel-org-ignore-elements
        (lambda (node)
          (push (list (gptel-org--element-begin node)
                      (gptel-org--element-end node))
                element-markers)))
      (dolist (bounds element-markers)
        (apply #'delete-region bounds)))))

(defun gptel-org--strip-block-headers ()
  "Remove gptel-specific block headers, footers, and special subtrees.
Every matching line or subtree is removed entirely.

REASONING subtrees are stripped completely since reasoning content
should not be sent back to the LLM.  TOOL heading lines are stripped
but their body text (plist + result) is kept since the LLM needs to
see tool return values.  Tool block headers/footers are stripped to
avoid auto-mimicry.

This removal is necessary to avoid auto-mimicry by LLMs."
  (save-excursion
    ;; First pass: strip REASONING heading subtrees entirely
    (goto-char (point-min))
    (while (re-search-forward "^\\(\\*+\\) REASONING " nil t)
      (let* ((stars (length (match-string 1)))
             (subtree-start (match-beginning 0))
             (subtree-end
              (save-excursion
                ;; Find the end of this subtree: next heading at same
                ;; or higher level, or end of buffer
                (forward-line 1)
                (if (re-search-forward
                     (format "^\\*\\{1,%d\\} " stars) nil t)
                    (match-beginning 0)
                  (point-max)))))
        (delete-region subtree-start subtree-end)))
    ;; Second pass: strip TOOL heading lines only (keep body text
    ;; with plist and result since the LLM needs tool return values)
    (goto-char (point-min))
    (while (re-search-forward "^\\*+ TOOL " nil t)
      (delete-region (match-beginning 0)
                     (min (point-max) (1+ (line-end-position)))))
    ;; Third pass: strip remaining block headers/footers (custom tool/reasoning
    ;; blocks, and legacy gptel-reasoning/gptel-tool src blocks)
    (goto-char (point-min))
    (while (re-search-forward
            (rx line-start
                (literal "#+")
                (or (seq (or (literal "begin") (literal "end"))
                         (or (literal "_tool") (literal "_reasoning")))
                    (literal "begin_src gptel-reasoning")
                    (literal "begin_src gptel-tool")))
            nil t)
      (delete-region (match-beginning 0)
                     (min (point-max) (1+ (line-end-position)))))))

(defun gptel-org--unescape-tool-results ()
  "Undo escapes done to keep results from escaping blocks.
Scans backward for gptel tool text property, then unescapes the block
contents."
  (save-excursion
    (goto-char (point-max))
    (let ((prev-pt (point)))
      (while (> prev-pt (point-min))
        (goto-char
         (previous-single-char-property-change (point) 'gptel))
        (let ((prop (get-text-property (point) 'gptel))
              (backward-progress (point)))
          (when (eq (car-safe prop) 'tool)
            ;; User edits to clean up can potentially insert a tool-call header
            ;; that is propertized.  Tool call headers should not be
            ;; propertized.
            (when (looking-at-p "[[:space:]]*#\\+begin\\(_src gptel-\\|_\\)tool")
              (goto-char (match-end 0)))
            ;; TODO this code is able to put the point behind prev-pt, which
            ;; makes the region inverted.  The `max' catches this, but really
            ;; `read' and `looking-at' are the culprits.  Badly formed tool
            ;; blocks can lead to this being necessary.
            (org-unescape-code-in-region
             (min prev-pt (point)) prev-pt))
          (goto-char (setq prev-pt backward-progress)))))))

(defun gptel-org--link-standalone-p (object)
  "Check if link OBJECT is on a line by itself."
  (when-let* ((par (gptel-org--element-parent object))
              ((eq (org-element-type par) 'paragraph)))
    (and (= (gptel-org--element-begin object)
            (save-excursion
              (goto-char (org-element-property :contents-begin par))
              (skip-chars-forward "\t ")
              (point)))                 ;account for leading space before object
         (<= (- (org-element-property :contents-end par)
                (org-element-property :end object))
             1))))

(defsubst gptel-org--validate-link (link)
  "Validate an Org LINK as sendable under the current gptel settings.

Return a form (validp link-type path . REST), where REST is a list
explaining why sending the link is not supported by gptel.  Only the
first nil value in REST is guaranteed to be correct."
  (let ((mime))
    (if-let* ((link-type (org-element-property :type link))
              (resource-type
               (or (and (member link-type '("attachment" "file")) 'file)
                   (and (gptel--model-capable-p 'url)
                        (member link-type '("http" "https" "ftp")) 'url)))
              (path (org-element-property :path link))
              (user-check (funcall gptel-org-validate-link link))
              (readablep (or (eq resource-type 'url) (file-remote-p path)
                             (file-readable-p path)))
              (mime-valid
               (if (or (eq resource-type 'url)
                       (cdr (with-memoization
                                (alist-get (expand-file-name path)
                                           gptel--link-type-cache
                                           nil nil #'string=)
                              (cons t (gptel--file-binary-p path)))))
                   (gptel--model-mime-capable-p
                    (setq mime (mailcap-file-name-to-mime-type path)))
                 t)))
        (list t link-type path resource-type user-check readablep mime-valid mime)
      (list nil link-type path resource-type user-check readablep mime-valid mime))))

(cl-defmethod gptel--parse-media-links ((_mode (eql 'org-mode)) beg end)
  "Parse text and actionable links between BEG and END.

Return a list of the form
 ((:text \"some text\")
  (:media \"/path/to/media.png\" :mime \"image/png\")
  (:text \"More text\"))
for inclusion into the user prompt for the gptel request."
  (let ((parts) (from-pt))
    (save-excursion
      (setq from-pt (goto-char beg))
      (while (re-search-forward gptel-org--link-regex end t)
        (let* ((link (org-element-context))
               (link-status (gptel-org--validate-link link)))
          (cl-destructuring-bind
              (valid type path resource-type user-check readablep mime-valid mime)
              link-status
            (cond
             ((and valid (member type '("file" "attachment")))
              ;; Text file or supported binary file: collect text up to link
              (when-let* ((text (buffer-substring-no-properties
                                 from-pt (gptel-org--element-begin link))))
                (unless (string-blank-p text) (push (list :text text) parts)))
              ;; collect link
              (push (if mime (list :media path :mime mime) (list :textfile path))
                    parts)
              (setq from-pt (point)))
             ((and valid (member type '("http" "https" "ftp")))
              ;; Collect text up to this image, and collect this image url
              (when-let* ((text (buffer-substring-no-properties
                                 from-pt (gptel-org--element-begin link))))
                (unless (string-blank-p text) (push (list :text text) parts)))
              (push (list :url (org-element-property :raw-link link) :mime mime) parts)
              (setq from-pt (point)))
             ((not resource-type)
              (message "Link source not followed for unsupported link type \"%s\"." type))
             ((not user-check)
              (message (if (eq gptel-org-validate-link 'gptel--link-standalone-p)
                           "Ignoring non-standalone link \"%s\"."
                         "Link %s failed to validate, see `gptel-org-validate-link'.")
                       path))
             ((not readablep)
              (message "Ignoring inaccessible file \"%s\"." path))
             ((and (not mime-valid) (eq resource-type 'file))
              (message "Ignoring unsupported binary file \"%s\"." path))))))
      (unless (= from-pt end)
        (push (list :text (buffer-substring-no-properties from-pt end)) parts)))
    (nreverse parts)))

(defun gptel-org--annotate-links (beg end)
  "Annotate Org links whose sources will be sent with `gptel-send'.

Search between BEG and END."
  (when gptel-track-media
    (save-excursion
      (goto-char beg) (forward-line -1)
      (let ((link-ovs (cl-loop for o in (overlays-in (point) end)
                               if (overlay-get o 'gptel-track-media)
                               collect o into os finally return os)))
        (while (re-search-forward gptel-org--link-regex end t)
          (unless (gptel--in-response-p (1- (point)))
            (let* ((link (org-element-context))
                   (from (org-element-begin link))
                   (to (org-element-end link))
                   (link-status (gptel-org--validate-link link))
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

(defun gptel-org--send-with-props (send-fun &rest args)
  "Conditionally modify SEND-FUN's calling environment.

If in an Org buffer under a heading containing a stored gptel
configuration, use that for requests instead.  This includes the
system message, model and provider (backend), among other
parameters.

When `gptel-org-model-from-user-tag' is enabled, model tags on the
current user heading (like :haiku:, :sonnet:) take precedence over
stored properties.

When `gptel-org-model-from-todo-tag' is enabled, model tags on the
enclosing TODO heading (with keywords like AI-DO, AI-DOING) also
take precedence.

ARGS are the original function call arguments."
  (if gptel-org--in-send-with-props
      ;; Prevent recursion - just call the original function
      (apply send-fun args)
    (let ((gptel-org--in-send-with-props t))
      (if (derived-mode-p 'org-mode)
          (pcase-let* ((`(,org-preset ,org-system ,org-backend ,org-model
                          ,org-temperature ,org-tokens ,org-num ,org-tools)
                        (gptel-org--entry-properties))
                       ;; When a preset is found in org properties, apply it to
                       ;; expand its settings into individual variables.  Then
                       ;; overlay any explicitly-set org properties on top.
                       (`(,preset-system ,preset-backend ,preset-model
                          ,preset-temperature ,preset-tokens ,preset-num ,preset-tools)
                        (if org-preset
                            (let (p-system p-backend p-model p-temperature p-tokens p-num p-tools)
                              (gptel--apply-preset
                               org-preset
                               (lambda (sym val)
                                 (pcase sym
                                   ('gptel--system-message (setq p-system val))
                                   ('gptel-backend (setq p-backend val))
                                   ('gptel-model (setq p-model val))
                                   ('gptel-temperature (setq p-temperature val))
                                   ('gptel-max-tokens (setq p-tokens val))
                                   ('gptel--num-messages-to-send (setq p-num val))
                                   ('gptel-tools (setq p-tools val))
                                   ('gptel--preset nil)))) ;ignore, already have it
                              (list p-system p-backend p-model
                                    p-temperature p-tokens p-num p-tools))
                          (list nil nil nil nil nil nil nil)))
                       ;; Priority: org explicit properties > preset values > buffer defaults
                       (gptel--preset (or org-preset gptel--preset))
                       (gptel--system-message (or org-system preset-system gptel--system-message))
                       (gptel-backend (or org-backend preset-backend gptel-backend))
                       (gptel-model (or org-model preset-model gptel-model))
                       (gptel-temperature (or org-temperature preset-temperature gptel-temperature))
                       (gptel-max-tokens (or org-tokens preset-tokens gptel-max-tokens))
                       (gptel--num-messages-to-send (or org-num preset-num gptel--num-messages-to-send))
                       (gptel-tools (or org-tools preset-tools gptel-tools))
                       ;; Check for model tag on user heading (takes precedence)
                       (user-heading-model (gptel-org--get-user-heading-model))
                       ;; Check for model tag on TODO heading (also takes precedence)
                       (todo-heading-model (gptel-org--get-todo-heading-model))
                       ;; User heading model takes precedence over TODO heading model
                       (heading-model (or user-heading-model todo-heading-model)))
            ;; Apply model from heading tag if found
            (when heading-model
              (when-let* ((backend (plist-get heading-model :backend)))
                (setq gptel-backend backend))
              (when-let* ((model (plist-get heading-model :model)))
                (setq gptel-model model))
              (gptel-org--debug "Using model from heading tag: %s"
                                (gptel--model-name gptel-model)))
            (when org-preset
              (gptel-org--debug "Applied preset from org property: %s" org-preset))
            (when gptel-log-level
              (gptel--log
               (format "send-with-props: gptel--preset=%s backend=%s model=%s (org-preset=%s, buffer-default=%s)"
                       gptel--preset
                       (and gptel-backend (gptel-backend-name gptel-backend))
                       gptel-model org-preset
                       (default-value 'gptel--preset))
               "preset-debug" t))
            (apply send-fun args))
        (apply send-fun args)))))

(advice-add 'gptel-send :around #'gptel-org--send-with-props)
(advice-add 'gptel--suffix-send :around #'gptel-org--send-with-props)

;; ;; NOTE: Basic uses in org-mode are covered by advising gptel-send and
;; ;; gptel--suffix-send.  For custom commands it might be necessary to advise
;; ;; gptel-request instead.
;; (advice-add 'gptel-request :around #'gptel-org--send-with-props)


;;; Saving and restoring state
(defun gptel-org--entry-properties (&optional pt)
  "Find gptel configuration properties stored at PT."
  (pcase-let
      ((`(,preset ,system ,backend ,model ,temperature ,tokens ,num ,tools)
         (mapcar
          (lambda (prop) (org-entry-get (or pt (point)) prop 'selective))
          '("GPTEL_PRESET" "GPTEL_SYSTEM" "GPTEL_BACKEND"
            "GPTEL_MODEL" "GPTEL_TEMPERATURE" "GPTEL_MAX_TOKENS"
            "GPTEL_NUM_MESSAGES_TO_SEND" "GPTEL_TOOLS"))))
    (when preset (setq preset (gptel--intern preset)))
    (when system
      (setq system (string-replace "\\n" "\n" system)))
    (when backend
      (setq backend (alist-get backend gptel--known-backends
                               nil nil #'equal)))
    (when model (setq model (gptel--intern model)))
    (when temperature
      (setq temperature (gptel--to-number temperature)))
    (when tokens (setq tokens (gptel--to-number tokens)))
    (when num (setq num (gptel--to-number num)))
    (when tools
      (setq tools (cl-loop
                   for tname in (split-string tools)
                   if (string-prefix-p "@" tname)
                   ;; Category reference: expand to all tools in category
                   nconc (let ((cat-tools
                                (with-demoted-errors "gptel: %S"
                                  (gptel-get-tool (substring tname 1)))))
                           (if (listp cat-tools) cat-tools
                             (prog1 nil
                               (display-warning
                                '(gptel org tools)
                                (format "Tool category %s not found, ignoring"
                                        (substring tname 1))))))
                   else
                   ;; Individual tool name
                   if (with-demoted-errors "gptel: %S"
                        (gptel-get-tool tname))
                   collect it else do
                   (display-warning
                    '(gptel org tools)
                    (format "Tool %s not found, ignoring" tname)))))
    (list preset system backend model temperature tokens num tools)))

(defun gptel-org--heading-has-tag-p (tag)
  "Check if current heading has TAG (case-insensitive)."
  (when (org-at-heading-p)
    (let ((tags (org-get-tags nil t)))  ; local tags only
      (cl-some (lambda (tg) (string-equal-ignore-case tg tag)) tags))))


(defun gptel-org--heading-is-assistant-p ()
  "Check if current heading is an assistant message.
Uses TODO keywords when `gptel-org-use-todo-keywords' is enabled,
otherwise falls back to tag detection.

In keyword mode, a heading is assistant when its TODO state has the
\"AI-\" prefix (AI-DO, AI-DOING, AI-DONE, etc.) or is one of the
special gptel states: REASONING, TOOL, PENDING, ALLOWED, DENIED.
All these states contain AI-authored or AI-system content.  Headings
without a TODO state (plain headings) return nil here and inherit
from their parent via `gptel-org--restore-bounds-from-tags'."
  (if gptel-org-use-todo-keywords
      (when (org-at-heading-p)
        (let ((todo (org-get-todo-state)))
          (and todo
               (or (string-prefix-p "AI-" todo)
                   (member todo '("REASONING" "TOOL" "PENDING" "ALLOWED" "DENIED"))))))
    (gptel-org--heading-has-tag-p gptel-org-assistant-tag)))

(defun gptel-org--heading-is-user-p ()
  "Check if current heading is a user message.
Uses TODO keywords when `gptel-org-use-todo-keywords' is enabled,
otherwise falls back to tag detection.

In keyword mode, a heading is user when it has a TODO state that is
NOT an AI-prefixed state.  This covers all user-authored lifecycle
states: FEEDBACK, TODO, DOING, DONE, CANCELED, etc.  Headings
without a TODO state (plain headings) return nil here and inherit
from their parent via `gptel-org--restore-bounds-from-tags'."
  (if gptel-org-use-todo-keywords
      (when (org-at-heading-p)
        (let ((todo (org-get-todo-state)))
          (and todo
               (not (string-prefix-p "AI-" todo)))))
    (gptel-org--heading-has-tag-p gptel-org-user-tag)))
(defun gptel-org--heading-has-agent-tag-p ()
  "Check if current heading has a tag matching the `*@agent' pattern."
  (when (org-at-heading-p)
    (cl-some (lambda (tag)
               (and (stringp tag)
                    (string-suffix-p "@agent" tag)))
             (org-get-tags nil t))))

(defun gptel-org--heading-has-todo-keyword-p ()
  "Check if current heading has a TODO keyword from `gptel-org-todo-keywords'."
  (when (org-at-heading-p)
    (save-excursion
      (beginning-of-line)
      (and (looking-at "^\\*+ +\\([^ \t\n]+\\)")
           (member (match-string-no-properties 1) gptel-org-todo-keywords)))))

(defun gptel-org--find-todo-in-lineage (start-bounds)
  "Find the innermost TODO heading in the lineage START-BOUNDS.

START-BOUNDS is an ordered list of heading positions (innermost-first)
as produced by `gptel-org--element-lineage-map'.

Returns the zero-based index of the first position where the heading
has a TODO keyword from `gptel-org-todo-keywords', or nil if no TODO
heading is found in the lineage."
  (cl-loop for pos in start-bounds
           for idx from 0
           when (save-excursion
                  (goto-char pos)
                  (gptel-org--heading-has-todo-keyword-p))
           return idx))

(defun gptel-org--find-model-in-tags (tags &optional skip-tag)
  "Find a model specification in TAGS.

Searches TAGS for model aliases or model names.  If SKIP-TAG is provided,
that tag is skipped (e.g., the user tag itself).

Returns a plist (:backend BACKEND :model MODEL) if a matching model
is found, nil otherwise.

Model aliases are symbols with a :model-id property (like \\=`haiku\\=',
\\=`sonnet\\=', \\=`opus\\=' for Anthropic models).  Model names are the actual
model identifiers in backends."
  (cl-block gptel-org--find-model-in-tags
    (cl-loop
     for tag in tags
     for tag-sym = (intern (downcase tag))
     ;; Skip the specified tag (e.g., user tag)
     unless (and skip-tag (string-equal-ignore-case tag skip-tag))
     ;; Check if tag is a model alias (has :model-id property)
     if (get tag-sym :model-id)
     return (cl-loop for (_name . backend) in gptel--known-backends
                     when (memq tag-sym (gptel-backend-models backend))
                     return (list :backend backend :model tag-sym))
     ;; Check if tag matches a model name in any backend
     else do
     (cl-loop for (_name . backend) in gptel--known-backends
              for models = (gptel-backend-models backend)
              do (cl-loop for model in models
                          when (string-equal-ignore-case
                                tag (gptel--model-name model))
                          return (cl-return-from gptel-org--find-model-in-tags
                                   (list :backend backend :model model)))))))

(defun gptel-org--find-model-from-tags ()
  "Find a model specification from tags on the current user heading.

Searches the tags on the current heading (if it's a user heading)
for model aliases or model names.

Returns a plist (:backend BACKEND :model MODEL) if a matching model
is found, nil otherwise."
  (when (and gptel-org-model-from-user-tag
             (org-at-heading-p)
             (gptel-org--heading-has-tag-p gptel-org-user-tag))
    (gptel-org--find-model-in-tags (org-get-tags nil t) gptel-org-user-tag)))

(defun gptel-org--find-model-from-todo-tags ()
  "Find a model specification from tags on the current TODO heading.

Searches the tags on the current heading (if it's a TODO heading
with a keyword in `gptel-org-todo-keywords') for model aliases or
model names.

Returns a plist (:backend BACKEND :model MODEL) if a matching model
is found, nil otherwise."
  (when (and gptel-org-model-from-todo-tag
             (org-at-heading-p)
             (gptel-org--heading-has-todo-keyword-p))
    (gptel-org--find-model-in-tags (org-get-tags nil t))))

(defun gptel-org--get-user-heading-model ()
  "Get model from current or nearest user heading tags.

Searches for a user heading at or before point and checks its tags
for model specifications.

Returns a plist (:backend BACKEND :model MODEL) if found, nil otherwise."
  (when gptel-org-model-from-user-tag
    (save-excursion
      ;; If we're at a user heading, check it directly
      (if (and (org-at-heading-p)
               (gptel-org--heading-has-tag-p gptel-org-user-tag))
          (gptel-org--find-model-from-tags)
        ;; Otherwise, search backwards for the nearest user heading
        (let ((found nil))
          (while (and (not found)
                      (ignore-errors (outline-previous-heading)))
            (when (gptel-org--heading-has-tag-p gptel-org-user-tag)
              (setq found (gptel-org--find-model-from-tags))))
          found)))))

(defun gptel-org--get-todo-heading-model ()
  "Get model from the enclosing TODO heading tags.

Searches upward from point for a heading with a TODO keyword from
`gptel-org-todo-keywords' and checks its tags for model specifications.

Returns a plist (:backend BACKEND :model MODEL) if found, nil otherwise."
  (when gptel-org-model-from-todo-tag
    (save-excursion
      ;; Move to enclosing heading if not already at one
      (unless (org-at-heading-p)
        (ignore-errors (outline-previous-heading)))
      ;; Search upward for a TODO heading
      (let ((found nil))
        (while (and (not found)
                    (org-at-heading-p))
          (when (gptel-org--heading-has-todo-keyword-p)
            (setq found (gptel-org--find-model-from-todo-tags)))
          (unless found
            (unless (ignore-errors (outline-up-heading 1 t))
              ;; No parent heading, stop the loop
              (goto-char (point-min)))))
        found))))

(defun gptel-org--restore-bounds-from-tags ()
  "Restore gptel response properties based on heading tags or TODO keywords.

Scans all headings in the buffer for role indicators:
- When `gptel-org-use-todo-keywords' is enabled: looks for AI/HI TODO keywords
- Otherwise: looks for :assistant:/:user: tags

Headings marked as assistant get their subtree content marked with
the gptel response property.  User headings within an assistant
subtree have their gptel property removed.  Conversely, assistant
headings within a user subtree get their gptel response property
restored.

Returns non-nil if any role headings were found and processed."
  (save-excursion
    (goto-char (point-min))
    (let ((found-tags nil))
      (while (outline-next-heading)
        (cond
         ;; Assistant heading - mark subtree as response, but respect user headings within
         ((gptel-org--heading-is-assistant-p)
          (setq found-tags t)
          (let ((assistant-beg (point))
                (assistant-end (save-excursion
                                 (org-end-of-subtree t t)
                                 (point))))
            ;; First, mark the entire assistant subtree as response
            (add-text-properties assistant-beg assistant-end
                                 '(gptel response front-sticky (gptel)))
            ;; Then, scan for user headings within and remove gptel property from those
            (save-excursion
              (goto-char assistant-beg)
              (while (and (outline-next-heading)
                          (< (point) assistant-end))
                (when (gptel-org--heading-is-user-p)
                  (let ((user-beg (point))
                        (user-end (save-excursion
                                    (org-end-of-subtree t t)
                                    (min (point) assistant-end))))
                    (remove-text-properties user-beg user-end '(gptel nil))))))
            ;; Skip past the assistant subtree
            (goto-char (1- assistant-end))))
         ;; User heading - ensure no gptel property, but mark assistant sub-headings
         ((gptel-org--heading-is-user-p)
          (setq found-tags t)
          (let ((beg (point))
                (end (save-excursion
                       (org-end-of-subtree t t)
                       (point))))
            (remove-text-properties beg end '(gptel nil))
            ;; Scan for assistant headings within and mark them as response
            (save-excursion
              (goto-char beg)
              (while (and (outline-next-heading)
                          (< (point) end))
                (when (gptel-org--heading-is-assistant-p)
                  (let ((asst-beg (point))
                        (asst-end (save-excursion
                                    (org-end-of-subtree t t)
                                    (min (point) end))))
                    (add-text-properties asst-beg asst-end
                                         '(gptel response front-sticky (gptel)))))))
            (goto-char (1- end))))
         ;; No role indicator - clear stale gptel properties
         (t
          (when (get-text-property (point) 'gptel)
            (let ((beg (point))
                  (end (save-excursion
                         (org-end-of-subtree t t)
                         (point))))
              (remove-text-properties beg end '(gptel nil))
              (goto-char (1- end)))))))
      found-tags)))

(defvar-local gptel-org--bounds-from-tags nil
  "Non-nil if bounds were restored from heading tags.

When this is non-nil, `gptel-org--save-state' will skip writing
GPTEL_BOUNDS since the bounds are determined by heading tags.")

(defun gptel-org--restore-state ()
  "Restore gptel state for Org buffers when turning on `gptel-mode'."
  (save-restriction
    (let ((modified (buffer-modified-p)))
      (widen)
      (condition-case status
          (progn
            ;; Try tag/keyword-based bounds first if enabled
            (if (and (or gptel-org-infer-bounds-from-tags
                        gptel-org-use-todo-keywords)
                     (gptel-org--restore-bounds-from-tags))
                (setq gptel-org--bounds-from-tags t)
              ;; Fall back to GPTEL_BOUNDS property
              (setq gptel-org--bounds-from-tags nil)
              (when-let* ((bounds (org-entry-get (point-min) "GPTEL_BOUNDS")))
                (gptel--restore-props (read bounds))))
            (pcase-let ((`(,preset ,system ,backend ,model ,temperature ,tokens ,num ,tools)
                         (gptel-org--entry-properties (point-min))))
              (when preset
                (if (gptel-get-preset preset)
                    (progn (gptel--apply-preset
                            preset (lambda (sym val) (set (make-local-variable sym) val)))
                           (setq gptel--preset preset))
                  (display-warning
                   '(gptel presets)
                   (format "Could not activate gptel preset `%s' in buffer \"%s\""
                           preset (buffer-name)))))
              (when system (setq-local gptel--system-message system))
              (if backend (setq-local gptel-backend backend)
                (message
                 (substitute-command-keys
                  (concat
                   "Could not activate gptel backend \"%s\"!  "
                   "Switch backends with \\[universal-argument] \\[gptel-send]"
                   " before using gptel."))
                 backend))
              (when model (setq-local gptel-model model))
              (when temperature (setq-local gptel-temperature temperature))
              (when tokens (setq-local gptel-max-tokens tokens))
              (when num (setq-local gptel--num-messages-to-send num))
              (when tools (setq-local gptel-tools tools))))
        (:success (message "gptel chat restored."))
        (error (message "Could not restore gptel state, sorry! Error: %s" status)))
      (set-buffer-modified-p modified)))
  (gptel-org--register-todo-keywords))

(defun gptel-org-set-properties (pt &optional msg)
  "Store the active gptel configuration under the current heading.

PT is the cursor position by default.  If MSG is non-nil (default),
display a message afterwards.

If a gptel preset has been applied in this buffer, a reference to it is
saved.

Additional metadata is stored only if no preset was applied or if it
differs from the preset specification.  This is limited to the active
gptel model and backend names, the system message, active tools, the
response temperature, max tokens and number of conversation turns to
send in queries.  (See `gptel--num-messages-to-send' for the last one.)"
  (interactive (list (point) t))
  (let ((preset-spec (and gptel--preset (gptel-get-preset gptel--preset))))
    (if preset-spec
        (org-entry-put pt "GPTEL_PRESET" (gptel--to-string gptel--preset))
      (org-entry-delete pt "GPTEL_PRESET"))

    ;; FIXME: nil can mean "no value was explicitly set by the user" as well as
    ;; "this setting has been set to nil".  We are not yet distinguishing
    ;; between the two when saving Org properties.  This is particularly
    ;; relevant for the system message, whose explicit nil value will not be
    ;; captured when saving Org buffers.

    ;; Model and backend
    (if (gptel--preset-mismatch-value preset-spec :model gptel-model)
        (org-entry-put pt "GPTEL_MODEL" (gptel--model-name gptel-model)))
    (if (gptel--preset-mismatch-value preset-spec :backend gptel-backend)
        (org-entry-put pt "GPTEL_BACKEND" (gptel-backend-name gptel-backend)))
    ;; System message
    (let ((parsed (car-safe (gptel--parse-directive gptel--system-message))))
      (if (gptel--preset-mismatch-value preset-spec :system parsed)
          (when parsed
            (org-entry-put pt "GPTEL_SYSTEM" (string-replace "\n" "\\n" parsed)))
        (org-entry-delete pt "GPTEL_SYSTEM")))
    ;; Tools
    (let ((tool-names (mapcar #'gptel-tool-name gptel-tools)))
      (if (gptel--preset-mismatch-value preset-spec :tools tool-names)
          (org-entry-put pt "GPTEL_TOOLS" (string-join tool-names " "))
        (org-entry-delete pt "GPTEL_TOOLS")))
    ;; Temperature, max tokens and cutoff
    (if (and (gptel--preset-mismatch-value preset-spec :temperature gptel-temperature)
             (not (equal (default-value 'gptel-temperature) gptel-temperature)))
        (org-entry-put pt "GPTEL_TEMPERATURE" (number-to-string gptel-temperature))
      (org-entry-delete pt "GPTEL_TEMPERATURE"))
    (if (and (gptel--preset-mismatch-value preset-spec :max-tokens gptel-max-tokens)
             gptel-max-tokens)
        (org-entry-put pt "GPTEL_MAX_TOKENS" (number-to-string gptel-max-tokens))
      (org-entry-delete pt "GPTEL_MAX_TOKENS"))
    (if (and (gptel--preset-mismatch-value
              preset-spec :num-messages-to-send gptel--num-messages-to-send)
             (natnump gptel--num-messages-to-send))
        (org-entry-put pt "GPTEL_NUM_MESSAGES_TO_SEND"
                       (number-to-string gptel--num-messages-to-send))
      (org-entry-delete pt "GPTEL_NUM_MESSAGES_TO_SEND")))
  (when msg
    (message "Added gptel configuration to current headline.")))

(defun gptel-org--save-state ()
  "Write the gptel state to the Org buffer as Org properties.
Respects `gptel-org-save-state'; does nothing if that is nil."
  (when gptel-org-save-state
    (org-with-wide-buffer
     (goto-char (point-min))
     (when (org-at-heading-p)
       (org-open-line 1))
     (gptel-org-set-properties (point-min))
     ;; Save response boundaries (skip if using tag-based bounds)
     (if gptel-org--bounds-from-tags
         ;; Remove GPTEL_BOUNDS if it exists, since bounds are from tags
         (org-entry-delete (point-min) "GPTEL_BOUNDS")
       ;; Save bounds the traditional way
       (letrec ((write-bounds
                 (lambda (attempts)
                   (when-let* ((bounds (gptel--get-buffer-bounds))
                               ;; first value of ((prop . ((beg end val)...))...)
                               (offset (caadar bounds))
                               (offset-marker (set-marker (make-marker) offset)))
                     (org-entry-put (point-min) "GPTEL_BOUNDS"
                                    (prin1-to-string (gptel--get-buffer-bounds)))
                     (when (and (not (= (marker-position offset-marker) offset))
                                (> attempts 0))
                       (funcall write-bounds (1- attempts)))))))
         (funcall write-bounds 6))))))


;;; Transforming responses
;;;###autoload
(defun gptel--convert-markdown->org (str)
  "Convert string STR from markdown to org markup.

This is a very basic converter that handles only a few markup
elements."
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (while (re-search-forward "`+\\|\\*\\{1,2\\}\\|_\\|^#+" nil t)
      (pcase (match-string 0)
        ;; Handle backticks
        ((and (guard (eq (char-before) ?`)) ticks)
         (gptel--replace-source-marker (length ticks))
         (save-match-data
           (catch 'block-end
             (while (search-forward ticks nil t)
               (unless (or (eq (char-before (match-beginning 0)) ?`)
                           (eq (char-after) ?`))
                 (gptel--replace-source-marker (length ticks) 'end)
                 (throw 'block-end nil))))))
        ;; Handle headings
        ((and (guard (eq (char-before) ?#)) heading)
         (cond
          ((looking-at "[[:space:]]")   ;Handle headings
           (delete-region (line-beginning-position) (point))
           (insert (make-string (length heading) ?*)))
          ((looking-at "\\+begin_src") ;Overeager LLM switched to using Org src blocks
           (save-match-data (re-search-forward "^#\\+end_src" nil t)))))
        ;; Handle emphasis
        ("**" (cond
               ;; ((looking-at "\\*\\(?:[[:word:]]\\|\s\\)")
               ;;  (delete-char 1))
               ((looking-back "\\(?:[[:word:][:punct:]\n]\\|\s\\)\\*\\{2\\}"
                              (max (- (point) 3) (point-min)))
                (delete-char -1))))
        ("*"
         (cond
          ((save-match-data
             (and (or (= (point) 2)
                      (looking-back "\\(?:[[:space:]]\\|\s\\)\\(?:_\\|\\*\\)"
                                    (max (- (point) 2) (point-min))))
                  (not (looking-at "[[:space:]]\\|\s"))))
           ;; Possible beginning of emphasis
           (and
            (save-excursion
              (when (and (re-search-forward (regexp-quote (match-string 0))
                                            (line-end-position) t)
                         (looking-at "[[:space:][:punct:]]\\|\s")
                         (not (looking-back "\\(?:[[:space]]\\|\s\\)\\(?:_\\|\\*\\)"
                                            (max (- (point) 2) (point-min)))))
                (delete-char -1) (insert "/") t))
            (progn (delete-char -1) (insert "/"))))
          ((save-excursion
             (ignore-errors (backward-char 2))
             (or (and (bobp) (looking-at "\\*[[:space:]]"))
                 (looking-at "\\(?:$\\|\\`\\)\n\\*[[:space:]]")))
           ;; Bullet point, replace with hyphen
           (delete-char -1) (insert "-"))))))
    (buffer-string)))

(defun gptel--replace-source-marker (num-ticks &optional end)
  "Replace markdown style backticks with Org equivalents.

NUM-TICKS is the number of backticks being replaced.  If END is
true these are \"ending\" backticks.

This is intended for use in the markdown to org stream converter."
  (let ((from (match-beginning 0)))
    (delete-region from (point))
    (if (and (= num-ticks 3)
             (save-excursion (beginning-of-line)
                             (skip-chars-forward " \t")
                             (eq (point) from)))
        (insert (if end "#+end_src" "#+begin_src "))
      (insert "="))))

;;;###autoload
(defun gptel--stream-convert-markdown->org (start-marker)
  "Return a Markdown to Org converter.

This function parses a stream of Markdown text to Org
continuously when it is called with successive chunks of the
text stream.

START-MARKER is used to identify the corresponding process when
cleaning up after."
  (letrec ((in-src-block nil)           ;explicit nil to address BUG #183
           (in-org-src-block nil)
           (temp-buf ; NOTE: Switch to `generate-new-buffer' after we drop Emacs 27.1
            (gptel--temp-buffer " *gptel-temp*"))
           (start-pt (make-marker))
           (ticks-total 0)      ;MAYBE should we let-bind case-fold-search here?
           (cleanup-fn
            (lambda (beg _)
              (when (and (equal beg (marker-position start-marker))
                         (eq (current-buffer) (marker-buffer start-marker)))
                (when (buffer-live-p (get-buffer temp-buf))
                  (set-marker start-pt nil)
                  (kill-buffer temp-buf))
                (remove-hook 'gptel-post-response-functions cleanup-fn)))))
    (add-hook 'gptel-post-response-functions cleanup-fn)
    (lambda (str)
      (let ((noop-p) (ticks 0))
        (with-current-buffer (get-buffer temp-buf)
          (save-excursion (goto-char (point-max)) (insert str))
          (when (marker-position start-pt) (goto-char start-pt))
          (when in-src-block (setq ticks ticks-total))
          (save-excursion
            (while (re-search-forward "`\\|\\*\\{1,2\\}\\|_\\|^#+" nil t)
              (pcase (match-string 0)
                ("`"
                 ;; Count number of consecutive backticks
                 (backward-char)
                 (while (and (char-after) (eq (char-after) ?`))
                   (forward-char)
                   (if in-src-block (cl-decf ticks) (cl-incf ticks)))
                 ;; Set the verbatim state of the parser
                 (if (and (eobp)
                          ;; Special case heuristic: If the response ends with
                          ;; ^``` we don't wait for more input.
                          ;; FIXME: This can have false positives.
                          (not (save-excursion (beginning-of-line)
                                               (looking-at "^```$"))))
                     ;; End of input => there could be more backticks coming,
                     ;; so we wait for more input
                     (progn (setq noop-p t) (set-marker start-pt (match-beginning 0)))
                   ;; We reached a character other than a backtick
                   (cond
                    ;; Ticks balanced, end src block
                    ((= ticks 0)
                     (progn (setq in-src-block nil)
                            (gptel--replace-source-marker ticks-total 'end)))
                    ;; Positive number of ticks, start an src block
                    ((and (> ticks 0) (not in-src-block))
                     (setq ticks-total ticks
                           in-src-block t)
                     (gptel--replace-source-marker ticks-total))
                    ;; Negative number of ticks or in a src block already,
                    ;; reset ticks
                    (t (setq ticks ticks-total)))))
                ;; Handle headings and misguided #+begin_src text
                ((and (guard (and (eq (char-before) ?#) (or (not in-src-block) in-org-src-block)))
                      heading)
                 (if in-org-src-block
                     ;; If we are inside an Org-style src block, look for #+end_src
                     (cond
                      ((< (- (point-max) (point)) 8) ;not enough information to close Org src block
                       (setq noop-p t) (set-marker start-pt (match-beginning 0)))
                      ((looking-at "\\+end_src") ;Close Org src block
                       (setq in-src-block nil in-org-src-block nil)))
                   ;; Otherwise check for Markdown headings, or for #+begin_src
                   (cond
                    ((eobp)       ; Not enough information about the heading yet
                     (setq noop-p t) (set-marker start-pt (match-beginning 0)))
                    ((looking-at "[[:space:]]") ; Convert markdown heading to Org heading
                     (delete-region (line-beginning-position) (point))
                     (insert (make-string (length heading) ?*)))
                    ((< (- (point-max) (point)) 11) ;Not enough information to check if Org src block
                     (setq noop-p t) (set-marker start-pt (match-beginning 0)))
                    ((looking-at "\\+begin_src ") ;Overeager LLM switched to using Org src blocks
                     (setq in-src-block t in-org-src-block t)))))
                ;; Handle other chars: emphasis, bold and bullet items
                ((and "**" (guard (not in-src-block)))
                 (cond
                  ;; TODO Not sure why this branch was needed
                  ;; ((looking-at "\\*\\(?:[[:word:]]\\|\s\\)") (delete-char 1))

                  ;; Looking back at "w**" or " **"
                  ((looking-back "\\(?:[[:word:][:punct:]\n]\\|\s\\)\\*\\{2\\}"
                                 (max (- (point) 3) (point-min)))
                   (delete-char -1))))
                ((and "*" (guard (not in-src-block)))
                 (if (eobp)
                     ;; Not enough information about the "*" yet
                     (progn (setq noop-p t) (set-marker start-pt (match-beginning 0)))
                   ;; "*" is either emphasis or a bullet point
                   (save-match-data
                     (save-excursion
                       (ignore-errors (backward-char 2))
                       (cond
                        ((and     ; At bob, underscore/asterisk followed by word
                          (or (and (bobp) (looking-at "\\(?:_\\|\\*\\)\\([^[:space:][:punct:]]\\|$\\)"))
                              (looking-at ; word followed by underscore/asterisk
                               "[^[:space:]\n]\\(?:_\\|\\*\\)\\(?:[[:space:][:punct:]]\\|$\\)")
                              (looking-at ; underscore/asterisk followed by word
                               "\\(?:[[:space:]]\\)\\(?:_\\|\\*\\)\\([^[:space:]]\\|$\\)"))
                          (not (looking-at "[[:punct:]]\\(?:_\\|\\*\\)[[:punct:]]")))
                         ;; Emphasis, replace with slashes
                         (forward-char (if (bobp) 1 2)) (delete-char -1) (insert "/"))
                        ((or (and (bobp) (looking-at "\\*[[:space:]]"))
                             (looking-at "\\(?:$\\|\\`\\)\n\\*[[:space:]]"))
                         ;; Bullet point, replace with hyphen
                         (forward-char (if (bobp) 1 2)) (delete-char -1) (insert "-"))))))))))
          (if noop-p
              (buffer-substring (point) start-pt)
            (prog1 (buffer-substring (point) (point-max))
                   (set-marker start-pt (point-max)))))))))


;;; Dynamic prefix support for subtree context

(defun gptel-org--in-prompt-buffer-p ()
  "Return non-nil if current buffer is a temporary prompt buffer.

This detects gptel's temp buffers used for parsing/constructing prompts,
where dynamic prefix adjustment should not be applied."
  (string-prefix-p " *gptel-prompt*" (buffer-name)))

(defun gptel-org--advice-prompt-prefix (orig-fun)
  "Advice for `gptel-prompt-prefix-string' to support dynamic org heading levels.

ORIG-FUN is the original function.  When `gptel-org-subtree-context'
is enabled, adjusts the prefix to use the correct heading level.

Dynamic adjustment is skipped in temp prompt buffers (used for parsing)
since those buffers don't have the proper org heading structure."
  (if gptel-org--in-prefix-advice
      (funcall orig-fun)
    (let ((gptel-org--in-prefix-advice t))
      (let ((result (funcall orig-fun)))
        (gptel-org--debug "advice-prompt-prefix: orig=%S org-mode=%s subtree-context=%s prompt-buf=%s"
                          result (derived-mode-p 'org-mode) gptel-org-subtree-context
                          (gptel-org--in-prompt-buffer-p))
        (if (and (derived-mode-p 'org-mode)
                 (not (gptel-org--in-prompt-buffer-p)))
            (gptel-org--dynamic-prefix-string result 'for-prompt)
          result)))))

(defun gptel-org--advice-response-prefix (orig-fun)
  "Advice for `gptel-response-prefix-string' to support dynamic org heading levels.

ORIG-FUN is the original function.  When `gptel-org-subtree-context'
is enabled, adjusts the prefix to use the correct heading level.

Dynamic adjustment is skipped in temp prompt buffers (used for parsing)
since those buffers don't have the proper org heading structure."
  (if gptel-org--in-prefix-advice
      (funcall orig-fun)
    (let ((gptel-org--in-prefix-advice t))
      (let ((result (funcall orig-fun)))
        (gptel-org--debug "advice-response-prefix: orig=%S org-mode=%s subtree-context=%s prompt-buf=%s"
                          result (derived-mode-p 'org-mode) gptel-org-subtree-context
                          (gptel-org--in-prompt-buffer-p))
        (if (and (derived-mode-p 'org-mode)
                 (not (gptel-org--in-prompt-buffer-p)))
            (gptel-org--dynamic-prefix-string result nil) ;nil = for response
          result)))))

(advice-add 'gptel-prompt-prefix-string :around #'gptel-org--advice-prompt-prefix)
(advice-add 'gptel-response-prefix-string :around #'gptel-org--advice-response-prefix)


;;; Response heading adjustment for subtree context

(defun gptel-org--find-matching-block-end (block-type)
  "Find the matching #+end for a #+begin block of BLOCK-TYPE.
Handles nested blocks of the same type by counting nesting depth.
Point should be on the line after #+begin.  Returns the position of
the #+end line beginning, or nil if no match found.
Point is moved to after the #+end line on success."
  (let ((depth 1)
        (begin-re (format "^[ \t]*#\\+begin_%s\\(?:[ \t]\\|$\\)" block-type))
        (end-re (format "^[ \t]*#\\+end_%s[ \t]*$" block-type))
        ;; Combined regex matches either begin or end
        (combined-re (format "^[ \t]*#\\+\\(begin_%s\\(?:[ \t]\\|$\\)\\|end_%s[ \t]*$\\)"
                             block-type block-type)))
    (while (and (> depth 0)
                (re-search-forward combined-re nil t))
      (beginning-of-line)
      (cond
       ((looking-at-p begin-re) (cl-incf depth))
       ((looking-at-p end-re) (cl-decf depth)))
      (end-of-line))
    (when (= depth 0)
      (beginning-of-line)
      (prog1 (point)
        (end-of-line)))))

(defun gptel-org--escape-example-blocks (beg end)
  "Prefix lines in example blocks with comma between BEG and END.

Per Org manual, lines starting with `*' or `#+' inside example blocks
must be prefixed with a comma to prevent them from being interpreted
as outline nodes or special syntax.  Org strips these commas when
accessing the block contents.

Lines already escaped (starting with `,*' or `,#+') are left unchanged.

Handles nested example/src blocks correctly: the content of the
outermost block is processed in a temporary buffer (similar to
`org-edit-special'), where all special lines are escaped.  Nested
block delimiters (#+begin/#+end) within the outer block are also
escaped to prevent org from interpreting them as block structure.

This should be called before `gptel-org--adjust-response-headings'
so that headings inside examples are not modified."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      ;; Find each top-level example/src block
      (while (re-search-forward
              "^[ \t]*#\\+begin_\\(example\\|src\\)\\(?:[ \t]\\|$\\)" nil t)
        (let* ((block-type (match-string 1))
               (block-start (line-beginning-position 2)) ; line after #+begin
               (block-end nil))
          ;; Find the matching #+end, handling nesting
          (setq block-end (gptel-org--find-matching-block-end block-type))
          (when block-end
            ;; Process the block content in a temporary buffer to
            ;; handle escaping cleanly, similar to org-edit-special.
            ;; This avoids issues with nested blocks where inner
            ;; #+begin/#+end markers could confuse simple regex scanning.
            (let* ((block-content (buffer-substring block-start block-end))
                   (escaped-content
                    (with-temp-buffer
                      (insert block-content)
                      (goto-char (point-min))
                      (while (not (eobp))
                        ;; Escape lines starting with * or #+
                        ;; but NOT already-escaped ,* or ,#+
                        (when (looking-at "^\\(\\*\\|#\\+\\)")
                          (insert ","))
                        (forward-line 1))
                      (buffer-string))))
              ;; Replace the original block content with escaped version
              (unless (string= block-content escaped-content)
                (delete-region block-start block-end)
                (goto-char block-start)
                (insert escaped-content)))))))))


(defun gptel-org--in-example-block-p ()
  "Return non-nil if point is inside an example or src block.
Point is considered inside the block if it is between the end of the
opening delimiter and the beginning of the nesting-matched closing
delimiter.  The delimiter lines themselves are not inside the block.

Handles nesting: inner #+begin_/#+end_ pairs (including those that
have been comma-escaped by the auto-corrector) are correctly skipped
when searching for the matching outer closer."
  (save-excursion
    (save-match-data
      (let ((pos (point))
            (in-block nil)
            (done nil))
        (goto-char (point-min))
        (while (and (not in-block)
                    (not done)
                    (re-search-forward
                     "^[ \t]*#\\+begin_\\(example\\|src\\)\\(?:[ \t]\\|$\\)" pos t))
          (let ((block-start (point))
                (block-type (match-string 1)))
            ;; Find the nesting-matched closer for this opener.
            (let ((end-pos (gptel-org--find-block-end-pos block-type)))
              (if end-pos
                  (if (< pos end-pos)
                      ;; pos is before the closing delimiter line
                      (when (>= pos block-start)
                        (setq in-block t))
                    ;; pos is at or past the closer; not inside this
                    ;; block.  Stop if point moved past pos.
                    (when (> (point) pos)
                      (setq done t)))
                ;; No closing delimiter found — block is unclosed.
                ;; During streaming, content may arrive before the
                ;; closer.  Treat content after the opener as inside.
                (when (>= pos block-start)
                  (setq in-block t))))))
        in-block))))

(defun gptel-org--find-block-end-pos (block-type)
  "Find the line-beginning position of the matching #+end_ for BLOCK-TYPE.
Handles nesting by counting depth of same-type #+begin_/#+end_ pairs.
Point should be after the opening #+begin_ line.  Moves point to the
end of the matched #+end_ line on success.  Returns the line-beginning
position of the #+end_ line, or nil if no match."
  (let ((depth 1)
        (begin-re (format "^[ \t]*,*#\\+begin_%s\\(?:[ \t]\\|$\\)" block-type))
        (end-re (format "^[ \t]*,*#\\+end_%s[ \t]*$" block-type))
        (combined-re (format "^[ \t]*,*#\\+\\(begin_%s\\(?:[ \t]\\|$\\)\\|end_%s[ \t]*$\\)"
                             block-type block-type))
        (result nil))
    (while (and (> depth 0)
                (re-search-forward combined-re nil t))
      (beginning-of-line)
      (cond
       ((looking-at-p begin-re) (cl-incf depth))
       ((looking-at-p end-re)
        (cl-decf depth)
        (when (= depth 0)
          (setq result (point)))))
      (end-of-line))
    result))

(defun gptel-org--in-agent-indirect-buffer-p ()
  "Return non-nil if the current buffer is an agent indirect buffer.
An agent indirect buffer is an indirect buffer whose first heading
has a tag matching the `*@agent' pattern, or has been marked with
the buffer-local flag `gptel-org--agent-indirect-buffer-p'."
  (and (buffer-base-buffer (current-buffer))
       (derived-mode-p 'org-mode)
       (or (bound-and-true-p gptel-org--agent-indirect-buffer-p)
           (save-excursion
             (goto-char (point-min))
             (gptel-org--heading-has-agent-tag-p)))))

(defun gptel-org--adjust-response-headings (beg end)
  "Adjust heading levels in the response region from BEG to END.

When in an agent indirect buffer (i.e. `gptel-org-subtree-context'
is enabled), any org headings in the AI response should be demoted
to be children of the preceding
heading.  This prevents response headings from escaping the
assistant subtree and breaking the conversation structure.

First, lines in example blocks that start with `*' or `#+' are
prefixed with comma per Org manual requirements.  Then headings
outside of example blocks are adjusted.

For example, if the @assistant heading is at level 4 (****), any
headings in the response should be at level 5 or deeper.

In agent indirect buffers, the reference level is determined by
searching backward from BEG for the nearest heading.  If no heading
is found before BEG, the first heading in the narrowed buffer is
used as the top-level reference, and response headings are demoted
to be children of it."
  (when (and (derived-mode-p 'org-mode)
             (gptel-org--in-agent-indirect-buffer-p))
    ;; First: escape special lines in example blocks
    (gptel-org--escape-example-blocks beg end)
    (save-excursion
      ;; Find the reference heading level.
      ;; Search backward from beg for the nearest heading — this should
      ;; be the @assistant heading that precedes the response content.
      ;; In indirect buffers (agent subtrees), search respects narrowing
      ;; so the heading must be within the narrowed region.
      ;; If no heading is found before beg, use the first heading in
      ;; the buffer as the top-level reference (relevant for agent
      ;; indirect buffers where the agent heading is the first heading).
      (let ((reference-level
             (save-excursion
               (goto-char beg)
               (if (re-search-backward org-outline-regexp-bol nil t)
                   (let ((level (org-outline-level)))
                     (gptel-org--debug
                      "adjust-headings: found heading at level %d, line %d"
                      level (line-number-at-pos))
                     level)
                 ;; No heading before beg — in an agent indirect buffer,
                 ;; point-min IS the agent heading.  Use it directly.
                 (goto-char (point-min))
                 (if (org-at-heading-p)
                     (let ((level (org-outline-level)))
                       (gptel-org--debug
                        "adjust-headings: using agent heading at point-min, level %d"
                        level)
                       level)
                   (gptel-org--debug
                    "adjust-headings: no heading at point-min, using level 1")
                   1))))
            (min-response-level nil))
        (gptel-org--debug
         "adjust-headings: beg=%d end=%d reference-level=%d buffer=%S narrowed=%s agent-indirect=%s"
         beg end reference-level (buffer-name)
         (if (buffer-narrowed-p) "yes" "no")
         (if (gptel-org--in-agent-indirect-buffer-p) "yes" "no"))
        (save-restriction
          (narrow-to-region beg end)
          ;; First pass: find the minimum heading level in the response
          ;; (only for headings outside example blocks, and skip
          ;; empty headings which are boundary markers like "*** ")
          (goto-char (point-min))
          (while (re-search-forward org-outline-regexp-bol nil t)
            (unless (or (gptel-org--in-example-block-p)
                        (looking-at-p "[ \t]*$"))
              (let ((level (org-outline-level)))
                (when (or (null min-response-level)
                          (< level min-response-level))
                  (setq min-response-level level)))))
          (gptel-org--debug
           "adjust-headings: min-response-level=%S"
           min-response-level)
          ;; Second pass: adjust headings if needed
          (when (and min-response-level
                     (<= min-response-level reference-level))
            ;; Need to demote all headings by (reference-level - min-response-level + 1)
            (let ((level-diff (- (1+ reference-level) min-response-level)))
              (gptel-org--debug
               "adjust-headings: demoting by %d levels" level-diff)
              (goto-char (point-min))
              (while (re-search-forward "^\\(\\*+\\)\\( \\)" nil t)
                (unless (gptel-org--in-example-block-p)
                  (let* ((current-stars (match-string 1))
                         (new-level (+ (length current-stars) level-diff))
                         (new-stars (make-string new-level ?*)))
                    (replace-match (concat new-stars "\\2"))))))))))))

;; (add-hook 'gptel-post-response-functions #'gptel-org--adjust-response-headings)


;;; Post-response sanitizer for org-format responses

(defun gptel-org--sanitize-org-response (beg end)
  "Fix common AI formatting mistakes in org-format responses.

When `gptel-org--org-format-response' is set, the markdown-to-org
converter is skipped.  This function runs as a post-response hook
to catch cases where the AI still uses markdown syntax despite
being instructed to respond in org format.

Called from `gptel-post-response-functions' with BEG and END
positions of the response."
  (when (and (derived-mode-p 'org-mode)
             gptel-org--org-format-response)
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (goto-char (point-min))
        ;; Convert markdown code fences to org src blocks
        (while (re-search-forward "^[ \t]*```\\([[:alnum:]_+-]*\\)[ \t]*$" nil t)
          (let ((lang (match-string 1)))
            (if (save-excursion
                  (re-search-forward "^[ \t]*```[ \t]*$" nil t))
                (progn
                  (replace-match (if (string-empty-p lang)
                                     "#+begin_src"
                                   (concat "#+begin_src " lang)))
                  (when (re-search-forward "^[ \t]*```[ \t]*$" nil t)
                    (replace-match "#+end_src")))
              ;; No closing fence found, leave as-is
              nil)))
        ;; Convert markdown headings (## Heading) to org headings
        (goto-char (point-min))
        (while (re-search-forward "^\\(#+\\) " nil t)
          (unless (or (gptel-org--in-example-block-p)
                      ;; Don't touch org keywords like #+begin_src
                      (looking-back "^#\\+[[:alpha:]]" (line-beginning-position)))
            (let* ((hashes (match-string 1))
                   (level (length hashes))
                   (stars (make-string level ?*)))
              (replace-match (concat stars " ")))))))))

;; Run at priority 5 (after heading adjustment at 0, before title at 80)
;; (add-hook 'gptel-post-response-functions #'gptel-org--sanitize-org-response 5)

;;; Idempotent auto-corrector for agent indirect buffers
;;
;; When `gptel-org-use-todo-keywords' is enabled, the AI writes headings
;; at level 1 (* Heading).  This corrector, hooked into
;; `after-change-functions' on each agent indirect buffer, rebases any
;; heading with fewer stars than `gptel-org--ref-level' by adding an
;; offset of (ref-level - 1).
;;
;; The correction is idempotent: headings already at or above ref-level
;; are left untouched.  This eliminates stateful tracking (no :last-pos
;; marker, no :in-src/:in-example state, no response-start marker) and
;; removes ordering dependencies with sub-agent cleanup, PENDING heading
;; insertion, etc.
;;
;; The hook is registered buffer-locally in each indirect buffer by
;; `gptel-org-agent--enable-auto-correct'.  No global hooks are needed.

(defun gptel-org--auto-correct-on-change (beg end _old-len)
  "Idempotent auto-corrector for agent indirect buffers.
Runs on `after-change-functions' (buffer-local) to fix heading
levels, escape example block content, and convert markdown fences.

BEG and END delimit the changed region (after the change).
Only complete lines within this region are processed.

Headings with level < `gptel-org--ref-level' are rebased by adding
\(ref-level - 1) stars.  Headings already at the correct level are
skipped, making this safe to run on any region at any time."
  (when (and (eq gptel-log-level 'debug)
             (save-excursion
               (goto-char beg)
               (re-search-forward "^\\*+ \\(TOOL\\|REASONING\\) " end t)))
    (gptel--log
     (format "auto-correct-entry: ref-level=%S auto-correcting=%S beg=%S end=%S buffer=%S text=\"%s\""
             gptel-org--ref-level gptel-org--auto-correcting beg end
             (current-buffer)
             (truncate-string-to-width
              (buffer-substring-no-properties beg (min end (+ beg 120))) 120))
     "tool-heading-debug" 'no-json))
  (when-let* ((ref-level gptel-org--ref-level)
              ((> ref-level 1))
              ((not gptel-org--auto-correcting)))
    (let ((gptel-org--auto-correcting t)
          (offset (1- ref-level))
          (inhibit-modification-hooks t)
          (modified nil))
      (save-excursion
        ;; Expand to full lines
        (goto-char beg)
        (setq beg (line-beginning-position))
        ;; Skip the agent heading at point-min.  It is the structural
        ;; root of the indirect buffer (always at ref-level − 1) and
        ;; must never be rebased.  Buffer modifications on or near this
        ;; line (e.g. create-subtree inserting a newline at eol, or
        ;; org-set-tags adjusting tags) can cause beg to snap back to
        ;; point-min after line-beginning-position expansion.
        (when (= beg (point-min))
          (forward-line 1)
          (setq beg (point)))
        (goto-char end)
        (unless (bolp) (setq end (line-beginning-position 2)))
        ;; Use a marker for end so it tracks insertions during correction
        (let ((end-marker (copy-marker end)))
          (goto-char beg)
          (while (< (point) (marker-position end-marker))
            (let ((line-start (point))
                  (line-text (buffer-substring-no-properties
                              (point) (line-end-position))))
              (cond
               ;; Convert markdown opening fence to org src block
               ((and (not (gptel-org--in-example-block-p))
                     (string-match
                      "^[ \t]*```\\([[:alnum:]_+-]*\\)[ \t]*$" line-text))
                (let ((lang (match-string 1 line-text)))
                  (delete-region line-start (line-end-position))
                  (insert (if (string-empty-p lang)
                              "#+begin_src"
                            (concat "#+begin_src " lang))))
                (setq modified t))
               ;; Convert markdown closing fence to org end_src
               ((and (string-match-p "^[ \t]*```[ \t]*$" line-text)
                     (not (gptel-org--in-example-block-p)))
                (delete-region line-start (line-end-position))
                (insert "#+end_src")
                (setq modified t))
               ;; Escape lines in example blocks that start with * or #+
               ((and (string-match-p "^\\*\\|^#\\+" line-text)
                     (gptel-org--in-example-block-p))
                (goto-char line-start)
                (insert ",")
                (setq modified t))
               ;; Remove incorrect comma from outer block closers.
               ;; AI models sometimes comma-escape #+end_example or
               ;; #+end_src even when it is the outermost block closer.
               ;; Org does not strip commas from delimiter lines, so
               ;; ,#+end_example is never recognised as a block closer
               ;; and the block runs to end-of-file.
               ;; The check: line matches ,#+end_TYPE and
               ;; `gptel-org--in-example-block-p' returns nil (meaning
               ;; this line IS recognised as the closer by the
               ;; comma-aware find-block-end-pos, so it is not content).
               ((and (string-match-p
                      "^[ \t]*,#\\+end_\\(example\\|src\\)[ \t]*$"
                      line-text)
                     (not (gptel-org--in-example-block-p)))
                (goto-char line-start)
                (skip-chars-forward " \t")
                (delete-char 1)         ; remove the comma
                (setq modified t))
               ;; Rebase heading levels (only outside blocks)
               ((and (string-match "^\\(\\*+\\) " line-text)
                     (not (gptel-org--in-example-block-p)))
                (let ((current-stars (length (match-string 1 line-text))))
                  (when (and (eq gptel-log-level 'debug)
                             (string-match-p "^\\*+ \\(TOOL\\|REASONING\\) " line-text))
                    (gptel--log
                     (format "auto-correct: found %s heading stars=%d ref-level=%d offset=%d will-rebase=%S line=\"%s\""
                             (if (string-match-p "TOOL" line-text) "TOOL" "REASONING")
                             current-stars ref-level offset
                             (< current-stars ref-level)
                             (truncate-string-to-width line-text 80))
                     "tool-heading-debug" 'no-json))
                  ;; Only rebase headings that are too shallow.
                  ;; Headings at or above ref-level are already correct.
                  (when (< current-stars ref-level)
                    (let ((new-stars (make-string
                                     (+ current-stars offset) ?*)))
                      (goto-char line-start)
                      (delete-char current-stars)
                      (insert new-stars)
                      (setq modified t)))))))
            (forward-line 1))
          (set-marker end-marker nil)))
      ;; The auto-corrector runs inside after-change-functions with
      ;; inhibit-modification-hooks t, so the org-element cache does not
      ;; see the secondary edits (heading rebasing, fence conversion,
      ;; escaping).  Reset the cache when changes were made so that
      ;; subsequent callers of org-end-of-subtree, org-element-at-point,
      ;; etc. get correct results.
      (when modified
        (org-element-cache-reset)))))

(defun gptel-org--enable-auto-correct ()
  "Enable the idempotent auto-corrector in the current indirect buffer.
Computes `gptel-org--ref-level' from the agent heading at `point-min'
and registers `gptel-org--auto-correct-on-change' as a buffer-local
`after-change-functions' hook.

This should be called once when an agent indirect buffer is created."
  (when (and (bound-and-true-p gptel-org-use-todo-keywords)
             (gptel-org--in-agent-indirect-buffer-p))
    (let ((ref-level (gptel-org--compute-response-level)))
      (when ref-level
        (setq-local gptel-org--ref-level ref-level)
        (add-hook 'after-change-functions
                  #'gptel-org--auto-correct-on-change nil t)
        (gptel-org--debug
         "auto-correct enabled: ref-level=%d buffer=%S"
         ref-level (buffer-name))))))



;;; Response heading title generation

(defun gptel-org-response-title-from-first-line (beg end _heading-pos)
  "Generate a response heading title from the first line of response.
BEG and END delimit the response region.  Returns the first
non-empty, non-block line, truncated to 50 characters.

Skips over REASONING and TOOL heading subtrees, and `gptel-tool'
source blocks at the start of the response so that the title
reflects the actual content."
  (save-excursion
    (goto-char beg)
    (let ((first-line nil))
      (while (and (not first-line) (< (point) end))
        ;; Skip blank lines
        (skip-chars-forward " \t\n")
        (when (>= (point) end) (cl-return nil))
        (let ((line (string-trim
                     (buffer-substring-no-properties
                      (point) (min end (line-end-position))))))
          (cond
           ;; Skip REASONING heading subtrees
           ((string-match-p "^\\*+ REASONING " line)
            (let ((stars (progn (string-match "^\\(\\*+\\)" line)
                                (length (match-string 1 line)))))
              (forward-line 1)
              (if (re-search-forward
                   (format "^\\*\\{1,%d\\} " stars) end t)
                  (beginning-of-line)
                (goto-char end))))
           ;; Skip TOOL heading subtrees
           ((string-match-p "^\\*+ TOOL " line)
            (let ((stars (progn (string-match "^\\(\\*+\\)" line)
                                (length (match-string 1 line)))))
              (forward-line 1)
              (if (re-search-forward
                   (format "^\\*\\{1,%d\\} " stars) end t)
                  (beginning-of-line)
                (goto-char end))))
           ;; Skip org src blocks (tool blocks, etc.)
           ((string-match-p "^#\\+begin_src" line)
            (if (re-search-forward "^#\\+end_src" end t)
                (forward-line 1)
              (goto-char end)))
           ;; Skip markdown code fences
           ((string-match-p "^```" line)
            (forward-line 1)
            (unless (re-search-forward "^```" end t)
              (goto-char end))
            (forward-line 1))
           ;; Skip empty lines and bare list markers
           ((or (string-empty-p line)
                (string-match-p "^[-*+] *$" line))
            (forward-line 1))
           ;; Found usable text
           (t (setq first-line line)))))
      (when first-line
        (truncate-string-to-width first-line 50 nil nil "...")))))

(defun gptel-org--find-response-heading (pos)
  "Find the assistant response heading containing or just before POS.
Returns the position of the heading, or nil if not found.

Recognizes headings with the `gptel-org-assistant-tag' (usually
\":assistant:\"), agent tags matching \"*@agent\", or chat heading
markers like \"@assistant\"."
  (save-excursion
    (goto-char pos)
    ;; Go to beginning of line to handle being at end of heading line
    (beginning-of-line)
    (gptel-org--debug "find-response-heading: pos=%d buf=%S indirect=%s at-heading=%s"
                      pos (buffer-name) (if (buffer-base-buffer) "yes" "no")
                      (org-at-heading-p))
    (if (and (org-at-heading-p)
             (or (gptel-org--heading-is-assistant-p)
                 (gptel-org--heading-has-agent-tag-p)
                 (gptel-org--chat-heading-p
                  (org-get-heading t t t t))))
        (progn
          (gptel-org--debug "find-response-heading: found at pos (heading at point) tags=%S heading=%S"
                            (org-get-tags nil t)
                            (org-get-heading t t t t))
          (point))
      ;; Search backward for assistant heading
      (gptel-org--debug "find-response-heading: not at matching heading, searching backward")
      (if (re-search-backward org-heading-regexp nil t)
          (let ((has-assistant (gptel-org--heading-is-assistant-p))
                (has-agent (gptel-org--heading-has-agent-tag-p))
                (is-chat (gptel-org--chat-heading-p
                          (org-get-heading t t t t))))
            (gptel-org--debug "find-response-heading: backward search found heading at %d tags=%S heading=%S assistant=%s agent=%s chat=%s"
                              (point) (org-get-tags nil t)
                              (org-get-heading t t t t)
                              has-assistant has-agent is-chat)
            (when (or has-assistant has-agent is-chat)
              (point)))
        (gptel-org--debug "find-response-heading: no heading found searching backward")
        nil))))

(defun gptel-org--set-heading-title (heading-pos title &optional todo-keyword)
  "Set the title of the heading at HEADING-POS to TITLE.
Preserves existing tags on the heading.  When TODO-KEYWORD is
non-nil, include it as the TODO keyword on the heading."
  (when (and heading-pos title (not (string-empty-p title)))
    (save-excursion
      (goto-char heading-pos)
      (when (org-at-heading-p)
        (let* ((tags (org-get-tags nil t))
               (level (org-outline-level))
               (stars (make-string level ?*)))
          ;; Replace the entire heading line
          (beginning-of-line)
          (delete-region (point) (line-end-position))
          (if todo-keyword
              (insert stars " " todo-keyword " " title)
            (insert stars " " title))
          ;; Re-apply tags
          (when tags
            (org-set-tags tags)))))))

(defun gptel-org--apply-response-title (beg end)
  "Apply a title to the response heading if configured.
Called from `gptel-post-response-functions' with BEG and END
positions of the response."
  (gptel-org--debug "apply-response-title: beg=%d end=%d buf=%S title-fn=%S org-mode=%s"
                    beg end (buffer-name)
                    gptel-org-response-title-function
                    (derived-mode-p 'org-mode))
  (if (not gptel-org-response-title-function)
      (gptel-org--debug "apply-response-title: SKIPPED - no title function configured")
    (if gptel-org-use-todo-keywords
        (gptel-org--debug "apply-response-title: SKIPPED - using TODO keywords, AI wrote its own heading")
    (if (not (derived-mode-p 'org-mode))
        (gptel-org--debug "apply-response-title: SKIPPED - not org-mode")
      (let ((heading-pos (gptel-org--find-response-heading beg)))
        (gptel-org--debug "apply-response-title: heading-pos=%S" heading-pos)
        (if (not heading-pos)
            (gptel-org--debug "apply-response-title: SKIPPED - no response heading found")
          (condition-case err
              (let ((title (funcall gptel-org-response-title-function
                                    beg end heading-pos))
                    ;; For agent headings, include the done keyword
                    (done-keyword
                     (when (and (gptel-org--in-agent-indirect-buffer-p)
                                (boundp 'gptel-org-tasks-done-keyword)
                                gptel-org-tasks-done-keyword)
                       gptel-org-tasks-done-keyword)))
                (gptel-org--debug "apply-response-title: generated title=%S done-keyword=%S"
                                  title done-keyword)
                (when title
                  (gptel-org--set-heading-title heading-pos title done-keyword)
                  (gptel-org--debug "apply-response-title: title applied successfully")))
            (error
             (gptel-org--debug "apply-response-title: ERROR %S" err)
             (message "gptel: Error generating response title: %S" err)))))))))

;; Add hook with high priority (run late, after heading adjustments)
(add-hook 'gptel-post-response-functions #'gptel-org--apply-response-title 80)



;;; Reasoning indirect buffer display

(defun gptel-org--reasoning-create-indirect-buffer (reasoning-heading-pos)
  "Create an indirect buffer for the REASONING heading at REASONING-HEADING-POS.

Creates an indirect buffer narrowed to the REASONING subtree and
displays it in a side window.  The buffer auto-expands as streaming
content is inserted.

Delegates buffer creation to `gptel-org-ib-create' for consistent
indirect buffer management (fold decoupling, end-marker, registry
tracking).  Applies reasoning-specific setup: read-only mode,
subtree unfolding, side window display.

Returns the indirect buffer, or nil if creation failed."
  (gptel-org--debug
   "reasoning IB create: pos=%S cur-buf=%S base=%S"
   reasoning-heading-pos (buffer-name)
   (when (buffer-base-buffer) (buffer-name (buffer-base-buffer))))
  (condition-case err
      (let* ((base-buf (or (buffer-base-buffer (current-buffer))
                           (current-buffer)))
             (buf-name (format "*gptel-reasoning:%s*"
                               (buffer-name (current-buffer))))
             indirect-buf)
        (gptel-org--debug
         "reasoning IB create: base-buf=%S buf-name=%S"
         (buffer-name base-buf) buf-name)
        ;; Delegate creation to the indirect buffer module
        (setq indirect-buf (gptel-org-ib-create
                            base-buf reasoning-heading-pos buf-name))
        (gptel-org--debug
         "reasoning IB create: gptel-org-ib-create returned %S (live=%S)"
         (when indirect-buf (buffer-name indirect-buf))
         (when indirect-buf (buffer-live-p indirect-buf)))
        (with-current-buffer indirect-buf
          ;; Unfold the REASONING heading so content is visible
          (goto-char (point-min))
          (when (org-at-heading-p)
            (org-fold-subtree nil))
          ;; Make buffer read-only for the user (content comes from streaming)
          (setq buffer-read-only t)
          ;; Clean up tracking var if user kills the buffer externally
          (add-hook 'kill-buffer-hook
                    (lambda ()
                      (let ((this-buf (current-buffer)))
                        (when-let* ((base (buffer-base-buffer this-buf)))
                          (with-current-buffer base
                            (when (eq gptel-org--reasoning-indirect-buffer
                                      this-buf)
                              (setq gptel-org--reasoning-indirect-buffer nil))))))
                    nil t))
        ;; Display in side window
        (gptel-org--debug "reasoning IB create: displaying in side window")
        (display-buffer indirect-buf
                        '((display-buffer-in-side-window)
                          (side . bottom)
                          (slot . 0)
                          (window-height . 0.3)
                          (preserve-size . (nil . t))))
        ;; Store reference in base buffer
        (with-current-buffer base-buf
          (setq gptel-org--reasoning-indirect-buffer indirect-buf))
        ;; Also store in the current buffer if it's an IB itself
        (unless (eq (current-buffer) base-buf)
          (setq gptel-org--reasoning-indirect-buffer indirect-buf))
        (gptel-org--debug
         "reasoning IB create: success, buffer=%S" (buffer-name indirect-buf))
        indirect-buf)
    (error
     (gptel-org--debug "reasoning IB: creation failed: %S" err)
     nil)))

(defun gptel-org--reasoning-close-indirect-buffer ()
  "Close the reasoning indirect buffer if one exists.

Delegates cleanup to `gptel-org-ib-close' for consistent marker and
registry cleanup.  Handles the reasoning-specific side window
deletion and tracking variable reset.

Safe to call even if no reasoning buffer exists."
  (gptel-org--debug
   "reasoning IB close: cur-buf=%S ib-var=%S ib-live=%S"
   (buffer-name)
   (when gptel-org--reasoning-indirect-buffer
     (buffer-name gptel-org--reasoning-indirect-buffer))
   (when gptel-org--reasoning-indirect-buffer
     (buffer-live-p gptel-org--reasoning-indirect-buffer)))
  (when-let* ((ib gptel-org--reasoning-indirect-buffer)
              ((buffer-live-p ib)))
    (gptel-org--debug
     "reasoning IB close: closing %S, window=%S"
     (buffer-name ib) (get-buffer-window ib t))
    ;; Delete the side window showing the buffer (if any)
    (when-let* ((win (get-buffer-window ib t)))
      (delete-window win))
    ;; Delegate cleanup to the indirect buffer module
    ;; (handles unregister, marker cleanup, kill)
    (gptel-org-ib-close ib))
  (setq gptel-org--reasoning-indirect-buffer nil)
  ;; Also clear in base buffer
  (when-let* ((base (buffer-base-buffer (current-buffer))))
    (with-current-buffer base
      (setq gptel-org--reasoning-indirect-buffer nil))))

;;; Post-response block folding

(defun gptel-org--fold-special-blocks (beg end)
  "Fold REASONING/TOOL headings and gptel-tool source blocks between BEG and END.

Runs as a `gptel-post-response-functions' hook to ensure all special
blocks and headings are folded after the response is complete.  This
supplements the inline folding done during streaming (which can be
unreliable in indirect buffers or when subsequent text insertion
disrupts overlays)."
  (when (derived-mode-p 'org-mode)
    ;; Close any orphaned reasoning indirect buffer (e.g. from errors)
    (when gptel-org--reasoning-indirect-buffer
      (gptel-org--reasoning-close-indirect-buffer))
    (save-excursion
      ;; Fold REASONING heading subtrees
      (goto-char beg)
      (while (re-search-forward "^\\*+ REASONING " end t)
        (beginning-of-line)
        (condition-case nil
            (when (and (org-at-heading-p)
                       (not (org-fold-folded-p (point) 'headline)))
              (org-fold-subtree t))
          (error nil))
        (forward-line 1))
      ;; Fold TOOL heading subtrees
      (goto-char beg)
      (while (re-search-forward "^\\*+ TOOL " end t)
        (beginning-of-line)
        (condition-case nil
            (when (and (org-at-heading-p)
                       (not (org-fold-folded-p (point) 'headline)))
              (org-fold-subtree t))
          (error nil))
        (forward-line 1))
      ;; Fold #+begin_tool special blocks
      (goto-char beg)
      (while (re-search-forward
              "^[ \t]*#\\+begin_tool" end t)
        (beginning-of-line)
        (condition-case nil
            (let ((elem (org-element-at-point)))
              (when (and elem
                         (eq (org-element-type elem) 'special-block)
                         (not (org-fold-folded-p (point) 'block)))
                (org-fold-hide-block-toggle 'hide nil elem)))
          (error nil))
        (if (re-search-forward "^[ \t]*#\\+end_tool" end t)
            (forward-line 1)
          (goto-char end)))
      ;; Fold gptel-tool source blocks (legacy)
      (goto-char beg)
      (while (re-search-forward
              "^[ \t]*#\\+begin_src gptel-tool" end t)
        (beginning-of-line)
        (condition-case nil
            (let ((elem (org-element-at-point)))
              (when (and elem
                         (eq (org-element-type elem) 'src-block)
                         (not (org-fold-folded-p (point) 'block)))
                (org-fold-hide-block-toggle 'hide nil elem)))
          (error nil))
        (if (re-search-forward "^[ \t]*#\\+end_src" end t)
            (forward-line 1)
          (goto-char end))))))

;; Run at priority 90 (after heading adjustment at 0 and title at 80)
(add-hook 'gptel-post-response-functions #'gptel-org--fold-special-blocks 90)


(defcustom gptel-org-assistant-keyword-face
  '(:foreground "#88C0D0" :weight bold)
  "Face specification for the AI assistant TODO keyword.
Applied via `org-todo-keyword-faces' when `gptel-org-use-todo-keywords'
is enabled."
  :type '(plist)
  :group 'gptel)

(defcustom gptel-org-user-keyword-face
  '(:foreground "#EBCB8B" :weight bold)
  "Face specification for the HI user TODO keyword.
Applied via `org-todo-keyword-faces' when `gptel-org-use-todo-keywords'
is enabled."
  :type '(plist)
  :group 'gptel)

(defun gptel-org--register-todo-keywords ()
  "Register AI/HI/REASONING/TOOL TODO keywords and their faces if needed.

When `gptel-org-use-todo-keywords' is enabled, ensures that
`gptel-org-assistant-keyword', `gptel-org-user-keyword', REASONING,
and TOOL are present in `org-todo-keywords' and have faces registered
in `org-todo-keyword-faces'."
  (when gptel-org-use-todo-keywords
    (let ((ai-kw gptel-org-assistant-keyword)
          (hi-kw gptel-org-user-keyword))
      (let ((changed nil))
        ;; Register AI/HI keywords in org-todo-keywords if missing
        (unless (and (cl-some (lambda (seq)
                                (and (listp seq)
                                     (member ai-kw (cl-remove-if-not #'stringp seq))))
                              org-todo-keywords)
                     (cl-some (lambda (seq)
                                (and (listp seq)
                                     (member hi-kw (cl-remove-if-not #'stringp seq))))
                              org-todo-keywords))
          (push (list 'sequence ai-kw hi-kw) org-todo-keywords)
          (setq changed t))
        ;; Register REASONING and TOOL as done-state keywords if missing
        (unless (cl-some (lambda (seq)
                           (and (listp seq)
                                (member "REASONING" (cl-remove-if-not #'stringp seq))))
                         org-todo-keywords)
          (push '(sequence "|" "REASONING" "TOOL") org-todo-keywords)
          (setq changed t))
        ;; Register faces in org-todo-keyword-faces if missing
        (unless (assoc ai-kw org-todo-keyword-faces)
          (push (cons ai-kw gptel-org-assistant-keyword-face)
                org-todo-keyword-faces)
          (setq changed t))
        (unless (assoc hi-kw org-todo-keyword-faces)
          (push (cons hi-kw gptel-org-user-keyword-face)
                org-todo-keyword-faces)
          (setq changed t))
        ;; REASONING face: dimmed/italic to de-emphasize thinking
        (unless (assoc "REASONING" org-todo-keyword-faces)
          (push '("REASONING" . (:foreground "#8FBCBB" :weight light :slant italic))
                org-todo-keyword-faces)
          (setq changed t))
        ;; TOOL face: distinct color for tool execution state
        (unless (assoc "TOOL" org-todo-keyword-faces)
          (push '("TOOL" . (:foreground "#A3BE8C" :weight bold))
                org-todo-keyword-faces)
          (setq changed t))
        ;; Refresh org to pick up changes
        (when (and changed (derived-mode-p 'org-mode))
          (org-mode-restart))))))
;;; Unified Org Mode

;;;###autoload
(define-minor-mode gptel-org-mode
  "Unified org-mode integration for gptel.

This mode provides a single entry point for all gptel org-mode
features.  Enabling it activates:

- `gptel-mode' (core chat functionality)
- `gptel-org-subtree-context' (agent subtree isolation)
- `gptel-org-use-todo-keywords' (AI/HI keyword-based role detection)
- `gptel-org-tasks-mode' (AI-DO/DOING/DONE state machine)
- `gptel-highlight-mode' (visual response highlighting)

Use this instead of manually configuring individual org-mode
settings.  All features can still be customized via their
respective variables after activation.

This mode is only intended for `org-mode' buffers."
  :lighter " GPT-Org"
  :group 'gptel
  (if gptel-org-mode
      (progn
        (unless (derived-mode-p 'org-mode)
          (setq gptel-org-mode nil)
          (user-error "`gptel-org-mode' can only be used in org-mode buffers"))
        ;; Core chat mode
        (unless gptel-mode (gptel-mode 1))
        ;; Org-specific settings
        (setq-local gptel-org-subtree-context t)
        (setq-local gptel-org-use-todo-keywords t)
        ;; Task workflow
        (require 'gptel-org-tasks nil t)
        (when (fboundp 'gptel-org-tasks-mode)
          (gptel-org-tasks-mode 1))
        ;; Visual highlighting
        (when (fboundp 'gptel-highlight-mode)
          (gptel-highlight-mode 1)))
    ;; Disable chain
    (when (fboundp 'gptel-org-tasks-mode)
      (gptel-org-tasks-mode -1))
    (when (fboundp 'gptel-highlight-mode)
      (gptel-highlight-mode -1))
    (kill-local-variable 'gptel-org-subtree-context)
    (kill-local-variable 'gptel-org-use-todo-keywords)))

(provide 'gptel-org)
;;; gptel-org.el ends here

;; Silence warnings about `org-element-type-p' and `org-element-parent', see #294.
;; Local Variables:
;; byte-compile-warnings: (not unresolved)
;; End:
