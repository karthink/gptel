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
(eval-when-compile (require 'gptel))
(require 'cl-lib)
(require 'org-element)
(require 'outline)
(require 'mailcap)                    ;FIXME Avoid this somehow

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

;; Recursive guards for advice functions
(defvar gptel-org--in-send-with-props nil
  "Non-nil when inside `gptel-org--send-with-props' to prevent recursion.")
(defvar gptel-org--in-prefix-advice nil
  "Non-nil when inside prefix advice functions to prevent recursion.")

;; Debug support
(defvar gptel-org-debug nil
  "When non-nil, output debug messages for subtree context operations.
Set to t to enable debug output to *Messages* buffer.
Useful for diagnosing heading level issues.")

(defun gptel-org--debug (format-string &rest args)
  "Output debug message when `gptel-org-debug' is non-nil.
FORMAT-STRING and ARGS are passed to `message'."
  (when gptel-org-debug
    (apply #'message (concat "[gptel-org] " format-string) args)))

(defvar org-link-angle-re)
(defvar org-link-bracket-re)
(declare-function mailcap-file-name-to-mime-type "mailcap")
(declare-function gptel--model-capable-p "gptel")
(declare-function gptel--model-mime-capable-p "gptel")
(declare-function gptel--model-name "gptel")
(declare-function gptel--to-string "gptel")
(declare-function gptel--to-number "gptel")
(declare-function gptel--intern "gptel")
(declare-function gptel--get-buffer-bounds "gptel")
(declare-function gptel-backend-name "gptel")
(declare-function gptel--parse-buffer "gptel")
(declare-function gptel--parse-directive "gptel")
(declare-function gptel--restore-props "gptel")
(declare-function gptel--with-buffer-copy "gptel")
(declare-function gptel--file-binary-p "gptel")
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
  "Include sibling conversation headings in context when enabled.

When non-nil, gptel will include sibling @user and @assistant
headings under the same parent heading in the context.  This is
useful for task-oriented workflows where conversations happen
under TODO headings.

The user prompt prefix (`gptel-prompt-prefix-alist') and response
prefix (`gptel-response-prefix-alist') will be adjusted to create
headings one level deeper than the parent heading.

Example structure:
  *** TODO Some task
  My question here
  **** :assistant:
  Response here
  ***** Details
  More details in subheading
  **** :user: User feedback here
  This message should be last in the context just before system message

When sending from under a TODO heading with this option enabled,
the @user and @assistant siblings will be included in context,
and new responses will be inserted at the correct heading level."
  :type 'boolean
  :group 'gptel)

(defcustom gptel-org-chat-heading-markers '("@user" "@assistant")
  "List of heading text markers that indicate chat entries.

These markers are used to identify conversation turns when
`gptel-org-subtree-context' is enabled.  Headings containing any
of these strings will be recognized as part of the conversation."
  :type '(repeat string)
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

(defun gptel-org--get-parent-heading-level ()
  "Return the level of the task/conversation heading.

This finds the heading that should contain the chat entries.  It walks
up the heading hierarchy looking for the first heading whose direct
children include chat headings (based on `gptel-org-chat-heading-markers'),
or a heading that is not inside a chat subtree.

This is used to determine the correct heading level for chat entries
when `gptel-org-subtree-context' is enabled."
  (save-excursion
    ;; First, get to the current heading if not already there
    (let ((started-at-heading (org-at-heading-p))
          (start-pos (point)))
      (gptel-org--debug "get-parent-heading-level: start at pos %d, line %d, at-heading=%s"
                        start-pos (line-number-at-pos) started-at-heading)
      (unless started-at-heading
        (ignore-errors (outline-back-to-heading t)))
      (gptel-org--debug "get-parent-heading-level: after back-to-heading at line %d: %s"
                        (line-number-at-pos)
                        (buffer-substring (line-beginning-position) (line-end-position)))
      ;; Check if we're inside a chat subtree by looking at ancestors
      (let ((inside-chat-subtree
             (save-excursion
               (let ((in-chat nil))
                 (while (and (not in-chat)
                             (ignore-errors (outline-up-heading 1 t)))
                   (when (gptel-org--chat-heading-p)
                     (setq in-chat t)))
                 in-chat))))
        (gptel-org--debug "get-parent-heading-level: inside-chat-subtree=%s" inside-chat-subtree)
        ;; Walk up to find the task heading
        ;; A heading is the task heading if:
        ;; 1. It's not a chat heading itself, AND
        ;; 2. It has at least one direct child that is a chat heading,
        ;;    OR we started from body text NOT inside a chat subtree (new conversation)
        ;;    OR we've walked out of all chat-related subtrees
        (let ((found nil)
              (started-from-chat (and started-at-heading
                                      (gptel-org--chat-heading-p))))
          (gptel-org--debug "get-parent-heading-level: started-from-chat=%s" started-from-chat)
          (while (and (org-at-heading-p) (not found))
            (let ((current-heading (buffer-substring (line-beginning-position) (line-end-position)))
                  (is-chat (gptel-org--chat-heading-p)))
              (gptel-org--debug "get-parent-heading-level: checking heading: %s (is-chat=%s)"
                                current-heading is-chat)
              (if is-chat
                  ;; This is a chat heading, keep going up
                  (ignore-errors (outline-up-heading 1 t))
                ;; Not a chat heading - this is the parent if:
                ;; - It has chat children, OR
                ;; - We started from body/heading NOT inside a chat subtree (new conversation), OR
                ;; - We walked up from a chat heading
                (let ((current-level (org-outline-level))
                      (has-chat-child nil))
                  (save-excursion
                    (outline-next-heading)
                    (when (and (org-at-heading-p)
                               (= (org-outline-level) (1+ current-level))
                               (gptel-org--chat-heading-p))
                      (setq has-chat-child t)))
                  (gptel-org--debug "get-parent-heading-level: non-chat heading level=%d has-chat-child=%s"
                                    current-level has-chat-child)
                  (if (or has-chat-child
                          (and (not started-at-heading)      ; Started from body text
                               (not inside-chat-subtree))    ; and NOT inside chat subtree
                          started-from-chat)                 ; Walked up from chat heading
                      (setq found t)
                    ;; No chat children, continue up
                    (unless (ignore-errors (outline-up-heading 1 t))
                      ;; Can't go up anymore, use this level
                      (setq found t)))))))
          (let ((result (if (org-at-heading-p) (org-outline-level) 0)))
            (gptel-org--debug "get-parent-heading-level: result=%d" result)
            result))))))

(defun gptel-org--chat-heading-p (&optional heading-text)
  "Check if HEADING-TEXT (or current heading) is a chat entry.

Returns non-nil if the heading contains any of the markers in
`gptel-org-chat-heading-markers', or when `gptel-org-infer-bounds-from-tags'
is enabled, if the heading has :assistant: or :user: tags."
  (let* ((text (or heading-text
                   (and (org-at-heading-p)
                        (org-get-heading t t t t))))
         (marker-match
          (and text
               (cl-some (lambda (marker)
                          (string-match-p (regexp-quote marker) text))
                        gptel-org-chat-heading-markers)))
         (tag-match
          (and gptel-org-infer-bounds-from-tags
               (null heading-text)
               (org-at-heading-p)
               (or (gptel-org--heading-has-tag-p gptel-org-assistant-tag)
                   (gptel-org--heading-has-tag-p gptel-org-user-tag))))
         (result (or marker-match tag-match)))
    (gptel-org--debug "chat-heading-p: text=%S marker-match=%s tag-match=%s result=%s"
                      text marker-match tag-match result)
    result))

(defun gptel-org--get-chat-siblings ()
  "Get bounds of all sibling chat headings under the task heading.

Returns a list of (BEG . END) cons cells for each sibling heading
that matches `gptel-org-chat-heading-markers'.  Used when
`gptel-org-subtree-context' is enabled.

This function finds the task heading (the heading whose direct
children are chat headings) and collects all its direct children
that are chat headings."
  (when gptel-org-subtree-context
    (save-excursion
      (let ((siblings nil)
            (task-level (gptel-org--get-parent-heading-level)))
        (when (> task-level 0)
          ;; Navigate to the task heading
          (unless (org-at-heading-p)
            (ignore-errors (outline-back-to-heading t)))
          ;; Walk up to find the task heading
          (while (and (org-at-heading-p)
                      (> (org-outline-level) task-level))
            (ignore-errors (outline-up-heading 1 t)))
          ;; Now we should be at the task heading
          (when (and (org-at-heading-p)
                     (= (org-outline-level) task-level))
            (let ((chat-level (1+ task-level)))
              (outline-next-heading)
              ;; Collect all immediate children that are chat entries
              (while (and (not (eobp))
                          (looking-at outline-regexp)
                          (> (org-outline-level) task-level))
                (when (and (= (org-outline-level) chat-level)
                           (gptel-org--chat-heading-p))
                  (let ((beg (point))
                        (end (save-excursion
                               (org-end-of-subtree t t)
                               (point))))
                    (push (cons beg end) siblings)))
                (outline-next-heading)))))
        (nreverse siblings)))))

(defvar-local gptel-org--pending-heading-tag nil
  "Tag to apply to the heading after prefix insertion.
A cons cell (MARKER . TAG) where MARKER is a marker and TAG is the
tag name to apply using `org-set-tags'.")

(defun gptel-org--apply-pending-tag-on-change (beg _end _len)
  "Apply pending heading tag after insertion at BEG.
This is called from `after-change-functions' to apply tags using
`org-set-tags' after a heading prefix has been inserted."
  (when gptel-org--pending-heading-tag
    (let ((marker (car gptel-org--pending-heading-tag))
          (tag (cdr gptel-org--pending-heading-tag)))
      ;; Only apply if the change is near where we expect the heading
      (when (and (markerp marker)
                 (marker-buffer marker)
                 (<= (abs (- beg (marker-position marker))) 50))
        (gptel-org--debug "apply-pending-tag-on-change: beg=%d marker=%d tag=%S"
                          beg (marker-position marker) tag)
        (save-excursion
          (goto-char beg)
          ;; The heading starts at beg (the insertion point), so we need to
          ;; either be at a heading or search forward, not backward.
          ;; First check if we're at the heading (most common case).
          (unless (org-at-heading-p)
            ;; If not directly at heading, search forward within the inserted region
            (re-search-forward org-heading-regexp nil t)
            (beginning-of-line))
          (when (org-at-heading-p)
            (gptel-org--debug "apply-pending-tag-on-change: found heading at %d"
                              (point))
            ;; Use org-set-tags to properly set the tag with alignment
            (org-set-tags (list tag))))
        ;; Clean up the marker and remove this hook
        (set-marker marker nil)
        (setq gptel-org--pending-heading-tag nil)
        (remove-hook 'after-change-functions
                     #'gptel-org--apply-pending-tag-on-change t)))))

(defun gptel-org--dynamic-prefix-string (base-prefix &optional for-prompt)
  "Return BASE-PREFIX adjusted for current org heading context.

When `gptel-org-subtree-context' is enabled and we're in an org
buffer under a heading, adjust the number of stars in BASE-PREFIX
to be one level deeper than the parent heading.

When `gptel-org-infer-bounds-from-tags' is enabled, the prefix will
include the tag inline for consistency with regex matching.  Additionally,
a pending tag operation is scheduled so that after the heading is inserted,
`org-set-tags' will be called to properly align the tag and run
`org-after-tags-change-hook'.

The user types their content BELOW the heading line (in the body),
not on the heading line itself.  This ensures proper org structure
with tags at the end of headings.

If BASE-PREFIX starts with stars, those stars are replaced with
the correct number.  If BASE-PREFIX doesn't start with stars
\(e.g. \"@user\\n\"), stars are prepended to create a proper
org heading."
  (if (and gptel-org-subtree-context
           (derived-mode-p 'org-mode))
      (let* ((parent-level (gptel-org--get-parent-heading-level))
             (target-level (if (> parent-level 0) (1+ parent-level) 1))
             (stars (make-string target-level ?*)))
        (gptel-org--debug "dynamic-prefix-string: base=%S parent-level=%d target-level=%d for-prompt=%s"
                          base-prefix parent-level target-level for-prompt)
        (let ((result
               (cond
                ;; When using tag-based bounds, generate heading with tag and schedule
                ;; org-set-tags call for proper alignment
                (gptel-org-infer-bounds-from-tags
                 (let ((tag (if for-prompt
                                gptel-org-user-tag
                              gptel-org-assistant-tag))
                       (marker (make-marker)))
                   ;; Create a marker at current point to track insertion location
                   (set-marker marker (point))
                   ;; Store the pending tag to be applied after insertion
                   (setq gptel-org--pending-heading-tag (cons marker tag))
                   ;; Ensure the after-change function is active
                   (add-hook 'after-change-functions
                             #'gptel-org--apply-pending-tag-on-change nil t)
                   ;; Return heading with inline tag (for regex matching compatibility)
                   ;; The tag will be re-set properly via org-set-tags after insertion
                   (concat stars " :" tag ":\n")))
                ;; Prefix starts with stars - replace them
                ((string-match "^\\(\\*+\\)\\(\\(?:.*\n?\\)?\\)" base-prefix)
                 (let ((rest (match-string 2 base-prefix)))
                   (concat stars rest)))
                ;; Prefix doesn't start with stars but is non-empty - prepend stars and space
                ((not (string-empty-p base-prefix))
                 (concat stars " " base-prefix))
                ;; Empty prefix - just return stars with space and newline
                (t (concat stars " \n")))))
          (gptel-org--debug "dynamic-prefix-string: result=%S pending-tag=%S"
                            result gptel-org--pending-heading-tag)
          result))
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

When `gptel-org-subtree-context' is also enabled, sibling headings
that match `gptel-org-chat-heading-markers' will be included in the
context."
  ;; Refresh bounds from tags before constructing prompt to ensure text
  ;; properties reflect current buffer state (avoids stale markers)
  (when gptel-org-infer-bounds-from-tags
    (gptel-org--restore-bounds-from-tags))
  (when (use-region-p)
    (narrow-to-region (region-beginning) (region-end))
    (setq prompt-end (point-max)))
  (goto-char (or prompt-end (setq prompt-end (point))))
  (let ((topic-start (gptel-org--get-topic-start))
        (chat-siblings (when gptel-org-subtree-context
                         (gptel-org--get-chat-siblings))))
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
                   ;; (car start-bounds) is the begining of the current element,
                   ;; not relevant
                   for pos in (cdr start-bounds)
                   do (goto-char pos) (outline-next-heading)
                   collect (point) into ends
                   finally return (cons prompt-end ends))))
            (gptel--with-buffer-copy org-buf nil nil
              (cl-loop for start in start-bounds
                       for end in end-bounds
                       do (insert-buffer-substring org-buf start end)
                       (goto-char (point-min)))
              ;; When gptel-org-subtree-context is enabled, include sibling
              ;; chat headings (@user/@assistant) in the context.
              ;; Insert siblings in chronological order before the current chat entry.
              (when chat-siblings
                ;; Find the current chat entry's position (the first chat heading
                ;; in start-bounds that is a chat sibling)
                (let* ((current-chat-start (car start-bounds))
                       ;; Filter siblings that come before current entry and don't overlap
                       (earlier-siblings
                        (cl-remove-if
                         (lambda (sib)
                           (let ((sib-beg (car sib))
                                 (sib-end (cdr sib)))
                             ;; Skip if overlaps or comes after current position
                             (or (>= sib-beg current-chat-start)
                                 (cl-some (lambda (start-end)
                                            (and (< sib-beg (cdr start-end))
                                                 (> sib-end (car start-end))))
                                          (cl-mapcar #'cons start-bounds end-bounds)))))
                         chat-siblings)))
                  ;; Insert earlier siblings right after the task heading (before current chat)
                  ;; The task heading content ends where the first chat sibling would start
                  (when earlier-siblings
                    ;; Find where to insert: after the task heading but before current chat
                    ;; In the copied buffer, content from start-bounds was inserted at point-min
                    ;; The task heading content is now at the start
                    (goto-char (point-min))
                    ;; Skip to end of task heading content (first outline heading at chat level)
                    (when (re-search-forward "^\\*\\*" nil t)
                      (beginning-of-line)
                      ;; Insert earlier siblings here, in order
                      (dolist (sib earlier-siblings)
                        (insert-buffer-substring org-buf (car sib) (cdr sib)))))))
              (goto-char (point-max))
              (gptel-org--unescape-tool-results)
              (gptel-org--strip-block-headers)
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
          ;; When gptel-org-subtree-context is enabled, include sibling
          ;; chat headings (@user/@assistant) in the context
          (when chat-siblings
            (goto-char (point-max))
            (cl-loop for (sib-beg . sib-end) in chat-siblings
                     ;; Skip siblings that overlap with the range [beg, prompt-end]
                     ;; Overlap: sib-beg < prompt-end AND sib-end > beg
                     unless (and (< sib-beg prompt-end) (> sib-end beg))
                     do (insert-buffer-substring org-buf sib-beg sib-end)))
          (goto-char (point-max))
          (gptel-org--unescape-tool-results)
          (gptel-org--strip-block-headers)
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
  "Remove all gptel-specific block headers and footers.
Every line that matches will be removed entirely.

This removal is necessary to avoid auto-mimicry by LLMs."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            (rx line-start (literal "#+")
                (or (literal "begin") (literal "end"))
                (or (literal "_tool") (literal "_reasoning")))
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
            (when (looking-at-p "[[:space:]]*#\\+begin_tool")
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
          (pcase-let* ((`(,gptel--system-message ,gptel-backend ,gptel-model
                          ,gptel-temperature ,gptel-max-tokens
                          ,gptel--num-messages-to-send ,gptel-tools)
                        (seq-mapn (lambda (a b) (or a b))
                                  (gptel-org--entry-properties)
                                  (list gptel--system-message gptel-backend gptel-model
                                        gptel-temperature gptel-max-tokens
                                        gptel--num-messages-to-send gptel-tools)))
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
      ((`(,system ,backend ,model ,temperature ,tokens ,num ,tools)
         (mapcar
          (lambda (prop) (org-entry-get (or pt (point)) prop 'selective))
          '("GPTEL_SYSTEM" "GPTEL_BACKEND" "GPTEL_MODEL"
            "GPTEL_TEMPERATURE" "GPTEL_MAX_TOKENS"
            "GPTEL_NUM_MESSAGES_TO_SEND" "GPTEL_TOOLS"))))
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
                   for tool = (with-demoted-errors "gptel: %S"
                                (gptel-get-tool tname))
                   if tool collect tool else do
                   (display-warning
                    '(gptel org tools)
                    (format "Tool %s not found, ignoring" tname)))))
    (list system backend model temperature tokens num tools)))

(defun gptel-org--heading-has-tag-p (tag)
  "Check if current heading has TAG (case-insensitive)."
  (when (org-at-heading-p)
    (let ((tags (org-get-tags nil t)))  ; local tags only
      (cl-some (lambda (tg) (string-equal-ignore-case tg tag)) tags))))

(defun gptel-org--heading-has-todo-keyword-p ()
  "Check if current heading has a TODO keyword from `gptel-org-todo-keywords'."
  (when (org-at-heading-p)
    (let ((todo (org-get-todo-state)))
      (and todo (member todo gptel-org-todo-keywords)))))

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
  "Restore gptel response properties based on heading tags.

Scans all headings in the buffer for :assistant: and :user: tags
\(as configured by `gptel-org-assistant-tag' and `gptel-org-user-tag').
Headings tagged with the assistant tag get their subtree content
marked with the gptel response property.

User-tagged headings within an assistant subtree are treated as
user feedback - their content is NOT marked as assistant response.

Returns non-nil if any tagged headings were found and processed."
  (save-excursion
    (goto-char (point-min))
    (let ((found-tags nil))
      (while (outline-next-heading)
        (cond
         ;; Assistant tag - mark subtree as response, but respect user tags within
         ((gptel-org--heading-has-tag-p gptel-org-assistant-tag)
          (setq found-tags t)
          (let ((assistant-beg (point))
                (assistant-end (save-excursion
                                 (org-end-of-subtree t t)
                                 (point))))
            ;; First, mark the entire assistant subtree as response
            (add-text-properties assistant-beg assistant-end
                                 '(gptel response front-sticky (gptel)))
            ;; Then, scan for user tags within and remove gptel property from those
            (save-excursion
              (goto-char assistant-beg)
              (while (and (outline-next-heading)
                          (< (point) assistant-end))
                (when (gptel-org--heading-has-tag-p gptel-org-user-tag)
                  (let ((user-beg (point))
                        (user-end (save-excursion
                                    (org-end-of-subtree t t)
                                    (min (point) assistant-end))))
                    (remove-text-properties user-beg user-end '(gptel nil))))))))
         ;; User tag at top level - ensure no gptel property (remove if present)
         ((gptel-org--heading-has-tag-p gptel-org-user-tag)
          (setq found-tags t)
          (let ((beg (point))
                (end (save-excursion
                       (org-end-of-subtree t t)
                       (point))))
            (remove-text-properties beg end '(gptel nil))))))
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
            ;; Try tag-based bounds first if enabled
            (if (and gptel-org-infer-bounds-from-tags
                     (gptel-org--restore-bounds-from-tags))
                (setq gptel-org--bounds-from-tags t)
              ;; Fall back to GPTEL_BOUNDS property
              (setq gptel-org--bounds-from-tags nil)
              (when-let* ((bounds (org-entry-get (point-min) "GPTEL_BOUNDS")))
                (gptel--restore-props (read bounds))))
            (pcase-let ((`(,system ,backend ,model ,temperature ,tokens ,num ,tools)
                         (gptel-org--entry-properties (point-min))))
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
      (set-buffer-modified-p modified))))

(defun gptel-org-set-properties (pt &optional msg)
  "Store the active gptel configuration under the current heading.

The active gptel configuration includes the current system
message, language model and provider (backend), and additional
settings when applicable.

PT is the cursor position by default.  If MSG is
non-nil (default), display a message afterwards."
  (interactive (list (point) t))
  (org-entry-put pt "GPTEL_MODEL" (gptel--model-name gptel-model))
  (org-entry-put pt "GPTEL_BACKEND" (gptel-backend-name gptel-backend))
  (org-entry-put pt "GPTEL_SYSTEM"      ;TODO: Handle nil case correctly
                 (and-let* ((msg (car-safe
                                  (gptel--parse-directive
                                   gptel--system-message))))
                   (string-replace "\n" "\\n" msg)))
  (if gptel-tools
      (org-entry-put
       pt "GPTEL_TOOLS" (mapconcat #'gptel-tool-name gptel-tools " "))
    (org-entry-delete pt "GPTEL_TOOLS"))
  (if (equal (default-value 'gptel-temperature) gptel-temperature)
      (org-entry-delete pt "GPTEL_TEMPERATURE")
    (org-entry-put pt "GPTEL_TEMPERATURE"
                   (number-to-string gptel-temperature)))
  (if (natnump gptel--num-messages-to-send)
      (org-entry-put pt "GPTEL_NUM_MESSAGES_TO_SEND"
                     (number-to-string gptel--num-messages-to-send))
    (org-entry-delete pt "GPTEL_NUM_MESSAGES_TO_SEND"))
  (if gptel-max-tokens
      (org-entry-put
       pt "GPTEL_MAX_TOKENS" (number-to-string gptel-max-tokens))
    (org-entry-delete pt "GPTEL_MAX_TOKENS"))
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

(defun gptel-org--advice-prompt-prefix (orig-fun)
  "Advice for `gptel-prompt-prefix-string' to support dynamic org heading levels.

ORIG-FUN is the original function.  When `gptel-org-subtree-context'
is enabled, adjusts the prefix to use the correct heading level."
  (if gptel-org--in-prefix-advice
      (funcall orig-fun)
    (let ((gptel-org--in-prefix-advice t))
      (let ((result (funcall orig-fun)))
        (gptel-org--debug "advice-prompt-prefix: orig=%S org-mode=%s subtree-context=%s"
                          result (derived-mode-p 'org-mode) gptel-org-subtree-context)
        (if (derived-mode-p 'org-mode)
            (gptel-org--dynamic-prefix-string result 'for-prompt)
          result)))))

(defun gptel-org--advice-response-prefix (orig-fun)
  "Advice for `gptel-response-prefix-string' to support dynamic org heading levels.

ORIG-FUN is the original function.  When `gptel-org-subtree-context'
is enabled, adjusts the prefix to use the correct heading level."
  (if gptel-org--in-prefix-advice
      (funcall orig-fun)
    (let ((gptel-org--in-prefix-advice t))
      (let ((result (funcall orig-fun)))
        (gptel-org--debug "advice-response-prefix: orig=%S org-mode=%s subtree-context=%s"
                          result (derived-mode-p 'org-mode) gptel-org-subtree-context)
        (if (derived-mode-p 'org-mode)
            (gptel-org--dynamic-prefix-string result nil) ;nil = for response
          result)))))

(advice-add 'gptel-prompt-prefix-string :around #'gptel-org--advice-prompt-prefix)
(advice-add 'gptel-response-prefix-string :around #'gptel-org--advice-response-prefix)


;;; Response heading adjustment for subtree context

(defun gptel-org--escape-example-blocks (beg end)
  "Prefix lines in example blocks with comma between BEG and END.

Per Org manual, lines starting with `*' or `#+' inside example blocks
must be prefixed with a comma to prevent them from being interpreted
as outline nodes or special syntax.  Org strips these commas when
accessing the block contents.

Lines already escaped (starting with `,*' or `,#+') are left unchanged.

This should be called before `gptel-org--adjust-response-headings'
so that headings inside examples are not modified."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      ;; Find each example/src block and escape special lines within
      (while (re-search-forward
              "^[ \t]*#\\+begin_\\(example\\|src\\)\\(?:[ \t]\\|$\\)" nil t)
        (let ((block-start (line-beginning-position 2)) ; line after #+begin
              (block-end nil))
          ;; Find the matching #+end
          (when (re-search-forward
                 "^[ \t]*#\\+end_\\(example\\|src\\)[ \t]*$" nil t)
            (setq block-end (line-beginning-position))
            ;; Escape lines within the block
            (save-restriction
              (narrow-to-region block-start block-end)
              (goto-char (point-min))
              (while (not (eobp))
                ;; Only escape lines starting with * or #+, NOT already-escaped ,* or ,#+
                (when (looking-at "^\\(\\*\\|#\\+\\)")
                  (insert ","))
                (forward-line 1)))))))))

(defun gptel-org--in-example-block-p ()
  "Return non-nil if point is inside an example or src block."
  (save-excursion
    (let ((pos (point))
          (in-block nil))
      (goto-char (point-min))
      (while (and (not in-block)
                  (re-search-forward
                   "^[ \t]*#\\+begin_\\(example\\|src\\)\\(?:[ \t]\\|$\\)" pos t))
        (let ((block-start (point)))
          (when (re-search-forward
                 "^[ \t]*#\\+end_\\(example\\|src\\)[ \t]*$" nil t)
            (when (and (>= pos block-start)
                       (<= pos (point)))
              (setq in-block t)))))
      in-block)))

(defun gptel-org--adjust-response-headings (beg end)
  "Adjust heading levels in the response region from BEG to END.

When `gptel-org-subtree-context' is enabled, any org headings in
the AI response should be demoted to be children of the @assistant
heading.  This prevents response headings from escaping the
assistant subtree and breaking the conversation structure.

First, lines in example blocks that start with `*' or `#+' are
prefixed with comma per Org manual requirements.  Then headings
outside of example blocks are adjusted.

For example, if the @assistant heading is at level 4 (****), any
headings in the response should be at level 5 or deeper."
  (when (and gptel-org-subtree-context
             (derived-mode-p 'org-mode))
    ;; First: escape special lines in example blocks
    (gptel-org--escape-example-blocks beg end)
    (save-excursion
      ;; Find the @assistant heading level BEFORE narrowing
      (let ((assistant-level
             (save-excursion
               (goto-char beg)
               (if (re-search-backward org-outline-regexp-bol nil t)
                   (org-outline-level)
                 1)))
            (min-response-level nil))
        (save-restriction
          (narrow-to-region beg end)
          ;; First pass: find the minimum heading level in the response
          ;; (only for headings outside example blocks)
          (goto-char (point-min))
          (while (re-search-forward org-outline-regexp-bol nil t)
            (unless (gptel-org--in-example-block-p)
              (let ((level (org-outline-level)))
                (when (or (null min-response-level)
                          (< level min-response-level))
                  (setq min-response-level level)))))
          ;; Second pass: adjust headings if needed
          (when (and min-response-level
                     (<= min-response-level assistant-level))
            ;; Need to demote all headings by (assistant-level - min-response-level + 1)
            (let ((level-diff (- (1+ assistant-level) min-response-level)))
              (goto-char (point-min))
              (while (re-search-forward "^\\(\\*+\\)\\( \\)" nil t)
                (unless (gptel-org--in-example-block-p)
                  (let* ((current-stars (match-string 1))
                         (new-level (+ (length current-stars) level-diff))
                         (new-stars (make-string new-level ?*)))
                    (replace-match (concat new-stars "\\2"))))))))))))

(add-hook 'gptel-post-response-functions #'gptel-org--adjust-response-headings)


;;; Response heading title generation

(defun gptel-org-response-title-from-first-line (beg _end _heading-pos)
  "Generate a response heading title from the first line of response.
BEG is the start position of the response.  Returns the first
non-empty line, truncated to 50 characters."
  (save-excursion
    (goto-char beg)
    ;; Skip any blank lines at the start
    (skip-chars-forward " \t\n")
    (let ((first-line (buffer-substring-no-properties
                       (point) (line-end-position))))
      (setq first-line (string-trim first-line))
      ;; Skip if it looks like a code block or list marker only
      (unless (or (string-empty-p first-line)
                  (string-match-p "^```" first-line)
                  (string-match-p "^#\\+begin" first-line)
                  (string-match-p "^[-*+] *$" first-line))
        (truncate-string-to-width first-line 50 nil nil "...")))))

(defun gptel-org--find-response-heading (pos)
  "Find the assistant response heading containing or just before POS.
Returns the position of the heading, or nil if not found."
  (save-excursion
    (goto-char pos)
    ;; Go to beginning of line to handle being at end of heading line
    (beginning-of-line)
    (if (and (org-at-heading-p)
             (or (gptel-org--heading-has-tag-p gptel-org-assistant-tag)
                 (gptel-org--chat-heading-p
                  (org-get-heading t t t t))))
        (point)
      ;; Search backward for assistant heading
      (when (re-search-backward org-heading-regexp nil t)
        (when (or (gptel-org--heading-has-tag-p gptel-org-assistant-tag)
                  (gptel-org--chat-heading-p
                   (org-get-heading t t t t)))
          (point))))))

(defun gptel-org--set-heading-title (heading-pos title)
  "Set the title of the heading at HEADING-POS to TITLE.
Preserves existing tags on the heading."
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
          (insert stars " " title)
          ;; Re-apply tags
          (when tags
            (org-set-tags tags)))))))

(defun gptel-org--apply-response-title (beg end)
  "Apply a title to the response heading if configured.
Called from `gptel-post-response-functions' with BEG and END
positions of the response."
  (when (and gptel-org-response-title-function
             (derived-mode-p 'org-mode))
    (let ((heading-pos (gptel-org--find-response-heading beg)))
      (when heading-pos
        (condition-case err
            (let ((title (funcall gptel-org-response-title-function
                                  beg end heading-pos)))
              (when title
                (gptel-org--set-heading-title heading-pos title)))
          (error
           (message "gptel: Error generating response title: %S" err)))))))

;; Add hook with high priority (run late, after heading adjustments)
(add-hook 'gptel-post-response-functions #'gptel-org--apply-response-title 80)

(provide 'gptel-org)
;;; gptel-org.el ends here

;; Silence warnings about `org-element-type-p' and `org-element-parent', see #294.
;; Local Variables:
;; byte-compile-warnings: (not unresolved)
;; End:
