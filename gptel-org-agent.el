;;; gptel-org-agent.el --- Org agent subtree management for gptel  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Manages agent subtrees in org-mode buffers for gptel.
;;
;; When `gptel-send' is invoked on a TODO heading (one whose keyword is in
;; `gptel-org-todo-keywords'), this module can:
;;
;; 1. Create a child heading tagged with `:main@agent:' under the TODO heading.
;; 2. Open an indirect buffer narrowed to that agent subtree.
;; 3. Return the indirect buffer so the FSM's response is routed there.
;;
;; Agent tags follow the pattern `:<type>@agent:' and support recursive
;; delegation:
;;   - main@agent              -- main agent subtree
;;   - researcher@main@agent   -- researcher sub-agent of main
;;   - gatherer@main@agent     -- gatherer sub-agent of main
;;   - researcher@researcher@main@agent -- nested delegation
;;
;; Enable by setting `gptel-org-subtree-context' to non-nil.

;;; Code:

(require 'org)
(require 'org-fold)
(require 'gptel-indirect-buffer)

;; Forward declarations for functions defined in gptel-org.el
(declare-function gptel-org--debug "gptel-org")
(declare-function gptel-org--heading-has-tag-p "gptel-org")
(declare-function gptel-org--heading-has-todo-keyword-p "gptel-org")
(declare-function gptel-org--enable-auto-correct "gptel-org")
(declare-function gptel-org--ensure-todo-state "gptel-org")
(declare-function gptel-org--tool-state-keyword "gptel-org")
(declare-function gptel-org--format-tool-args-title "gptel-org")
(declare-function gptel-org--tool-args-title-excludes "gptel-org")
(declare-function gptel-org--tool-result-as-org-p "gptel-org")
(declare-function gptel-org--tool-body-text "gptel-org")
(declare-function gptel-org--compute-response-level "gptel-org")
(declare-function gptel-org--in-agent-indirect-buffer-p "gptel-org")

;; Forward declarations for variables defined in gptel-org.el
(defvar gptel-org-todo-keywords)
(defvar gptel-org-infer-bounds-from-tags)
(defvar gptel-org-subtree-context)
(defvar gptel-org--org-format-response)
(defvar gptel-org--ref-level)
(defvar gptel-org--auto-correcting)

;; Forward declarations for variables defined in org.el
;; org-state is dynamically bound by org-todo for hook functions
(defvar org-state)

;; Forward declarations for variables defined in gptel-request.el
(defvar gptel--system-message)

;; Forward declarations for functions defined in gptel-request.el
(declare-function gptel-fsm-info "gptel-request")

;; Forward declarations for functions defined in gptel.el
(declare-function gptel--display-tool-calls "gptel")
(declare-function gptel--display-tool-results "gptel")
(declare-function gptel--map-tool-args "gptel-request")
(declare-function gptel--update-status "gptel")
(declare-function gptel--to-string "gptel")
(declare-function gptel-tool-name "gptel")
(declare-function gptel-tool-async "gptel")
(declare-function gptel-tool-function "gptel")
(declare-function gptel-tool-include "gptel")

;; Forward declarations for variables defined in gptel.el
(defvar gptel-mode)
(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel--preset)
(defvar gptel-include-tool-results)


;;; ---- Helpers --------------------------------------------------------------

(defvar gptel-org-user-tag)

(defun gptel-org-agent--find-user-heading-after-agent (user-tag agent-tag)
  "Find a :USER-TAG: sibling heading after the :AGENT-TAG: heading.
Point must be on the parent TODO heading.
Return non-nil if found."
  (save-excursion
    (when (org-at-heading-p)
      (let ((parent-level (org-current-level))
            (bound (save-excursion (org-end-of-subtree t) (point)))
            (found-agent nil)
            (found-user nil))
        (org-end-of-meta-data t)
        (while (and (not found-user)
                    (< (point) bound)
                    (re-search-forward org-heading-regexp bound t))
          (beginning-of-line)
          (let ((level (org-current-level)))
            (cond
             ((= level (1+ parent-level))
              (let ((tags (org-get-tags nil t)))
                (when (cl-some (lambda (tg)
                                 (string-equal-ignore-case tg agent-tag))
                               tags)
                  (setq found-agent t))
                (when (and found-agent
                           (cl-some (lambda (tg)
                                      (string-equal-ignore-case tg user-tag))
                                    tags))
                  (setq found-user t))))
             ((> level (1+ parent-level)) nil)
             (t (goto-char bound))))
          (unless found-user (forward-line 1)))
        found-user))))

(defun gptel-org-agent--agent-tag-p (tag)
  "Return non-nil if TAG matches the `*@agent' pattern.
An agent tag is any tag string that ends with \"@agent\", such as
\"main@agent\" or \"researcher@main@agent\".  Used for context
filtering and subtree identification."
  (and (stringp tag)
       (string-suffix-p "@agent" tag)))

(defun gptel-org-agent--construct-tag (agent-type &optional parent-tag)
  "Construct an agent tag from AGENT-TYPE and optional PARENT-TAG.

AGENT-TYPE is a string like \"main\", \"researcher\", or \"gatherer\".

If PARENT-TAG is nil, the resulting tag is \"<AGENT-TYPE>@agent\"
\(e.g., \"main@agent\").

If PARENT-TAG is provided, the resulting tag is
\"<AGENT-TYPE>@<PARENT-TAG>\" (e.g., \"researcher@main@agent\")."
  (if parent-tag
      (concat agent-type "@" parent-tag)
    (concat agent-type "@agent")))

(defun gptel-org-agent--find-agent-subtree (agent-tag)
  "Find a child heading tagged with AGENT-TAG under the current heading.

Search the immediate children of the heading at point for one tagged
with AGENT-TAG (case-insensitive match on local tags only).

Return a marker to the child heading if found, or nil."
  (save-excursion
    (when (org-at-heading-p)
      (let ((parent-level (org-current-level))
            (bound (save-excursion (org-end-of-subtree t) (point)))
            (found nil))
        (gptel-org--debug "org-agent find-agent-subtree: searching for %S under level-%d heading"
                          agent-tag parent-level)
        ;; Move past the current heading line into its body/children
        (org-end-of-meta-data t)
        (while (and (not found)
                    (< (point) bound)
                    (re-search-forward org-heading-regexp bound t))
          (beginning-of-line)
          (let ((level (org-current-level)))
            (cond
             ;; Direct child heading -- check its tags
             ((= level (1+ parent-level))
              (let ((tags (org-get-tags nil t))) ; local tags only
                (when (cl-some (lambda (tg)
                                 (string-equal-ignore-case tg agent-tag))
                               tags)
                  (gptel-org--debug "org-agent find-agent-subtree: found %S at line %d"
                                    agent-tag (line-number-at-pos))
                  (setq found (point-marker)))))
             ;; Deeper heading -- skip it, we only want direct children
             ((> level (1+ parent-level))
              nil)
             ;; Same or shallower level -- we've left the subtree
             (t (goto-char bound))))
          (unless found
            (forward-line 1)))
        found))))


;;; ---- Core functions -------------------------------------------------------

(defun gptel-org-agent--create-subtree (agent-type &optional parent-tag description)
  "Create an agent child heading under the current heading.

AGENT-TYPE is a string identifying the agent (e.g., \"main\",
\"researcher\", \"gatherer\").

PARENT-TAG is an optional string for constructing recursive tags.
When nil, the tag is \"<AGENT-TYPE>@agent\".  When provided, the tag
is \"<AGENT-TYPE>@<PARENT-TAG>\".

DESCRIPTION is an optional string to use as the heading title.
When nil, the parent heading's text is used instead.

Point must be on the parent heading (the TODO heading or another agent
heading).

Uses `gptel-org-ib-create-heading' (after ensuring the terminator)
so that the new heading is inserted BEFORE any existing sibling
terminator (e.g. FEEDBACK) and therefore does not disturb existing
sibling indirect buffers narrowed to earlier subtrees.  When no
terminator is appropriate or present, create-heading falls back to
`org-end-of-subtree' (preserving prior behaviour for fresh trees).

Return a marker to the newly created heading."
  (save-excursion
    (unless (org-at-heading-p)
      (error "gptel-org-agent--create-subtree: point is not on a heading"))
    ;; Tool call confirmation text in the buffer may be marked read-only
    ;; (by `gptel--display-tool-calls').  Bypass read-only here.
    (let* ((inhibit-read-only t)
           (parent-level (org-current-level))
           (child-level (1+ parent-level))
           (tag (gptel-org-agent--construct-tag agent-type parent-tag))
           (heading-title
            (or description
                (org-element-property :raw-value (org-element-at-point))))
           (doing-keyword
            (or (bound-and-true-p gptel-org-tasks-doing-keyword) "AI-DOING"))
           ;; Terminator at parent level.  If a FEEDBACK (or custom user
           ;; keyword) child exists already, insert before it; otherwise
           ;; create-heading appends at end-of-subtree.
           (terminator-keyword
            (or (bound-and-true-p gptel-org-user-keyword) "FEEDBACK")))
      (gptel-org--debug
       "org-agent create-subtree: creating %S at level %d under level %d (terminator=%S)"
       tag child-level parent-level terminator-keyword)
      (let ((marker (gptel-org-ib-create-heading
                     (if heading-title doing-keyword nil)
                     (or heading-title "")
                     (list tag)
                     terminator-keyword)))
        (gptel-org--debug
         "org-agent create-subtree: created heading at line %d, marker at %d"
         (line-number-at-pos marker) (marker-position marker))
        marker))))


(defconst gptel-org-agent--ai-do-family-keywords
  '("AI-DO" "AI-DOING" "AI-DONE")
  "TODO keywords that form the AI-DO task lifecycle.
Per the Phase IB-4 heading-state grammar (see
`gptel-indirect-buffer-ai.org'), these keywords may appear only
directly under a user-owned TODO heading.  Nesting an AI-DO-family
heading under another AI-DO-family heading is a structural
violation and is rejected by
`gptel-org-agent--validate-ai-do-depth'.

Note: sub-agent subtrees currently reuse AI-DOING as their TODO
keyword (see `gptel-org-agent--setup-task-subtree').  This is a
known discrepancy between the grammar and the implementation that
Phase IB-5 will resolve by introducing per-agent state triads
(e.g. GATHER/GATHERING/GATHERED).  Until then the validator is
wired only into AI-DO-heading *creation* entry points
(`gptel-org-agent--create-handover-heading' and
`gptel-org-handoff'), not into the generic todo-keyword choke
point `gptel-org-agent--set-todo-keyword', to avoid spurious
failures during sub-agent setup.")

(defun gptel-org-agent--validate-ai-do-depth (&optional pos)
  "Validate that the heading at POS (default point) has no AI-DO ancestor.

AI-DO, AI-DOING, and AI-DONE may appear only directly under a
user-owned TODO heading.  Walks the heading chain from POS upward
and signals `user-error' if any ancestor has an AI-DO-family TODO
state.  Returns t on success.

The heading *at* POS is not flagged — only ancestors are checked.
To validate a would-be-child insertion, call this with point at the
prospective parent heading and additionally confirm the parent's
own TODO state is not in `gptel-org-agent--ai-do-family-keywords'
(the creation sites do this explicitly).

Failure is a fatal `user-error' per the IB-3 hard-fail policy."
  (save-excursion
    (when pos (goto-char pos))
    (unless (org-at-heading-p)
      (org-back-to-heading t))
    (let ((current-state (org-get-todo-state)))
      ;; Don't flag the heading itself; only ancestors.
      (while (org-up-heading-safe)
        (let ((state (org-get-todo-state)))
          (when (member state gptel-org-agent--ai-do-family-keywords)
            (user-error
             "AI-DO depth violation: %S nested under %S ancestor (line %d).  AI-DO may appear only directly under a user TODO"
             (or current-state "<new>")
             state
             (line-number-at-pos))))))
    t))

(defun gptel-org-agent--validate-ai-do-parent ()
  "Validate that point's heading is a legal parent for an AI-DO child.
Signals `user-error' if the heading at point is itself an
AI-DO-family heading or has an AI-DO-family ancestor.  Returns t on
success.  Used by AI-DO creation sites before insertion."
  (save-excursion
    (unless (org-at-heading-p)
      (org-back-to-heading t))
    (let ((parent-state (org-get-todo-state)))
      (when (member parent-state gptel-org-agent--ai-do-family-keywords)
        (user-error
         "AI-DO depth violation: cannot insert AI-DO child under %S heading (line %d).  AI-DO may appear only directly under a user TODO"
         parent-state
         (line-number-at-pos))))
    (gptel-org-agent--validate-ai-do-depth))
  t)

(defun gptel-org-agent--create-handover-heading (body description)
  "Create a new AI-DO heading at user task level for a handover task.

When called from an agent indirect buffer, resolve the agent heading
position via `gptel-org-ib-resolve-agent-heading', then navigate up
past all agent headings to the user-level task heading using
`gptel-org-ib-find-user-task-heading'.  The new heading is inserted
as a child of the user task heading (sibling of the top-level agent
subtree).

BODY is inserted as the heading content.  DESCRIPTION becomes the
heading title with TODO keyword AI-DO.

Signals `user-error' via `gptel-org-agent--validate-ai-do-parent'
if the user-level task heading is itself an AI-DO-family heading
or has an AI-DO ancestor (grammar violation, see
`gptel-org-agent--ai-do-family-keywords').

Returns the heading text of the created heading, or nil on failure."
  (let* ((base-buf (and (gptel-org-ib-registered-p (current-buffer))
                        (gptel-org-ib-base (current-buffer))))
         (agent-heading-pos
          (gptel-org-ib-resolve-agent-heading (current-buffer))))
    (when agent-heading-pos
      (with-current-buffer base-buf
        (save-excursion
          (goto-char agent-heading-pos)
          ;; Navigate up past all agent headings to the user task
          (when (gptel-org-ib-find-user-task-heading)
            ;; IB-4.7: validate grammar *before* insertion.  The user
            ;; task heading must not be AI-DO-family and must have no
            ;; AI-DO ancestor.  Fatal user-error on violation.
            (gptel-org-agent--validate-ai-do-parent)
            (let* ((inhibit-read-only t)
                   (parent-marker (point-marker))
                   (parent-level (org-current-level))
                   (child-level (1+ parent-level))
                   (stars (make-string child-level ?*))
                   (todo-keyword "AI-DO")
                   (heading-text (format "%s %s %s" stars todo-keyword description))
                   ;; Delegate to the unified parent-aware API.  The
                   ;; new heading lands BEFORE the user task's TERMINE
                   ;; child (regression fix: the previous hand-rolled
                   ;; org-end-of-subtree path appended the heading
                   ;; AFTER TERMINE, breaking handover terminator
                   ;; semantics).  No IB is created for the handover
                   ;; heading itself; the handover sub-agent gets its
                   ;; own subtree elsewhere.
                   (result (gptel-org-ib-insert-child
                            parent-marker todo-keyword description
                            :terminator-keyword "TERMINE"
                            :create-indirect-buffer nil))
                   (heading-marker (plist-get result :heading-marker)))
              ;; Insert the body content between the new heading line
              ;; and the parent's TERMINE child.  `create-heading'
              ;; already left a blank line after the heading; we
              ;; overwrite it with the body, ensuring a trailing
              ;; newline so the body cannot run into TERMINE.
              (when (and body (not (string-empty-p body))
                         (markerp heading-marker)
                         (marker-position heading-marker))
                (save-excursion
                  (goto-char (marker-position heading-marker))
                  (forward-line 1)
                  (let ((inhibit-read-only t))
                    (gptel-org-ib-ensure-bol)
                    (insert body)
                    (unless (string-suffix-p "\n" body)
                      (insert "\n")))))
              heading-text)))))))

(defun gptel-org-agent--extract-parent-context ()
  "Extract the full text of the user-level task heading for handover context.

When called from an agent indirect buffer, resolve the agent heading
position and navigate up past all agent headings to the user-level
task heading using `gptel-org-ib-find-user-task-heading'.  Returns
the full subtree content of the user task as a string.

This is used by the handover mechanism: the handover agent needs to
read all context accumulated under the user task heading, including
any triage agent findings.  Returns nil if the context cannot be
extracted."
  (let ((base-buf (and (gptel-org-ib-registered-p (current-buffer))
                       (gptel-org-ib-base (current-buffer)))))
    (when (and base-buf (buffer-live-p base-buf))
      (let ((agent-heading-pos
             (gptel-org-ib-resolve-agent-heading (current-buffer))))
        (when agent-heading-pos
          (with-current-buffer base-buf
            (save-excursion
              (goto-char agent-heading-pos)
              ;; Navigate up past all agent headings to user task
              (when (gptel-org-ib-find-user-task-heading)
                (let ((beg (point))
                      (end (save-excursion
                             (org-end-of-subtree t)
                             (point))))
                  (buffer-substring-no-properties beg end))))))))))


(defun gptel-org-agent--create-handover-data ()
  "Extract filtered handover data from the user task subtree.

Like `gptel-org-agent--extract-parent-context', navigates from the
current agent indirect buffer to the user-level task heading.
Instead of returning the raw subtree text, filters out raw tool
outputs (regions with `gptel' text property set to a (tool . ID)
cons cell) and heading lines marked with `gptel' = `ignore'.

Returns a string containing:
- The user task heading line and any body text before sub-headings
- For each sub-heading: the heading line plus only LLM response
  regions (where `gptel' property = `response')

This produces a compact summary of agent findings without the
bulk of raw tool call results, suitable for passing to the
handover agent.

Returns nil if the context cannot be extracted."
  (let ((base-buf (and (gptel-org-ib-registered-p (current-buffer))
                       (gptel-org-ib-base (current-buffer)))))
    (when (and base-buf (buffer-live-p base-buf))
      (let ((agent-heading-pos
             (gptel-org-ib-resolve-agent-heading (current-buffer))))
        (when agent-heading-pos
          (with-current-buffer base-buf
            (save-excursion
              (goto-char agent-heading-pos)
              ;; Navigate up past all agent headings to user task
              (when (gptel-org-ib-find-user-task-heading)
                (let ((task-beg (point))
                      (task-end (save-excursion
                                  (org-end-of-subtree t)
                                  (point)))
                      (parts nil))
                  ;; Collect heading line
                  (push (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position))
                        parts)
                  (push "\n" parts)
                  ;; Collect body text before first child heading
                  ;; (this is the user's task description)
                  (forward-line 1)
                  (let ((body-start (point))
                        (first-child (save-excursion
                                       (if (re-search-forward
                                            org-heading-regexp task-end t)
                                           (line-beginning-position)
                                         task-end))))
                    (when (< body-start first-child)
                      (push (string-trim-right
                             (buffer-substring-no-properties
                              body-start first-child))
                            parts)
                      (push "\n" parts)))
                  ;; Walk through child headings, extracting only
                  ;; response-propertied text
                  (goto-char task-beg)
                  (let ((_task-level (org-current-level)))
                    (while (and (outline-next-heading)
                                (< (point) task-end))
                      ;; Include every heading line for structure
                      (push (buffer-substring-no-properties
                             (line-beginning-position)
                             (line-end-position))
                            parts)
                      (push "\n" parts)
                      ;; Scan body of this heading (before next heading)
                      (forward-line 1)
                      (let ((body-start (point))
                            (body-end
                             (save-excursion
                               (if (re-search-forward
                                    org-heading-regexp task-end t)
                                   (line-beginning-position)
                                 task-end))))
                        (when (< body-start body-end)
                          (gptel-org-agent--collect-response-text
                           body-start body-end parts)))
                      ;; Stay at current heading for next iteration
                      (beginning-of-line)))
                  (let ((result (string-trim
                                 (mapconcat #'identity
                                            (nreverse parts) ""))))
                    (if (string-empty-p result) nil result)))))))))))

(defun gptel-org-agent--collect-response-text (beg end parts)
  "Collect LLM response text from region BEG to END into PARTS.

Walks through the region and pushes text segments where the
`gptel' text property equals `response' onto the front of PARTS
list (caller should `nreverse' later).  Skips regions with
property value (tool . ID) or `ignore'.

Unpropertied text (no `gptel' property at all) that looks like
regular prose is also included, as it may be user-written task
description or notes."
  (let ((pos beg))
    (while (< pos end)
      (let ((gptel-val (get-text-property pos 'gptel))
            (next-change (next-single-property-change
                          pos 'gptel nil end)))
        (cond
         ;; Include response-propertied text (LLM findings)
         ((eq gptel-val 'response)
          (push (buffer-substring-no-properties pos next-change) parts))
         ;; Include unpropertied text (user notes, task description)
         ((null gptel-val)
          (let ((text (buffer-substring-no-properties pos next-change)))
            (unless (string-blank-p text)
              (push text parts))))
         ;; Skip tool results: (tool . id) cons cells
         ;; Skip ignored regions: 'ignore
         ;; Skip anything else
         )
        (setq pos next-change)))))

(defun gptel-org-agent--indirect-buffer-name (base-buffer heading-pos tag)
  "Compute a unique indirect buffer name for an agent subtree.
Deprecated shim — delegates to `gptel-org-ib-compute-name' in the
`gptel-indirect-buffer' module, which is the canonical generator
for agent, reasoning, and tool indirect buffer names."
  (gptel-org-ib-compute-name base-buffer heading-pos tag))

(defun gptel-org-agent--open-indirect-buffer (base-buffer heading-marker)
  "Open an indirect buffer narrowed to the agent subtree.

BASE-BUFFER is the org buffer containing the agent heading.
HEADING-MARKER is a marker pointing to the agent heading.

Creates an indirect buffer via `gptel-org-ib-create' (which handles
subtree narrowing, end-marker creation, fold decoupling, and registry
tracking), then applies agent-specific setup: marks the buffer as an
agent indirect buffer and enables auto-correction.

Return the indirect buffer."
  (let* ((heading-pos (if (markerp heading-marker)
                          (marker-position heading-marker)
                        heading-marker))
         (indirect-buf (gptel-org-ib-create base-buffer heading-pos)))
    ;; Apply agent-specific setup that gptel-org-ib-create doesn't do.
    ;; TERMINE is already seeded by the factory (universal invariant —
    ;; see `gptel-indirect-buffer-ai.org' *** TERMINE), so no
    ;; additional seeding is required here.
    (with-current-buffer indirect-buf
      ;; Mark this buffer as an agent indirect buffer.  This flag persists
      ;; even after the agent tag is removed from the heading (e.g. by
      ;; `gptel-org-agent--insert-user-heading'), ensuring that
      ;; `gptel-org--in-agent-indirect-buffer-p' continues to return t
      ;; and the dynamic prefix suppression remains active.
      (setq-local gptel-org--agent-indirect-buffer-p t)
      ;; Store the end-marker on a buffer-local variable for backwards
      ;; compatibility with code that reads it directly.
      (let ((entry (gptel-org-ib-get (buffer-name indirect-buf))))
        (when entry
          (setq-local gptel-org-agent--narrow-end-marker
                      (plist-get entry :end-marker))))
      ;; Enable the idempotent auto-corrector for heading level rebasing
      (gptel-org--enable-auto-correct))
    (gptel-org--debug
     "org-agent open-indirect-buffer: CREATED %S base=%S ref-level=%S narrow=[%d,%d]"
     (buffer-name indirect-buf) (buffer-name base-buffer)
     (buffer-local-value 'gptel-org--ref-level indirect-buf)
     (with-current-buffer indirect-buf (point-min))
     (with-current-buffer indirect-buf (point-max)))
    indirect-buf))

(defvar-local gptel-org-agent--narrow-end-marker nil
  "Marker at the end of the narrowed region in an agent indirect buffer.
This marker has insertion-type t so the region grows as text is appended.
Stored for cleanup in `gptel-org-agent--close-indirect-buffer'.")

(defun gptel-org-agent--make-insertion-marker (buffer)
  "Create an FSM-position marker for BUFFER.

Delegates to `gptel-org-ib-streaming-marker' to choose a
terminator-aware position:

1. If BUFFER's narrowed subtree has a \"TERMINE\" child terminator
   (IB-4.2 unified terminator), the marker is at the start of
   TERMINE with insertion-type nil (streaming stays BEFORE it).
2. Else if BUFFER has a legacy \"RESULTS\" child terminator (pre
   IB-4.2 task / sub-agent IBs), the marker is at the start of
   RESULTS with insertion-type nil.
3. Else if BUFFER has a legacy \"FEEDBACK\" (or
   `gptel-org-user-keyword') child terminator (pre IB-4.2
   top-level agent IBs), the marker is at the start of FEEDBACK
   with insertion-type nil.
4. Otherwise the marker is at `point-max' with insertion-type t.

This preserves the contract that streamed text always lands before
any terminator heading, so sibling indirect buffers narrowed to
earlier subtrees are not disturbed."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((user-kw (or (bound-and-true-p gptel-org-user-keyword)
                         "FEEDBACK")))
        (cond
         ;; Try TERMINE first (IB-4.2 unified terminator).
         ((and (org-at-heading-p)
               (save-excursion
                 (gptel-org-ib-find-terminator "TERMINE")))
          (gptel-org-ib-streaming-marker "TERMINE"))
         ;; Legacy, pre-IB-4.2: RESULTS terminator (task / sub-agent IBs).
         ((and (org-at-heading-p)
               (save-excursion
                 (gptel-org-ib-find-terminator "RESULTS")))
          (gptel-org-ib-streaming-marker "RESULTS"))
         ;; Legacy, pre-IB-4.2: user FEEDBACK terminator (top-level agent IBs).
         ((and (org-at-heading-p)
               (save-excursion
                 (gptel-org-ib-find-terminator user-kw)))
          (gptel-org-ib-streaming-marker user-kw))
         ;; Fallback: end-of-buffer with insertion-type t.
         (t (gptel-org-ib-streaming-marker nil)))))))

(defun gptel-org-agent--close-indirect-buffer (indirect-buffer &optional fold)
  "Close INDIRECT-BUFFER and clean up associated resources.

If FOLD is non-nil, fold the agent subtree in the base buffer before
killing the indirect buffer.

Uses `gptel-org-ib-close' for registry cleanup, marker management,
and buffer killing.  Additionally folds TOOL and REASONING child
headings when FOLD is requested."
  (when (buffer-live-p indirect-buffer)
    (let* ((buf-name (buffer-name indirect-buffer))
           (entry (gptel-org-ib-get buf-name))
           (base-buf (plist-get entry :base))
           (end-marker (or (plist-get entry :end-marker)
                           (ignore-errors
                             (buffer-local-value
                              'gptel-org-agent--narrow-end-marker
                              indirect-buffer))))
           ;; Grab the narrowed region start before closing
           (subtree-start (with-current-buffer indirect-buffer
                            (point-min))))
      (gptel-org--debug
       "org-agent close-indirect-buffer: CLOSING %S base=%S fold=%s"
       buf-name (and base-buf (buffer-name base-buf)) fold)
      ;; When folding, first fold individual TOOL/REASONING headings so
      ;; they remain folded when user unfolds the agent subtree
      (when (and fold (buffer-live-p base-buf))
        (with-current-buffer base-buf
          (save-excursion
            (goto-char subtree-start)
            (let ((subtree-end (or (and (markerp end-marker)
                                       (marker-position end-marker))
                                  (save-excursion
                                    (goto-char subtree-start)
                                    (org-end-of-subtree t t)
                                    (point)))))
              (while (re-search-forward
                      "^\\*+ \\(?:TOOL\\|REASONING\\) " subtree-end t)
                (beginning-of-line)
                (ignore-errors
                  (when (and (org-at-heading-p)
                             (not (org-fold-folded-p (point) 'headline)))
                    (org-fold-subtree t)))
                (forward-line 1))))))
      ;; Delegate core cleanup (registry, markers, kill) to gptel-org-ib-close.
      ;; Pass fold=t so it handles the whole-subtree fold.
      (gptel-org-ib-close indirect-buffer fold))))


;;; ---- Integration point ----------------------------------------------------

(defun gptel-org-agent--preset-to-agent-type (preset)
  "Derive an agent type string from PRESET name.
PRESET is a symbol or string naming the active gptel preset.
Strips a leading \"gptel-\" prefix if present.  Returns \"main\"
if PRESET is nil or empty."
  (if (null preset)
      "main"
    (let ((name (cond
                 ((symbolp preset) (symbol-name preset))
                 ((stringp preset) preset)
                 (t nil))))
      (cond
       ((or (null name) (string-empty-p name)) "main")
       ((string-prefix-p "gptel-" name) (substring name 6))
       (t name)))))

(defun gptel-org-agent--maybe-setup-subtree (&optional preset)
  "Conditionally create an agent subtree and indirect buffer.

Check whether `gptel-org-subtree-context' is enabled and point is at
or under a heading with any org TODO keyword.

When point is on a user/feedback heading (created after an agent
response), handle it based on mode:

- Keyword mode: The FEEDBACK heading is a sibling of agent subtrees
  (same level, direct child of parent TODO).  It is mutated in-place
  to become the new agent subtree: state changes to AI-DOING, agent
  tag is added, and the parent TODO transitions to AI-DOING.

- Tag mode: The :user: heading is a child of the agent subtree.
  Walk up to the parent TODO heading to reuse the existing agent
  subtree.

If conditions are met:
  - Derive the agent type from PRESET (or fall back to \"main\").
  - Look for an existing agent child subtree and reuse it.
  - If none exists, create one via `gptel-org-agent--create-subtree'.
  - Open an indirect buffer narrowed to the agent subtree via
    `gptel-org-agent--open-indirect-buffer'.
  - Return the indirect buffer.

If conditions are not met, return nil so that `gptel-send' proceeds
with its normal behavior."
  (when (and gptel-org-subtree-context
             (derived-mode-p 'org-mode))
    (save-excursion
      ;; Navigate to enclosing heading if point is in the body
      (unless (org-at-heading-p)
        (ignore-errors (org-back-to-heading t)))
      (when (org-at-heading-p)
        ;; Ensure we're at the beginning of the heading
        (beginning-of-line)
        (let ((result nil)
              (user-keyword (and (boundp 'gptel-org-user-keyword)
                                 gptel-org-user-keyword))
              (keyword-mode (and (boundp 'gptel-org-use-todo-keywords)
                                 gptel-org-use-todo-keywords)))

          ;; --- Keyword mode: FEEDBACK heading as sibling of agent subtrees ---
          ;; The FEEDBACK heading is mutated into the new AI-DOING agent
          ;; subtree.  It is a direct child of the parent TODO, at the
          ;; same level as previous agent subtrees.
          (when (and keyword-mode
                     user-keyword
                     (equal (org-get-todo-state) user-keyword))
            (let ((feedback-marker (point-marker)))
              (gptel-org--debug
               "org-agent maybe-setup-subtree: on FEEDBACK heading at line %d, mutating to agent subtree"
               (line-number-at-pos))
              ;; Walk up one level: FEEDBACK is direct child of parent TODO
              (ignore-errors (outline-up-heading 1 t))
              (when (org-get-todo-state)
                (let* ((agent-type (gptel-org-agent--preset-to-agent-type preset))
                       (agent-tag (gptel-org-agent--construct-tag agent-type))
                       (doing-keyword (or (bound-and-true-p gptel-org-tasks-doing-keyword)
                                          "AI-DOING"))
                       (base-buffer (current-buffer)))
                  ;; Transition parent TODO heading to AI-DOING
                  (gptel-org-agent--set-todo-keyword doing-keyword)
                  ;; Set the active task marker so that
                  ;; gptel-org-tasks--clear-active-task can transition
                  ;; the parent back to FEEDBACK when AI completes, and
                  ;; after-abort can transition to CANCELED.
                  (when (boundp 'gptel-org-tasks--active-task-marker)
                    (setq gptel-org-tasks--active-task-marker
                          (point-marker)))
                  (gptel-org--debug
                   "org-agent maybe-setup-subtree: parent transitioned to %s at line %d"
                   doing-keyword (line-number-at-pos))
                  ;; Mutate the FEEDBACK heading: change state to AI-DOING
                  ;; and add the agent tag
                  (save-excursion
                    (goto-char feedback-marker)
                    (gptel-org-agent--set-todo-keyword doing-keyword)
                    (org-set-tags (list agent-tag))
                    (gptel-org--debug
                     "org-agent maybe-setup-subtree: FEEDBACK mutated to %s with tag %S at line %d"
                     doing-keyword agent-tag (line-number-at-pos)))
                  ;; Open indirect buffer on the mutated heading
                  (setq result
                        (gptel-org-agent--open-indirect-buffer
                         base-buffer feedback-marker))))))

          ;; --- Tag mode: :user: heading as child of agent subtree ---
          ;; Walk up from user heading through agent heading to parent TODO.
          (when (and (not result)
                     (or
                      ;; Tag mode: no TODO state, has :user: tag
                      (and (not (org-get-todo-state))
                           (let ((user-tag (if (boundp 'gptel-org-user-tag)
                                               gptel-org-user-tag
                                             "user")))
                             (cl-some (lambda (tg)
                                        (string-equal-ignore-case tg user-tag))
                                      (org-get-tags nil t))))
                      ;; Keyword mode fallback: if not handled above (e.g.
                      ;; old HI headings still in the buffer)
                      (and (not keyword-mode)
                           user-keyword
                           (equal (org-get-todo-state) user-keyword))))
            (gptel-org--debug
             "org-agent maybe-setup-subtree: on user heading at line %d, walking up to parent"
             (line-number-at-pos))
            (ignore-errors (outline-up-heading 1 t))
            ;; In tag mode, the user heading is a child of the agent
            ;; subtree, so one walk-up lands on the agent heading itself.
            ;; Walk up once more to reach the actual parent task.
            (when (cl-some #'gptel-org-agent--agent-tag-p
                           (org-get-tags nil t))
              (gptel-org--debug
               "org-agent maybe-setup-subtree: landed on agent heading at line %d, walking up again"
               (line-number-at-pos))
              (ignore-errors (outline-up-heading 1 t))))

          ;; --- Normal TODO heading path ---
          ;; On a TODO heading, find or create an agent subtree.
          (when (and (not result) (org-get-todo-state))
            (let* ((agent-type (gptel-org-agent--preset-to-agent-type preset))
                   (_ (gptel-org--debug
                       "org-agent maybe-setup-subtree: TODO heading at line %d, agent-type=%S (preset=%S)"
                       (line-number-at-pos) agent-type preset))
                   (agent-tag (gptel-org-agent--construct-tag agent-type))
                   (existing (gptel-org-agent--find-agent-subtree agent-tag))
                   (heading-marker (or existing
                                       (gptel-org-agent--create-subtree agent-type)))
                   (base-buffer (current-buffer)))
              (when existing
                (gptel-org--debug "org-agent maybe-setup-subtree: reusing existing %S subtree"
                                  agent-tag))
              (setq result
                    (gptel-org-agent--open-indirect-buffer base-buffer heading-marker))))

          result)))))

(defvar gptel-prompt-transform-functions)

(defun gptel-org-agent--recover-from-ib-fatal (err fsm)
  "Annotate user task with DENIED + TERMINE on an IB fatal error.

ERR is the signalled error condition (a cons `(user-error . MSG-PARTS)').
FSM is the active `gptel-fsm'.  By the time this runs, the FSM has
already been aborted by `gptel-org-ib-fatal'; this helper only
performs the UI annotation on the user-level task heading.

Locates the user task TODO via the FSM's `:buffer' (walking to the
base buffer when the redirect has already populated it with an agent
indirect buffer), sets its TODO keyword to DENIED, and inserts a
TERMINE child heading whose body is an example block containing the
error message.

Degraded recovery: if point cannot be located on a heading, or no
user task is found, this returns without annotation.  The caller is
expected to re-signal the original `user-error' so the user sees it
through normal channels."
  (let* ((msg (error-message-string err))
         (info (gptel-fsm-info fsm))
         (target-buf (plist-get info :buffer))
         (base-buf (cond
                    ((not (buffer-live-p target-buf)) nil)
                    ((gptel-org-ib-registered-p target-buf)
                     (gptel-org-ib-base target-buf))
                    (t target-buf))))
    (gptel-org--debug
     "org-agent ib-fatal-recover: msg=%S target=%S base=%S"
     msg (and (buffer-live-p target-buf) (buffer-name target-buf))
     (and (buffer-live-p base-buf) (buffer-name base-buf)))
    (when (and (buffer-live-p base-buf)
               (with-current-buffer base-buf (derived-mode-p 'org-mode)))
      (with-current-buffer base-buf
        (save-excursion
          (save-restriction
            (widen)
            (let ((pos (or (plist-get info :position) (point))))
              (goto-char (if (markerp pos) (marker-position pos) pos)))
            ;; Position at the nearest heading if not already there.
            ;; Backward search OK: navigating from a known FSM
            ;; :position into its containing heading for read-only
            ;; navigation (TODO-state lookup, terminator creation via
            ;; the IB API).  Not insertion-point discovery.
            (unless (org-at-heading-p)
              (when (re-search-backward org-heading-regexp nil t)
                (beginning-of-line)))
            (when (org-at-heading-p)
              (let ((user-pos (gptel-org-ib-find-user-task-heading)))
                (when user-pos
                  (goto-char user-pos)
                  (gptel-org-agent--set-todo-keyword "DENIED")
                  ;; Insert TERMINE child with error as example block.
                  (let ((term-marker
                         (gptel-org-ib-create-terminator "TERMINE")))
                    (when (markerp term-marker)
                      (goto-char term-marker)
                      (end-of-line)
                      (insert "\n#+begin_example\n" msg
                              "\n#+end_example"))))))))))))

(defun gptel-org-agent--transform-redirect (fsm)
  "Prompt transform: redirect response to an agent indirect buffer.

When the request originates from an org-mode buffer on a TODO heading
with `gptel-org-subtree-context' enabled, create (or reuse) an agent
child subtree and redirect the FSM's response buffer and position to
the indirect buffer.  The agent tag reflects the active preset name
\(e.g., `:triage@agent:' for gptel-triage preset).

This function is registered in `gptel-prompt-transform-functions' so
it runs during the prompt transform phase of `gptel-request'.  At this
point the prompt has already been built from the original buffer, so
this only affects where the response is inserted.

Skips redirection when the request already originates from an agent
indirect buffer (identified by `gptel-org-ib-registered-p' returning non-nil).

On `user-error' signalled from the IB API (via `gptel-org-ib-fatal'),
invoke `gptel-org-agent--recover-from-ib-fatal' to annotate the user
task with DENIED + TERMINE, then re-signal the error so the user
sees it through normal channels.  The FSM has already been aborted
by `gptel-org-ib-fatal' before this handler runs."
  (condition-case err
      (let* ((info (gptel-fsm-info fsm))
             (orig-buffer (plist-get info :buffer))
             (preset (plist-get info :preset)))
        (when (and gptel-org-subtree-context
                   (buffer-live-p orig-buffer)
                   ;; Only redirect from a base org buffer, not from an
                   ;; indirect buffer (which is already an agent subtree)
                   (not (gptel-org-ib-registered-p orig-buffer))
                   (with-current-buffer orig-buffer
                     (derived-mode-p 'org-mode)))
          (when-let* ((indirect-buf
                       (with-current-buffer orig-buffer
                         (gptel-org-agent--maybe-setup-subtree preset))))
            (let ((pos-marker (gptel-org-agent--make-insertion-marker indirect-buf)))
              (with-current-buffer indirect-buf
                ;; Ensure gptel-mode is active for proper response formatting
                (unless (bound-and-true-p gptel-mode)
                  (setq-local gptel-mode t))
                ;; Redirect the FSM's response target
                (plist-put info :buffer indirect-buf)
                (plist-put info :position pos-marker)
                ;; Store reference for potential cleanup
                (plist-put info :agent-indirect-buffer indirect-buf)
                (gptel-org--debug
                 "org-agent transform-redirect: REDIRECTED orig=%S -> indirect=%S pos=%d preset=%S ref-level=%S base=%S"
                 (buffer-name orig-buffer) (buffer-name indirect-buf)
                 (marker-position pos-marker) preset
                 (buffer-local-value 'gptel-org--ref-level indirect-buf)
                 (and (gptel-org-ib-registered-p indirect-buf)
                      (buffer-name (gptel-org-ib-base indirect-buf)))))))))
    (user-error
     (gptel-org-agent--recover-from-ib-fatal err fsm)
     (signal (car err) (cdr err)))))


;;; ---- Org format instructions for system message ----------------------------

(defvar gptel-org-agent-format-instructions t
  "When non-nil, inject org formatting instructions into the system message.

When enabled, a prompt transform dynamically resolves the heading
level context and appends instructions to the system message telling
the LLM to respond using org-mode formatting with correct heading
levels.

This works when `gptel-org-subtree-context' is enabled and the buffer
is an agent indirect buffer.")

(defun gptel-org-agent--response-heading-level (buffer)
  "Determine the heading level for AI response content in BUFFER.

Return the heading level at which the AI should write its top-level
headings, or nil if BUFFER is not in a relevant org-mode context.

In agent indirect buffers, this is one level deeper than the agent
heading (the first heading in the narrowed buffer)."
  (when (and (buffer-live-p buffer)
             (with-current-buffer buffer
               (derived-mode-p 'org-mode)))
    (with-current-buffer buffer
      (cond
       ;; Agent indirect buffer: response goes under the @agent heading
       ((and (bound-and-true-p gptel-org-subtree-context)
             (gptel-org-ib-registered-p buffer))
        (save-excursion
          (goto-char (point-min))
          (when (org-at-heading-p)
            (1+ (org-current-level)))))))))

(defun gptel-org-agent--seq-todo-line (buffer)
  "Return the #+SEQ_TODO line from BUFFER, or nil if none found.

Searches the base buffer (for indirect buffers) or BUFFER directly."
  (let ((search-buffer (if (gptel-org-ib-registered-p buffer)
                           (gptel-org-ib-base buffer)
                         buffer)))
    (when (buffer-live-p search-buffer)
      (with-current-buffer search-buffer
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (when (re-search-forward
                   "^#\\+SEQ_TODO:.*$" nil t)
              (match-string-no-properties 0))))))))

(defun gptel-org-agent--format-instructions (response-level &optional seq-todo)
  "Build org formatting instruction string.

RESPONSE-LEVEL is the heading level for the AI's top-level headings.
SEQ_TODO is the optional #+SEQ_TODO line from the buffer."
  (if (and (boundp 'gptel-org-use-todo-keywords) gptel-org-use-todo-keywords)
      ;; TODO keyword mode: AI writes headings starting from level 1,
      ;; auto-corrector rebases them to the correct level
      (let ((ai-kw (if (boundp 'gptel-org-assistant-keyword)
                       gptel-org-assistant-keyword "AI")))
        (concat
         "\n\n"
         "Respond using Emacs org-mode formatting.\n"
         (format "Start your response with: * %s <short descriptive title>\n" ai-kw)
         "Then write the response body below the heading.\n"
         "Use org headings for document structure, NOT markdown headings.\n"
         "Your top-level headings should be at level 1 (* Heading).\n"
         "Deeper sub-headings start at level 2 (** Sub-heading).\n"
         "Use org-mode markup: *bold*, /italic/, =verbatim=, ~code~.\n"
         "Use #+begin_src/#+end_src for code blocks (not markdown fences).\n"
         "Inside #+begin_example blocks, escape lines starting with * or #+ by prefixing with a comma.\n"
         "REMEMBER! Org strips the leading comma on export.\n"
         "IMPORTANT: Write your response as plain org-mode text directly.\n"
         "Do NOT wrap your entire response inside #+begin_src, #+begin_example, or any other block.\n"
         "Only use #+begin_src blocks for actual code snippets within your response.\n"
         (if seq-todo
             (format "The document uses these TODO keywords: %s\n" seq-todo)
           "")
         "\n"
         "Example of correct org response format:\n"
         "#+begin_src org\n"
         (format "  ,* %s Response title here\n" ai-kw)
         "  Response body text.\n"
         "  - List item 1\n"
         "  - List item 2\n"
         "  ,** Sub heading\n"
         "  Some text under sub heading\n"
         "  ,#+begin_src elisp\n"
         "  (message \"example code block\")\n"
         "  ,#+end_src\n"
         "#+end_src\n"))
    ;; Legacy tag-based mode: AI writes headings at the exact level
    (let* ((stars (make-string response-level ?*))
           (sub-stars (make-string (1+ response-level) ?*))
           (comma-stars (concat "," stars))
           (comma-sub-stars (concat "," sub-stars)))
      (concat
       "\n\n"
       "Respond using Emacs org-mode formatting.\n"
       "Use org headings for document structure, NOT markdown headings.\n"
       (format "Your top-level headings should be at level %d (%s Heading).\n"
                response-level stars)
       (format "Deeper sub-headings start at level %d (%s Sub-heading).\n"
                (1+ response-level) sub-stars)
       "Use org-mode markup: *bold*, /italic/, =verbatim=, ~code~.\n"
       "Use #+begin_src/#+end_src for code blocks (not markdown fences).\n"
       "Inside #+begin_example blocks, escape lines starting with * or #+ by prefixing with a comma.\n"
       "REMEMBER! Org strips the leading comma on export.\n"
       "IMPORTANT: Write your response as plain org-mode text directly.\n"
       "Do NOT wrap your entire response inside #+begin_src, #+begin_example, or any other block.\n"
       "Only use #+begin_src blocks for actual code snippets within your response.\n"
       (if seq-todo
           (format "The document uses these TODO keywords: %s\n" seq-todo)
         "")
       "\n"
       "Example of correct org response format:\n"
       "#+begin_src org\n"
       (format "  %s Top heading   <-- REMEMBER! Correct heading level is %d\n"
               comma-stars response-level)
       "  - List item 1\n"
       "  - List item 2\n"
       (format "  %s Sub heading\n" comma-sub-stars)
       "  Some text under sub heading\n"
       "  ,#+begin_src elisp\n"
       "  (message \"example code block\")\n"
       "  ,#+end_src\n"
       "#+end_src\n"))))

(defun gptel-org-agent--transform-org-instructions (fsm)
  "Prompt transform: append org formatting instructions to system message.

When the response buffer is an org-mode buffer in agent subtree mode
or legacy subtree mode, dynamically resolve the heading level context
and append formatting instructions to `gptel--system-message'.

This transform must run AFTER `gptel-org-agent--transform-redirect'
so that `:buffer' in the FSM info points to the final response buffer
\(which may be an agent indirect buffer after redirection).

FSM is the request state machine.

On `user-error' signalled from the IB API (via `gptel-org-ib-fatal'),
invoke `gptel-org-agent--recover-from-ib-fatal' to annotate the user
task with DENIED + TERMINE, then re-signal the error."
  (condition-case err
      (when gptel-org-agent-format-instructions
        (let* ((info (gptel-fsm-info fsm))
               (response-buffer (plist-get info :buffer))
               (response-level
                (gptel-org-agent--response-heading-level response-buffer)))
          (when response-level
            (let* ((seq-todo (gptel-org-agent--seq-todo-line response-buffer))
                   (instructions
                    (gptel-org-agent--format-instructions response-level seq-todo)))
              (gptel-org--debug
               "org-agent transform-org-instructions: level=%d seq-todo=%S buffer=%S"
               response-level seq-todo (buffer-name response-buffer))
              ;; Signal that the response will be in org format, so the
              ;; markdown-to-org converter should be skipped.
              (with-current-buffer response-buffer
                (setq gptel-org--org-format-response t))
              ;; Append instructions to the system message in the prompt buffer.
              ;; We're already executing in the prompt buffer context (via
              ;; run-hook-wrapped in gptel-request.el).
              (setq gptel--system-message
                    (if (stringp gptel--system-message)
                        (concat gptel--system-message instructions)
                      ;; Multi-part directive: append to the first element (system part)
                      (if (consp gptel--system-message)
                          (cons (concat (car gptel--system-message) instructions)
                                (cdr gptel--system-message))
                        ;; No system message at all, just use instructions
                        instructions)))))))
    (user-error
     (gptel-org-agent--recover-from-ib-fatal err fsm)
     (signal (car err) (cdr err)))))

(defun gptel-org-agent--ensure-bol-before-terminator (_beg _end)
  "Ensure no streamed body content runs into the IB's TERMINE child.

The streaming insertion path uses `gptel-org-ib-streaming-marker'
which pins the FSM tracking-marker BEFORE the IB's TERMINE child
with insertion-type nil.  Streamed text accumulates between the
last body line and the TERMINE heading.  When the LLM ends a
response without a trailing newline, the line containing TERMINE
absorbs the trailing body text and surfaces as e.g.

  The system is ready for further operations.***** TERMINE

The bug-state TERMINE is no longer at BOL, so the regular
`gptel-org-ib-find-terminator' (anchored at `^') cannot locate it.
This hook scans for any `*+ TERMINE' occurrence mid-line and
inserts a newline immediately before the leading stars, restoring
TERMINE to its own line via the universal newline guard.

Runs on `gptel-post-response-functions' at low depth (before the
FEEDBACK-heading insertion at depth 95).  No-op when the current
buffer is not a registered IB."
  (when (gptel-org-ib-registered-p (current-buffer))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        ;; Scan for any `*+ TERMINE' occurrence; if it does not start
        ;; at BOL, the streaming path concatenated body onto its line.
        ;; Apply the universal newline guard right before the stars.
        (let ((inhibit-read-only t))
          (while (re-search-forward "\\*+ TERMINE\\b" nil t)
            (goto-char (match-beginning 0))
            ;; `gptel-org-ib-ensure-bol' is a no-op when already at
            ;; BOL (the common, healthy case).
            (gptel-org-ib-ensure-bol)
            ;; Advance past this TERMINE heading so `re-search-forward'
            ;; cannot match the same occurrence again.  An insertion
            ;; above invalidates `match-end', so use `forward-line'.
            (forward-line 1)))))))

(defun gptel-org-agent--insert-user-heading (_beg _end)
  "Insert a user/feedback heading after the agent subtree when response completes.
Added to `gptel-post-response-functions'.

In keyword mode (`gptel-org-use-todo-keywords'), creates a FEEDBACK
heading as a sibling of the agent subtree (same level), transitions
the agent heading to AI-DONE (removing its agent tag), and transitions
the parent TODO heading to FEEDBACK.

In tag mode, creates a :user: tagged heading as a child of the agent
subtree (original behavior).

This heading serves as the prompt location for the user's next message
in the conversation."
  (gptel-org--debug "insert-user-heading: buf=%S indirect=%s"
                    (buffer-name)
                    (if (gptel-org-ib-registered-p (current-buffer)) "yes" "no"))
  (let ((in-agent (gptel-org--in-agent-indirect-buffer-p)))
    (gptel-org--debug "insert-user-heading: in-agent-indirect=%s" in-agent)
    (when-let* ((in-agent)
                ;; Only insert user heading for top-level agents, not
                ;; sub-agents.  Sub-agents (gatherer, researcher, etc.)
                ;; don't have interactive conversations — they return
                ;; results to the parent agent.  Sub-agent tags have 2+
                ;; '@' signs (e.g. "researcher@main@agent"), main agent
                ;; tags have exactly 1 (e.g. "main@agent").
                (agent-tag (gptel-org-agent--current-agent-tag))
                (base-buffer (and (gptel-org-ib-registered-p (current-buffer))
                                  (gptel-org-ib-base (current-buffer))))
                (user-tag (if (boundp 'gptel-org-user-tag)
                              gptel-org-user-tag
                            "user")))
      (gptel-org--debug "insert-user-heading: agent-tag=%S base-buffer=%S user-tag=%S"
                        agent-tag (buffer-name base-buffer) user-tag)
      ;; Skip sub-agent buffers: sub-agents don't have interactive
      ;; conversations, so no user heading is needed.
      (unless (string-match-p "@.*@" agent-tag)
      ;; Find the agent heading in the indirect buffer to locate its
      ;; position in the base buffer
      (let ((agent-heading-pos
             (save-excursion
               (goto-char (point-min))
               (when (org-at-heading-p) (point)))))
        (gptel-org--debug "insert-user-heading: agent-heading-pos=%S point-min=%d"
                          agent-heading-pos (point-min))
        (when agent-heading-pos
          (with-current-buffer base-buffer
            (save-excursion
              (goto-char agent-heading-pos)
              (if (not (org-at-heading-p))
                  (gptel-org--debug "insert-user-heading: NOT at heading in base buffer at pos %d"
                                    agent-heading-pos)
                (let* ((agent-level (org-current-level))
                       (agent-tags (org-get-tags nil t))
                       (agent-heading (org-get-heading t t t t))
                       (inhibit-read-only t)
                       (keyword-mode (and (boundp 'gptel-org-use-todo-keywords)
                                          gptel-org-use-todo-keywords)))
                  (gptel-org--debug "insert-user-heading: agent heading=%S level=%d tags=%S keyword-mode=%s"
                                    agent-heading agent-level agent-tags keyword-mode)
                  ;; In keyword mode, the user heading is a sibling of
                  ;; the agent subtree (same level).  In tag mode, it's a
                  ;; child (agent-level + 1) for backward compatibility.
                  (let* ((user-level (if keyword-mode agent-level (1+ agent-level)))
                         ;; Use a marker so the position tracks buffer
                         ;; modifications (keyword transition, tag removal)
                         ;; that happen before we insert the FEEDBACK heading.
                         (subtree-end (save-excursion
                                        (org-end-of-subtree t)
                                        (point-marker)))
                         (has-user
                          (if keyword-mode
                              ;; Keyword mode: check for FEEDBACK sibling
                              ;; after the agent subtree
                              (save-excursion
                                (goto-char subtree-end)
                                (and (re-search-forward org-heading-regexp nil t)
                                     (beginning-of-line)
                                     (= (org-current-level) user-level)
                                     (gptel-org--heading-is-user-p)))
                            ;; Tag mode: check for :user: child within
                            ;; agent subtree (original behavior).
                            ;; Backward search OK: probing the LAST
                            ;; heading in an existing subtree to detect
                            ;; an already-present :user: child.  This
                            ;; is content discovery, not IB insertion-
                            ;; point discovery — the actual user heading
                            ;; is created via the IB API below.
                            (save-excursion
                              (goto-char subtree-end)
                              (end-of-line)
                              (and (re-search-backward org-heading-regexp
                                                       agent-heading-pos t)
                                   (= (org-current-level) user-level)
                                   (let ((last-tags (org-get-tags nil t)))
                                     (cl-some
                                      (lambda (tg)
                                        (string-equal-ignore-case tg user-tag))
                                      last-tags)))))))
                    (gptel-org--debug "insert-user-heading: subtree-end=%d user-level=%d has-user=%s"
                                      (marker-position subtree-end) user-level has-user)
                    ;; In keyword mode, transition the agent heading to
                    ;; AI-DONE and remove its agent tag so that
                    ;; find-agent-subtree won't confuse completed subtrees
                    ;; with active ones.  The parent heading's transition
                    ;; to FEEDBACK is handled by
                    ;; gptel-org-tasks--clear-active-task (which runs at
                    ;; depth 0, before this function at depth 95).
                    (when keyword-mode
                      (let ((done-kw (or (bound-and-true-p gptel-org-tasks-done-keyword) "AI-DONE")))
                        ;; Agent heading -> AI-DONE, remove agent tag
                        (save-excursion
                          (goto-char agent-heading-pos)
                          (gptel-org-agent--set-todo-keyword done-kw)
                          (org-set-tags nil)
                          (gptel-org--debug
                           "insert-user-heading: agent heading transitioned to %s, tag removed"
                           done-kw))
                        ;; Transition any ALLOWED/DENIED tool confirmation
                        ;; headings within the agent subtree to AI-DONE.
                        ;; These headings were created by
                        ;; gptel-org-agent--display-tool-calls and left in
                        ;; their confirmation state after tool execution.
                        (let ((confirm-keywords
                               (when (boundp 'gptel-org-agent-tool-confirm-keywords)
                                 (cdr gptel-org-agent-tool-confirm-keywords))))
                          (when confirm-keywords
                            (save-excursion
                              (goto-char agent-heading-pos)
                              (let ((subtree-bound (save-excursion
                                                     (org-end-of-subtree t)
                                                     (point))))
                                (while (re-search-forward
                                        org-heading-regexp subtree-bound t)
                                  (let ((state (org-get-todo-state)))
                                    (when (and state
                                               (member state confirm-keywords))
                                      (gptel-org-agent--set-todo-keyword done-kw)
                                      (gptel-org--debug
                                       "insert-user-heading: tool confirm heading transitioned from %s to %s"
                                       state done-kw))))))))
                        ;; IB-4.3: remove the TERMINE child (if any) under
                        ;; the completing agent subtree.  TERMINE is a
                        ;; placeholder that keeps sibling IBs safe during
                        ;; concurrent streaming; once the agent has
                        ;; transitioned to AI-DONE, no more content will
                        ;; flow into this subtree, so the placeholder is
                        ;; redundant.  Also opportunistically sweep
                        ;; legacy RESULTS/FEEDBACK empty-body terminators
                        ;; — but don't let legacy data corruption break
                        ;; the normal completion path.
                        (save-excursion
                          (goto-char agent-heading-pos)
                          (when (org-at-heading-p)
                            (let ((removed
                                   (gptel-org-ib-remove-terminator "TERMINE")))
                              (when removed
                                (gptel-org--debug
                                 "insert-user-heading: removed TERMINE child of agent at %d"
                                 agent-heading-pos)))
                            (dolist (legacy-kw '("RESULTS" "FEEDBACK"))
                              (condition-case err
                                  (save-excursion
                                    (goto-char agent-heading-pos)
                                    (when (org-at-heading-p)
                                      (when (gptel-org-ib-remove-terminator
                                             legacy-kw)
                                        (gptel-org--debug
                                         "insert-user-heading: removed legacy %s terminator"
                                         legacy-kw))))
                                (error
                                 (gptel-org--debug
                                  "insert-user-heading: legacy %s sweep failed: %S (ignored)"
                                  legacy-kw err))))))))
                    (unless has-user
                      ;; Create the user/feedback heading via
                      ;; terminator-aware helpers so concurrent sibling
                      ;; IBs are not disturbed.
                      (save-excursion
                        (goto-char agent-heading-pos)
                        (if keyword-mode
                            ;; Sibling FEEDBACK at agent-level.
                            (let ((kw (or (bound-and-true-p gptel-org-user-keyword) "FEEDBACK")))
                              (gptel-org-ib-ensure-sibling-terminator kw user-level)
                              (gptel-org--debug
                               "insert-user-heading: ensured %s sibling at level %d"
                               kw user-level))
                          ;; Tag mode: CHILD :user: heading via
                          ;; terminator-aware create-heading.  We insert
                          ;; at end of the agent subtree (no sub-terminator
                          ;; for the user heading itself).
                          (let ((m (gptel-org-ib-create-heading
                                    nil "" (list user-tag) nil)))
                            (gptel-org--debug
                             "insert-user-heading: created :%s: child heading at level %d (marker %S)"
                             user-tag user-level m)))))
                    ;; Clean up marker
                    (set-marker subtree-end nil))))))))
      ;; Close the indirect buffer after all DONE processing completes.
      ;; Register on `gptel--done-cleanup-functions' so the closure runs
      ;; after post-response hooks, prompt prefix insertion, and
      ;; `gptel--fsm-last' — i.e. only when no code still references
      ;; the buffer via (plist-get info :buffer).  Using `run-at-time'
      ;; here is unreliable: timer dispatch can interleave with queued
      ;; request dispatch (`gptel--host-queue-dispatch'), which may
      ;; re-create or kill the buffer before the timer fires.
      (let ((buf (current-buffer)))
        (push (lambda ()
                (gptel-org-agent--close-indirect-buffer buf 'fold))
              gptel--done-cleanup-functions))))))

(defun gptel-org-agent--enable ()
  "Enable agent subtree integration for `gptel-send'.

Adds `gptel-org-agent--transform-redirect' to
`gptel-prompt-transform-functions' so that requests from org-mode
TODO headings are automatically routed to agent indirect buffers.
Also sets up tool confirmation advice and hooks."
  (add-to-list 'gptel-prompt-transform-functions
               #'gptel-org-agent--transform-redirect)
  ;; Org format instructions: must run AFTER transform-redirect so that
  ;; :buffer in FSM info points to the final response buffer.  Since
  ;; add-to-list prepends, adding this after redirect means it appears
  ;; earlier in the list and runs first.  We need it to run AFTER, so
  ;; append it to the end instead.
  (add-to-list 'gptel-prompt-transform-functions
               #'gptel-org-agent--transform-org-instructions t)
  ;; Add keybinding to gptel-mode-map for jumping to/from indirect buffers
  (when (boundp 'gptel-mode-map)
    (define-key gptel-mode-map (kbd "C-c g j")
                #'gptel-org-agent-jump-to-indirect-buffer))
  ;; Universal newline guard: ensure no streamed body runs into the
  ;; TERMINE sentinel.  Runs early (depth 10) so subsequent hooks
  ;; (e.g. `--insert-user-heading' at depth 95) operate on a
  ;; well-formed buffer.
  (add-hook 'gptel-post-response-functions
            #'gptel-org-agent--ensure-bol-before-terminator 10)
  ;; Insert :user: heading after agent response completes
  (add-hook 'gptel-post-response-functions
            #'gptel-org-agent--insert-user-heading 95)
  ;; Tool confirmation: advice on gptel--display-tool-calls
  (advice-add 'gptel--display-tool-calls :around
              #'gptel-org-agent--display-tool-calls-advice)
  ;; Tool results: advice to update existing headings instead of creating new ones
  (advice-add 'gptel--display-tool-results :around
              #'gptel-org-agent--display-tool-results-advice)
  ;; Tool confirmation: hook for org-mode buffers
  (add-hook 'org-mode-hook #'gptel-org-agent--setup-tool-confirm))

(defun gptel-org-agent--disable ()
  "Disable agent subtree integration for `gptel-send'."
  (setq gptel-prompt-transform-functions
        (remq #'gptel-org-agent--transform-redirect
              (remq #'gptel-org-agent--transform-org-instructions
                    gptel-prompt-transform-functions)))
  (when (boundp 'gptel-mode-map)
    (define-key gptel-mode-map (kbd "C-c g j") nil))
  ;; Remove user heading hook
  (remove-hook 'gptel-post-response-functions
               #'gptel-org-agent--insert-user-heading)
  ;; Remove BOL guard hook
  (remove-hook 'gptel-post-response-functions
               #'gptel-org-agent--ensure-bol-before-terminator)
  ;; Remove tool confirmation advice and hooks
  (advice-remove 'gptel--display-tool-calls
                 #'gptel-org-agent--display-tool-calls-advice)
  (advice-remove 'gptel--display-tool-results
                 #'gptel-org-agent--display-tool-results-advice)
  (remove-hook 'org-mode-hook #'gptel-org-agent--setup-tool-confirm))

;; Always register the transform when this module is loaded.
;; The transform function itself checks gptel-org-subtree-context
;; at runtime and is a no-op when disabled, so unconditional
;; registration is safe and avoids load-order races with
;; gptel-request's defcustom.
(with-eval-after-load 'gptel-request
  (gptel-org-agent--enable))


;;; ---- Sub-agent subtree integration (Phase 2) ------------------------------

(defun gptel-org-agent--current-agent-tag ()
  "Return the agent tag for the current buffer, or nil.

If the current buffer is an agent indirect buffer (created by
`gptel-org-agent--open-indirect-buffer'), return the agent tag from
the first heading's tags.  The agent tag is identified by matching
`gptel-org-agent--agent-tag-p'.

Returns nil if the current buffer is not an indirect buffer, not in
org-mode, or has no agent tag on its first heading."
  (when (and (gptel-org-ib-registered-p (current-buffer))
             (derived-mode-p 'org-mode))
    (save-excursion
      (goto-char (point-min))
      (when (org-at-heading-p)
        (cl-find-if #'gptel-org-agent--agent-tag-p
                    (org-get-tags nil t))))))

(defun gptel-org-agent--pending-tool-subtree-info (pending-id)
  "Return a subtree-info plist for a PENDING tool-call dispatch.

PENDING-ID is the tool-call entry key in
`gptel-org-agent--pending-tool-calls' (created by
`gptel-org-agent--display-tool-calls' at request-arrival time).

Returns a plist of the same shape as
`gptel-org-agent--setup-task-subtree' for use as the sub-agent's
indirect buffer + insertion marker:
  :indirect-buffer  - the entry's `:pending-ib'
  :heading-marker   - the entry's `:heading-marker' (in base buffer)
  :position-marker  - insertion marker inside the IB (BEFORE
                      its TERMINE child)
  :parent-tag       - nil (the PENDING IB has no agent tag)

Returns nil if the entry is missing, the IB is dead, or the
heading marker is detached.

This is the path used when the Agent tool is dispatched from a
PENDING tool-call confirmation: instead of creating a fresh
sub-agent subtree (legacy `setup-task-subtree' branch), the
sub-agent FSM streams into the existing PENDING IB.  See the
AI-DO \"EXECUTING should be state transition\" decisions in
gptel-ai.org."
  (when pending-id
    (let* ((entry (gethash pending-id gptel-org-agent--pending-tool-calls))
           (pending-ib (and entry (plist-get entry :pending-ib)))
           (heading-marker (and entry (plist-get entry :heading-marker))))
      (when (and entry
                 pending-ib (buffer-live-p pending-ib)
                 heading-marker (marker-buffer heading-marker))
        ;; Ensure gptel-mode is active in the IB so heading insertion
        ;; works correctly (mirrors the setup-task-subtree branch).
        (with-current-buffer pending-ib
          (unless gptel-mode
            (setq-local gptel-mode t))
          ;; Mark this IB as an agent indirect buffer (mirrors the
          ;; setup-task-subtree branch) so downstream code that checks
          ;; this flag for level/auto-correct decisions works.
          (setq-local gptel-org--agent-indirect-buffer-p t)
          ;; Enable auto-correct so gptel-org--ref-level is computed
          ;; and set on this IB.  Without this, sub-agent tool-call
          ;; heading insertion falls back to the parent agent's
          ;; ref-level, producing headings one level too shallow.
          (gptel-org--enable-auto-correct))
        (let ((pos-marker
               (gptel-org-agent--make-insertion-marker pending-ib)))
          (gptel-org--debug
           "org-agent pending-tool-subtree-info: REUSING pending-id=%s ib=%S pos=%d marker=%S"
           pending-id (buffer-name pending-ib)
           (marker-position pos-marker)
           (and (markerp heading-marker)
                (marker-position heading-marker)))
          (list :indirect-buffer pending-ib
                :heading-marker heading-marker
                :position-marker pos-marker
                :parent-tag nil))))))

(defun gptel-org-agent--setup-task-subtree (agent-type description)
  "Set up a subtree and indirect buffer for a sub-agent task.

AGENT-TYPE is the agent type string (e.g., \"researcher\", \"gatherer\").
DESCRIPTION is a short task description used for logging.

Must be called from within the parent agent's buffer context (which
could be the base org buffer or an indirect buffer for a parent agent).

Creates a child heading tagged with the appropriate agent tag, opens
an indirect buffer narrowed to it, and returns a plist with:
  :indirect-buffer  - the indirect buffer
  :heading-marker   - marker to the agent heading (in base buffer)
  :position-marker  - marker at the insertion point (inside indirect
                      buffer, after the heading line)
  :parent-tag       - the parent's agent tag (or nil for top-level)

Returns nil if `gptel-org-subtree-context' is disabled, we're not in
org-mode, or we can't find a heading context to create the subtree."
  (when (and gptel-org-subtree-context
             (derived-mode-p 'org-mode))
    (let* ((parent-tag (gptel-org-agent--current-agent-tag))
           (tag (gptel-org-agent--construct-tag agent-type parent-tag))
           ;; IB-5.3: derive the DOING keyword from the agent's
           ;; :state-words triad (e.g. "GATHERING" for the gatherer
           ;; agent) so sub-agent subtrees don't nest an AI-DOING
           ;; under an AI-DOING (IB-4 grammar violation).  Falls back
           ;; to the legacy gptel-org-tasks-doing-keyword when
           ;; gptel-agent is not loaded.
           (doing-keyword
            (or (and (fboundp 'gptel-agent-state-words)
                     (nth 1 (gptel-agent-state-words agent-type)))
                (bound-and-true-p gptel-org-tasks-doing-keyword)
                "AI-DOING"))
           ;; Determine the base buffer (for indirect buffers, go to the base)
           (base-buffer (if (gptel-org-ib-registered-p (current-buffer))
                            (gptel-org-ib-base (current-buffer))
                          (current-buffer))))
      (gptel-org--debug
       "org-agent setup-task-subtree: agent-type=%S parent-tag=%S tag=%S"
       agent-type parent-tag tag)
      ;; We need to be at a heading to create a child subtree.
      ;; In the indirect buffer, go to the first heading (the agent heading).
      ;; In the base buffer, we should be at or near a heading.
      (save-excursion
        ;; If we're in a narrowed indirect buffer, work within its restriction
        ;; but find the heading to create a child under.
        (goto-char (point-min))
        (unless (org-at-heading-p)
          (condition-case nil
              (org-back-to-heading t)
            (error nil)))
        (when (org-at-heading-p)
          ;; Check if there's already a subtree with this tag
          (let* ((existing (gptel-org-agent--find-agent-subtree tag))
                 (heading-marker nil)
                 (indirect-buf nil))
            (if existing
                ;; Reuse existing subtree — just open an indirect buffer
                (progn
                  (gptel-org--debug
                   "org-agent setup-task-subtree: reusing existing %S subtree"
                   tag)
                  (setq heading-marker existing)
                  (setq indirect-buf
                        (gptel-org-agent--open-indirect-buffer
                         base-buffer heading-marker)))
              ;; Create new subtree using terminator-aware insertion.
              ;; This ensures the heading is placed BEFORE the terminator
              ;; (TERMINE), so existing sibling indirect buffers are not
              ;; disturbed.
              ;; IB-4.2: sub-agents and top-level agents both use the
              ;; unified TERMINE terminator (previously RESULTS for
              ;; sub-agents and FEEDBACK/user-keyword for top-level).
              ;; Recognition of legacy RESULTS/FEEDBACK terminators is
              ;; preserved elsewhere (e.g. `make-insertion-marker') for
              ;; the transition window.
              ;; IB-5.3: ensure the per-agent DOING keyword (e.g.
              ;; GATHERING) is registered as an open/todo state in the
              ;; base buffer so `gptel-org-ib-insert-child' accepts it.
              ;; Uses done-state=nil to mirror how AI-DOING is
              ;; registered as an open state.
              (when (fboundp 'gptel-org--ensure-todo-state)
                (gptel-org--ensure-todo-state
                 doing-keyword
                 (and (boundp 'gptel-org--agent-state-face)
                      gptel-org--agent-state-face)
                 nil))
              (let* ((terminator-keyword "TERMINE")
                     ;; Capture the parent heading position as an
                     ;; explicit marker; this routes through the
                     ;; unified parent-aware API instead of the
                     ;; transitional `:point' shim.  marker-buffer
                     ;; will be the current (possibly indirect)
                     ;; buffer, and `gptel-org-ib-insert-child'
                     ;; resolves the rest.
                     (parent-marker (point-marker)))
                (gptel-org--debug
                 "org-agent setup-task-subtree: creating new subtree via insert-child (terminator=%S)"
                 terminator-keyword)
                (let ((result (gptel-org-ib-insert-child
                               parent-marker
                               doing-keyword
                               (or description "Sub-task")
                               :tags (list tag)
                               :terminator-keyword terminator-keyword)))
                  (setq indirect-buf (plist-get result :indirect-buffer))
                  (setq heading-marker (plist-get result :heading-marker)))
                ;; Apply agent-specific setup
                (with-current-buffer indirect-buf
                  (setq-local gptel-org--agent-indirect-buffer-p t)
                  (let ((entry (gptel-org-ib-get (buffer-name indirect-buf))))
                    (when entry
                      (setq-local gptel-org-agent--narrow-end-marker
                                  (plist-get entry :end-marker))))
                  (gptel-org--enable-auto-correct))))
            ;; Create a position marker inside the indirect buffer.
            ;; This is where the LLM response will start being inserted.
            (when indirect-buf
              ;; Ensure gptel-mode is active for proper heading insertion.
              (with-current-buffer indirect-buf
                (unless gptel-mode
                  (setq-local gptel-mode t)))
              (let ((pos-marker
                     (gptel-org-agent--make-insertion-marker indirect-buf)))
                (gptel-org--debug
                 "org-agent setup-task-subtree: CREATED indirect=%S pos=%d ref-level=%S base=%S tag=%S"
                 (buffer-name indirect-buf) (marker-position pos-marker)
                 (buffer-local-value 'gptel-org--ref-level indirect-buf)
                 (buffer-name base-buffer) tag)
                (list :indirect-buffer indirect-buf
                      :heading-marker heading-marker
                      :position-marker pos-marker
                      :parent-tag parent-tag)))))))))

(defun gptel-org-agent--extract-final-text (indirect-buffer)
  "Extract the final assistant response text from INDIRECT-BUFFER.

Scans the narrowed region of INDIRECT-BUFFER for text with the
`gptel' text property set to `response', which marks LLM-generated
content.  Returns the last contiguous block of such text.

If no `gptel' response text is found, returns all body text after
the first heading line as a fallback.

Returns nil if INDIRECT-BUFFER is not live or contains no text."
  (when (buffer-live-p indirect-buffer)
    (with-current-buffer indirect-buffer
      (save-excursion
        (save-restriction
          ;; Stay within the narrowed region
          (goto-char (point-min))
          (let ((result nil))
            ;; Strategy 1: Find the last region with 'gptel 'response property.
            ;; Walk forward collecting response regions, keep the last one.
            (let ((pos (point-min))
                  last-start last-end)
              (while (< pos (point-max))
                (let ((gptel-val (get-text-property pos 'gptel))
                      (next-change (next-single-property-change
                                    pos 'gptel nil (point-max))))
                  (when (eq gptel-val 'response)
                    (setq last-start pos
                          last-end next-change))
                  (setq pos next-change)))
              (when (and last-start last-end)
                (setq result (string-trim
                              (buffer-substring-no-properties
                               last-start last-end)))))
            ;; Strategy 2: Fallback - get all body text after first heading,
            ;; stopping at a TERMINE terminator child if one exists (TERMINE
            ;; is pre-seeded by `gptel-org-ib-create' and is not response
            ;; content).
            (unless (and result (not (string-empty-p result)))
              (goto-char (point-min))
              (when (org-at-heading-p)
                (forward-line 1))       ;skip the heading line
              (let* ((body-start (point))
                     (term-pos (save-excursion
                                 (goto-char (point-min))
                                 (when (org-at-heading-p)
                                   (gptel-org-ib-find-terminator "TERMINE"))))
                     (body-end (or term-pos (point-max))))
                (setq result (string-trim
                              (buffer-substring-no-properties
                               body-start body-end)))))
            (if (and result (not (string-empty-p result)))
                result
              nil)))))))


(defun gptel-org-agent--set-todo-keyword (keyword)
  "Set the TODO keyword of the heading at point to KEYWORD.
Uses `org-todo' for proper state tracking.  Idempotent: a no-op if
the heading already has KEYWORD."
  (when (org-at-heading-p)
    (let* ((current (org-get-todo-state))
           (level-before (org-current-level))
           (heading-before (buffer-substring-no-properties
                            (line-beginning-position)
                            (line-end-position))))
      (gptel-org--debug
       "set-todo-keyword: BEFORE org-todo: keyword=%s current=%s level=%d heading=%S"
       keyword current level-before heading-before)
      (unless (equal current keyword)
        (org-todo keyword)
        (let ((level-after (org-current-level))
              (heading-after (buffer-substring-no-properties
                              (line-beginning-position)
                              (line-end-position))))
          (gptel-org--debug
           "set-todo-keyword: AFTER org-todo: level=%d heading=%S"
           level-after heading-after)
          (when (/= level-before level-after)
            (gptel-org--debug
             "set-todo-keyword: WARNING level changed! %d -> %d"
             level-before level-after)))))))


;;; ---- Advisor agent integration (Phase 5) ----------------------------------

(defvar-local gptel-org-agent-include-subtrees nil
  "When non-nil, include @agent subtrees in context collection.
Set by the Advisor agent preset.  When nil (the default), @agent
subtrees are stripped from the prompt to keep context focused.")

(defun gptel-org-agent--strip-agent-subtrees (&optional protected-positions)
  "Remove @agent subtrees from the current buffer.
Scans all headings and deletes entire subtrees whose heading has
a tag matching `gptel-org-agent--agent-tag-p'.

This is called during prompt construction to exclude internal
agent conversation trees from the context sent to the LLM.

When PROTECTED-POSITIONS is non-nil, it should be a list of
buffer positions (beginning-of-line positions of headings).
Agent headings at these positions are skipped, preserving them
and their children.  This is used in branching context mode to
protect lineage headings that are structural ancestors of the
current conversation.

Subtrees are deleted in reverse buffer order (bottom to top) to
avoid position shifts affecting subsequent deletions."
  (save-excursion
    (let (regions)
      ;; First pass: collect regions of all @agent subtrees.
      ;; We collect outermost subtrees only — if a parent is an @agent
      ;; subtree, its children are included in the parent's region.
      (goto-char (point-min))
      (while (re-search-forward org-heading-regexp nil t)
        (let ((match-end (point)))      ;save end-of-match for forward progress
          (beginning-of-line)
          (if (and (org-at-heading-p)
                   (cl-some #'gptel-org-agent--agent-tag-p
                            (org-get-tags nil t))
                   ;; Skip lineage headings that must be preserved
                   (not (memq (point) protected-positions)))
              ;; Agent heading: collect region and skip past subtree
              (let ((beg (point))
                    (end (save-excursion
                           (org-end-of-subtree t)
                           ;; Include trailing newline if present
                           (when (and (not (eobp)) (looking-at-p "\n"))
                             (forward-char 1))
                           (point))))
                (unless (cl-some (lambda (r) (and (<= (car r) beg) (>= (cdr r) end)))
                                regions)
                  (push (cons beg end) regions))
                (goto-char end))
            ;; Non-agent heading: restore point past the match to ensure
            ;; forward progress (beginning-of-line moved us backwards)
            (goto-char match-end))))
      ;; Second pass: delete in reverse order (bottom to top)
      ;; regions were pushed, so they're already in reverse buffer order
      (dolist (region regions)
        (delete-region (car region) (cdr region))))))

(defun gptel-org-agent--advisor-preset ()
  "Return the Advisor agent preset plist.
The Advisor agent has read access to all agent subtrees, allowing
it to analyze what each sub-agent did, what tools they used, and
what they found."
  (list :include-agent-subtrees t
        :system "You are an Advisor agent with full visibility into all sub-agent conversations. Analyze what each agent did, what tools they used, and what they found. Provide insights, identify issues, and suggest improvements."))

(defun gptel-org-agent-register-advisor ()
  "Register the Advisor agent preset with gptel-agent.
Call this after gptel-agent is loaded."
  (when (boundp 'gptel-agent--agents)
    (setf (alist-get "advisor" gptel-agent--agents nil nil #'equal)
          (gptel-org-agent--advisor-preset))))


;;; ---- Tool confirmation subtree integration (Phase 6) ----------------------
;;
;; When `gptel-org-subtree-context' is enabled, tool calls requiring
;; confirmation are displayed as org headings with a PENDING TODO state
;; instead of the legacy overlay-based widget.  The user changes the
;; heading state to ALLOWED or DENIED (via org-todo) to approve or deny
;; the tool call, which triggers the FSM to continue.
;;
;; This replaces the overlay-based confirmation UI (C-c C-c / C-c C-k)
;; with an org-native workflow:
;;
;;   ******** PENDING Requesting permission to run: Bash
;;   (Bash "ls -la")
;;
;; The user changes PENDING → ALLOWED to run, or PENDING → DENIED to
;; deny (with optional note).  The org-after-todo-state-change-hook
;; handles the dispatch.

(defcustom gptel-org-agent-tool-confirm-keywords
  '("PENDING" "ALLOWED" "DENIED")
  "TODO keywords used for tool confirmation headings.

A list of three strings: (PENDING-KW ALLOWED-KW DENIED-KW).
These should be defined in the buffer's `#+SEQ_TODO' line.

PENDING: Initial state when tool call awaits user decision.
ALLOWED: User approves the tool call.
DENIED: User denies the tool call."
  :type '(list string string string)
  :group 'gptel)

;; IB-4.8: Universal request-prefix invariant for tool headings.
;; Every tool/agent heading is written as `<REQ> <STATE> <summary>'
;; during the request phase and transitions to bare `<STATE> <summary>'
;; after execution.  These pure helpers codify parsing of that form so
;; call sites and tests can agree on the grammar.
;;
;; Currently this invariant is enforced only along the confirmation
;; path — tools with `:confirm nil' bypass heading creation entirely.
;; See IB-4.8b for the follow-up that would extend the grammar to
;; auto-run tools.

(defun gptel-org-agent--tool-heading-req-prefix-p (heading-line)
  "Return the REQ keyword if HEADING-LINE has a request prefix.
HEADING-LINE is a single org heading string (with or without leading
stars).  REQ must be one of `gptel-org-agent-tool-confirm-keywords'
and must appear as the first word after the stars, followed by at
least one more word (the tool STATE).

Returns the matched REQ keyword as a string, or nil if no prefix."
  (when (and heading-line (stringp heading-line))
    (let* ((stripped (replace-regexp-in-string "^\\*+ +" "" heading-line))
           (tokens (split-string stripped " +" t))
           (first-word (car tokens)))
      (and first-word
           (cdr tokens)
           (member first-word gptel-org-agent-tool-confirm-keywords)
           first-word))))

(defun gptel-org-agent--tool-heading-strip-req-prefix (heading-line)
  "Remove the REQ prefix from HEADING-LINE if present.
Returns the modified string, or HEADING-LINE unchanged if no REQ
prefix is found.  Preserves leading stars."
  (if-let* ((req (gptel-org-agent--tool-heading-req-prefix-p heading-line)))
      (let* ((stars (if (string-match "^\\*+ +" heading-line)
                        (match-string 0 heading-line)
                      ""))
             (rest (substring heading-line (length stars)))
             (without-req (replace-regexp-in-string
                           (format "\\`%s +" (regexp-quote req)) "" rest)))
        (concat stars without-req))
    heading-line))

(defun gptel-org-agent--tool-state-triad (tool-name &optional args)
  "Return the three-state triad for a tool-call heading.

Returns (REQUEST DOING DONE) — three strings used for the PENDING
phase heading, the execution phase heading, and the completion phase
heading respectively.

For the Agent tool with a resolvable :subagent_type in ARGS, the
triad is taken from the target agent's :state-words property
\(see `gptel-agent-state-words').  For all other tools, the triad
collapses to three copies of the tool's state keyword
\(e.g. (\"BASH\" \"BASH\" \"BASH\")), preserving historical behaviour.

When the agent's state-words cannot be resolved (e.g. gptel-agent
not yet loaded), falls back to the triad derived from the
uppercased subagent_type so the existing PENDING→TOOLSTATE
transition continues to work."
  (let ((default-state (gptel-org--tool-state-keyword tool-name args)))
    (if (and (stringp tool-name)
             (string= tool-name "Agent")
             (listp args))
        (let ((st (plist-get args :subagent_type)))
          (cond
           ((and (stringp st) (not (string-empty-p st))
                 (fboundp 'gptel-agent-state-words))
            (gptel-agent-state-words st))
           (t (list default-state default-state default-state))))
      (list default-state default-state default-state))))

(defmacro gptel-org-agent--with-info-buffer (buf &rest body)
  "Execute BODY in BUF if it is a live buffer, otherwise in current buffer.

This ensures tool functions see the correct buffer-local variables
\(e.g. `gptel--fsm-last') even when triggered from the base buffer."
  (declare (indent 1) (debug t))
  `(if (and ,buf (buffer-live-p ,buf))
       (with-current-buffer ,buf ,@body)
     ,@body))

(defvar gptel-org-agent--pending-tool-calls (make-hash-table :test 'equal)
  "Global hash table mapping unique IDs to pending tool call data.

Each entry is ID → (:tool-calls TOOL-CALLS :info INFO :buffer BUFFER)
where TOOL-CALLS is the list of (tool-spec arg-plist process-tool-result)
and INFO is the FSM info plist.")

(defvar gptel-org-agent--pending-id-counter 0
  "Counter for generating unique pending tool call IDs.")

(defvar gptel-org-agent--dispatching-pending-id nil
  "Pending-id of the tool call currently being dispatched.

Bound dynamically by `gptel-org-agent--accept-tool-calls' to the
pending-id of the PENDING tool-call entry whose tool function is
being invoked.

Read by `gptel-agent--task' (the Agent tool function) to locate
the PENDING IB and heading marker for sub-agent dispatch, so that
the sub-agent re-uses the existing PENDING IB instead of creating
a fresh sub-agent subtree.

When nil (e.g. tool invoked outside the confirm flow), callers
fall back to the legacy `gptel-org-agent--setup-task-subtree'
path.")

(defun gptel-org-agent--generate-pending-id ()
  "Generate a unique ID for a pending tool call entry."
  (format "gptel-pending-%d-%s"
          (cl-incf gptel-org-agent--pending-id-counter)
          (format-time-string "%s")))

(defun gptel-org-agent--subtree-tool-confirm-p (info)
  "Return non-nil if tool confirmation should use subtree headings.

INFO is the FSM info plist.  Returns t when `gptel-org-subtree-context'
is enabled and the request buffer is an agent indirect buffer in
org-mode."
  (and gptel-org-subtree-context
       (let ((buf (plist-get info :buffer)))
         (and buf (buffer-live-p buf)
              (with-current-buffer buf
                (and (derived-mode-p 'org-mode)
                     (gptel-org-ib-registered-p buf)))))))

(defun gptel-org-agent--ensure-tool-confirm-hook (buf)
  "Ensure the tool confirmation hook is registered on BUF and its base buffer.

The PENDING heading is created in an indirect buffer, but users
typically change the TODO state from the base buffer.  This
ensures the `org-after-todo-state-change-hook' handler is
registered on both buffers so the state change is always caught."
  (dolist (b (delq nil (list buf (and (gptel-org-ib-registered-p buf)
                                      (gptel-org-ib-base buf)))))
    (when (buffer-live-p b)
      (with-current-buffer b
        (unless (memq #'gptel-org-agent--on-todo-state-change
                      org-after-todo-state-change-hook)
          (add-hook 'org-after-todo-state-change-hook
                    #'gptel-org-agent--on-todo-state-change nil t)
          (gptel-org--debug
           "org-agent tool-confirm: registered hook on buffer %s"
           (buffer-name b)))))))

(defun gptel-org-agent--display-tool-calls (tool-calls info)
  "Display TOOL-CALLS as separate PENDING org headings in dedicated IBs.

Creates one child heading per tool call under the agent heading,
each with a PENDING TODO state and its own unique ID.  Each PENDING
heading lives in its own dedicated indirect buffer so that subsequent
TODO state transitions (PENDING → ALLOWED → DOING → DONE) can be
performed via real `org-todo' calls on the heading at point-min of
that IB.

Stores each tool call's data in `gptel-org-agent--pending-tool-calls'
keyed by pending-id, with `:pending-ib' and `:heading-marker' so
`gptel-org-agent--update-tool-heading' can locate the heading without
buffer scanning.

TOOL-CALLS is a list of (tool-spec arg-plist process-tool-result).
INFO is the FSM info plist."
  (let* ((buf (plist-get info :buffer))
         (pending-kw (nth 0 gptel-org-agent-tool-confirm-keywords)))
    (with-current-buffer buf
      ;; Ensure the todo-state-change hook is registered on both the
      ;; indirect buffer and its base buffer so that the user can
      ;; change PENDING→ALLOWED from either buffer.
      (gptel-org-agent--ensure-tool-confirm-hook buf)
      ;; The `ensure-terminator' call below is a cheap idempotent
      ;; safety net for ill-formed inputs (e.g. older buffers loaded
      ;; from disk that pre-date the universal TERMINE invariant).
      ;; TERMINE is seeded by the generic factory `gptel-org-ib-create'
      ;; for every IB, so on a correctly-constructed IB this is a
      ;; no-op (returns a marker to the existing TERMINE).
      (save-excursion
        (goto-char (point-min))
        (when (org-at-heading-p)
          (ignore-errors
            (gptel-org-ib-ensure-terminator "TERMINE"))))
      ;; Suppress the auto-corrector during insertion.
      (let ((gptel-org--auto-correcting t))
        (gptel-org--debug
         "display-tool-calls: buffer=%S narrowed=%s num-tools=%d"
         (buffer-name) (if (buffer-narrowed-p) "yes" "no")
         (length tool-calls))
        ;; Create one PENDING heading + dedicated IB per tool call
        (dolist (tc tool-calls)
          (let* ((tool-spec (car tc))
                 (arg-plist (cadr tc))
                 (tool-name (gptel-tool-name tool-spec))
                 (pending-id (gptel-org-agent--generate-pending-id))
                 ;; Build heading title in the same format used when
                 ;; the heading later transitions to the DOING state.
                 ;; IB-5.3: the STATE word is the REQUEST token of
                 ;; the (REQUEST DOING DONE) triad.
                 (triad (gptel-org-agent--tool-state-triad
                         tool-name arg-plist))
                 (tool-state (nth 0 triad))
                 (args-title (gptel-org--format-tool-args-title
                              arg-plist
                              (gptel-org--tool-args-title-excludes tool-name)))
                 (full-title (if (string-empty-p args-title)
                                 tool-state
                               (concat tool-state " " args-title)))
                 (truncated-title
                  (string-replace
                   "\n" " "
                   (truncate-string-to-width
                    full-title
                    (max 60 (floor (* (window-width) 0.6)))
                    0 nil " ..."))))
            ;; Register ALL THREE triad words eagerly so the later
            ;; REQUEST → DOING → DONE transitions can switch to
            ;; recognised org-todo keywords without an org-mode
            ;; restart.
            (when (fboundp 'gptel-org--ensure-todo-state)
              (dolist (state triad)
                (gptel-org--ensure-todo-state
                 state
                 (and (boundp 'gptel-org--tool-state-face)
                      gptel-org--tool-state-face)
                 t)))
            ;; Create the PENDING heading and its dedicated indirect
            ;; buffer.  The heading is inserted as a child of the
            ;; agent heading (point-min) BEFORE the TERMINE
            ;; terminator, so existing sibling IBs are not disturbed.
            (let* ((heading-marker nil)
                   (pending-ib nil)
                   (ib-name (format "*gptel:tool-%s*" pending-id)))
              (save-excursion
                (goto-char (point-min))
                (unless (org-at-heading-p)
                  (org-back-to-heading t))
                ;; IB-4.8 invariant: the request phase writes
                ;; `<REQ> <STATE> <summary>'.  PENDING-KW is the REQ
                ;; keyword (from `gptel-org-agent-tool-confirm-keywords');
                ;; TRUNCATED-TITLE already begins with TOOL-STATE (the
                ;; STATE keyword).  The REQ prefix is dropped by
                ;; `gptel-org-agent--update-tool-heading' once the tool
                ;; runs.  See `gptel-org-agent--tool-heading-req-prefix-p'
                ;; for the pure parser used by tests.
                (let* ((parent-marker (point-marker))
                       (result (gptel-org-ib-insert-child
                                parent-marker
                                pending-kw truncated-title
                                :terminator-keyword "TERMINE"
                                :name ib-name)))
                  (setq heading-marker (plist-get result :heading-marker)
                        pending-ib (plist-get result :indirect-buffer))))
              ;; Inside the dedicated IB, set GPTEL_PENDING_ID and
              ;; insert the body (the (:name :args ...) plist that the
              ;; rest of the system reads back).
              (with-current-buffer pending-ib
                (let ((inhibit-read-only t)
                      (gptel-org--auto-correcting t))
                  (save-excursion
                    (goto-char (point-min))
                    (unless (org-at-heading-p)
                      (org-back-to-heading t))
                    (org-set-property "GPTEL_PENDING_ID" pending-id)
                    ;; Move past the heading + property drawer to
                    ;; insert the body.
                    (goto-char (point-min))
                    (org-end-of-meta-data t)
                    (insert (format "(:name %S :args %S)\n\n"
                                    tool-name arg-plist)))))
              ;; Register the hook on the dedicated IB too — the user
              ;; might transition state from inside the pending IB.
              (gptel-org-agent--ensure-tool-confirm-hook pending-ib)
              ;; Store the entry with all the data needed for
              ;; --update-tool-heading and --on-todo-state-change.
              (puthash pending-id
                       (list :tool-calls (list tc)
                             :info info
                             :buffer buf
                             :pending-ib pending-ib
                             :heading-marker heading-marker)
                       gptel-org-agent--pending-tool-calls)
              ;; Fold the PENDING heading inside its dedicated IB so
              ;; it doesn't clutter the agent buffer view either.
              (with-current-buffer pending-ib
                (save-excursion
                  (goto-char (point-min))
                  (when (org-at-heading-p)
                    (ignore-errors (org-fold-subtree t)))))
              (gptel-org--debug
               "org-agent tool-confirm: created PENDING heading for %s (id=%s ib=%s)"
               tool-name pending-id (buffer-name pending-ib)))))
        ;; Mark tool-pending so the FSM knows we're waiting
        (plist-put info :tool-pending t)))))

(defun gptel-org-agent--accept-tool-calls (tool-calls info &optional pending-id)
  "Accept and run TOOL-CALLS with explicit INFO.

Restores the correct dynamic bindings from INFO before running each
tool.  Executes in the buffer stored in INFO's :buffer slot so that
buffer-local state (including `gptel--fsm-last') is available to
tool functions like `gptel-agent--task'.

PENDING-ID, when non-nil, is bound to
`gptel-org-agent--dispatching-pending-id' for the duration of the
tool dispatch.  This lets `gptel-agent--task' (the Agent tool
function) locate the existing PENDING IB and reuse it for
sub-agent dispatch instead of creating a fresh subtree (see
IB-7 and the AI-DO \"EXECUTING should be state transition of
ALLOWED EXECUTE heading\" decisions in gptel-ai.org)."
  (let ((gptel--preset (or (plist-get info :preset)
                           (and (boundp 'gptel--preset) gptel--preset)))
        (gptel-backend (or (plist-get info :backend)
                           (and (boundp 'gptel-backend) gptel-backend)))
        (gptel-model (or (plist-get info :model)
                         (and (boundp 'gptel-model) gptel-model)))
        (gptel-org-agent--dispatching-pending-id pending-id)
        (buf (plist-get info :buffer)))
    (when (eq gptel-log-level 'debug)
      (gptel--log
       (format "accept-tool-calls ENTRY: buffer=%s base-buffer=%s info-buffer=%s buf-live=%s num-tools=%d"
               (buffer-name) (and (gptel-org-ib-registered-p (current-buffer))
                                  (buffer-name (gptel-org-ib-base (current-buffer))))
               (and buf (buffer-name buf)) (and buf (buffer-live-p buf))
               (length tool-calls))
       "tool-call-debug" 'no-json)
      (gptel--log
       (format "accept-tool-calls: info keys=%S fsm-last-in-current=%s fsm-last-in-info-buf=%s"
               (cl-loop for (k _v) on info by #'cddr collect k)
               (if (and (boundp 'gptel--fsm-last) gptel--fsm-last)
                   (gptel--fsm-summary gptel--fsm-last) "nil")
               (if (and buf (buffer-live-p buf)
                         (buffer-local-value 'gptel--fsm-last buf))
                   (gptel--fsm-summary
                    (buffer-local-value 'gptel--fsm-last buf))
                 "nil"))
       "tool-call-debug" 'no-json))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        (gptel--update-status " Calling tool..." 'mode-line-emphasis)))
    ;; Execute tools in the info buffer so that buffer-local variables
    ;; (gptel--fsm-last, gptel-mode, etc.) are correct.  When the user
    ;; changes PENDING→ALLOWED from the base buffer, the current buffer
    ;; lacks the FSM that tool functions (e.g. gptel-agent--task) need.
    (gptel-org-agent--with-info-buffer buf
    (cl-loop for (tool-spec arg-plist process-tool-result) in tool-calls
             for idx from 0
             do (when (eq gptel-log-level 'debug)
                  (gptel--log
                   (format "accept-tool-calls: [%d] tool=%s process-tool-result=%s"
                           idx (gptel-tool-name tool-spec)
                           (if process-tool-result "fn" "nil"))
                   "tool-call-debug" 'no-json))
             do (gptel-org--debug
                 "org-agent tool-confirm: executing tool %s"
                 (gptel-tool-name tool-spec))
             for arg-values = (gptel--map-tool-args tool-spec arg-plist)
             do (if (gptel-tool-async tool-spec)
                    (progn
                      (when (eq gptel-log-level 'debug)
                        (gptel--log
                         (format "debug-state-change: accept-tool[%d] ASYNC tool=%s calling in buffer=%s"
                                 idx (gptel-tool-name tool-spec) (buffer-name))
                         "debug-state-change" 'no-json))
                      (apply (gptel-tool-function tool-spec)
                             process-tool-result arg-values))
                  (let ((result
                         (condition-case errdata
                             (apply (gptel-tool-function tool-spec) arg-values)
                           (error
                            (gptel-org--debug
                             "org-agent tool-confirm: tool %s errored: %s"
                             (gptel-tool-name tool-spec) errdata)
                            (mapconcat #'gptel--to-string errdata " ")))))
                    (gptel-org--debug
                     "org-agent tool-confirm: tool %s completed, calling process-tool-result"
                     (gptel-tool-name tool-spec))
                    (when (eq gptel-log-level 'debug)
                      (gptel--log
                       (format "debug-state-change: accept-tool[%d] SYNC tool=%s completed, calling process-tool-result in buffer=%s result-len=%d"
                               idx (gptel-tool-name tool-spec) (buffer-name)
                               (length (gptel--to-string result)))
                       "debug-state-change" 'no-json))
                    (funcall process-tool-result result)))))))

(defun gptel-org-agent--deny-tool-calls (tool-calls info &optional reason)
  "Deny TOOL-CALLS with explicit INFO and optional REASON.

Sends a denial message back to the LLM for each tool call so it
can adjust its approach.  Does not depend on overlays."
  (let ((denial-msg
         (if (or (null reason) (string-empty-p reason))
             "Error: The user denied execution of this tool call.  \
Adjust your approach or ask the user for guidance."
           (format "Error: The user denied execution of this tool call.  \
Reason: %s" reason))))
    (let ((buf (plist-get info :buffer)))
      (when buf
        (with-current-buffer buf
          (gptel--update-status " Tools denied, informing LLM..." 'warning)))
      ;; Execute in the info buffer for correct buffer-local state
      (gptel-org-agent--with-info-buffer buf
        (cl-loop for (_tool-spec _arg-plist process-tool-result) in tool-calls
                 do (funcall process-tool-result denial-msg))))))

(defun gptel-org-agent--on-todo-state-change ()
  "Handle TODO state changes for PENDING tool confirmation headings.

When a PENDING heading is changed to ALLOWED, run the pending tool
calls.  When changed to DENIED, deny them and inform the LLM.

This function is added to `org-after-todo-state-change-hook'."
  (let ((hook-level (and (org-at-heading-p) (org-current-level)))
        (hook-heading (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
    (gptel-org--debug
     "org-agent tool-confirm: todo-state-change hook fired in buffer %s, state=%s, subtrees=%s level=%S heading=%S"
     (buffer-name) (and (boundp 'org-state) org-state)
     gptel-org-subtree-context hook-level hook-heading))
  (when (and (bound-and-true-p gptel-org-subtree-context)
             (org-at-heading-p))
    (let ((new-state org-state)
          (allowed-kw (nth 1 gptel-org-agent-tool-confirm-keywords))
          (denied-kw (nth 2 gptel-org-agent-tool-confirm-keywords))
          (pending-id (org-entry-get nil "GPTEL_PENDING_ID"))
          (level-at-entry (org-current-level)))
      (gptel-org--debug
       "org-agent tool-confirm: new-state=%s, pending-id=%s, hash-count=%d"
       new-state pending-id
       (hash-table-count gptel-org-agent--pending-tool-calls))
      (when pending-id
        (let ((entry (gethash pending-id
                              gptel-org-agent--pending-tool-calls)))
          (if (null entry)
              (gptel-org--debug
               "org-agent tool-confirm: WARNING no entry in hash table for id=%s"
               pending-id)
            (let ((tool-calls (plist-get entry :tool-calls))
                  (info (plist-get entry :info))
                  (stored-buf (plist-get entry :buffer)))
              (when (eq gptel-log-level 'debug)
                (gptel--log
                 (format "on-todo-change: entry found, tool-calls=%d stored-buf=%s stored-buf-live=%s info-buffer=%s"
                         (length tool-calls) stored-buf
                         (and stored-buf (buffer-live-p stored-buf))
                         (and info (plist-get info :buffer)
                              (buffer-name (plist-get info :buffer))))
                 "tool-call-debug" 'no-json)
                ;; Log each tool-call closure for debugging
                (cl-loop for (ts _ap ptr) in tool-calls
                         for i from 0
                         do (gptel--log
                             (format "on-todo-change: tool-call[%d] tool=%s process-tool-result=%s"
                                     i (and ts (gptel-tool-name ts))
                                     (if ptr "fn" "nil"))
                             "tool-call-debug" 'no-json)))
              ;; Check that the stored buffer is still alive
              (unless (and stored-buf (buffer-live-p stored-buf))
                (gptel-org--debug
                 "org-agent tool-confirm: WARNING stored buffer %s is dead"
                 stored-buf))
              ;; Lifecycle: do NOT remhash or delete GPTEL_PENDING_ID
              ;; here for the ALLOWED case — the entry's `:pending-ib'
              ;; and `:heading-marker' are needed by
              ;; `gptel-org-agent--update-tool-heading' once the tool
              ;; result arrives.  The DENIED branch performs its own
              ;; remhash + IB close below.
              (cond
               ((equal new-state allowed-kw)
                (gptel-org--debug
                 "org-agent tool-confirm: ALLOWED (id=%s), running %d tool calls, level=%d"
                 pending-id (length tool-calls) (org-current-level))
                (when (eq gptel-log-level 'debug)
                  (gptel--log
                   (format "debug-state-change: ALLOWED in buffer=%s base-buffer=%s stored-buf=%s stored-buf-live=%s info-buffer=%s info-buffer-live=%s fsm-last=%s"
                           (buffer-name)
                           (and (gptel-org-ib-registered-p (current-buffer))
                                (buffer-name (gptel-org-ib-base (current-buffer))))
                           (and stored-buf (buffer-name stored-buf))
                           (and stored-buf (buffer-live-p stored-buf))
                           (and info (plist-get info :buffer)
                                (buffer-name (plist-get info :buffer)))
                           (and info (plist-get info :buffer)
                                (buffer-live-p (plist-get info :buffer)))
                           (if (and (boundp 'gptel--fsm-last) gptel--fsm-last)
                               (gptel--fsm-summary gptel--fsm-last) "nil"))
                   "debug-state-change" 'no-json)
                  ;; Log closures to verify FSM captured in process-tool-result
                  (cl-loop for (ts _ap ptr) in tool-calls
                           for i from 0
                           do (gptel--log
                               (format "debug-state-change: ALLOWED tool-call[%d] tool=%s process-tool-result=%s"
                                       i (and ts (gptel-tool-name ts))
                                       (if ptr "fn" "nil"))
                               "debug-state-change" 'no-json)))
                (gptel-org-agent--accept-tool-calls tool-calls info
                                                    pending-id)
                (let ((level-after-accept (org-current-level))
                      (heading-after-accept
                       (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position))))
                  (gptel-org--debug
                   "org-agent tool-confirm: AFTER accept: level=%d (was %d) heading=%S"
                   level-after-accept level-at-entry heading-after-accept)
                  (when (/= level-at-entry level-after-accept)
                    (gptel-org--debug
                     "org-agent tool-confirm: WARNING level changed after accept! %d -> %d"
                     level-at-entry level-after-accept)))
                (message "Tool calls accepted, continuing..."))
               ((equal new-state denied-kw)
                (gptel-org--debug
                 "org-agent tool-confirm: DENIED (id=%s)" pending-id)
                ;; Check if user added a note after the heading (org prompts
                ;; for a note when DENIED has @ in SEQ_TODO)
                (let ((reason (org-entry-get nil "GPTEL_DENY_REASON")))
                  (gptel-org-agent--deny-tool-calls tool-calls info reason))
                ;; Lifecycle: on DENIED no tool result will follow, so
                ;; close the dedicated pending-ib and clear the entry
                ;; now.  The DENIED heading itself remains in the base
                ;; buffer for user reference.
                (let ((pending-ib (plist-get entry :pending-ib)))
                  (when (and pending-ib (buffer-live-p pending-ib))
                    (ignore-errors
                      (gptel-org-ib-close pending-ib nil))))
                (remhash pending-id gptel-org-agent--pending-tool-calls)
                (message "Tool calls denied, informing LLM..."))
               (t
                (gptel-org--debug
                 "org-agent tool-confirm: state %s is neither ALLOWED(%s) nor DENIED(%s), ignoring"
                 new-state allowed-kw denied-kw))))))))))

(defun gptel-org-agent--display-tool-calls-advice (orig-fn tool-calls info
                                                            &optional use-minibuffer)
  "Advice for `gptel--display-tool-calls' to use subtree headings.

When in agent subtree mode, create PENDING org headings instead of
the overlay-based confirmation widget.  Otherwise, delegate to the
original function ORIG-FN."
  (if (gptel-org-agent--subtree-tool-confirm-p info)
      (gptel-org-agent--display-tool-calls tool-calls info)
    (funcall orig-fn tool-calls info use-minibuffer)))


(defun gptel-org-agent--find-pending-entry (tool-spec args)
  "Return (PENDING-ID . ENTRY) matching TOOL-SPEC and ARGS, or nil.

Scans `gptel-org-agent--pending-tool-calls' for the oldest entry
whose stored tool-call's tool-spec matches TOOL-SPEC (by `eq', then
falling back to tool-name string comparison) and whose arg-plist
equals ARGS.  Returns nil if no match is found.

Used by `gptel-org-agent--update-tool-heading' to locate the
dedicated indirect buffer for a tool result without scanning
buffer text."
  (let ((tool-name (gptel-tool-name tool-spec))
        (best-id nil)
        (best-entry nil)
        (best-counter most-positive-fixnum))
    (maphash
     (lambda (id entry)
       (let* ((tcs (plist-get entry :tool-calls))
              (tc (car tcs)))
         (when (and tc
                    (let ((stored-spec (car tc))
                          (stored-args (cadr tc)))
                      (and (or (eq stored-spec tool-spec)
                               (and stored-spec
                                    (equal (gptel-tool-name stored-spec)
                                           tool-name)))
                           (equal stored-args args))))
           ;; Prefer the oldest entry by extracting the counter portion
           ;; of the pending-id ("gptel-pending-N-TIMESTAMP").
           (when (string-match "\\`gptel-pending-\\([0-9]+\\)-" id)
             (let ((counter (string-to-number (match-string 1 id))))
               (when (< counter best-counter)
                 (setq best-counter counter
                       best-id id
                       best-entry entry)))))))
     gptel-org-agent--pending-tool-calls)
    (when best-id
      (cons best-id best-entry))))

(defun gptel-org-agent--update-tool-heading (tool-spec args result id)
  "Transition the PENDING tool heading to its DOING state with RESULT.

Looks up the pending entry in `gptel-org-agent--pending-tool-calls'
by matching TOOL-SPEC and ARGS.  If found, switches to the entry's
`:pending-ib' (the dedicated indirect buffer holding the heading)
and uses real `org-todo' transitions to advance the heading from
PENDING/ALLOWED to its DOING state (triad[1]; e.g. EXECUTING for
the executor agent, GATHERING for the gatherer).

The heading title is rewritten via `org-edit-headline' to drop the
REQ-state prefix (the title alone, no leading state keyword), and
the body content is replaced with the call+result rendering.

ID is the tool-use identifier from the LLM response, used for the
`gptel' text property on the result body.

Returns non-nil if a matching entry was found and updated, nil
otherwise.  When nil is returned, the caller falls through to the
default rendering path."
  (let* ((tool-name (gptel-tool-name tool-spec))
         (entry-pair (gptel-org-agent--find-pending-entry tool-spec args))
         (pending-id (car entry-pair))
         (entry (cdr entry-pair))
         (pending-ib (and entry (plist-get entry :pending-ib)))
         (heading-marker (and entry (plist-get entry :heading-marker))))
    (when (and entry
               pending-ib (buffer-live-p pending-ib)
               heading-marker (marker-buffer heading-marker))
      (let* ((triad (gptel-org-agent--tool-state-triad tool-name args))
             (doing-state (nth 1 triad))
             (call (prin1-to-string `(:name ,tool-name :args ,args)))
             (result-str (if (stringp result) result (format "%S" result)))
             ;; For most tools the result is opaque and must be
             ;; protected inside a #+begin_tool block; for tools
             ;; whose result is already org prose (currently the
             ;; Agent dispatcher) we emit it verbatim under a
             ;; RESULTS child heading instead.  See
             ;; `gptel-org--tool-result-as-org-p'.
             (as-org (and (fboundp 'gptel-org--tool-result-as-org-p)
                          (gptel-org--tool-result-as-org-p tool-name)))
             (escaped-result (if as-org result-str
                               (org-escape-code-in-string result-str)))
             (args-title (gptel-org--format-tool-args-title
                          args
                          (gptel-org--tool-args-title-excludes tool-name)))
             (full-title (if (string-empty-p args-title)
                             doing-state
                           (concat doing-state " " args-title)))
             (truncated-title
              (string-replace
               "\n" " "
               (truncate-string-to-width
                full-title
                (max 60 (floor (* (window-width) 0.6)))
                0 nil " ...")))
             ;; Strip leading "DOING-STATE " from truncated-title to
             ;; get the bare title for `org-edit-headline'.  The TODO
             ;; keyword is set separately via `org-todo' so the title
             ;; portion of the heading line should NOT repeat it.
             (title-no-state
              (cond
               ((string-empty-p args-title) "")
               ((string-prefix-p (concat doing-state " ") truncated-title)
                (substring truncated-title (1+ (length doing-state))))
               (t truncated-title))))
        ;; Register the DOING state keyword so `org-todo' accepts it.
        ;; IB-4.8 invariant: this is the REQ-dropping site.  After
        ;; this function runs, `gptel-org-agent--tool-heading-req-prefix-p'
        ;; returns nil on the rewritten heading.
        (when (fboundp 'gptel-org--ensure-todo-state)
          (gptel-org--ensure-todo-state
           doing-state
           (and (boundp 'gptel-org--tool-state-face)
                gptel-org--tool-state-face)
           t))
        (with-current-buffer pending-ib
          (save-excursion
            (let ((inhibit-read-only t)
                  (gptel-org--auto-correcting t))
              (goto-char (point-min))
              (unless (org-at-heading-p)
                (org-back-to-heading t))
              ;; 1. Real `org-todo' state transition.  Suppress the
              ;;    confirm-state-change hook because this is a
              ;;    programmatic transition driven by the tool result,
              ;;    not by user action.
              (let ((org-after-todo-state-change-hook nil))
                (gptel-org-agent--set-todo-keyword doing-state))
              ;; 2. Rewrite the headline title (drops the REQ-state
              ;;    prefix; keeps just the args summary).  org-todo
              ;;    above set the TODO keyword to DOING-STATE; the
              ;;    title still reads "<old-REQ-state> <args-title>",
              ;;    so we replace it with `<args-title>' alone.
              (org-back-to-heading t)
              (org-edit-headline title-no-state)
              ;; Mark the heading line itself as gptel 'ignore so the
              ;; outline parser skips it.
              (let ((line-beg (line-beginning-position))
                    (line-end (line-end-position)))
                (put-text-property line-beg line-end
                                   'gptel 'ignore)
                (put-text-property line-beg line-end
                                   'front-sticky '(gptel)))
              ;; 3. Replace the body: delete everything from end of
              ;;    heading line through end of subtree (this also
              ;;    wipes the GPTEL_PENDING_ID property drawer), then
              ;;    insert the new call+result body.
              (let* ((stars (make-string (org-current-level) ?*))
                     (body-text
                      (if (fboundp 'gptel-org--tool-body-text)
                          (gptel-org--tool-body-text
                           tool-name call escaped-result stars)
                        (concat call "\n"
                                "#+begin_tool\n"
                                escaped-result "\n"
                                "#+end_tool\n")))
                     (heading-line-end (line-end-position))
                     (subtree-end (save-excursion
                                    (org-end-of-subtree t)
                                    (point))))
                (let ((inhibit-modification-hooks t))
                  (delete-region heading-line-end subtree-end)
                  (goto-char heading-line-end)
                  (insert "\n"
                          (propertize body-text
                                      'gptel `(tool . ,id)))))
              ;; 4. Fold the heading.
              (goto-char (point-min))
              (when (org-at-heading-p)
                (ignore-errors (org-fold-subtree t)))))
          ;; Reset cache so subsequent operations see the new state.
          (org-element-cache-reset))
        (gptel-org--debug
         "org-agent update-tool-heading: id=%s tool=%s -> %s (ib=%s)"
         pending-id tool-name doing-state (buffer-name pending-ib))
        ;; Mark the entry as transitioned by recording the doing-state.
        ;; Lifecycle: entry remains in the hash table; IB stays alive
        ;; until a separate cleanup step (mirrors sub-agent IB
        ;; semantics — the heading is queryable in its IB until the
        ;; user or supervisor closes it).
        (puthash pending-id
                 (plist-put (copy-sequence entry)
                            :doing-state doing-state)
                 gptel-org-agent--pending-tool-calls)
        t))))

(defun gptel-org-agent--display-tool-results-advice (orig-fn tool-results info)
  "Advice for `gptel--display-tool-results' to update existing headings.

When in agent subtree mode, find the existing PENDING/ALLOWED headings
created by `gptel-org-agent--display-tool-calls' and transition them to
TOOL state with the result body, instead of creating new TOOL headings.

For tool results that don't have a matching heading (shouldn't normally
happen), fall through to ORIG-FN for default handling.

Respects `gptel-include-tool-results' filtering: only updates headings
for tools whose results should be displayed."
  (when (eq gptel-log-level 'debug)
    (gptel--log
     (format "display-tool-results-advice: subtree-tool-confirm=%S agent-indirect=%S buffer=%S"
             (gptel-org-agent--subtree-tool-confirm-p info)
             (and (fboundp 'gptel-org--in-agent-indirect-buffer-p)
                  (with-current-buffer (plist-get info :buffer)
                    (gptel-org--in-agent-indirect-buffer-p)))
             (plist-get info :buffer))
     "tool-heading-debug" 'no-json))
  (if (gptel-org-agent--subtree-tool-confirm-p info)
      (let* ((start-marker (plist-get info :position))
             (buf (plist-get info :buffer)))
        (when (and buf (buffer-live-p buf)
                   gptel-include-tool-results)
          (with-current-buffer buf
            (let* ((include-names
                    (mapcar #'gptel-tool-name
                            (cl-remove-if-not #'gptel-tool-include
                                              (plist-get info :tools))))
                   (unmatched nil))
              (save-excursion
                ;; Position near the end of content so backward search
                ;; finds the most recent headings first
                (goto-char (or (plist-get info :tracking-marker)
                               start-marker))
                (cl-loop
                 for (tool-spec args result) in tool-results
                 for name = (gptel-tool-name tool-spec)
                 ;; Respect the same include filtering as the original
                 when (or (eq gptel-include-tool-results t)
                          (member name include-names))
                 do (let* ((tool-use
                            (cl-find-if
                             (lambda (tu) (equal (plist-get tu :name) name))
                             (plist-get info :tool-use)))
                           (id (plist-get tool-use :id)))
                      (let ((updated (gptel-org-agent--update-tool-heading
                                      tool-spec args result id)))
                        (when (eq gptel-log-level 'debug)
                          (gptel--log
                           (format "display-tool-results-advice: tool=%s update-heading=%S"
                                   name (if updated "matched" "unmatched"))
                           "tool-heading-debug" 'no-json))
                        (unless updated
                          ;; No matching heading found — collect for fallback
                          (push (list tool-spec args result) unmatched))))))
              ;; Any unmatched results get the default treatment
              (when unmatched
                (when (eq gptel-log-level 'debug)
                  (gptel--log
                   (format "display-tool-results-advice: falling through to orig-fn with %d unmatched results"
                           (length unmatched))
                   "tool-heading-debug" 'no-json))
                (funcall orig-fn (nreverse unmatched) info))))))
    ;; Not in agent subtree mode — use original function
    (when (eq gptel-log-level 'debug)
      (gptel--log
       "display-tool-results-advice: NOT in agent subtree mode, calling orig-fn directly"
       "tool-heading-debug" 'no-json))
    (funcall orig-fn tool-results info)))

(defun gptel-org-agent--setup-tool-confirm ()
  "Set up tool confirmation hooks for the current org buffer.

Adds `gptel-org-agent--on-todo-state-change' to
`org-after-todo-state-change-hook' (buffer-local)."
  (when (and (derived-mode-p 'org-mode)
             (bound-and-true-p gptel-org-subtree-context))
    (add-hook 'org-after-todo-state-change-hook
              #'gptel-org-agent--on-todo-state-change nil t)))


;;; ---- Navigation between base and indirect buffers -------------------------

(defun gptel-org-agent--find-indirect-buffer-at-point ()
  "Find the agent indirect buffer corresponding to the subtree at point.

Search upward from point through headings looking for one tagged
with an agent tag that has a live indirect buffer.  Return the
buffer, or nil if none found."
  (save-excursion
    (when (or (org-at-heading-p)
              (ignore-errors (org-back-to-heading t)))
      (let ((base-buffer (if (gptel-org-ib-registered-p (current-buffer))
                             (gptel-org-ib-base (current-buffer))
                           (current-buffer)))
            result)
        (while (and (not result) (org-at-heading-p))
          (let ((tag (cl-find-if #'gptel-org-agent--agent-tag-p
                                 (org-get-tags nil t))))
            (when tag
              (let* ((buf-name (gptel-org-ib-compute-name
                                base-buffer (point) tag))
                     (buf (get-buffer buf-name)))
                (when (and buf (buffer-live-p buf))
                  (setq result buf)))))
          (unless result
            (condition-case nil
                (org-up-heading-all 1)
              (error (goto-char (point-min))))))
        result))))

(defun gptel-org-agent--find-parent-indirect-buffer ()
  "Find the parent indirect buffer for the current agent indirect buffer.

If the current buffer is an indirect buffer narrowed to a nested
agent subtree (e.g., researcher@main@agent), find the indirect
buffer for the parent agent (e.g., main@agent).  Returns nil if
no parent indirect buffer exists or we're already at the top
level."
  (when-let* ((base (and (gptel-org-ib-registered-p (current-buffer))
                         (gptel-org-ib-base (current-buffer))))
              (tag (gptel-org-agent--current-agent-tag)))
    ;; Strip the first component to get parent tag:
    ;; "researcher@main@agent" -> "main@agent"
    (let ((parent-tag (when (string-match "^[^@]+@\\(.+\\)" tag)
                        (match-string 1 tag))))
      (when (and parent-tag
                 (gptel-org-agent--agent-tag-p parent-tag))
        ;; Find the heading with the parent tag in the base buffer
        (with-current-buffer base
          (save-excursion
            (goto-char (point-min))
            (let (result)
              (while (and (not result)
                          (re-search-forward org-heading-regexp nil t))
                (beginning-of-line)
                (when (member parent-tag (org-get-tags nil t))
                  (let* ((buf-name (gptel-org-ib-compute-name
                                    base (point) parent-tag))
                         (buf (get-buffer buf-name)))
                    (when (and buf (buffer-live-p buf))
                      (setq result buf))))
                (end-of-line))
              result)))))))

;;;###autoload
(defun gptel-org-agent-jump-to-indirect-buffer ()
  "Jump between base org buffer and agent indirect buffers.

When in a base org buffer (or parent indirect buffer), find the
agent indirect buffer for the subtree at point and switch to it.

When in an agent indirect buffer, jump to the parent indirect
buffer if one exists, otherwise jump to the base buffer at the
position of the agent subtree.

\\[gptel-org-agent-jump-to-indirect-buffer] is bound to \\`C-c g j' in
`gptel-mode'."
  (interactive)
  (cond
   ;; In an indirect buffer: jump to parent or base
   ((gptel-org-ib-registered-p (current-buffer))
    (let ((parent (gptel-org-agent--find-parent-indirect-buffer))
          (base (gptel-org-ib-base (current-buffer)))
          (pos (+ (point-min)
                  (- (point) (point-min)))))
      (if parent
          (progn
            (pop-to-buffer parent)
            (gptel-org--debug "org-agent jump: indirect -> parent %S"
                              (buffer-name parent)))
        ;; Jump to base buffer at same position
        (pop-to-buffer base)
        (goto-char pos)
        (gptel-org--debug "org-agent jump: indirect -> base %S"
                          (buffer-name base)))))
   ;; In a base buffer: find and jump to indirect buffer at point
   (t
    (if-let ((indirect (gptel-org-agent--find-indirect-buffer-at-point)))
        (progn
          (pop-to-buffer indirect)
          (gptel-org--debug "org-agent jump: base -> indirect %S"
                            (buffer-name indirect)))
      (user-error "No agent indirect buffer found for subtree at point")))))


;;; ---- Manual PENDING task execution ----------------------------------------

(defun gptel-org-agent--find-pending-heading ()
  "Find the nearest PENDING tool confirmation heading.

Search at point first, then search forward and backward from point
for a heading with the PENDING TODO keyword and a GPTEL_PENDING_ID
property.  Return the position of the heading, or nil."
  (let ((pending-kw (nth 0 gptel-org-agent-tool-confirm-keywords)))
    (or
     ;; Check current heading
     (save-excursion
       (when (and (or (org-at-heading-p)
                      (ignore-errors (org-back-to-heading t)))
                  (equal (org-get-todo-state) pending-kw)
                  (org-entry-get nil "GPTEL_PENDING_ID"))
         (point)))
     ;; Search forward
     (save-excursion
       (let ((found nil))
         (while (and (not found)
                     (re-search-forward org-heading-regexp nil t))
           (beginning-of-line)
           (when (and (equal (org-get-todo-state) pending-kw)
                      (org-entry-get nil "GPTEL_PENDING_ID"))
             (setq found (point))))
         found))
     ;; Search backward.
     ;; Backward search OK: locating an existing PENDING tool-call
     ;; heading by GPTEL_PENDING_ID for the interactive
     ;; `gptel-org-agent-run-pending' fallback.  This is content
     ;; discovery, not IB insertion-point discovery.
     (save-excursion
       (let ((found nil))
         (while (and (not found)
                     (re-search-backward org-heading-regexp nil t))
           (when (and (equal (org-get-todo-state) pending-kw)
                      (org-entry-get nil "GPTEL_PENDING_ID"))
             (setq found (point))))
         found)))))

(defun gptel-org-agent-run-pending ()
  "Run the PENDING tool confirmation heading at or near point.

This is a backup command for when changing the TODO state from
PENDING to ALLOWED does not trigger tool execution via the
`org-after-todo-state-change-hook'.

Finds the nearest PENDING heading with a GPTEL_PENDING_ID property,
retrieves the stored tool calls, changes the state to ALLOWED, and
executes them."
  (interactive)
  (let ((pos (gptel-org-agent--find-pending-heading)))
    (unless pos
      (user-error "No PENDING tool confirmation heading found"))
    (save-excursion
      (goto-char pos)
      (let* ((pending-id (org-entry-get nil "GPTEL_PENDING_ID"))
             (entry (gethash pending-id
                             gptel-org-agent--pending-tool-calls))
             (heading (org-get-heading t t t t)))
        (unless entry
          (user-error "No pending tool call data for id=%s (heading: %s)"
                      pending-id heading))
        (let ((tool-calls (plist-get entry :tool-calls))
              (info (plist-get entry :info))
              (_stored-buf (plist-get entry :buffer)))
          (gptel-org--debug
           "org-agent run-pending: manually running id=%s, %d tool calls"
           pending-id (length tool-calls))
          ;; Change state to ALLOWED visually (inhibit hooks to avoid
          ;; double execution if the hook does fire)
          (let ((org-after-todo-state-change-hook nil))
            (org-todo (nth 1 gptel-org-agent-tool-confirm-keywords)))
          ;; Remove from pending table and clean up property
          (remhash pending-id gptel-org-agent--pending-tool-calls)
          (org-delete-property "GPTEL_PENDING_ID")
          ;; Execute the tool calls
          (gptel-org-agent--accept-tool-calls tool-calls info pending-id)
          (message "Tool calls executed manually for: %s" heading))))))

(defun gptel-org-agent-accept-all-pending ()
  "Accept all PENDING tool call headings in the accessible portion of the buffer.

Changes each PENDING heading to ALLOWED, triggering tool execution
via `org-after-todo-state-change-hook'.

In agent indirect buffers (narrowed to the agent subtree), this
accepts all pending tool calls for the current agent.  In a
non-narrowed buffer it accepts every pending tool call in the
buffer."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an org-mode buffer"))
  (let ((pending-kw (nth 0 gptel-org-agent-tool-confirm-keywords))
        (allowed-kw (nth 1 gptel-org-agent-tool-confirm-keywords))
        (count 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-heading-regexp nil t)
        (beginning-of-line)
        (when (and (org-at-heading-p)
                   (string= (org-get-todo-state) pending-kw))
          (org-todo allowed-kw)
          (cl-incf count))
        (forward-line 1)))
    (if (zerop count)
        (message "No PENDING tool calls found")
      (message "Accepted %d tool call%s" count (if (= count 1) "" "s")))))

;;; ---- Task handoff ---------------------------------------------------------

(declare-function gptel-abort "gptel-request")

(defun gptel-org-agent--find-user-task-at-point ()
  "Find the user-level task heading from the current context.

If in an agent indirect buffer, resolve the agent heading and navigate
up to the user task via `gptel-org-ib-find-user-task-heading'.
If in a base org buffer, walk up from point to find the nearest heading
without an agent tag.

Returns a marker to the user task heading, or nil if not found."
  (let ((base-buf (if (gptel-org-ib-registered-p (current-buffer))
                      (gptel-org-ib-base (current-buffer))
                    (current-buffer))))
    (if (gptel-org-ib-registered-p (current-buffer))
        ;; In an indirect buffer
        (let ((agent-pos (gptel-org-ib-resolve-agent-heading
                          (current-buffer))))
          (when agent-pos
            (with-current-buffer base-buf
              (save-excursion
                (goto-char agent-pos)
                (when (gptel-org-ib-find-user-task-heading)
                  (point-marker))))))
      ;; In base buffer: walk up from point to find non-agent heading
      (save-excursion
        (when (or (org-at-heading-p)
                  (ignore-errors (org-back-to-heading t)))
          (if (cl-some #'gptel-org-agent--agent-tag-p
                       (org-get-tags nil t))
              ;; On an agent heading, walk up
              (when (gptel-org-ib-find-user-task-heading)
                (point-marker))
            ;; Already on a non-agent heading
            (point-marker)))))))

(defun gptel-org-handoff ()
  "Stop the current task and create a new AI-DO heading for continuation.

This command is used to optimize token usage when a conversation has
grown large.  It:

1. Finds the user-level task heading from the current context.
2. Aborts any running gptel request in the buffer.
3. Creates a new AI-DO sibling heading after the current task subtree.

The new heading inherits the title of the current task (prefixed with
\"Continue:\") and can be picked up by a fresh agent with clean context.
The user can edit the new heading to add notes before triggering it.

Works from both base org buffers and agent indirect buffers."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an org-mode buffer"))
  (let* ((base-buf (if (gptel-org-ib-registered-p (current-buffer))
                       (gptel-org-ib-base (current-buffer))
                     (current-buffer)))
         (task-marker (gptel-org-agent--find-user-task-at-point)))
    (unless task-marker
      (user-error "No task heading found at or above point"))
    ;; Abort any running requests in the base buffer
    (when (fboundp 'gptel-abort)
      (condition-case nil
          (gptel-abort base-buf)
        (error nil)))
    ;; Create the handoff heading
    (with-current-buffer base-buf
      (save-excursion
        (goto-char task-marker)
        ;; IB-4.7: the handoff heading is inserted as a SIBLING of the
        ;; current task, so its parent is the task's parent.  Validate
        ;; that the parent chain contains no AI-DO-family heading.
        ;; `gptel-org-agent--validate-ai-do-depth' walks from point
        ;; upward past the task heading itself, which is exactly the
        ;; chain we need to check.  Additionally reject the case where
        ;; the current task is itself AI-DO-family (a handoff from
        ;; within an AI-DO subtree would nest).
        (let ((task-state (org-get-todo-state)))
          (when (member task-state gptel-org-agent--ai-do-family-keywords)
            (user-error
             "AI-DO depth violation: cannot handoff from %S heading (line %d).  AI-DO may appear only directly under a user TODO"
             task-state (line-number-at-pos))))
        (gptel-org-agent--validate-ai-do-depth)
        (let* ((task-title (org-get-heading t t t t))
               (task-level (org-current-level))
               (inhibit-read-only t)
               (stars (make-string task-level ?*))
               (todo-keyword "AI-DO")
               (new-title (format "Continue: %s" task-title))
               (heading-text (format "%s %s %s" stars todo-keyword new-title)))
          ;; Go to end of current task subtree
          (org-end-of-subtree t)
          (gptel-org-ib-ensure-bol)
          ;; Insert the new heading
          (insert heading-text "\n")
          (message "Created handoff heading: %s" new-title))))
    ;; Switch to base buffer if in indirect buffer
    (when (gptel-org-ib-registered-p (current-buffer))
      (let ((pos (marker-position task-marker)))
        (switch-to-buffer base-buf)
        (goto-char pos)
        (org-end-of-subtree t)
        (recenter)))))

(provide 'gptel-org-agent)
;;; gptel-org-agent.el ends here
