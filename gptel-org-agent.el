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

;; Forward declarations for variables defined in gptel-org.el
(defvar gptel-org-todo-keywords)
(defvar gptel-org-infer-bounds-from-tags)
(defvar gptel-org-subtree-context)
(defvar gptel-org--org-format-response)

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
(declare-function gptel--format-tool-call "gptel")
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
heading).  The new heading is inserted at the end of the parent's
subtree content, before any sibling heading.

The child heading uses DESCRIPTION (or the parent heading's text) and
includes the AI-DOING keyword (from `gptel-org-tasks-doing-keyword').

Return a marker to the newly created heading."
  (save-excursion
    (unless (org-at-heading-p)
      (error "gptel-org-agent--create-subtree: point is not on a heading"))
    ;; Tool call confirmation text in the buffer may be marked read-only
    ;; (by `gptel--display-tool-calls').  The overlay cleanup happens
    ;; after tool functions run, so we must bypass read-only here.
    (let* ((inhibit-read-only t)
           (parent-level (org-current-level))
           (child-level (1+ parent-level))
           (tag (gptel-org-agent--construct-tag agent-type parent-tag))
           (heading-title (or description
                              (org-element-property :raw-value (org-element-at-point))))
           (doing-keyword (or (bound-and-true-p gptel-org-tasks-doing-keyword) "AI-DOING"))
           (stars (make-string child-level ?*))
           (heading-text (if heading-title
                             (format "%s %s %s :%s:" stars doing-keyword heading-title tag)
                           (format "%s :%s:" stars tag)))
           marker)
      (gptel-org--debug "org-agent create-subtree: creating %S at level %d under level %d"
                        tag child-level parent-level)
      ;; Move to the end of the parent subtree's content.
      ;; `org-end-of-subtree' goes to the end of the whole subtree including
      ;; children, so we need to position before the next sibling or at end
      ;; of subtree.  For inserting a new last child, we go to end of subtree.
      (org-end-of-subtree t)
      ;; Ensure we're at the beginning of a line for clean insertion
      (unless (bolp) (insert "\n"))
      ;; Insert the new heading
      (insert heading-text "\n")
      ;; Move back to the heading we just inserted
      (forward-line -1)
      (beginning-of-line)
      ;; Use org-set-tags to properly format the tag on the heading
      ;; (aligns tag column, runs org-after-tags-change-hook, etc.)
      (org-set-tags (list tag))
      ;; Create the marker before inserting the body newline
      (setq marker (point-marker))
      ;; Move to end of heading line and insert a newline so the agent
      ;; has somewhere to write its response
      (end-of-line)
      (insert "\n")
      (gptel-org--debug "org-agent create-subtree: created heading at line %d, marker at %d"
                        (line-number-at-pos marker) (marker-position marker))
      marker)))


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

Returns the heading text of the created heading, or nil on failure."
  (let* ((base-buf (or (buffer-base-buffer (current-buffer))
                       (current-buffer)))
         (agent-heading-pos
          (gptel-org-ib-resolve-agent-heading (current-buffer))))
    (when agent-heading-pos
      (with-current-buffer base-buf
        (save-excursion
          (goto-char agent-heading-pos)
          ;; Navigate up past all agent headings to the user task
          (when (gptel-org-ib-find-user-task-heading)
            (let* ((inhibit-read-only t)
                   (parent-level (org-current-level))
                   (child-level (1+ parent-level))
                   (stars (make-string child-level ?*))
                   (todo-keyword (gptel-org-agent--status-to-keyword "pending"))
                   (heading-text (format "%s %s %s" stars todo-keyword description)))
              ;; Go to end of parent subtree to insert as last child
              (org-end-of-subtree t)
              (unless (bolp) (insert "\n"))
              ;; Insert the heading
              (insert heading-text "\n")
              ;; Insert the body content
              (when (and body (not (string-empty-p body)))
                (insert body)
                (unless (string-suffix-p "\n" body)
                  (insert "\n")))
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
  (let ((base-buf (buffer-base-buffer (current-buffer))))
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
  (let ((base-buf (buffer-base-buffer (current-buffer))))
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
                  (let ((task-level (org-current-level)))
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

Uses TAG for the agent type and a short hash derived from the
outline path (parent headings) at HEADING-POS in BASE-BUFFER.
This ensures each agent subtree gets a distinct buffer name even
when multiple TODO headings spawn agents with the same tag.

The format is *gptel:TAG-HASH* where HASH is a 6-character hex
string."
  (let* ((path-str
          (with-current-buffer base-buffer
            (save-excursion
              (goto-char heading-pos)
              (let ((path (org-get-outline-path t)))
                (mapconcat #'identity path "/")))))
         (hash (substring (md5 (concat (buffer-name base-buffer) ":" path-str))
                          0 6)))
    (format "*gptel:%s-%s*" tag hash)))

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
    ;; Apply agent-specific setup that gptel-org-ib-create doesn't do
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

(defvar-local gptel-org-agent--parent-indirect-buffer nil
  "Parent indirect buffer when this is a TodoWrite sub-task buffer.
Set by `gptel-org-agent--redirect-markers-to-heading' so the FSM can
be restored to the parent buffer when the sub-task completes.")

(defvar-local gptel-org-agent--todo-task-ibs nil
  "Alist mapping TODO task content string to its indirect buffer.
Populated by `gptel-org-agent--write-todo-org' when new task
headings are created.  Consulted by
`gptel-org-agent--redirect-markers-to-heading' to reuse per-task IBs
instead of creating new ones.  Entries are removed when the task
heading is removed by `write-todo-org' idempotent cleanup, and the
associated IB is closed when the agent IB itself is closed.")


(defun gptel-org-agent--make-insertion-marker (buffer)
  "Create an insertion-type marker at the end of BUFFER's content.
Position at end of the last non-empty line in BUFFER, create a marker
with insertion-type t so it advances as text is appended.  This is the
standard way to set up the FSM's `:position' marker in an indirect buffer."
  (with-current-buffer buffer
    (goto-char (point-max))
    (skip-chars-backward "\n")
    (end-of-line)
    (let ((m (point-marker)))
      (set-marker-insertion-type m t)
      m)))

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
      ;; Cascade-close any task IBs owned by this agent IB.  This
      ;; happens BEFORE the agent IB itself is closed so the task IB
      ;; cleanup can still resolve base buffers / registry entries.
      (let ((task-ibs (buffer-local-value
                       'gptel-org-agent--todo-task-ibs
                       indirect-buffer)))
        (when task-ibs
          (gptel-org--debug
           "org-agent close-indirect-buffer: cascading close of %d task IB(s)"
           (length task-ibs))
          (dolist (pair task-ibs)
            (let ((task-ib (cdr pair)))
              (when (and (buffer-live-p task-ib)
                         (not (eq task-ib indirect-buffer)))
                (gptel-org-ib-close task-ib nil))))
          (with-current-buffer indirect-buffer
            (setq gptel-org-agent--todo-task-ibs nil))))
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
indirect buffer (identified by `buffer-base-buffer' returning non-nil)."
  (let* ((info (gptel-fsm-info fsm))
         (orig-buffer (plist-get info :buffer))
         (preset (plist-get info :preset)))
    (when (and gptel-org-subtree-context
               (buffer-live-p orig-buffer)
               ;; Only redirect from a base org buffer, not from an
               ;; indirect buffer (which is already an agent subtree)
               (not (buffer-base-buffer orig-buffer))
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
             (and (buffer-base-buffer indirect-buf)
                  (buffer-name (buffer-base-buffer indirect-buf))))))))))


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
             (buffer-base-buffer buffer))
        (save-excursion
          (goto-char (point-min))
          (when (org-at-heading-p)
            (1+ (org-current-level)))))))))

(defun gptel-org-agent--seq-todo-line (buffer)
  "Return the #+SEQ_TODO line from BUFFER, or nil if none found.

Searches the base buffer (for indirect buffers) or BUFFER directly."
  (let ((search-buffer (or (buffer-base-buffer buffer) buffer)))
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

FSM is the request state machine."
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
                    instructions))))))))

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
                    (if (buffer-base-buffer) "yes" "no"))
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
                (base-buffer (buffer-base-buffer (current-buffer)))
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
                            ;; agent subtree (original behavior)
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
                                       state done-kw))))))))))
                    (unless has-user
                      ;; Create the user/feedback heading
                      (goto-char subtree-end)
                      (unless (bolp) (insert "\n"))
                      (let ((stars (make-string user-level ?*)))
                        (if keyword-mode
                            (let ((kw (or (bound-and-true-p gptel-org-user-keyword) "FEEDBACK")))
                              (insert (format "%s %s \n" stars kw))
                              (gptel-org--debug
                               "insert-user-heading: created %s sibling heading at level %d after pos %d"
                               kw user-level (marker-position subtree-end)))
                          (insert (format "%s \n" stars))
                          (forward-line -1)
                          (beginning-of-line)
                          (org-set-tags (list user-tag))
                          (gptel-org--debug
                           "insert-user-heading: created :%s: heading at level %d after pos %d"
                           user-tag user-level (marker-position subtree-end)))))
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
  (when (and (buffer-base-buffer (current-buffer))
             (derived-mode-p 'org-mode))
    (save-excursion
      (goto-char (point-min))
      (when (org-at-heading-p)
        (cl-find-if #'gptel-org-agent--agent-tag-p
                    (org-get-tags nil t))))))

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
           (doing-keyword (or (bound-and-true-p gptel-org-tasks-doing-keyword) "AI-DOING"))
           ;; Determine the base buffer (for indirect buffers, go to the base)
           (base-buffer (or (buffer-base-buffer (current-buffer))
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
              ;; (e.g., FEEDBACK or Results), so existing sibling indirect
              ;; buffers are not disturbed.
              ;; Sub-agents (parent-tag non-nil) use "Results" terminator;
              ;; top-level agents use FEEDBACK (or user's custom keyword).
              (let ((terminator-keyword
                     (if parent-tag
                         "Results"
                       (or (bound-and-true-p gptel-org-user-keyword)
                           "FEEDBACK"))))
                (gptel-org--debug
                 "org-agent setup-task-subtree: creating new subtree via safe-insert-sibling (terminator=%S)"
                 terminator-keyword)
                (setq indirect-buf
                      (gptel-org-ib-safe-insert-sibling
                       doing-keyword
                       (or description "Sub-task")
                       (list tag)
                       terminator-keyword))
                ;; Extract the heading marker from the registry
                (let ((entry (gptel-org-ib-get (buffer-name indirect-buf))))
                  (setq heading-marker (plist-get entry :heading-marker)))
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
            ;; Strategy 2: Fallback - get all body text after first heading.
            (unless (and result (not (string-empty-p result)))
              (goto-char (point-min))
              (when (org-at-heading-p)
                (forward-line 1))       ;skip the heading line
              (let ((body-start (point)))
                (setq result (string-trim
                              (buffer-substring-no-properties
                               body-start (point-max))))))
            (if (and result (not (string-empty-p result)))
                result
              nil)))))))


;;; ---- TodoWrite org integration (Phase 4) ----------------------------------

(defcustom gptel-org-agent-todo-keywords nil
  "TODO keyword sequence for agent task headings.
When non-nil, added to `org-todo-keywords' to ensure required
keywords are available.  When nil (the default), the buffer's
existing TODO keywords are used as-is.

The mapping from TodoWrite statuses to org keywords is controlled by
`gptel-org-agent-status-keyword-map'."
  :type '(choice (const :tag "Use buffer keywords" nil)
                 (sexp :tag "Custom keyword sequence"))
  :group 'gptel)

(defcustom gptel-org-agent-status-keyword-map
  '(("pending"     . "AI-DO")
    ("in_progress" . "AI-DOING")
    ("completed"   . "AI-DONE"))
  "Alist mapping TodoWrite status strings to org TODO keywords.
Each entry is (STATUS . KEYWORD) where STATUS is one of the strings
the LLM sends (\"pending\", \"in_progress\", \"completed\") and
KEYWORD is the org TODO keyword to use.

The keywords should match keywords defined in the buffer's
`#+SEQ_TODO' line or `org-todo-keywords'.  Defaults align with
the `gptel-org-tasks' workflow (AI-DO/AI-DOING/AI-DONE)."
  :type '(alist :key-type string :value-type string)
  :group 'gptel)

(defun gptel-org-agent--ensure-todo-keywords ()
  "Ensure agent TODO keywords are available in the current buffer.
When `gptel-org-agent-todo-keywords' is non-nil, adds its entries
to `org-todo-keywords' if not already present, then refreshes org's
TODO keyword parsing.  When nil, assumes the buffer already defines
the required keywords (e.g. via #+SEQ_TODO)."
  (when gptel-org-agent-todo-keywords
    (let ((needs-refresh nil))
      (dolist (kw-seq gptel-org-agent-todo-keywords)
        (unless (member kw-seq org-todo-keywords)
          (push kw-seq org-todo-keywords)
          (setq needs-refresh t)))
      (when needs-refresh
        ;; Make the change buffer-local so we don't pollute other buffers
        (setq-local org-todo-keywords org-todo-keywords)
        (org-set-regexps-and-options)))))

(defun gptel-org-agent--status-to-keyword (status)
  "Map a TodoWrite STATUS string to an org TODO keyword.
Uses `gptel-org-agent-status-keyword-map' for the mapping.
Falls back to the uppercased STATUS if no mapping is found."
  (or (alist-get status gptel-org-agent-status-keyword-map nil nil #'equal)
      (upcase (or status "AI-DO"))))

(defun gptel-org-agent--find-or-create-tasks-heading (level)
  "Find or create a \"Tasks\" heading at LEVEL under the current heading.
Point should be on the agent heading.  Returns the position of the
Tasks heading."
  (let ((agent-end (save-excursion (org-end-of-subtree t) (point)))
        (child-re (format "^\\*\\{%d\\} +Tasks\\b" level))
        found)
    ;; Search for existing Tasks heading among direct children
    (save-excursion
      (org-end-of-meta-data t)
      (while (and (not found)
                  (< (point) agent-end)
                  (re-search-forward org-heading-regexp agent-end t))
        (beginning-of-line)
        (when (and (= (org-current-level) level)
                   (looking-at child-re))
          (setq found (point)))
        (unless found
          (forward-line 1))))
    (or found
        ;; Create the Tasks heading at the end of the agent subtree
        (save-excursion
          (org-end-of-subtree t)
          (unless (bolp) (insert "\n"))
          (let ((stars (make-string level ?*))
                (pos (point)))
            (insert stars " Tasks\n")
            pos)))))

(defun gptel-org-agent--collect-todo-headings (level parent-pos)
  "Collect existing todo headings at LEVEL under PARENT-POS.
Only headings with a TODO keyword from `gptel-org-agent-status-keyword-map'
are collected.  Returns an alist of (CONTENT . MARKER) where CONTENT
is the heading text stripped of the TODO keyword and MARKER is a marker
at the beginning of the heading line.  Markers are used instead of
positions to survive subsequent buffer modifications (e.g. keyword
changes that shift text)."
  (let ((todo-keywords (mapcar #'cdr gptel-org-agent-status-keyword-map))
        result)
    (save-excursion
      (goto-char parent-pos)
      (let ((parent-end (save-excursion (org-end-of-subtree t) (point))))
        (forward-line 1)
        (while (and (< (point) parent-end)
                    (re-search-forward org-heading-regexp parent-end t))
          (beginning-of-line)
          (when (= (org-current-level) level)
            (let* ((components (org-heading-components))
                   ;; org-heading-components returns:
                   ;; (level reduced-level todo priority heading tags)
                   (todo-kw (nth 2 components))
                   (heading-text (nth 4 components)))
              (when (and heading-text
                         (member todo-kw todo-keywords))
                (push (cons (org-trim heading-text)
                            (copy-marker (point)))
                      result))))
          (forward-line 1))))
    (nreverse result)))

(defun gptel-org-agent--set-todo-keyword (keyword)
  "Set the TODO keyword of the heading at point to KEYWORD.
Uses `org-todo' for proper state tracking, but falls back to direct
text replacement if the keyword is not in org's known set."
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
        ;; org-todo with a specific keyword argument sets it directly
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

(defun gptel-org-agent--create-todo-heading (content keyword parent-pos terminator-keyword)
  "Create a TODO heading and its indirect buffer for a TodoWrite task.

CONTENT is the task description text.
KEYWORD is the TODO keyword (e.g., \"AI-DO\", \"AI-DOING\").
PARENT-POS is the position of the parent agent heading (in the
current buffer, which is assumed to be the agent's indirect buffer
or narrowed region containing the agent subtree).
TERMINATOR-KEYWORD is the terminator under the parent subtree that
new task headings must be inserted BEFORE (e.g., \"FEEDBACK\" for
top-level agents, \"Results\" for sub-agents).

Uses `gptel-org-ib-safe-insert-sibling' to:
  1. Ensure the parent's TERMINATOR-KEYWORD exists.
  2. Insert the new task heading BEFORE the terminator (so existing
     sibling IBs narrowed to earlier tasks are not disturbed).
  3. Create an indirect buffer narrowed to the new task's subtree.

Inside the new task IB, ensure a \"Results\" child terminator so
that subsequent heading creation inside the task (tool confirmations,
reasoning headings, sub-task delegations) also respects the rule.

Returns the newly created task indirect buffer."
  (let (task-ib)
    (save-excursion
      (goto-char parent-pos)
      (unless (org-at-heading-p)
        (ignore-errors (org-back-to-heading t)))
      (setq task-ib
            (gptel-org-ib-safe-insert-sibling
             keyword content nil terminator-keyword)))
    ;; Inside the task IB, ensure a Results terminator so subsequent
    ;; child headings (TOOL / REASONING / sub-agents) are inserted
    ;; before it.
    (when (buffer-live-p task-ib)
      (with-current-buffer task-ib
        (save-excursion
          (goto-char (point-min))
          (when (org-at-heading-p)
            (gptel-org-ib-ensure-terminator "Results")))))
    task-ib))

(defun gptel-org-agent--remove-todo-heading (pos)
  "Remove the heading at POS and its entire subtree."
  (save-excursion
    (goto-char pos)
    (when (org-at-heading-p)
      (let ((beg (point))
            (end (save-excursion (org-end-of-subtree t)
                                (if (and (bolp) (not (eobp)))
                                    (point)
                                  (min (1+ (point)) (point-max))))))
        (delete-region beg end)))))


(defun gptel-org-agent--agent-terminator-keyword (&optional agent-buffer)
  "Return the terminator keyword used under the agent in AGENT-BUFFER.

Returns \"Results\" for sub-agent buffers (tag contains two
@-separated components, e.g. \"researcher@triage@agent\"), and
\"FEEDBACK\" (or `gptel-org-user-keyword') for top-level agents.

AGENT-BUFFER defaults to the current buffer."
  (let* ((buf (or agent-buffer (current-buffer)))
         (entry (gptel-org-ib-get (buffer-name buf)))
         (tag (and entry (plist-get entry :tag))))
    (if (and (stringp tag)
             ;; Sub-agent tags look like "foo@bar@agent" — at least two @s.
             (>= (cl-count ?@ tag) 2))
        "Results"
      (or (bound-and-true-p gptel-org-user-keyword)
          "FEEDBACK"))))

(defun gptel-org-agent--find-task-ib-for-heading (heading-pos)
  "Return the task IB from `gptel-org-agent--todo-task-ibs' for HEADING-POS.

Looks up the alist entry whose IB's registered heading-marker points
to HEADING-POS (in the parent/base buffer).  Returns the IB on match,
or nil if no match is found.

Called from the PARENT agent indirect buffer."
  (let ((ibs gptel-org-agent--todo-task-ibs)
        (match nil))
    (while (and ibs (not match))
      (let* ((pair (car ibs))
             (ib (cdr pair))
             (entry (and (buffer-live-p ib)
                         (gptel-org-ib-get (buffer-name ib))))
             (hm (and entry (plist-get entry :heading-marker))))
        (when (and (markerp hm)
                   (= (marker-position hm) heading-pos))
          (setq match ib)))
      (setq ibs (cdr ibs)))
    match))

(defun gptel-org-agent--redirect-markers-to-heading (heading-pos)
  "Redirect FSM streaming to the sub-task heading at HEADING-POS.

Preferred path: if `gptel-org-agent--todo-task-ibs' contains a
pre-created IB for the task whose heading is at HEADING-POS,
REUSE that IB (created by `gptel-org-agent--write-todo-org').

Fallback: if no pre-existing IB is found, create one via
`gptel-org-agent--open-indirect-buffer'.  This handles sub-task
heading cases not originating from TodoWrite.

Updates the FSM's `:buffer' and `:position' so streaming inserts
into the task buffer.  Streaming uses `gptel-org-ib-streaming-marker'
with the \"Results\" terminator so content lands BEFORE Results
inside the task IB.  The parent indirect buffer is saved as
`gptel-org-agent--parent-indirect-buffer' for later restoration.

Accesses the FSM via `gptel--fsm-last' which is buffer-local."
  (when (and (boundp 'gptel--fsm-last) gptel--fsm-last)
    (let* ((info (gptel-fsm-info gptel--fsm-last))
           (base-buffer (or (buffer-base-buffer (current-buffer))
                            (current-buffer)))
           (parent-buffer (current-buffer))
           (reused (gptel-org-agent--find-task-ib-for-heading heading-pos))
           (sub-indirect-buf
            (or reused
                (let ((heading-marker (save-excursion
                                        (goto-char heading-pos)
                                        (point-marker))))
                  (gptel-org-agent--open-indirect-buffer
                   base-buffer heading-marker)))))
      (gptel-org--debug
       "redirect-markers: heading-pos=%S parent=%S (ref-level=%S) base=%S reused=%s"
       heading-pos (buffer-name parent-buffer)
       (buffer-local-value 'gptel-org--ref-level parent-buffer)
       (buffer-name base-buffer) (and reused t))
      ;; Create terminator-aware position marker inside the task IB.
      ;; Try "Results" first (task IBs should have one created by
      ;; `create-todo-heading'), then fall back to end-of-buffer.
      (let ((pos-marker
             (with-current-buffer sub-indirect-buf
               (save-excursion
                 (goto-char (point-min))
                 (gptel-org-ib-streaming-marker "Results")))))
        ;; Save parent buffer reference for restoration
        (with-current-buffer sub-indirect-buf
          (setq-local gptel-org-agent--parent-indirect-buffer parent-buffer))
        ;; Redirect FSM to the sub-task buffer
        (plist-put info :buffer sub-indirect-buf)
        (plist-put info :position pos-marker)
        ;; Reset tracking-marker so next streaming turn starts fresh
        (plist-put info :tracking-marker nil)
        (gptel-org--debug
         "redirect-markers: task buffer %S pos=%S ref-level=%S"
         (buffer-name sub-indirect-buf)
         (marker-position pos-marker)
         (buffer-local-value 'gptel-org--ref-level sub-indirect-buf))))))

(defun gptel-org-agent--restore-from-subtask-buffer ()
  "Restore FSM from a sub-task indirect buffer back to the parent buffer.

If the current FSM buffer is a sub-task buffer (has
`gptel-org-agent--parent-indirect-buffer' set), switches the FSM's
`:buffer' and `:position' back to the parent.

Does NOT kill the sub-task buffer: task IBs are now long-lived and
tracked by `gptel-org-agent--todo-task-ibs' on the parent.  The
task IB is killed either when the task is removed by
`write-todo-org' or when the parent agent IB is closed (see
`gptel-org-agent--close-indirect-buffer' which cascades).

Returns the parent buffer if a restore was performed, nil otherwise."
  (when (and (boundp 'gptel--fsm-last) gptel--fsm-last)
    (let* ((info (gptel-fsm-info gptel--fsm-last))
           (current-buf (plist-get info :buffer)))
      (when (and current-buf (buffer-live-p current-buf))
        (let ((parent-buf (buffer-local-value
                           'gptel-org-agent--parent-indirect-buffer
                           current-buf)))
          (when (and parent-buf (buffer-live-p parent-buf))
            (gptel-org--debug
             "restore-from-subtask: %S (ref-level=%S) -> %S (ref-level=%S)"
             (buffer-name current-buf)
             (buffer-local-value 'gptel-org--ref-level current-buf)
             (buffer-name parent-buf)
             (buffer-local-value 'gptel-org--ref-level parent-buf))
            ;; Create new position marker in parent buffer at end of content
            (let ((pos-marker
                   (gptel-org-agent--make-insertion-marker parent-buf)))
              ;; Restore FSM to parent buffer
              (plist-put info :buffer parent-buf)
              (plist-put info :position pos-marker)
              (plist-put info :tracking-marker nil)
              ;; Task IB remains alive — it's owned by parent's
              ;; `gptel-org-agent--todo-task-ibs' and cleaned up there.
              parent-buf)))))))

(defun gptel-org-agent--write-todo-org (todos)
  "Display TODOS as org TODO headings in the agent subtree.

Each todo becomes a heading at the appropriate level with a TODO keyword
mapped via `gptel-org-agent-status-keyword-map' (default: AI-DO/AI-DOING/AI-DONE).

TODOS is a list of plists with :content, :activeForm, and :status.
This function is idempotent: calling it multiple times with the same
data produces the same result.  It handles adding new tasks, updating
existing task states, and removing tasks that are no longer in the list.

Todo headings are created directly under the agent heading (no
intermediate \"Tasks\" container).  When a todo transitions to
in_progress, the FSM's position and tracking markers are moved to the
end of that heading's subtree so subsequent AI responses stream under
the active task heading."
  (gptel-org-agent--ensure-todo-keywords)
  (when (vectorp todos) (setq todos (append todos nil)))
  ;; If the FSM was previously redirected into a sub-task buffer,
  ;; restore back to the parent agent buffer first.  write-todo-org
  ;; needs the full agent subtree visible (point-min = agent heading)
  ;; to create/update todo headings.  The restore returns the parent
  ;; buffer so we can switch context.
  (let ((parent-buf (gptel-org-agent--restore-from-subtask-buffer)))
    (when parent-buf
      (gptel-org--debug "write-todo-org: switched to parent buffer %S"
                        (buffer-name parent-buf))))
  ;; Determine the correct buffer: use FSM :buffer (which is the
  ;; parent agent buffer after restore, or the current agent buffer
  ;; if no restore was needed).
  (let ((agent-buf (if (and (boundp 'gptel--fsm-last) gptel--fsm-last)
                       (or (plist-get (gptel-fsm-info gptel--fsm-last) :buffer)
                           (current-buffer))
                     (current-buffer))))
    (with-current-buffer agent-buf
      (gptel-org--debug "write-todo-org: called with %d todos in buffer %S (base: %S)"
                        (length todos) (buffer-name) (buffer-name (buffer-base-buffer)))
      (gptel-org--debug "write-todo-org: narrowed=%S point-min=%d point-max=%d"
                        (buffer-narrowed-p) (point-min) (point-max))
      (save-excursion
        (goto-char (point-min))             ;agent heading in narrowed buffer
    (gptel-org--debug "write-todo-org: at point-min, org-at-heading-p=%S line=%S"
                      (org-at-heading-p)
                      (buffer-substring-no-properties
                       (point) (min (+ (point) 80) (point-max))))
    (unless (org-at-heading-p)
      (condition-case nil (org-back-to-heading t) (error nil)))
    (if (not (org-at-heading-p))
        (gptel-org--debug "write-todo-org: BAILING - no heading context at point %d" (point))
      (let* ((agent-level (org-current-level))
             (todo-level  (1+ agent-level))
             (agent-pos   (point)))
        (gptel-org--debug "write-todo-org: agent-level=%d todo-level=%d agent-pos=%d"
                          agent-level todo-level agent-pos)
        ;; Determine terminator keyword for the agent subtree.  Top-level
        ;; agents use FEEDBACK; sub-agents use Results.  This is what
        ;; new task headings must be inserted BEFORE.
        (let* ((terminator-keyword
                (gptel-org-agent--agent-terminator-keyword agent-buf))
               ;; Collect existing todo headings directly under the agent heading
               (existing-headings
                (gptel-org-agent--collect-todo-headings todo-level agent-pos))
               (seen-contents (make-hash-table :test 'equal))
               in-progress-pos)
          (gptel-org--debug
           "write-todo-org: %d existing headings found (terminator=%S)"
           (length existing-headings) terminator-keyword)
          ;; Update or create todo headings
          (dolist (todo todos)
            (let* ((content (plist-get todo :content))
                   (status  (plist-get todo :status))
                   (keyword (gptel-org-agent--status-to-keyword status))
                   (existing (assoc content existing-headings)))
              (puthash content t seen-contents)
              (if existing
                  ;; Update existing heading's TODO keyword
                  (progn
                    (gptel-org--debug "write-todo-org: updating %S -> %S at %S"
                                      content keyword (cdr existing))
                    (save-excursion
                      (goto-char (cdr existing))
                      (gptel-org-agent--set-todo-keyword keyword))
                    (when (equal status "in_progress")
                      (setq in-progress-pos (cdr existing))))
                ;; Create new heading under agent as its own task IB.
                ;; create-todo-heading returns the new task IB; track it
                ;; in `gptel-org-agent--todo-task-ibs' so redirect and
                ;; cleanup can find it by task content.
                (gptel-org--debug
                 "write-todo-org: creating %S %S at agent-pos=%d (terminator=%S)"
                 keyword content agent-pos terminator-keyword)
                (let ((task-ib
                       (gptel-org-agent--create-todo-heading
                        content keyword agent-pos terminator-keyword)))
                  (when (buffer-live-p task-ib)
                    (setq gptel-org-agent--todo-task-ibs
                          (cons (cons content task-ib)
                                (assoc-delete-all
                                 content gptel-org-agent--todo-task-ibs))))
                  (when (equal status "in_progress")
                    ;; Find the just-created heading position in the
                    ;; agent buffer (for redirect's heading-pos lookup).
                    (setq in-progress-pos
                          (save-excursion
                            (goto-char agent-pos)
                            (let ((end (save-excursion
                                         (org-end-of-subtree t) (point))))
                              (catch 'found
                                (while (re-search-forward
                                        org-heading-regexp end t)
                                  (beginning-of-line)
                                  (when (and (= (org-current-level) todo-level)
                                             (let ((h (nth 4 (org-heading-components))))
                                               (equal (and h (org-trim h)) content)))
                                    (throw 'found (point)))
                                  (forward-line 1)))))))))))
          ;; Remove headings not in the current todo list.
          ;; Process in reverse order to avoid position shifts.
          (dolist (existing (reverse existing-headings))
            (unless (gethash (car existing) seen-contents)
              (gptel-org--debug "write-todo-org: removing %S" (car existing))
              ;; Close the task's indirect buffer first (if any).
              (let ((pair (assoc (car existing)
                                 gptel-org-agent--todo-task-ibs)))
                (when (and pair (buffer-live-p (cdr pair)))
                  (gptel-org--debug
                   "write-todo-org: closing task IB %S for removed task"
                   (buffer-name (cdr pair)))
                  (gptel-org-agent--close-indirect-buffer (cdr pair)))
                (when pair
                  (setq gptel-org-agent--todo-task-ibs
                        (assoc-delete-all
                         (car existing)
                         gptel-org-agent--todo-task-ibs))))
              (gptel-org-agent--remove-todo-heading (cdr existing))))
          ;; Redirect FSM into a sub-task indirect buffer so subsequent
          ;; AI text streams under the in_progress heading with its own
          ;; auto-corrector at the correct ref-level.
          (when in-progress-pos
            (gptel-org-agent--redirect-markers-to-heading in-progress-pos)))))))
  (gptel-org--debug "write-todo-org: done, buffer size now %d" (buffer-size))
  t))


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
                     (buffer-base-buffer buf)))))))

(defun gptel-org-agent--ensure-tool-confirm-hook (buf)
  "Ensure the tool confirmation hook is registered on BUF and its base buffer.

The PENDING heading is created in an indirect buffer, but users
typically change the TODO state from the base buffer.  This
ensures the `org-after-todo-state-change-hook' handler is
registered on both buffers so the state change is always caught."
  (dolist (b (delq nil (list buf (buffer-base-buffer buf))))
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
  "Display TOOL-CALLS as separate PENDING org headings in the agent subtree.

Creates one child heading per tool call under the current position,
each with a PENDING TODO state and its own unique ID.  Stores each
tool call's data separately in `gptel-org-agent--pending-tool-calls'.

TOOL-CALLS is a list of (tool-spec arg-plist process-tool-result).
INFO is the FSM info plist."
  (let* ((buf (plist-get info :buffer))
         (start-marker (plist-get info :position))
         (pending-kw (nth 0 gptel-org-agent-tool-confirm-keywords)))
    (with-current-buffer buf
      ;; Ensure the todo-state-change hook is registered on both the
      ;; indirect buffer and its base buffer so that the user can
      ;; change PENDING→ALLOWED from either buffer.
      (gptel-org-agent--ensure-tool-confirm-hook buf)
      (save-excursion
        ;; Compute the PENDING heading level.  Use `gptel-org--ref-level'
        ;; when available — it tracks the current response level and is
        ;; updated by `gptel-org-agent--redirect-markers-to-heading'
        ;; when TodoWrite redirects streaming into a sub-task.  This
        ;; ensures PENDING headings are created at the correct depth
        ;; (e.g. agent-level+2 inside a sub-task, not agent-level+1).
        ;;
        ;; Falls back to `gptel-org--compute-response-level' (which
        ;; always returns agent-level+1 from point-min) when ref-level
        ;; is not set, and finally to point-min (the agent heading in IB).
        (let* ((pending-level (or (bound-and-true-p gptel-org--ref-level)
                                  (gptel-org--compute-response-level)
                                  ;; Fallback: use point-min (agent heading in IB)
                                  (save-excursion
                                    (goto-char (point-min))
                                    (if (org-at-heading-p)
                                        (1+ (org-current-level))
                                      1))))
               (stars (make-string pending-level ?*))
               (inhibit-read-only t)
               ;; Suppress the auto-corrector during insertion.
               ;; If the AI response contains an unclosed example/src
               ;; block, `gptel-org--in-example-block-p' returns t for
               ;; all subsequent positions.  Without this guard the
               ;; auto-corrector would comma-escape the PENDING heading
               ;; and its body text (they start with * or #+).
               (gptel-org--auto-correcting t))
          (gptel-org--debug
           "display-tool-calls: ref-level=%S pending-level=%d stars=%S buffer=%S narrowed=%s point-min-heading=%S num-tools=%d"
           gptel-org--ref-level pending-level stars (buffer-name)
           (if (buffer-narrowed-p) "yes" "no")
           (save-excursion
             (goto-char (point-min))
             (when (org-at-heading-p)
               (buffer-substring-no-properties
                (point) (line-end-position))))
           (length tool-calls))
          ;; Position at the end of the buffer content for insertion.
          ;; Use start-marker (which has drifted to end of content) as
          ;; the insertion point, since that's where new content should
          ;; go — after the AI response text.
          (goto-char (or (plist-get info :tracking-marker) start-marker))
          (unless (bolp) (insert "\n"))
          ;; Create one PENDING heading per tool call
          (dolist (tc tool-calls)
            (let* ((tool-spec (car tc))
                   (arg-plist (cadr tc))
                   (arg-values (gptel--map-tool-args tool-spec arg-plist))
                   (tool-name (gptel-tool-name tool-spec))
                   (pending-id (gptel-org-agent--generate-pending-id))
                   (formatted-call (gptel--format-tool-call tool-name arg-values))
                   ;; Build heading title from the formatted call,
                   ;; e.g. "PENDING (Eval \"(* 7 10)\")"
                   (call-title (string-trim-right formatted-call)))
              (let ((heading-pos (point)))
                ;; Insert the PENDING heading
                (insert (format "%s %s %s\n" stars pending-kw call-title))
                ;; Insert tool call details as body
                (insert (format "(:name %S :args %S)\n\n"
                                tool-name arg-plist))
                ;; Verify the heading was inserted correctly
                (save-excursion
                  (goto-char heading-pos)
                  (gptel-org--debug
                   "display-tool-calls: inserted heading at pos=%d: %S"
                   heading-pos
                   (buffer-substring-no-properties
                    (point) (line-end-position))))
                ;; Store the pending ID as an org property on the heading
                (save-excursion
                  (goto-char heading-pos)
                  (org-set-property "GPTEL_PENDING_ID" pending-id)
                  ;; Verify level after property insertion (org-set-property
                  ;; may modify heading structure)
                  (goto-char heading-pos)
                  (when (org-at-heading-p)
                    (let ((level-after-prop (org-current-level))
                          (heading-after-prop
                           (buffer-substring-no-properties
                            (line-beginning-position) (line-end-position))))
                      (gptel-org--debug
                       "display-tool-calls: after org-set-property: level=%d heading=%S"
                       level-after-prop heading-after-prop)
                      (when (/= pending-level level-after-prop)
                        (gptel-org--debug
                         "display-tool-calls: WARNING org-set-property changed level! %d -> %d"
                         pending-level level-after-prop)))))
                ;; Store SINGLE tool call in hash table (list of one,
                ;; preserving the expected list-of-lists structure for
                ;; gptel-org-agent--accept-tool-calls / --deny-tool-calls)
                (puthash pending-id
                         (list :tool-calls (list tc)
                               :info info
                               :buffer buf)
                         gptel-org-agent--pending-tool-calls)
                ;; Fold the PENDING heading so it doesn't clutter the
                ;; buffer during task execution
                (save-excursion
                  (goto-char heading-pos)
                  (when (org-at-heading-p)
                    (ignore-errors (org-fold-subtree t))))
                (gptel-org--debug
                 "org-agent tool-confirm: created PENDING heading for %s (id=%s)"
                 tool-name pending-id))))
          ;; Mark tool-pending so the FSM knows we're waiting
          (plist-put info :tool-pending t))))))

(defun gptel-org-agent--accept-tool-calls (tool-calls info)
  "Accept and run TOOL-CALLS with explicit INFO.

Restores the correct dynamic bindings from INFO before running each
tool.  Executes in the buffer stored in INFO's :buffer slot so that
buffer-local state (including `gptel--fsm-last') is available to
tool functions like `gptel-agent--task'."
  (let ((gptel--preset (or (plist-get info :preset)
                           (and (boundp 'gptel--preset) gptel--preset)))
        (gptel-backend (or (plist-get info :backend)
                           (and (boundp 'gptel-backend) gptel-backend)))
        (gptel-model (or (plist-get info :model)
                         (and (boundp 'gptel-model) gptel-model)))
        (buf (plist-get info :buffer)))
    (when (eq gptel-log-level 'debug)
      (gptel--log
       (format "accept-tool-calls ENTRY: buffer=%s base-buffer=%s info-buffer=%s buf-live=%s num-tools=%d"
               (buffer-name) (and (buffer-base-buffer) (buffer-name (buffer-base-buffer)))
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
                (cl-loop for (ts ap ptr) in tool-calls
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
              ;; Remove from pending table
              (remhash pending-id gptel-org-agent--pending-tool-calls)
              ;; Remove the property since it's no longer needed
              (org-delete-property "GPTEL_PENDING_ID")
              (let ((level-after-cleanup (org-current-level))
                    (heading-after-cleanup
                     (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position))))
                (gptel-org--debug
                 "org-agent tool-confirm: after property removal: level=%d (was %d) heading=%S"
                 level-after-cleanup level-at-entry heading-after-cleanup)
                (when (/= level-at-entry level-after-cleanup)
                  (gptel-org--debug
                   "org-agent tool-confirm: WARNING level changed after property removal! %d -> %d"
                   level-at-entry level-after-cleanup)))
              (cond
               ((equal new-state allowed-kw)
                (gptel-org--debug
                 "org-agent tool-confirm: ALLOWED (id=%s), running %d tool calls, level=%d"
                 pending-id (length tool-calls) (org-current-level))
                (when (eq gptel-log-level 'debug)
                  (gptel--log
                   (format "debug-state-change: ALLOWED in buffer=%s base-buffer=%s stored-buf=%s stored-buf-live=%s info-buffer=%s info-buffer-live=%s fsm-last=%s"
                           (buffer-name)
                           (and (buffer-base-buffer) (buffer-name (buffer-base-buffer)))
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
                (gptel-org-agent--accept-tool-calls tool-calls info)
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


(defun gptel-org-agent--update-tool-heading (tool-spec args result id)
  "Update a PENDING/ALLOWED heading to TOOL state with RESULT.

Searches backward from point for a heading matching TOOL-SPEC's name
in PENDING or ALLOWED state.  Changes the state to TOOL, updates the
heading title with a truncated call display, and inserts the tool call
details and RESULT as body text with appropriate text properties.

ID is the tool-use identifier from the LLM response, used for the
`gptel' text property on the result body.

Returns non-nil if a heading was found and updated, nil otherwise."
  (let* ((tool-name (gptel-tool-name tool-spec))
         (pending-kw (nth 0 gptel-org-agent-tool-confirm-keywords))
         (allowed-kw (nth 1 gptel-org-agent-tool-confirm-keywords))
         (found nil))
    (save-excursion
      ;; Search backward for a PENDING or ALLOWED heading whose title
      ;; contains this tool name in parentheses — the format created by
      ;; gptel-org-agent--display-tool-calls is "PENDING (ToolName ...)"
      (when (re-search-backward
             (format "^\\(\\*+\\) \\(?:%s\\|%s\\) .*(\\(?:%s\\)[ \t\"]"
                     (regexp-quote pending-kw)
                     (regexp-quote allowed-kw)
                     (regexp-quote tool-name))
             nil t)
        (let* ((heading-pos (line-beginning-position))
               (stars (match-string 1))
               (level (length stars))
               (inhibit-read-only t)
               (inhibit-modification-hooks t))
          ;; Calculate end of this heading's subtree (up to next heading
          ;; at same or higher level, or end of buffer)
          (let ((subtree-end
                 (save-excursion
                   (forward-line 1)
                   (if (re-search-forward
                        (format "^\\*\\{1,%d\\} " level) nil t)
                       (match-beginning 0)
                     (point-max)))))
            ;; Delete existing body content (the old plist details and
            ;; any PROPERTIES drawer from the PENDING heading)
            (save-excursion
              (goto-char heading-pos)
              (forward-line 1)
              (delete-region (point) subtree-end))
            ;; Replace the heading line: change keyword to TOOL and
            ;; update the title to match the standard TOOL heading format
            (goto-char heading-pos)
            (let* ((display-call (format "(%s %s)" tool-name
                                         (string-trim
                                          (prin1-to-string args) "(" ")")))
                   (truncated-call
                    (string-replace
                     "\n" " "
                     (truncate-string-to-width
                      display-call
                      (max 60 (floor (* (window-width) 0.6)))
                      0 nil " ...)")))
                   (call (prin1-to-string `(:name ,tool-name :args ,args)))
                   (result-str (if (stringp result) result
                                 (format "%S" result)))
                   ;; Escape result to protect org formatting inside
                   ;; the #+begin_tool special block
                   (escaped-result (org-escape-code-in-string result-str))
                   ;; Build the new heading line with TOOL keyword
                   (new-heading (concat stars " TOOL " truncated-call "\n"))
                   ;; Build the body with plist and result wrapped in
                   ;; special block
                   (body-text (concat call "\n"
                                      "#+begin_tool\n"
                                      escaped-result "\n"
                                      "#+end_tool\n")))
              ;; Delete the old heading line
              (let ((line-end (min (1+ (line-end-position)) (point-max))))
                (delete-region heading-pos line-end))
              ;; Insert new heading line with proper text properties
              ;; (gptel 'ignore so the heading line itself is skipped by parser)
              (goto-char heading-pos)
              (let ((prop-heading (propertize new-heading
                                             'gptel 'ignore
                                             'front-sticky '(gptel)))
                    (prop-body (propertize body-text
                                          'gptel `(tool . ,id))))
                (insert prop-heading prop-body)))
            ;; Fold the TOOL heading
            (goto-char heading-pos)
            (ignore-errors
              (when (org-at-heading-p)
                (org-fold-subtree t)))
            (setq found t)))
        ;; The edits above ran with inhibit-modification-hooks t, so the
        ;; org-element cache did not see the deletions and insertions.
        ;; Reset it now to prevent stale cache entries from causing
        ;; "Invalid search bound (wrong side of point)" errors when
        ;; subsequent inserts (e.g. streaming tool results via orig-fn
        ;; fallback) trigger org-element--cache-after-change.
        (when found
          (org-element-cache-reset))))
    found))

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
      (let ((base-buffer (or (buffer-base-buffer (current-buffer))
                             (current-buffer)))
            result)
        (while (and (not result) (org-at-heading-p))
          (let ((tag (cl-find-if #'gptel-org-agent--agent-tag-p
                                 (org-get-tags nil t))))
            (when tag
              (let* ((buf-name (gptel-org-agent--indirect-buffer-name
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
  (when-let* ((base (buffer-base-buffer (current-buffer)))
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
                  (let* ((buf-name (gptel-org-agent--indirect-buffer-name
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
   ((buffer-base-buffer (current-buffer))
    (let ((parent (gptel-org-agent--find-parent-indirect-buffer))
          (base (buffer-base-buffer (current-buffer)))
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
     ;; Search backward
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
              (stored-buf (plist-get entry :buffer)))
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
          (gptel-org-agent--accept-tool-calls tool-calls info)
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
  (let ((base-buf (or (buffer-base-buffer (current-buffer))
                      (current-buffer))))
    (if (buffer-base-buffer (current-buffer))
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
  (let* ((base-buf (or (buffer-base-buffer (current-buffer))
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
        (let* ((task-title (org-get-heading t t t t))
               (task-level (org-current-level))
               (inhibit-read-only t)
               (stars (make-string task-level ?*))
               (todo-keyword (gptel-org-agent--status-to-keyword "pending"))
               (new-title (format "Continue: %s" task-title))
               (heading-text (format "%s %s %s" stars todo-keyword new-title)))
          ;; Go to end of current task subtree
          (org-end-of-subtree t)
          (unless (bolp) (insert "\n"))
          ;; Insert the new heading
          (insert heading-text "\n")
          (message "Created handoff heading: %s" new-title))))
    ;; Switch to base buffer if in indirect buffer
    (when (buffer-base-buffer (current-buffer))
      (let ((pos (marker-position task-marker)))
        (switch-to-buffer base-buf)
        (goto-char pos)
        (org-end-of-subtree t)
        (recenter)))))

(provide 'gptel-org-agent)
;;; gptel-org-agent.el ends here
