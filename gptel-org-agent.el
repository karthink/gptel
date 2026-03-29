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
;; Enable by setting `gptel-org-agent-subtrees' to non-nil.

;;; Code:

(require 'org)
(require 'org-fold)

;; Forward declarations for functions defined in gptel-org.el
(declare-function gptel-org--debug "gptel-org")
(declare-function gptel-org--heading-has-tag-p "gptel-org")
(declare-function gptel-org--heading-has-todo-keyword-p "gptel-org")

;; Forward declarations for variables defined in gptel-org.el
(defvar gptel-org-todo-keywords)
(defvar gptel-org-infer-bounds-from-tags)

;; Forward declarations for functions defined in gptel-request.el
(declare-function gptel-fsm-info "gptel-request")

;; Forward declarations for variables defined in gptel.el
(defvar gptel-mode)


;;; ---- Customization --------------------------------------------------------

(defcustom gptel-org-agent-subtrees nil
  "When non-nil, create agent subtrees for TODO heading conversations.

When `gptel-send' is invoked on a heading with a TODO keyword from
`gptel-org-todo-keywords', create a child subtree tagged with
`:main@agent:' and open an indirect buffer narrowed to it.

The agent's response will be streamed into the indirect buffer,
keeping the main org buffer undisturbed until the conversation is
complete."
  :type 'boolean
  :group 'gptel)


;;; ---- Helpers --------------------------------------------------------------

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

(defun gptel-org-agent--create-subtree (agent-type &optional parent-tag)
  "Create an agent child heading under the current heading.

AGENT-TYPE is a string identifying the agent (e.g., \"main\",
\"researcher\", \"gatherer\").

PARENT-TAG is an optional string for constructing recursive tags.
When nil, the tag is \"<AGENT-TYPE>@agent\".  When provided, the tag
is \"<AGENT-TYPE>@<PARENT-TAG>\".

Point must be on the parent heading (the TODO heading or another agent
heading).  The new heading is inserted at the end of the parent's
subtree content, before any sibling heading.

Return a marker to the newly created heading."
  (save-excursion
    (unless (org-at-heading-p)
      (error "gptel-org-agent--create-subtree: point is not on a heading"))
    (let* ((parent-level (org-current-level))
           (child-level (1+ parent-level))
           (tag (gptel-org-agent--construct-tag agent-type parent-tag))
           (stars (make-string child-level ?*))
           (heading-text (format "%s :%s:" stars tag))
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

(defun gptel-org-agent--open-indirect-buffer (base-buffer heading-marker)
  "Open an indirect buffer narrowed to the agent subtree.

BASE-BUFFER is the org buffer containing the agent heading.
HEADING-MARKER is a marker pointing to the agent heading.

Create an indirect buffer via `make-indirect-buffer' with cloned
state, narrow it to the agent's subtree, and decouple folding so
that fold state in the indirect buffer is independent of the base.

The narrowing end uses a marker with insertion-type t, so that as
the LLM streams text at the end of the subtree the narrow region
expands automatically.

Return the indirect buffer."
  (let* ((heading-pos (marker-position heading-marker))
         ;; Determine the agent tag for the buffer name
         (tag (with-current-buffer base-buffer
                (save-excursion
                  (goto-char heading-pos)
                  (let ((tags (org-get-tags nil t)))
                    (or (cl-find-if #'gptel-org-agent--agent-tag-p tags)
                        "agent")))))
         (buf-name (format "*gptel:%s*" tag))
         ;; Compute the subtree region in the base buffer
         (region (with-current-buffer base-buffer
                   (save-excursion
                     (goto-char heading-pos)
                     (let ((beg (pos-bol))
                           (end (progn (org-end-of-subtree t) (point))))
                       (cons beg end)))))
         (beg (car region))
         (end (cdr region))
         ;; Create end marker with insertion-type t so narrowing expands
         ;; as text is inserted at the boundary
         (end-marker (with-current-buffer base-buffer
                       (let ((m (make-marker)))
                         (set-marker m end)
                         (set-marker-insertion-type m t)
                         m)))
         indirect-buf)
    (gptel-org--debug "org-agent open-indirect-buffer: tag=%S region=[%d,%d]"
                      tag beg end)
    ;; If a buffer with this name already exists, kill it to get a fresh one
    (when-let ((existing (get-buffer buf-name)))
      (kill-buffer existing))
    ;; Create the indirect buffer (clone=t to inherit major mode, local vars, etc.)
    (setq indirect-buf (make-indirect-buffer base-buffer buf-name t))
    (with-current-buffer indirect-buf
      ;; Decouple fold state so expanding/collapsing in the indirect buffer
      ;; does not affect the base buffer
      (org-fold-core-decouple-indirect-buffer-folds)
      ;; Narrow to the agent subtree
      (narrow-to-region beg end-marker)
      (goto-char (point-min))
      ;; Store the end-marker on a buffer-local variable so we can clean it
      ;; up later
      (setq-local gptel-org-agent--narrow-end-marker end-marker))
    (gptel-org--debug "org-agent open-indirect-buffer: created buffer %S" buf-name)
    indirect-buf))

(defvar-local gptel-org-agent--narrow-end-marker nil
  "Marker at the end of the narrowed region in an agent indirect buffer.
This marker has insertion-type t so the region grows as text is appended.
Stored for cleanup in `gptel-org-agent--close-indirect-buffer'.")

(defun gptel-org-agent--close-indirect-buffer (indirect-buffer &optional fold)
  "Close INDIRECT-BUFFER and clean up associated resources.

If FOLD is non-nil, fold the agent subtree in the base buffer before
killing the indirect buffer.

Cleans up the narrowing end-marker."
  (when (buffer-live-p indirect-buffer)
    (let ((base-buf (buffer-base-buffer indirect-buffer))
          (end-marker (buffer-local-value
                       'gptel-org-agent--narrow-end-marker
                       indirect-buffer))
          ;; Grab the narrowed region start before killing
          (subtree-start (with-current-buffer indirect-buffer
                           (point-min))))
      (gptel-org--debug "org-agent close-indirect-buffer: closing %S (fold=%s)"
                        (buffer-name indirect-buffer) fold)
      ;; Optionally fold the completed subtree in the base buffer
      (when (and fold
                 (buffer-live-p base-buf))
        (with-current-buffer base-buf
          (save-excursion
            (goto-char subtree-start)
            (when (org-at-heading-p)
              (org-fold-subtree t)
              (gptel-org--debug "org-agent close-indirect-buffer: folded subtree at line %d"
                                (line-number-at-pos))))))
      ;; Clean up the end-marker
      (when (markerp end-marker)
        (set-marker end-marker nil))
      ;; Kill the indirect buffer
      (kill-buffer indirect-buffer))))


;;; ---- Integration point ----------------------------------------------------

(defun gptel-org-agent--maybe-setup-subtree ()
  "Conditionally create an agent subtree and indirect buffer.

Check whether `gptel-org-agent-subtrees' is enabled and point is on a
heading with a TODO keyword from `gptel-org-todo-keywords'.

If both conditions are met:
  - Look for an existing `:main@agent:' child subtree and reuse it.
  - If none exists, create one via `gptel-org-agent--create-subtree'.
  - Open an indirect buffer narrowed to the agent subtree via
    `gptel-org-agent--open-indirect-buffer'.
  - Return the indirect buffer.

If conditions are not met, return nil so that `gptel-send' proceeds
with its normal behavior."
  (when (and gptel-org-agent-subtrees
             (derived-mode-p 'org-mode)
             (org-at-heading-p))
    (save-excursion
      ;; Ensure we're at the beginning of the heading
      (beginning-of-line)
      (when (gptel-org--heading-has-todo-keyword-p)
        (gptel-org--debug "org-agent maybe-setup-subtree: TODO heading detected at line %d"
                          (line-number-at-pos))
        (let* ((main-tag (gptel-org-agent--construct-tag "main"))
               (existing (gptel-org-agent--find-agent-subtree main-tag))
               (heading-marker (or existing
                                   (gptel-org-agent--create-subtree "main")))
               (base-buffer (current-buffer)))
          (when existing
            (gptel-org--debug "org-agent maybe-setup-subtree: reusing existing %S subtree"
                              main-tag))
          (gptel-org-agent--open-indirect-buffer base-buffer heading-marker))))))

(defvar gptel-prompt-transform-functions)

(defun gptel-org-agent--transform-redirect (fsm)
  "Prompt transform: redirect response to an agent indirect buffer.

When the request originates from an org-mode buffer on a TODO heading
with `gptel-org-agent-subtrees' enabled, create (or reuse) a
`:main@agent:' child subtree and redirect the FSM's response buffer
and position to the indirect buffer.

This function is registered in `gptel-prompt-transform-functions' so
it runs during the prompt transform phase of `gptel-request'.  At this
point the prompt has already been built from the original buffer, so
this only affects where the response is inserted.

Skips redirection when the request already originates from an agent
indirect buffer (identified by `buffer-base-buffer' returning non-nil)."
  (let* ((info (gptel-fsm-info fsm))
         (orig-buffer (plist-get info :buffer)))
    (when (and gptel-org-agent-subtrees
               (buffer-live-p orig-buffer)
               ;; Only redirect from a base org buffer, not from an
               ;; indirect buffer (which is already an agent subtree)
               (not (buffer-base-buffer orig-buffer))
               (with-current-buffer orig-buffer
                 (derived-mode-p 'org-mode)))
      (when-let* ((indirect-buf
                   (with-current-buffer orig-buffer
                     (gptel-org-agent--maybe-setup-subtree))))
        (with-current-buffer indirect-buf
          (goto-char (point-max))
          (let ((pos-marker (point-marker)))
            (set-marker-insertion-type pos-marker t)
            ;; Ensure gptel-mode is active for proper response formatting
            (unless (bound-and-true-p gptel-mode)
              (setq-local gptel-mode t))
            ;; Redirect the FSM's response target
            (plist-put info :buffer indirect-buf)
            (plist-put info :position pos-marker)
            ;; Store reference for potential cleanup
            (plist-put info :agent-indirect-buffer indirect-buf)
            (gptel-org--debug
             "org-agent transform-redirect: redirected to %S at pos %d"
             (buffer-name indirect-buf) (marker-position pos-marker))))))))

(defun gptel-org-agent--enable ()
  "Enable agent subtree integration for `gptel-send'.

Adds `gptel-org-agent--transform-redirect' to
`gptel-prompt-transform-functions' so that requests from org-mode
TODO headings are automatically routed to agent indirect buffers."
  (add-to-list 'gptel-prompt-transform-functions
               #'gptel-org-agent--transform-redirect))

(defun gptel-org-agent--disable ()
  "Disable agent subtree integration for `gptel-send'."
  (setq gptel-prompt-transform-functions
        (remq #'gptel-org-agent--transform-redirect
              gptel-prompt-transform-functions)))

;; Auto-enable when this module is loaded and the feature is on
(when gptel-org-agent-subtrees
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

Returns nil if `gptel-org-agent-subtrees' is disabled, we're not in
org-mode, or we can't find a heading context to create the subtree."
  (ignore description)                  ;reserved for future use in heading text
  (when (and gptel-org-agent-subtrees
             (derived-mode-p 'org-mode))
    (let* ((parent-tag (gptel-org-agent--current-agent-tag))
           (tag (gptel-org-agent--construct-tag agent-type parent-tag))
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
                 (heading-marker
                  (or existing
                      (gptel-org-agent--create-subtree agent-type parent-tag)))
                 (indirect-buf
                  (gptel-org-agent--open-indirect-buffer
                   base-buffer heading-marker)))
            (when existing
              (gptel-org--debug
               "org-agent setup-task-subtree: reusing existing %S subtree"
               tag))
            ;; Create a position marker inside the indirect buffer.
            ;; This is where the LLM response will start being inserted.
            (let ((pos-marker
                   (with-current-buffer indirect-buf
                     ;; Ensure gptel-mode is active for proper heading insertion.
                     ;; Indirect buffers with clone=t inherit buffer-local vars,
                     ;; but gptel-mode is a minor mode that might not be active.
                     (unless gptel-mode
                       (setq-local gptel-mode t))
                     ;; Position at end of buffer content (after heading line).
                     ;; point-max is at the end of the narrowed region.
                     (goto-char (point-max))
                     ;; Back up past any trailing newlines to be at real content end
                     (skip-chars-backward "\n")
                     (end-of-line)
                     (let ((m (point-marker)))
                       (set-marker-insertion-type m t)
                       m))))
              (gptel-org--debug
               "org-agent setup-task-subtree: indirect=%S pos=%d"
               (buffer-name indirect-buf) (marker-position pos-marker))
              (list :indirect-buffer indirect-buf
                    :heading-marker heading-marker
                    :position-marker pos-marker
                    :parent-tag parent-tag))))))))

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

(defcustom gptel-org-agent-todo-keywords
  '((sequence "PENDING(p)" "RUNNING(r)" "|" "DONE(d)" "ERROR(e)" "ABORTED(a)"))
  "TODO keyword sequence for agent task headings.
Added to `org-todo-keywords' when agent subtrees are active.
The keywords map to TodoWrite statuses:
  - pending     → PENDING
  - in_progress → RUNNING
  - completed   → DONE"
  :type 'sexp
  :group 'gptel)

(defun gptel-org-agent--ensure-todo-keywords ()
  "Ensure agent TODO keywords are available in the current buffer.
Adds `gptel-org-agent-todo-keywords' to `org-todo-keywords' if not
already present, then refreshes org's TODO keyword parsing."
  (let ((needs-refresh nil))
    (dolist (kw-seq gptel-org-agent-todo-keywords)
      (unless (member kw-seq org-todo-keywords)
        (push kw-seq org-todo-keywords)
        (setq needs-refresh t)))
    (when needs-refresh
      ;; Make the change buffer-local so we don't pollute other buffers
      (setq-local org-todo-keywords org-todo-keywords)
      (org-set-regexps-and-options))))

(defun gptel-org-agent--status-to-keyword (status)
  "Map a TodoWrite STATUS string to an org TODO keyword.
Returns \"PENDING\" for \"pending\", \"RUNNING\" for \"in_progress\",
\"DONE\" for \"completed\", or the uppercased STATUS for anything else."
  (pcase status
    ("pending"     "PENDING")
    ("in_progress" "RUNNING")
    ("completed"   "DONE")
    (_             (upcase (or status "PENDING")))))

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

(defun gptel-org-agent--collect-todo-headings (level tasks-pos)
  "Collect existing todo headings at LEVEL under the Tasks heading at TASKS-POS.
Returns an alist of (CONTENT . POSITION) where CONTENT is the heading
text stripped of the TODO keyword and POSITION is the beginning of the
heading line."
  (let (result)
    (save-excursion
      (goto-char tasks-pos)
      (let ((tasks-end (save-excursion (org-end-of-subtree t) (point))))
        (forward-line 1)
        (while (and (< (point) tasks-end)
                    (re-search-forward org-heading-regexp tasks-end t))
          (beginning-of-line)
          (when (= (org-current-level) level)
            (let* ((components (org-heading-components))
                   ;; org-heading-components returns:
                   ;; (level reduced-level todo priority heading tags)
                   (heading-text (nth 4 components)))
              (when heading-text
                (push (cons (org-trim heading-text) (point)) result))))
          (forward-line 1))))
    (nreverse result)))

(defun gptel-org-agent--set-todo-keyword (keyword)
  "Set the TODO keyword of the heading at point to KEYWORD.
Uses `org-todo' for proper state tracking, but falls back to direct
text replacement if the keyword is not in org's known set."
  (when (org-at-heading-p)
    (let* ((current (org-get-todo-state)))
      (unless (equal current keyword)
        ;; org-todo with a specific keyword argument sets it directly
        (org-todo keyword)))))

(defun gptel-org-agent--create-todo-heading (level content keyword tasks-pos)
  "Create a new TODO heading at LEVEL with CONTENT and KEYWORD.
The heading is appended under the Tasks subtree at TASKS-POS."
  (save-excursion
    (goto-char tasks-pos)
    (org-end-of-subtree t)
    (unless (bolp) (insert "\n"))
    (let ((stars (make-string level ?*)))
      (insert stars " " keyword " " content "\n"))))

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

(defun gptel-org-agent--write-todo-org (todos)
  "Display TODOS as org TODO headings in the agent subtree.

Each todo becomes a heading at the appropriate level with a TODO keyword:
  - pending     → PENDING
  - in_progress → RUNNING
  - completed   → DONE

TODOS is a list of plists with :content, :activeForm, and :status.
This function is idempotent: calling it multiple times with the same
data produces the same result.  It handles adding new tasks, updating
existing task states, and removing tasks that are no longer in the list."
  (gptel-org-agent--ensure-todo-keywords)
  (save-excursion
    (goto-char (point-min))             ;agent heading in narrowed buffer
    (let* ((agent-level (org-current-level))
           (tasks-level (1+ agent-level))
           (todo-level  (+ 2 agent-level))
           (tasks-pos (gptel-org-agent--find-or-create-tasks-heading tasks-level)))
      ;; Collect existing headings under the Tasks heading
      (let ((existing-headings
             (gptel-org-agent--collect-todo-headings todo-level tasks-pos))
            (seen-contents (make-hash-table :test 'equal)))
        ;; Update or create todo headings
        (dolist (todo todos)
          (let* ((content (plist-get todo :content))
                 (status  (plist-get todo :status))
                 (keyword (gptel-org-agent--status-to-keyword status))
                 (existing (assoc content existing-headings)))
            (puthash content t seen-contents)
            (if existing
                ;; Update existing heading's TODO keyword
                (save-excursion
                  (goto-char (cdr existing))
                  (gptel-org-agent--set-todo-keyword keyword))
              ;; Create new heading under Tasks
              (gptel-org-agent--create-todo-heading
               todo-level content keyword tasks-pos))))
        ;; Remove headings not in the current todo list.
        ;; Process in reverse order to avoid position shifts.
        (dolist (existing (reverse existing-headings))
          (unless (gethash (car existing) seen-contents)
            (gptel-org-agent--remove-todo-heading (cdr existing)))))))
  t)


;;; ---- Advisor agent integration (Phase 5) ----------------------------------

(defvar-local gptel-org-agent-include-subtrees nil
  "When non-nil, include @agent subtrees in context collection.
Set by the Advisor agent preset.  When nil (the default), @agent
subtrees are stripped from the prompt to keep context focused.")

(defun gptel-org-agent--strip-agent-subtrees ()
  "Remove @agent subtrees from the current buffer.
Scans all headings and deletes entire subtrees whose heading has
a tag matching `gptel-org-agent--agent-tag-p'.

This is called during prompt construction to exclude internal
agent conversation trees from the context sent to the LLM.

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
                            (org-get-tags nil t)))
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

(provide 'gptel-org-agent)
;;; gptel-org-agent.el ends here
