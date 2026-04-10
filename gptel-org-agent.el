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

;; Forward declarations for functions defined in gptel-org.el
(declare-function gptel-org--debug "gptel-org")
(declare-function gptel-org--heading-has-tag-p "gptel-org")
(declare-function gptel-org--heading-has-todo-keyword-p "gptel-org")

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
(declare-function gptel--format-tool-call "gptel")
(declare-function gptel--map-tool-args "gptel-request")
(declare-function gptel--update-status "gptel")
(declare-function gptel--to-string "gptel")
(declare-function gptel-tool-name "gptel")
(declare-function gptel-tool-async "gptel")
(declare-function gptel-tool-function "gptel")

;; Forward declarations for variables defined in gptel.el
(defvar gptel-mode)
(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel--preset)


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

The child heading copies the parent heading's text and includes the
AI-DOING keyword (from `gptel-org-tasks-doing-keyword').

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
           (parent-heading (org-element-property :raw-value (org-element-at-point)))
           (doing-keyword (or (bound-and-true-p gptel-org-tasks-doing-keyword) "AI-DOING"))
           (stars (make-string child-level ?*))
           (heading-text (if parent-heading
                             (format "%s %s %s :%s:" stars doing-keyword parent-heading tag)
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
  "Create a new AI-DO sibling heading for a handover task.

Navigate from the end of the buffer backwards to find the current
agent subtree heading, then go to its parent task heading.  Insert a
new child heading under that parent with TODO keyword AI-DO and
DESCRIPTION as the heading text.  BODY is inserted as the heading
content.

This is used by the handover mechanism: when a triage agent hands
over to a specialist, the handover agent's output becomes a new
AI-DO task heading that can be picked up by the scheduler or user.

Returns the heading text of the created heading, or nil on failure."
  (save-excursion
    ;; Find an agent heading by searching backwards
    (goto-char (point-max))
    (let ((found nil))
      (while (and (not found)
                  (re-search-backward org-heading-regexp nil t))
        (let ((tags (org-get-tags nil t)))
          (when (cl-some #'gptel-org-agent--agent-tag-p tags)
            ;; Found an agent heading — go up to its parent (the task heading)
            (when (> (org-current-level) 1)
              (org-up-heading-safe)
              (setq found t)))))
      (when found
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
          heading-text)))))

(defun gptel-org-agent--extract-parent-context ()
  "Extract the full text of the parent TODO heading for handover context.

When called from an agent indirect buffer, navigate to the base buffer,
find the parent heading of the current agent subtree, and return its
full subtree content as a string.

This is used by the handover mechanism: the handover agent needs to read
all the triage agent's findings, which are written under the parent
TODO heading.  Returns nil if the context cannot be extracted."
  (let ((base-buf (buffer-base-buffer (current-buffer))))
    (when (and base-buf (buffer-live-p base-buf))
      (let ((agent-tag (gptel-org-agent--current-agent-tag)))
        (when agent-tag
          (with-current-buffer base-buf
            (save-excursion
              (goto-char (point-min))
              ;; Find the heading with our agent tag
              (let ((found nil))
                (while (and (not found)
                            (re-search-forward org-heading-regexp nil t))
                  (beginning-of-line)
                  (let ((tags (org-get-tags nil t)))
                    (when (cl-some (lambda (tg)
                                     (string-equal-ignore-case tg agent-tag))
                                   tags)
                      (setq found t)))
                  (unless found (forward-line 1)))
                (when found
                  ;; Go up to the parent heading (the task heading)
                  (when (org-up-heading-safe)
                    (let ((beg (point))
                          (end (save-excursion
                                 (org-end-of-subtree t)
                                 (point))))
                      (buffer-substring-no-properties beg end))))))))))))

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

Create an indirect buffer via `make-indirect-buffer' with cloned
state, narrow it to the agent's subtree, and decouple folding so
that fold state in the indirect buffer is independent of the base.

The narrowing end uses a marker with insertion-type t, so that as
the LLM streams text at the end of the subtree the narrow region
expands automatically.

Each indirect buffer gets a unique name based on the agent tag and
a short hash of the outline path, so multiple agent conversations
can coexist without killing each other's buffers.

Return the indirect buffer."
  (let* ((heading-pos (marker-position heading-marker))
         ;; Determine the agent tag for the buffer name
         (tag (with-current-buffer base-buffer
                (save-excursion
                  (goto-char heading-pos)
                  (let ((tags (org-get-tags nil t)))
                    (or (cl-find-if #'gptel-org-agent--agent-tag-p tags)
                        "agent")))))
         (buf-name (gptel-org-agent--indirect-buffer-name
                    base-buffer heading-pos tag))
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
    ;; If a buffer with this name already exists (same heading re-sent),
    ;; kill it to get a fresh one.  Different headings produce different
    ;; hashes so their buffers coexist.
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

When point is on a `:user:' heading (created after an agent response),
walk up to the parent TODO heading so the existing agent subtree is
reused for the follow-up request.

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
        ;; If on a :user: heading (no TODO state), walk up to parent TODO
        ;; heading.  This handles the case where the user sends a follow-up
        ;; from the :user: sibling created after an agent response.
        (unless (org-get-todo-state)
          (let ((user-tag (if (boundp 'gptel-org-user-tag)
                              gptel-org-user-tag
                            "user")))
            (when (cl-some (lambda (tg)
                             (string-equal-ignore-case tg user-tag))
                           (org-get-tags nil t))
              (gptel-org--debug
               "org-agent maybe-setup-subtree: on :user: heading at line %d, walking up to parent"
               (line-number-at-pos))
              (ignore-errors (outline-up-heading 1 t)))))
        ;; Accept any org TODO keyword, not just gptel-org-todo-keywords.
        ;; The gptel-org-todo-keywords list is for model-tag extraction,
        ;; agent subtrees should work with any task heading.
        (when (org-get-todo-state)
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
            (gptel-org-agent--open-indirect-buffer base-buffer heading-marker)))))))

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
             "org-agent transform-redirect: redirected to %S at pos %d (preset=%S)"
             (buffer-name indirect-buf) (marker-position pos-marker) preset)))))))


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
  "Insert a :user: heading after the agent subtree when response completes.
Added to `gptel-post-response-functions'.

Creates an empty heading tagged with `gptel-org-user-tag' as a sibling
after the agent subtree in the base buffer.  This heading serves as the
prompt location for the user's next message in the conversation."
  (gptel-org--debug "insert-user-heading: buf=%S indirect=%s"
                    (buffer-name)
                    (if (buffer-base-buffer) "yes" "no"))
  (let ((in-agent (gptel-org--in-agent-indirect-buffer-p)))
    (gptel-org--debug "insert-user-heading: in-agent-indirect=%s" in-agent)
    (when-let* ((in-agent)
                (base-buffer (buffer-base-buffer (current-buffer)))
                (user-tag (if (boundp 'gptel-org-user-tag)
                              gptel-org-user-tag
                            "user")))
      (gptel-org--debug "insert-user-heading: base-buffer=%S user-tag=%S"
                        (buffer-name base-buffer) user-tag)
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
                       (inhibit-read-only t))
                  (gptel-org--debug "insert-user-heading: agent heading=%S level=%d tags=%S"
                                    agent-heading agent-level agent-tags)
                  ;; Check if a user heading already exists as next sibling
                  (org-end-of-subtree t)
                  (let* ((after-agent (point))
                         (at-heading (org-at-heading-p))
                         (next-level (when at-heading (org-current-level)))
                         (has-user
                          (and at-heading
                               (= next-level agent-level)
                               (if (and (boundp 'gptel-org-use-todo-keywords)
                                        gptel-org-use-todo-keywords)
                                   (gptel-org--heading-is-user-p)
                                 (let ((next-tags (org-get-tags nil t)))
                                   (cl-some
                                    (lambda (tg)
                                      (string-equal-ignore-case tg user-tag))
                                    next-tags))))))
                    (gptel-org--debug "insert-user-heading: after-subtree pos=%d at-heading=%s next-level=%S has-user=%s"
                                      after-agent at-heading next-level has-user)
                    (unless has-user
                      ;; No user heading exists, create one
                      (goto-char after-agent)
                      (unless (bolp) (insert "\n"))
                      (let ((stars (make-string agent-level ?*)))
                        (if (and (boundp 'gptel-org-use-todo-keywords)
                                 gptel-org-use-todo-keywords)
                            (progn
                              (insert (format "%s %s \n" stars
                                              (if (boundp 'gptel-org-user-keyword)
                                                  gptel-org-user-keyword "HI")))
                              (gptel-org--debug
                               "insert-user-heading: created %s heading at level %d after pos %d"
                               (if (boundp 'gptel-org-user-keyword) gptel-org-user-keyword "HI")
                               agent-level after-agent))
                          (insert (format "%s \n" stars))
                          (forward-line -1)
                          (beginning-of-line)
                          (org-set-tags (list user-tag))
                          (gptel-org--debug
                           "insert-user-heading: created :%s: heading at level %d after pos %d"
                           user-tag agent-level after-agent))))))))))))))

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
  (ignore description)                  ;reserved for future use in heading text
  (when (and gptel-org-subtree-context
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

Each todo becomes a heading at the appropriate level with a TODO keyword
mapped via `gptel-org-agent-status-keyword-map' (default: AI-DO/AI-DOING/AI-DONE).

TODOS is a list of plists with :content, :activeForm, and :status.
This function is idempotent: calling it multiple times with the same
data produces the same result.  It handles adding new tasks, updating
existing task states, and removing tasks that are no longer in the list."
  (gptel-org-agent--ensure-todo-keywords)
  (when (vectorp todos) (setq todos (append todos nil)))
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
             (tasks-level (1+ agent-level))
             (todo-level  (+ 2 agent-level))
             (tasks-pos (gptel-org-agent--find-or-create-tasks-heading tasks-level)))
        (gptel-org--debug "write-todo-org: agent-level=%d tasks-level=%d todo-level=%d tasks-pos=%d"
                          agent-level tasks-level todo-level tasks-pos)
        ;; Collect existing headings under the Tasks heading
        (let ((existing-headings
               (gptel-org-agent--collect-todo-headings todo-level tasks-pos))
              (seen-contents (make-hash-table :test 'equal)))
          (gptel-org--debug "write-todo-org: %d existing headings found"
                            (length existing-headings))
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
                    (gptel-org--debug "write-todo-org: updating %S -> %S at %d"
                                      content keyword (cdr existing))
                    (save-excursion
                      (goto-char (cdr existing))
                      (gptel-org-agent--set-todo-keyword keyword)))
                ;; Create new heading under Tasks
                (gptel-org--debug "write-todo-org: creating %S %S at tasks-pos=%d"
                                  keyword content tasks-pos)
                (gptel-org-agent--create-todo-heading
                 todo-level content keyword tasks-pos))))
          ;; Remove headings not in the current todo list.
          ;; Process in reverse order to avoid position shifts.
          (dolist (existing (reverse existing-headings))
            (unless (gethash (car existing) seen-contents)
              (gptel-org--debug "write-todo-org: removing %S" (car existing))
              (gptel-org-agent--remove-todo-heading (cdr existing))))))))
  (gptel-org--debug "write-todo-org: done, buffer size now %d" (buffer-size))
  t)


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
  "Display TOOL-CALLS as a PENDING org heading in the agent subtree.

Creates a child heading under the current position with the PENDING
TODO state and inserts tool call details as body text.  Stores the
tool call data in `gptel-org-agent--pending-tool-calls' keyed by a
unique ID stored as an org property on the heading.

TOOL-CALLS is a list of (tool-spec arg-plist process-tool-result).
INFO is the FSM info plist."
  (let* ((buf (plist-get info :buffer))
         (start-marker (plist-get info :position))
         (pending-kw (nth 0 gptel-org-agent-tool-confirm-keywords))
         (pending-id (gptel-org-agent--generate-pending-id)))
    (with-current-buffer buf
      ;; Ensure the todo-state-change hook is registered on both the
      ;; indirect buffer and its base buffer so that the user can
      ;; change PENDING→ALLOWED from either buffer.
      (gptel-org-agent--ensure-tool-confirm-hook buf)
      (save-excursion
        (goto-char start-marker)
        ;; Navigate to enclosing heading to determine level
        (unless (org-at-heading-p)
          (ignore-errors (org-back-to-heading t)))
        (when (org-at-heading-p)
          (let* ((parent-level (org-current-level))
                 (child-level (1+ parent-level))
                 (stars (make-string child-level ?*))
                 (tool-names (mapconcat
                              (lambda (tc)
                                (gptel-tool-name (car tc)))
                              tool-calls ", "))
                 (inhibit-read-only t))
            ;; Move to end of current subtree content (before any child subtrees)
            (org-end-of-subtree t)
            (unless (bolp) (insert "\n"))
            (let ((heading-pos (point)))
              ;; Insert the PENDING heading
              (insert (format "%s %s Requesting permission to run: %s\n"
                              stars pending-kw tool-names))
              ;; Insert tool call details as body
              (dolist (tc tool-calls)
                (let* ((tool-spec (car tc))
                       (arg-plist (cadr tc))
                       (arg-values (gptel--map-tool-args tool-spec arg-plist)))
                  (insert (gptel--format-tool-call
                           (gptel-tool-name tool-spec) arg-values))))
              (insert "\n")
              ;; Store the pending ID as an org property on the heading
              (save-excursion
                (goto-char heading-pos)
                (org-set-property "GPTEL_PENDING_ID" pending-id))
              ;; Store tool call data in global hash table
              (puthash pending-id
                       (list :tool-calls tool-calls
                             :info info
                             :buffer buf)
                       gptel-org-agent--pending-tool-calls)
              ;; Mark tool-pending so the FSM knows we're waiting
              (plist-put info :tool-pending t)
              (gptel-org--debug
               "org-agent tool-confirm: created PENDING heading for %s (id=%s)"
               tool-names pending-id))))))))

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
       (format "accept-tool-calls: info keys=%S fsm-last-in-current=%S fsm-last-in-info-buf=%S"
               (cl-loop for (k _v) on info by #'cddr collect k)
               (and (boundp 'gptel--fsm-last) gptel--fsm-last)
               (and buf (buffer-live-p buf)
                    (buffer-local-value 'gptel--fsm-last buf)))
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
                   (format "accept-tool-calls: [%d] tool=%s process-tool-result=%S"
                           idx (gptel-tool-name tool-spec) process-tool-result)
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
  (gptel-org--debug
   "org-agent tool-confirm: todo-state-change hook fired in buffer %s, state=%s, subtrees=%s"
   (buffer-name) (and (boundp 'org-state) org-state)
   gptel-org-subtree-context)
  (when (and (bound-and-true-p gptel-org-subtree-context)
             (org-at-heading-p))
    (let ((new-state org-state)
          (allowed-kw (nth 1 gptel-org-agent-tool-confirm-keywords))
          (denied-kw (nth 2 gptel-org-agent-tool-confirm-keywords))
          (pending-id (org-entry-get nil "GPTEL_PENDING_ID")))
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
                             (format "on-todo-change: tool-call[%d] tool=%s process-tool-result=%S"
                                     i (and ts (gptel-tool-name ts)) ptr)
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
              (cond
               ((equal new-state allowed-kw)
                (gptel-org--debug
                 "org-agent tool-confirm: ALLOWED (id=%s), running %d tool calls"
                 pending-id (length tool-calls))
                (when (eq gptel-log-level 'debug)
                  (gptel--log
                   (format "debug-state-change: ALLOWED in buffer=%s base-buffer=%s stored-buf=%s stored-buf-live=%s info-buffer=%s info-buffer-live=%s fsm-last=%S"
                           (buffer-name)
                           (and (buffer-base-buffer) (buffer-name (buffer-base-buffer)))
                           (and stored-buf (buffer-name stored-buf))
                           (and stored-buf (buffer-live-p stored-buf))
                           (and info (plist-get info :buffer)
                                (buffer-name (plist-get info :buffer)))
                           (and info (plist-get info :buffer)
                                (buffer-live-p (plist-get info :buffer)))
                           (and (boundp 'gptel--fsm-last) gptel--fsm-last))
                   "debug-state-change" 'no-json)
                  ;; Log closures to verify FSM captured in process-tool-result
                  (cl-loop for (ts _ap ptr) in tool-calls
                           for i from 0
                           do (gptel--log
                               (format "debug-state-change: ALLOWED tool-call[%d] tool=%s process-tool-result=%S"
                                       i (and ts (gptel-tool-name ts)) ptr)
                               "debug-state-change" 'no-json)))
                (gptel-org-agent--accept-tool-calls tool-calls info)
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

(provide 'gptel-org-agent)
;;; gptel-org-agent.el ends here
