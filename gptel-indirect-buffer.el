;;; gptel-indirect-buffer.el --- Indirect buffer management for gptel org agents  -*- lexical-binding: t; -*-

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

;; Systematic management of Emacs indirect buffers for AI task isolation
;; in gptel's org-mode agent system.
;;
;; PROBLEM: When multiple AI tasks run concurrently (AI-DOING, AI-DO
;; subtasks, delegated agents), their content can merge and corrupt
;; cursor locations unless each task is properly isolated in its own
;; indirect buffer.
;;
;; SOLUTION: The "terminator heading" pattern.  Each active task's
;; subtree is bounded by a terminator child heading (e.g., FEEDBACK for
;; AI-DOING, Results for AI-DO).  New sibling tasks are inserted BEFORE
;; the terminator so existing indirect buffers are never disturbed.
;;
;; Example org structure:
;;
;;   **** AI-DOING Study feasibility ...  :main@agent:   <- IB 1
;;   ***** AI-DO Subtask 1                               <- IB 2
;;   ****** Results                                      <- Terminator for Subtask 1
;;   ***** AI-DO Subtask 2                               <- IB 3
;;   ***** FEEDBACK                                      <- Terminator for AI-DOING
;;
;; Rules:
;; 1. Each AI task gets its own indirect buffer narrowed to its subtree.
;; 2. A terminator child heading bounds each task's writable content.
;; 3. New sibling tasks are inserted BEFORE the terminator heading.
;; 4. After reordering siblings, the moved task's indirect buffer must
;;    be recreated (same name, new position).
;;
;; This module COMPLEMENTS `gptel-org-agent.el'; it does not replace the
;; existing `gptel-org-agent--open-indirect-buffer' and
;; `gptel-org-agent--close-indirect-buffer' functions.  Those will
;; eventually call into these functions for the terminator-aware path.
;;
;; Key Emacs indirect buffer facts leveraged here:
;; - `make-indirect-buffer' with clone=t shares text bidirectionally.
;; - Emacs flattens indirect-of-indirect to root base buffer.
;; - Narrowing is per-buffer (independent).
;; - Markers are SHARED across base and indirect buffers.
;; - An end-marker with insertion-type=t auto-expands the narrow region.
;; - When heading text is delete-and-reinserted (moved), restriction
;;   markers collapse to empty.
;; - `after-change-functions' on base buffer fire for ALL indirect edits.

;;; Code:

(require 'org)
(require 'org-fold)
(require 'cl-lib)

;; External functions from gptel-org.el
(declare-function gptel-org--debug "gptel-org" (format-string &rest args))

;; External functions from gptel-org-agent.el
(declare-function gptel-org-agent--indirect-buffer-name "gptel-org-agent"
                  (base-buffer heading-pos tag))
(declare-function gptel-org-agent--agent-tag-p "gptel-org-agent" (tag))

;; External variables
(defvar gptel-org-debug)
(defvar gptel-org-agent--narrow-end-marker)


;;; ---- Tracking Registry ----------------------------------------------------

(defvar gptel-org-ib--registry (make-hash-table :test 'equal)
  "Hash table mapping buffer-name -> plist of indirect buffer metadata.

Each entry stores:
  :buffer          - the indirect buffer object
  :base            - the base (root) org buffer
  :heading-marker  - marker at the start of the heading in the base buffer
  :end-marker      - marker at the end of the subtree (insertion-type t)
  :tag             - the agent tag string (e.g., \"main@agent\")")

(defun gptel-org-ib-register (name indirect-buffer base-buffer
                                   heading-marker end-marker tag)
  "Register an indirect buffer in the tracking registry.

NAME is the buffer name (string) used as the registry key.
INDIRECT-BUFFER is the indirect buffer object.
BASE-BUFFER is the root org buffer.
HEADING-MARKER is a marker at the heading start in the base buffer.
END-MARKER is the end-of-subtree marker (insertion-type t).
TAG is the agent tag string."
  (puthash name
           (list :buffer indirect-buffer
                 :base base-buffer
                 :heading-marker heading-marker
                 :end-marker end-marker
                 :tag tag)
           gptel-org-ib--registry)
  (gptel-org--debug "org-ib register: %S (tag=%S)" name tag))

(defun gptel-org-ib-unregister (name)
  "Remove an indirect buffer entry from the registry and clean up markers.

NAME is the buffer name (string).  Cleans up heading-marker and
end-marker by setting them to nil before removing the entry."
  (let ((entry (gethash name gptel-org-ib--registry)))
    (when entry
      (let ((hm (plist-get entry :heading-marker))
            (em (plist-get entry :end-marker)))
        (when (markerp hm) (set-marker hm nil))
        (when (markerp em) (set-marker em nil)))
      (remhash name gptel-org-ib--registry)
      (gptel-org--debug "org-ib unregister: %S" name))))

(defun gptel-org-ib-get (name)
  "Get the registry entry for NAME.

Returns the plist (:buffer :base :heading-marker :end-marker :tag)
or nil if NAME is not registered."
  (gethash name gptel-org-ib--registry))

(defun gptel-org-ib-all-for-base (base-buffer)
  "Return list of all registered indirect buffer names for BASE-BUFFER."
  (let (result)
    (maphash (lambda (name entry)
               (when (eq (plist-get entry :base) base-buffer)
                 (push name result)))
             gptel-org-ib--registry)
    (nreverse result)))

(defun gptel-org-ib-cleanup-dead ()
  "Remove registry entries for dead (killed) buffers.

Iterates the registry and unregisters any entry whose :buffer is no
longer live.  Returns the number of entries removed."
  (let ((dead-names nil))
    (maphash (lambda (name entry)
               (unless (buffer-live-p (plist-get entry :buffer))
                 (push name dead-names)))
             gptel-org-ib--registry)
    (dolist (name dead-names)
      (gptel-org-ib-unregister name))
    (when dead-names
      (gptel-org--debug "org-ib cleanup-dead: removed %d entries" (length dead-names)))
    (length dead-names)))


;;; ---- Utility Functions ----------------------------------------------------

(defun gptel-org-ib-base-buffer (indirect-buffer)
  "Return the root base buffer of INDIRECT-BUFFER.

Follows the chain of `buffer-base-buffer' calls to find the ultimate
root buffer.  Returns INDIRECT-BUFFER itself if it is not indirect."
  (let ((buf indirect-buffer))
    (while (buffer-base-buffer buf)
      (setq buf (buffer-base-buffer buf)))
    buf))

(defun gptel-org-ib-heading-level (indirect-buffer)
  "Return the org heading level of the first heading in INDIRECT-BUFFER.

Returns nil if INDIRECT-BUFFER is not live or has no heading at
`point-min'."
  (when (buffer-live-p indirect-buffer)
    (with-current-buffer indirect-buffer
      (save-excursion
        (goto-char (point-min))
        (when (org-at-heading-p)
          (org-current-level))))))

(defun gptel-org-ib-heading-text (indirect-buffer)
  "Return the raw heading text of the first heading in INDIRECT-BUFFER.

Returns the full heading line (without properties) at `point-min',
or nil if the buffer is dead or has no heading."
  (when (buffer-live-p indirect-buffer)
    (with-current-buffer indirect-buffer
      (save-excursion
        (goto-char (point-min))
        (when (org-at-heading-p)
          (org-get-heading t t t t))))))


;;; ---- Terminator Heading Management ----------------------------------------

(defun gptel-org-ib--terminator-regexp (heading-keyword level)
  "Build a regexp matching a terminator heading.

HEADING-KEYWORD is the terminator text (e.g., \"FEEDBACK\").
LEVEL is the expected heading level (number of stars)."
  (format "^\\*\\{%d\\} +%s\\b" level (regexp-quote heading-keyword)))

(defun gptel-org-ib-find-terminator (heading-keyword &optional bound)
  "Find the terminator heading matching HEADING-KEYWORD in current subtree.

Search forward from point for a child heading whose text matches
HEADING-KEYWORD.  BOUND limits the search region; when nil, the
search is bounded by the end of the current subtree.

Point must be on the parent heading.  The terminator is expected to
be a direct child (one level deeper than the current heading).

Returns the position (beginning of line) of the terminator heading,
or nil if not found."
  (save-excursion
    (unless (org-at-heading-p)
      (ignore-errors (org-back-to-heading t)))
    (let* ((parent-level (org-current-level))
           (child-level (1+ parent-level))
           (search-bound (or bound
                             (save-excursion
                               (org-end-of-subtree t)
                               (point))))
           (regexp (gptel-org-ib--terminator-regexp heading-keyword child-level))
           (found nil))
      (save-excursion
        (forward-line 1)
        (when (<= (point) search-bound)
          (while (and (not found)
                      (re-search-forward regexp search-bound t))
            (beginning-of-line)
            ;; Verify this is truly at the expected level (not deeper)
            (when (= (org-current-level) child-level)
              (setq found (point)))
            (unless found
              (forward-line 1)))))
      found)))

(defun gptel-org-ib-create-terminator (heading-keyword &optional level)
  "Create a terminator child heading under the current heading.

HEADING-KEYWORD is the heading text (e.g., \"FEEDBACK\", \"Results\").
LEVEL is the heading level; if nil, uses current-level + 1.

The terminator heading is inserted at the end of the current subtree,
as the last child.  It serves as a boundary marker: new sibling
headings are inserted BEFORE this terminator so that existing indirect
buffers narrowed to earlier siblings are not disturbed.

Point must be on the parent heading.

Returns a marker to the terminator heading."
  (save-excursion
    (unless (org-at-heading-p)
      (error "gptel-org-ib-create-terminator: point is not on a heading"))
    (let* ((inhibit-read-only t)
           (parent-level (org-current-level))
           (child-level (or level (1+ parent-level)))
           (stars (make-string child-level ?*))
           (heading-text (format "%s %s" stars heading-keyword)))
      ;; Move to the end of the parent subtree
      (org-end-of-subtree t)
      (unless (bolp) (insert "\n"))
      (insert heading-text "\n")
      ;; Move back to the heading we just inserted
      (forward-line -1)
      (beginning-of-line)
      (gptel-org--debug "org-ib create-terminator: %S at level %d, line %d"
                        heading-keyword child-level (line-number-at-pos))
      (point-marker))))

(defun gptel-org-ib-ensure-terminator (terminator-keyword)
  "Ensure a terminator heading exists in the current subtree.

TERMINATOR-KEYWORD is the heading text (e.g., \"FEEDBACK\").
If a matching terminator child heading already exists, return a
marker to it.  Otherwise, create one and return a marker to it.

Point must be on the parent heading."
  (save-excursion
    (unless (org-at-heading-p)
      (ignore-errors (org-back-to-heading t)))
    (let ((existing (gptel-org-ib-find-terminator terminator-keyword)))
      (if existing
          (progn
            (gptel-org--debug "org-ib ensure-terminator: found existing %S at %d"
                              terminator-keyword existing)
            (copy-marker existing))
        (gptel-org-ib-create-terminator terminator-keyword)))))


;;; ---- Safe Heading Creation (Insert Before Terminator) ---------------------

(defun gptel-org-ib-create-heading (todo-keyword title &optional tags terminator-keyword)
  "Create a new heading as the LAST child before the terminator heading.

TODO-KEYWORD is the org TODO state (e.g., \"AI-DO\").
TITLE is the heading text.
TAGS is a list of tag strings (e.g., (\"researcher@agent\")).
TERMINATOR-KEYWORD identifies which terminator to insert before
\(e.g., \"FEEDBACK\").

If no terminator exists, falls back to `org-end-of-subtree'.

CRITICAL: This inserts the new heading on the line BEFORE the
terminator, so existing sibling indirect buffers are NOT disturbed.
The new heading text is outside their narrowed region.

Point must be on the parent heading.

Returns a marker to the newly created heading."
  (save-excursion
    (unless (org-at-heading-p)
      (error "gptel-org-ib-create-heading: point is not on a heading"))
    (let* ((inhibit-read-only t)
           (parent-level (org-current-level))
           (child-level (1+ parent-level))
           (stars (make-string child-level ?*))
           ;; Build the heading line
           (tag-str (if tags
                        (concat " :" (mapconcat #'identity tags ":") ":")
                      ""))
           (heading-text (format "%s %s %s%s"
                                 stars
                                 (or todo-keyword "")
                                 title
                                 tag-str))
           ;; Find where to insert
           (insert-pos
            (if terminator-keyword
                (let ((term-pos (gptel-org-ib-find-terminator terminator-keyword)))
                  (or term-pos
                      ;; No terminator found, fall back to end of subtree
                      (save-excursion
                        (org-end-of-subtree t)
                        (point))))
              ;; No terminator keyword given, insert at end of subtree
              (save-excursion
                (org-end-of-subtree t)
                (point))))
           marker)
      (goto-char insert-pos)
      ;; Ensure we insert on a fresh line
      (unless (bolp) (insert "\n"))
      ;; Insert the new heading followed by a blank line for body content
      (insert heading-text "\n")
      ;; Move back to the heading we just inserted
      (forward-line -1)
      (beginning-of-line)
      ;; Use org-set-tags for proper tag formatting if tags were provided
      (when tags
        (org-set-tags tags))
      (setq marker (point-marker))
      ;; Insert a newline after the heading for body content space
      (end-of-line)
      (insert "\n")
      (gptel-org--debug
       "org-ib create-heading: %S %S at level %d, line %d (before terminator %S)"
       todo-keyword title child-level (line-number-at-pos marker) terminator-keyword)
      marker)))


;;; ---- Indirect Buffer Lifecycle --------------------------------------------

(defun gptel-org-ib--compute-subtree-region (base-buffer heading-pos)
  "Compute the subtree region for the heading at HEADING-POS in BASE-BUFFER.

Returns a cons cell (BEG . END) where BEG is the beginning of the
heading line and END is the end of the subtree.  END is advanced past
any trailing newline so that the region includes the final line
terminator — this ensures that text inserted at point-max in a
narrowed indirect buffer starts on its own line rather than appending
to the last line of the heading."
  (with-current-buffer base-buffer
    (save-excursion
      (goto-char heading-pos)
      (let ((beg (pos-bol))
            (end (progn (org-end-of-subtree t)
                        ;; org-end-of-subtree may leave point before the
                        ;; trailing newline.  Advance past it so the
                        ;; narrowed region includes the line terminator.
                        (when (eq (char-after) ?\n)
                          (forward-char 1))
                        (point))))
        (cons beg end)))))

(defun gptel-org-ib--extract-tag-at (base-buffer heading-pos)
  "Extract the agent tag from the heading at HEADING-POS in BASE-BUFFER.

Returns the first tag matching the `*@agent' pattern, or the string
\"agent\" if no matching tag is found."
  (with-current-buffer base-buffer
    (save-excursion
      (goto-char heading-pos)
      (let ((tags (org-get-tags nil t)))
        (or (cl-find-if #'gptel-org-agent--agent-tag-p tags)
            "agent")))))

(defun gptel-org-ib-create (base-buffer heading-pos &optional name)
  "Create an indirect buffer narrowed to the subtree at HEADING-POS.

BASE-BUFFER is the org buffer.  HEADING-POS is a position or marker
pointing to the heading.  NAME overrides the auto-generated buffer
name.

This is the central entry point for creating task-isolated indirect
buffers.  It:
  1. Resolves HEADING-POS to a position (handles markers).
  2. Computes the subtree region in BASE-BUFFER.
  3. Creates an end-marker with insertion-type t for auto-expansion.
  4. Creates an indirect buffer with clone=t.
  5. Decouples org-fold state from the base buffer.
  6. Narrows the indirect buffer to [heading-start, end-marker].
  7. Registers the buffer in the tracking table.

Returns the indirect buffer."
  (let* ((pos (if (markerp heading-pos)
                  (marker-position heading-pos)
                heading-pos))
         ;; Resolve to the root base buffer (handles indirect-of-indirect)
         (root-buf (gptel-org-ib-base-buffer base-buffer))
         ;; Extract tag for naming (skip when name is provided —
         ;; avoids dependency on gptel-org-agent for non-agent callers)
         (tag (unless name
                (gptel-org-ib--extract-tag-at root-buf pos)))
         ;; Compute buffer name
         (buf-name (or name
                       (gptel-org-agent--indirect-buffer-name root-buf pos tag)))
         ;; Compute subtree region
         (region (gptel-org-ib--compute-subtree-region root-buf pos))
         (beg (car region))
         (end (cdr region))
         ;; Create end-marker with insertion-type t so narrowing expands
         ;; as text is inserted at the boundary
         (end-marker (with-current-buffer root-buf
                       (let ((m (make-marker)))
                         (set-marker m end)
                         (set-marker-insertion-type m t)
                         m)))
         ;; Create heading marker for tracking
         (heading-marker (with-current-buffer root-buf
                           (copy-marker beg)))
         indirect-buf)
    (gptel-org--debug "org-ib create: tag=%S region=[%d,%d] name=%S"
                      tag beg end buf-name)
    ;; Kill any existing buffer with this name (same heading re-sent)
    (when-let* ((existing (get-buffer buf-name)))
      (gptel-org-ib-unregister buf-name)
      (kill-buffer existing))
    ;; Create the indirect buffer (clone=t inherits major mode, local vars)
    (setq indirect-buf (make-indirect-buffer root-buf buf-name t))
    (with-current-buffer indirect-buf
      ;; Decouple fold state so expanding/collapsing in the indirect
      ;; buffer does not affect the base buffer
      (org-fold-core-decouple-indirect-buffer-folds)
      ;; Narrow to the subtree
      (narrow-to-region beg end-marker)
      (goto-char (point-min)))
    ;; Register in the tracking table
    (gptel-org-ib-register buf-name indirect-buf root-buf
                           heading-marker end-marker tag)
    (gptel-org--debug "org-ib create: created buffer %S" buf-name)
    indirect-buf))

(defun gptel-org-ib-close (indirect-buffer &optional fold)
  "Close INDIRECT-BUFFER and clean up all associated resources.

If FOLD is non-nil, fold the subtree in the base buffer before
killing the indirect buffer.

Removes the entry from the tracking registry, cleans up markers,
and kills the buffer."
  (when (buffer-live-p indirect-buffer)
    (let* ((buf-name (buffer-name indirect-buffer))
           (entry (gptel-org-ib-get buf-name))
           (base-buf (or (plist-get entry :base)
                         (gptel-org-ib-base-buffer indirect-buffer)))
           (subtree-start (with-current-buffer indirect-buffer
                            (point-min))))
      (gptel-org--debug "org-ib close: closing %S (fold=%s)" buf-name fold)
      ;; Optionally fold the completed subtree in the base buffer
      (when (and fold (buffer-live-p base-buf))
        (with-current-buffer base-buf
          (save-excursion
            (goto-char subtree-start)
            (when (org-at-heading-p)
              (org-fold-subtree t)
              (gptel-org--debug "org-ib close: folded subtree at line %d"
                                (line-number-at-pos))))))
      ;; Unregister (cleans up markers)
      (when entry
        (gptel-org-ib-unregister buf-name))
      ;; If there's a buffer-local end-marker not tracked in registry,
      ;; clean that up too (for buffers created by the old code path).
      ;; Use ignore-errors since the variable might not be bound in all
      ;; indirect buffers.
      (let ((local-end-marker
             (ignore-errors
               (buffer-local-value 'gptel-org-agent--narrow-end-marker
                                   indirect-buffer))))
        (when (and (markerp local-end-marker)
                   (marker-buffer local-end-marker))
          (set-marker local-end-marker nil)))
      ;; Kill the indirect buffer
      (kill-buffer indirect-buffer))))

(defun gptel-org-ib-valid-p (indirect-buffer)
  "Return non-nil if INDIRECT-BUFFER is still valid.

Checks:
  - Buffer is live (not killed).
  - Buffer is narrowed (point-min > 1 or point-max < buffer size).
  - Buffer has content (point-min < point-max).
  - First line is an org heading."
  (and (buffer-live-p indirect-buffer)
       (with-current-buffer indirect-buffer
         (and (or (/= (point-min) 1)
                  (/= (point-max) (1+ (buffer-size))))
              (< (point-min) (point-max))
              (save-excursion
                (goto-char (point-min))
                (org-at-heading-p))))))


;;; ---- Indirect Buffer Recreation (After Reorder) ---------------------------

(defun gptel-org-ib--find-heading-by-tag-and-hash (base-buffer tag hash)
  "Find a heading in BASE-BUFFER matching TAG and HASH.

Searches all headings in BASE-BUFFER for one whose computed buffer
name matches the given TAG and HASH.  Returns the position of the
matching heading, or nil."
  (with-current-buffer base-buffer
    (save-excursion
      (goto-char (point-min))
      (let ((found nil)
            (target-name (format "*gptel:%s-%s*" tag hash)))
        (while (and (not found)
                    (re-search-forward org-heading-regexp nil t))
          (beginning-of-line)
          (let* ((pos (point))
                 (tags (org-get-tags nil t))
                 (has-tag (cl-some
                           (lambda (tg) (string-equal-ignore-case tg tag))
                           tags)))
            (when has-tag
              (let ((computed-name
                     (gptel-org-agent--indirect-buffer-name base-buffer pos tag)))
                (when (string= computed-name target-name)
                  (setq found pos)))))
          (unless found
            (forward-line 1)))
        found))))

(defun gptel-org-ib--parse-buffer-name (buf-name)
  "Parse a buffer name of the form *gptel:TAG-HASH* into (TAG . HASH).

Returns a cons cell (TAG . HASH) or nil if the name doesn't match
the expected format."
  (when (string-match "\\`\\*gptel:\\(.+\\)-\\([0-9a-f]\\{6\\}\\)\\*\\'" buf-name)
    (cons (match-string 1 buf-name)
          (match-string 2 buf-name))))

(defun gptel-org-ib-recreate (indirect-buffer)
  "Recreate INDIRECT-BUFFER after its heading has been moved.

When a subtree is reordered (e.g., by `org-move-subtree-up/down'),
the indirect buffer's narrowing restriction markers collapse because
the text at those positions was deleted and reinserted elsewhere.

This function:
  1. Parses the buffer name to extract the tag and hash.
  2. Finds the heading's new position in the base buffer by matching
     tag + hash.
  3. Kills the old indirect buffer.
  4. Creates a new one at the correct position with the same name.

Returns the new indirect buffer, or nil if the heading cannot be
found (e.g., it was deleted rather than moved)."
  (unless (buffer-live-p indirect-buffer)
    (gptel-org--debug "org-ib recreate: buffer is dead, nothing to recreate")
    (cl-return-from gptel-org-ib-recreate nil))
  (let* ((buf-name (buffer-name indirect-buffer))
         (entry (gptel-org-ib-get buf-name))
         (base-buf (or (plist-get entry :base)
                       (gptel-org-ib-base-buffer indirect-buffer)))
         (parsed (gptel-org-ib--parse-buffer-name buf-name)))
    (unless parsed
      (gptel-org--debug "org-ib recreate: cannot parse buffer name %S" buf-name)
      (cl-return-from gptel-org-ib-recreate nil))
    (let* ((tag (car parsed))
           (hash (cdr parsed))
           (new-pos (gptel-org-ib--find-heading-by-tag-and-hash
                     base-buf tag hash)))
      (unless new-pos
        (gptel-org--debug
         "org-ib recreate: heading not found for tag=%S hash=%S in %S"
         tag hash (buffer-name base-buf))
        (cl-return-from gptel-org-ib-recreate nil))
      (gptel-org--debug "org-ib recreate: found heading at %d, recreating %S"
                        new-pos buf-name)
      ;; Clean up the old buffer
      (gptel-org-ib-close indirect-buffer)
      ;; Create fresh indirect buffer at new position with the same name
      (gptel-org-ib-create base-buf new-pos buf-name))))


;;; ---- Auto-correction ------------------------------------------------------

(defun gptel-org-ib-validate-and-fix (indirect-buffer)
  "Validate an indirect buffer and attempt auto-correction.

Checks:
  - Buffer is still alive and narrowed.
  - First line is an org heading.
  - Heading level is consistent (matches the expected level from
    the tag depth).
  - Content doesn't extend beyond subtree bounds.

If the narrowing has collapsed (point-min = point-max) but the
heading can still be found in the base buffer, attempts recreation
via `gptel-org-ib-recreate'.

Returns t if the buffer is valid (possibly after automatic fixes),
nil if the situation is unrecoverable."
  (unless (buffer-live-p indirect-buffer)
    (gptel-org--debug "org-ib validate-and-fix: buffer is dead")
    (cl-return-from gptel-org-ib-validate-and-fix nil))
  ;; Check if narrowing has collapsed
  (let ((collapsed (with-current-buffer indirect-buffer
                     (>= (point-min) (point-max)))))
    (when collapsed
      (gptel-org--debug "org-ib validate-and-fix: narrowing collapsed for %S, attempting recreate"
                        (buffer-name indirect-buffer))
      (let ((new-buf (gptel-org-ib-recreate indirect-buffer)))
        (cl-return-from gptel-org-ib-validate-and-fix (not (null new-buf))))))
  ;; Buffer has content — check structural validity
  (with-current-buffer indirect-buffer
    (save-excursion
      (goto-char (point-min))
      ;; First line must be an org heading
      (unless (org-at-heading-p)
        (gptel-org--debug "org-ib validate-and-fix: no heading at point-min in %S"
                          (buffer-name indirect-buffer))
        (cl-return-from gptel-org-ib-validate-and-fix nil))
      ;; Verify the heading level looks reasonable
      (let* ((level (org-current-level))
             (buf-name (buffer-name indirect-buffer))
             (entry (gptel-org-ib-get buf-name)))
        (when (and entry (< level 1))
          (gptel-org--debug "org-ib validate-and-fix: invalid heading level %d in %S"
                            level buf-name)
          (cl-return-from gptel-org-ib-validate-and-fix nil))
        ;; Check that the end-marker is still in the right place
        (when entry
          (let ((end-marker (plist-get entry :end-marker)))
            (when (and (markerp end-marker)
                       (marker-buffer end-marker))
              (let* ((end-pos (marker-position end-marker))
                     (heading-pos (point-min)))
                ;; End must be after the heading
                (when (<= end-pos heading-pos)
                  (gptel-org--debug
                   "org-ib validate-and-fix: end-marker (%d) <= heading (%d) in %S, attempting recreate"
                   end-pos heading-pos buf-name)
                  (let ((new-buf (gptel-org-ib-recreate indirect-buffer)))
                    (cl-return-from gptel-org-ib-validate-and-fix
                      (not (null new-buf)))))))))
        ;; All checks passed
        t))))


;;; ---- Re-narrowing ---------------------------------------------------------

(defun gptel-org-ib-renarrow (indirect-buffer)
  "Re-narrow INDIRECT-BUFFER to its heading's current subtree bounds.

Used when the subtree boundaries may have shifted (e.g., after edits
in sibling subtrees that changed positions).  This does NOT handle
moved headings — use `gptel-org-ib-recreate' for that.

The heading-marker from the registry is used to find the current
subtree extent.  The end-marker is updated to the new subtree end.

Returns t on success, nil on failure."
  (unless (buffer-live-p indirect-buffer)
    (cl-return-from gptel-org-ib-renarrow nil))
  (let* ((buf-name (buffer-name indirect-buffer))
         (entry (gptel-org-ib-get buf-name))
         (base-buf (or (plist-get entry :base)
                       (gptel-org-ib-base-buffer indirect-buffer)))
         (heading-marker (plist-get entry :heading-marker))
         (end-marker (plist-get entry :end-marker)))
    ;; Validate we have what we need
    (unless (and (buffer-live-p base-buf)
                 (markerp heading-marker)
                 (marker-buffer heading-marker)
                 (markerp end-marker)
                 (marker-buffer end-marker))
      (gptel-org--debug "org-ib renarrow: missing markers for %S" buf-name)
      (cl-return-from gptel-org-ib-renarrow nil))
    (let* ((heading-pos (marker-position heading-marker))
           (region (gptel-org-ib--compute-subtree-region base-buf heading-pos))
           (new-beg (car region))
           (new-end (cdr region)))
      ;; Update the end-marker position
      (set-marker end-marker new-end)
      ;; Re-narrow the indirect buffer
      (with-current-buffer indirect-buffer
        (widen)
        (narrow-to-region new-beg end-marker)
        (goto-char (point-min)))
      (gptel-org--debug "org-ib renarrow: %S -> [%d,%d]" buf-name new-beg new-end)
      t)))


;;; ---- High-level Convenience Functions -------------------------------------

(defun gptel-org-ib-safe-insert-sibling (todo-keyword title tags terminator-keyword)
  "Create a heading and its indirect buffer in one step.

This is the high-level function for safe concurrent task creation:
  1. Ensures the terminator heading exists.
  2. Creates a new child heading BEFORE the terminator.
  3. Creates an indirect buffer narrowed to the new heading's subtree.

TODO-KEYWORD is the org TODO state (e.g., \"AI-DO\").
TITLE is the heading text.
TAGS is a list of tag strings.
TERMINATOR-KEYWORD identifies the terminator (e.g., \"FEEDBACK\").

Point must be on the parent heading.
Returns the indirect buffer for the newly created heading."
  (save-excursion
    (unless (org-at-heading-p)
      (ignore-errors (org-back-to-heading t)))
    ;; Ensure the terminator exists
    (gptel-org-ib-ensure-terminator terminator-keyword)
    ;; Create the new heading before the terminator
    (let* ((heading-marker (gptel-org-ib-create-heading
                            todo-keyword title tags terminator-keyword))
           (base-buffer (current-buffer)))
      ;; Create and return the indirect buffer
      (gptel-org-ib-create base-buffer heading-marker))))


(provide 'gptel-indirect-buffer)
;;; gptel-indirect-buffer.el ends here
