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
;; AI-DOING, RESULTS for AI-DO).  New sibling tasks are inserted BEFORE
;; the terminator so existing indirect buffers are never disturbed.
;;
;; Example org structure:
;;
;;   **** AI-DOING Study feasibility ...  :main@agent:   <- IB 1
;;   ***** AI-DO Subtask 1                               <- IB 2
;;   ****** RESULTS                                      <- Terminator for Subtask 1
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
(declare-function gptel-org-agent--agent-tag-p "gptel-org-agent" (tag))

;; External function from gptel.el (main file) - may not be loaded yet.
(declare-function gptel-abort "gptel" (buf))

;; External variables
(defvar gptel-org-debug)
(defvar gptel-org-agent--narrow-end-marker)


;;; ---- Canonical Node Struct ------------------------------------------------

(cl-defstruct gptel-org-ib-node
  "Canonical record for a tracked gptel indirect-buffer node.

A node represents one AI task's indirect buffer along with the
information needed to re-narrow, re-create, and reason about its
position within an org tree.

Slots:
  BUFFER         The indirect buffer object (or nil if not yet
                 created / already killed).
  BASE           The base (root) org buffer that BUFFER is indirect of.
                 For the tree root node this is the org file buffer
                 itself.
  PARENT         Another `gptel-org-ib-node' that owns this node in
                 the task hierarchy, or nil.  A nil PARENT means the
                 parent is the BASE buffer directly (i.e. this node
                 is a top-level task in the file).
  CHILDREN       List of child `gptel-org-ib-node' instances whose
                 PARENT is this node.  May be nil.
  HEADING-MARKER Marker at the start of this node's org heading in
                 BASE.  Shared with BUFFER because indirect buffers
                 share markers with their base.
  END-MARKER     Marker at the end of this node's subtree in BASE,
                 typically created with insertion-type t so it
                 auto-expands as content is appended.
  TAG            Agent tag string for this node (e.g. \"main@agent\"),
                 used to build the indirect buffer name and to
                 recognize agent-owned headings.
  HASH           Opaque content/identity hash for integrity checks
                 and fast equality comparisons; may be nil when not
                 yet computed.

This struct is the target representation for the indirect-buffer
canonical refactor.  During the migration it coexists with the
plist-based `gptel-org-ib--registry' entries; later phases will
replace the plists with these nodes."
  buffer base parent children heading-marker end-marker tag hash)


;;; ---- Tracking Registry ----------------------------------------------------

(defvar gptel-org-ib--registry (make-hash-table :test 'equal)
  "Hash table mapping buffer-name -> `gptel-org-ib-node' struct.

Each entry is a `gptel-org-ib-node' carrying the node's indirect
buffer, base buffer, parent/children links in the task hierarchy,
heading and end markers in the base buffer, and agent tag.  See the
`gptel-org-ib-node' defstruct for slot documentation.

The public `gptel-org-ib-get' accessor returns a plist-shaped view
for backward compatibility; internal callers that need the node
should use `gptel-org-ib--get-node'.")

(defun gptel-org-ib-fatal (fmt &rest args)
  "Signal a fatal indirect-buffer inconsistency.

Log FMT/ARGS to the gptel log, abort the active FSM in the current
base buffer if any, then signal `user-error' with the same message.

Callers must not wrap this in `ignore-errors' or `condition-case';
IB invariants are hard preconditions, not recoverable conditions.

The internal `ignore-errors'/`condition-case' here guard only the
abort-cleanup path: the function must always reach the final
`user-error' signal regardless of abort outcome."
  (let ((msg (apply #'format fmt args)))
    (apply #'gptel-org--debug (concat "org-ib FATAL: " fmt) args)
    (when (fboundp 'gptel-abort)
      (let ((base (ignore-errors
                    (gptel-org-ib-base-buffer (current-buffer)))))
        (when (buffer-live-p base)
          (condition-case _err
              (with-current-buffer base (gptel-abort base))
            (error nil)))))
    (user-error "%s" msg)))

(defun gptel-org-ib-register (name indirect-buffer base-buffer
                                   heading-marker end-marker tag)
  "Register an indirect buffer in the tracking registry.

NAME is the buffer name (string) used as the registry key.
INDIRECT-BUFFER is the indirect buffer object.
BASE-BUFFER is the root org buffer.
HEADING-MARKER is a marker at the heading start in the base buffer.
END-MARKER is the end-of-subtree marker (insertion-type t).
TAG is the agent tag string.

Constructs a fresh `gptel-org-ib-node' with no parent/children links
and stores it under NAME.  Callers that need parent/children links
should construct and register the node directly (see
`gptel-org-ib-create')."
  (let ((node (make-gptel-org-ib-node
               :buffer indirect-buffer
               :base base-buffer
               :heading-marker heading-marker
               :end-marker end-marker
               :tag tag)))
    (puthash name node gptel-org-ib--registry)
    (gptel-org--debug "org-ib register: %S (tag=%S)" name tag)))

(defun gptel-org-ib-unregister (name)
  "Remove an indirect buffer entry from the registry and clean up markers.

NAME is the buffer name (string).  Clears heading-marker and
end-marker (sets them to nil), splices this node out of its parent's
children list (if any), and removes the entry from the registry."
  (when-let* ((node (gethash name gptel-org-ib--registry)))
    (when-let* ((parent (gptel-org-ib-node-parent node)))
      (setf (gptel-org-ib-node-children parent)
            (delq node (gptel-org-ib-node-children parent))))
    (let ((hm (gptel-org-ib-node-heading-marker node))
          (em (gptel-org-ib-node-end-marker node)))
      (when (markerp hm) (set-marker hm nil))
      (when (markerp em) (set-marker em nil)))
    (remhash name gptel-org-ib--registry)
    (gptel-org--debug "org-ib unregister: %S" name)))

(defun gptel-org-ib-get (name)
  "Get the registry entry for NAME as a plist.

Returns a plist with keys :buffer :base :heading-marker :end-marker
:tag constructed from the underlying `gptel-org-ib-node', or nil if
NAME is not registered.

This is a backward-compatibility shim.  New code should use
`gptel-org-ib--get-node' to access the node struct directly."
  (when-let* ((node (gethash name gptel-org-ib--registry)))
    (list :buffer         (gptel-org-ib-node-buffer node)
          :base           (gptel-org-ib-node-base node)
          :heading-marker (gptel-org-ib-node-heading-marker node)
          :end-marker     (gptel-org-ib-node-end-marker node)
          :tag            (gptel-org-ib-node-tag node))))

(defun gptel-org-ib--get-node (name)
  "Return the `gptel-org-ib-node' registered under NAME, or nil."
  (gethash name gptel-org-ib--registry))

(defun gptel-org-ib-all-for-base (base-buffer)
  "Return list of all registered indirect buffer names for BASE-BUFFER."
  (let (result)
    (maphash (lambda (name node)
               (when (eq (gptel-org-ib-node-base node) base-buffer)
                 (push name result)))
             gptel-org-ib--registry)
    (nreverse result)))

(defun gptel-org-ib-cleanup-dead ()
  "Remove registry entries for dead (killed) buffers.

Iterates the registry and unregisters any entry whose buffer is no
longer live.  Returns the number of entries removed."
  (let ((dead-names nil))
    (maphash (lambda (name node)
               (unless (buffer-live-p (gptel-org-ib-node-buffer node))
                 (push name dead-names)))
             gptel-org-ib--registry)
    (dolist (name dead-names)
      (gptel-org-ib-unregister name))
    (when dead-names
      (gptel-org--debug "org-ib cleanup-dead: removed %d entries" (length dead-names)))
    (length dead-names)))


;;; ---- Canonical Resolvers -------------------------------------------------

(defun gptel-org-ib-parent (ib)
  "Return the logical parent of indirect buffer IB.

If IB's node has a parent node, return that node.  Otherwise, return
the base buffer object (IB is a top-level task whose logical parent is
the base buffer itself).

Signals a `user-error' if IB is not a registered gptel indirect buffer,
since the canonical resolver API forbids silent fallbacks.

IB may be a buffer object or a buffer name string."
  (let* ((name (cond ((bufferp ib) (buffer-name ib))
                     ((stringp ib) ib)
                     (t (user-error "gptel-org-ib-parent: invalid IB: %S" ib))))
         (node (gptel-org-ib--get-node name)))
    (unless node
      (user-error "gptel-org-ib-parent: not a registered gptel IB: %s" name))
    (or (gptel-org-ib-node-parent node)
        (gptel-org-ib-node-base node))))

(defun gptel-org-ib-children (ib)
  "Return the list of child `gptel-org-ib-node' instances of IB.

Returns the registered children of IB's node directly — a list of
nodes whose parent is IB, or nil when IB has no children.

Signals a `user-error' if IB is not a registered gptel indirect buffer,
since the canonical resolver API forbids silent fallbacks.

IB may be a buffer object or a buffer name string."
  (let* ((name (cond ((bufferp ib) (buffer-name ib))
                     ((stringp ib) ib)
                     (t (user-error "gptel-org-ib-children: invalid IB: %S" ib))))
         (node (gptel-org-ib--get-node name)))
    (unless node
      (user-error "gptel-org-ib-children: not a registered gptel IB: %s" name))
    (gptel-org-ib-node-children node)))

(defun gptel-org-ib-base (ib)
  "Return the base buffer of indirect buffer IB.

Walks the registry's parent chain — never calls `buffer-base-buffer'
directly.  Starting from IB's node, follows each node's `:parent' link
until a nil parent is found; the resulting node's `:base' slot is the
base buffer object.  For nested IB-of-IB, this yields the same base
buffer for every node in the chain (the parent pointer chain always
terminates at a top-level node whose `:base' is the real file buffer).

Includes a cycle guard that signals `error' if the parent chain
exceeds a fixed iteration limit, defending against corrupt registry
state that would otherwise loop forever.

Signals a `user-error' if IB is not a registered gptel indirect buffer,
since the canonical resolver API forbids silent fallbacks.

IB may be a buffer object or a buffer name string."
  (let* ((name (cond ((bufferp ib) (buffer-name ib))
                     ((stringp ib) ib)
                     (t (user-error "gptel-org-ib-base: invalid IB: %S" ib))))
         (node (gptel-org-ib--get-node name)))
    (unless node
      (user-error "gptel-org-ib-base: not a registered gptel IB: %s" name))
    ;; Walk node.parent chain; cap iterations to detect cycles.
    (let ((cur node)
          (guard 0)
          (limit 1024))
      (while (let ((parent (gptel-org-ib-node-parent cur)))
               (and (gptel-org-ib-node-p parent)
                    (progn (setq cur parent) t)))
        (setq guard (1+ guard))
        (when (> guard limit)
          (error "gptel-org-ib-base: parent chain exceeds %d nodes \
(corrupt registry or cycle at %s)" limit name)))
      (gptel-org-ib-node-base cur))))

(defun gptel-org-ib-bounds (ib)
  "Return the bounds of indirect buffer IB as a cons (START . END).

START and END are integer positions in IB's parent buffer — the
buffer returned by `gptel-org-ib-parent' — taken directly from the
node's `:heading-marker' and `:end-marker' slots.  This resolver
never consults IB's narrowing (`point-min'/`point-max') nor calls
`buffer-base-buffer'; the registry markers are the single source of
truth for the subtree region.

For a top-level IB the parent buffer is the base file buffer, so
the positions are absolute positions in that file.  For a nested
IB-of-IB the positions are still valid in the shared base buffer
(markers are shared across indirect buffers), which is the same
buffer that holds the parent IB's text — see `gptel-org-ib-parent'.

Signals a `user-error' if IB is not a registered gptel indirect
buffer, or if either marker is missing or has been cleared (for
example, after `gptel-org-ib-unregister' ran).  The canonical
resolver API forbids silent fallbacks, so callers must handle the
error rather than receive stale bounds.

IB may be a buffer object or a buffer name string."
  (let* ((name (cond ((bufferp ib) (buffer-name ib))
                     ((stringp ib) ib)
                     (t (user-error "gptel-org-ib-bounds: invalid IB: %S" ib))))
         (node (gptel-org-ib--get-node name)))
    (unless node
      (user-error "gptel-org-ib-bounds: not a registered gptel IB: %s" name))
    (let ((hm (gptel-org-ib-node-heading-marker node))
          (em (gptel-org-ib-node-end-marker node)))
      (unless (and (markerp hm) (marker-position hm))
        (user-error "gptel-org-ib-bounds: missing heading-marker for %s" name))
      (unless (and (markerp em) (marker-position em))
        (user-error "gptel-org-ib-bounds: missing end-marker for %s" name))
      (cons (marker-position hm) (marker-position em)))))

(defun gptel-org-ib-point (ib)
  "Return the current point position as observed inside IB.

Each indirect buffer maintains its own `point' independent of its
base buffer (though text and markers are shared).  This resolver
returns that per-IB point by switching into IB's buffer and calling
`point' — it never consults the registry's markers, and it never
falls back to the base buffer's point.

The returned integer always satisfies `point-min' ≤ value ≤ `point-max'
of IB (i.e. it respects IB's current narrowing), because `point' in
Emacs is always clamped to the accessible region of the current
buffer.  Callers may rely on this invariant.

Signals a `user-error' if IB is not a registered gptel indirect
buffer, or if its node's `:buffer' slot is missing or not live.
The canonical resolver API forbids silent fallbacks, so callers
must handle the error rather than receive a stale or nil position.

IB may be a buffer object or a buffer name string."
  (let* ((name (cond ((bufferp ib) (buffer-name ib))
                     ((stringp ib) ib)
                     (t (user-error "gptel-org-ib-point: invalid IB: %S" ib))))
         (node (gptel-org-ib--get-node name)))
    (unless node
      (user-error "gptel-org-ib-point: not a registered gptel IB: %s" name))
    (let ((buf (gptel-org-ib-node-buffer node)))
      (unless (buffer-live-p buf)
        (user-error "gptel-org-ib-point: IB buffer is not live: %s" name))
      (with-current-buffer buf (point)))))

(defun gptel-org-ib-absolute-position (ib)
  "Return IB's current point as an absolute position in its base buffer.

Returns a cons (BUFFER . POS):
  BUFFER  The base (file) buffer — the terminal buffer found by
          walking IB's logical parent chain through the registry.
          Never retrieved via `buffer-base-buffer'.
  POS     IB's current point expressed as an absolute position in
          BUFFER.

In this codebase indirect-of-indirect buffers are flattened at the
Emacs level: every IB in a chain shares the same base file buffer,
and markers (including `point') are expressed directly in that
shared base buffer.  IB's point is therefore already an absolute
position in the base buffer; this resolver's job is to return that
position together with the base buffer that the logical parent
chain terminates at, without ever consulting `buffer-base-buffer'.

The walk follows `gptel-org-ib-node-parent' links until a nil parent
is found, mirroring `gptel-org-ib-base'.  It includes a cycle guard
that signals `error' if the parent chain exceeds a fixed iteration
limit, defending against corrupt registry state.

Signals a `user-error' if IB is not a registered gptel indirect
buffer, or if its node's `:buffer' slot is missing or not live.
The canonical resolver API forbids silent fallbacks.

IB may be a buffer object or a buffer name string."
  (let* ((name (cond ((bufferp ib) (buffer-name ib))
                     ((stringp ib) ib)
                     (t (user-error
                         "gptel-org-ib-absolute-position: invalid IB: %S"
                         ib))))
         (node (gptel-org-ib--get-node name)))
    (unless node
      (user-error
       "gptel-org-ib-absolute-position: not a registered gptel IB: %s"
       name))
    (let ((buf (gptel-org-ib-node-buffer node)))
      (unless (buffer-live-p buf)
        (user-error
         "gptel-org-ib-absolute-position: IB buffer is not live: %s"
         name))
      ;; Walk node.parent chain; cap iterations to detect cycles.
      ;; The terminal node's :base slot is the base file buffer.
      (let ((cur node)
            (guard 0)
            (limit 1024))
        (while (let ((parent (gptel-org-ib-node-parent cur)))
                 (and (gptel-org-ib-node-p parent)
                      (progn (setq cur parent) t)))
          (setq guard (1+ guard))
          (when (> guard limit)
            (error "gptel-org-ib-absolute-position: parent chain exceeds \
%d nodes (corrupt registry or cycle at %s)" limit name)))
        (cons (gptel-org-ib-node-base cur)
              (with-current-buffer buf (point)))))))


(defun gptel-org-ib-registered-p (buf)
  "Return non-nil if BUF is a registered gptel indirect buffer.

BUF may be a buffer object or a buffer-name string.  The check
consults only the IB registry — it never calls `buffer-base-buffer'
or otherwise inspects Emacs's low-level indirect-buffer relationship.
This predicate is the canonical way for gptel code to ask \"am I
inside a gptel-managed indirect buffer?\".

Returns nil for buffer objects and names that are not in the registry,
including live buffers that happen to be Emacs indirect buffers but
were not created by `gptel-org-ib-create'."
  (let ((name (cond ((bufferp buf) (and (buffer-live-p buf) (buffer-name buf)))
                    ((stringp buf) buf)
                    (t nil))))
    (and name (gptel-org-ib--get-node name) t)))

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


;;; ---- Heading Navigation ---------------------------------------------------

(declare-function gptel-org-agent--agent-tag-p "gptel-org-agent")

(defun gptel-org-ib-find-user-task-heading ()
  "Navigate from current position up to the user-level task heading.

Walk up the org heading hierarchy past all agent headings (those with
tags matching *@agent) until reaching a heading with no agent tag.
This is the user-level task heading that owns the agent subtree chain.

Point must be on an org heading in the base buffer (or at a position
returned by `point-min' in an agent indirect buffer, resolved to the
base buffer).

Returns the position of the user-level heading, or nil if not found.
Point is moved to the user-level heading on success."
  (when (org-at-heading-p)
    ;; Walk up past all agent headings
    (while (and (> (org-current-level) 1)
                (cl-some #'gptel-org-agent--agent-tag-p
                         (org-get-tags nil t))
                (org-up-heading-safe)))
    ;; Verify we landed on a non-agent heading
    (unless (cl-some #'gptel-org-agent--agent-tag-p
                     (org-get-tags nil t))
      (point))))

(defun gptel-org-ib-resolve-agent-heading (indirect-buffer)
  "Return the position of the agent heading for INDIRECT-BUFFER.

In an indirect buffer, `point-min' IS the agent heading.  This function
resolves that position in the base buffer context.

INDIRECT-BUFFER must be an actual indirect buffer.  Returns the
position, or nil if not found or if called with a non-indirect buffer."
  (if (buffer-base-buffer indirect-buffer)
      ;; Indirect buffer: point-min is the agent heading
      (with-current-buffer indirect-buffer
        (save-excursion
          (goto-char (point-min))
          (when (org-at-heading-p) (point))))
    ;; Not an indirect buffer — caller error
    (gptel-org--debug
     "org-ib resolve-agent-heading: called with non-indirect buffer %S"
     (buffer-name indirect-buffer))
    nil))


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
      (gptel-org-ib-fatal
       "find-terminator: point %d not at heading in buffer %s"
       (point) (buffer-name)))
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

(defun gptel-org-ib-streaming-marker (&optional terminator-keyword)
  "Return a marker safe for FSM streaming into the current indirect buffer.

If TERMINATOR-KEYWORD names a child terminator heading of the
current subtree (e.g., \"FEEDBACK\" or \"RESULTS\"), position the
marker at the beginning of that terminator's heading line with
insertion-type nil.  Text inserted at this marker is placed BEFORE
the terminator, and the marker stays pinned to the terminator line
as content accumulates.

If no terminator exists, place the marker at `point-max' with
insertion-type t (advances with appended text).

Typically called with the current buffer narrowed to an agent or
task subtree, point on the narrowed-to heading.  When point is not
on a heading, only the `point-max' fallback is used."
  (let ((term-pos (and terminator-keyword
                       (save-excursion
                         (goto-char (point-min))
                         (when (org-at-heading-p)
                           (gptel-org-ib-find-terminator terminator-keyword))))))
    (if term-pos
        (let ((m (make-marker)))
          (set-marker m term-pos)
          (set-marker-insertion-type m nil)
          m)
      (save-excursion
        (goto-char (point-max))
        (skip-chars-backward "\n")
        (end-of-line)
        (let ((m (point-marker)))
          (set-marker-insertion-type m t)
          m)))))

(defun gptel-org-ib-ensure-sibling-terminator (terminator-keyword level)
  "Ensure a sibling terminator heading exists at LEVEL after current subtree.

Unlike `gptel-org-ib-ensure-terminator' which manages a CHILD
terminator heading, this variant manages a SIBLING terminator at
the same LEVEL as the heading at point.  This is used for
sibling-level FEEDBACK markers (e.g., when the agent heading and
FEEDBACK heading share a parent).

TERMINATOR-KEYWORD is the heading text (e.g., \"FEEDBACK\").
LEVEL is the heading level (number of stars) for the terminator.

Point must be on a heading.  Searches forward from the end of the
current subtree for an existing sibling at LEVEL with matching
heading keyword/title before the next heading of a shallower level
or end of accessible portion of buffer.  If found, returns a marker
to it.  Otherwise inserts a new one at the end of the current
subtree and returns a marker to it.

Returns a marker to the terminator heading."
  (save-excursion
    (unless (org-at-heading-p)
      (gptel-org-ib-fatal
       "ensure-sibling-terminator: point %d not at heading in buffer %s"
       (point) (buffer-name)))
    (let* ((inhibit-read-only t)
           (_current-level (org-current-level))
           (subtree-end (save-excursion
                          (org-end-of-subtree t t)
                          (point)))
           (regexp (gptel-org-ib--terminator-regexp terminator-keyword level))
           (found nil))
      (save-excursion
        (goto-char subtree-end)
        ;; Search forward for sibling terminator.  Stop at a heading of
        ;; lower (shallower) level than LEVEL, which indicates we've
        ;; left the parent's scope.
        (let ((stop-regexp (format "^\\*\\{1,%d\\} " (1- level)))
              (limit nil))
          (save-excursion
            (when (re-search-forward stop-regexp nil t)
              (setq limit (match-beginning 0))))
          (when (re-search-forward regexp limit t)
            (beginning-of-line)
            (when (= (org-current-level) level)
              (setq found (point))))))
      (if found
          (let ((m (make-marker)))
            (set-marker m found)
            m)
        ;; Not found: insert new sibling at end of subtree
        (goto-char subtree-end)
        (unless (bolp) (insert "\n"))
        (let* ((stars (make-string level ?*))
               (heading-text (format "%s %s" stars terminator-keyword))
               (start (point)))
          (insert heading-text "\n")
          (gptel-org--debug
           "org-ib ensure-sibling-terminator: %S at level %d, line %d"
           terminator-keyword level (line-number-at-pos start))
          (let ((m (make-marker)))
            (set-marker m start)
            m))))))

(defun gptel-org-ib-create-terminator (heading-keyword &optional level)
  "Create a terminator child heading under the current heading.

HEADING-KEYWORD is the heading text (e.g., \"FEEDBACK\", \"RESULTS\").
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
      (gptel-org-ib-fatal
       "ensure-terminator: point %d not at heading in buffer %s"
       (point) (buffer-name)))
    (let ((existing (gptel-org-ib-find-terminator terminator-keyword)))
      (if existing
          (progn
            (gptel-org--debug "org-ib ensure-terminator: found existing %S at %d"
                              terminator-keyword existing)
            (copy-marker existing))
        (gptel-org-ib-create-terminator terminator-keyword)))))

(defun gptel-org-ib-remove-terminator (heading-keyword)
  "Remove the terminator child heading matching HEADING-KEYWORD.
Point must be on the parent heading.

HEADING-KEYWORD is the terminator state (e.g., \"TERMINE\").
Verifies the terminator body is empty before deletion; fatal if not.
Idempotent: if no matching terminator is found, does nothing.
Returns t if a terminator was removed, nil otherwise.

If multiple child headings match HEADING-KEYWORD (should not happen in
valid data), all are removed.

Before deleting a terminator, any registered indirect buffer whose
narrowing (`:end-marker' position) falls inside or past the terminator
heading's region is closed via `gptel-org-ib-close'.  If close fails
for any reason, the error is logged and the removal proceeds."
  (save-excursion
    (unless (org-at-heading-p)
      (gptel-org-ib-fatal
       "remove-terminator: point %d not at heading in buffer %s"
       (point) (buffer-name)))
    (let ((removed 0)
          (parent-pos (point))
          (parent-level (org-current-level))
          (base-buffer (current-buffer)))
      ;; Loop because there may (invalidly) be more than one terminator.
      ;; `find-terminator' returns the first match; after deleting, search again.
      (catch 'done
        (while t
          (save-excursion
            (goto-char parent-pos)
            (let ((term-pos (gptel-org-ib-find-terminator heading-keyword)))
              (unless term-pos
                (throw 'done nil))
              ;; Compute the region to delete: from the terminator's
              ;; line start to the next heading at same-or-shallower
              ;; level (or point-max).
              (goto-char term-pos)
              (beginning-of-line)
              (let* ((del-start (point))
                     (term-level (org-current-level))
                     (del-end
                      (save-excursion
                        (forward-line 1)
                        (let ((found nil)
                              (limit (save-excursion
                                       (goto-char parent-pos)
                                       (org-end-of-subtree t)
                                       (point))))
                          (while (and (not found)
                                      (< (point) limit)
                                      (re-search-forward org-heading-regexp limit t))
                            (beginning-of-line)
                            (if (<= (org-current-level) term-level)
                                (setq found (point))
                              (forward-line 1)))
                          (or found limit))))
                     ;; Body: everything between the terminator heading
                     ;; line (exclusive) and del-end.
                     (body-start (save-excursion
                                   (goto-char del-start)
                                   (forward-line 1)
                                   (point)))
                     (body (buffer-substring-no-properties
                            body-start del-end)))
                ;; Verify body is empty (only whitespace).
                (unless (string-match-p "\\`[ \t\n]*\\'" body)
                  (gptel-org-ib-fatal
                   "remove-terminator: %S has non-empty body (preview: %S)"
                   heading-keyword
                   (substring body 0 (min 80 (length body)))))
                ;; Before deleting: close any IBs whose narrowing
                ;; covers the terminator region.  Be conservative: log
                ;; and skip on error rather than crashing.
                (let ((victims nil))
                  (maphash
                   (lambda (name node)
                     (when (eq (gptel-org-ib-node-base node) base-buffer)
                       (let ((em (gptel-org-ib-node-end-marker node)))
                         (when (and (markerp em)
                                    (marker-position em)
                                    (>= (marker-position em) del-start))
                           (push (cons name node) victims)))))
                   gptel-org-ib--registry)
                  (dolist (v victims)
                    (let* ((name (car v))
                           (node (cdr v))
                           (ib (gptel-org-ib-node-buffer node)))
                      (gptel-org--debug
                       "org-ib remove-terminator: closing IB %S whose narrowing covers %S at %d"
                       name heading-keyword del-start)
                      (condition-case err
                          (when (buffer-live-p ib)
                            (gptel-org-ib-close ib nil))
                        (error
                         (gptel-org--debug
                          "org-ib remove-terminator: close %S failed: %S (continuing)"
                          name err))))))
                ;; Delete the terminator region.
                (let ((inhibit-read-only t))
                  (delete-region del-start del-end))
                (gptel-org--debug
                 "org-ib remove-terminator: removed %S at level %d (parent level %d)"
                 heading-keyword term-level parent-level)
                (setq removed (1+ removed)))))))
      (> removed 0))))


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


;;; ---- Buffer Name Generator ------------------------------------------------

(defun gptel-org-ib-compute-name (base-buffer heading-pos tag)
  "Compute a unique indirect buffer name for a subtree at HEADING-POS.

Uses TAG as the kind string and a short hash derived from the outline
path (parent headings) at HEADING-POS in BASE-BUFFER.  Format is
*gptel:TAG-HASH* where HASH is a 6-character hex string derived from
the base buffer name and full outline path.  The hash survives heading
reorder because it is path-based, not position-based."
  (let* ((path-str
          (with-current-buffer base-buffer
            (save-excursion
              (goto-char heading-pos)
              (let ((path (org-get-outline-path t)))
                (mapconcat #'identity path "/")))))
         (hash (substring
                (md5 (concat (buffer-name base-buffer) ":" path-str))
                0 6)))
    (format "*gptel:%s-%s*" tag hash)))


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
         ;; Capture the parent node BEFORE creating the indirect buffer.
         ;; `current-buffer' at call time is the enclosing indirect buffer
         ;; if registered; a nil parent-node means the parent is the base
         ;; buffer directly (top-level task in the file).
         (parent-node (gptel-org-ib--get-node
                       (buffer-name (current-buffer))))
         ;; Extract tag for naming (skip when name is provided —
         ;; avoids dependency on gptel-org-agent for non-agent callers)
         (resolved-tag (unless name
                         (gptel-org-ib--extract-tag-at root-buf pos)))
         ;; Compute buffer name
         (buf-name (or name
                       (gptel-org-ib-compute-name
                        root-buf pos resolved-tag)))
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
                      resolved-tag beg end buf-name)
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
    ;; Register in the tracking table with parent/children links wired up.
    ;; We construct the node directly (rather than calling
    ;; `gptel-org-ib-register') so that the parent slot is populated
    ;; atomically with insertion and we can push onto the parent's
    ;; children list.
    (let ((node (make-gptel-org-ib-node
                 :buffer indirect-buf
                 :base root-buf
                 :parent parent-node
                 :heading-marker heading-marker
                 :end-marker end-marker
                 :tag resolved-tag)))
      (when parent-node
        (push node (gptel-org-ib-node-children parent-node)))
      (puthash buf-name node gptel-org-ib--registry)
      (gptel-org--debug
       "org-ib register: %S (tag=%S parent=%S)"
       buf-name resolved-tag
       (when parent-node
         (buffer-name (gptel-org-ib-node-buffer parent-node)))))
    (gptel-org--debug "org-ib create: created buffer %S" buf-name)
    ;; Seed the universal TERMINE terminator child inside the freshly
    ;; narrowed subtree.  Per the design invariant in
    ;; `gptel-indirect-buffer-ai.org' (see *** TERMINE), every IB
    ;; created by gptel — agent, sub-agent (tool-call), tool body
    ;; (BASH/FILE-READ/...), REASONING, RESPOND — has TERMINE as its
    ;; last child.  Eager seeding pins
    ;; `gptel-org-ib-streaming-marker' BEFORE TERMINE from the moment
    ;; the IB exists, avoiding marker-drift-past-terminator bugs that
    ;; occur with lazy creation when the first streamed content
    ;; happens to be a tool call.
    (with-current-buffer indirect-buf
      (save-excursion
        (goto-char (point-min))
        (when (org-at-heading-p)
          (gptel-org-ib-ensure-terminator "TERMINE"))))
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
           (base-buf (plist-get entry :base))
           (subtree-start (with-current-buffer indirect-buffer
                            (point-min))))
      (unless entry
        (gptel-org--debug
         "org-ib close: WARNING buffer %S not in registry" buf-name))
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
      ;; Guard with an explicit liveness check: close is the graceful
      ;; teardown path and must remain idempotent even when the
      ;; indirect buffer has already been killed.  Registry-consistency
      ;; fatals live on the create/op paths, not here.
      (let ((local-end-marker
             (and (buffer-live-p indirect-buffer)
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
  - Buffer has content, i.e. narrowing has not collapsed
    (point-min < point-max).
  - First line is an org heading.

Note: we intentionally do NOT check whether narrowing spans less
than the entire buffer.  A subtree may legitimately cover the full
base buffer, and after `narrow-to-region' with identical bounds
`point-min'/`point-max' match the base-buffer extents — but the IB
is still functionally valid."
  (and (buffer-live-p indirect-buffer)
       (with-current-buffer indirect-buffer
         (and (< (point-min) (point-max))
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
                     (gptel-org-ib-compute-name base-buffer pos tag)))
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

(cl-defun gptel-org-ib-recreate (indirect-buffer)
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
         (base-buf (plist-get entry :base))
         (parsed (gptel-org-ib--parse-buffer-name buf-name)))
    (unless base-buf
      (gptel-org--debug
       "org-ib recreate: no registry entry for %S, cannot recreate" buf-name)
      (cl-return-from gptel-org-ib-recreate nil))
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

(cl-defun gptel-org-ib-validate-and-fix (indirect-buffer)
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
         (base-buf (plist-get entry :base))
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

(cl-defun gptel-org-ib-insert-child (parent todo-keyword title
                                            &key tags terminator-keyword name
                                            (create-indirect-buffer t))
  "Insert a child heading under PARENT and (optionally) create an IB for it.

This is the single, parent-aware entry point for creating a new
TODO-keyword heading inside an existing parent subtree.  Replaces
the implicit-=point=-based functions
`gptel-org-ib-safe-insert-sibling' and
`gptel-org-ib-create-tool-heading'.

PARENT identifies the parent heading and may be:
- A marker into the base buffer, pointing at the parent heading.
- A registered indirect buffer (gptel IB) whose narrowed subtree's
  root heading is the parent.  Resolved via `gptel-org-ib-get' /
  `gptel-org-ib-base'.
- The symbol `:point' meaning \"use current point in the current
  buffer\".  This branch is a transitional shim for callers that
  have not yet been migrated; it logs a debug message.

TODO-KEYWORD is the org TODO state for the new heading (e.g.
\"AI-DOING\").  TITLE is the heading text.

Keyword arguments:
- :TAGS                   list of tag strings, applied via
                          `org-set-tags'.
- :TERMINATOR-KEYWORD     when non-nil, ensure the parent has a
                          child heading named (\"TERMINE\",
                          \"FEEDBACK\", ...) and insert the new
                          heading immediately BEFORE it.  When nil,
                          insert at end of parent subtree.
                          Idempotent (existing terminator is
                          reused).
- :NAME                   optional override for the indirect
                          buffer name passed to
                          `gptel-org-ib-create'.
- :CREATE-INDIRECT-BUFFER non-nil (default t) creates an IB
                          narrowed to the new heading.  Pass nil
                          to suppress IB creation.

Behavior:
1. Resolve PARENT to (PARENT-CONTEXT-BUFFER . PARENT-MARKER) where
   PARENT-CONTEXT-BUFFER is the buffer used as `current-buffer'
   while editing (and during `gptel-org-ib-create' so the parent
   IB node is captured for the registry).
2. Inside PARENT-CONTEXT-BUFFER, with point at PARENT-MARKER,
   ensure the terminator (if requested) and insert the new heading
   immediately before it (or at end of subtree if no terminator).
   The universal newline guard `(unless (bolp) (insert \"\\n\"))'
   is applied before any heading text is inserted.
3. Optionally create an indirect buffer narrowed to the new
   heading.

`save-excursion' wraps every layer; callers do not observe point
movement.

Returns a plist of the form
  (:heading-marker MARKER :indirect-buffer IB-or-nil)
where MARKER points at the new heading line and IB-or-nil is the
freshly-created indirect buffer (or nil when CREATE-INDIRECT-BUFFER
is nil)."
  (let (parent-context-buffer parent-marker)
    (cond
     ((eq parent :point)
      (gptel-org--debug
       "org-ib insert-child: :point shim path in buffer %s at %d"
       (buffer-name) (point))
      (unless (org-at-heading-p)
        (gptel-org-ib-fatal
         "insert-child: :point but point %d not at heading in buffer %s"
         (point) (buffer-name)))
      (setq parent-context-buffer (current-buffer)
            parent-marker (point-marker)))
     ((markerp parent)
      (unless (and (marker-buffer parent) (marker-position parent))
        (gptel-org-ib-fatal
         "insert-child: parent marker has no live buffer/position"))
      (setq parent-context-buffer (marker-buffer parent)
            parent-marker parent))
     ((bufferp parent)
      (unless (buffer-live-p parent)
        (gptel-org-ib-fatal
         "insert-child: parent buffer %S is not live" parent))
      (let* ((info (gptel-org-ib-get (buffer-name parent))))
        (unless info
          (gptel-org-ib-fatal
           "insert-child: buffer %S is not a registered gptel IB"
           (buffer-name parent)))
        (let ((hm (plist-get info :heading-marker)))
          (unless (and (markerp hm) (marker-position hm))
            (gptel-org-ib-fatal
             "insert-child: IB %S has no live :heading-marker"
             (buffer-name parent)))
          (setq parent-context-buffer parent
                parent-marker hm))))
     (t
      (gptel-org-ib-fatal
       "insert-child: unsupported PARENT %S (must be :point, marker, or buffer)"
       parent)))
    (let (heading-marker indirect-buf)
      (with-current-buffer parent-context-buffer
        (save-excursion
          (goto-char (marker-position parent-marker))
          (unless (org-at-heading-p)
            (gptel-org-ib-fatal
             "insert-child: parent marker position %d not at heading in buffer %s"
             (point) (buffer-name)))
          ;; Ensure terminator (idempotent) on the parent.
          (when terminator-keyword
            (gptel-org-ib-ensure-terminator terminator-keyword))
          ;; Delegate the actual insertion to the internal helper,
          ;; which centralises the universal newline guard, level
          ;; computation, and trailing-blank-line handling.
          (setq heading-marker
                (gptel-org-ib-create-heading
                 todo-keyword title tags terminator-keyword)))
        (when create-indirect-buffer
          (save-excursion
            (setq indirect-buf
                  (gptel-org-ib-create parent-context-buffer
                                       heading-marker name)))))
      (list :heading-marker heading-marker
            :indirect-buffer indirect-buf))))

(defun gptel-org-ib-safe-insert-sibling (todo-keyword title tags terminator-keyword)
  "Create a heading and its indirect buffer in one step.

OBSOLETE since the IB/heading insertion unification: thin shim that
delegates to `gptel-org-ib-insert-child' with PARENT = `:point'.
New code should call `gptel-org-ib-insert-child' directly with an
explicit parent (marker or IB).

TODO-KEYWORD is the org TODO state (e.g., \"AI-DO\").
TITLE is the heading text.
TAGS is a list of tag strings.
TERMINATOR-KEYWORD identifies the terminator (e.g., \"FEEDBACK\").

Point must be on the parent heading.
Returns the indirect buffer for the newly created heading."
  (let ((result (gptel-org-ib-insert-child
                 :point todo-keyword title
                 :tags tags
                 :terminator-keyword terminator-keyword)))
    (plist-get result :indirect-buffer)))

(make-obsolete 'gptel-org-ib-safe-insert-sibling
               'gptel-org-ib-insert-child "next")

(defun gptel-org-ib-create-tool-heading (todo-keyword title
                                                      &optional tags
                                                      terminator-keyword
                                                      name)
  "Create a tool-call heading and its dedicated indirect buffer.

OBSOLETE since the IB/heading insertion unification: thin shim that
delegates to `gptel-org-ib-insert-child' with PARENT = `:point' and
the supplied NAME.  New code should call `gptel-org-ib-insert-child'
directly with an explicit parent (marker or IB).

TODO-KEYWORD, TITLE, TAGS, TERMINATOR-KEYWORD have the same meaning
as in `gptel-org-ib-create-heading'.  TERMINATOR-KEYWORD is the
parent's terminator (e.g. \"TERMINE\") that the new heading must
be inserted BEFORE; ensures it exists first.  NAME, when non-nil,
overrides the auto-computed indirect buffer name.

Point must be on the parent heading.

Returns a cons cell (HEADING-MARKER . INDIRECT-BUFFER)."
  (let ((result (gptel-org-ib-insert-child
                 :point todo-keyword title
                 :tags tags
                 :terminator-keyword terminator-keyword
                 :name name)))
    (cons (plist-get result :heading-marker)
          (plist-get result :indirect-buffer))))

(make-obsolete 'gptel-org-ib-create-tool-heading
               'gptel-org-ib-insert-child "next")


(provide 'gptel-indirect-buffer)
;;; gptel-indirect-buffer.el ends here
