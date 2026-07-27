;;; gptel-context.el --- Context aggregator for gptel  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026  Karthik Chikmagalur

;; Author: daedsidog <contact@daedsidog.com>
;; Keywords: convenience, buffers

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

;;; Commentary:

;; The context system manages additional sources of information sent
;; with gptel queries.  Each kind of source is defined as a
;; `gptel-context-type' struct and registered with
;; `gptel-context-register-type'.

;; Context entries
;;
;; Context entries are cons cells (SOURCE . SPEC) in the buffer-local
;; variable `gptel-context'.  SPEC is a plist that must include a :type
;; key.  Entries without :type are resolved at collect time by testing
;; each registered type's predicate.

;;   (buffer-object :type buffer-region)
;;   ("/path/to/file" :type file)
;;   ("/path/to/image.png" :type file-media :mime "image/png")
;;   ("/path/to/dir" :type directory)

;; Step-by-step process of adding context to LLM query:
;;
;; 1. Entries are placed in `gptel-context' by user commands or code.
;; 2. `gptel-context--normalize' attaches :type via
;;    predicates, then dispatches to each type's normalize-fn (filtering
;;    dead entries, upgrading file->file-media, expanding directories).
;;    Type changes are re-normalized recursively.
;; 3. Entries with a format-fn produce annotated text via
;;    `gptel-context--string'.  Entries without (file-media) are
;;    collected by `gptel-context--collect-media' for separate injection.
;; 4. The formatted string is inserted into the request buffer
;;    according to `gptel-use-context'.

;; Defining new context types
;;
;; Use `gptel-context-register-type' with a `gptel-context-type' struct.
;; See the slot docstrings on `gptel-context-type' for details.

;;   (gptel-context-register-type
;;    (make-gptel-context-type
;;     :name 'my-type
;;     :description "..."
;;     :predicate (lambda (e) ...)
;;     :normalize-fn #'my-normalize
;;     :format-fn #'my-format
;;     :display-fn #'my-display
;;     :add-fn #'my-add
;;     :remove-fn #'my-remove)
;;    :add-dwim -90 :remove-dwim -90)

;;; Code:

(eval-and-compile (require 'gptel-request))
(require 'cl-lib)
(require 'project)

(declare-function gptel-menu "gptel-transient" nil t)
(declare-function dired-get-marked-files "dired")
(declare-function dired-get-filename "dired")
(declare-function ibuffer-get-marked-buffers "ibuffer")
(declare-function ibuffer-current-buffer "ibuffer")
(declare-function image-file-name-regexp "image-file")
(declare-function create-image "image")

(defface gptel-context-highlight-face
  '((((background dark)  (min-colors 88)) :background "gray4" :extend t)
    (((background light) (min-colors 88)) :background "alice blue" :extend t)
    (t :inherit mode-line))
  "Face used to highlight gptel contexts in buffers."
  :group 'gptel)

(defface gptel-context-deletion-face
  '((((class color) (min-colors 257) (background light))
     :background "#ffeeee" :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#ffdddd" :extend t)
    (((class color) (min-colors 88) (background dark))
     :background "#553333" :extend t)
    (((class color)) :foreground "red" :extend t))
  "Face used to highlight gptel contexts to be deleted.

This is used in gptel context buffers."
  :group 'gptel)

(defvar gptel-context-wrap-function nil
  "Function to format the context string sent with the gptel request.")
(make-obsolete-variable
 'gptel-context-wrap-function
 "Custom functions for wrapping context are no longer supported by gptel.\
  See `gptel-context--wrap-in-buffer' for details."
 "0.9.9")

(defcustom gptel-context-string-function #'gptel-context--string
  "Function to prepare the context string sent with the gptel request.

This function can be synchronous or asynchronous, and receives one or
two arguments respectively.

Synchronous: an alist of normalized context entries.
Asynchronous: a callback to call with the resulting string, and the
  context alist.

Each normalized entry is a cons (SOURCE . SPEC-PLIST).  SPEC-PLIST
always includes at least a :type key naming a `gptel-context-type'.

To format an entry, retrieve its type struct with
`gptel-context-entry-type', then call its format-fn:

  (when-let* ((type (gptel-context-entry-type entry))
              (format-fn (gptel-context-type-format-fn type)))
    (funcall format-fn source spec))

Format-fn receives SOURCE and SPEC-PLIST and inserts the formatted
text at point.  Entries of type `file-media' are additionally
collected by `gptel-context--collect-media' and injected as media.

Common entry forms from built-in types:

  (buf . (:type buffer-region :overlay ov1 ov2 ...))
  (buf . (:type buffer-region))               ;entire buffer
  (\"/path/to/file\" . (:type file))
  (\"/path/to/file\" . (:type file-media :mime \"image/jpeg\"))
  (\"/path/to/dir\" . (:type directory))

New context types can be registered with `gptel-context-register-type'.
See `gptel-context--string' for the default implementation."
  :group 'gptel
  :type 'function)

(defvar gptel-context-add-dwim-functions nil
  "Abnormal hook for adding context in a DWIM fashion.

Each function is called with the argument `dwim'.  It should return
non-nil if it successfully handled the DWIM add request, which stops
the hook from running further functions.

Functions are added automatically by `gptel-context-register-type'
for types that have an `add-fn'.")

(defvar gptel-context-remove-dwim-functions nil
  "Abnormal hook for removing context in a DWIM fashion.

Each function is called with the argument `dwim'.  It should return
non-nil if it successfully handled the DWIM remove request, which
stops the hook from running further functions.

Functions are added automatically by `gptel-context-register-type'
for types that have a `remove-fn'.")

(defcustom gptel-context-restrict-to-project-files t
  "Restrict files eligible to be added to the context to project files.

When set to t, files in a VCS that are not project files (such as files
listed in `.gitignore' in a Git repository) will not be added to the
context."
  :group 'gptel
  :type 'boolean)

(defvar gptel-context--project-files nil
  "Cached alist of project files per project.")

(defvar gptel-context--reset-cache nil
  "Whether a project files cache-buster has been scheduled.")

;;; Context sources

(cl-defstruct (gptel-context-type (:copier nil))
  "A type of context source for gptel queries.

Each type defines how gptel context entries are normalized, formatted,
and displayed.  A context entry is a cons cell (SOURCE . SPEC) stored
in `gptel-context', where SPEC is a plist that always includes a :type
key.  The value of :type is a symbol matching the `name' slot of a
`gptel-context-type' struct.  SPEC may also carry type-specific keys
like :overlays, :lines, :bounds, or :mime.

See `gptel-context' for the full entry format and the built-in
type registrations below for examples of recognized SPEC
keys.  Custom types can define additional keys."
  ( name nil :type symbol :read-only t
    :documentation
    "A symbol identifying this context type.
Used as the :type property value for matching entries in
`gptel-context'.")
  ( description "" :type string :read-only t
    :documentation
    "A human-readable description shown in menus and help buffers.")
  ( predicate nil :type function
    :documentation
    "A function called with a raw `gptel-context' entry.
Must return non-nil if the entry belongs to this type.
Used by `gptel-context--normalize-entry' to attach :type to
entries lacking an explicit one.")
  ( normalize-fn nil :type function :read-only t
    :documentation
    "A function called with a `gptel-context' entry.
Must return a list of entries for the final context alist.  Called
recursively by `gptel-context--normalize': if an entry's normalize-fn
returns entries of other types, those are normalized in turn.")
  ( format-fn nil :type function
    :documentation
    "Function that inserts prompt-formatted text at point, or nil.
It receives two arguments: source (any Lisp object; for built-in
types a buffer or file path) and spec (a plist -- see the struct
docstring and individual type registrations for recognized keys).
If nil, the entry is handled separately (e.g., by normalizing to other
context types).")
  ( display-fn nil :type function
    :documentation
    "Function that inserts a human-readable representation for the inspection buffer.
It receives two mandatory arguments: source, spec (see `format-fn'),
and an optional LABEL-P flag.  When LABEL-P is non-nil, the function
should return a short label string instead of inserting.  When
inserting, it should create overlays with `gptel-context' and
`gptel-overlay' properties for navigation, via
`gptel-context-make-display-overlay'.")
  ( transient-spec nil :type plist
    :documentation
    "Plist specifying the transient menu suffix for this type, or nil.
Recognized keys:
  :key -- string (e.g. \"-b\") or nil for auto-assignment.
  :description -- string or function of no arguments returning a string.
  :if -- function or nil to conditionally show the suffix.")
  ( add-fn nil :type function
    :documentation
    "Command to add entries of this type, or nil.
Called to add entries to `gptel-context'.
It may be called with an optional ACTION argument:
- `dwim': act in a context-aware manner and return non-nil if
  it handled the request.
- nil or omitted: query the user interactively.  The DWIM target,
  if any, should be offered as the default value.

If nil, the type cannot be added manually.")
  ( remove-fn nil :type function
    :documentation
    "Command to remove entries of this type, or nil.
Called to remove entries from `gptel-context'.
It may be called with an optional ACTION argument:
- `dwim': act in a context-aware manner and return non-nil if
  it handled the request.
- nil or omitted: query the user interactively.  The DWIM target,
  if any, should be offered as the default value.

If nil, the type has no DWIM removal support."))

(defvar gptel-context--type-registry nil
  "Alist of (NAME . gptel-context-type) for registered context types.")

;;;###autoload
(cl-defun gptel-context-register-type
    (context-type &key (add-dwim t) (remove-dwim t))
  "Register a new gptel CONTEXT-TYPE in `gptel-context--type-registry'.

CONTEXT-TYPE must be a `gptel-context-type' struct.  Its name
\\\\(symbol) is used as the key in the registry.

Unless ADD-DWIM is nil, the type's `add-fn' is added to
`gptel-context-add-dwim-functions'.  When ADD-DWIM is a number, it is
used as the DEPTH argument to `add-hook'.  When ADD-DWIM is t
\\\\(the default), the function is added at depth 0.

Unless REMOVE-DWIM is nil, the type's `remove-fn' is added to
`gptel-context-remove-dwim-functions'.  When REMOVE-DWIM is a
number, it is used as the DEPTH argument to `add-hook'.  When
REMOVE-DWIM is t \\(the default), the function is added at depth 0."
  (setf (alist-get (gptel-context-type-name context-type)
                   gptel-context--type-registry)
        context-type)
  (when-let* ((add-dwim)
              (add-fn (gptel-context-type-add-fn context-type)))
    (add-hook 'gptel-context-add-dwim-functions
              add-fn (if (numberp add-dwim) add-dwim 0)))
  (when-let* ((remove-dwim)
              (remove-fn (gptel-context-type-remove-fn context-type)))
    (add-hook 'gptel-context-remove-dwim-functions
              remove-fn (if (numberp remove-dwim) remove-dwim 0))))

;;;###autoload
(defun gptel-context-unregister-type (context-type)
  "Unregister CONTEXT-TYPE from `gptel-context--type-registry'.

CONTEXT-TYPE can be a `gptel-context-type' struct or a symbol
naming a registered type.  Removes the type from the registry
and removes its add-fn and remove-fn from the DWIM hooks."
  (when-let* ((name (if (gptel-context-type-p context-type)
                        (gptel-context-type-name context-type)
                      context-type))
              (type (alist-get name gptel-context--type-registry)))
    (when-let* ((add-fn (gptel-context-type-add-fn type)))
      (remove-hook 'gptel-context-add-dwim-functions add-fn))
    (when-let* ((remove-fn (gptel-context-type-remove-fn type)))
      (remove-hook 'gptel-context-remove-dwim-functions remove-fn))
    (setf (alist-get name gptel-context--type-registry nil 'remove) nil)))

(defun gptel-context-entry-type (context-entry)
  "Return the context type struct for CONTEXT-ENTRY, or nil.

CONTEXT-ENTRY should be a normalized context entry with a :type
property."
  (when-let* ((spec (cdr-safe context-entry))
              (type-name (and (consp spec) (plist-get spec :type))))
    (alist-get type-name gptel-context--type-registry)))

(defun gptel-context--normalize-entry (context-entry)
  "Normalize CONTEXT-ENTRY by attaching :type via predicate matching.

If CONTEXT-ENTRY already carries an explicit :type property, return it
unchanged.  Otherwise test each registered type's predicate and attach
the matching :type.  Signals an error if no registered predicate
matches."
  (if (and (consp context-entry) (consp (cdr context-entry))
           (plist-get (cdr context-entry) :type))
      context-entry
    (catch 'found
      (dolist (pair gptel-context--type-registry)
        (when-let* ((type (cdr pair))
                    (pred (gptel-context-type-predicate type)))
          (when (funcall pred context-entry)
            (throw 'found
                   (if (consp context-entry)
                       (cons (car context-entry)
                             (plist-put (cdr context-entry)
                                        :type (gptel-context-type-name type)))
                     (list context-entry :type (gptel-context-type-name type)))))))
      (error "Unrecognized gptel context entry: %S" context-entry))))

;;; Commands

(defun gptel-context--remove-dwim (&optional confirm)
  "Remove context in a DWIM fashion.
Tries each function in `gptel-context-remove-dwim-functions' in order.
If CONFIRM is non-nil and no specific target is found, offer to
remove all contexts as a last resort."
  (or (run-hook-with-args-until-success
       'gptel-context-remove-dwim-functions 'dwim)
      (when confirm
        (when (and gptel-context
                   (y-or-n-p "Remove all contexts from this buffer? "))
          (gptel-context-remove-all)
          t))))

(defun gptel-context-add-current-kill (&optional arg)
  "Add current kill to gptel, accumulating if ARG is non-nil."
  (interactive "P")
  (let ((kill (current-kill 0)))
    (with-current-buffer (get-buffer-create " *gptel-kill-ring-context*")
      (if (not arg)
          (kill-region (point-min) (point-max))
        (goto-char (point-max))
        (unless (bobp)
          (insert "\n----\n")))
      (insert kill)
      (gptel-context--add-region (current-buffer)
                                 (point-min) (point-max))
      (message "*current-kill* has been added as context."))))

(defun gptel-context-add (&optional arg confirm)
  "Add or remove context to gptel in a DWIM fashion.

Without prefix ARG (or zero ARG), first attempt to remove context
at point via `gptel-context-remove-dwim-functions'.  If nothing is
removed, add context via `gptel-context-add-dwim-functions'.

With positive prefix ARG, prompt for a buffer to add.
With negative prefix ARG, remove context in a DWIM fashion.

When CONFIRM is nil (non-interactive calls), confirmation prompts
in the negative-ARG path are suppressed."
  (interactive "P\np")
  (cond
   ;; Positive ARG: prompt for a buffer to add
   ((and arg (> (prefix-numeric-value arg) 0))
    (let ((buf (read-buffer "Choose buffer to add as context: "
                            (current-buffer) t)))
      (gptel-context--add-buffer (get-buffer buf))))
   ;; Negative ARG: remove context in DWIM fashion
   ((and arg (< (prefix-numeric-value arg) 0))
    (gptel-context--remove-dwim confirm))
   ;; DWIM: try removal first, then addition
   (t
    (or (run-hook-with-args-until-success
         'gptel-context-remove-dwim-functions 'dwim)
        (run-hook-with-args-until-success
         'gptel-context-add-dwim-functions 'dwim)))))

;;;###autoload (autoload 'gptel-add "gptel-context" "Add/remove regions or buffers from gptel's context." t)
(defalias 'gptel-add #'gptel-context-add)

(defun gptel-context--add-buffer (buffer)
  "Add BUFFER to context as a full-buffer entry.

Creates a typed context entry with :type `buffer-region'.

Any existing context entries for BUFFER are removed first, since a
full-buffer entry supersedes them.

Also places an overlay spanning the whole buffer as a visual indicator."
  (gptel-context-remove buffer)
  (push (list buffer :type 'buffer-region) gptel-context)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (gptel-context--make-overlay (point-min) (point-max))))
  (message "Buffer \"%s\" added to context." (buffer-name buffer)))

(defun gptel-context--add-text-file (path)
  "Add text file at PATH to context.

Creates a typed context entry with :type `file'."
  (cl-pushnew (list path :type 'file) gptel-context :test #'equal)
  (message "File \"%s\" added to context." path)
  path)

(defun gptel-context--add-binary-file (path)
  "Add binary file at PATH to context if supported.
Return PATH if added, nil if ignored.

Creates a typed context entry with :type `file-media'."
  (if-let* (((gptel--model-capable-p 'media))
            (mime (mailcap-file-name-to-mime-type path))
            ((gptel--model-mime-capable-p mime)))
      (prog1 path
        (cl-pushnew (list path :type 'file-media :mime mime)
                    gptel-context :test #'equal)
        (message "File \"%s\" added to context." path))
    (message "Ignoring unsupported binary file \"%s\"." path)
    nil))

(defun gptel-context--add-directory (path action)
  "Process all files in directory at PATH according to ACTION.
ACTION should be either `add' or `remove'."
  (dolist (file (directory-files-recursively path "."))
    (pcase-exhaustive action
      ('add
       (unless gptel-context--reset-cache
         (setq gptel-context--reset-cache t)
         (run-at-time
          0 nil
          (lambda () (setq gptel-context--reset-cache nil
                      gptel-context--project-files nil))))
       (if (gptel-context--skip-p file)
           ;; Don't message about .git, as this creates thousands of messages
           (unless (string-match-p "\\.git/" file)
             (gptel-context--message-skipped file))
         (gptel-context-add-file file)))
      ('remove
       (setf (alist-get file gptel-context nil 'remove #'equal) nil)))))

(defun gptel-context-add-file (path)
  "Add the file at PATH to the gptel context.

If PATH is a directory, recursively add all files in it.  PATH should be
readable as text."
  (interactive "fChoose file to add to context: ")
  (cond ((file-directory-p path)
         (gptel-context--add-directory path 'add))
	((gptel--file-binary-p path)
         (gptel-context--add-binary-file path))
	(t (gptel-context--add-text-file path))))

;;;###autoload (autoload 'gptel-add-file "gptel-context" "Add files to gptel's context." t)
(defalias 'gptel-add-file #'gptel-context-add-file)

;;; project-related functions
(defun gptel-context--get-project-files (dir)
  "Return a list of files in the project DIR, or nil if no project is found."
  (when-let* ((project (project-current nil dir)))
    (with-memoization (alist-get dir gptel-context--project-files
                                 nil nil #'equal)
      (project-files project))))

(defun gptel-context--skip-p (file)
  "Return non-nil if FILE should not be added to the context."
  (when (and gptel-context-restrict-to-project-files
	     (not (file-remote-p file)))
    (and-let* ((file-dir (or (file-name-directory file) default-directory))
               (project (project-current nil file-dir)))
      (not (member (expand-file-name file)
                   (gptel-context--get-project-files (project-root project)))))))

(defun gptel-context--message-skipped (file)
  "Message that FILE is skipped because it is not a project file."
  (let* ((type (if (file-directory-p file) "directory" "file"))
	 (reminder (format "To include it, unset `%S'."
			   'gptel-context-restrict-to-project-files)))
    (if-let* ((root (cl-some (lambda (dir) (and (file-in-directory-p file dir) dir))
                             (map-keys gptel-context--project-files)))
	      (rel-file (file-relative-name file root)))
	(message "Skipping %s \"%s\" in project \"%s\".  %s"
		 type rel-file root reminder)
      (message "Skipping %s \"%s\". %s" type file reminder))))

;;; Remove context
(defun gptel-context-remove (&optional context)
  "Remove the CONTEXT overlay from the contexts list.

If CONTEXT is nil and selection is active, removes all contexts
within the selection via DWIM dispatch.
If CONTEXT is nil and point is on a context overlay, removes it.
If CONTEXT is a directory, recursively removes all files in it."
  (interactive)
  (cond
   ((overlayp context)                  ;Overlay in buffer
    (when-let* ((buf (overlay-buffer context)))
      (delete-overlay context)
      ;; FIXME: Quadratic cost when clearing a bunch of contexts at once
      (unless
          (or (cl-loop
               for ov in
               (plist-get (alist-get buf gptel-context) :overlays)
               thereis (overlay-start ov))
              (when-let* ((spec (alist-get buf gptel-context)))
                (or (plist-get spec :lines)
                    (plist-get spec :bounds))))
        (setf (alist-get buf gptel-context nil 'remove) nil))))
   ((bufferp context)                   ;Full buffer
    (setf (alist-get context gptel-context nil 'remove) nil)
    (when (buffer-live-p context)
      (with-current-buffer context
        (without-restriction
          (remove-overlays nil nil 'gptel-context t)))))
   ((stringp context)                   ;file or directory
    (if (file-directory-p context)
        (gptel-context--add-directory context 'remove)
      (setf (alist-get context gptel-context nil 'remove #'equal) nil)
      (message "File \"%s\" removed from context." context)))
   (t                                   ;DWIM dispatch
    (or (gptel-context--remove-dwim)
        (gptel-context--prompt-remove)))))

(defun gptel-context--prompt-remove ()
  "Prompt the user to select a context entry to remove.

Uses each context type's `display-fn' with LABEL-P non-nil
to produce prompt-friendly labels.  For types without a
display-fn, uses the type name as the label.

Returns t if an entry was removed, nil otherwise."
  (when-let* ((entries gptel-context))
    (let* ((alist
            (mapcar
             (lambda (entry)
               (let* ((norm (gptel-context--normalize-entry entry))
                      (source (car norm))
                      (spec (cdr norm))
                      (type (gptel-context-entry-type norm))
                      (display-fn (and type
                                       (gptel-context-type-display-fn type)))
                      (label
                       (if display-fn
                           (funcall display-fn source spec t)
                         (format "%s: %s"
                                 (or (and type
                                          (gptel-context-type-name type))
                                     "unknown")
                                 (cond
                                  ((bufferp source)
                                   (buffer-name source))
                                  ((stringp source)
                                   (abbreviate-file-name source))
                                  (t (format "%s" source)))))))
                 (cons label entry)))
             entries))
           (choice (completing-read "Remove context: " alist nil t)))
      (when-let* ((entry (cdr (assoc choice alist))))
        (let ((source (if (consp entry) (car entry) entry)))
          (when (buffer-live-p source)
            (with-current-buffer source
              (remove-overlays nil nil 'gptel-context t))))
        (setq gptel-context (delete entry gptel-context))
        (message "Context removed.")
        t))))

(defun gptel-context-remove-all (&optional verbose)
  "Remove all gptel context.

If VERBOSE is non-nil, ask for confirmation and message
afterwards."
  (interactive (list t))
  (if (null gptel-context)
      (when verbose (message "No gptel context sources to remove."))
    (when (or (not verbose) (y-or-n-p "Remove all context? "))
      (cl-loop
       for context in gptel-context
       for (source . spec) = (ensure-list context)
       if (bufferp source) do           ;Buffers and buffer regions
       (mapc #'gptel-context-remove (plist-get spec :overlays))
       else do (gptel-context-remove source) ;files or other types
       finally do (setq gptel-context nil))
      (when verbose (message "Removed all gptel context sources.")))))

;;; Context wrap
(defun gptel-context--make-overlay (start end &optional advance)
  "Highlight the region from START to END.

ADVANCE controls the overlay boundary behavior."
  (let ((overlay (make-overlay start end nil (not advance) advance))
        (buf-entry (alist-get (current-buffer) gptel-context)))
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'face 'gptel-context-highlight-face)
    (overlay-put overlay 'gptel-context t)
    (setf (alist-get (current-buffer) gptel-context)
          (plist-put buf-entry :overlays
                     (cons overlay (plist-get buf-entry :overlays))))
    overlay))

(defun gptel-context-make-display-overlay (beg end source)
  "Create an inspection-buffer overlay from BEG to END for SOURCE.

Tag the overlay with a `gptel-context' property holding SOURCE and
a `gptel-overlay' property, so that navigation commands in the
gptel context inspection buffer can locate it.  Return the overlay."
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'gptel-context source)
    (overlay-put ov 'gptel-overlay t)
    (overlay-put ov 'evaporate t)
    ov))

;;;###autoload
(defun gptel-context--wrap (callback data-buf)
  "Add request context to DATA-BUF and run CALLBACK.

DATA-BUF is the buffer where the request prompt is constructed."
  (if (= (car (func-arity gptel-context-string-function)) 2)
      (funcall gptel-context-string-function
               (lambda (c) (with-current-buffer data-buf
                        (gptel-context--wrap-in-buffer c))
                 (funcall callback))
               (gptel-context--normalize))
    (with-current-buffer data-buf
      (thread-last (gptel-context--normalize)
                   (funcall gptel-context-string-function)
                   (gptel-context--wrap-in-buffer)))
    (funcall callback)))

(defun gptel-context--wrap-in-buffer (context-string &optional method)
  "Inject CONTEXT-STRING to current buffer using METHOD.

METHOD is either system or user, and defaults to `gptel-use-context'.
This modifies the buffer."
  (when (length> context-string 0)
    (pcase (or method gptel-use-context)
      ('system
       (if (gptel--model-capable-p 'nosystem)
           (gptel-context--wrap-in-buffer context-string 'user)
         (if gptel-system-prompt
             (cl-etypecase gptel-system-prompt
               (string
                (setq gptel-system-prompt
                      (concat context-string "\n\n" gptel-system-prompt)))
               (function
                ;; FIXME: This parses the directive in the prompt construction
                ;; buffer, which is wrong.  Wrap in a function instead.
                (setq gptel-system-prompt
                      (gptel--parse-directive gptel-system-prompt 'raw))
                (gptel-context--wrap-in-buffer context-string))
               (list
                (setq gptel-system-prompt ;cons a new list to avoid mutation
                      (cons (concat context-string "\n\n" (car gptel-system-prompt))
                            (cdr gptel-system-prompt)))))
           (setq gptel-system-prompt context-string))))
      ('user
       (goto-char (point-max))
       (text-property-search-backward 'gptel nil t)
       (and gptel-mode
            (looking-at
             (concat "[\n[:blank:]]*"
                     (and-let* ((prefix (gptel-prompt-prefix-string))
                                ((not (string-empty-p prefix))))
                       (concat "\\(?:" (regexp-quote prefix) "\\)?"))))
            (delete-region (match-beginning 0) (match-end 0)))
       (insert "\n" context-string "\n\n")))))

(defun gptel-context--collect-media (&optional context-entries)
  "Collect media context entries for base64 encoding.

CONTEXT-ENTRIES, which are typically paths to binary files, are
base64-encoded and prepended to the first user prompt."
  (cl-loop for context-entry in (or context-entries gptel-context)
           for normalized = (gptel-context--normalize-entry context-entry)
           for (path . props) = (ensure-list normalized)
           when (eq (plist-get props :type) 'file-media)
           collect (list :media path :mime (plist-get props :mime))))

(cl-defun gptel-context--add-region (buffer region-beginning region-end &optional advance)
  "Add region delimited by REGION-BEGINNING, REGION-END in BUFFER as context.

If ADVANCE is non-nil, the context overlay envelopes changes at
the beginning and end."
  ;; Remove existing contexts in the same region, if any.
  (mapc #'gptel-context-remove
        (gptel-context--in-region buffer region-beginning region-end))
  (prog1 (with-current-buffer buffer
           (gptel-context--make-overlay region-beginning region-end advance))
    (message "Region added to context buffer.")))

(defun gptel-context--in-region (buffer start end)
  "Return the list of context overlays in the given region, if any, in BUFFER.
START and END signify the region delimiters."
  (with-current-buffer buffer
    (cl-remove-if-not (lambda (ov) (overlay-get ov 'gptel-context))
                      (overlays-in start end))))

(defun gptel-context--at-point ()
  "Return the context overlay at point, if any."
  (cl-find-if (lambda (ov) (overlay-get ov 'gptel-context))
              (overlays-at (point))))

;;;###autoload
(defun gptel-context--normalize (&optional context-alist)
  "Normalize all context entries in CONTEXT-ALIST recursively.

CONTEXT-ALIST defaults to the current value of `gptel-context'.

Each entry is first normalized via `gptel-context--normalize-entry'
\\(attaching a :type if absent).  It is then dispatched to its context
type's normalize function.  If the normalize function returns entries
of a different type, those are normalized in turn.  Entries without a
registered type signal an error.

Ignore overlays, buffers and files that are not live or readable."
  (let ((queue (copy-sequence (or context-alist gptel-context)))
        (res))
    (while queue
      (let* ((context-entry (pop queue))
             (normalized (gptel-context--normalize-entry context-entry)))
        (if-let* ((type (gptel-context-entry-type normalized))
                  (normalize-fn (gptel-context-type-normalize-fn type)))
            (let ((type-name (gptel-context-type-name type)))
              (dolist (sub (funcall normalize-fn normalized))
                (if-let* ((sub-type (gptel-context-entry-type sub))
                          ((not (eq (gptel-context-type-name sub-type) type-name))))
                    ;; Type changed -- push back for further normalization
                    (push sub queue)
                  ;; Same type -- terminal
                  (push sub res))))
          ;; No registered type handler -- should not happen if
          ;; normalize-entry succeeded
          (push normalized res))))
    (nreverse res)))

(defun gptel-context--collect-regions (buffer context-data)
  "Collect BUFFER regions from CONTEXT-DATA specification.

CONTEXT-DATA is a plist with keys :overlays, :lines and :bounds.
Returns a sorted list of (START . END) position pairs."
  (let ((regions                   ; Collect bounds (already in position format)
         (when-let* ((bounds (plist-get context-data :bounds)))
           (if (consp (car bounds)) bounds (list bounds)))))
    (with-current-buffer buffer
      (without-restriction
        ;; Collect overlays
        (dolist (ov (plist-get context-data :overlays))
          (when (overlay-start ov)
            (push (cons (overlay-start ov) (overlay-end ov))
                  regions)))                 ;(start1 . end1)
        ;; Collect lines (convert line numbers to positions)
        (when-let* ((line-bounds (plist-get context-data :lines)))
          (dolist (pair (if (consp (car line-bounds)) ;Handle single (BEG . END)
                            line-bounds (list line-bounds)))
            (push (cons (progn (goto-char (point-min))
                               (forward-line (1- (car pair)))
                               (point))
                        (progn (goto-char (point-min))
                               (forward-line (cdr pair))
                               (point)))
                  regions)))))
    ;; TODO: Update sort for Emacs 28+ calling convention
    ;; Sort by start position.
    ;; NOTE: This can modify `:bounds' of `context-data' by side-effect!
    (sort regions #'car-less-than-car)))

(defun gptel-context--insert-buffer-string (buffer context-data &optional header)
  "Insert at point a context string from CONTEXT-DATA in BUFFER.

CONTEXT-DATA is a plist with keys :overlays, :lines and :bounds to
include specific overlays, line ranges or position bounds instead of the
entire buffer.  See `gptel-context'.

HEADER is an optional header to insert before the contents.

This is a shared helper used by built-in context types for prompt
formatting."
  (let ((is-top-snippet t)
        (previous-line 1)
        (regions (gptel-context--collect-regions buffer context-data)))

    ;; Insert header
    (insert (or header (format "In buffer `%s`:\n\n```"(buffer-name buffer)))
            (gptel--strip-mode-suffix (buffer-local-value
                                       'major-mode buffer))
            "\n")
    (if (not regions)
        (insert-buffer-substring-no-properties buffer)
      (dolist (region regions)
        (let ((start (car region)) (end (cdr region)))
          (let (lineno column)
            (with-current-buffer buffer
              (without-restriction
                (setq lineno (line-number-at-pos start t)
                      column (save-excursion (goto-char start) (current-column)))))
            ;; We do not need to insert a line number indicator if we have two regions
            ;; on the same line, because the previous region should have already put the
            ;; indicator.
            (unless (= previous-line lineno)
              (unless (= lineno 1)
                (unless is-top-snippet (insert "\n"))
                (insert (format "... (Line %d)\n" lineno))))
            (setq previous-line lineno)
            (unless (zerop column) (insert " ..."))
            (if is-top-snippet
                (setq is-top-snippet nil)
              (unless (= previous-line lineno) (insert "\n"))))
          (insert-buffer-substring-no-properties buffer start end)))
      (unless (>= (cdr (car (last regions))) (point-max))
        (insert "\n...")))
    (insert "\n```")))

(defun gptel-context--insert-file-string (path &optional spec)
  "Insert at point the contents of file at PATH as context.

SPEC is a plist specifying :lines or position :bounds to include instead
of the entire file.  See `gptel-context' for details."
  (if (not (and spec (or (plist-member spec :lines)
                         (plist-member spec :bounds))))
      ;; Insert whole file
      (gptel--insert-file-string path)
    ;; Insert only regions from lines and/or bounds
    (let* ((visiting-buf (find-buffer-visiting ;Reuse buffer
                          path (lambda (b) (not (buffer-modified-p b)))))
           (file-buf (or visiting-buf   ;temp buf to dump file contents
                         (gptel--temp-buffer " *gptel-file-context*"))))
      (unless visiting-buf
        (with-current-buffer file-buf (insert-file-contents path)))
      (gptel-context--insert-buffer-string
       file-buf spec (format "In file `%s`:\n\n```\n"
                             (abbreviate-file-name path)))
      (unless visiting-buf (kill-buffer file-buf)))))

(defun gptel-context--string (context-alist)
  "Format the aggregated gptel context as annotated markdown fragments.

Returns a string.  CONTEXT-ALIST is a structure containing
context entries as produced by `gptel-context--normalize'.

Each context entry is dispatched to its context type's format
function.  Entries whose type has no format function (e.g., media
files) are skipped -- they are handled separately by media injection."
  (with-temp-buffer
    (cl-loop for context-entry in context-alist
             for (source . spec) = (ensure-list context-entry)
             for type = (gptel-context-entry-type context-entry)
             for format-fn = (and type (gptel-context-type-format-fn type))
             if (and format-fn source)
             do (funcall format-fn source spec)
             and do (insert "\n\n")
             finally do
             (skip-chars-backward "\n\t\r ")
             (delete-region (point) (point-max))
             (unless (bobp)
               (goto-char (point-min))
               (insert "Request context:\n\n"))
             finally return
             (and (> (buffer-size) 0)
                  (buffer-string)))))

;;; Major mode for context inspection buffers
(defvar-keymap gptel-context-buffer-mode-map
  "C-c C-c" #'gptel-context-confirm
  "C-c C-k" #'gptel-context-quit
  "RET"     #'gptel-context-visit
  "n"       #'gptel-context-next
  "p"       #'gptel-context-previous
  "d"       #'gptel-context-flag-deletion)

(define-derived-mode gptel-context-buffer-mode special-mode "gptel-context"
  "Major-mode for inspecting context used by gptel."
  :group 'gptel
  (add-hook 'post-command-hook #'gptel-context--post-command
            nil t)
  (setq-local revert-buffer-function #'gptel-context--buffer-setup))

(defun gptel-context--buffer-setup (&optional _ignore-auto _noconfirm context-alist)
  "Set up the gptel context buffer.

CONTEXT-ALIST is the alist of contexts to use to populate the buffer.

All context entries are normalized via `gptel-context--normalize-entry'
before being dispatched to their type's display function."
  (with-current-buffer (get-buffer-create "*gptel-context*")
    (gptel-context-buffer-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (setq header-line-format
            (substitute-command-keys
             (concat
              "\\[gptel-context-flag-deletion]: Mark/unmark deletion, "
              "\\[gptel-context-next]/\\[gptel-context-previous]: next/previous, "
              "\\[gptel-context-visit]: visit, "
              "\\[gptel-context-confirm]: apply, "
              "\\[gptel-context-quit]: cancel, "
              "\\[quit-window]: quit")))
      (save-excursion
        (let ((contexts (gptel-context--normalize context-alist)))
          (if (length= contexts 0)
              (insert "There are no active gptel contexts.")
            (pcase-dolist (`(,source . ,spec) contexts)
              (let* ((type-name (plist-get spec :type))
                     (type (alist-get type-name gptel-context--type-registry)))
                (when-let* ((display-fn (and type (gptel-context-type-display-fn type))))
                  (funcall display-fn source spec)
                  (insert "\n" (make-separator-line) "\n"))))
            (goto-char (point-min))))))
    (display-buffer (current-buffer)
                    `((display-buffer-reuse-window
                       display-buffer-reuse-mode-window
                       display-buffer-below-selected)
                      (body-function . ,#'select-window)
                      (window-height . ,#'fit-window-to-buffer)))))

(defvar gptel-context--buffer-reverse nil
  "Last direction of cursor movement in gptel context buffer.

If non-nil, indicates backward movement.")

(defalias 'gptel-context--post-command
  (let ((highlight-overlay))
    (lambda ()
      ;; Only update if point moved outside the current region.
      (unless (memq highlight-overlay (overlays-at (point)))
        (let ((context-overlay
               (cl-loop for ov in (overlays-at (point))
                        thereis (and (overlay-get ov 'gptel-overlay) ov))))
          (when highlight-overlay
            (overlay-put highlight-overlay 'face nil))
          (when context-overlay
            (overlay-put context-overlay 'face 'highlight))
          (setq highlight-overlay context-overlay))))))

(defun gptel-context-visit ()
  "Display the location of this gptel context chunk in its original buffer."
  (interactive)
  (let ((ov-here (car (overlays-at (point)))))
    (if-let* ((source (overlay-get ov-here 'gptel-context))
              (buf (if (overlayp source)
                       (overlay-buffer source)
                     (find-file-noselect source)))
              (offset (- (point) (overlay-start ov-here))))
        (with-selected-window (display-buffer buf)
          (goto-char (if (overlayp source)
                         (overlay-start source)
                       (point-min)))
          (forward-char offset)
          (recenter))
      (message "No source location for this gptel context chunk."))))

(defun gptel-context-next ()
  "Move to next gptel context chunk."
  (interactive)
  (let ((ov-here (car (overlays-at (point))))
        (next-start (next-overlay-change (point))))
    (when (and (/= (point-max) next-start) ov-here)
      ;; We were inside the overlay, so we want the next overlay change, which
      ;; would be the start of the next overlay.
      (setq next-start (next-overlay-change next-start)))
    (when (/= next-start (point-max))
      (setq gptel-context--buffer-reverse nil)
      (goto-char next-start)
      (recenter (floor (window-height) 4)))))

(defun gptel-context-previous ()
  "Move to previous gptel context chunk."
  (interactive)
  (let ((ov-here (car (overlays-at (point)))))
    (when ov-here (goto-char (overlay-start ov-here)))
    (let ((previous-context-pos (previous-overlay-change
                                 (previous-overlay-change (point)))))
      ;; Prevent point from jumping to the start of the buffer.
      (unless (= previous-context-pos (point-min))
        (goto-char previous-context-pos)
        (recenter (floor (window-height) 4))
        (setq gptel-context--buffer-reverse t)))))

(defun gptel-context-flag-deletion ()
  "Mark gptel context chunk at point for removal."
  (interactive)
  (let* ((overlays (if (use-region-p)
                       (overlays-in (region-beginning) (region-end))
                     (overlays-at (point))))
         (deletion-ov)
         (marked-ovs (cl-remove-if-not (lambda (ov) (overlay-get ov 'gptel-context-deletion-mark))
                                       overlays)))
    (if marked-ovs
        (mapc #'delete-overlay marked-ovs)
      (save-excursion
        (dolist (ov overlays)
          (when (overlay-get ov 'gptel-context)
            (goto-char (overlay-start ov))
            (setq deletion-ov (make-overlay (overlay-start ov) (overlay-end ov)))
            (overlay-put deletion-ov 'gptel-context (overlay-get ov 'gptel-context))
            (overlay-put deletion-ov 'priority -80)
            (overlay-put deletion-ov 'face 'gptel-context-deletion-face)
            (overlay-put deletion-ov 'gptel-context-deletion-mark t)))))
    (if (use-region-p)
        (deactivate-mark)
      (if gptel-context--buffer-reverse
          (gptel-context-previous)
        (gptel-context-next)))))

(defun gptel-context-quit ()
  "Cancel pending operations and return to gptel's menu."
  (interactive)
  (quit-window)
  (call-interactively #'gptel-menu))

(defun gptel-context-confirm ()
  "Confirm pending operations and return to gptel's menu."
  (interactive)
  ;; Delete all the context overlays that have been marked for deletion.
  (when-let* ((deletion-marks
               (delq nil (mapcar
                        (lambda (ov)
                          (and
                           (overlay-get ov 'gptel-context-deletion-mark)
                           (overlay-get ov 'gptel-context)))
                        (overlays-in (point-min) (point-max))))))
    (mapc #'gptel-context-remove deletion-marks)
    (revert-buffer))
  ;; FIXME(context): This should run in the buffer from which the context
  ;; inspection buffer was visited.
  ;; Update contexts and revert buffer (#482)
  (setq gptel-context (gptel-context--normalize))
  (gptel-context-quit))

;;; Built-in context type definitions

;;; Buffer-region context type
(defun gptel-context--normalize-buffer-region (context-entry)
  "Normalize a buffer-region CONTEXT-ENTRY.
Clean dead overlays and return a list with the normalized entry.
Return nil when specified buffer is not live."
  (let* ((buf (car context-entry))
         (spec (cdr context-entry)))
    (if (not (buffer-live-p buf))
        nil
      ;; Backward compatibility: the pre-type context format stored
      ;; overlays directly after the buffer, as in
      ;; (BUFFER OVERLAY1 OVERLAY2 ...).  Convert these to the plist
      ;; format, with overlays under the :overlays key.
      ;; Note that (BUFFER :overlays (OV1 OV2) ...) will not match.
      (when (and spec (cl-some #'overlayp spec))
        (setq spec (list :overlays (cl-remove-if-not #'overlayp spec)
                         :type 'buffer-region)))
      (when-let* ((ovs (plist-get spec :overlays)))
        (plist-put spec :overlays
                   (seq-filter (lambda (ov) (eq buf (overlay-buffer ov)))
                               ovs)))
      (list (cons buf (plist-put spec :type 'buffer-region))))))

(defun gptel-context--format-buffer-region (source spec)
  "Format buffer-region context SOURCE at point for the LLM prompt.
SPEC is a plist of the context entry."
  (gptel-context--insert-buffer-string source spec))

(defun gptel-context--display-buffer-region (source spec &optional label-p)
  "Insert buffer-region SOURCE with SPEC in the inspection buffer.

When SPEC has :overlays, :lines, or :bounds, insert each covered
region with line numbers.  Otherwise, insert the entire buffer
SOURCE.

When optional LABEL-P is non-nil, return a short one-line label
string instead of inserting into the buffer."
  (if label-p
      (if (or (plist-get spec :overlays)
              (plist-get spec :lines)
              (plist-get spec :bounds))
          (format "Buffer region: %s" (buffer-name source))
        (format "Buffer: %s" (buffer-name source)))
    (let ((region-pairs (gptel-context--collect-regions source spec))
          (overlay-alist nil))
      ;; Map overlay start -> overlay for navigation reuse
      (dolist (ov (plist-get spec :overlays))
        (when (overlay-start ov)
          (push (cons (overlay-start ov) ov) overlay-alist)))
      (if region-pairs
          (dolist (pair region-pairs)
            (let* ((start (car pair))
                   (end (cdr pair))
                   ;; Prefer the original source overlay so
                   ;; gptel-context-visit can navigate to the exact
                   ;; position.  Fall back to storing the source
                   ;; buffer itself (visit will go to point-min).
                   (source-ref (or (cdr (assq start overlay-alist))
                                   source))
                   (l1 (with-current-buffer source
                         (line-number-at-pos start)))
                   (l2 (with-current-buffer source
                         (line-number-at-pos end))))
              (insert (propertize (format "In buffer %s (lines %d-%d):\n\n"
					  (buffer-name source) l1 l2)
				  'face 'bold))
              (let ((beg (point)))
                (insert-buffer-substring source start end)
                (insert "\n")
                (gptel-context-make-display-overlay beg (point) source-ref))))
        ;; Full buffer display
        (insert (propertize (format "In buffer %s:\n\n" (buffer-name source))
                            'face 'bold))
        (let ((beg (point)))
	  (insert-buffer-substring source)
	  (insert "\n")
	  (gptel-context-make-display-overlay beg (point) source))))))

(defun gptel-context--buffer-region-add (&optional action)
  "Add buffer-region context entries.

When a region is active, add it as a context region.

Otherwise, when ACTION is `dwim', add the current buffer (or
marked buffers in IBuffer).  When called interactively (ACTION
nil), prompt for a buffer to add, defaulting to the DWIM target."
  (interactive)
  (cond ((use-region-p)
         (gptel-context--add-region (current-buffer)
                                    (region-beginning) (region-end))
         (deactivate-mark)
         (message "Current region added as context.")
         t)
        ((eq action 'dwim)
         (let ((dwim-buffers
                (cond ((derived-mode-p 'ibuffer-mode)
                       (or (ibuffer-get-marked-buffers)
                           (list (ibuffer-current-buffer))))
                      (t (list (current-buffer))))))
           (mapc #'gptel-context--add-buffer dwim-buffers)
           t))
        (t
         (let ((default (cond ((derived-mode-p 'ibuffer-mode)
                               (or (car (ibuffer-get-marked-buffers))
                                   (ibuffer-current-buffer)))
                              (t (current-buffer)))))
           (gptel-context--add-buffer
            (get-buffer
             (read-buffer "Choose buffer to add as context: " default t)))))))

(defun gptel-context--buffer-region-remove (&optional action)
  "Remove buffer-region context entries.

When ACTION is `dwim', act in a context-aware manner:
- If a region is active, remove all context overlays in it.
- If point is on a context overlay, remove it.
- In IBuffer, remove marked buffers or the buffer at point.
- Otherwise, take no action (return nil).

When called interactively (ACTION nil), prompt for a buffer to
remove, defaulting to the DWIM target."
  (interactive)
  (pcase action
    ('dwim
     (cond ((region-active-p)
            (when-let* ((contexts
                         (gptel-context--in-region
                          (current-buffer) (region-beginning) (region-end))))
              (cl-loop for ctx in contexts do (gptel-context-remove ctx))
              t))
           ((gptel-context--at-point)
            (gptel-context-remove (gptel-context--at-point))
            t)
           ((derived-mode-p 'ibuffer-mode)
            (let ((buffers (or (ibuffer-get-marked-buffers)
                               (list (ibuffer-current-buffer)))))
              (mapc #'gptel-context-remove buffers)
              t))
           (t nil)))
    (_
     ;; Interactive query with DWIM default
     (let ((default (cond ((derived-mode-p 'ibuffer-mode)
                           (or (car (ibuffer-get-marked-buffers))
                               (ibuffer-current-buffer)))
                          (t (current-buffer)))))
       (gptel-context-remove
        (read-buffer "Remove buffer from context: " default t))))))

;; Recognized SPEC keys:
;;   :overlays -- list of overlay objects covering context regions
;;   :lines    -- list of (BEG . END) line-number pairs
;;   :bounds   -- list of (BEG . END) buffer-position pairs
(gptel-context-register-type
 (make-gptel-context-type
  :name 'buffer-region
  :description "Buffer contents (full buffer or specific regions)."
  :predicate
  (lambda (context-entry)
    (or (bufferp context-entry)
        (and (consp context-entry) (bufferp (car context-entry)))))
  :normalize-fn #'gptel-context--normalize-buffer-region
  :format-fn #'gptel-context--format-buffer-region
  :display-fn #'gptel-context--display-buffer-region
  :transient-spec (list :key "-b"
                        :description "Add buffer/region to context")
  :add-fn #'gptel-context--buffer-region-add
  :remove-fn #'gptel-context--buffer-region-remove)
 :add-dwim -90 :remove-dwim -90)

;;; File context type (text files)
(defun gptel-context--normalize-file (context-entry)
  "Normalize a file CONTEXT-ENTRY.
Returns the entry, upgrading to file-media if the file is binary,
preserving all spec keys."
  (let* ((path (car context-entry))
         (spec (cdr context-entry)))
    (if (file-readable-p path)
        (if (gptel--file-binary-p path)
            (list (cons path
                        (plist-put
                         (plist-put spec :type 'file-media)
                         :mime (or (plist-get spec :mime)
                                   (mailcap-file-name-to-mime-type path)))))
          (list (cons path (plist-put spec :type 'file))))
      nil)))

(defun gptel-context--format-file (source spec)
  "Format file context SOURCE at point for the LLM prompt.
SPEC is a plist of the context entry."
  (gptel-context--insert-file-string source spec))

(defun gptel-context--display-file (source spec &optional label-p)
  "Display file context SOURCE at point in the inspection buffer.

When SPEC has :lines or :bounds, display only those regions with
line numbers.  Otherwise display the entire file.

When optional LABEL-P is non-nil, return a short one-line label
string instead of inserting into the buffer."
  (if label-p
      (format "File: %s" (abbreviate-file-name source))
    (if (not (and spec (or (plist-get spec :lines)
                         (plist-get spec :bounds))))
        ;; Whole file
        (progn
          (insert (propertize (format "In file %s:\n\n"
                                      (file-name-nondirectory source))
                              'face 'bold))
          (let ((beg (point)))
            (insert-file-contents source)
            (goto-char (point-max))
            (insert "\n")
            (gptel-context-make-display-overlay beg (point) source)))
      ;; Regions from :lines or :bounds
      (let* ((visiting-buf (find-buffer-visiting
                            source (lambda (b) (not (buffer-modified-p b)))))
             (file-buf (or visiting-buf
                           (gptel--temp-buffer " *gptel-file-context*"))))
        (unless visiting-buf
          (with-current-buffer file-buf (insert-file-contents source)))
        (let ((region-pairs (gptel-context--collect-regions
                             file-buf spec)))
          (dolist (pair region-pairs)
            (let* ((start (car pair)) (end (cdr pair))
                   (l1 (with-current-buffer file-buf
                         (line-number-at-pos start)))
                   (l2 (with-current-buffer file-buf
                         (line-number-at-pos end))))
              (insert (propertize (format "In file %s (lines %d-%d):\n\n"
                                          (file-name-nondirectory source)
                                          l1 l2)
                                  'face 'bold))
              (let ((beg (point)))
                (insert-buffer-substring file-buf start end)
                (insert "\n")
                (gptel-context-make-display-overlay beg (point) source)))))
        (unless visiting-buf (kill-buffer file-buf))))))

(defun gptel-context--file-add (&optional action)
  "Add file context entries.

When ACTION is `dwim', act in a context-aware manner:
- In Dired, add marked files (with confirmation for directories).
- In Image mode, add the image file if the model supports media.
- Otherwise, take no action (return nil).

When called interactively (ACTION nil), query the user for a file
to add, defaulting to the DWIM target."
  (interactive)
  (let ((dwim-files
         (cond ((derived-mode-p 'dired-mode)
                (dired-get-marked-files))
               ((and (derived-mode-p 'image-mode)
                     (gptel--model-capable-p 'media)
                     (buffer-file-name)
                     (not (gptel-context--skip-p (buffer-file-name))))
                (list (buffer-file-name))))))
    (if (eq action 'dwim)
        (when dwim-files
          (let ((dirs (cl-remove-if-not #'file-directory-p dwim-files)))
            (when (or (null dirs)
                      (y-or-n-p
                       (format "Recursively add files from %d director%s? "
                               (length dirs)
                               (if (= (length dirs) 1) "y" "ies"))))
              (mapc #'gptel-context-add-file dwim-files)))
          t)
      ;; Interactive query with DWIM default
      (let ((default (or (car dwim-files) (buffer-file-name))))
        (if default
            (gptel-context-add-file
             (read-file-name "Choose file to add to context: "
                             nil default t))
          (call-interactively #'gptel-add-file))))))

(defun gptel-context--file-remove (&optional action)
"Remove file context entries.
When ACTION is `dwim', act in a context-aware manner:
- In Dired, remove marked files.
- Otherwise, take no action (return nil).

When called interactively (ACTION nil), query the user for a file
to remove, defaulting to the DWIM target."
(interactive)
(pcase action
  ('dwim
   (cond ((derived-mode-p 'dired-mode)
          (let ((files (dired-get-marked-files)))
            (mapc #'gptel-context-remove files)
            t))
         (t nil)))
  (_
   ;; Interactive query with DWIM default
   (let ((default (cond ((derived-mode-p 'dired-mode)
                         (dired-get-filename nil t))
                        ((buffer-file-name) (buffer-file-name))
                        (t default-directory))))
     (gptel-context-remove
      (read-file-name "Remove file from context: " nil default t))))))

;; Recognized SPEC keys:
;;   :lines  -- list of (BEG . END) line-number pairs
;;   :bounds -- list of (BEG . END) position pairs
;;   :mime   -- MIME type string (set when normalized to file-media)
(gptel-context-register-type
 (make-gptel-context-type
  :name 'file
  :description "A file on disk."
  :predicate
  (lambda (context-entry)
    (let ((path (if (consp context-entry) (car context-entry) context-entry)))
      (and (stringp path) (not (file-directory-p path))
           (file-readable-p path) (not (gptel--file-binary-p path)))))
  :normalize-fn #'gptel-context--normalize-file
  :format-fn #'gptel-context--format-file
  :display-fn #'gptel-context--display-file
  :transient-spec (list :key "-f" :description "Add a file to context")
  :add-fn #'gptel-context--file-add
  :remove-fn #'gptel-context--file-remove)
 :add-dwim -80 :remove-dwim -80)

;;; File-media context type (binary/media files)
(defun gptel-context--display-file-media (source spec &optional label-p)
  "Display media file SOURCE at point in the inspection buffer.
SPEC is a plist of the context entry.

When optional LABEL-P is non-nil, return a short one-line label
string instead of inserting into the buffer."
  (if label-p
      (format "Media: %s" (abbreviate-file-name source))
    (insert (propertize (format "In file %s:\n\n" (file-name-nondirectory source))
                        'face 'bold))
    (let ((beg (point)))
      (if-let* ((mime (plist-get spec :mime))
                ((not (string-match-p "^text/" mime)))
                ((string-match-p (image-file-name-regexp) source))
                (img (create-image source)))
          (insert-image img "*")
        (insert source " "
                (propertize "(No preview for binary file)"
                            'face '(:inherit shadow :slant italic))))
      (goto-char (point-max))
      (insert "\n")
      (gptel-context-make-display-overlay beg (point) source))))

;; Recognized SPEC keys:
;;   :mime -- MIME type string (e.g. "image/png")
(gptel-context-register-type
 (make-gptel-context-type
  :name 'file-media
  :description "A binary or media file."
  :predicate
  (lambda (context-entry)
    (let ((path (if (consp context-entry) (car context-entry) context-entry)))
      (and (stringp path) (not (file-directory-p path))
           (file-readable-p path) (gptel--file-binary-p path))))
  ;; Format and normalization are handled by media injection, although
  ;; one could modify these slots, say, to add media metadata into the
  ;; context.
  :normalize-fn nil
  :format-fn nil
  :display-fn #'gptel-context--display-file-media
  :transient-spec nil))

;;; Directory context type
(defun gptel-context--normalize-directory (context-entry)
"Normalize a directory context entry by expanding to file entries.
CONTEXT-ENTRY is a normalized entry of type `directory'."
(let* ((dir (car context-entry))
       (res))
  (unless gptel-context--reset-cache
    (setq gptel-context--reset-cache t)
    (run-at-time 0 nil
                 (lambda ()
                   (setq gptel-context--reset-cache nil
                         gptel-context--project-files nil))))
  (dolist (f (directory-files-recursively dir "."))
    (unless (gptel-context--skip-p f)
      (push (list f :type (if (gptel--file-binary-p f) 'file-media 'file)
                  :mime (and (gptel--file-binary-p f)
                             (mailcap-file-name-to-mime-type f)))
            res)))
  res))

;; No type-specific SPEC keys.
;; Expands to file and file-media entries at normalize time.
(gptel-context-register-type
 (make-gptel-context-type
  :name 'directory
  :description "A directory (expanded to file entries at collect time)."
  :predicate
  (lambda (context-entry)
    (let ((path (if (consp context-entry) (car context-entry) context-entry)))
      (and (stringp path) (file-directory-p path))))
  :normalize-fn #'gptel-context--normalize-directory
  :format-fn nil                    ; expands into individual file entries
  :display-fn nil                   ; no direct display
  :transient-spec nil))

(defun gptel-context--kill-ring-add (&optional _action)
  "Add the current kill to context as a `kill-ring' entry.

When called interactively, call `gptel-context-add-current-kill'.

ACTION is accepted for compatibility with the gptel context type
protocol but is unused (the `kill-ring' type is not on the DWIM hook)."
  (interactive)
  (call-interactively #'gptel-context-add-current-kill))

;;; Kill-ring context type
;; Stored internally as a buffer-region entry.  See buffer-region for
;; recognized SPEC keys.
(gptel-context-register-type
(make-gptel-context-type
 :name 'kill-ring
 :description "Contents of the kill ring (stored as a buffer region)."
 :predicate (lambda (_context-entry) nil) ; Never matched by predicate; only created via add-fn
 ;; normalize/format/display-fn deliberately omitted:
 ;; entries produced by :add-fn are typed as `buffer-region' by
 ;; predicate matching, so the buffer-region functions handle
 ;; normalization, formatting, and display.
 :transient-spec (list :key "C-y" :description "Yank to context"
                       :if (lambda () gptel-expert-commands))
 :add-fn #'gptel-context--kill-ring-add
 :remove-fn nil)
:add-dwim nil :remove-dwim nil)

(provide 'gptel-context)
;;; gptel-context.el ends here.
