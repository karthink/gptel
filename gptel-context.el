;;; gptel-context.el --- Context aggregator for gptel  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025  Karthik Chikmagalur

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

;; The context allows you to conveniently create contexts which can be fed
;; to gptel.

;;; Code:

(require 'gptel)
(require 'cl-lib)
(require 'project)

(declare-function gptel-menu "gptel-transient")
(declare-function dired-get-marked-files "dired")
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

Synchronous: An alist of contexts with buffers or files (the context
alist).
Asynchronous: A callback to call with the result, and the context alist.

Entries in the context alist can have one of these forms:

 (buffer1 overlay1 overlay2 ...)             ;text overlays in a buffer
 (buffer2)                                   ;a buffer object
 (\"/path/to/file\")                         ;a text file
 (\"/path/to/file\" :mime \"text/markdown\") ;with explicit mime type
 (\"/path/to/image\" :mime \"image/jpeg\")   ;media file

Each overlay covers a buffer region containing the
context chunk.  This is accessible as, for example:

 (with-current-buffer buffer1
   (buffer-substring (overlay-start overlay1)
                     (overlay-end   overlay1)))"
  :group 'gptel
  :type 'function)

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

;;; Commands

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
  "Add context to gptel in a DWIM fashion.

- If a region is selected, add the selected region to the
  context.  If there is already a gptel context at point, remove it
  instead.

- If in Dired, add marked files or file at point to the context.  If
  the selection includes directories, add all their files recursively,
  prompting the user for confirmation if called interactively or
  CONFIRM is non-nil.  With negative prefix ARG, remove all files from
  the context instead.

- Otherwise add the current buffer to the context.  With positive
  prefix ARG, prompt for a buffer name and add it to the context.

- With negative prefix ARG, remove all gptel contexts from the current
  buffer, prompting the user for confirmation if called interactively
  or CONFIRM is non-nil."
  (interactive "P\np")
  (cond
   ;; A region is selected.
   ((use-region-p)
    (gptel-context--add-region (current-buffer)
                               (region-beginning)
                               (region-end))
    (deactivate-mark)
    (message "Current region added as context."))
   ;; If in dired
   ((derived-mode-p 'dired-mode)
    (let* ((files (dired-get-marked-files))
           (dirs (cl-remove-if-not #'file-directory-p files))
           (remove-p (< (prefix-numeric-value arg) 0))
	   (action-fn (if remove-p
			  #'gptel-context-remove
			#'gptel-context-add-file)))
      (when (or remove-p (null dirs) (null confirm)
		(y-or-n-p (format "Recursively add files from %d director%s? "
				  (length dirs)
				  (if (= (length dirs) 1) "y" "ies"))))
	(mapc action-fn files))))
   ;; If in an image buffer
   ((and (derived-mode-p 'image-mode)
	 (gptel--model-capable-p 'media)
	 (buffer-file-name)
	 (not (gptel-context--skip-p (buffer-file-name))))
    (funcall (if (and arg (< (prefix-numeric-value arg) 0))
                 #'gptel-context-remove
               #'gptel-context-add-file)
             (buffer-file-name)))
   ;; No region is selected, and ARG is positive.
   ((and arg (> (prefix-numeric-value arg) 0))
    (let* ((buffer-name (read-buffer "Choose buffer to add as context: "
                                     (current-buffer) t))
           (start (with-current-buffer buffer-name (point-min)))
           (end (with-current-buffer buffer-name (point-max))))
      (gptel-context--add-region
       (get-buffer buffer-name) start end t)
      (message "Buffer '%s' added as context." buffer-name)))
   ;; No region is selected, and ARG is negative.
   ((and arg (< (prefix-numeric-value arg) 0))
    (when (or (null confirm)
	      (y-or-n-p "Remove all contexts from this buffer? "))
      (let ((removed-contexts 0))
        (cl-loop for cov in
                 (gptel-context--in-region (current-buffer) (point-min) (point-max))
                 do (progn
                      (cl-incf removed-contexts)
                      (gptel-context-remove cov)))
        (message (format "%d context%s removed from current buffer."
                         removed-contexts
                         (if (= removed-contexts 1) "" "s"))))))
   (t ; Default behavior
    (if (gptel-context--at-point)
        (progn
          (gptel-context-remove (car (gptel-context--in-region (current-buffer)
                                                               (max (point-min) (1- (point)))
                                                               (point))))
          (message "Context under point has been removed."))
      (gptel-context--add-region (current-buffer) (point-min) (point-max) t)
      (message "Current buffer added as context.")))))

;;;###autoload (autoload 'gptel-add "gptel-context" "Add/remove regions or buffers from gptel's context." t)
(defalias 'gptel-add #'gptel-context-add)

(defun gptel-context--add-text-file (path)
  "Add text file at PATH to context."
  (cl-pushnew (list path) gptel-context :test #'equal)
  (message "File \"%s\" added to context." path)
  path)

(defun gptel-context--add-binary-file (path)
  "Add binary file at PATH to context if supported.
Return PATH if added, nil if ignored."
  (if-let* (((gptel--model-capable-p 'media))
            (mime (mailcap-file-name-to-mime-type path))
            ((gptel--model-mime-capable-p mime)))
      (prog1 path
        (cl-pushnew (list path :mime mime)
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

If CONTEXT is nil, removes the context at point.
If selection is active, removes all contexts within selection.
If CONTEXT is a directory, recursively removes all files in it."
  (cond
   ((overlayp context)                  ;Overlay in buffer
    (delete-overlay context)
    ;; FIXME: Quadratic cost when clearing a bunch of contexts at once
    (unless
        (cl-loop
         for ov in (alist-get (current-buffer) gptel-context)
         thereis (overlay-start ov))
      (setf (alist-get (current-buffer) gptel-context nil 'remove) nil)))
   ((bufferp context)                   ;Full buffer
    (setf (alist-get context gptel-context nil 'remove) nil))
   ((stringp context)                   ;file or directory
    (if (file-directory-p context)
        (gptel-context--add-directory context 'remove)
      (setf (alist-get context gptel-context nil 'remove #'equal) nil)
      (message "File \"%s\" removed from context." context)))
   ((region-active-p)                   ;Overlays in region
    (when-let* ((contexts (gptel-context--in-region (current-buffer)
                                                    (region-beginning)
                                                    (region-end))))
      (cl-loop for ctx in contexts do (delete-overlay ctx))))
   (t                                   ;Anything at point
    (when-let* ((ctx (gptel-context--at-point)))
      (delete-overlay ctx)))))

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
       for (source . ovs) = (ensure-list context)
       if (cl-every #'overlayp ovs) do           ;Buffers and buffer regions
       (mapc #'gptel-context-remove ovs)
       else do (gptel-context-remove source) ;files or other types
       finally do (setq gptel-context nil)))
    (when verbose (message "Removed all gptel context sources."))))

;;; Context wrap
(defun gptel-context--make-overlay (start end &optional advance)
  "Highlight the region from START to END.

ADVANCE controls the overlay boundary behavior."
  (let ((overlay (make-overlay start end nil (not advance) advance)))
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'face 'gptel-context-highlight-face)
    (overlay-put overlay 'gptel-context t)
    (push overlay (alist-get (current-buffer)
                             gptel-context))
    overlay))

;;;###autoload
(defun gptel-context--wrap (callback data-buf)
  "Add request context to DATA-BUF and run CALLBACK.

DATA-BUF is the buffer where the request prompt is constructed."
  (if (= (car (func-arity gptel-context-string-function)) 2)
      (funcall gptel-context-string-function
               (lambda (c) (with-current-buffer data-buf
                             (gptel-context--wrap-in-buffer c))
                 (funcall callback))
               (gptel-context--collect))
    (with-current-buffer data-buf
      (thread-last (gptel-context--collect)
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
         (if gptel--system-message
             (cl-etypecase gptel--system-message
               (string
                (setq gptel--system-message
                      (concat gptel--system-message "\n\n" context-string)))
               (function
                (setq gptel--system-message
                      (gptel--parse-directive gptel--system-message 'raw))
                (gptel-context--wrap-in-buffer context-string))
               (list
                (setq gptel--system-message ;cons a new list to avoid mutation
                      (cons (concat (car gptel--system-message) "\n\n" context-string)
                            (cdr gptel--system-message)))))
           (setq gptel--system-message context-string))))
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

(defun gptel-context--collect-media (&optional contexts)
  "Collect media CONTEXTS.

CONTEXTS, which are typically paths to binary files, are
base64-encoded and prepended to the first user prompt."
  (cl-loop for context in (or contexts gptel-context)
           for (path . props) = (ensure-list context)
           when (and (stringp path) (plist-get props :mime))
           collect (cons :media context)))

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
(defun gptel-context--collect (&optional context-alist)
  "Get the list of all active context sources from CONTEXT-ALIST.

CONTEXT-ALIST defaults to the current value of `gptel-context'.

Ignore overlays, buffers and files that are not live or readable."
  ;; Get only the non-degenerate overlays, collect them, and update the overlays variable.
  (let ((res))
    (dolist (entry (or context-alist gptel-context))
      (pcase entry                      ;Context entry is:
        (`(,buf . ,ovs)
         (cond
          ((buffer-live-p buf)          ;Overlay(s) in a buffer
           (if-let* ((live-ovs (cl-loop for ov in ovs
                                        when (overlay-start ov)
                                        collect ov)))
               (push (cons buf live-ovs) res)))
          ((and (stringp buf) (file-readable-p buf))
           (push (cons buf ovs) res)))) ;A file list with (maybe) a mimetype

        ((and (pred stringp) (pred file-readable-p)) ;Just a file, figure out mimetype
         (if (file-directory-p entry)
             (progn
               (unless gptel-context--reset-cache
                 (setq gptel-context--reset-cache t)
                 (run-at-time
                  0 nil
                  (lambda () (setq gptel-context--reset-cache nil
                              gptel-context--project-files nil))))
               (dolist (f (directory-files-recursively entry "."))
                 (unless (gptel-context--skip-p f)
                   (push `(,f ,@(and (gptel--file-binary-p f)
                                     (list :mime (mailcap-file-name-to-mime-type entry))))
                         res))))
           (push `(,entry ,@(and (gptel--file-binary-p entry)
                                 (list :mime (mailcap-file-name-to-mime-type entry))))
                 res)))
        ((pred buffer-live-p) (push (list entry) res)))) ;Just a buffer
    (nreverse res)))

(defun gptel-context--insert-buffer-string (buffer overlays)
  "Insert at point a context string from all OVERLAYS in BUFFER.

If OVERLAYS is nil add the entire buffer text."
  (let ((is-top-snippet t)
        (previous-line 1))
    (insert (format "In buffer `%s`:" (buffer-name buffer))
            "\n\n```" (gptel--strip-mode-suffix (buffer-local-value
                                                 'major-mode buffer))
            "\n")
    (if (not overlays)
        (insert-buffer-substring-no-properties buffer)
      (dolist (context overlays)
        (let* ((start (overlay-start context))
               (end (overlay-end context)))
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
      (unless (>= (overlay-end (car (last overlays))) (point-max))
        (insert "\n...")))
    (insert "\n```")))

(defun gptel-context--string (context-alist)
  "Format the aggregated gptel context as annotated markdown fragments.

Returns a string.  CONTEXT-ALIST is a structure containing
context overlays, see `gptel-context'."
  (with-temp-buffer
    (cl-loop for entry in context-alist
             for (buf . ovs) = (ensure-list entry)
             if (bufferp buf)
             do (gptel-context--insert-buffer-string buf ovs)
             else if (or (not (plist-get ovs :mime))
                         (string-match-p "^text/" (plist-get ovs :mime)))
             do (gptel--insert-file-string buf) end
             do (insert "\n\n")
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

CONTEXT-ALIST is the alist of contexts to use to populate the buffer."
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
        (let ((contexts (gptel-context--collect context-alist)))
          (if (length= contexts 0)
              (insert "There are no active gptel contexts.")
            (let (beg ov l1 l2)
              (pcase-dolist (`(,buf . ,ovs) contexts)
                (cond
                 ((bufferp buf)
                  (if (not ovs)     ;BUF is a full buffer, not specific overlays
                      (progn
                        (insert (propertize (format "In buffer %s:\n\n"
                                                    (buffer-name buf))
                                            'face 'bold))
                        (setq beg (point))
                        (insert-buffer-substring buf)
                        (insert "\n")
                        (setq ov (make-overlay beg (point))))
                    (dolist (source-ov ovs) ;BUF is a buffer with some overlay(s)
                      (with-current-buffer buf
                        (setq l1 (line-number-at-pos (overlay-start source-ov))
                              l2 (line-number-at-pos (overlay-end source-ov))))
                      (insert (propertize (format "In buffer %s (lines %d-%d):\n\n"
                                                  (buffer-name buf) l1 l2)
                                          'face 'bold))
                      (setq beg (point))
                      (insert-buffer-substring
                       buf (overlay-start source-ov) (overlay-end source-ov))
                      (insert "\n")
                      (setq ov (make-overlay beg (point)))
                      (overlay-put ov 'gptel-context source-ov)))
                  (overlay-put ov 'gptel-overlay t)
                  (overlay-put ov 'evaporate t)
                  (insert "\n" (make-separator-line) "\n"))
                 (t                     ;BUF is a file path, not a buffer
                  (insert (propertize (format "In file %s:\n\n" (file-name-nondirectory buf))
                                      'face 'bold))
                  (setq beg (point))
                  (if-let* ((mime (plist-get ovs :mime))
                            ((not (string-match-p "^text/" mime)))) ;BUF is a binary file
                      (if-let* (((string-match-p (image-file-name-regexp) buf))
                                (img (create-image buf)))
                          (insert-image img "*") ; Can be displayed
                        (insert
                         buf " " (propertize "(No preview for binary file)"
                                             'face '(:inherit shadow :slant italic))))
                    (insert-file-contents buf))
                  (goto-char (point-max))
                  (insert "\n")
                  (setq ov (make-overlay beg (point)))
                  (overlay-put ov 'gptel-context buf)
                  (overlay-put ov 'gptel-overlay t)
                  (overlay-put ov 'evaporate t)
                  (insert "\n" (make-separator-line) "\n"))))
              (goto-char (point-min)))))))
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
    ;; FIXME(context): This should run in the buffer from which the context
    ;; inspection buffer was visited.
    ;; Update contexts and revert buffer (#482)
    (setq gptel-context (gptel-context--collect))
    (revert-buffer))
  (gptel-context-quit))

(provide 'gptel-context)
;;; gptel-context.el ends here.
