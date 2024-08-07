;;; gptel-context.el --- Context aggregator for GPTel  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Karthik Chikmagalur

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
;; to GPTel.

;;; Code:

;;; -*- lexical-binding: t -*-
(require 'gptel)
(require 'cl-lib)

(declare-function gptel-menu "gptel-transient")
(declare-function dired-get-marked-files "dired")

(defface gptel-context-highlight-face
  '((t :inherit header-line))
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

(defcustom gptel-context-wrap-function #'gptel-context--wrap-default
  "Function to format the context string sent with the gptel request.

This function receives two argument, the message to wrap with the
context, and an alist of contexts organized by buffer.  It should
return a string containing the message and the context, formatted as
necessary.

The message is either the system message or the last user prompt,
as configured by `gptel-use-context'.

The alist of contexts is structured as follows:

 ((buffer1 . (overlay1 overlay2)
  (\"path/to/file\")
  (buffer2 . (overlay3 overlay4 overlay5))))

Each gptel \"context\" is either a file path or an overlay in a
buffer.  Each overlay covers a buffer region containing the
context chunk.  This is accessible as, for example:

 (with-current-buffer buffer1
   (buffer-substring (overlay-start overlay1)
                     (overlay-end   overlay1)))"
  :group 'gptel
  :type 'function)

(defun gptel-context-add (&optional arg)
  "Add context to gptel in a DWIM fashion.

- If a region is selected, add the selected region to the
  context.  If there is already a gptel context at point, remove it
  instead.

- If in Dired, add marked files or file at point to the context.
  With negative prefix ARG, remove them from the context instead.

- Otherwise add the current buffer to the context.  With positive
  prefix ARG, prompt for a buffer name and add it to the context.

- With negative prefix ARG, remove all gptel contexts from the
  current buffer."
  (interactive "P")
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
    (mapc (if (and arg (< (prefix-numeric-value arg) 0))
              #'gptel-context-remove
              #'gptel-context-add-file)
          (dired-get-marked-files)))
   ;; No region is selected, and ARG is positive.
   ((and arg (> (prefix-numeric-value arg) 0))
    (let* ((buffer-name (read-buffer "Choose buffer to add as context: " nil t))
           (start (with-current-buffer buffer-name (point-min)))
           (end (with-current-buffer buffer-name (point-max))))
      (gptel-context--add-region
       (get-buffer buffer-name) start end t)
      (message "Buffer '%s' added as context." buffer-name)))
   ;; No region is selected, and ARG is negative.
   ((and arg (< (prefix-numeric-value arg) 0))
    (when (y-or-n-p "Remove all contexts from this buffer? ")
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
  
(defun gptel-context-add-file (path)
  "Add the file at PATH to the gptel context.

PATH should be readable as text."
  (interactive "fChoose file to add to context: ")
  (condition-case nil
      ;; Check if file is binary
      (if (with-temp-buffer ;; (create-file-buffer file)
            (insert-file-contents path nil 1 512 'replace)
            (eq buffer-file-coding-system 'no-conversion))
          (message "Ignoring unsupported binary file \"%s\"." path)
        (cl-pushnew (list path) gptel-context--alist :test #'equal)
        (message "File \"%s\" added to context." path)
        path)
    (file-missing (message "File \"%s\" does not exist." path))))

;;;###autoload (autoload 'gptel-add-file "gptel-context" "Add files to gptel's context." t)
(defalias 'gptel-add-file #'gptel-context-add-file)

(defun gptel-context-remove (&optional context)
  "Remove the CONTEXT overlay from the contexts list.
If CONTEXT is nil, removes the context at point.
If selection is active, removes all contexts within selection."
  (cond
   ((overlayp context)
    (delete-overlay context)
    ;; FIXME: Quadratic cost when clearing a bunch of contexts at once
    (unless
        (cl-loop
         for ov in (alist-get (current-buffer) gptel-context--alist)
         thereis (overlay-start ov))
      (setf (alist-get (current-buffer) gptel-context--alist nil 'remove) nil)))
   ((stringp context)                   ;file
    (setf (alist-get context gptel-context--alist nil 'remove #'equal) 
          nil))
   ((region-active-p)
    (when-let ((contexts (gptel-context--in-region (current-buffer)
                                                   (region-beginning)
                                                   (region-end))))
      (cl-loop for ctx in contexts do (delete-overlay ctx))))
   (t
    (when-let ((ctx (gptel-context--at-point)))
      (delete-overlay ctx)))))

(defun gptel-context--make-overlay (start end &optional advance)
  "Highlight the region from START to END."
  (let ((overlay (make-overlay start end nil (not advance) advance)))
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'face 'gptel-context-highlight-face)
    (overlay-put overlay 'gptel-context t)
    (push overlay (alist-get (current-buffer)
                             gptel-context--alist))
    overlay))

;;;###autoload
(defun gptel-context--wrap (message)
  (funcall gptel-context-wrap-function
           message (gptel-context--collect)))

(defun gptel-context--wrap-default (message contexts)
  "Add CONTEXTS to MESSAGE.

MESSAGE is usually either the system message or the user prompt.
The accumulated context from CONTEXTS is appended or prepended to
it, respectively."
  ;; Append context before/after system message.
  (let ((context-string (gptel-context--string contexts)))
    (if (> (length context-string) 0)
        (pcase-exhaustive gptel-use-context
          ('system (concat message "\n\n" context-string))
          ('user   (concat context-string "\n\n" message))
          ('nil    message))
      message)))

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
(defun gptel-context--collect ()
  "Get the list of all active context overlays."
  ;; Get only the non-degenerate overlays, collect them, and update the overlays variable.
  (setq gptel-context--alist
        (cl-loop for (buf . ovs) in gptel-context--alist
                 if (buffer-live-p buf)
                   if (cl-loop for ov in ovs when (overlay-start ov) collect ov)
                   collect (cons buf it) into elements
                   end
                 else if (and (stringp buf) (file-exists-p buf))
                   collect (list buf) into elements
                 finally return elements)))

(defun gptel-context--insert-buffer-string (buffer contexts)
  "Insert at point a context string from all CONTEXTS in BUFFER."
    (let ((is-top-snippet t)
          (previous-line 1))
      (insert (format "In buffer `%s`:" (buffer-name buffer))
              "\n\n```" (gptel--strip-mode-suffix (buffer-local-value
                                                   'major-mode buffer))
              "\n")
      (dolist (context contexts)
        (let* ((start (overlay-start context))
               (end (overlay-end context))
               content)
          (let (lineno column)
            (with-current-buffer buffer
              (without-restriction
                (setq lineno (line-number-at-pos start t)
                      column (save-excursion (goto-char start)
                                             (current-column))
                      content (buffer-substring-no-properties start end))))
            ;; We do not need to insert a line number indicator if we have two regions
            ;; on the same line, because the previous region should have already put the
            ;; indicator.
            (unless (= previous-line lineno)
              (unless (= lineno 1)
                (unless is-top-snippet
                  (insert "\n"))
                (insert (format "... (Line %d)\n" lineno))))
            (setq previous-line lineno)
            (unless (zerop column) (insert " ..."))
            (if is-top-snippet
                (setq is-top-snippet nil)
              (unless (= previous-line lineno) (insert "\n"))))
          (insert content)))
      (unless (>= (overlay-end (car (last contexts))) (point-max))
        (insert "\n..."))
      (insert "\n```")))

(defun gptel-context--insert-file-string (path)
  "Insert at point the contents of the file at PATH as context."
  (insert (format "In file `%s`:" (file-name-nondirectory path))
          "\n\n```\n")
  (insert-file-contents path)
  (goto-char (point-max))
  (insert "\n```\n"))

(defun gptel-context--string (context-alist)
  "Format the aggregated gptel context as annotated markdown fragments.

Returns a string.  CONTEXT-ALIST is a structure containing
context overlays, see `gptel-context--alist'."
  (with-temp-buffer
    (insert "Request context:\n\n")
    (cl-loop for (buf . ovs) in context-alist
             if (bufferp buf)
             do (gptel-context--insert-buffer-string buf ovs)
             else do (gptel-context--insert-file-string buf)
             do (insert "\n\n")
             finally do
             (skip-chars-backward "\n\t\r ")
             (delete-region (point) (point-max))
             finally return (buffer-string))))

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

(defun gptel-context--buffer-setup (&optional _ignore-auto _noconfirm)
  "Set up the gptel context buffer."
  (with-current-buffer (get-buffer-create "*gptel-context*")
    (gptel-context-buffer-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (setq header-line-format
            (concat
             (propertize "d" 'face 'help-key-binding) ": Mark/unmark deletion, "
             (propertize "n" 'face 'help-key-binding) "/"
             (propertize "p" 'face 'help-key-binding) ": jump to next/previous, "
             (propertize "C-c C-c" 'face 'help-key-binding) ": apply, "
             (propertize "C-c C-k" 'face 'help-key-binding) ": cancel, "
             (propertize "q" 'face 'help-key-binding) ": quit"))
      (save-excursion
        (let ((contexts gptel-context--alist))
          (if (length> contexts 0)
              (let (beg ov l1 l2)
                (pcase-dolist (`(,buf . ,ovs) contexts)
                  (if (bufferp buf)
                      ;; It's a buffer with some overlay(s)
                      (dolist (source-ov ovs)
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
                        (overlay-put ov 'gptel-context source-ov)
                        (overlay-put ov 'gptel-overlay t)
                        (overlay-put ov 'evaporate t)
                        (insert "\n" (make-separator-line) "\n"))
                    ;; BUF is a file path, not a buffer
                    (insert (propertize (format "In file %s:\n\n" (file-name-nondirectory buf))
                                        'face 'bold))
                    (setq beg (point))
                    (insert-file-contents buf)
                    (goto-char (point-max))
                    (insert "\n")
                    (setq ov (make-overlay beg (point)))
                    (overlay-put ov 'gptel-context buf)
                    (overlay-put ov 'gptel-overlay t)
                    (overlay-put ov 'evaporate t)
                    (insert "\n" (make-separator-line) "\n")))
                (goto-char (point-min)))
            (insert "There are no active gptel contexts.")))))
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
    (if-let* ((orig-ov (overlay-get ov-here 'gptel-context))
              (buf (overlay-buffer orig-ov))
              (offset (- (point) (overlay-start ov-here))))
        (with-selected-window (display-buffer buf)
          (goto-char (overlay-start orig-ov))
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
  (mapc #'gptel-context-remove
        (delq nil (mapcar (lambda (ov)
                            (and
                             (overlay-get ov 'gptel-context-deletion-mark)
                             (overlay-get ov 'gptel-context)))
                          (overlays-in (point-min) (point-max)))))
  (gptel-context-quit))

(provide 'gptel-context)
;;; gptel-context.el ends here.
