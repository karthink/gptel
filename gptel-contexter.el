;;; gptel-contexter.el --- Context aggregator for GPTel  -*- lexical-binding: t; -*-

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

;; The contexter allows you to conveniently create contexts which can be fed
;; to GPTel.

;;; Code:

;;; -*- lexical-binding: t -*-

(require 'cl-lib)

(defcustom gptel-context-highlight-face 'header-line
  "Face to use to highlight selected context in the buffers."
  :group 'gptel
  :type 'symbol)

(defcustom gptel-context-string-function #'gptel-context-string--default
  "Function to format the context string sent with the gptel request.

This function receives one argument, an alist of context overlays
organized by buffer.  It should return a string containing the
formatted context and any additional comments you wish to
include.

The alist of context overlays is structured as follows:

((buffer1 . (overlay1 overlay2)
 (buffer2 . (overlay3 overlay4 overlay5))))

Each overlay covers a buffer region containing the
context chunk.  This is accessible as, for example:

(with-current-buffer buffer1
  (buffer-substring (overlay-start overlay1)
                    (overlay-end   overlay1)))"
  :type 'function)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ------------------------------ FUNCTIONS ------------------------------- ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun gptel-add-context (&optional arg)
  "Add context to GPTel.

When called without a prefix argument, adds the current buffer as context.
When ARG is positive, prompts for a buffer name and adds it as context.
When ARG is negative, removes all contexts from the current buffer.
When called with a region selected, adds the selected region as context.

If there is a context under point, it is removed when called without a prefix."
  (interactive "P")
  (cond
   ;; A region is selected.
   ((use-region-p)
    (gptel--add-region-as-context (current-buffer)
                                  (region-beginning)
                                  (region-end))
    (deactivate-mark)
    (message "Current region added as context."))
   ;; No region is selected, and ARG is positive.
   ((and arg (> (prefix-numeric-value arg) 0))
    (let ((buffer-name (read-buffer "Choose buffer to add as context: " nil t)))
      (gptel--add-region-as-context (get-buffer buffer-name) (point-min) (point-max))
      (message "Buffer '%s' added as context." buffer-name)))
   ;; No region is selected, and ARG is negative.
   ((and arg (< (prefix-numeric-value arg) 0))
    (when (y-or-n-p "Remove all contexts from this buffer? ")
      (let ((removed-contexts 0))
        (cl-loop for cov in
                 (gptel-contexts-in-region (current-buffer) (point-min) (point-max))
                 do (progn
                      (cl-incf removed-contexts)
                      (gptel-remove-context cov)))
        (message (format "%d context%s removed from current buffer."
                         removed-contexts
                         (if (= removed-contexts 1) "" "s"))))))
   (t ; Default behavior
    (if (gptel-context-at-point)
        (progn
          (gptel-remove-context (car (gptel-contexts-in-region (current-buffer)
                                                               (max (point-min) (1- (point)))
                                                               (point))))
          (message "Context under point has been removed."))
      (gptel--add-region-as-context (current-buffer) (point-min) (point-max))
      (message "Current buffer added as context.")))))
  
(defun gptel--make-context-overlay (start end)
  "Highlight the region from START to END."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'face gptel-context-highlight-face)
    (overlay-put overlay 'gptel-context t)
    (push overlay (alist-get (current-buffer)
                             gptel-context--overlay-alist))
    overlay))

;;;###autoload
(defun gptel-context--wrap (message)
  "Wrap MESSAGE with context.

The message is usually either a system message or user prompt."
  ;; Append context before/after system message.
  (let ((context (gptel-context-string)))
    (if (> (length context) 0)
        (pcase-exhaustive gptel-use-context
          ('system (concat message "\n\n" context))
          ('user   (concat context "\n\n" message))
          ('nil    message))
      message)))

(cl-defun gptel--add-region-as-context (buffer region-beginning region-end)
  "Add region delimited by REGION-BEGINNING, REGION-END in BUFFER as context."
  ;; Remove existing contexts in the same region, if any.
  (mapc #'gptel-remove-context
        (gptel-contexts-in-region buffer region-beginning region-end))
    (prog1 (gptel--make-context-overlay region-beginning region-end)
      (message "Region added to context buffer.")))

;;;###autoload
(defun gptel-contexts-in-region (buffer start end)
  "Return the list of context overlays in the given region, if any, in BUFFER.
START and END signify the region delimiters."
  (with-current-buffer buffer
    (cl-remove-if-not (lambda (ov) (overlay-get ov 'gptel-context))
                      (overlays-in start end))))

;;;###autoload
(defun gptel-context-at-point ()
  "Return the context overlay at point, if any."
  (cl-find-if (lambda (ov) (overlay-get ov 'gptel-context))
              (overlays-at (point))))
    
;;;###autoload
(defun gptel-remove-context (&optional context)
  "Remove the CONTEXT overlay from the contexts list.
If CONTEXT is nil, removes the context at point.
If selection is active, removes all contexts within selection."
  (interactive)
  (cond
   ((overlayp context)
    (delete-overlay context))
   ((region-active-p)
    (let ((contexts (gptel-contexts-in-region (current-buffer)
                                              (region-beginning)
                                              (region-end))))
      (when contexts
        (cl-loop for ctx in contexts do (delete-overlay ctx)))))
   (t
    (let ((ctx (gptel-context-at-point)))
      (when ctx
        (delete-overlay ctx))))))

;;;###autoload
(defun gptel-contexts ()
  "Get the list of all active context overlays."
  ;; Get only the non-degenerate overlays, collect them, and update the overlays variable.
  (let ((overlay-alist
         (cl-loop for (buf . ovs) in gptel-context--overlay-alist
                  when (buffer-live-p buf)
                  for updated-ovs = (cl-loop for ov in ovs
                                             when (overlay-start ov)
                                             collect ov)
                  when updated-ovs
                  collect (cons buf updated-ovs))))
    (setq gptel-context--overlay-alist overlay-alist)))

;;;###autoload
(defun gptel-contexts-in-buffer (buffer)
  "Get the list of all context overlays in BUFFER."
  (cl-remove-if-not
   #'(lambda (ov)
       (overlay-get ov 'gptel-context))
   (let ((all-overlays '()))
     (with-current-buffer buffer
       (setq all-overlays
             (append all-overlays
                     (overlays-in (point-min)
                                  (point-max)))))
     all-overlays)))

;;;###autoload
(defun gptel-remove-all-contexts ()
  "Clear all contexts."
  (interactive)
  (mapc #'gptel-remove-context
        (gptel-contexts)))

(defun gptel--region-inline-p (buffer previous-region current-region)
  "Return non-nil if CURRENT-REGION begins on the line PREVIOUS-REGION ends in.
This check pertains only to regions in BUFFER.

PREVIOUS-REGION and CURRENT-REGION should be cons cells (START . END) which
representthe regions' boundaries within BUFFER."
  (with-current-buffer buffer
    (let ((prev-line-end (line-number-at-pos (cdr previous-region)))
          (curr-line-start (line-number-at-pos (car current-region))))
      (= prev-line-end curr-line-start))))

(defun gptel-buffer-insert-context-string (buffer)
  "Insert at point a context string from all contexts in BUFFER."
    (let ((is-top-snippet t)
          (previous-line 1)
          prog-lang-tag
          (contexts (alist-get buffer gptel-context--overlay-alist)))
      (setq prog-lang-tag (gptel--strip-mode-suffix
                             (buffer-local-value 'major-mode buffer)))
      (insert (format "In buffer `%s`:" (buffer-name buffer)))
      (insert "\n\n```" prog-lang-tag "\n")
      (cl-loop for context in contexts do
               (progn
                 (let* ((start (overlay-start context))
                        (end (overlay-end context)))
                   (let (lineno column)
                     (with-current-buffer buffer
                       (setq lineno (line-number-at-pos start t))
                       (setq column (save-excursion
                                      (goto-char start)
                                      (current-column))))
                     ;; We do not need to insert a line number indicator if we have two regions
                     ;; on the same line, because the previous region should have already put the
                     ;; indicator.
                     (unless (= previous-line lineno)
                       (unless (= lineno 1)
                         (unless is-top-snippet
                           (insert "\n"))
                         (insert (format "... (Line %d)\n" lineno))))
                     (setq previous-line lineno)
                     (unless (zerop column)
                       (insert " ..."))
                     (if is-top-snippet
                         (setq is-top-snippet nil)
                       (unless (= previous-line lineno)
                         (insert "\n"))))
                   (insert-buffer-substring-no-properties
                    buffer start end))))
      (unless (>= (overlay-end (car (last contexts))) (point-max))
        (insert "\n..."))
      (insert "\n```")))

(defun gptel-context-string--default (context-alist)
  (with-temp-buffer
    (insert "Request context:\n\n")
    (cl-loop for (buf . ovs) in context-alist
             do (gptel-buffer-insert-context-string buf)
             (insert "\n\n")
             finally return (buffer-string))))

;;;###autoload
(defun gptel-context-string ()
  "Return a string containing the aggregated gptel context."
  (funcall gptel-context-string-function
           (gptel-contexts)))

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
  (add-hook 'post-command-hook #'gptel--context-post-command
            nil t)
  (setq-local revert-buffer-function #'gptel--context-buffer-setup))

(defun gptel--context-buffer-setup (&optional _ignore-auto _noconfirm)
  (with-current-buffer (get-buffer-create "*gptel-context*")
    (gptel-context-buffer-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (setq header-line-format
            (concat
             "Mark/unmark deletion with "
             (propertize "d" 'face 'help-key-binding)
             ", jump to next/previous with "
             (propertize "n" 'face 'help-key-binding)
             "/"
             (propertize "p" 'face 'help-key-binding)
             ", respectively. "
             (propertize "C-c C-c" 'face 'help-key-binding)
             " to apply, or "
             (propertize "C-c C-k" 'face 'help-key-binding)
             " to abort."))
      (save-excursion
        (let ((contexts gptel-context--overlay-alist))
          (if (length> contexts 0)
              (let (beg ov l1 l2)
                (pcase-dolist (`(,buf . ,ovs) contexts)
                  (dolist (source-ov ovs)
                    (with-current-buffer buf
                      (setq l1 (line-number-at-pos (overlay-start source-ov))
                            l2 (line-number-at-pos (overlay-end source-ov))))
                    (insert (make-separator-line)
                            (propertize (format "In buffer %s (lines %d-%d):\n\n"
                                                (buffer-name buf) l1 l2)
                                        'face 'bold))
                    (setq beg (point))
                    (insert-buffer-substring
                     buf (overlay-start source-ov) (overlay-end source-ov))
                    (setq ov (make-overlay beg (point)))
                    (overlay-put ov 'gptel-context source-ov)
                    (overlay-put ov 'gptel-overlay t)
                    (insert "\n")))
                (goto-char (point-min)))
            (insert "There are no active contexts in any buffer.")))))
    (display-buffer (current-buffer)
                    `((display-buffer-reuse-window
                       display-buffer-reuse-mode-window
                       display-buffer-below-selected)
                      (body-function . ,#'select-window)
                      (window-height . ,#'fit-window-to-buffer)))))

(defvar gptel--context-buffer-reverse nil
  "Last direction of cursor movement in gptel context buffer.

If non-nil, indicates backward movement.")

(defalias 'gptel--context-post-command
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
  (interactive)
  (let ((ov-here (car (overlays-at (point)))))
    (if-let* ((orig-ov (overlay-get ov-here 'gptel-context))
              (buf (overlay-buffer orig-ov))
              (offset (- (point) (overlay-start ov-here))))
        (with-selected-window (display-buffer buf)
          (goto-char (overlay-start orig-ov))
          (forward-char offset)
          (recenter))
      (message "No source location for this context chunk."))))

(defun gptel-context-next ()
  (interactive)
  (let ((ov-here (car (overlays-at (point))))
        (next-start (next-overlay-change (point))))
    (when (and (/= (point-max) next-start) ov-here)
      ;; We were inside the overlay, so we want the next overlay change, which
      ;; would be the start of the next overlay.
      (setq next-start (next-overlay-change next-start)))
    (when (/= next-start (point-max))
      (setq gptel--context-buffer-reverse nil)
      (goto-char next-start))))

(defun gptel-context-previous ()
  (interactive)
  (let ((ov-here (car (overlays-at (point))))
        (previous-end (previous-overlay-change (point))))
    (when ov-here (goto-char (overlay-start ov-here)))
    (goto-char (previous-overlay-change
                (previous-overlay-change (point))))
    (setq gptel--context-buffer-reverse t)))

(defun gptel-context-flag-deletion ()
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
          (goto-char (overlay-start ov))
          (setq deletion-ov (make-overlay (overlay-start ov) (overlay-end ov)))
          (overlay-put deletion-ov 'gptel-context (overlay-get ov 'gptel-context))
          (overlay-put deletion-ov 'priority -80)
          (overlay-put deletion-ov 'face 'diff-indicator-removed)
          (overlay-put deletion-ov 'gptel-context-deletion-mark t))))
    (if (use-region-p)
        (deactivate-mark)
      (if gptel--context-buffer-reverse
          (gptel-context-previous)
        (gptel-context-next)))))

(defun gptel-context-quit ()
  (interactive)
  (quit-window)
  (call-interactively #'gptel-menu))

(defun gptel-context-confirm ()
  (interactive)
  ;; Delete all the context overlays that have been marked for deletion.
  (mapc #'delete-overlay
        (delq nil (mapcar (lambda (ov)
                            (and
                             (overlay-get ov 'gptel-context-deletion-mark)
                             (overlay-get ov 'gptel-context)))
                          (overlays-in (point-min) (point-max)))))
  (gptel-context-quit))

(provide 'gptel-contexter)
;;; gptel-contexter.el ends here.
