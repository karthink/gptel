
;;; gptel-contexter.el --- Context aggregator for GPTel

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

(defcustom gptel-use-context-in-chat nil
  "Determines if context should be injected when using the dedicated chat buffer.
If non-nil, then the model will use the context in the chat buffer."
  :group 'gptel
  :type 'symbol)

(defcustom gptel-context-injection-destination :nowhere
  "Where to inject the context.  Currently supported options are:

    :nowhere               - Do not use the context at all.
    :before-system-message - Inject the context right before the system message.
    :after-system-message  - Inject the context right after the system emssage.
    :before-user-prompt    - Inject the context right before the user prompt.
    :after-user-prompt     - Inject the context right after the user prompt."
  :group 'gptel
  :type 'symbol)

(defcustom gptel-context-preamble "Request context:"
  "A string to be prepended to the context."
  :group 'gptel
  :type 'string)

(defcustom gptel-context-postamble ""
  "A string to be appended to the context."
  :group 'gptel
  :type 'string)

(defvar gptel--context-overlays '())

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
    (push overlay gptel--context-overlays)
    overlay))

(defun gptel--wrap-in-context (message)
  "Wrap MESSAGE with context.
The message is usually either a system message or user prompt."
  ;; Append context before/after system message.
  (if (and (bound-and-true-p gptel-mode)
           (not gptel-use-context-in-chat))
      ;; If we are in the dedicated chat buffer, we would like to consult
      ;; `gptel-use-context-in-chat' to see if context should be used inside.
      message
    (let ((context (gptel-context-string)))
      (if (> (length context) 0)
          (if (memq gptel-context-injection-destination
                    '(:before-system-message
                      :before-user-prompt))
              (concat gptel-context-preamble
                      (when (not (zerop (length gptel-context-preamble))) "\n\n")
                      context
                      "\n\n"
                      gptel-context-postamble
                      (when (not (zerop (length gptel-context-postamble))) "\n\n")
                      message)
            (concat message
                    "\n\n"
                    gptel-context-preamble
                    (when (not (zerop (length gptel-context-preamble))) "\n\n")
                    context
                    (when (not (zerop (length gptel-context-postamble))) "\n\n")
                    gptel-context-postamble))
        message))))

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
    (cl-remove-if-not #'(lambda (overlay)
                          (overlay-get overlay 'gptel-context))
                      (overlays-in start end))))

;;;###autoload
(defun gptel-context-at-point ()
  "Return the context overlay at point, if any."
  (car (cl-remove-if-not #'(lambda (ov)
                             (overlay-get ov 'gptel-context))
                         (overlays-at (point)))))
    
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
  (let ((overlays (cl-loop for ov in gptel--context-overlays
                           when (overlay-start ov)
                           collect ov)))
    (setq gptel--context-overlays overlays)
    overlays))

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

;;;###autoload
(defun gptel-major-mode-md-prog-lang (mode)
  "Get the Markdown programming language string for the given MODE."
  (cond
   ((eq mode 'emacs-lisp-mode) "emacs-lisp")
   ((eq mode 'lisp-mode) "common-lisp")
   ((eq mode 'c-mode) "c")
   ((eq mode 'c++-mode) "c++")
   ((eq mode 'javascript-mode) "javascript")
   ((eq mode 'python-mode) "python")
   ((eq mode 'ruby-mode) "ruby")
   ((eq mode 'java-mode) "java")
   ((eq mode 'go-mode) "go")
   ((eq mode 'rust-mode) "rust")
   ((eq mode 'haskell-mode) "haskell")
   ((eq mode 'scala-mode) "scala")
   ((eq mode 'kotlin-mode) "kotlin")
   ((eq mode 'typescript-mode) "typescript")
   ((eq mode 'css-mode) "css")
   ((eq mode 'html-mode) "html")
   ((eq mode 'xml-mode) "xml")
   ((eq mode 'swift-mode) "swift")
   ((eq mode 'perl-mode) "perl")
   ((eq mode 'php-mode) "php")
   ((eq mode 'csharp-mode) "csharp")
   ((eq mode 'sql-mode) "sql")
   (t "")))

(defun gptel--region-inline-p (buffer previous-region current-region)
  "Return non-nil if CURRENT-REGION begins on the line PREVIOUS-REGION ends in.
This check pertains only to regions in BUFFER.

PREVIOUS-REGION and CURRENT-REGION should be cons cells (START . END) which
representthe regions' boundaries within BUFFER."
  (with-current-buffer buffer
    (let ((prev-line-end (line-number-at-pos (cdr previous-region)))
          (curr-line-start (line-number-at-pos (car current-region))))
      (= prev-line-end curr-line-start))))

(defun gptel-buffer-context-string (buffer &optional depropertize)
  "Create a context string from all contexts in BUFFER.
If DEPROPERTIZE is non-nil, remove the properties from the final substring."
    (let ((is-top-snippet t)
          buffer-file
          (previous-line 1)
          prog-lang-tag
          (contexts (gptel-contexts-in-buffer buffer)))
      (with-current-buffer buffer
        (setq prog-lang-tag (gptel-major-mode-md-prog-lang
                             major-mode)))
      (setq buffer-file
            ;; Use file path if buffer has one, otherwise use its regular name.
            (if (buffer-file-name buffer)
                (format "`%s`"
                        (buffer-file-name buffer))
              (format "buffer `%s`"
                      (buffer-name buffer))))
      (with-temp-buffer
        (insert (format "In %s:" buffer-file))
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
                           (insert (format "\n... (Line %d)\n" lineno))))
                       (setq previous-line lineno)
                       (unless (zerop column)
                         (insert " ..."))
                       (if is-top-snippet
                           (setq is-top-snippet nil)
                         (unless (= previous-line lineno)
                           (insert "\n"))))
                     (let (substring)
                       (with-current-buffer buffer
                         (setq substring (buffer-substring-no-properties
                                          start end)))
                       ;; This text property will allow us to know what overlay
                       ;; is associated to which context.
                       (put-text-property 0 (length substring)
                                          'gptel-context-overlay
                                          context substring)
                       (insert substring)))))
        (unless (>= (overlay-end (car (last contexts))) (point-max))
          (insert "\n..."))
        (insert "\n```")
        (let ((context-snippet (buffer-substring (point-min) (point-max))))
          (when depropertize
            (set-text-properties 0 (length context-snippet) nil context-snippet))
          context-snippet))))

;;;###autoload
(defun gptel-context-string (&optional propertize)
  "Return the context string of all aggregated contexts.
If PROPERTIZE is non-nil, keep the text properties."
  (without-restriction
    (let ((context (string-trim-right
                    (cl-loop for buffer in
                             (delete-dups (mapcar #'overlay-buffer (gptel-contexts)))
                             concat (concat (gptel-buffer-context-string buffer) "\n\n")))))
      (if propertize
          context
        (set-text-properties 0 (length context) nil context)
        context))))

(provide 'gptel-contexter)
;;; gptel-contexter.el ends here.
