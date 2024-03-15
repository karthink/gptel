
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
  "Determines if context should be injected when using the dedicated chat buffer."
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ------------------------------ FUNCTIONS ------------------------------- ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun gptel-add-context (&optional arg)
  "Add context to GPTel.

When called regularly, adds current buffer as context.
When ARG is positive, prompts for buffer name to add as context.
When ARG is negative, removes current buffer from context.
When called with region selected, adds selected region as context."
  (interactive)
  (cond
   ;; A region is selected.
   ((use-region-p)
    (gptel--add-region-as-context (current-buffer)
                                  (region-beginning)
                                  (region-end))
    (deactivate-mark)
    (message "Current region added as context."))
   ;; No region is currently selected, so delete a context under point if there
   ;; is one.
   ((gptel-context-at-point)
    (gptel-remove-context (gptel-context-at-point))
    (message "Context under point has been removed."))
   ;; No region is selected and no context is under point.  The default behavior
   ;; is to add the entire buffer as context.
   (t
    (gptel--add-region-as-context (current-buffer) (point-min) (point-max))
    (message "Current buffer added as context."))))
  
(defun gptel--make-context-overlay (start end)
  "Highlight the region from START to END."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face gptel-context-highlight-face)
    (overlay-put overlay 'gptel-context t)
    overlay))

(cl-defun gptel--add-region-as-context (buffer region-beginning region-end)
  "Add region delimited by REGION-BEGINNING, REGION-END in BUFFER as context."
  ;; Remove existing contexts in the same region, if any.
  (mapc #'gptel-remove-context
        (gptel-contexts-in-region buffer region-beginning region-end))
  (let ((start (make-marker))
        (end (make-marker)))
    (set-marker start region-beginning (current-buffer))
    (set-marker end region-end (current-buffer))
    ;; Trim the unnecessary parts of the context content.
    (let* ((content (buffer-substring-no-properties start end))
           (fat-at-end (progn
                         (let ((match-pos
                                (string-match-p (rx (+ (any "\t" "\n" " ")) eos)
                                                content)))
                           (when match-pos
                             (- (- end start) match-pos)))))
           (fat-at-start (progn
                           (when (string-match (rx bos (+ (any "\t" "\n" " ")))
                                               content)
                             (match-end 0)))))
      (when fat-at-start
        (set-marker start (+ start fat-at-start)))
      (when fat-at-end
        (set-marker end (- end fat-at-end))))
    (when (= start end)
      (message "No content in selected region.")
      (cl-return-from gptel--add-region-to-contexts nil))
    ;; First, highlight the region.
    (prog1 (gptel--make-context-overlay start end)
      (message "Region added to context buffer."))))

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
  (car (overlays-in (point) (point))))

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
  "Get the list of all context overlays in all active buffers."
  (cl-remove-if-not #'(lambda (ov)
                        (overlay-get ov 'gptel-context))
                    (let ((all-overlays '()))
                      (dolist (buf (buffer-list))
                        (with-current-buffer buf
                          (setq all-overlays
                                (append all-overlays
                                        (overlays-in (point-min)
                                                     (point-max))))))
                      all-overlays)))

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

(defun gptel--regions-continuous-p (buffer previous-region current-region)
  "Return non-nil if CURRENT-REGION is a continuation of PREVIOUS-REGION.
Pretains only to regions in BUFFER.

A region is considered a continuation of another if it is only separated by
newlines and whitespaces.  PREVIOUS-REGION and CURRENT-REGION should be cons
cells (START . END) representing the boundaries of the regions within BUFFER."
  (with-current-buffer buffer
    (let ((gap (buffer-substring-no-properties
                (cdr previous-region) (car current-region))))
      (string-match-p
       (rx bos (* (any "\t" "\n" " ")) eos)
       gap))))

(defun gptel-buffer-context-string (buffer)
  "Create a context string from all contexts in BUFFER."
    (let ((is-top-snippet t)
          buffer-file
          previous-region
          buffer-point-min
          buffer-point-max
          prog-lang-tag
          (contexts (gptel-contexts-in-buffer buffer)))
      (with-current-buffer buffer
        (setq buffer-point-min (save-excursion
                                 (goto-char (point-min))
                                 (skip-chars-forward " \t\n\r")
                                 (point))
              buffer-point-max (save-excursion
                                 (goto-char (point-max))
                                 (skip-chars-backward " \t\n\r")
                                 (point))
              prog-lang-tag (gptel-major-mode-md-prog-lang
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
                          (end (overlay-end context))
                          (region-inline
                           ;; Does the current region start on the same line the
                           ;; previous region ends?
                           (when previous-region
                             (gptel--region-inline-p buffer
                                                     previous-region
                                                     (cons start end))))
                          (region-continuous
                           ;; Is the current region a continuation of the
                           ;; previous region? I.e., is it only separated by
                           ;; newlines and whitespaces?
                           (when previous-region
                             (gptel--regions-continuous-p buffer
                                                          previous-region
                                                          (cons start end)))))
                     (unless (<= start buffer-point-min)
                       (if region-continuous
                           ;; If the regions are continuous, insert the
                           ;; whitespaces that separate them.
                           (insert-buffer-substring-no-properties
                            buffer
                            (cdr previous-region)
                            start)
                         ;; Regions are not continuous. Are they on the same
                         ;; line?
                         (if region-inline
                             ;; Region is inline but not continuous, so we
                             ;; should just insert an ellipsis.
                             (insert " ... ")
                           ;; Region is neither inline nor continuous, so just
                           ;; insert an ellipsis on a new line.
                           (unless is-top-snippet
                             (insert "\n"))
                           (insert "...")))
                       (let (lineno)
                         (with-current-buffer buffer
                           (setq lineno (line-number-at-pos start)))
                         ;; We do not need to insert a line number indicator on
                         ;; inline regions.
                         (unless (or region-inline region-continuous)
                           (insert (format " (Line %d)" lineno)))))
                     (when (or (and (not region-inline)
                                    (not region-continuous)
                                    (not is-top-snippet))
                               is-top-snippet)
                       (insert "\n"))
                     (if is-top-snippet
                         (setq is-top-snippet nil))
                     (let (substring)
                       (with-current-buffer buffer
                         (setq substring (buffer-substring-no-properties
                                          start end)))
                       ;; This text property will allow us to know what overlay
                       ;; is associated to which context.
                       (put-text-property 0 (length substring)
                                          'gptel-context-overlay
                                          context substring)
                       (insert substring))
                     (setq previous-region (cons start end)))))
        (unless (>= (overlay-end (car (last contexts))) buffer-point-max)
          (insert "\n..."))
        (insert "\n```")
        (buffer-substring (point-min) (point-max)))))

;;;###autoload
(defun gptel-context-string ()
  "Return the context string of all aggregated contexts."
  (string-trim-right
   (cl-loop for buffer in
            (delete-dups (mapcar #'overlay-buffer (gptel-contexts)))
            concat (concat (gptel-buffer-context-string buffer) "\n\n"))))

(provide 'gptel-contexter)
;;; gptel-contexter.el ends here.
