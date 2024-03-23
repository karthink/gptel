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

(defvar gptel--context-buffer nil)
(defvar gptel--contexts-alist nil
  "An association list from buffers to a list of regions.")
(defvar gptel--current-highlight-region nil)
(defvar gptel--context-buffer-point nil)

;; We need a minor mode for a custom keymap, so that we may be able to remove
;; contexts directly from the contexts buffer.
(define-minor-mode context-mode
  "A minor mode for working with context."
  :lighter " Context"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "c")
                        'gptel-remove-context-at-point-from-context-buffer)
            map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ------------------------------ FUNCTIONS ------------------------------- ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gptel--highlight-region (start end)
  "Highlight the region from START to END."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'secondary-selection)
    (overlay-put overlay 'contexter t)))

(defun gptel--unhighlight-region (start end)
  "Remove highlighting from the region between START and END."
  (dolist (overlay (overlays-in start end))
    (when (and (overlay-get overlay 'contexter)
               (overlay-get overlay 'face))
      (delete-overlay overlay))))

(cl-defun gptel--add-region-to-contexts ()
  "Add the selected region to te contexts.
Order of the contexts in a buffer is determined by their order in the buffer."
  (unless (use-region-p)
    (error "No region selected"))
  (let ((start (make-marker))
        (end (make-marker)))
    (set-marker start (region-beginning) (current-buffer))
    (set-marker end (region-end) (current-buffer))
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
    (gptel--highlight-region start end)
    ;; Then, add the region to `gptel--contexts-alist', associating it with
    ;; the current buffer.
    (let ((existing-entry (assoc (current-buffer) gptel--contexts-alist)))
      (if (not existing-entry)
          (add-to-list 'gptel--contexts-alist
                       (cons (current-buffer) (list (list start end))))
        (let ((regions (cdr existing-entry))
              (new-region (list (list start end))))
          (setcdr existing-entry
                  (cl-sort (append regions new-region)
                           (lambda (a b)
                             (or (<= (car a) (car b)))))))))
    (message "Region added to context buffer.")
    t))

;;;###autoload
(defun gptel-context-in-region (buffer start end)
  "Return the context in the given region, if any, in BUFFER.
START and END signify the region delimiters.
A context in this function is a cons cell with the buffer as the CAR and the
region tuple as the CDR.  The tuple is a list, not a cons cell."
  (interactive)
  (let (context regions)
    (setq context (assoc buffer gptel--contexts-alist))
    (when context
      (cl-loop for (ctx-start ctx-end) in (cdr context)
               ;; If the current context range overlaps with the argument range.
               when (or (<= ctx-start start end ctx-end)
                        (<= ctx-start start ctx-end end)
                        (<= start ctx-start end ctx-end)
                        (<= start ctx-start ctx-end end))
               do (push (list ctx-start ctx-end) regions)))
    (if (not (zerop (length regions)))
        (setq context (cons buffer regions))
      (setq context nil))
    context))

;;;###autoload
(defun gptel-context-at-point ()
  "Return the context at point, if any.
A context in this function is a cons cell with the buffer as the CAR and the
region tuple as the CDR.  The tuple is a list, not a cons cell."
  (interactive)
  (let ((point (point))
        (buffer (current-buffer))
        context
        regions '())
    (setq context (assoc buffer gptel--contexts-alist))
    (when context
      (cl-loop for (start end) in (cdr context)
               when (and (>= point start) (<= point end))
               do (push (list start end) regions)))
    (if (not (zerop (length regions)))
        (setq context (cons buffer regions))
      nil)))

;;;###autoload
(defun gptel-remove-context (context &optional refresh-context-buffer)
  "Remove the CONTEXT snippet and unhighlight its region.
If REFRESH-CONTEXT-BUFFER is set to t, then also refresh the context buffer."
  (let* ((buffer (car context))
         (regions (cdr context)))
    ;; Unhighlight each region.
    (with-current-buffer buffer
      (cl-loop for (start end) in regions
               do (gptel--unhighlight-region start end)))
    ;; Remove regions from the list and clean up if empty.
    (setq gptel--contexts-alist
          (delq nil
                (mapcar (lambda (item)
                          (if (eq (car item) buffer)
                              (let ((new-value
                                     (seq-difference (cdr item) regions)))
                                (unless (seq-empty-p new-value)
                                  (cons (car item) new-value)))
                            item))
                        gptel--contexts-alist))))
  (when refresh-context-buffer
    (gptel--refresh-context-buffer)))

;;;###autoload
(cl-defun gptel-pop-or-push-context ()
  "Pop or push text into the context buffer depending on selection state.

If a region is selected, push the region as context.
If no region is selected, try to pop the context at the point.
If a region is selected but it contains contexts, pop all contexts within it and
add the region as a single context..

Popping context has no other meaning other than erasing it from the context
buffer."
  (interactive)
  (let ((context
         (if (use-region-p)
             (gptel-context-in-region (current-buffer)
                                      (region-beginning)
                                      (region-end))
           (gptel-context-at-point))))
    (when context
      (gptel-remove-context context))
    (when (use-region-p)
      (unless (gptel--add-region-to-contexts)
        (cl-return-from gptel-pop-or-push-context))
      (deactivate-mark)))
  (gptel--refresh-context-buffer))

;;;###autoload
(defun gptel-remove-all-contexts ()
  "Clear all saved context regions."
  (interactive)
  (mapc #'gptel-remove-context gptel--contexts-alist)
  (gptel--refresh-context-buffer))

(defun gptel--cleanup-killed-buffer ()
  "Remove contexts if their buffer was killed."
  (let ((context (assoc (current-buffer) gptel--contexts-alist)))
    (when context
      (setq gptel--contexts-alist (assoc-delete-all (current-buffer)
                                                    gptel--contexts-alist))
      (gptel--refresh-context-buffer))))

;; We don't care about the hook parameters.
(defun gptel--cleanup-degenerate-contexts (_ _ _)
  "Clean up contexts by degenerate regions."
  (let ((context (assoc (current-buffer) gptel--contexts-alist)))
    (when context
      (setcdr context
              (cl-remove-if (lambda (region)
                              (= (cl-first region) (cl-second region)))
                            (cdr context)))
      (when (zerop (length (cdr context))) ; All regions were removed!
        (setq gptel--contexts-alist
              (assoc-delete-all (current-buffer) gptel--contexts-alist)))
      (gptel--refresh-context-buffer))))

(defun gptel--sync-context-buffer (start end _)
  "See if the markers in the buffer have been changed between START and END.
If they have, update the context buffer."
  (when (gptel-context-in-region (current-buffer) start end)
    (gptel--refresh-context-buffer)))

(defun gptel--ensure-context-buffer-exists ()
  "Make sure the context buffer exists.  Create it if it does not."
  (with-current-buffer (get-buffer-create "*Context*") ; Create, if nonexistant.
    (setq gptel--context-buffer (current-buffer))
    ;; Ensure the minor mode context-mode is enabled in this buffer.
    (unless (bound-and-true-p context-mode)
      (context-mode 1))
    (read-only-mode 1))) ; Set read-only mode.

(defun gptel--sort-pairs-by-ascending-order (regions)
  "Sort the pairs in REGIONS by ascending order."
  (sort regions (lambda (a b) (< (car a) (car b)))))

;;;###autoload
(defun gptel--major-mode-md-prog-lang (mode)
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

(cl-defun gptel--compress-code (code given-major-mode)
  "Return a string which represents CODE with superfluous information removed.
GIVEN-MAJOR-MODE helps determine the method of compression."
  (with-temp-buffer
    (insert code)
    (goto-char (point-min))
    (cond
     ((member given-major-mode '(emacs-lisp-mode lisp-mode))
      (let (form-start form-end)
        (while (search-forward-regexp
                (concat "\\(("
                        (regexp-opt '("defun*" "defun" "defmacro" "cl-defun"))
                        "[^)]+\\)")
                nil
                t)
          ;; We are at the name of the form.
          (ignore-errors (while t (backward-up-list)))
          ;; We are at the start of the sexp.
          (setq form-start (point))
          ;; If error, exit function.
          (ignore-errors (forward-sexp)
                         (setq form-end (point)))
          (if (null form-end) (cl-return-from gptel--compress-code code))
          ;; We are at the end of the sexp.
          (goto-char form-start)
          (forward-char 1)
          ;; Docstring should be four sexps down the line.
          (forward-sexp)
          (forward-sexp)
          (forward-sexp)
          (forward-sexp)
          ;; If there is a docstring, we should be at its end.
          (when (eq (char-before) ?\")
            ;; We are, in fact, at the end of the docstring.
            ;; Get the indendation to be used for the next line.
            (backward-sexp)
            (let ((indentation (current-column)))
              (forward-sexp)
              ;; Remove everything from this point until the end of the form.
              (delete-region (point) form-end)
              (insert "\n" (make-string indentation ? )))
            (insert "...)"))))))
    (buffer-substring (point-min) (point-max))))

(defun gptel--regions-inline-p (buffer previous-region current-region)
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

(defun gptel-context-substring (buffer regions &optional compress-code)
  "Create a context substring from the REGIONS in BUFFER.
If COMPRESS-CODE is non-nil, try to compress code to save space.
REGIONS is a list of pairs of (start, end) lists."
  (with-temp-buffer
    (let ((buffer-file
           ;; Use file path if buffer has one, otherwise use its regular name.
           (if (buffer-file-name buffer)
               (format "`%s`"
                       (buffer-file-name buffer))
             (format "buffer `%s`"
                     (buffer-name buffer)))))
      (insert (format "In %s:" buffer-file)))
    (let ((is-top-snippet t)
          previous-region
          buffer-point-min
          buffer-point-max
          prog-lang-tag)
      (with-current-buffer buffer
        (setq buffer-point-min (save-excursion
                                 (goto-char (point-min))
                                 (skip-chars-forward " \t\n\r")
                                 (point))
              buffer-point-max (save-excursion
                                 (goto-char (point-max))
                                 (skip-chars-backward " \t\n\r")
                                 (point))
              prog-lang-tag (gptel--major-mode-md-prog-lang
                             major-mode)))
      (insert "\n\n```" prog-lang-tag "\n")
      (cl-loop for (start end) in regions do
               (progn
                 (let* ((region-inline
                         ;; Does the current region start on the same line the
                         ;; previous region ends?
                         (when previous-region
                           (gptel--regions-inline-p buffer
                                                    previous-region
                                                    (cons start end))))
                        (region-continuous
                         ;; Is the current region a continuation of the previous
                         ;; region? I.e., is it only separated by newlines and
                         ;; whitespaces?
                         (when previous-region
                           (gptel--regions-continuous-p buffer
                                                        previous-region
                                                        (cons start end)))))
                   (unless (<= start buffer-point-min)
                     (if region-continuous
                         ;; If the regions are continuous, insert the
                         ;; whitespaces that separate them.
                         (insert-buffer-substring buffer
                                                  (cdr previous-region)
                                                  start)
                       ;; Regions are not continuous. Are they on the same line?
                       (if region-inline
                           ;; Region is inline but not continuous, so we should
                           ;; just insert an ellipsis.
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
                         (insert (format " (Line %d)\n" lineno)))))
                   (when (and (not region-inline)
                              (not region-continuous)
                              (not is-top-snippet))
                     (insert "\n"))
                   (if is-top-snippet
                       (setq is-top-snippet nil))
                   (let (substring)
                     (with-current-buffer buffer
                       (setq substring
                             (if compress-code
                                 (gptel--compress-code
                                  (buffer-substring start end)
                                  major-mode)
                               (buffer-substring start end))))
                     (let (ss-start ss-end)
                       (setq ss-start (point))
                       (insert substring)
                       (setq ss-end (point))
                       ;; Save the context as a text property, so
                       ;; that we may later be able to delete the
                       ;; context snippet from the context buffer.
                       (put-text-property
                        ss-start ss-end
                        'gptel--context
                        (cons buffer
                              (list
                               (list start end)))))))
                 (setq previous-region (cons start end))))
      (unless (>= (cl-second (car (last regions))) buffer-point-max)
        (insert "\n..."))
      (insert "\n```"))
    (buffer-substring (point-min) (point-max))))

(defun gptel--refresh-context-buffer ()
  "Write the actual context to the context buffer.
This might require optimization later on."
  (gptel--ensure-context-buffer-exists)
  (setq gptel--current-highlight-region nil)
  (with-current-buffer gptel--context-buffer
    (with-silent-modifications
      (erase-buffer)
      (cl-loop for (buffer . regions) in gptel--contexts-alist do
               (progn
                 (unless (= (point) (point-min))
                   (insert "\n\n"))
                 (insert (gptel-context-substring buffer regions t)))))))

;;;###autoload
(defun gptel-remove-context-at-point-from-context-buffer ()
  "Remove the context entry at the current point inside the context buffer.
Does nothing if there is no entry at the current point."
  (interactive)
  (unless (eq (current-buffer) gptel--context-buffer)
    (error "This function can only be used inside the context buffer"))
  (let ((context (get-text-property (point) 'gptel--context)))
    (gptel-remove-context context))
  (gptel--refresh-context-buffer)
  (when (> gptel--context-buffer-point (point-max))
    (setq gptel--context-buffer-point (point-max)))
  (goto-char gptel--context-buffer-point)
  (gptel--highlight-selected-context-in-context-buffer))

(defun gptel--text-property-from-point-region (property from-point)
  "Find the region around FROM-POINT that has a specific text PROPERTY."
  (let ((start (or from-point (point)))
        (end (or from-point (point))))
    ;; Search backwards for the start of the property region.
    (while (and (> start (point-min))
                (get-text-property (1- start) property))
      (setq start (1- start)))
    ;; If the exact property is not at the start, move one character forward.
    (unless (get-text-property start property)
      (setq start (next-single-property-change start property nil (point-max))))
    ;; Search forwards for the end of the property region.
    (while (and (< end (point-max))
                (get-text-property end property))
      (setq end (1+ end)))
    ;; Return the region as a list.
    (list start end)))

(defun gptel--highlight-selected-context-in-context-buffer ()
  "Highlight the selected context within the context buffer.
In essence, just highlights the context where the point is in the buffer."
  (unless (eq (current-buffer) gptel--context-buffer)
    (error "This function can only be used inside the context buffer"))
  (let* ((context (get-text-property (point) 'gptel--context))
         (current-region (gptel--text-property-from-point-region
                          'gptel--context
                          (point)))
         (start (cl-first current-region))
         (end (cl-second current-region))
         (current-start (cl-first gptel--current-highlight-region))
         (current-end (cl-second gptel--current-highlight-region)))
    (when (and gptel--current-highlight-region
               (not (and (= start current-start)
                         (= end current-end))))
      ;; Current context is not the one under the point, so unhighlight it.
      (gptel--unhighlight-region current-start current-end)
      (setq gptel--current-highlight-region nil))
    (when context
      (gptel--highlight-region start end)
      (setq gptel--current-highlight-region (list start end)))))

(cl-defun gptel--monitor-context-buffer-point-change ()
  "Monitor change in the point position within the context buffer.
Used mainly for selecting contexts when the point has moved."
  (unless (eq (current-buffer) gptel--context-buffer)
    (cl-return-from gptel--monitor-context-buffer-point-change))
  (unless (and gptel--context-buffer-point
               (= (point) gptel--context-buffer-point))
    (setq gptel--context-buffer-point (point))
    (gptel--highlight-selected-context-in-context-buffer)))

;;;###autoload
(defun gptel-context-string ()
  "Return the contents of the context buffer."
  (gptel--ensure-context-buffer-exists)
  (with-current-buffer gptel--context-buffer
    (buffer-substring-no-properties (point-min) (point-max))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -------------------------------- HOOKS --------------------------------- ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'kill-buffer-hook #'gptel--cleanup-killed-buffer)
(add-hook 'after-change-functions
          #'gptel--cleanup-degenerate-contexts
          #'gptel--sync-context-buffer)
(add-hook 'post-command-hook #'gptel--monitor-context-buffer-point-change)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ---------------------------- INITIALIZATION ---------------------------- ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Just make sure an empty buffer exists.
(gptel--refresh-context-buffer)

(provide 'contexter)
;;; gptel-contexter.el ends here.
