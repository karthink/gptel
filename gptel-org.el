;;; gptel-org.el --- Org functions for gptel         -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Keywords: 

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

;; 

;;; Code:
(eval-when-compile (require 'cl-lib))
(require 'org-element)
(require 'outline)

(declare-function org-at-heading-p "org")


;;; User options
(defcustom gptel-org-branching-context nil
  "Use the lineage of the current heading as the context for gptel in Org buffers.

This makes each same level heading a separate conversation
branch.

By default, gptel uses a linear context: all the text up to the
cursor is sent to the LLM.  Enabling this option makes the
context the hierarchical lineage of the current Org heading.  In
this example:

-----
Top level text

* Heading 1
heading 1 text

* Heading 2
heading 2 text

** Heading 2.1
heading 2.1 text
** Heading 2.2
heading 2.2 text
-----

With the cursor at the end of the buffer, the text sent to the
LLM will be limited to

-----
Top level text

* Heading 2
heading 2 text

** Heading 2.2
heading 2.2 text
-----

This makes it feasible to have multiple conversation branches."
  :local t
  :type 'boolean
  :group 'gptel)


;;; Setting context and creating queries
(defun gptel-org--get-topic-start ()
  "If a conversation topic is set, return it."
  (pcase major-mode
    ('org-mode
     (when (org-entry-get (point) "GPTEL_TOPIC" 'inherit)
         (marker-position org-entry-property-inherited-from)))
    ('markdown-mode nil)))

(defun gptel-org-set-topic (topic)
  "Set a topic and limit this conversation to the current heading.

This limits the context sent to the LLM to the text between the
current heading and the cursor position."
  (interactive
   (list
    (progn
      (or (derived-mode-p 'org-mode)
          (user-error "Support for multiple topics per buffer is only implemented for `org-mode'."))
      (completing-read "Set topic as: "
                       (org-property-values "GPTEL_TOPIC")
                       nil nil (downcase
                                (truncate-string-to-width
                                 (substring-no-properties
                                  (replace-regexp-in-string
                                   "\\s-+" "-"
                                   (org-get-heading)))
                                 50))))))
  (when (stringp topic) (org-set-property "GPTEL_TOPIC" topic)))

(defun gptel-org--create-prompt (&optional prompt-end)
  "Return a full conversation prompt from the contents of this Org buffer.

If `gptel--num-messages-to-send' is set, limit to that many
recent exchanges.

The prompt is constructed from the contents of the buffer up to
point, or PROMPT-END if provided.  Its contents depend on the
value of `gptel-org-branching-context', which see."
  (unless prompt-end (setq prompt-end (point)))
  (let ((max-entries (and gptel--num-messages-to-send
                          (* 2 gptel--num-messages-to-send)))
        (topic-start (gptel--get-topic-start)))
    (when topic-start
      ;; narrow to GPTEL_TOPIC property scope
      (narrow-to-region topic-start prompt-end))
    (if gptel-org-branching-context
        ;; Create prompt from direct ancestors of point
        (save-excursion
          (let* ((org-buf (current-buffer))
                 (start-bounds (org-element-lineage-map
                                   (org-element-at-point) #'org-element-begin
                                 '(headline org-data) 'with-self))
                 (end-bounds
                  (cl-loop
                   for pos in (cdr start-bounds)
                   while
                   (and (>= pos (point-min)) ;respect narrowing
                        (goto-char pos)
                        ;; org-element-lineage always returns an extra
                        ;; (org-data) element at point 1.  If there is also a
                        ;; heading here, it is either a false positive or we
                        ;; would be double counting it.  So we reject this node
                        ;; when also at a heading.
                        (not (and (eq pos 1) (org-at-heading-p))))
                   do (outline-next-heading)
                   collect (point) into ends
                   finally return (cons prompt-end ends))))
            (with-temp-buffer
              (setq gptel-backend
                    (buffer-local-value 'gptel-backend org-buf)
                    gptel--system-message
                    (buffer-local-value 'gptel--system-message org-buf)
                    gptel-model
                    (buffer-local-value 'gptel-model org-buf))
              (cl-loop for start in start-bounds
                       for end   in end-bounds
                       do (insert-buffer-substring org-buf start end)
                       (goto-char (point-min)))
              (goto-char (point-max))
              (let ((major-mode 'org-mode))
                (gptel--parse-buffer gptel-backend max-entries)))))
      ;; Create prompt the usual way
      (gptel--parse-buffer gptel-backend max-entries))))



(provide 'gptel-org)
;;; gptel-org.el ends here
