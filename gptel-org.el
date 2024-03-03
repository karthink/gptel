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


;;; Saving and restoring state
(defun gptel-org--entry-properties (&optional pt)
  "Find gptel configuration properties stored in the current heading."
  (pcase-let
      ((`(,system ,backend ,model ,temperature ,tokens)
         (mapcar
          (lambda (prop) (org-entry-get (or pt (point)) prop 'selective))
          '("GPTEL_SYSTEM" "GPTEL_BACKEND" "GPTEL_MODEL"
            "GPTEL_TEMPERATURE" "GPTEL_MAX_TOKENS"))))
    (when system
      (setq system (string-replace "\\n" "\n" system)))
    (when backend
      (setq backend (alist-get backend gptel--known-backends
                               nil nil #'equal)))
    (when temperature
      (setq temperature (gptel--numberize temperature)))
    (when tokens (setq tokens (gptel--numberize tokens)))
    (list system backend model temperature tokens)))

  ;; (pcase-let ((`(,gptel--system-message ,gptel-backend
  ;;                ,gptel-model ,gptel-temperature)
  ;;              (if (derived-mode-p 'org-mode)
  ;;                  (progn (require 'gptel-org)
  ;;                         (gptel-org--entry-properties))
  ;;                `(,gptel--system-message ,gptel-backend
  ;;                  ,gptel-model ,gptel-temperature)))))
  
(defun gptel-org--restore-state ()
  "Restore gptel state for Org buffers when turning on `gptel-mode'."
  (save-restriction
    (widen)
    (condition-case status
        (progn
          (when-let ((bounds (org-entry-get (point-min) "GPTEL_BOUNDS")))
            (mapc (pcase-lambda (`(,beg . ,end))
                    (put-text-property beg end 'gptel 'response))
                  (read bounds)))
          (pcase-let ((`(,system ,backend ,model ,temperature ,tokens)
                       (gptel-org--entry-properties (point-min))))
            (when system (setq-local gptel--system-message system))
            (if backend (setq-local gptel-backend backend)
              (message
               (substitute-command-keys
                (concat
                 "Could not activate gptel backend \"%s\"!  "
                 "Switch backends with \\[universal-argument] \\[gptel-send]"
                 " before using gptel."))
               backend))
            (when model (setq-local gptel-model model))
            (when temperature (setq-local gptel-temperature temperature))
            (when tokens (setq-local gptel-max-tokens tokens))))
      (:success (message "gptel chat restored."))
      (error (message "Could not restore gptel state, sorry! Error: %s" status)))))

(defun gptel-org-set-config (pt &optional msg)
  "Store the active gptel configuration under the current heading.

The active gptel configuration includes the current system
message, language model and provider (backend), and additional
settings when applicable.

PT is the curosr position by default. If MSG is
non-nil (default), display a message afterwards."
  (interactive (list (point) t))
  (org-entry-put pt "GPTEL_MODEL" gptel-model)
  (org-entry-put pt "GPTEL_BACKEND" (gptel-backend-name gptel-backend))
  (unless (equal (default-value 'gptel-temperature) gptel-temperature)
    (org-entry-put pt "GPTEL_TEMPERATURE"
                   (number-to-string gptel-temperature)))
  (unless (string= (default-value 'gptel--system-message)
                   gptel--system-message)
    (org-entry-put pt "GPTEL_SYSTEM"
                   (string-replace "\n" "\\n" gptel--system-message)))   
  (when gptel-max-tokens
    (org-entry-put
     pt "GPTEL_MAX_TOKENS" (number-to-string gptel-max-tokens)))
  (when msg
    (message "Added gptel configuration to current headline.")))

(defun gptel-org--save-state ()
  "Write the gptel state to the Org buffer as Org properties."
  (org-with-wide-buffer
   (goto-char (point-min))
   (when (org-at-heading-p)
     (org-open-line 1))
   (gptel-org-set-config (point-min))
   ;; Save response boundaries
   (letrec ((write-bounds
             (lambda (attempts)
               (let* ((bounds (gptel--get-buffer-bounds))
                      (offset (caar bounds))
                      (offset-marker (set-marker (make-marker) offset)))
                 (org-entry-put (point-min) "GPTEL_BOUNDS"
                                (prin1-to-string (gptel--get-buffer-bounds)))
                 (when (and (not (= (marker-position offset-marker) offset))
                            (> attempts 0))
                   (funcall write-bounds (1- attempts)))))))
     (funcall write-bounds 6))))
                                

(provide 'gptel-org)
;;; gptel-org.el ends here
