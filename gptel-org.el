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

(declare-function org-element-begin "org-element")

;; Functions used for saving/restoring gptel state in Org buffers
(defvar org-entry-property-inherited-from)
(declare-function org-entry-get "org")
(declare-function org-entry-put "org")
(declare-function org-with-wide-buffer "org-macs")
(declare-function org-set-property "org")
(declare-function org-property-values "org")
(declare-function org-open-line "org")
(declare-function org-at-heading-p "org")
(declare-function org-get-heading "org")
(declare-function org-at-heading-p "org")

;; Bundle `org-element-lineage-map' if it's not available (for Org 9.67 or older)
(eval-when-compile
  (if (fboundp 'org-element-lineage-map)
      (progn (declare-function org-element-lineage-map "org-element-ast")
             (defalias 'gptel-org--element-lineage-map 'org-element-lineage-map))
    (defun gptel-org--element-lineage-map (datum fun &optional types with-self first-match)
      "Map FUN across ancestors of DATUM, from closest to furthest.

DATUM is an object or element.  For TYPES, WITH-SELF and
FIRST-MATCH see `org-element-lineage-map'.

This function is provided for compatibility with older versions
of Org."
      (declare (indent 2))
      (setq fun (if (functionp fun) fun `(lambda (node) ,fun)))
      (let ((up (if with-self datum (org-element-parent datum)))
	    acc rtn)
        (catch :--first-match
          (while up
            (when (or (not types) (org-element-type-p up types))
              (setq rtn (funcall fun up))
              (if (and first-match rtn)
                  (throw :--first-match rtn)
                (when rtn (push rtn acc))))
            (setq up (org-element-parent up)))
          (nreverse acc))))))


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
  (when (org-entry-get (point) "GPTEL_TOPIC" 'inherit)
    (marker-position org-entry-property-inherited-from)))

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

;; NOTE: This can be converted to a cl-defmethod for `gptel--parse-buffer'
;; (conceptually cleaner), but will cause load-order issues in gptel.el and
;; might be harder to debug.
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
        (topic-start (gptel-org--get-topic-start)))
    (when topic-start
      ;; narrow to GPTEL_TOPIC property scope
      (narrow-to-region topic-start prompt-end))
    (if gptel-org-branching-context
        ;; Create prompt from direct ancestors of point
        (save-excursion
          (let* ((org-buf (current-buffer))
                 (start-bounds (gptel-org--element-lineage-map
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
              (setq-local gptel-backend
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

(defun gptel-org--send-with-props (send-fun &rest args)
  "Conditionally modify SEND-FUN's calling environment.

If in an Org buffer under a heading containing a stored gptel
configuration, use that for requests instead.  This includes the
system message, model and provider (backend), among other
parameters."
  (if (derived-mode-p 'org-mode)
      (pcase-let ((`(,gptel--system-message ,gptel-backend ,gptel-model
                     ,gptel-temperature ,gptel-max-tokens)
                   (seq-mapn (lambda (a b) (or a b))
                             (gptel-org--entry-properties)
                             (list gptel--system-message gptel-backend gptel-model
                                   gptel-temperature gptel-max-tokens))))
        (apply send-fun args))
    (apply send-fun args)))

(advice-add 'gptel-send :around #'gptel-org--send-with-props)
(advice-add 'gptel--suffix-send :around #'gptel-org--send-with-props)

;; ;; NOTE: Basic uses in org-mode are covered by advising gptel-send and
;; ;; gptel--suffix-send.  For custom commands it might be necessary to advise
;; ;; gptel-request instead.
;; (advice-add 'gptel-request :around #'gptel-org--send-with-props)


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

(defun gptel-org-set-properties (pt &optional msg)
  "Store the active gptel configuration under the current heading.

The active gptel configuration includes the current system
message, language model and provider (backend), and additional
settings when applicable.

PT is the cursor position by default.  If MSG is
non-nil (default), display a message afterwards."
  (interactive (list (point) t))
  (org-entry-put pt "GPTEL_MODEL" gptel-model)
  (org-entry-put pt "GPTEL_BACKEND" (gptel-backend-name gptel-backend))
  (unless (equal (default-value 'gptel-temperature) gptel-temperature)
    (org-entry-put pt "GPTEL_TEMPERATURE"
                   (number-to-string gptel-temperature)))
  (org-entry-put pt "GPTEL_SYSTEM"
                 (string-replace "\n" "\\n" gptel--system-message))   
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
   (gptel-org-set-properties (point-min))
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


;;; Transforming responses
(defun gptel--convert-markdown->org (str)
  "Convert string STR from markdown to org markup.

This is a very basic converter that handles only a few markup
elements."
  (interactive)
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (while (re-search-forward "`\\|\\*\\{1,2\\}\\|_" nil t)
      (pcase (match-string 0)
        ("`" (if (save-excursion
                   (beginning-of-line)
                   (skip-chars-forward " \t")
                   (looking-at "```"))
                 (progn (backward-char)
                        (delete-char 3)
                        (insert "#+begin_src ")
                        (when (re-search-forward "^```" nil t)
                          (replace-match "#+end_src")))
               (replace-match "=")))
        ("**" (cond
               ((looking-at "\\*\\(?:[[:word:]]\\|\s\\)")
                (delete-char 1))
               ((looking-back "\\(?:[[:word:]]\\|\s\\)\\*\\{2\\}"
                              (max (- (point) 3) (point-min)))
                (delete-char -1))))
        ("*"
         (cond
          ((save-match-data
             (and (looking-back "\\(?:[[:space:]]\\|\s\\)\\(?:_\\|\\*\\)"
                                (max (- (point) 2) (point-min)))
                  (not (looking-at "[[:space:]]\\|\s"))))
           ;; Possible beginning of emphasis
           (and
            (save-excursion
              (when (and (re-search-forward (regexp-quote (match-string 0))
                                            (line-end-position) t)
                         (looking-at "[[:space]]\\|\s")
                         (not (looking-back "\\(?:[[:space]]\\|\s\\)\\(?:_\\|\\*\\)"
                                            (max (- (point) 2) (point-min)))))
                (delete-char -1) (insert "/") t))
            (progn (delete-char -1) (insert "/"))))
          ((save-excursion
             (ignore-errors (backward-char 2))
             (looking-at "\\(?:$\\|\\`\\)\n\\*[[:space:]]"))
           ;; Bullet point, replace with hyphen
           (delete-char -1) (insert "-"))))))
    (buffer-string)))

(defun gptel--replace-source-marker (num-ticks &optional end)
  "Replace markdown style backticks with Org equivalents.

NUM-TICKS is the number of backticks being replaced.  If END is
true these are \"ending\" backticks.

This is intended for use in the markdown to org stream converter."
  (let ((from (match-beginning 0)))
    (delete-region from (point))
    (if (and (= num-ticks 3)
             (save-excursion (beginning-of-line)
                             (skip-chars-forward " \t")
                             (eq (point) from)))
        (insert (if end "#+end_src" "#+begin_src "))
      (insert "="))))

(defun gptel--stream-convert-markdown->org ()
  "Return a Markdown to Org converter.

This function parses a stream of Markdown text to Org
continuously when it is called with successive chunks of the
text stream."
  (letrec ((in-src-block nil)           ;explicit nil to address BUG #183
           (temp-buf (generate-new-buffer-name "*gptel-temp*"))
           (start-pt (make-marker))
           (ticks-total 0)
           (cleanup-fn
            (lambda (&rest _)
              (when (buffer-live-p (get-buffer temp-buf))
                (set-marker start-pt nil)
                (kill-buffer temp-buf))
              (remove-hook 'gptel-post-response-functions cleanup-fn))))
    (add-hook 'gptel-post-response-functions cleanup-fn)
    (lambda (str)
      (let ((noop-p) (ticks 0))
        (with-current-buffer (get-buffer-create temp-buf)
          (save-excursion (goto-char (point-max)) (insert str))
          (when (marker-position start-pt) (goto-char start-pt))
          (when in-src-block (setq ticks ticks-total))
          (save-excursion
            (while (re-search-forward "`\\|\\*\\{1,2\\}\\|_\\|^#+" nil t)
              (pcase (match-string 0)
                ("`"
                 ;; Count number of consecutive backticks
                 (backward-char)
                 (while (and (char-after) (eq (char-after) ?`))
                   (forward-char)
                   (if in-src-block (cl-decf ticks) (cl-incf ticks)))
                 ;; Set the verbatim state of the parser
                 (if (and (eobp)
                          ;; Special case heuristic: If the response ends with
                          ;; ^``` we don't wait for more input.
                          ;; FIXME: This can have false positives.
                          (not (save-excursion (beginning-of-line)
                                               (looking-at "^```$"))))
                     ;; End of input => there could be more backticks coming,
                     ;; so we wait for more input
                     (progn (setq noop-p t) (set-marker start-pt (match-beginning 0)))
                   ;; We reached a character other than a backtick
                   (cond
                    ;; Ticks balanced, end src block
                    ((= ticks 0)
                     (progn (setq in-src-block nil)
                            (gptel--replace-source-marker ticks-total 'end)))
                    ;; Positive number of ticks, start an src block
                    ((and (> ticks 0) (not in-src-block))
                     (setq ticks-total ticks
                           in-src-block t)
                     (gptel--replace-source-marker ticks-total))
                    ;; Negative number of ticks or in a src block already,
                    ;; reset ticks
                    (t (setq ticks ticks-total)))))
                ;; Handle other chars: heading, emphasis, bold and bullet items
                ((and (guard (and (not in-src-block) (eq (char-before) ?#))) heading)
                 (if (eobp)
                     ;; Not enough information about the heading yet
                     (progn (setq noop-p t) (set-marker start-pt (match-beginning 0)))
                   ;; Convert markdown heading to Org heading
                   (when (looking-at "[[:space:]]")
                     (delete-region (line-beginning-position) (point))
                     (insert (make-string (length heading) ?*)))))
                ((and "**" (guard (not in-src-block)))
                 (cond
                  ;; TODO Not sure why this branch was needed
                  ;; ((looking-at "\\*\\(?:[[:word:]]\\|\s\\)") (delete-char 1))

                  ;; Looking back at "w**" or " **"
                  ((looking-back "\\(?:[[:word:][:punct:]\n]\\|\s\\)\\*\\{2\\}"
                                 (max (- (point) 3) (point-min)))
                   (delete-char -1))))
                ((and "*" (guard (not in-src-block)))
                 (if (eobp)
                     ;; Not enough information about the "*" yet
                     (progn (setq noop-p t) (set-marker start-pt (match-beginning 0)))
                   ;; "*" is either emphasis or a bullet point
                   (save-match-data
                     (save-excursion
                       (ignore-errors (backward-char 2))
                       (cond
                        ((or (looking-at
                              "[^[:space:][:punct:]\n]\\(?:_\\|\\*\\)\\(?:[[:space:][:punct:]]\\|$\\)")
                             (looking-at
                              "\\(?:[[:space:][:punct:]]\\)\\(?:_\\|\\*\\)\\([^[:space:][:punct:]]\\|$\\)"))
                         ;; Emphasis, replace with slashes
                         (forward-char 2) (delete-char -1) (insert "/"))
                        ((looking-at "\\(?:$\\|\\`\\)\n\\*[[:space:]]")
                         ;; Bullet point, replace with hyphen
                         (forward-char 2) (delete-char -1) (insert "-"))))))))))
          (if noop-p
              (buffer-substring (point) start-pt)
            (prog1 (buffer-substring (point) (point-max))
                   (set-marker start-pt (point-max)))))))))

(provide 'gptel-org)
;;; gptel-org.el ends here
