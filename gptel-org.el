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
(eval-when-compile (require 'gptel))
(require 'cl-lib)
(require 'org-element)
(require 'outline)

;; Functions used for saving/restoring gptel state in Org buffers
(defvar gptel--num-messages-to-send)
(defvar org-entry-property-inherited-from)
(defvar gptel-backend)
(defvar gptel--known-backends)
(defvar gptel--system-message)
(defvar gptel-model)
(defvar gptel-temperature)
(defvar gptel-max-tokens)

(defvar org-link-angle-re)
(defvar org-link-bracket-re)
(declare-function mailcap-file-name-to-mime-type "mailcap")
(declare-function gptel--model-capable-p "gptel")
(declare-function gptel--model-mime-capable-p "gptel")
(declare-function gptel--model-name "gptel")
(declare-function gptel--to-string "gptel")
(declare-function gptel--to-number "gptel")
(declare-function gptel--intern "gptel")
(declare-function gptel--get-buffer-bounds "gptel")
(declare-function gptel-backend-name "gptel")
(declare-function gptel--parse-buffer "gptel")
(declare-function gptel--parse-directive "gptel")
(declare-function gptel--restore-props "gptel")
(declare-function gptel--with-buffer-copy "gptel")
(declare-function gptel--file-binary-p "gptel")
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
(eval-and-compile
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
          (nreverse acc)))))
  (if (fboundp 'org-element-begin)
      (progn (declare-function org-element-begin "org-element")
             (declare-function org-element-end "org-element")
             (declare-function org-element-parent "org-element")
             (defalias 'gptel-org--element-begin 'org-element-begin)
             (defalias 'gptel-org--element-end 'org-element-end)
             (defalias 'gptel-org--element-parent 'org-element-parent))
    (defsubst gptel-org--element-begin (node)
      "Get `:begin' property of NODE."
      (org-element-property :begin node))
    (defsubst gptel-org--element-end (node)
      "Get `:end' property of NODE."
      (org-element-property :end node))
    (defsubst gptel-org--element-parent (node)
      "Return `:parent' property of NODE."
      (org-element-property :parent node))))


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
  :type 'boolean
  :group 'gptel)

(defcustom gptel-org-ignore-elements '(property-drawer)
  "List of Org elements that should be stripped from the prompt
before sending it.

By default gptel will remove Org property drawers from the
prompt.  For the full list of available elements, please see
`org-element-all-elements'.

Please note: Removing property-drawer elements is fast, but
adding elements to this list can significantly slow down
`gptel-send'."
  :group 'gptel
  :type '(repeat symbol))


;;; Setting context and creating queries
(defun gptel-org--get-topic-start ()
  "If a conversation topic is set, return it."
  (when (org-entry-get (point) "GPTEL_TOPIC" 'inherit)
    (marker-position org-entry-property-inherited-from)))

(defun gptel-org-set-topic (topic)
  "Set a TOPIC and limit this conversation to the current heading.

This limits the context sent to the LLM to the text between the current
heading (i.e. the heading with the topic set) and the cursor position."
  (interactive
   (list
    (progn
      (or (derived-mode-p 'org-mode)
          (user-error "Support for multiple topics per buffer is only implemented for `org-mode'"))
      (completing-read "Set topic as: "
                       (org-property-values "GPTEL_TOPIC")
                       nil nil (downcase
                                (truncate-string-to-width
                                 (substring-no-properties
                                  (replace-regexp-in-string
                                   "\\s-+" "-"
                                   (org-entry-get nil "ITEM")))
                                 50))))))
  (when (stringp topic) (org-set-property "GPTEL_TOPIC" topic)))

;; NOTE: This can be converted to a cl-defmethod for
;; `gptel--create-prompt-buffer' (conceptually cleaner), but will cause
;; load-order issues in gptel.el and might be harder to debug.
(defun gptel-org--create-prompt-buffer (&optional prompt-end)
  "Return a buffer with the conversation prompt to be sent.

If the region is active limit the prompt text to the region contents.
Otherwise the prompt text is constructed from the contents of the
current buffer up to point, or PROMPT-END if provided.  Its contents
depend on the value of `gptel-org-branching-context', which see."
  (when (use-region-p)
    (narrow-to-region (region-beginning) (region-end))
    (setq prompt-end (point-max)))
  (goto-char (or prompt-end (setq prompt-end (point))))
  (let ((topic-start (gptel-org--get-topic-start)))
    (when topic-start
      ;; narrow to GPTEL_TOPIC property scope
      (narrow-to-region topic-start prompt-end))
    (if (and gptel-org-branching-context
             (or (fboundp 'org-element-lineage-map)
                 (prog1 nil
                   (display-warning
                    '(gptel org)
                    "Using `gptel-org-branching-context' requires Org version 9.7 or higher, it will be ignored."))))
        ;; Create prompt from direct ancestors of point
        (save-excursion
          (let* ((org-buf (current-buffer))
                 ;; Collect all heading start positions in the lineage
                 (full-bounds (gptel-org--element-lineage-map
                                  (org-element-at-point) #'gptel-org--element-begin
                                '(headline) 'with-self) )
                 ;; lineage-map returns the full lineage in the unnarrowed
                 ;; buffer.  Remove heading start positions before (point-min)
                 ;; that are invalid due to narrowing, and add (point-min) if
                 ;; it's not already included in the lineage
                 (start-bounds
                  (nconc (cl-delete-if (lambda (p) (< p (point-min)))
                                       full-bounds)
                         (unless (save-excursion (goto-char (point-min))
                                                 (looking-at-p outline-regexp))
                           (list (point-min)))))
                 (end-bounds
                  (cl-loop
                   ;; (car start-bounds) is the begining of the current element,
                   ;; not relevant
                   for pos in (cdr start-bounds)
                   do (goto-char pos) (outline-next-heading)
                   collect (point) into ends
                   finally return (cons prompt-end ends))))
            (gptel--with-buffer-copy org-buf nil nil
              (cl-loop for start in start-bounds
                       for end in end-bounds
                       do (insert-buffer-substring org-buf start end)
                       (goto-char (point-min)))
              (goto-char (point-max))
              (gptel-org--unescape-tool-results)
              (gptel-org--strip-block-headers)
              (when-let* ((gptel-org-ignore-elements ;not copied by -with-buffer-copy
                           (buffer-local-value 'gptel-org-ignore-elements
                                               org-buf)))
                (gptel-org--strip-elements))
              (setq org-complex-heading-regexp ;For org-element-context to run
                    (buffer-local-value 'org-complex-heading-regexp org-buf))
              (current-buffer))))
      ;; Create prompt the usual way
      (let ((org-buf (current-buffer))
            (beg (point-min)))
        (gptel--with-buffer-copy org-buf beg prompt-end
          (gptel-org--unescape-tool-results)
          (gptel-org--strip-block-headers)
          (when-let* ((gptel-org-ignore-elements ;not copied by -with-buffer-copy
                       (buffer-local-value 'gptel-org-ignore-elements
                                           org-buf)))
                (gptel-org--strip-elements))
          (setq org-complex-heading-regexp ;For org-element-context to run
                (buffer-local-value 'org-complex-heading-regexp org-buf))
          (current-buffer))))))

(defun gptel-org--strip-elements ()
  "Remove all elements in `gptel-org-ignore-elements' from the
prompt."
  (let ((major-mode 'org-mode) element-markers)
    (if (equal '(property-drawer) gptel-org-ignore-elements)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward org-property-drawer-re nil t)
            ;; ;; Slower but accurate
            ;; (let ((drawer (org-element-at-point)))
            ;;   (when (org-element-type-p drawer 'property-drawer)
            ;;     (delete-region (org-element-begin drawer) (org-element-end drawer))))

            ;; Fast but inexact, can have false positives
            (delete-region (match-beginning 0) (match-end 0))))
      ;; NOTE: Parsing the buffer is extremely slow.  Avoid this path unless
      ;; required.
      ;; NOTE: `org-element-map' takes a third KEEP-DEFERRED argument in newer
      ;; Org versions
      (org-element-map (org-element-parse-buffer 'element nil)
          gptel-org-ignore-elements
        (lambda (node)
          (push (list (gptel-org--element-begin node)
                      (gptel-org--element-end node))
                element-markers)))
      (dolist (bounds element-markers)
        (apply #'delete-region bounds)))))

(defun gptel-org--strip-block-headers ()
  "Remove all gptel-specific block headers and footers.
Every line that matches will be removed entirely.

This removal is necessary to avoid auto-mimicry by LLMs."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            (rx line-start (literal "#+")
                (or (literal "begin") (literal "end"))
                (or (literal "_tool") (literal "_reasoning")))
            nil t)
      (delete-region (match-beginning 0)
                     (min (point-max) (1+ (line-end-position)))))))

(defun gptel-org--unescape-tool-results ()
  "Undo escapes done to keep results from escaping blocks.
Scans backward for gptel tool text property, reads the arguments, then
unescapes the remainder."
  (save-excursion
    (goto-char (point-max))
    (let ((prev-pt (point)))
      (while (> prev-pt (point-min))
        (goto-char
         (previous-single-char-property-change (point) 'gptel))
        (let ((prop (get-text-property (point) 'gptel))
              (backward-progress (point)))
          (when (eq (car-safe prop) 'tool)
            ;; User edits to clean up can potentially insert a tool-call header
            ;; that is propertized.  Tool call headers should not be
            ;; propertized.
            (when (looking-at-p "[[:space:]]*#\\+begin_tool")
              (goto-char (match-end 0)))
            (condition-case nil
                (read (current-buffer))
              ((end-of-file invalid-read-syntax)
               (message "Could not read tool arguments")))
            ;; TODO this code is able to put the point behind prev-pt, which
            ;; makes the region inverted.  The `max' catches this, but really
            ;; `read' and `looking-at' are the culprits.  Badly formed tool
            ;; blocks can lead to this being necessary.
            (org-unescape-code-in-region
             (min prev-pt (point)) prev-pt))
          (goto-char (setq prev-pt backward-progress)))))))

;; Handle media links in the buffer
(cl-defmethod gptel--parse-media-links ((_mode (eql 'org-mode)) beg end)
  "Parse text and actionable links between BEG and END.

Return a list of the form
 ((:text \"some text\")
  (:media \"/path/to/media.png\" :mime \"image/png\")
  (:text \"More text\"))
for inclusion into the user prompt for the gptel request."
  (require 'mailcap)                    ;FIXME Avoid this somehow
  (let ((parts) (from-pt) (mime)
        (link-regex (concat "\\(?:" org-link-bracket-re "\\|"
                            org-link-angle-re "\\)")))
    (save-excursion
      (setq from-pt (goto-char beg))
      (while (re-search-forward link-regex end t)
        (setq mime nil)
        (when-let* ((link (org-element-context))
                    ((gptel-org--link-standalone-p link))
                    (raw-link (org-element-property :raw-link link))
                    (path (org-element-property :path link))
                    (type (org-element-property :type link))
                    ;; FIXME This is not a good place to check for url capability!
                    ((member type `("attachment" "file"
                                    ,@(and (gptel--model-capable-p 'url)
                                       '("http" "https" "ftp"))))))
          (cond
           ((member type '("file" "attachment"))
            (if (file-readable-p path)
              (if (or (not (gptel--file-binary-p path))
                      (and (setq mime (mailcap-file-name-to-mime-type path))
                           (gptel--model-mime-capable-p mime)))
                  (progn                ; text file or supported binary file
                    ;; collect text up to link
                    (when-let* ((text (buffer-substring-no-properties
                                       from-pt (gptel-org--element-begin link))))
                      (unless (string-blank-p text) (push (list :text text) parts)))
                    ;; collect link
                    (push (if mime (list :media path :mime mime) (list :textfile path)) parts)
                    (setq from-pt (point)))
                (message "Ignoring unsupported binary file \"%s\"." path))
              (message "Ignoring inaccessible file \"%s\"." path)))
           ((and (member type '("http" "https" "ftp"))
                 (setq mime (mailcap-file-name-to-mime-type path))
                 (gptel--model-capable-p mime))
            ;; Collect text up to this image, and collect this image url
            (when-let* ((text (buffer-substring-no-properties
                               from-pt (gptel-org--element-begin link))))
              (unless (string-blank-p text) (push (list :text text) parts)))
            (push (list :url raw-link :mime mime) parts)
            (setq from-pt (point))))))
      (unless (= from-pt end)
        (push (list :text (buffer-substring-no-properties from-pt end)) parts)))
    (nreverse parts)))

(defun gptel-org--link-standalone-p (object)
  "Check if link OBJECT is on a line by itself."
  (when-let* ((par (gptel-org--element-parent object))
              ((eq (org-element-type par) 'paragraph)))
    (and (= (gptel-org--element-begin object)
            (save-excursion
              (goto-char (org-element-property :contents-begin par))
              (skip-chars-forward "\t ")
              (point)))                 ;account for leading space before object
         (<= (- (org-element-property :contents-end par)
                (org-element-property :end object))
             1))))

(defun gptel-org--send-with-props (send-fun &rest args)
  "Conditionally modify SEND-FUN's calling environment.

If in an Org buffer under a heading containing a stored gptel
configuration, use that for requests instead.  This includes the
system message, model and provider (backend), among other
parameters.

ARGS are the original function call arguments."
  (if (derived-mode-p 'org-mode)
      (pcase-let ((`(,gptel--system-message ,gptel-backend ,gptel-model
                     ,gptel-temperature ,gptel-max-tokens
                     ,gptel--num-messages-to-send ,gptel-tools)
                   (seq-mapn (lambda (a b) (or a b))
                             (gptel-org--entry-properties)
                             (list gptel--system-message gptel-backend gptel-model
                                   gptel-temperature gptel-max-tokens
                                   gptel--num-messages-to-send gptel-tools))))
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
  "Find gptel configuration properties stored at PT."
  (pcase-let
      ((`(,system ,backend ,model ,temperature ,tokens ,num ,tools)
         (mapcar
          (lambda (prop) (org-entry-get (or pt (point)) prop 'selective))
          '("GPTEL_SYSTEM" "GPTEL_BACKEND" "GPTEL_MODEL"
            "GPTEL_TEMPERATURE" "GPTEL_MAX_TOKENS"
            "GPTEL_NUM_MESSAGES_TO_SEND" "GPTEL_TOOLS"))))
    (when system
      (setq system (string-replace "\\n" "\n" system)))
    (when backend
      (setq backend (alist-get backend gptel--known-backends
                               nil nil #'equal)))
    (when model (setq model (gptel--intern model)))
    (when temperature
      (setq temperature (gptel--to-number temperature)))
    (when tokens (setq tokens (gptel--to-number tokens)))
    (when num (setq num (gptel--to-number num)))
    (when tools
      (setq tools (cl-loop
                   for tname in (split-string tools)
                   for tool = (with-demoted-errors "gptel: %S"
                                (gptel-get-tool tname))
                   if tool collect tool else do
                   (display-warning
                    '(gptel org tools)
                    (format "Tool %s not found, ignoring" tname)))))
    (list system backend model temperature tokens num tools)))

(defun gptel-org--restore-state ()
  "Restore gptel state for Org buffers when turning on `gptel-mode'."
  (save-restriction
    (let ((modified (buffer-modified-p)))
      (widen)
      (condition-case status
          (progn
            (when-let* ((bounds (org-entry-get (point-min) "GPTEL_BOUNDS")))
              (gptel--restore-props (read bounds)))
            (pcase-let ((`(,system ,backend ,model ,temperature ,tokens ,num ,tools)
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
              (when tokens (setq-local gptel-max-tokens tokens))
              (when num (setq-local gptel--num-messages-to-send num))
              (when tools (setq-local gptel-tools tools))))
        (:success (message "gptel chat restored."))
        (error (message "Could not restore gptel state, sorry! Error: %s" status)))
      (set-buffer-modified-p modified))))

(defun gptel-org-set-properties (pt &optional msg)
  "Store the active gptel configuration under the current heading.

The active gptel configuration includes the current system
message, language model and provider (backend), and additional
settings when applicable.

PT is the cursor position by default.  If MSG is
non-nil (default), display a message afterwards."
  (interactive (list (point) t))
  (org-entry-put pt "GPTEL_MODEL" (gptel--model-name gptel-model))
  (org-entry-put pt "GPTEL_BACKEND" (gptel-backend-name gptel-backend))
  (org-entry-put pt "GPTEL_SYSTEM"      ;TODO: Handle nil case correctly
                 (and-let* ((msg (car-safe
                                  (gptel--parse-directive
                                   gptel--system-message))))
                   (string-replace "\n" "\\n" msg)))
  (if gptel-tools
      (org-entry-put
       pt "GPTEL_TOOLS" (mapconcat #'gptel-tool-name gptel-tools " "))
    (org-entry-delete pt "GPTEL_TOOLS"))
  (if (equal (default-value 'gptel-temperature) gptel-temperature)
      (org-entry-delete pt "GPTEL_TEMPERATURE")
    (org-entry-put pt "GPTEL_TEMPERATURE"
                   (number-to-string gptel-temperature)))
  (if (natnump gptel--num-messages-to-send)
      (org-entry-put pt "GPTEL_NUM_MESSAGES_TO_SEND"
                     (number-to-string gptel--num-messages-to-send))
    (org-entry-delete pt "GPTEL_NUM_MESSAGES_TO_SEND"))
  (if gptel-max-tokens
      (org-entry-put
       pt "GPTEL_MAX_TOKENS" (number-to-string gptel-max-tokens))
    (org-entry-delete pt "GPTEL_MAX_TOKENS"))
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
               (when-let* ((bounds (gptel--get-buffer-bounds))
                           ;; first value of ((prop . ((beg end val)...))...)
                           (offset (caadar bounds))
                           (offset-marker (set-marker (make-marker) offset)))
                 (org-entry-put (point-min) "GPTEL_BOUNDS"
                                (prin1-to-string (gptel--get-buffer-bounds)))
                 (when (and (not (= (marker-position offset-marker) offset))
                            (> attempts 0))
                   (funcall write-bounds (1- attempts)))))))
     (funcall write-bounds 6))))


;;; Transforming responses
;;;###autoload
(defun gptel--convert-markdown->org (str)
  "Convert string STR from markdown to org markup.

This is a very basic converter that handles only a few markup
elements."
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (while (re-search-forward "`+\\|\\*\\{1,2\\}\\|_\\|^#+" nil t)
      (pcase (match-string 0)
        ;; Handle backticks
        ((and (guard (eq (char-before) ?`)) ticks)
         (gptel--replace-source-marker (length ticks))
         (save-match-data
           (catch 'block-end
             (while (search-forward ticks nil t)
               (unless (or (eq (char-before (match-beginning 0)) ?`)
                           (eq (char-after) ?`))
                 (gptel--replace-source-marker (length ticks) 'end)
                 (throw 'block-end nil))))))
        ;; Handle headings
        ((and (guard (eq (char-before) ?#)) heading)
         (cond
          ((looking-at "[[:space:]]")   ;Handle headings
           (delete-region (line-beginning-position) (point))
           (insert (make-string (length heading) ?*)))
          ((looking-at "\\+begin_src") ;Overeager LLM switched to using Org src blocks
           (save-match-data (re-search-forward "^#\\+end_src" nil t)))))
        ;; Handle emphasis
        ("**" (cond
               ;; ((looking-at "\\*\\(?:[[:word:]]\\|\s\\)")
               ;;  (delete-char 1))
               ((looking-back "\\(?:[[:word:][:punct:]\n]\\|\s\\)\\*\\{2\\}"
                              (max (- (point) 3) (point-min)))
                (delete-char -1))))
        ("*"
         (cond
          ((save-match-data
             (and (or (= (point) 2)
                      (looking-back "\\(?:[[:space:]]\\|\s\\)\\(?:_\\|\\*\\)"
                                    (max (- (point) 2) (point-min))))
                  (not (looking-at "[[:space:]]\\|\s"))))
           ;; Possible beginning of emphasis
           (and
            (save-excursion
              (when (and (re-search-forward (regexp-quote (match-string 0))
                                            (line-end-position) t)
                         (looking-at "[[:space:][:punct:]]\\|\s")
                         (not (looking-back "\\(?:[[:space]]\\|\s\\)\\(?:_\\|\\*\\)"
                                            (max (- (point) 2) (point-min)))))
                (delete-char -1) (insert "/") t))
            (progn (delete-char -1) (insert "/"))))
          ((save-excursion
             (ignore-errors (backward-char 2))
             (or (and (bobp) (looking-at "\\*[[:space:]]"))
                 (looking-at "\\(?:$\\|\\`\\)\n\\*[[:space:]]")))
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

;;;###autoload
(defun gptel--stream-convert-markdown->org (start-marker)
  "Return a Markdown to Org converter.

This function parses a stream of Markdown text to Org
continuously when it is called with successive chunks of the
text stream.

START-MARKER is used to identify the corresponding process when
cleaning up after."
  (letrec ((in-src-block nil)           ;explicit nil to address BUG #183
           (in-org-src-block nil)
           (temp-buf ; NOTE: Switch to `generate-new-buffer' after we drop Emacs 27.1
            (gptel--temp-buffer " *gptel-temp*"))
           (start-pt (make-marker))
           (ticks-total 0)      ;MAYBE should we let-bind case-fold-search here?
           (cleanup-fn
            (lambda (beg _)
              (when (and (equal beg (marker-position start-marker))
                         (eq (current-buffer) (marker-buffer start-marker)))
                (when (buffer-live-p (get-buffer temp-buf))
                  (set-marker start-pt nil)
                  (kill-buffer temp-buf))
                (remove-hook 'gptel-post-response-functions cleanup-fn)))))
    (add-hook 'gptel-post-response-functions cleanup-fn)
    (lambda (str)
      (let ((noop-p) (ticks 0))
        (with-current-buffer (get-buffer temp-buf)
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
                ;; Handle headings and misguided #+begin_src text
                ((and (guard (and (eq (char-before) ?#) (or (not in-src-block) in-org-src-block)))
                      heading)
                 (if in-org-src-block
                     ;; If we are inside an Org-style src block, look for #+end_src
                     (cond
                      ((< (- (point-max) (point)) 8) ;not enough information to close Org src block
                       (setq noop-p t) (set-marker start-pt (match-beginning 0)))
                      ((looking-at "\\+end_src") ;Close Org src block
                       (setq in-src-block nil in-org-src-block nil)))
                   ;; Otherwise check for Markdown headings, or for #+begin_src
                   (cond
                    ((eobp)       ; Not enough information about the heading yet
                     (setq noop-p t) (set-marker start-pt (match-beginning 0)))
                    ((looking-at "[[:space:]]") ; Convert markdown heading to Org heading
                     (delete-region (line-beginning-position) (point))
                     (insert (make-string (length heading) ?*)))
                    ((< (- (point-max) (point)) 11) ;Not enough information to check if Org src block
                     (setq noop-p t) (set-marker start-pt (match-beginning 0)))
                    ((looking-at "\\+begin_src ") ;Overeager LLM switched to using Org src blocks
                     (setq in-src-block t in-org-src-block t)))))
                ;; Handle other chars: emphasis, bold and bullet items
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
                        ((and     ; At bob, underscore/asterisk followed by word
                          (or (and (bobp) (looking-at "\\(?:_\\|\\*\\)\\([^[:space:][:punct:]]\\|$\\)"))
                              (looking-at ; word followed by underscore/asterisk
                               "[^[:space:]\n]\\(?:_\\|\\*\\)\\(?:[[:space:][:punct:]]\\|$\\)")
                              (looking-at ; underscore/asterisk followed by word
                               "\\(?:[[:space:]]\\)\\(?:_\\|\\*\\)\\([^[:space:]]\\|$\\)"))
                          (not (looking-at "[[:punct:]]\\(?:_\\|\\*\\)[[:punct:]]")))
                         ;; Emphasis, replace with slashes
                         (forward-char (if (bobp) 1 2)) (delete-char -1) (insert "/"))
                        ((or (and (bobp) (looking-at "\\*[[:space:]]"))
                             (looking-at "\\(?:$\\|\\`\\)\n\\*[[:space:]]"))
                         ;; Bullet point, replace with hyphen
                         (forward-char (if (bobp) 1 2)) (delete-char -1) (insert "-"))))))))))
          (if noop-p
              (buffer-substring (point) start-pt)
            (prog1 (buffer-substring (point) (point-max))
                   (set-marker start-pt (point-max)))))))))

(provide 'gptel-org)
;;; gptel-org.el ends here

;; Silence warnings about `org-element-type-p' and `org-element-parent', see #294.
;; Local Variables:
;; byte-compile-warnings: (not unresolved)
;; End:
