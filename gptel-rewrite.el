;;; gptel-rewrite.el --- Refactoring functions for gptel  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Keywords: hypermedia, convenience, tools

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
(require 'gptel-transient)
(require 'cl-lib)

(defvar eldoc-documentation-functions)
(defvar diff-entire-buffers)
(defvar ediff-window-setup-function)
(defvar ediff-split-window-function)

(declare-function diff-no-select "diff")
(declare-function rmc--add-key-description "rmc")
(declare-function ediff-setup-windows-plain "ediff-wind")

;; * User options

(defcustom gptel-rewrite-directives-hook nil
  "Hook run to generate gptel's default rewrite directives.

Each function in this hook is called with no arguments until one
returns a non-nil value, the base string to use as the
rewrite instruction.

Use this hook to tailor context-specific rewrite directives.
For example, you can specialize the default rewrite directive
for a particular major-mode or project."
  :group 'gptel
  :type 'hook)

(defcustom gptel-post-rewrite-functions nil
  "Abnormal hook run after a `gptel-rewrite' action.

This hook is called after the LLM response for the rewrite action
has been fully received in a temporary buffer.  Each function is
called with two arguments: the response beginning and end
positions.

Note: this hook only runs if the rewrite request succeeds."
  :type 'hook
  :group 'gptel)

(defcustom gptel-rewrite-default-action nil
  "Action to take when rewriting a text region using gptel.

When the LLM response with the rewritten text is received, you can
- merge it with the current region, possibly creating a merge conflict,
- diff or ediff against the original region,
- or accept it in place, replacing the original region.
- display a dispatch menu with the above choices.

If this option is nil (the default), gptel waits for an explicit
command.  Set it to the symbol `merge', `diff', `ediff', `accept'
or `dispatch' to automatically do one of these things instead.

You can also set it to a function of your choosing for a custom
action.  This function receives one argument, the rewrite
overlay."
  :group 'gptel
  :type '(choice
          (const :tag "Wait" nil)
          (const :tag "Merge with current region" merge)
          (const :tag  "Diff against current region" diff)
          (const :tag "Ediff against current region" ediff)
          (const :tag "Accept rewrite" accept)
          (const :tag "Dispatch" dispatch)
          (function :tag "Custom action")))

(defface gptel-rewrite-highlight-face
  '((((class color) (min-colors 88) (background dark))
     :background "#041714" :extend t :inherit default)
    (((class color) (min-colors 88) (background light))
     :background "light goldenrod yellow" :extend t :inherit default)
    (t :inherit secondary-selection))
  "Face for highlighting regions with pending rewrites."
  :group 'gptel)

;; * Variables

(defvar-keymap gptel-rewrite-actions-map
  :doc "Keymap for gptel rewrite actions at point."
  "RET" #'gptel--rewrite-dispatch
  "<mouse-1>" #'gptel--rewrite-dispatch
  "C-c C-a" #'gptel--rewrite-accept
  "C-c C-r" #'gptel--rewrite-iterate
  "C-c C-k" #'gptel--rewrite-reject
  "C-c C-d" #'gptel--rewrite-diff
  "C-c C-e" #'gptel--rewrite-ediff
  "C-c C-n" #'gptel--rewrite-next
  "C-c C-p" #'gptel--rewrite-previous
  "C-c C-m" #'gptel--rewrite-merge)

(defvar-local gptel--rewrite-overlays nil
  "List of active rewrite overlays in the buffer.")

(defvar-local gptel--rewrite-message nil
  "Request-specific instructions for a gptel-rewrite action.")

;; Add the rewrite directive to `gptel-directives'
(unless (alist-get 'rewrite gptel-directives)
  (add-to-list 'gptel-directives `(rewrite . ,#'gptel--rewrite-directive-default)))

(defvar gptel--rewrite-directive
  (or (alist-get 'rewrite gptel-directives)
      #'gptel--rewrite-directive-default)
  "Active system message for rewrite actions.

This variable is for internal use only.  To customize the rewrite
system message, set a system message (or function that generates
the system message) as the value of the `rewrite' key in
`gptel-directives':

 (setf (alist-get \\='rewrite gptel-directives)
       #\\='my-rewrite-message-generator)

You can also customize `gptel-rewrite-directives-hook' to
dynamically inject a rewrite-specific system message.")

(defun gptel--rewrite-directive-default ()
  "Generic directive for rewriting or refactoring.

These are instructions not specific to any particular required
change.

The returned string is interpreted as the system message for the
rewrite request.  To use your own, add a different directive to
`gptel-directives', or add to `gptel-rewrite-directives-hook',
which see."
  (or (save-mark-and-excursion
        (run-hook-with-args-until-success
         'gptel-rewrite-directives-hook))
      (let* ((lang (downcase (gptel--strip-mode-suffix major-mode)))
             (article (if (and lang (not (string-empty-p lang))
                               (memq (aref lang 0) '(?a ?e ?i ?o ?u)))
                          "an" "a")))
        (if (derived-mode-p 'prog-mode)
            (format (concat "You are %s %s programmer.  "
                            "Follow my instructions and refactor %s code I provide.\n"
                            "- Generate ONLY %s code as output, without "
                            "any explanation or markdown code fences.\n"
                            "- Generate code in full, do not abbreviate or omit code.\n"
                            "- Do not produce intermediate text or report on your progress.\n"
                            "- Do not ask for further clarification, and make "
                            "any assumptions you need to follow instructions.")
                    article lang lang lang)
          (concat
           (if (string-empty-p lang)
               "You are an editor."
             (format "You are %s %s editor." article lang))
           "  Follow my instructions and improve or rewrite the text I provide."
           "  Do not produce intermediate text or report on your progress."
           "  Generate ONLY the replacement text,"
           " without any explanation or markdown code fences.")))))

(defvar gptel--rewrite-handlers
  `((WAIT ,#'gptel--handle-wait)
    (TOOL ,#'gptel--update-tool-call ,#'gptel--handle-tool-use))
  "Alist specifying FSM handlers for `gptel-rewrite' state transitions.")

;; * Helper functions

(defun gptel--rewrite-key-help (callback)
  "Eldoc documentation function for gptel rewrite actions.

CALLBACK is supplied by Eldoc, see
`eldoc-documentation-functions'."
  (when (and gptel--rewrite-overlays
             (get-char-property (point) 'gptel-rewrite))
      (funcall callback
               (format (substitute-command-keys "%s rewrite available: accept \\[gptel--rewrite-accept], iterate \\[gptel--rewrite-iterate], clear \\[gptel--rewrite-reject], merge \\[gptel--rewrite-merge], diff \\[gptel--rewrite-diff] or ediff \\[gptel--rewrite-ediff]")
                       (propertize (gptel--model-name gptel-model) 'face 'mode-line-emphasis)))))

(defun gptel--rewrite-move (search-func)
  "Move directionally to a gptel rewrite location using SEARCH-FUNC."
  (let* ((ov (cdr (get-char-property-and-overlay (point) 'gptel-rewrite)))
         (pt (save-excursion
               (if ov
                   (goto-char
                    (funcall search-func (overlay-start ov) 'gptel-rewrite))
                 (goto-char
                  (max (1- (funcall search-func (point) 'gptel-rewrite))
                       (point-min))))
               (funcall search-func (point) 'gptel-rewrite))))
    (if (get-char-property pt 'gptel-rewrite)
        (goto-char pt)
      (user-error "No further rewrite regions!"))))

(defun gptel--rewrite-next ()
  "Go to next pending LLM rewrite in buffer, if one exists."
  (interactive)
  (gptel--rewrite-move #'next-single-char-property-change))

(defun gptel--rewrite-previous ()
  "Go to previous pending LLM rewrite in buffer, if one exists."
  (interactive)
  (gptel--rewrite-move #'previous-single-char-property-change))

(defun gptel--rewrite-overlay-at (&optional pt)
  "Check for a gptel rewrite overlay at PT and return it.

If no suitable overlay is found, raise an error."
  (pcase-let ((`(,response . ,ov)
               (get-char-property-and-overlay (or pt (point)) 'gptel-rewrite))
              (diff-entire-buffers nil))
    (unless ov (user-error "Could not find region being rewritten."))
    (unless response (user-error "No LLM output available for this rewrite."))
    ov))

(defun gptel--rewrite-prepare-buffer (ovs &optional buf)
  "Prepare new buffer with LLM changes applied and return it.

This is used for (e)diff purposes.

RESPONSE is the LLM response.  OVS are the overlays specifying
the changed regions. BUF is the (current) buffer."
  (setq buf (or buf (overlay-buffer (or (car-safe ovs) ovs))))
  (with-current-buffer buf
    (let ((pmin (point-min))
          (pmax (point-max))
          (pt   (point))
          ;; (mode major-mode)
          (newbuf (get-buffer-create "*gptel-diff*"))
          (inhibit-read-only t)
          (inhibit-message t))
      (save-restriction
        (widen)
        (with-current-buffer newbuf
          (erase-buffer)
          (insert-buffer-substring buf)))
      (with-current-buffer newbuf
        (narrow-to-region pmin pmax)
        (goto-char pt)
        ;; We mostly just want font-locking
        ;; (delay-mode-hooks (funcall mode))
        ;; Apply the changes to the new buffer
        (save-excursion
          (gptel--rewrite-accept ovs newbuf)))
      newbuf)))

(defun gptel--rewrite-read-message (prompt &optional _ history)
  "Read a rewrite message from the minibuffer with custom keybindings for
cycling, editing, and submitting the gptel rewrite.

PROMPT is the prompt string to display.  HISTORY, if provided, is the
input history list."
  (let* ((rewrite-directive
          (car-safe (gptel--parse-directive gptel--rewrite-directive 'raw)))
         (cb (current-buffer))
         (cycle-prefix (lambda () (interactive)
                         (gptel--read-with-prefix rewrite-directive)
                         (push-mark) (goto-char (point-max))
                         (activate-mark)))
         (set-rewrite-message
          (lambda ()
            (let ((message (buffer-substring-no-properties
                            (minibuffer-prompt-end) (point-max))))
              (with-current-buffer cb (setq gptel--rewrite-message message))
              (setf (alist-get 'gptel--infix-rewrite-extra transient-history)
                    (delete-dups (cons message transient--history))))))
         (start-rewrite-maybe
          (lambda () (interactive)
            (if transient--prefix    ;Called from transient? Don't start rewrite
                (run-at-time 0 nil #'transient-setup 'gptel-rewrite)
              (run-at-time 0 nil #'gptel--suffix-rewrite gptel--rewrite-message))
            (when (minibufferp)
              (funcall set-rewrite-message)
              (exit-minibuffer))))
         (start-transient
          (lambda () (interactive)
            (run-at-time 0 nil #'transient-setup 'gptel-rewrite)
            (when (minibufferp)
              (funcall set-rewrite-message)
              (exit-minibuffer))))
         (edit-in-buffer
          (lambda () (interactive)
            (let ((offset (- (point) (minibuffer-prompt-end))))
              (gptel--edit-directive 'gptel--rewrite-message
                :prompt rewrite-directive :initial (minibuffer-contents)
                :buffer cb :setup (lambda () (ignore-errors (forward-char offset)))
                :callback
                (lambda (msg)
                  (when msg
                    (run-at-time 0 nil #'gptel--suffix-rewrite)
                    (push (buffer-local-value 'gptel--rewrite-message cb)
                          (alist-get 'gptel--infix-rewrite-extra transient-history)))
                  (when (minibufferp) (exit-minibuffer)))))))
         (minibuffer-local-map
          (make-composed-keymap (define-keymap
                                  "TAB" cycle-prefix "<tab>" cycle-prefix
                                  "C-c C-e" edit-in-buffer
                                  "<remap> <exit-minibuffer>" start-rewrite-maybe
                                  "M-RET" start-transient)
                                minibuffer-local-map)))
    (minibuffer-with-setup-hook cycle-prefix
      (read-string
       prompt (or gptel--rewrite-message "Rewrite: ")
       history))))

;; * Rewrite action functions

(defun gptel--rewrite-reject (&optional ovs)
  "Clear pending LLM responses in OVS or at point."
  (interactive (list (gptel--rewrite-overlay-at)))
  (dolist (ov (ensure-list ovs))
    (setq gptel--rewrite-overlays (delq ov gptel--rewrite-overlays))
    (delete-overlay ov))
  (unless gptel--rewrite-overlays
    (remove-hook 'eldoc-documentation-functions 'gptel--rewrite-key-help 'local))
  (message "Cleared pending LLM response(s)."))

(defun gptel--rewrite-accept (&optional ovs buf)
  "Apply pending LLM responses in OVS or at point.

BUF is the buffer to modify, defaults to the overlay buffer."
  (interactive (list (gptel--rewrite-overlay-at)))
  (when-let* ((ov-buf (overlay-buffer (or (car-safe ovs) ovs)))
              (buf (or buf ov-buf))
              ((buffer-live-p buf)))
    (with-current-buffer ov-buf
      (cl-loop for ov in (ensure-list ovs)
               for ov-beg = (overlay-start ov)
               for ov-end = (overlay-end ov)
               for response = (overlay-get ov 'gptel-rewrite)
               do (overlay-put ov 'before-string nil)
               (with-current-buffer buf
                 (goto-char ov-beg)
                 (delete-region ov-beg ov-end)
                 (insert response))))
    (message "Replaced region(s) with LLM output in buffer: %s."
             (buffer-name ov-buf))))

(defalias 'gptel--rewrite-iterate 'gptel-rewrite
  "Iterate on pending LLM response at point.")

(defun gptel--rewrite-diff (&optional ovs switches)
  "Diff pending LLM responses in OVS or at point."
  (interactive (list (gptel--rewrite-overlay-at)))
  (when-let* ((ov-buf (overlay-buffer (or (car-safe ovs) ovs)))
              ((buffer-live-p ov-buf)))
    (require 'diff)
    (let* ((newbuf (gptel--rewrite-prepare-buffer ovs))
           (diff-buf (diff-no-select ov-buf newbuf switches)))
      (with-current-buffer diff-buf
        (setq-local diff-jump-to-old-file t))
      (display-buffer diff-buf))))

(defun gptel--rewrite-ediff (&optional ovs)
  "Ediff pending LLM responses in OVS or at point."
  (interactive (list (gptel--rewrite-overlay-at)))
  (when-let* ((ov-buf (overlay-buffer (or (car-safe ovs) ovs)))
              ((buffer-live-p ov-buf)))
    (letrec ((newbuf (gptel--rewrite-prepare-buffer ovs))
             (cwc (current-window-configuration))
             (hideshow
              (lambda (&optional restore)
                (dolist (ov (ensure-list ovs))
                  (when-let* ((overlay-buffer ov))
                    (let ((disp (overlay-get ov 'display))
                          (stored (overlay-get ov 'gptel--ediff)))
                      (overlay-put ov 'face (and restore 'gptel-rewrite-highlight-face))
                      (overlay-put ov 'display (and restore stored))
                      (overlay-put ov 'gptel--ediff (unless restore disp)))))))
             (gptel--ediff-restore
              (lambda ()
                (when (window-configuration-p cwc)
                  (set-window-configuration cwc))
                (funcall hideshow 'restore)
                (remove-hook 'ediff-quit-hook gptel--ediff-restore))))
      (funcall hideshow)
      (add-hook 'ediff-quit-hook gptel--ediff-restore)
      (let ((ediff-window-setup-function #'ediff-setup-windows-plain)
            (ediff-split-window-function #'split-window-horizontally))
        (ediff-buffers ov-buf newbuf)))))

(defun gptel--rewrite-merge-git (beg end new-str)
  "Produce a merge conflict region between BEG and END.

Merge the region with NEW-STR using git merge-file."
  (let ((original-temp-file (make-temp-file "gptel-merge-"))
        (empty-temp-file (make-temp-file "gptel-merge-")) ; use /dev/null? (windows?)
        (new-temp-file (make-temp-file "gptel-merge-")))
    (unwind-protect
        (progn (write-region beg end original-temp-file)
               (with-temp-file empty-temp-file (insert ""))
               (with-temp-file new-temp-file (insert new-str))
               (goto-char beg)
               (delete-region beg end)
               (call-process
                "git" nil (list (current-buffer) nil) nil
                "merge-file" "--no-diff3" "-L" "original" "-L" "Empty" "-L"
                (gptel-backend-name gptel-backend) "-p"
                original-temp-file empty-temp-file new-temp-file)
               ;; Make merge marker active if required
               (goto-char beg) (unless (bolp) (insert "\n")))
      (delete-file original-temp-file)
      (delete-file empty-temp-file)
      (delete-file new-temp-file))))

(defun gptel--rewrite-merge-simple (beg end new-str)
  "Produce a merge conflict region between BEG and END.

NEW-STR is the new string intended to replace the region."
  (goto-char end)                       ;End first to preserve ordering
  (unless (bolp) (insert "\n"))
  (insert "=======\n" new-str "\n>>>>>>> "
          (gptel-backend-name gptel-backend) "\n")
  (goto-char beg)
  (unless (bolp) (insert "\n"))
  (insert-before-markers "<<<<<<< original\n"))

(defun gptel--rewrite-merge (&optional ovs)
  "Insert pending LLM responses in OVS as merge conflicts."
  (interactive (list (gptel--rewrite-overlay-at)))
  (when-let* ((ov-buf (overlay-buffer (or (car-safe ovs) ovs)))
              ((buffer-live-p ov-buf)))
    (with-current-buffer ov-buf
      (let ((changed))
        (dolist (ov (ensure-list ovs))
          (save-excursion
            (when-let* ((new-str (overlay-get ov 'gptel-rewrite)))
              (if (executable-find "git") ;Replace overlay content with merge result
                  (gptel--rewrite-merge-git (overlay-start ov) (overlay-end ov) new-str)
                (gptel--rewrite-merge-simple (overlay-start ov) (overlay-end ov) new-str))
              (setq changed t))))
        (when changed (smerge-mode 1)))
      (gptel--rewrite-reject ovs))))

(defun gptel--rewrite-dispatch (&optional ov ci)
  "Dispatch actions for gptel rewrites.

OV is the rewrite overlay, CI is true for interactive calls."
  (interactive (list (gptel--rewrite-overlay-at) t))
  (let ((choice))
    (unwind-protect
        (pcase-let ((choices '((?a "accept") (?k "reject") (?r "iterate")
                               (?m "merge") (?d "diff") (?e "ediff")))
                    (hint-str (concat "[" (gptel--model-name gptel-model) "]\n")))
          (overlay-put
           ov 'before-string
           (concat
            (unless (eq (char-before (overlay-start ov)) ?\n) "\n")
            (propertize "REWRITE READY: " 'face 'success)
	    (when (fboundp #'rmc--add-key-description)  ; introduced in Emacs 29
              (mapconcat (lambda (e) (cdr e)) (mapcar #'rmc--add-key-description choices) ", "))
            (propertize
             " " 'display `(space :align-to (- right ,(1+ (length hint-str)))))
            (propertize hint-str 'face 'success)))
          (setq choice (read-multiple-choice "Action: " choices)))
      (overlay-put ov 'before-string nil))
    (if ci
        (call-interactively (intern (concat "gptel--rewrite-" (cadr choice))))
      (funcall (intern (concat "gptel--rewrite-" (cadr choice))) ov))))

(defun gptel--rewrite-callback (response info)
  "Callback for gptel rewrite actions.

Show the rewrite result in an overlay over the original text, and
set up dispatch actions.

RESPONSE is the response received.  It may also be t (to indicate
success) nil (to indicate failure), or the symbol `abort'.

INFO is the async communication channel for the rewrite request."
  (when-let* ((ov-and-buf (plist-get info :context))
              (ov (car ov-and-buf))
              (proc-buf (cdr ov-and-buf))
              (buf (overlay-buffer ov)))
    (cond
     ((stringp response)            ;partial or fully successful result
      (with-current-buffer proc-buf ;auxiliary buffer, insert text here and copy to overlay
        (let ((inhibit-modification-hooks nil)
              (inhibit-read-only t))
          (when (= (buffer-size) 0)
            (buffer-disable-undo)
            (overlay-put ov 'gptel-rewrite nil)
            (insert-buffer-substring buf (overlay-start ov) (overlay-end ov))
            (when (eq (char-before (point-max)) ?\n)
              (plist-put info :newline t))
            (setq major-mode (buffer-local-value 'major-mode buf)) ;Don't turn on major-mode (#730, #722)
            (add-text-properties (point-min) (point-max) '(face shadow font-lock-face shadow))
            (goto-char (point-min)))
          (insert response)
          (unless (eobp) (ignore-errors (delete-char (length response))))
          (font-lock-ensure)
          (overlay-put ov 'display (buffer-string))))
      (unless (plist-get info :stream) (gptel--rewrite-callback t info)))

     ((eq response 'abort)              ;request aborted
      (when-let* ((proc-buf (cdr-safe (plist-get info :context))))
        (kill-buffer proc-buf))
      (delete-overlay ov))

     ((eq (car-safe response) 'tool-call) ;tool call confirmation
      (gptel--display-tool-calls          ;use minibuffer if buffer is read-only
       (cdr response) info (buffer-local-value 'buffer-read-only buf)))

     ((null response)                   ;finished with error
      (message (concat "LLM response error: %s. Rewrite in buffer %s canceled.")
               (plist-get info :status) (plist-get info :buffer))
      (gptel--rewrite-callback 'abort info))

     ((consp response))             ;reasoning or tool call result -- don't care

     (t
      (if (plist-get info :tool-use)    ;stopped to use tools
          ;; Clear text inserted so far
          (with-current-buffer proc-buf (delete-region (point-min) (point)))
        (let ((mkb (propertize "<mouse-1>" 'face 'help-key-binding))) ;or finished successfully
          (with-current-buffer proc-buf
            (let ((inhibit-read-only t))
              (delete-region (point) (point-max))
              ;; Run post-rewrite-functions on rewritten text in its buffer
              (setq-local gptel-post-rewrite-functions
                          (buffer-local-value 'gptel-post-rewrite-functions buf))
              (with-demoted-errors "gptel-post-rewrite-functions: %S"
                (run-hook-with-args 'gptel-post-rewrite-functions (point-min) (point-max)))
              (when (and (plist-get info :newline)
                         (not (eq (char-before (point-max)) ?\n)))
                (insert "\n"))
              (font-lock-ensure))
            (overlay-put ov 'display (buffer-string))
            (overlay-put ov 'gptel-rewrite (buffer-string))
            (kill-buffer proc-buf))
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (pulse-momentary-highlight-region (overlay-start ov) (overlay-end ov))
              (add-hook 'eldoc-documentation-functions #'gptel--rewrite-key-help nil 'local)
              ;; (overlay-put ov 'gptel-rewrite response)
              (overlay-put ov 'face 'gptel-rewrite-highlight-face)
	      (overlay-put ov 'priority 2000)
              (overlay-put ov 'keymap gptel-rewrite-actions-map)
              (overlay-put ov 'mouse-face 'highlight)
              (overlay-put
               ov 'help-echo
               (format (concat "%s rewrite available: %s or \\[gptel--rewrite-dispatch] for options")
                       (concat (gptel-backend-name gptel-backend) ":" (gptel--model-name gptel-model))
                       mkb))
              (push ov gptel--rewrite-overlays))
            (if-let* ((sym gptel-rewrite-default-action))
                (if-let* ((action (intern (concat "gptel--rewrite-" (symbol-name sym))))
                          ((functionp action)))
                    (funcall action ov) (funcall sym ov))
              (message (concat
                        "LLM rewrite output"
                        (unless (eq (current-buffer) buf)
                          (format " in buffer %s " (buffer-name buf)))
                        (concat " ready: " mkb ", " (propertize "RET" 'face 'help-key-binding)
                                " or " (substitute-command-keys "\\[gptel-rewrite] to continue."))))))))))))

;; * Transient Prefixes for rewriting

(transient-define-prefix gptel--rewrite-directive-menu ()
  "Set the directive (system message) for rewrite actions.

By default, gptel uses the directive associated with the `rewrite'
 key in `gptel-directives'.  You can add more rewrite-specific
 directives to `gptel-directives' and pick one from here."
  [:description gptel-system-prompt--format
   [(gptel--suffix-rewrite-directive)]
   [(gptel--infix-variable-scope)]]
   [:class transient-column
    :setup-children
    (lambda (_) (transient-parse-suffixes
            'gptel--rewrite-directive-menu
            (gptel--setup-directive-menu
             'gptel--rewrite-directive "Rewrite directive")))
    :pad-keys t])

;;;###autoload (autoload 'gptel-rewrite "gptel-rewrite" nil t)
(transient-define-prefix gptel-rewrite ()
  "Rewrite or refactor text region using an LLM."
  [:description
   (lambda ()
     (gptel--describe-directive
      gptel--rewrite-directive (max (- (window-width) 14) 20) " "))
   [""
    (gptel--preset
     :if (lambda () (or (get-char-property (point) 'gptel-rewrite)
                   (use-region-p)))
     :key "@" :format "%d"
     :description
     (lambda ()
       (concat (propertize "Instructions" 'face 'transient-heading)
               (gptel--format-preset-string))))
    ("s" "Set full directive" gptel--rewrite-directive-menu)
    (gptel--infix-rewrite-extra)]]
  ;; FIXME: We are requiring `gptel-transient' because of this suffix, perhaps
  ;; we can get find some way around that?
  [:description "Context for rewrite"
   :if use-region-p
   (gptel--infix-context-remove-all :key "-d")
   (gptel--suffix-context-buffer :key "C" :format "  %k %d")]
  [[:description "Diff Options"
    :if (lambda () gptel--rewrite-overlays)
    ("-b" "Ignore whitespace changes"      ("-b" "--ignore-space-change"))
    ("-w" "Ignore all whitespace"          ("-w" "--ignore-all-space"))
    ("-i" "Ignore case"                    ("-i" "--ignore-case"))
    (gptel--infix-rewrite-diff:-U)]
   [:description "Accept all"
    :if (lambda () gptel--rewrite-overlays)
    (gptel--suffix-rewrite-merge)
    (gptel--suffix-rewrite-accept)
    "Reject all"
    (gptel--suffix-rewrite-reject)]]
  [[:description "Diff rewrite regions"
    :if (lambda () gptel--rewrite-overlays)
    (gptel--suffix-rewrite-diff)
    (gptel--suffix-rewrite-ediff)]]
  [[:description "Rewrite"
    :if (lambda () (or (get-char-property (point) 'gptel-rewrite)
                  (use-region-p)))
    (gptel--suffix-rewrite)]
   ["Dry Run"
    :if (lambda () (and (or gptel-log-level gptel-expert-commands)
                   (or (get-char-property (point) 'gptel-rewrite)
                       (use-region-p))))
    ("I" "Inspect query (Lisp)"
     (lambda ()
       "Inspect the query that will be sent as a lisp object."
       (interactive)
       (gptel--sanitize-model)
       (gptel--inspect-query
        (gptel--suffix-rewrite gptel--rewrite-message t))))
    ("J" "Inspect query (JSON)"
     (lambda ()
       "Inspect the query that will be sent as a JSON object."
       (interactive)
       (gptel--sanitize-model)
       (gptel--inspect-query
        (gptel--suffix-rewrite gptel--rewrite-message t)
        'json)))]]
  (interactive)
  (gptel--rewrite-sanitize-overlays)
  (cond
   ((use-region-p)                      ;Start a/another rewrite
    (let ((transient--history ;No transient reader, so We manage history ourselves
           (alist-get 'gptel--infix-rewrite-extra transient-history)))
      (gptel--rewrite-read-message
       (concat "Instructions (" gptel--read-with-prefix-help
               (format " %s%s) "
                       (propertize "M-RET" 'face 'help-key-binding)
                       (propertize ": More options" 'face 'default)))
       nil (cons 'transient--history 1))))
   (gptel--rewrite-overlays             ;Rewrite actions pending, show options
    (transient-setup 'gptel-rewrite))
   (t (user-error
       "`gptel-rewrite' requires an active region or rewrite in progress."))))

;; * Transient infixes for rewriting

(transient-define-infix gptel--infix-rewrite-extra ()
  "Chat directive (system message) to use for rewriting or refactoring."
  :description "Rewrite instruction"
  :class 'gptel-lisp-variable
  :variable 'gptel--rewrite-message
  :set-value #'gptel--set-with-scope
  :display-nil "(None)"
  :key "d"
  :format " %k %d %v"
  :prompt (concat "Instructions (" gptel--read-with-prefix-help ") ")
  :reader #'gptel--rewrite-read-message)

(transient-define-argument gptel--infix-rewrite-diff:-U ()
  :description "Context lines"
  :class 'transient-option
  :argument "-U"
  :reader #'transient-read-number-N0)

;; * Transient suffixes for rewriting

(transient-define-suffix gptel--suffix-rewrite-directive (&optional cancel)
  "Edit Rewrite directive.

CANCEL is used to avoid touching dynamic rewrite directives,
generated from functions."
  :transient 'transient--do-exit
  :description "Edit full rewrite directive"
  :key "s"
  (interactive
   (list (and
          (functionp gptel--rewrite-directive)
          (not (y-or-n-p
                "Rewrite directive is dynamically generated: Edit its current value instead?")))))
  (if cancel (progn (message "Edit canceled")
                    (call-interactively #'gptel-rewrite))
    (gptel--edit-directive 'gptel--rewrite-directive
      :callback (lambda (_) (call-interactively #'gptel-rewrite))
      :setup #'activate-mark)))

(transient-define-suffix gptel--suffix-rewrite (&optional rewrite-message dry-run)
  "Rewrite or refactor region contents."
  :key "r"
  :description (lambda () (if (get-char-property (point) 'gptel-rewrite) "Iterate" "Rewrite"))
  (interactive (list gptel--rewrite-message))
  (let* ((nosystem (gptel--model-capable-p 'nosystem))
         ;; Try to send context with system message
         (gptel-use-context
          (and gptel-use-context (if nosystem 'user 'system)))
         (prompt (list (or (get-char-property (point) 'gptel-rewrite)
                           (buffer-substring-no-properties (region-beginning) (region-end)))
                       "What is the required change?  I will generate only the final replacement."
                       (or rewrite-message gptel--rewrite-message))))
    (when nosystem
      (setcar prompt (concat (car-safe (gptel--parse-directive
                                        gptel--rewrite-directive 'raw))
                             "\n\n" (car prompt))))
    (prog1 (gptel-request prompt
             :dry-run dry-run
             :system gptel--rewrite-directive
             :stream gptel-stream
             :context
             (let ((ov (or (cdr-safe (get-char-property-and-overlay (point) 'gptel-rewrite))
                           (make-overlay (region-beginning) (region-end) nil t))))
               (overlay-put ov 'category 'gptel)
               (overlay-put ov 'evaporate t)
               ;; NOTE: Switch to `generate-new-buffer' after we drop Emacs 27.1 (#724)
               (cons ov (gptel--temp-buffer " *gptel-rewrite*")))
             :transforms gptel-prompt-transform-functions
             :fsm (gptel-make-fsm :handlers gptel--rewrite-handlers)
             :callback #'gptel--rewrite-callback)
      ;; Move back so that the cursor is on the overlay when done.
      (unless (get-char-property (point) 'gptel-rewrite)
        (when (= (point) (region-end)) (backward-char 1)))
      (deactivate-mark))))

;; Allow this to be called non-interactively for dry runs
(put 'gptel--suffix-rewrite 'interactive-only nil)

(transient-define-suffix gptel--suffix-rewrite-diff (&optional switches)
  "Diff LLM output against buffer."
  :if (lambda () gptel--rewrite-overlays)
  :key "D"
  :description "Diff  LLM rewrites"
  (interactive (list (transient-args transient-current-command)))
  (gptel--rewrite-diff gptel--rewrite-overlays switches))

(transient-define-suffix gptel--suffix-rewrite-ediff ()
  "Ediff LLM output against buffer."
  :if (lambda () gptel--rewrite-overlays)
  :key "E"
  :description "Ediff LLM rewrites"
  (interactive)
  (gptel--rewrite-ediff gptel--rewrite-overlays))

(transient-define-suffix gptel--suffix-rewrite-merge ()
  "Insert LLM output as merge conflicts"
  :if (lambda () gptel--rewrite-overlays)
  :key "M"
  :description "Merge with conflicts"
  (interactive)
  (gptel--rewrite-merge gptel--rewrite-overlays))

(transient-define-suffix gptel--suffix-rewrite-accept ()
  "Accept pending LLM rewrites."
  :if (lambda () gptel--rewrite-overlays)
  :key "A"
  :description "Accept and replace"
  (interactive)
  (gptel--rewrite-accept gptel--rewrite-overlays))

(transient-define-suffix gptel--suffix-rewrite-reject ()
  "Clear pending LLM rewrites."
  :if (lambda () gptel--rewrite-overlays)
  :key "K"
  :description "Clear pending rewrites"
  (interactive)
  (gptel--rewrite-reject gptel--rewrite-overlays))

(provide 'gptel-rewrite)
;;; gptel-rewrite.el ends here

;; Local Variables:
;; outline-regexp: "^;; \\*+"
;; End:
