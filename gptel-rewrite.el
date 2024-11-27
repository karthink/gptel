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

(declare-function diff-no-select "diff")

;; * User options

(defcustom gptel-rewrite-directives-hook nil
  "Hook run to generate gptel's default rewrite directives.

Each function in this hook is called with no arguments until one
returns a non-nil value, the base string to use as the
rewrite/refactor instruction.

Use this hook to tailor context-specific refactoring directives.
For example, you can specialize the default refactor directive
for a particular major-mode or project."
  :group 'gptel
  :type 'hook)

(defcustom gptel-rewrite-default-action nil
  "Action to take when rewriting a text region using gptel.

When the LLM response with the rewritten text is received, you can
- merge it with the current region, possibly creating a merge conflict,
- diff or ediff against the original region,
- or accept it in place, overwriting the original region.

If this option is nil (the default), gptel waits for an explicit
command.  Set it to the symbol merge, diff, ediff or replace to
automatically do one of these things instead."
  :group 'gptel
  :type '(choice
          (const :tag "Wait" nil)
          (const :tag "Merge with current region" gptel--rewrite-merge)
          (const :tag  "Diff against current region" gptel--rewrite-diff)
          (const :tag "Ediff against current region" gptel--rewrite-ediff)
          (const :tag "Replace current region" gptel--rewrite-apply)
          (function :tag "Custom action")))

(defface gptel-rewrite-highlight-face
  '((((class color) (min-colors 88) (background dark))
     :background "#041714" :extend t)
    (((class color) (min-colors 88) (background light))
     :background "light goldenrod yellow" :extend t)
    (t :inherit secondary-selection))
  "Face for highlighting regions with pending rewrites."
  :group 'gptel)

;; * Variables

(defvar-keymap gptel-rewrite-actions-map
  :doc "Keymap for gptel rewrite actions at point."
  "C-c C-k" #'gptel--rewrite-clear
  "C-c C-a" #'gptel--rewrite-apply
  "C-c C-d" #'gptel--rewrite-diff
  "C-c C-e" #'gptel--rewrite-ediff
  "C-c C-n" #'gptel--rewrite-next
  "C-c C-p" #'gptel--rewrite-previous
  "C-c C-m" #'gptel--rewrite-merge)

(defvar-local gptel--rewrite-overlays nil
  "List of active rewrite overlays in the buffer.")

(defvar-local gptel--rewrite-message nil
  "Request-specific instructions for a gptel-rewrite action.")

;; ;; NOTE: Should we expose this option?  `gptel-rewrite-directives-hook'
;; ;; already functions as a robust way to customize the system message for
;; ;; rewriting.
;;
;; (defcustom gptel-rewrite-directive
;;   #'gptel--rewrite-directive
;;   "A gptel template (system message, a string), or function that
;; returns a system message intended for a rewrite action."
;;   :group 'gptel
;;   :type '(choice
;;           (string :tag "System message")
;;           (function :tag "Function that returns system message")))

(defun gptel--rewrite-directive ()
  "General gptel directive when rewriting or refactoring.

This supplies the general instructions to the LLM that are not
specific to any particular required change.

The returned string is interpreted as the system message for the
rewrite request.  To substitute your own, add to
`gptel-rewrite-directives-hook', which see."
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
                            "- Do not ask for further clarification, and make "
                            "any assumptions you need to follow instructions.")
                    article lang lang lang)
          (concat
           (if (string-empty-p lang)
               "You are an editor."
             (format "You are %s %s editor." article lang))
           "  Follow my instructions and improve or rewrite the text I provide."
           "  Generate ONLY the replacement text,"
           " without any explanation or markdown code fences.")))))

;; * Helper functions

(defun gptel--rewrite-sanitize-overlays ()
  "Ensure gptel's rewrite overlays in buffer are consistent."
  (setq gptel--rewrite-overlays
        (cl-delete-if-not #'overlay-buffer
                          gptel--rewrite-overlays)))

(defsubst gptel--refactor-or-rewrite ()
  "Rewrite should be refactored into refactor.

Or is it the other way around?"
  (if (derived-mode-p 'prog-mode)
      "Refactor" "Rewrite"))

(defun gptel--rewrite-expand-prompt ()
  "Show the active rewrite directive in the minibuffer.

The directive, in this case just the system message, is shown in
an overlay.  Repeated calls to this command will toggle its
visibility state."
  (interactive)
  (unless (minibufferp)
    (user-error "This command is intended to be used in the minibuffer."))
  (let ((full (with-minibuffer-selected-window
                (gptel--rewrite-directive)))
        (update
         (lambda (ov s)
           (overlay-put
            ov 'after-string
            (and s (concat (propertize (concat "\n" s "\n") 'face 'shadow)
                           (make-separator-line)))))))
    (when full
      (unless visual-line-mode (visual-line-mode 1))
      (goto-char (minibuffer-prompt-end))
      (pcase-let ((`(,prop . ,ov)
                   (get-char-property-and-overlay
                    (point-min) 'gptel)))
        (unless ov
          (setq ov (make-overlay
                    (point-min) (minibuffer-prompt-end) nil t)))
        (pcase prop
          ('partial
           (if (> (length full) (window-width))
               (progn (overlay-put ov 'gptel 'full)
                      (funcall update ov full))
             (overlay-put ov 'gptel 'hide)
             (funcall update ov nil)))
          ('full (overlay-put ov 'gptel 'hide)
                 (funcall update ov nil))
          (_ (overlay-put ov 'gptel 'partial)
             (funcall update ov (truncate-string-to-width
                                 full (window-width) nil nil
                                 'ellipsis))))))))

(defun gptel--rewrite-key-help (callback)
  "Eldoc documentation function for gptel rewrite actions.

CALLBACK is supplied by Eldoc, see
`eldoc-documentation-functions'."
  (when (and gptel--rewrite-overlays
             (get-char-property (point) 'gptel-rewrite))
      (funcall callback
               (format (substitute-command-keys "%s rewrite available: accept \\[gptel--rewrite-apply], clear \\[gptel--rewrite-clear], merge \\[gptel--rewrite-merge], diff \\[gptel--rewrite-diff] or ediff \\[gptel--rewrite-ediff]")
                       (propertize (concat (gptel-backend-name gptel-backend)
                                           ":" (gptel--model-name gptel-model))
                                   'face 'mode-line-emphasis)))))

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
          (gptel--rewrite-apply ovs newbuf)))
      newbuf)))

;; * Refactor action functions

(defun gptel--rewrite-clear (&optional ovs)
  "Clear pending LLM responses in OVS or at point."
  (interactive (list (gptel--rewrite-overlay-at)))
  (dolist (ov (ensure-list ovs))
    (setq gptel--rewrite-overlays (delq ov gptel--rewrite-overlays))
    (delete-overlay ov))
  (unless gptel--rewrite-overlays
    (remove-hook 'eldoc-documentation-functions 'gptel--rewrite-key-help 'local))
  (message "Cleared pending LLM response(s)."))

(defun gptel--rewrite-apply (&optional ovs buf)
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

(defun gptel--rewrite-diff (&optional ovs switches)
  "Diff pending LLM responses in OVS or at point."
  (interactive (list (gptel--rewrite-overlay-at)))
  (when-let* ((ov-buf (overlay-buffer (or (car-safe ovs) ovs)))
              ((buffer-live-p ov-buf)))
    (let* ((newbuf (gptel--rewrite-prepare-buffer ovs))
           (diff-buf (diff-no-select
                      (if-let ((buf-file (buffer-file-name ov-buf)))
                          (expand-file-name buf-file) ov-buf)
                      newbuf switches)))
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
             (gptel--ediff-restore
              (lambda ()
                (when (window-configuration-p cwc)
                  (set-window-configuration cwc))
                (remove-hook 'ediff-quit-hook gptel--ediff-restore))))
      (add-hook 'ediff-quit-hook gptel--ediff-restore)
      (ediff-buffers ov-buf newbuf))))

(defun gptel--rewrite-merge (&optional ovs)
  "Insert pending LLM responses in OVS as merge conflicts."
  (interactive (list (gptel--rewrite-overlay-at)))
  (when-let* ((ov-buf (overlay-buffer (or (car-safe ovs) ovs)))
              ((buffer-live-p ov-buf)))
    (with-current-buffer ov-buf
      (let ((changed))
        (dolist (ov (ensure-list ovs))
          (save-excursion
            (when-let (new-str (overlay-get ov 'gptel-rewrite))
              ;; Insert merge
              (goto-char (overlay-start ov))
              (unless (bolp) (insert "\n"))
              (insert-before-markers "<<<<<<< original\n")
              (goto-char (overlay-end ov))
              (unless (bolp) (insert "\n"))
              (insert
               "=======\n" new-str
               "\n>>>>>>> " (gptel-backend-name gptel-backend) "\n")
              (setq changed t))))
        (when changed (smerge-mode 1)))
      (gptel--rewrite-clear ovs))))

;; * Transient Prefix for rewriting/refactoring

;;;###autoload (autoload 'gptel-rewrite-menu "gptel-rewrite" nil t)
(transient-define-prefix gptel-rewrite-menu ()
  "Rewrite or refactor text region using an LLM."
  [:description
   (lambda ()
     (format "%s" (truncate-string-to-width
                   gptel--rewrite-message
                   (max (- (window-width) 14) 20) nil nil t)))
   (gptel--infix-rewrite-prompt)]
  ;; FIXME: We are requiring `gptel-transient' because of this suffix, perhaps
  ;; we can get find some way around that?
  [:description (lambda () (concat "Context for " (gptel--refactor-or-rewrite)))
   :if use-region-p
   (gptel--suffix-context-buffer :key "C")]
  [[:description "Diff Options"
    :if (lambda () gptel--rewrite-overlays)
    ("-b" "Ignore whitespace changes"      ("-b" "--ignore-space-change"))
    ("-w" "Ignore all whitespace"          ("-w" "--ignore-all-space"))
    ("-i" "Ignore case"                    ("-i" "--ignore-case"))
    (gptel--rewrite-infix-diff:-U)]
   [:description gptel--refactor-or-rewrite
    :if use-region-p
    (gptel--suffix-rewrite)]
   ["Dry Run"
    :if (lambda () (and (use-region-p)
                   (or gptel-log-level gptel-expert-commands)))
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
  [[:description (lambda () (concat "Diff " (gptel--refactor-or-rewrite) "s"))
    :if (lambda () gptel--rewrite-overlays)
    (gptel--suffix-rewrite-diff)
    (gptel--suffix-rewrite-ediff)]
   [:description (lambda () (concat "Continue " (gptel--refactor-or-rewrite) "s"))
    :if (lambda () (gptel--rewrite-sanitize-overlays))
    (gptel--suffix-rewrite-merge)
    (gptel--suffix-rewrite-apply)]
   [:description (lambda () (concat "Reject " (gptel--refactor-or-rewrite) "s"))
    :if (lambda () (gptel--rewrite-sanitize-overlays))
    (gptel--suffix-rewrite-clear)]]
  (interactive)
  (unless gptel--rewrite-message
    (setq gptel--rewrite-message
          (concat (gptel--refactor-or-rewrite) ": ")))
  (transient-setup 'gptel-rewrite-menu))

;; * Transient infixes for rewriting/refactoring

(transient-define-infix gptel--infix-rewrite-prompt ()
  "Chat directive (system message) to use for rewriting or refactoring."
  :description (lambda () (if (derived-mode-p 'prog-mode)
                         "Set directives for refactor"
                       "Set directives for rewrite"))
  :format "%k %d"
  :class 'transient-lisp-variable
  :variable 'gptel--rewrite-message
  :key "d"
  :prompt (concat "Instructions ("
                  (propertize "TAB" 'face 'help-key-binding) " to expand, "
                  (propertize "M-n" 'face 'help-key-binding) "/"
                  (propertize "M-p" 'face 'help-key-binding) " for next/previous): ")
  :reader (lambda (prompt _ history)
            (let ((minibuffer-local-map
                   (make-composed-keymap
                    (define-keymap "TAB" #'gptel--rewrite-expand-prompt
                      "<tab>" #'gptel--rewrite-expand-prompt)
                    minibuffer-local-map)))
              (minibuffer-with-setup-hook #'gptel--rewrite-expand-prompt
                (read-string
                 prompt
                 (or gptel--rewrite-message
                     (concat (gptel--refactor-or-rewrite) ": "))
                 history)))))

(transient-define-argument gptel--rewrite-infix-diff:-U ()
  :description "Context lines"
  :class 'transient-option
  :argument "-U"
  :reader #'transient-read-number-N0)

;; * Transient suffixes for rewriting/refactoring

(transient-define-suffix gptel--suffix-rewrite (&optional rewrite-message dry-run)
  "Rewrite or refactor region contents."
  :key "r"
  :description #'gptel--refactor-or-rewrite
  (interactive (list gptel--rewrite-message))
  (let* ((nosystem (gptel--model-capable-p 'nosystem))
         ;; Try to send context with system message
         (gptel-use-context
          (and gptel-use-context (if nosystem 'user 'system)))
         (prompt (list (buffer-substring-no-properties (region-beginning) (region-end))
                       "What is the required change?"
                       (or rewrite-message gptel--rewrite-message))))
    (deactivate-mark)
    (when nosystem (setcar prompt (concat (gptel--rewrite-directive)
                                          "\n\n" (car prompt))))
    (gptel-request prompt
      :dry-run dry-run
      :system #'gptel--rewrite-directive
      :context
      (let ((ov (make-overlay (region-beginning) (region-end))))
        (overlay-put ov 'category 'gptel)
        (overlay-put ov 'evaporate t)
        ov)
      :callback
      (lambda (response info)
        (if (not response)
            (message (concat "LLM response error: %s. Rewrite/refactor in buffer %s canceled."
                             (propertize "❌" 'face 'error))
                     (plist-get info :status)
                     (plist-get info :buffer))
          ;; Store response
          (let ((buf (plist-get info :buffer))
                 (ov  (plist-get info :context))
                 (action-str) (hint-str))
            (when (buffer-live-p buf)
              (with-current-buffer buf
                (if (derived-mode-p 'prog-mode)
                    (progn
                      (setq action-str "refactor")
                      (when (string-match-p "^```" response)
                        (setq response (replace-regexp-in-string "^```.*$" "" response))))
                  (setq action-str "rewrite"))
                (setq hint-str (concat "[" (gptel-backend-name gptel-backend)
                                       ":" (gptel--model-name gptel-model) "] "
                                       (upcase action-str) " READY ✓\n"))
                (add-hook 'eldoc-documentation-functions #'gptel--rewrite-key-help nil 'local)
                (overlay-put ov 'gptel-rewrite response)
                (overlay-put ov 'face 'gptel-rewrite-highlight-face)
                (overlay-put ov 'keymap gptel-rewrite-actions-map)
                (overlay-put ov 'before-string
                             (concat (propertize
                                      " " 'display `(space :align-to (- right ,(1+ (length hint-str)))))
                                     (propertize hint-str 'face 'success)))
                (overlay-put
                 ov 'help-echo
                 (format "%s rewrite available:
- accept \\[gptel--rewrite-apply],
- clear  \\[gptel--rewrite-clear],
- merge  \\[gptel--accept-merge],
- diff   \\[gptel--rewrite-diff],
- ediff  \\[gptel--rewrite-ediff]"
                         (propertize (concat (gptel-backend-name gptel-backend)
                                             ":" (gptel--model-name gptel-model)))))
                (push ov gptel--rewrite-overlays))
              (if (functionp gptel-rewrite-default-action)
                  (funcall gptel-rewrite-default-action ov)
                ;; Message user
                (message
                 (concat
                  "LLM %s output"
                  (unless (eq (current-buffer) buf) (format " in buffer %s " buf))
                  (substitute-command-keys " ready, \\[gptel-menu] to continue."))
                 action-str)))))))))

(transient-define-suffix gptel--suffix-rewrite-diff (&optional switches)
  "Diff LLM output against buffer."
  :if (lambda () gptel--rewrite-overlays)
  :key "D"
  :description (concat "Diff  LLM " (downcase (gptel--refactor-or-rewrite)) "s")
  (interactive (list (transient-args transient-current-command)))
  (gptel--rewrite-diff gptel--rewrite-overlays switches))

(transient-define-suffix gptel--suffix-rewrite-ediff ()
  "Ediff LLM output against buffer."
  :if (lambda () gptel--rewrite-overlays)
  :key "E"
  :description (concat "Ediff LLM " (downcase (gptel--refactor-or-rewrite)) "s")
  (interactive)
  (gptel--rewrite-ediff gptel--rewrite-overlays))

(transient-define-suffix gptel--suffix-rewrite-merge ()
  "Insert LLM output as merge conflicts"
  :if (lambda () gptel--rewrite-overlays)
  :key "cm"
  :description "Accept as merge conflicts"
  (interactive)
  (gptel--rewrite-merge gptel--rewrite-overlays))

(transient-define-suffix gptel--suffix-rewrite-apply ()
  "Accept pending LLM rewrites."
  :if (lambda () gptel--rewrite-overlays)
  :key "ca"
  :description "Accept in-place"
  (interactive)
  (gptel--rewrite-apply gptel--rewrite-overlays))

(transient-define-suffix gptel--suffix-rewrite-clear ()
  "Clear pending LLM rewrites."
  :if (lambda () gptel--rewrite-overlays)
  :key "ck"
  :description (concat "Clear pending "
                       (downcase (gptel--refactor-or-rewrite))
                       "s")
  (interactive)
  (gptel--rewrite-clear gptel--rewrite-overlays))

(provide 'gptel-rewrite)
;;; gptel-rewrite.el ends here

;; Local Variables:
;; outline-regexp: "^;; \\*+"
;; End:
