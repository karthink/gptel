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

(defcustom gptel-rewrite-directives-hook (list #'gptel--rewrite-message)
  "Hook run to generate gptel's default rewrite directives.

Each function in this hook is called with no arguments until one
returns a non-nil value, the base string to use as the
rewrite/refactor instruction.

Use this hook to tailor context-specific refactoring directives.
For example, you can specialize the default refactor directive
for a particular major-mode."
  :group 'gptel
  :type 'hook)

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

(defvar-local gptel--rewrite-message nil)

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

(defun gptel--rewrite-message ()
  "Set a generic refactor/rewrite message for the buffer."
  (if (derived-mode-p 'prog-mode)
      (format "You are a %s programmer. Generate only code, no explanation, no code fences. Refactor the following code."
              (gptel--strip-mode-suffix major-mode))
    (format "You are a prose editor. Rewrite the following text to be more professional.")))

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
  (setq buf (or buf (current-buffer)))
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
          (gptel--rewrite-apply ovs)))
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

(defun gptel--rewrite-apply (&optional ovs)
  "Apply pending LLM responses in OVS or at point."
  (interactive (list (gptel--rewrite-overlay-at)))
  (cl-loop for ov in (ensure-list ovs)
           for ov-beg = (overlay-start ov)
           for ov-end = (overlay-end ov)
           for response = (overlay-get ov 'gptel-rewrite)
           do (overlay-put ov 'before-string nil)
           (goto-char ov-beg)
           (delete-region ov-beg ov-end)
           (insert response))
  (message "Replaced region(s) with LLM output."))

(defun gptel--rewrite-diff (&optional ovs switches)
  "Diff pending LLM responses in OVS or at point."
  (interactive (list (gptel--rewrite-overlay-at)))
  (let* ((buf (current-buffer))
         (newbuf (gptel--rewrite-prepare-buffer ovs))
         (diff-buf (diff-no-select
                    (if-let ((buf-file (buffer-file-name buf)))
                        (expand-file-name buf-file) buf)
                    newbuf switches)))
    (with-current-buffer diff-buf
      (setq-local diff-jump-to-old-file t))
    (display-buffer diff-buf)))

(defun gptel--rewrite-ediff (&optional ovs)
  "Ediff pending LLM responses in OVS or at point."
  (interactive (list (gptel--rewrite-overlay-at)))
  (letrec ((newbuf (gptel--rewrite-prepare-buffer ovs))
           (cwc (current-window-configuration))
           (gptel--ediff-restore
            (lambda ()
              (when (window-configuration-p cwc)
                (set-window-configuration cwc))
              (remove-hook 'ediff-quit-hook gptel--ediff-restore))))
    (add-hook 'ediff-quit-hook gptel--ediff-restore)
    (ediff-buffers (current-buffer) newbuf)))

(defun gptel--rewrite-merge (&optional ovs)
  "Insert pending LLM responses in OVS as merge conflicts."
  (interactive (list (gptel--rewrite-overlay-at)))
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
  (gptel--rewrite-clear ovs))

;; * Transient Prefix for rewriting/refactoring

;;;###autoload (autoload 'gptel-rewrite-menu "gptel-rewrite" nil t)
(transient-define-prefix gptel-rewrite-menu ()
  "Rewrite or refactor text region using an LLM."
  [:description
   (lambda ()
     (format "Directive:  %s"
             (truncate-string-to-width
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
    (gptel--suffix-rewrite)]]
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
          (save-mark-and-excursion
            (run-hook-with-args-until-success
             'gptel-rewrite-directives-hook))))
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
  :prompt "Set directive for rewrite: "
  :reader (lambda (prompt _ history)
            (read-string
             prompt
             (save-mark-and-excursion
               (run-hook-with-args-until-success
                'gptel-rewrite-directives-hook))
             history)))

(transient-define-argument gptel--rewrite-infix-diff:-U ()
  :description "Context lines"
  :class 'transient-option
  :argument "-U"
  :reader #'transient-read-number-N0)

;; * Transient suffixes for rewriting/refactoring

(transient-define-suffix gptel--suffix-rewrite (&optional rewrite-message)
  "Rewrite or refactor region contents."
  :key "r"
  :description #'gptel--refactor-or-rewrite
  (interactive (list gptel--rewrite-message))
  (let* ((prompt (buffer-substring-no-properties
                  (region-beginning) (region-end)))
         (gptel--system-message (or rewrite-message gptel--rewrite-message))
         ;; always send context with system message
         (gptel-use-context (and gptel-use-context 'system)))
    (deactivate-mark)
    (gptel-request prompt
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
            ;; Message user
            (message
             (concat
              "LLM %s output"
              (unless (eq (current-buffer) buf) (format " in buffer %s " buf))
              (substitute-command-keys " ready, \\[gptel-menu] to continue."))
             action-str)))))))

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
