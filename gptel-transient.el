;;; gptel-transient.el --- Transient menu for GPTel  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Keywords: convenience

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

;;

;;; Code:
(require 'cl-lib)
(require 'gptel)
(require 'transient)

(declare-function ediff-regions-internal "ediff")
(declare-function ediff-make-cloned-buffer "ediff-utils")


;; * Helper functions and vars

(defvar-local gptel--rewrite-overlays nil
  "List of active rewrite overlays in the buffer.")

(defun gptel--rewrite-sanitize-overlays ()
  "Ensure gptel's rewrite overlays in buffer are consistent."
  (setq gptel--rewrite-overlays
        (cl-delete-if-not #'overlay-buffer
                          gptel--rewrite-overlays)))

(defvar gptel--set-buffer-locally nil
  "Set model parameters from `gptel-menu' buffer-locally.

Affects the system message too.")

(defun gptel--set-with-scope (sym value &optional scope)
  "Set SYMBOL's symbol-value to VALUE with SCOPE.

If SCOPE is non-nil, set it buffer-locally, else clear any
buffer-local value and set its default global value."
  (if scope
      (set (make-local-variable sym) value)
    (kill-local-variable sym)
    (set sym value)))

(defun gptel--get-directive (args)
  "Find the additional directive in the transient ARGS.

Meant to be called when `gptel-menu' is active."
  (cl-some (lambda (s) (and (stringp s) (string-prefix-p ":" s)
                       (substring s 1)))
                  args))

(defun gptel--instructions-make-overlay (text &optional ov)
  "Make or move overlay OV with TEXT."
  (save-excursion
    ;; Move point to overlay position
    (cond
     ((use-region-p)
      (if (pos-visible-in-window-p (region-beginning))
          (goto-char (region-beginning))))
     ((gptel--in-response-p)
      (gptel-beginning-of-response)
      (skip-chars-forward "\n \t"))
     (t (text-property-search-backward 'gptel 'response)
        (skip-chars-forward "\n \t")))
    ;; Make overlay
    (if (and ov (overlayp ov))
        (move-overlay ov (point) (point) (current-buffer))
      (setq ov (make-overlay (point) (point) nil t)))
    (overlay-put ov 'before-string nil)
    ;; (unless (or (bobp) (eq (char-before) "\n"))
    ;;   (overlay-put ov 'before-string (propertize "\n" 'font-lock-face 'shadow)))
    (overlay-put ov 'category 'gptel)
    (overlay-put
     ov 'after-string
     (concat (propertize (concat "DIRECTIVE: " text)
                         'font-lock-face '(:inherit shadow :weight bold  :box t))
      "\n"))
    ov))

(defconst gptel--read-with-prefix-help
  (concat
   (propertize "(" 'face 'default)
   (propertize "TAB" 'face 'help-key-binding)
   (propertize " to expand, " 'face 'default)
   (propertize "M-n" 'face 'help-key-binding)
   (propertize "/" 'face 'default)
   (propertize "M-p" 'face 'help-key-binding)
   (propertize " for next/previous): " 'face 'default))
  "Help string ;TODO: ")

(defun gptel--read-with-prefix (prefix)
  "Show string PREFIX in the minibuffer after the minibuffer prompt.

PREFIX is shown in an overlay.  Repeated calls to this function
will toggle its visibility state."
  (unless (minibufferp)
    (user-error "This command is intended to be used in the minibuffer."))
  (let* ((update
         (lambda (ov s)
           (overlay-put
            ov 'after-string
            (and s (concat (propertize (concat "\n" s "\n") 'face 'shadow)
                           (make-separator-line))))))
         (max-width (- (window-width) (minibuffer-prompt-end)))
         (max (or max-mini-window-height 0.4))
         (max-height (- (or (and (natnump max) max)
                            (floor (* max (frame-height))))
                        5)))
    (when (and prefix (not (string-empty-p prefix)) (> max-height 1))
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
           (if (> (length prefix) max-width)
               (progn
                 (overlay-put ov 'gptel 'prefix)
                 (let ((disp-size
                        (cl-loop for char across prefix
                                 for idx upfrom 0
                                 with n = 0 with max-length = (* max-height max-width)
                                 if (eq char ?\n) do (cl-incf n)
                                 if (> n max-height) return idx
                                 if (> idx max-length)
                                 return idx
                                 finally return nil)))
                   (funcall update ov
                            (if disp-size
                                (truncate-string-to-width
                                 prefix disp-size  nil nil 'ellipsis)
                              prefix))))
             (overlay-put ov 'gptel 'hide)
             (funcall update ov nil)))
          ('prefix (overlay-put ov 'gptel 'hide)
                 (funcall update ov nil))
          (_ (overlay-put ov 'gptel 'partial)
             (funcall update ov (truncate-string-to-width
                                 prefix max-width nil nil
                                 'ellipsis))))))))

(defun gptel--transient-read-variable (prompt initial-input history)
  "Read value from minibuffer and interpret the result as a Lisp object.

PROMPT, INITIAL-INPUT and HISTORY are as in the Transient reader
documention."
  (ignore-errors
    (read-from-minibuffer prompt initial-input read-expression-map t history)))

(defsubst gptel--refactor-or-rewrite ()
  "Rewrite should be refactored into refactor.

Or is it the other way around?"
  (if (derived-mode-p 'prog-mode)
      "Refactor" "Rewrite"))

(defun gptel-system-prompt--format (&optional message)
  "Format the system MESSAGE for display in gptel's transient menus.

Handle formatting for system messages when the active
`gptel-model' does not support system messages."
  (setq message (or message gptel--system-message))
  (if (gptel--model-capable-p 'nosystem)
      (concat (propertize "[No system message support for model "
                          'face 'transient-heading)
              (propertize (gptel--model-name gptel-model)
                          'face 'warning)
              (propertize "]" 'face 'transient-heading))
    (if message
        (gptel--describe-directive
         message (max (- (window-width) 12) 14) "⮐ ")
      "[No system message set]")))

(defvar gptel--crowdsourced-prompts-url
  "https://raw.githubusercontent.com/f/awesome-chatgpt-prompts/main/prompts.csv"
  "URL for crowdsourced LLM system prompts.")

(defvar gptel--crowdsourced-prompts
  (make-hash-table :test #'equal)
  "Crowdsourced LLM system prompts.")

(defun gptel--crowdsourced-prompts ()
  "Acquire and read crowdsourced LLM system prompts.

These are stored in the variable `gptel--crowdsourced-prompts',
which see."
  (when (hash-table-p gptel--crowdsourced-prompts)
    (when (hash-table-empty-p gptel--crowdsourced-prompts)
      (unless gptel-crowdsourced-prompts-file
        (run-at-time 0 nil #'gptel-system-prompt)
        (user-error "No crowdsourced prompts available"))
      (unless (and (file-exists-p gptel-crowdsourced-prompts-file)
                   (time-less-p
                    (time-subtract (current-time) (days-to-time 14))
                    (file-attribute-modification-time
                     (file-attributes gptel-crowdsourced-prompts-file))))
        (when (y-or-n-p
               (concat
                "Fetch crowdsourced system prompts from "
                (propertize "https://github.com/f/awesome-chatgpt-prompts" 'face 'link)
                "?"))
          ;; Fetch file
          (message "Fetching prompts...")
          (let ((dir (file-name-directory gptel-crowdsourced-prompts-file)))
            (unless (file-exists-p dir) (mkdir dir 'create-parents))
            (if (url-copy-file gptel--crowdsourced-prompts-url
                               gptel-crowdsourced-prompts-file
                               'ok-if-already-exists)
		(message "Fetching prompts... done.")
              (message "Could not retrieve new prompts.")))))
      (if (not (file-readable-p gptel-crowdsourced-prompts-file))
          (progn (message "No crowdsourced prompts available")
                 (call-interactively #'gptel-system-prompt))
        (with-temp-buffer
          (insert-file-contents gptel-crowdsourced-prompts-file)
          (goto-char (point-min))
          (forward-line 1)
          (while (not (eobp))
            (when-let ((act (read (current-buffer))))
              (forward-char)
              (save-excursion
                (while (re-search-forward "\"\"" (line-end-position) t)
                  (replace-match "\\\\\"")))
              (when-let ((prompt (read (current-buffer))))
                (puthash act prompt gptel--crowdsourced-prompts)))
            (forward-line 1)))))
    gptel--crowdsourced-prompts))


;; * Transient classes and methods for gptel

(defclass gptel-lisp-variable (transient-lisp-variable)
  ((display-nil :initarg :display-nil)  ;String to display if value if nil
   (display-map :initarg :display-map :initform nil)) ;Display string from alist display-map
  "Lisp variables that show :display-nil instead of nil.")

(cl-defmethod transient-format-value ((obj gptel-lisp-variable))
  (let ((display-value
         (with-slots (value display-nil display-map) obj
           (cond ((null value) display-nil)
                 (display-map (cdr (assoc value display-map)))
                 (t value)))))
    (propertize
     (if (stringp display-value) display-value (prin1-to-string display-value))
     'face 'transient-value)))

(cl-defmethod transient-infix-set ((obj gptel-lisp-variable) value)
  (funcall (oref obj set-value)
           (oref obj variable)
           (oset obj value value)
           gptel--set-buffer-locally))

(defclass gptel--switches (gptel-lisp-variable)
  ((display-if-true :initarg :display-if-true :initform "True")
   (display-if-false :initarg :display-if-false :initform "False"))
  "Boolean lisp variable class for gptel-transient.")

(cl-defmethod transient-infix-read ((obj gptel--switches))
  "Cycle through the mutually exclusive switches."
  (not (oref obj value)))

(cl-defmethod transient-format-value ((obj gptel--switches))
  (with-slots (value display-if-true display-if-false) obj
      (format
       (propertize "(%s)" 'face 'transient-delimiter)
       (concat
        (propertize display-if-false
                    'face (if value 'transient-inactive-value 'transient-value))
        (propertize "|" 'face 'transient-delimiter)
        (propertize display-if-true
                    'face (if value 'transient-value 'transient-inactive-value))))))

(defclass gptel--scope (gptel--switches)
  ((display-if-true :initarg :display-if-true :initform "for this buffer")
   (display-if-false :initarg :display-if-false :initform "globally"))
  "Singleton lisp variable class for `gptel--set-buffer-locally'.

This is used only for setting this variable via `gptel-menu'.")

(cl-defmethod transient-infix-set ((obj gptel--scope) value)
  (funcall (oref obj set-value)
           (oref obj variable)
           (oset obj value value)))

(defclass gptel-provider-variable (transient-lisp-variable)
  ((model       :initarg :model)
   (model-value :initarg :model-value)
   (always-read :initform t)
   (set-value :initarg :set-value :initform #'set))
  "Class used for gptel-backends.")

(cl-defmethod transient-format-value ((obj gptel-provider-variable))
  (propertize (concat
               (gptel-backend-name (oref obj value)) ":"
               (gptel--model-name
                (buffer-local-value (oref obj model) transient--original-buffer)))
              'face 'transient-value))

(cl-defmethod transient-infix-set ((obj gptel-provider-variable) value)
  (pcase-let ((`(,backend-value ,model-value) value))
    (funcall (oref obj set-value)
             (oref obj variable)
             (oset obj value backend-value)
             gptel--set-buffer-locally)
    (funcall (oref obj set-value)
             (oref obj model)
             (oset obj model-value model-value)
             gptel--set-buffer-locally))
  (transient-setup))

(defclass gptel-option-overlaid (transient-option)
  ((display-nil :initarg :display-nil)
   (overlay :initarg :overlay))
  "Transient options for overlays displayed in the working buffer.")

(cl-defmethod transient-format-value ((obj gptel-option-overlaid))
  "set up the in-buffer overlay for additional directive, a string.

Also format its value in the Transient menu."
  (let ((value (oref obj value))
        (ov    (oref obj overlay))
        (argument (oref obj argument)))
    ;; Making an overlay
    (if (or (not value) (string-empty-p value))
        (when ov (delete-overlay ov))
      (with-current-buffer transient--original-buffer
        (oset obj overlay (gptel--instructions-make-overlay value ov)))
      (letrec ((ov-clear-hook
                (lambda () (when-let* ((ov (oref obj overlay))
                                  ((overlayp ov)))
                        (remove-hook 'transient-exit-hook
                                     ov-clear-hook)
                        (delete-overlay ov)))))
        (add-hook 'transient-exit-hook ov-clear-hook)))
    ;; Updating transient menu display
    (if value
        (propertize (concat argument (truncate-string-to-width value 25 nil nil "..."))
                    'face 'transient-value)
      (propertize
       (concat "(" (symbol-name (oref obj display-nil)) ")")
       'face 'transient-inactive-value))))


;; * Transient Prefixes

(define-obsolete-function-alias 'gptel-send-menu 'gptel-menu "0.3.2")

;; BUG: The `:incompatible' spec doesn't work if there's a `:description' below it.
;;;###autoload (autoload 'gptel-menu "gptel-transient" nil t)
(transient-define-prefix gptel-menu ()
  "Change parameters of prompt to send to the LLM."
  ;; :incompatible '(("-m" "-n" "-k" "-e"))
  [:description gptel-system-prompt--format
   [""
    :if (lambda () (not (gptel--model-capable-p 'nosystem)))
    "Instructions"
    ("s" "Set system message" gptel-system-prompt :transient t)
    (gptel--infix-add-directive)]
   [:pad-keys t
    ""
    "Context"
    (gptel--infix-context-add-region)
    (gptel--infix-context-add-buffer)
    (gptel--infix-context-add-file)
    (gptel--infix-context-remove-all)
    (gptel--suffix-context-buffer)]]
  [["Request Parameters"
    :pad-keys t
    (gptel--infix-variable-scope)
    (gptel--infix-provider)
    (gptel--infix-max-tokens)
    (gptel--infix-num-messages-to-send
     :if (lambda () (or gptel-mode gptel-track-response)))
    (gptel--infix-temperature :if (lambda () gptel-expert-commands))
    (gptel--infix-use-context)
    (gptel--infix-track-response
     :if (lambda () (and gptel-expert-commands (not gptel-mode))))
    (gptel--infix-track-media
     :if (lambda () (and gptel-mode (gptel--model-capable-p 'media))))]
   [" <Prompt from"
    ("m" "Minibuffer instead" "m")
    ("y" "Kill-ring instead" "y")
    ""
    ("i" "Respond in place" "i")]
    [" >Response to"
    ("e" "Echo area" "e")
    ("g" "gptel session" "g"
     :class transient-option
     :prompt "Existing or new gptel session: "
     :reader
     (lambda (prompt _ _history)
       (read-buffer
        prompt (generate-new-buffer-name
                (concat "*" (gptel-backend-name gptel-backend) "*"))
        nil (lambda (buf-name)
              (if (consp buf-name) (setq buf-name (car buf-name)))
              (let ((buf (get-buffer buf-name)))
                (and (buffer-local-value 'gptel-mode buf)
                     (not (eq (current-buffer) buf))))))))
    ("b" "Any buffer" "b"
     :class transient-option
     :prompt "Output to buffer: "
     :reader
     (lambda (prompt _ _history)
       (read-buffer prompt (buffer-name (other-buffer)) nil)))
    ("k" "Kill-ring" "k")]]
  [["Send"
    (gptel--suffix-send)
    ("M-RET" "Regenerate" gptel--regenerate :if gptel--in-response-p)]
   [:description (lambda ()
                   (concat
                    (and gptel--rewrite-overlays "Continue ")
                    (gptel--refactor-or-rewrite)))
    :if (lambda () (or (use-region-p)
                  (and gptel--rewrite-overlays
                       (gptel--rewrite-sanitize-overlays))))
    ("r"
     ;;FIXME: Transient complains if I use `gptel--refactor-or-rewrite' here. It
     ;;reads this function as a suffix instead of a function that returns the
     ;;description.
     (lambda () (if (derived-mode-p 'prog-mode) "Refactor" "Rewrite"))
     gptel-rewrite)]
   ["Tweak Response" :if gptel--in-response-p :pad-keys t
    ("SPC" "Mark" gptel--mark-response)
    ("P" "Previous variant" gptel--previous-variant
     :if gptel--at-response-history-p
     :transient t)
    ("N" "Next variant" gptel--previous-variant
     :if gptel--at-response-history-p
     :transient t)
    ("E" "Ediff previous" gptel--ediff
     :if gptel--at-response-history-p)]
   ["Dry Run" :if (lambda () (or gptel-log-level gptel-expert-commands))
    ("I" "Inspect query (Lisp)"
     (lambda ()
       "Inspect the query that will be sent as a lisp object."
       (interactive)
       (gptel--sanitize-model)
       (gptel--inspect-query
        (gptel--suffix-send
         (cons "I" (transient-args transient-current-command))))))
    ("J" "Inspect query (JSON)"
     (lambda ()
       "Inspect the query that will be sent as a JSON object."
       (interactive)
       (gptel--sanitize-model)
       (gptel--inspect-query
        (gptel--suffix-send
         (cons "I" (transient-args transient-current-command)))
        'json)))]]
  (interactive)
  (gptel--sanitize-model)
  (transient-setup 'gptel-menu))

;; ** Prefix for setting the system prompt.

(defun gptel--setup-directive-menu (sym msg &optional external)
  "Return a list of transient infix definitions for setting gptel
directives.

SYM is the symbol whose value is set to the selected directive..
MSG is the meaning of symbol, used when messaging.
If EXTERNAL is non-nil, include external sources of directives."
  (cl-loop for (type . prompt) in gptel-directives
           ;; Avoid clashes with the custom directive key
           with unused-keys = (delete ?s (number-sequence ?a ?z))
           with width = (window-width)
           for name = (symbol-name type)
           for key = (seq-find (lambda (k) (member k unused-keys)) name (seq-first unused-keys))
           do (setq unused-keys (delete key unused-keys))
           ;; The explicit declaration ":transient transient--do-return" here
           ;; appears to be required for Transient v0.5 and up.  Without it, these
           ;; are treated as suffixes when invoking `gptel-system-prompt' directly,
           ;; and infixes when going through `gptel-menu'.
           ;; TODO: Raise an issue with Transient.
           collect
           (list (key-description (list key))
                 (concat (capitalize name) " "
                         (propertize " " 'display '(space :align-to 20))
                         (propertize
                          (concat "(" (gptel--describe-directive prompt (- width 30)) ")")
                          'face 'shadow))
                 `(lambda () (interactive)
                    (message "%s: %s" ,msg ,(gptel--describe-directive prompt 100 "⮐ "))
                    (gptel--set-with-scope ',sym ',prompt gptel--set-buffer-locally))
	         :transient 'transient--do-return)
           into prompt-suffixes
           finally return
           (nconc
            prompt-suffixes
            (list (list "DEL" "None"
                        `(lambda () (interactive)
                           (message "%s unset" ,msg)
                           (gptel--set-with-scope ',sym nil gptel--set-buffer-locally))
                        :transient 'transient--do-return))
            (and external
                 (list (list "SPC" "Pick crowdsourced prompt"
                             'gptel--read-crowdsourced-prompt
		             ;; NOTE: Quitting the completing read when picking a
		             ;; crowdsourced prompt will cause the transient to exit
		             ;; instead of returning to the system prompt menu.
                             :transient 'transient--do-exit))))))

;;;###autoload (autoload 'gptel-system-prompt "gptel-transient" nil t)
(transient-define-prefix gptel-system-prompt ()
  "Set the LLM system message for LLM interactions.

The \"system message\" establishes directives for the chat
session and modifies the behavior of the LLM. Some examples of
system prompts are:

You are a helpful assistant. Answer as concisely as possible.
Reply only with shell commands and no prose.
You are a poet. Reply only in verse.

More extensive system messages can be useful for specific tasks.

Customize `gptel-directives' for task-specific prompts."
  [:description gptel-system-prompt--format
   [(gptel--suffix-system-message)]
   [(gptel--infix-variable-scope)]]
   [:class transient-column
    :setup-children
    (lambda (_) (transient-parse-suffixes
            'gptel-system-prompt
            (gptel--setup-directive-menu
             'gptel--system-message "Directive" t)))
    :pad-keys t])


;; * Transient Infixes

;; ** Infixes for context aggregation

(transient-define-infix gptel--infix-use-context ()
  "Describe target destination for context injection.

gptel will include with the LLM request any additional context
added with `gptel-add'.  This context can be ignored, included
with the system message or included with the user prompt.

Where in the request this context is included depends on the
value of `gptel-use-context', set from here."
  :description "Include context"
  :class 'gptel-lisp-variable
  :variable 'gptel-use-context
  :format " %k %d %v"
  :set-value #'gptel--set-with-scope
  :display-nil "No"
  :display-map '((nil    . "No")
                 (system . "with system message")
                 (user   . "with user prompt"))
  :key "-i"
  :reader (lambda (prompt &rest _)
            (let* ((choices '(("No"                  . nil)
                              ("with system message" . system)
                              ("with user prompt"    . user)))
                   (destination (completing-read prompt choices nil t)))
              (cdr (assoc destination choices)))))

;; ** Infixes for model parameters

(transient-define-infix gptel--infix-variable-scope ()
  "Set gptel's model parameters and system message in this buffer or globally."
  :argument "scope"
  :variable 'gptel--set-buffer-locally
  :class 'gptel--scope
  :format "  %k %d %v"
  :key "="
  :description (propertize "Set" 'face 'transient-inactive-argument))

(transient-define-infix gptel--infix-num-messages-to-send ()
  "Number of recent messages to send with each exchange.

By default, the full conversation history is sent with every new
prompt. This retains the full context of the conversation, but
can be expensive in token size. Set how many recent messages to
include."
  :description "previous responses"
  :class 'gptel-lisp-variable
  :variable 'gptel--num-messages-to-send
  :set-value #'gptel--set-with-scope
  :display-nil 'all
  :format " %k %v %d"
  :key "-n"
  :prompt "Number of past messages to include for context (leave empty for all): "
  :reader 'gptel--transient-read-variable)

(transient-define-infix gptel--infix-max-tokens ()
  "Max tokens per response.

This is roughly the number of words in the response. 100-300 is a
reasonable range for short answers, 400 or more for longer
responses."
  :description "Response length (tokens)"
  :class 'gptel-lisp-variable
  :variable 'gptel-max-tokens
  :set-value #'gptel--set-with-scope
  :display-nil 'auto
  :key "-c"
  :prompt "Response length in tokens (leave empty: default, 80-200: short, 200-500: long): "
  :reader 'gptel--transient-read-variable)

(transient-define-infix gptel--infix-provider ()
  "AI Provider for Chat."
  :description "GPT Model"
  :class 'gptel-provider-variable
  :prompt "Model: "
  :variable 'gptel-backend
  :set-value #'gptel--set-with-scope
  :model 'gptel-model
  :key "-m"
  :reader (lambda (prompt &rest _)
            (cl-loop
             for (name . backend) in gptel--known-backends
             nconc (cl-loop for model in (gptel-backend-models backend)
                            collect (list (concat name ":" (gptel--model-name model))
                                          backend model))
             into models-alist
             with completion-extra-properties =
             `(:annotation-function
               ,(lambda (comp)
		  (let* ((model (nth 2 (assoc comp models-alist)))
			 (desc (get model :description))
			 (caps (get model :capabilities))
			 (context (get model :context-window))
			 (input-cost (get model :input-cost))
			 (output-cost (get model :output-cost))
			 (cutoff (get model :cutoff-date)))
		    (when (or desc caps context input-cost output-cost cutoff)
		      (concat
		       (propertize " " 'display `(space :align-to 40))
		       (when desc (truncate-string-to-width desc 70 nil ? t t))
		       " " (propertize " " 'display `(space :align-to 112))
		       (when caps (truncate-string-to-width (prin1-to-string caps) 21 nil ? t t))
		       " " (propertize " " 'display `(space :align-to 134))
		       (when context (format "%5dk" context))
		       " " (propertize " " 'display `(space :align-to 142))
		       (when input-cost (format "$%5.2f in" input-cost))
		       (if (and input-cost output-cost) "," " ")
		       " " (propertize " " 'display `(space :align-to 153))
		       (when output-cost (format "$%6.2f out" output-cost))
		       " " (propertize " " 'display `(space :align-to 166))
		       cutoff)))))
             finally return
             (cdr (assoc (completing-read prompt models-alist nil t)
                         models-alist)))))

(transient-define-infix gptel--infix-temperature ()
  "Temperature of request."
  :description "Temperature (0 - 2.0)"
  :class 'gptel-lisp-variable
  :variable 'gptel-temperature
  :set-value #'gptel--set-with-scope
  :key "-t"
  :prompt "Temperature controls the response randomness (0.0-2.0, leave empty for default): "
  :reader 'gptel--transient-read-variable)

(transient-define-infix gptel--infix-track-response ()
  "Distinguish between user messages and LLM responses.

When creating a prompt to send to the LLM, gptel distinguishes
between text entered by the user and past LLM responses.  This is
required for multi-turn conversations, and is always the case in
dedicated chat buffers (in `gptel-mode').

In regular buffers, you can toggle this behavior here or by
customizing `gptel-track-response'.  When response tracking is
turned off, all text will be assigned the \"user\" role when
querying the LLM."
  :description "Track LLM responses"
  :class 'gptel--switches
  :variable 'gptel-track-response
  :set-value #'gptel--set-with-scope
  :display-if-true "Yes"
  :display-if-false "No"
  :key "-v")

(transient-define-infix gptel--infix-track-media ()
  "Send media from \"standalone\" links in the prompt.

When the active `gptel-model' supports it, gptel can send images
or other media from links in the buffer to the LLM.  Only
\"standalone\" links are considered: these are links on their own
line with no surrounding text.

What link types are sent depends on the mime-types the model
supports.  See `gptel-track-media' for more information."
  :description "Send media from links"
  :class 'gptel--switches
  :variable 'gptel-track-media
  :set-value #'gptel--set-with-scope
  :display-if-true "Yes"
  :display-if-false "No"
  :key "-I")

;; ** Infixes for adding and removing context

(declare-function gptel-context--at-point "gptel-context")
(declare-function gptel-add "gptel-context")

(transient-define-suffix gptel--infix-context-add-region ()
  "Add current region to gptel's context."
  :transient 'transient--do-stay
  :key "-r"
  :if (lambda () (or (use-region-p)
                (and (fboundp 'gptel-context--at-point)
                     (gptel-context--at-point))))
  :description
  (lambda ()
    (if (and (fboundp 'gptel-context--at-point)
             (gptel-context--at-point))
        "Remove context at point"
      "Add region to context"))
  (interactive)
  (gptel-add)
  (transient-setup))

(transient-define-suffix gptel--infix-context-add-buffer ()
  "Add a buffer to gptel's context."
  :transient 'transient--do-stay
  :key "-b"
  :description "Add a buffer to context"
  (interactive)
  (gptel-add '(4))
  (transient-setup))

(declare-function gptel-add-file "gptel-context")
(declare-function gptel-context-remove-all "gptel-context")

(transient-define-suffix gptel--infix-context-add-file ()
  "Add a file to gptel's context."
  :transient 'transient--do-stay
  :key "-f"
  :description "Add a file to context"
  (interactive)
  (call-interactively #'gptel-add-file)
  (transient-setup))

(transient-define-suffix gptel--infix-context-remove-all ()
  "Clear gptel's context."
  :if (lambda () gptel-context--alist)
  :transient 'transient--do-stay
  :key "-d"
  :description "Remove all"
  (interactive)
  (when (y-or-n-p "Remove all context? ")
    (gptel-context-remove-all)
    (transient-setup)))

;; ** Infix for the refactor/rewrite system message

(transient-define-infix gptel--infix-add-directive ()
  "Additional directive intended for the next query only.

This is useful to define a quick task on top of a more extensive
or detailed system message.

For example, with code/text selected:

- Rewrite this function to do X while avoiding Y.
- Change the tone of the following paragraph to be more direct.

Or in an extended conversation:

- Phrase you next response in ten words or less.
- Pretend for now that you're an anthropologist."
  :class 'gptel-option-overlaid
  ;; :variable 'gptel--instructions
  :display-nil 'none
  :overlay nil
  :argument ":"
  :prompt (concat "Add instructions for next request only "
                  gptel--read-with-prefix-help)
  :reader (lambda (prompt initial history)
            (let* ((directive
                    (car-safe (gptel--parse-directive gptel--system-message 'raw)))
                   (cycle-prefix (lambda () (interactive)
                                   (gptel--read-with-prefix directive)))
                   (minibuffer-local-map
                    (make-composed-keymap
                     (define-keymap "TAB" cycle-prefix "<tab>" cycle-prefix)
                     minibuffer-local-map))
                   (extra (minibuffer-with-setup-hook cycle-prefix
                            (read-string prompt (or initial " ") history))))
              (unless (string-empty-p extra) extra)))
  :format " %k %d %v"
  :key "d"
  :argument ":"
  :description "Add instruction"
  :transient t)


;; * Transient Suffixes

;; ** Suffix to send prompt

(transient-define-suffix gptel--suffix-send (args)
  "Send ARGS."
  :key "RET"
  :description "Send prompt"
  (interactive (list (transient-args
                      (or transient-current-command 'gptel-menu))))
  (let ((stream gptel-stream)
        (in-place (and (member "i" args) t))
        (output-to-other-buffer-p)
        (backend gptel-backend)
        (model gptel-model)
        (backend-name (gptel-backend-name gptel-backend))
        (buffer) (position)
        (callback) (gptel-buffer-name)
        (system-extra (gptel--get-directive args))
        (dry-run (and (member "I" args) t))
        ;; Input redirection: grab prompt from elsewhere?
        (prompt
         (cond
          ((member "m" args)
           (read-string
            (format "Ask %s: " (gptel-backend-name gptel-backend))
            (and (use-region-p)
                 (buffer-substring-no-properties
                  (region-beginning) (region-end)))))
          ((member "y" args)
           (unless (car-safe kill-ring)
             (user-error "`kill-ring' is empty!  Nothing to send"))
           (if current-prefix-arg
               (read-from-kill-ring "Prompt from kill-ring: ")
             (current-kill 0))))))

    ;; Output redirection: Send response elsewhere?
    (cond
     ((member "e" args)
      (setq stream nil)
      (setq callback
            (lambda (resp info)
              (if resp
                  (message "%s response: %s" backend-name resp)
                (message "%s response error: %s" backend-name (plist-get info :status))))))
     ((member "k" args)
      (setq stream nil)
      (setq callback
            (lambda (resp info)
              (if (not resp)
                  (message "%s response error: %s" backend-name (plist-get info :status))
                (kill-new resp)
                (message "%s response: \"%s\" copied to kill-ring."
                         backend-name
                         (truncate-string-to-width resp 30))))))
     ((setq gptel-buffer-name
            (cl-some (lambda (s) (and (stringp s) (string-prefix-p "g" s)
                                 (substring s 1)))
                     args))
      (setq output-to-other-buffer-p t)
      (let ((reduced-prompt             ;For inserting into the gptel buffer as
                                        ;context, not the prompt used for the
                                        ;request itself
             (or prompt
                 (if (use-region-p)
                     (buffer-substring-no-properties (region-beginning)
                                                     (region-end))
                   (buffer-substring-no-properties
                    (save-excursion
                      (text-property-search-backward
                       'gptel 'response
                       (when (get-char-property (max (point-min) (1- (point)))
                                                'gptel)
                         t))
                      (point))
                    (gptel--at-word-end (point)))))))
        (cond
         ((buffer-live-p (get-buffer gptel-buffer-name))
          ;; Insert into existing gptel session
          (progn
            (setq buffer (get-buffer gptel-buffer-name))
            (with-current-buffer buffer
              (goto-char (point-max))
              (unless (or buffer-read-only
                          (get-char-property (point) 'read-only))
                (insert reduced-prompt))
              (setq position (point))
              (when (and gptel-mode (not dry-run))
                (gptel--update-status " Waiting..." 'warning)))))
         ;; Insert into new gptel session
         (t (setq buffer
                  (gptel gptel-buffer-name
                         (condition-case nil
                             (gptel--get-api-key)
                           ((error user-error)
                            (setq gptel-api-key
                                  (read-passwd
                                   (format "%s API key: "
                                           (gptel-backend-name
                                            gptel-backend))))))
                         reduced-prompt))
            ;; Set backend and model in new session from current buffer
            (with-current-buffer buffer
              (setq gptel-backend backend)
              (setq gptel-model model)
              (unless dry-run
                (gptel--update-status " Waiting..." 'warning))
              (setq position (point)))))))
     ((setq gptel-buffer-name
            (cl-some (lambda (s) (and (stringp s) (string-prefix-p "b" s)
                                 (substring s 1)))
                     args))
      (setq output-to-other-buffer-p t)
      (setq buffer (get-buffer-create gptel-buffer-name))
      (with-current-buffer buffer (setq position (point)))))

    (prog1 (gptel-request prompt
             :buffer (or buffer (current-buffer))
             :position position
             :in-place (and in-place (not output-to-other-buffer-p))
             :stream stream
             :system
             (if system-extra
                 (gptel--merge-additional-directive system-extra)
               gptel--system-message)
             :callback callback
             :dry-run dry-run)

      (unless dry-run
        (gptel--update-status " Waiting..." 'warning))

      ;; NOTE: Possible future race condition here if Emacs ever drops the GIL.
      ;; The HTTP request callback might modify the buffer before the in-place
      ;; text is killed below.
      (when in-place
        ;; Kill the latest prompt
        (let ((beg
               (if (use-region-p)
                   (region-beginning)
                 (save-excursion
                   (text-property-search-backward
                    'gptel 'response
                    (when (get-char-property (max (point-min) (1- (point)))
                                             'gptel)
                      t))
                   (point))))
              (end (if (use-region-p) (region-end) (point))))
          (unless output-to-other-buffer-p
            ;; store the killed text in gptel-history
            (gptel--attach-response-history
             (list (buffer-substring-no-properties beg end))))
          (kill-region beg end)))

      (when output-to-other-buffer-p
        (message (concat "Prompt sent to buffer: "
                         (propertize gptel-buffer-name 'face 'help-key-binding)))
        (display-buffer
         buffer '((display-buffer-reuse-window
                   display-buffer-pop-up-window)
                  (reusable-frames . visible)))))))

(defun gptel--merge-additional-directive (additional &optional full)
  "Merge ADDITIONAL gptel directive with the full system message.

The ADDITIONAL directive is typically specified from `gptel-menu'
and applies only to the next gptel request, see
`gptel--infix-add-directive'.

FULL defaults to the active, full system message.  It may be a
string, a list of prompts or a function, see `gptel-directives'
for details."
  (setq full (or full gptel--system-message))
  (cl-typecase full
    (string (concat full "\n\n" additional))
    (list (let ((copy (copy-sequence full)))
            (setcar copy (concat (car copy) "\n\n" additional))
            copy))
    (function (lambda () (gptel--merge-additional-directive
                     additional (funcall full))))
    (otherwise additional)))

;; Allow calling from elisp
(put 'gptel--suffix-send 'interactive-only nil)

;; ** Suffix to regenerate response

(defun gptel--regenerate ()
  "Regenerate gptel response at point."
  (interactive)
  (when (gptel--in-response-p)
    (pcase-let* ((`(,beg . ,end) (gptel--get-bounds))
                 (history (get-char-property (point) 'gptel-history))
                 (prev-responses (cons (buffer-substring-no-properties beg end)
                                       history)))
      (when gptel-mode                  ;Remove prefix/suffix
        (save-excursion
          (goto-char beg)
          (when (looking-back (concat "\n+" (regexp-quote (gptel-response-prefix-string)))
                              (point-min) 'greedy)
            (setq beg (match-beginning 0)))
          (goto-char end)
          (when (looking-at
                 (concat "\n+" (regexp-quote (gptel-prompt-prefix-string))))
            (setq end (match-end 0)))))
      (delete-region beg end)
      (gptel--attach-response-history prev-responses)
      (call-interactively #'gptel--suffix-send))))

;; ** Set system message
(defun gptel--read-crowdsourced-prompt ()
  "Pick a crowdsourced system prompt for gptel.

This uses the prompts in the variable
`gptel--crowdsourced-prompts', which see."
  (interactive)
  (if (not (hash-table-empty-p (gptel--crowdsourced-prompts)))
      (let ((choice
             (completing-read
              "Pick and edit prompt: "
              (lambda (str pred action)
                (if (eq action 'metadata)
                    `(metadata
                      (affixation-function .
                       (lambda (cands)
                         (mapcar
                          (lambda (c)
                            (list c ""
                             (concat (propertize " " 'display '(space :align-to 22))
                              " " (propertize (gethash c gptel--crowdsourced-prompts)
                               'face 'completions-annotations))))
                          cands))))
                  (complete-with-action action gptel--crowdsourced-prompts str pred)))
              nil t)))
        (when-let ((prompt (gethash choice gptel--crowdsourced-prompts)))
            (setq gptel--system-message prompt)
            (call-interactively #'gptel--suffix-system-message)))
    (message "No prompts available.")))

(transient-define-suffix gptel--suffix-system-message (&optional cancel)
  "Edit LLM system message.

CANCEL is used to avoid touching dynamic system messages,
generated from functions."
  :transient 'transient--do-exit
  :description "Set or edit system message"
  :format " %k   %d"
  :key "s"
  (interactive
   (list (and (functionp gptel--system-message)
              (not (y-or-n-p
                    "Active directive is dynamically generated: Edit its current value instead?")))))
  (if cancel (progn (message "Edit canceled")
                    (call-interactively #'gptel-menu))
    (gptel--edit-directive 'gptel--system-message)))

;; MAYBE: Eventually can be simplified with string-edit, after we drop support
;; for Emacs 28.2.
(defun gptel--edit-directive (sym &optional callback-cmd)
  "Edit a gptel directive in a dedicated buffer.

Store the result in SYM, a symbol.  If CALLBACK-CMD is specified,
it is run after exiting the edit."
  (let ((orig-buf (current-buffer))
        (msg-start (make-marker))
        (directive (symbol-value sym)))
    (when (functionp directive)
      (setq directive (funcall directive)))
    ;; TODO: Handle editing list-of-strings directives
    (with-current-buffer (get-buffer-create "*gptel-system*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (text-mode)
        (setq header-line-format
              (concat
               "Edit your system message below and press "
               (propertize "C-c C-c" 'face 'help-key-binding)
               " when ready, or "
               (propertize "C-c C-k" 'face 'help-key-binding)
               " to abort."))
        (insert
         "# Example: You are a helpful assistant. Answer as concisely as possible.\n"
         "# Example: Reply only with shell commands and no prose.\n"
         "# Example: You are a poet. Reply only in verse.\n\n")
        (add-text-properties
         (point-min) (1- (point))
         (list 'read-only t 'face 'font-lock-comment-face))
        ;; TODO: make-separator-line requires Emacs 28.1+.
        ;; (insert (propertize (make-separator-line) 'rear-nonsticky t))
        (set-marker msg-start (point))
        (save-excursion
          ;; If it's a list, insert only the system message part
          (insert (car-safe (gptel--parse-directive directive 'raw)))
          (push-mark nil 'nomsg))
        (activate-mark))
      (display-buffer (current-buffer)
                      `((display-buffer-below-selected)
                        (body-function . ,#'select-window)
                        (window-height . ,#'fit-window-to-buffer)))
      (let ((quit-to-menu
             (lambda ()
               "Cancel system message update and return."
               (interactive)
               (quit-window)
               (display-buffer
                orig-buf
                `((display-buffer-reuse-window
                   display-buffer-use-some-window)
                  (body-function . ,#'select-window)))
               (when (commandp callback-cmd)
                 (call-interactively callback-cmd)))))
        (use-local-map
         (make-composed-keymap
          (define-keymap
            "C-c C-c" (lambda ()
                        "Confirm system message and return."
                        (interactive)
                        (let ((system-message
                               (buffer-substring-no-properties msg-start (point-max))))
                          (with-current-buffer orig-buf
                            (gptel--set-with-scope sym
                                                   (if (cdr-safe directive) ;Handle list of strings
                                                       (prog1 directive (setcar directive system-message))
                                                     system-message)
                                                   gptel--set-buffer-locally)))
                        (funcall quit-to-menu))
            "C-c C-k" quit-to-menu)
          text-mode-map))))))

;; ** Suffix for displaying and removing context
(declare-function gptel-context--buffer-setup "gptel-context")
(declare-function gptel-context--collect "gptel-context")

(transient-define-suffix gptel--suffix-context-buffer ()
  "Display all contexts from all buffers & files."
  :transient 'transient--do-exit
  :key " C"
  :if (lambda () gptel-context--alist)
  :description
  (lambda ()
    (pcase-let*
        ((contexts (and gptel-context--alist (gptel-context--collect)))
         (buffer-count (length contexts))
         (`(,file-count ,ov-count)
          (if (> buffer-count 0)
              (cl-loop for (buf-file . ovs) in contexts
                       if (bufferp buf-file)
                       sum (length ovs) into ov-count
                       else count (stringp buf-file) into file-count
                       finally return (list file-count ov-count))
            (list 0 0))))
      (concat "Inspect "
              (format
               (propertize "(%s)" 'face 'transient-delimiter)
               (propertize
                (concat
                 (and (> ov-count 0)
                      (format "%d region%s in %d buffer%s"
                              ov-count (if (> ov-count 1) "s" "")
                              (- buffer-count file-count)
                              (if (> ( - buffer-count file-count) 1) "s" "")))
                 (and (> file-count 0)
                      (propertize
                       (format "%s%d file%s"
                               (if (> ov-count 0) ", " "") file-count
                               (if (> file-count 1) "s" "")))))
                'face (if (zerop (length contexts))
                          'transient-inactive-value
                        'transient-value))))))
  (interactive)
  (gptel-context--buffer-setup))

(provide 'gptel-transient)
;;; gptel-transient.el ends here

;; Local Variables:
;; outline-regexp: "^;; \\*+"
;; eval: (outline-minor-mode 1)
;; End:
