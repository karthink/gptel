;;; gptel-transient.el --- Transient menu for gptel  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025  Karthik Chikmagalur

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

If SCOPE is t, set it buffer-locally.
If SCOPE is 1, reset it after the next gptel-request. (oneshot)
Otherwise, clear any buffer-local value and set its default
global value."
  (pcase scope
    (1 (put sym 'gptel-history (symbol-value sym))
       (set sym value)
       (letrec ((restore-value
                 (lambda ()
                   (remove-hook 'gptel-post-request-hook restore-value)
                   (run-at-time         ; Required to work around let bindings
                    0 nil (lambda (s)        ; otherwise this change is overwritten!
                            (set s (get s 'gptel-history))
                            (put s 'gptel-history nil))
                    sym))))
         (add-hook 'gptel-post-request-hook restore-value)))
    ('t (set (make-local-variable sym) value))
    (_ (kill-local-variable sym)
       (set sym value))))

(defvar gptel--preset nil
  "Name of last applied gptel preset.

For internal use only.")

(defun gptel--preset-mismatch-p (name)
  "Check if gptel preset with NAME is in effect."
  (let ((elm (or (gptel-get-preset name)
                 (gptel-get-preset (intern-soft name))))
        key val)
    (catch 'mismatch
      (while elm
        (setq key (pop elm) val (pop elm))
        (cond
         ((memq key '(:description :parents)) 'nil)
         ((eq key :system)
          (or (equal gptel--system-message val)
              (and-let* (((symbolp val))
                         (p (assq val gptel-directives)))
                (equal gptel--system-message (cdr p)))
              (throw 'mismatch t)))
         ((eq key :backend)
          (or (if (stringp val)
                  (equal (gptel-backend-name gptel-backend) val)
                (eq gptel-backend val))
              (throw 'mismatch t)))
         ((eq key :tools)
          (if (eq (car-safe val) :append)
              (cl-loop for name in (cdr val) ;preset tools contained in gptel-tools
                       unless (memq (gptel-get-tool name) gptel-tools)
                       do (throw 'mismatch t))
            (or (equal (sort val #'string-lessp) ;preset tools same as gptel-tools
                       (sort (mapcar #'gptel-tool-name gptel-tools)
                             #'string-lessp))
                (throw 'mismatch t))))
         (t (let* ((suffix (substring
                            (if (symbolp key) (symbol-name key) key) 1))
                   (sym (or (intern-soft (concat "gptel-" suffix))
                            (intern-soft (concat "gptel--" suffix)))))
              (or (null sym)
                  (and (boundp sym) (equal (eval sym) val))
                  (throw 'mismatch t)))))))))

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
   (propertize "TAB" 'face 'help-key-binding)
   (propertize ": expand, " 'face 'default)
   (propertize "M-n" 'face 'help-key-binding)
   (propertize "/" 'face 'default)
   (propertize "M-p" 'face 'help-key-binding)
   (propertize ": next/previous" 'face 'default))
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
    (if (and prefix (not (string-empty-p prefix)) (> max-height 1))
        (progn
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
                                     'ellipsis))))))
      (when-let* ((prop-ov (get-char-property-and-overlay (point-min) 'gptel)))
        (when (overlayp (cdr prop-ov)) (delete-overlay (cdr prop-ov)))))))

(defvar gptel--minibuffer-prompt-history nil
  "History of prompts read from the minibuffer by gptel.")

(defun gptel--read-minibuffer-prompt (&optional read-prompt)
  "Read a user prompt from the minibuffer.

Prompt with READ-PROMPT if supplied.  Return a cons cell of the buffer
region (if included) and the provided instructions."
  (let* ((include-region (use-region-p))
         (cb (current-buffer))
         (get-region (lambda () (with-current-buffer cb
                             (and include-region
                                  (buffer-substring-no-properties
                                   (region-beginning) (region-end))))))
         (cycle-prefix (lambda () (interactive)
                         (let ((p (point)))
                           (gptel--read-with-prefix (funcall get-region))
                           (goto-char p))))
         (toggle-region (lambda () (interactive)
                          (if include-region
                              (progn (setq include-region nil)
                                     (gptel--read-with-prefix nil))
                            (setq include-region t)
                            (funcall cycle-prefix))))
         (edit-in-buffer
          (lambda () (interactive)
            (gptel--edit-directive nil
              :initial (minibuffer-contents)
              :prompt (if include-region
                          (with-current-buffer cb (buffer-substring-no-properties
                                                   (region-beginning) (region-end)))
                        "# Edit prompt below")
              :setup (lambda () (goto-char (point-max)) (run-at-time 0 nil #'recenter))
              :callback (lambda (msg)
                          (if (not msg)
                              (minibuffer-quit-recursive-edit)
                            (delete-region (minibuffer-prompt-end) (point-max))
                            (insert msg) (exit-minibuffer))))))
         (minibuffer-local-map
          (make-composed-keymap (define-keymap
                                  "M-RET" toggle-region "C-c C-e" edit-in-buffer)
                                minibuffer-local-map)))
    (let ((user-prompt
           (minibuffer-with-setup-hook
               (lambda () (add-hook 'completion-at-point-functions
                               #'gptel-preset-capf nil t)
                 (funcall cycle-prefix)
                 ;; HACK for lucid Emacs, where `make-separator-line' is wonky.  The
                 ;; minibuffer prompt gets cut off -- force redisplay to fix:
                 (insert " ") (redisplay) (delete-char -1))
             (read-string
              (or read-prompt
                  (concat (format "Ask %s" (gptel-backend-name gptel-backend))
                          (if (use-region-p) ;NOTE: not "include-region" as this is only read once
                            (concat " (" (propertize "M-RET" 'face 'help-key-binding)
                                    (propertize ": Include/Ignore selection" 'face 'default)
                                    "): ")
                            ": ")))
              nil 'gptel--minibuffer-prompt-history))))
      (cons (funcall get-region) user-prompt))))

(defun gptel--transient-read-number (prompt _initial-input history)
  "Read a numeric value from the minibuffer.

PROMPT, _INITIAL-INPUT and HISTORY are as in the transient reader
documention.  Return nil if user does not provide a number, for default."
  ;; Workaround for buggy transient behaviour when dealing with
  ;; non-string values.  See: https://github.com/magit/transient/issues/172
  (when-let* ((val (symbol-value history)))
    (when (not (stringp (car val)))
      (setcar val (number-to-string (car val)))))
  (let* ((minibuffer-default-prompt-format "")
	 (num (read-number prompt -1 history)))
    (if (= num -1) nil num)))

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

(defun gptel--tools-init-value (obj)
  "Set the initial state of a tool OBJ in `gptel-tools'.

OBJ is a tool-infix of type `gptel--switch'."
  (when-let* ((name (car (member (oref obj argument)
                                 (mapcar #'cadr
                                         (plist-get (transient-scope) :tools))))))
    (oset obj value (list (oref obj category) name))))

(defvar gptel--crowdsourced-prompts-url
  "https://raw.githubusercontent.com/f/awesome-chatgpt-prompts/main/prompts.csv"
  "URL for crowdsourced LLM system prompts.")

(defvar gptel--crowdsourced-prompts
  (make-hash-table :test #'equal)
  "Crowdsourced LLM system prompts.")

(defun gptel--read-csv-column ()
  "Read next CSV column in the current buffer.

Supports both quoted and non-quoted columns (RFC 4180)."
  (let ((start (point)))
    (unless (eolp)
      (let ((column
	     (if (eq (char-after) ?\")
		 (when (re-search-forward "\",\\|\"$" nil t)
		   (let ((end (match-beginning 0)))
		     (buffer-substring-no-properties (+ start 1) (if (eolp) (- end 1) end))))
	       (when (search-forward "," (line-end-position) t)
		 (let ((end (match-beginning 0)))
		   (buffer-substring-no-properties start end))))))
	(string-replace "\"\"" "\"" column)))))

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
	    (when-let* ((act (gptel--read-csv-column))
			(prompt (gptel--read-csv-column)))
		(puthash act prompt gptel--crowdsourced-prompts))
	      (forward-line 1)))))
    gptel--crowdsourced-prompts))

(defun gptel--describe-infix-context ()
  (if (null gptel-context--alist) "Context"
    (pcase-let*
        ((contexts (gptel-context--collect))
         (buffer-count (length contexts))
         (`(,file-count ,ov-count)
          (if (> buffer-count 0)
              (cl-loop for (buf-file . ovs) in contexts
                       if (bufferp buf-file)
                       sum (length ovs) into ov-count
                       else count (stringp buf-file) into file-count
                       finally return (list file-count ov-count))
            (list 0 0))))
      (concat "Context ("
              (propertize
               (concat
                (and (> ov-count 0)
                     (format "%d region%s in %d buffer%s"
                             ov-count (if (> ov-count 1) "s" "")
                             (- buffer-count file-count)
                             (if (> ( - buffer-count file-count) 1) "s" "")))
                (and (> file-count 0)
                     (format "%s%d file%s"
                             (if (> ov-count 0) ", " "") file-count
                             (if (> file-count 1) "s" ""))))
               'face 'warning)
              ")"))))

(defun gptel--describe-suffix-send ()
  "Describe the action of `gptel--suffix-send'."
  (cl-flet ((ptv (s) (propertize s 'face 'warning))
            (pth (s) (propertize s 'face 'transient-heading)))
    (let* ((args (or (and transient-current-command
                          (transient-args transient-current-command))
                     ;; Not yet exported, simulate.  HACK: We are accessing
                     ;; Transient's internal variables here for live updates.
                     (let* ((transient-current-command (oref transient--prefix command))
                            (transient-current-suffixes transient--suffixes))
                       (transient-args transient-current-command))))
           (lbeg (line-number-at-pos (if (use-region-p) (region-beginning)
                                       (point-min))))
           (lend (line-number-at-pos (if (use-region-p) (region-end)
                                       (point))))
           (ltext (ptv (if (> lend lbeg)
                           (format " (lines %d-%d)" lbeg lend)
                         (format " (line %d)" lbeg))))
           (dest) (context))
      (setq dest (cond
                  ((member "e" args) (ptv "echo area"))
                  ((member "k" args) (ptv "kill-ring"))
                  ((cl-some (lambda (s)
                              (and (stringp s) (memq (aref s 0) '(?g ?b))
                                   (not (equal (substring s 1) (buffer-name)))
                                   (concat (pth "buffer ") (ptv (substring s 1)))))
                            args))))
      (setq context
            (and gptel-context--alist
                 (let ((lc (length gptel-context--alist)))
                   (concat (pth " along with ") (ptv (format "%d" lc))
                           (pth (concat " context source" (and (/= lc 1) "s")))))))
      (cond ((member "m" args)
             (concat (pth "Read prompt from ") (ptv "minibuffer")
                     context
                     (if dest (concat (pth ", response to ") dest)
                       (concat (pth ", insert response at point")))))
            ((member "y" args)
             (concat (pth "Send prompt from ")
                     (concat (ptv "kill-ring (")
                             (if-let* ((val (current-kill 0))
                                       (val (substring-no-properties val))
                                       (len (length val)))
                                 (ptv (concat
                                       "\"" (string-replace
                                             "\n" "⮐"
                                             (truncate-string-to-width
                                              val 20 nil nil t))
                                       "\"" (when (> len 20)
                                              (concat
                                               ", "
                                               (file-size-human-readable len 'si " ")
                                               " chars"))))
                               (propertize "empty" 'face 'error))
                             (ptv ")"))
                     context
                     (if dest (concat (pth ", response to ") dest)
                       (concat (pth ", insert response at point")))))
            ((member "i" args)
             (let* ((reg (use-region-p))
                    (src (ptv (if reg "selection" (buffer-name)))))
               (if dest (concat (pth "Send ") src ltext context (pth ", with response to ")
                                (ptv dest) (pth "; kill") ltext
                                (and (not reg) (concat (pth " in ") src)))
                 (concat (pth "Replace ") src ltext (pth " with response")
                         (and context
                              (concat (pth " ( with") (substring context 11) " )"))))))
            ((use-region-p)
             (concat (pth "Send ") (ptv "selection") ltext
                     context (if dest (concat (pth ", with response to ") dest)
                               (concat (pth ", insert response at region end")))))
            (t (concat (pth "Send ") (ptv (buffer-name)) ltext
                       context (if dest (concat (pth ", with response to ") dest)
                                 (concat (pth ", insert response at point")))))))))

(defun gptel--format-preset-string ()
  "Format the preset indicator display for `gptel-menu'."
  (if (and gptel--known-presets gptel--preset)
      (apply
       #'format " (%s%s)"
       (let ((mismatch (gptel--preset-mismatch-p gptel--preset)))
         (list (propertize "@" 'face (if mismatch 'transient-key
                                       '( :inherit transient-key
                                          :inherit secondary-selection
                                          :box -1 :weight bold)))
               (propertize (format "%s" gptel--preset) 'face
                           (if mismatch
                               '(:inherit warning :strike-through t)
                             '(:inherit secondary-selection :box -1))))))
    (format " (%s%s)"
            (propertize "@" 'face 'transient-key)
            (propertize "preset" 'face 'transient-inactive-value))))


;; * Transient classes and methods for gptel

;; ** Class for generic gptel elisp variables

(defclass gptel-lisp-variable (transient-lisp-variable)
  ((display-nil :initarg :display-nil)  ;String to display if value if nil
   (display-map :initarg :display-map :initform nil)) ;Display string from alist display-map
  "Lisp variables that show :display-nil instead of nil.")

(cl-defmethod transient-format-value ((obj gptel-lisp-variable))
  (let ((display-value
         (with-slots (value display-nil display-map) obj
           (cond ((null value) display-nil)
                 (display-map (or (cdr (assoc value display-map)) value))
                 (t value)))))
    (propertize
     (if (stringp display-value) display-value (prin1-to-string display-value))
     'face 'transient-value)))

(cl-defmethod transient-infix-set ((obj gptel-lisp-variable) value)
  (funcall (oref obj set-value)
           (oref obj variable)
           (oset obj value value)
           gptel--set-buffer-locally))

;; ** Class for managing gptel tools

(defclass gptel--switch (transient-switch)
  ((category :initarg :category))
  "Class used for arguments that share a category.")

(cl-defmethod transient-infix-set ((obj gptel--switch) value)
  "Set VALUE of a `gptel--switch' OBJ.

It is a list of the category and argument, e.g.
 (\"filesystem\" \"read_file\")."
  (let ((state (transient-scope))
        (category (oref obj category)))
    (if value
        (progn
          (cl-pushnew (list category value)
                      (plist-get state :tools) :test #'equal)
          (oset obj value (list category value)))
      (plist-put state :tools
                 (delete (list category (oref obj argument))
                         (plist-get state :tools)))
      (oset obj value nil))
    (oset transient--prefix scope state)))

;; ** Class for managing gptel tool categories

(defclass gptel--switch-category (transient-switch)
  ((category :initarg :category))
  "Class used for arguments that switch a group of other arguments.

Their own value is ignored")

(cl-defmethod transient-format-value ((obj gptel--switch-category))
  (let* ((category (oref obj category))
         (active-count
          (cl-count-if (lambda (tl) (equal (car tl) category))
                       (plist-get (transient-scope) :tools)))
         (total-count (length (cdr (assoc category gptel--known-tools)))))
    (if (> active-count 0)
        (propertize (format "(%d/%d)" active-count total-count) 'face 'transient-value)
      (propertize (format "(0/%d)" total-count) 'face 'transient-inactive-value))))

;; Pressing a tool category key should have different behaviors in different
;; contexts:
;; - If the tools for the category are not shown, show them, do nothing else
;; - If the tools are showing and any of them are selected, deselect all
;; - If the tools are showing and none of them are selected, select all

;; To do this we independently track whether the category tools are visible
;; ("active"), and whether any category tools have been "selected":
(cl-defmethod transient-infix-read ((obj gptel--switch-category))
  "Determine OBJ value according to category toggle settings."
  (let* ((category (oref obj category))
         (active (equal category (plist-get (transient-scope) :category)))
         (selected (cl-some (lambda (tool-spec) (equal category (car tool-spec)))
                            (plist-get (transient-scope) :tools))))
    (if (not active)
        (oref obj value)
      (if selected nil (oref obj argument)))))

(cl-defmethod transient-infix-set ((obj gptel--switch-category) value)
  "When setting VALUE, set all options in the category of OBJ."
  (dolist (suffix-obj transient--suffixes)
    ;; Find all suffixes that have this category
    (when-let* (((cl-typep suffix-obj 'gptel--switch))
                ((equal (oref suffix-obj category)
                        (oref obj category)))
                (arg (if (slot-boundp suffix-obj 'argument)
                         (oref suffix-obj argument)
                       (oref obj argument-format))))
      (if value                         ; Turn on/off all members in category
          (transient-infix-set suffix-obj arg)
        (transient-infix-set suffix-obj nil))))
  ;; Update the active menu category and key in the prefix scope
  (plist-put (transient-scope) :category (oref obj category))
  (plist-put (transient-scope) :key (oref obj key))
  ;; Finally set the "value" of the category itself
  (oset obj value value))

;; ** Class for gptel options that are three-way switches

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

;; ** Class for gptel's scope management, singleton

(defclass gptel--scope (gptel--switches)
  ((display-if-true :initarg :display-if-true :initform "buffer")
   (display-if-false :initarg :display-if-false :initform "global"))
  "Singleton lisp variable class for `gptel--set-buffer-locally'.

This is used only for setting this variable via `gptel-menu'.")

(cl-defmethod transient-infix-read ((obj gptel--scope))
  "Cycle through the mutually exclusive switches."
  (with-slots (value) obj
    (pcase value
      ('t (message "Parameters will be set for the next request only"))
      ('nil (message "Parameters will be set buffer-locally"))
      (1 (message "Parameters will be set globally")))
    (pcase value ('t 1) ('nil t) (1 nil))))

(cl-defmethod transient-format-value ((obj gptel--scope))
  (with-slots (value display-if-true display-if-false) obj
      (format
       (propertize "(%s)" 'face 'transient-delimiter)
       (concat
        (propertize display-if-false
                    'face (if (null value) 'transient-value 'transient-inactive-value))
        (propertize "|" 'face 'transient-delimiter)
        (propertize display-if-true
                    'face (if (eq value t) 'transient-value 'transient-inactive-value))
        (propertize "|" 'face 'transient-delimiter)
        (propertize "oneshot" 'face
                    (if (eql value 1) 'transient-value 'transient-inactive-value))))))

(cl-defmethod transient-infix-set ((obj gptel--scope) value)
  (funcall (oref obj set-value)
           (oref obj variable)
           (oset obj value value)))

;; ** Class for managing gptel's backend and model, singleton

(defclass gptel-provider-variable (transient-lisp-variable)
  ((backend       :initarg :backend)
   (backend-value :initarg :backend-value)
   (always-read :initform t)
   (set-value :initarg :set-value :initform #'set))
  "Class used for gptel-backends.")

(cl-defmethod transient-format-value ((obj gptel-provider-variable))
  (propertize (concat
               (gptel-backend-name
                (buffer-local-value (oref obj backend) transient--original-buffer)) ":"
               (gptel--model-name (oref obj value)))
              'face 'transient-value))

(cl-defmethod transient-infix-set ((obj gptel-provider-variable) value)
  (pcase-let ((`(,backend-value ,model-value) value))
    (funcall (oref obj set-value)
             (oref obj variable)
             (oset obj value model-value)
             gptel--set-buffer-locally)
    (funcall (oref obj set-value)
             (oref obj backend)
             (oset obj backend-value backend-value)
             gptel--set-buffer-locally))
  (transient-setup))

;; ** Class for infix options with in-buffer overlay display

(defclass gptel-option-overlaid (transient-option)
  ((display-nil :initarg :display-nil)
   (overlay :initarg :overlay))
  "Transient options for overlays displayed in the working buffer.")

(cl-defmethod transient-format-value ((obj gptel-option-overlaid))
  "Set up the in-buffer overlay for additional directive, a string.

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
        (propertize (concat argument (truncate-string-to-width value 35 nil nil t))
                    'face 'transient-value)
      (propertize
       (concat "(" (symbol-name (oref obj display-nil)) ")")
       'face 'transient-inactive-value))))


;; * Transient Prefixes

;;;###autoload (autoload 'gptel-menu "gptel-transient" nil t)
(transient-define-prefix gptel-menu ()
  "Change parameters of prompt to send to the LLM."
  :incompatible '(("m" "y" "i") ("e" "g" "b" "k"))
  ;; :value (list (concat "b" (buffer-name)))
  [:description gptel-system-prompt--format
   [""
    :if (lambda () (not (gptel--model-capable-p 'nosystem)))
    "Instructions"
    ("s" "Set system message" gptel-system-prompt :transient t)
    (gptel--infix-add-directive)]
   [:pad-keys t ""
    (:info #'gptel--describe-infix-context
     :face transient-heading :format "%d")
    (gptel--infix-context-add-current-kill)
    (gptel--infix-context-add-region)
    (gptel--infix-context-add-buffer)
    (gptel--infix-context-add-file)
    (gptel--infix-context-remove-all)
    (gptel--suffix-context-buffer)]
   [:pad-keys t
    :if (lambda () (and gptel-use-tools
                   (or gptel--known-tools (featurep 'gptel-integrations))))
    "" (:info
        (lambda ()
          (concat
           "Tools" (and gptel-tools
                        (concat " (" (propertize (format "%d selected"
                                                         (length gptel-tools))
                                                 'face 'warning)
                                ")"))))
        :format "%d" :face transient-heading)
    ("t" "Select tools" gptel-tools :transient t)
    ("T" "Continue tool calls"
     (lambda () (interactive) (gptel--handle-tool-use gptel--fsm-last))
     :if (lambda () (and gptel--fsm-last
                    (eq (gptel-fsm-state gptel--fsm-last) 'TOOL))))]]
  [[(gptel--preset
     :key "@" :format "%d"
     :description
     (lambda ()
       (concat (propertize "Request Parameters" 'face 'transient-heading)
               (gptel--format-preset-string))))
    (gptel--infix-variable-scope)
    (gptel--infix-provider)
    (gptel--infix-max-tokens)
    (gptel--infix-num-messages-to-send
     :if (lambda () (and gptel-expert-commands
                    (or gptel-mode gptel-track-response))))
    (gptel--infix-temperature :if (lambda () gptel-expert-commands))
    (gptel--infix-use-context)
    (gptel--infix-include-reasoning)
    (gptel--infix-use-tools)
    (gptel--infix-track-response
     :if (lambda () (and gptel-expert-commands (not gptel-mode))))
    (gptel--infix-track-media :if (lambda () gptel-mode))]
   [" <Prompt from"
    ("m" "Minibuffer instead" "m")
    ("y" "Kill-ring instead" "y")
    ""
    ("i" "Respond in place" "i")]
   [" >Response to"
    ("e" "Echo area" "e")
    ("b" "Other buffer" "b"
     :class transient-option
     :prompt "Output to buffer: "
     :reader (lambda (prompt _ _history)
               (read-buffer prompt (buffer-name (other-buffer)) nil)))
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
    ("k" "Kill-ring" "k")]]
  [[:description (lambda () (concat (and gptel--rewrite-overlays "Continue ")
                               "Rewrite"))
    :if (lambda () (or (use-region-p)
                  (and gptel--rewrite-overlays
                       (gptel--rewrite-sanitize-overlays))))
    ("r"
     (lambda () (if (get-char-property (point) 'gptel-rewrite)
               "Iterate" "Rewrite"))
     gptel-rewrite)]
   ["Tweak Response" :if gptel--in-response-p :pad-keys t
    ("SPC" "Mark" gptel--mark-response)
    ("M-RET" "Regenerate" gptel--regenerate :if gptel--in-response-p)
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
        'json)))]
   ["Logging"
    :if (lambda () (or gptel-log-level gptel-expert-commands))
    ("-l" "Log level" "-l"
     :class gptel-lisp-variable
     :variable gptel-log-level
     :set-value gptel--set-with-scope
     :display-nil "Off"
     :prompt "Log level: "
     :reader
     (lambda (prompt _ _)
       "Manage gptel's logging."
       (let ((state (completing-read
                     prompt '("off" "info" "debug") nil t)))
         (message "Log level set to %s" state)
         (if (string= state "off") nil (intern state)))))
    ("L" "Inspect Log"
     (lambda () (interactive)
       (pop-to-buffer (get-buffer-create gptel--log-buffer-name)))
     :format "  %k %d")]]
  [(gptel--suffix-send)]
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
           with unused-keys = (delete ?s (nconc (number-sequence ?a ?z)
                                                (number-sequence ?0 ?9)))
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

;; ** Prefix for saving and applying presets

(transient-define-prefix gptel--preset ()
  "Apply a gptel preset, or save the current configuration as a preset.

A \"preset\" is a collection of gptel settings, such as the model,
backend, system message and enabled tools, that are applied and used
together.  See `gptel-make-preset' for details."
  :transient-suffix #'transient--do-return
  [:description "Save or apply a preset collection of gptel options"
   [:pad-keys t
    ("C-s" "Save current settings as new preset" gptel--save-preset)]]
  [:if (lambda () gptel--known-presets)
   :class transient-column
   :setup-children
   (lambda (_)
     (transient-parse-suffixes
      'gptel--preset
      (cl-loop
       for (name-sym . preset) in gptel--known-presets
       for name = (format "%s" name-sym)
       with unused-keys = (nconc (number-sequence ?a ?z)
                                 (number-sequence ?0 ?9))
       for description = (plist-get preset :description)
       for key = (seq-find (lambda (k) (member k unused-keys))
                           name (seq-first unused-keys))
       do (setq unused-keys (delq key unused-keys))
       collect
       (list
        (key-description (list key))
        (concat name
                (propertize " " 'display '(space :align-to 20))
                (and description
                     (propertize (concat
                                  "(" (gptel--describe-directive
                                       description (- (window-width) 30))
                                  ")")
                                 'face 'shadow)))
        `(lambda () (interactive)
           (gptel--set-with-scope 'gptel--preset ',name-sym
            gptel--set-buffer-locally)
           (gptel--apply-preset ',preset
            (lambda (sym val) (gptel--set-with-scope
                          sym val gptel--set-buffer-locally)))
           (message "Applied gptel preset %s"
            (propertize ,name 'face 'transient-value))
           (when transient--stack
            (run-at-time 0 nil #'transient-setup))))
       into generated
       finally return
       (nconc (list '(gptel--infix-variable-scope
                      :format "%d %k %v"
                      :description
                      (lambda () (format "%s        %s"
                             (propertize "Apply preset" 'face 'transient-heading)
                             (propertize "Scope" 'face 'transient-active-prefix)))))
              generated))))])

;; ** Prefix for selecting tools

;; gptel-tools offers a two-level menu for selecting tools, its design is a
;; little convoluted so here's an explanation:
;;
;; Normally a transient prefix exports its value via transient-args, to be
;; consumed by suffixes, where these args are determined by the state of the
;; menu at the time of export.  The gptel-tools menu is dynamic and needs to
;; store tool selections that may not be visible in the meny any more, so we
;; cannot use the transient-args.
;;
;; We can not (should not?) control the value of the prefix directly, so we
;; instead use the scope (a secondary value) of the prefix to maintain the
;; history of selections.  When running a suffix, we gather tool selections from
;; the scope.  The scope is also used as a message channel for connecting the
;; category menu and the tool list menu for that category.

;;;###autoload (autoload 'gptel-tools "gptel-transient" nil t)
(transient-define-prefix gptel-tools ()
  "Select tools to include with gptel requests.

Tools are organized into categories.  Selecting the category
toggles all the tools with that category.

To add tools to this list, use `gptel-make-tool', which see.

Using the scope option, you can set tools to use with gptel
requests globally, in this buffer or for the next request
only (\"oneshot\")."
  :refresh-suffixes t
  [:description "Provide the LLM with tools to run tasks for you"
   [""
    (gptel--infix-variable-scope)
    (gptel--infix-use-tools)
    (gptel--infix-confirm-tool-calls)
    (gptel--infix-include-tool-results)]
   [""
    ("RET" "Confirm selection"
     (lambda (tools)
       ;; We don't care about the transient args of this prefix at all, since
       ;; the state is managed entirely through its transient-scope:
       (interactive (list (plist-get (transient-scope 'gptel-tools) :tools)))
       (gptel--set-with-scope
        'gptel-tools
        (mapcar (lambda (category-and-name)
                  (map-nested-elt gptel--known-tools category-and-name))
                (cl-delete-if-not #'consp tools))
        gptel--set-buffer-locally))
     :transient transient--do-return)
    ("q" "Cancel" transient-quit-one)]]
  [[:class transient-column             ;Display known categories
    :setup-children
    (lambda (_)
      (transient-parse-suffixes
       'gptel-tools
       (cl-loop          ;loop through gptel--known tools and collect categories
        for (category . tools-alist) in gptel--known-tools
        with unused-keys = (nconc (delete ?q (number-sequence ?a ?z))
                                  (number-sequence ?0 ?9)
                                  (delete ?M (number-sequence ?A ?Z))) ;M used by MCP integration
        for category-key = (seq-find (lambda (k) (member k unused-keys))
                                     (string-remove-prefix "mcp-" category)
                                     (seq-first unused-keys))
        do (setq unused-keys (delete category-key unused-keys))
        collect (list (key-description (list category-key))
                      (concat (propertize category 'face 'transient-heading)
                              (make-string (max (- 14 (length category)) 0) ? ))
                      (char-to-string category-key)
                      :format " %k %d %v"
                      :class 'gptel--switch-category
                      :category category)
        into categories
        finally do (plist-put (transient-scope) :keys unused-keys)
        finally return categories)))]
   [:class transient-column           ;Display known tools for selected category
    :setup-children
    (lambda (_)
      (transient-parse-suffixes
       'gptel-tools
       (when-let* ((category (plist-get (transient-scope) :category))
                   (tool-keys (plist-get (transient-scope) :keys)))
         (cl-loop                   ;for each category, collect tools as infixes
          with tools-alist = (cdr (assoc category gptel--known-tools))
          for (name . tool) in tools-alist
          for tool-key = (seq-find (lambda (k) (member k tool-keys)) name (seq-first tool-keys))
          do (setq tool-keys (delete tool-key tool-keys))
          collect          ;Each list is a transient infix of type gptel--switch
          (list (key-description (list tool-key))
                (concat (make-string (max (- 20 (length name)) 0) ? )
                        (propertize
                         (concat "(" (gptel--describe-directive
                                      (gptel-tool-description tool) (- (window-width) 60))
                                 ")")
                         'face 'shadow))
                (gptel-tool-name tool)
                :format " %k %v %d"
                :init-value #'gptel--tools-init-value
                :class 'gptel--switch
                :category category)
          into infixes-for-category
          finally return
          (cons (list :info
                      (lambda () (concat
                             (propertize (plist-get (transient-scope) :key)
                                         'face 'transient-key)
                             (propertize " toggle all" 'face 'transient-heading)))
                      :format " %d")
                infixes-for-category)))))]]
  (interactive)
  (transient-setup
   'gptel-tools nil nil
   :scope (list :tools (mapcar (lambda (tool) (list (gptel-tool-category tool)
                                               (gptel-tool-name tool)))
                               gptel-tools))))


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
  :description (propertize "Scope" 'face 'transient-inactive-argument))

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
  :reader 'gptel--transient-read-number)

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
  :reader 'gptel--transient-read-number)

(transient-define-infix gptel--infix-provider ()
  "AI Provider for Chat."
  :description "Model"
  :class 'gptel-provider-variable
  :prompt "Model: "
  :variable 'gptel-model
  :set-value #'gptel--set-with-scope
  :backend 'gptel-backend
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
             (cdr (assoc (completing-read prompt models-alist nil t nil nil
					  (concat (gptel-backend-name gptel-backend) ":"
						  (gptel--model-name gptel-model)))
                         models-alist)))))

(transient-define-infix gptel--infix-temperature ()
  "Temperature of request."
  :description "Temperature (0 - 2.0)"
  :display-nil "default"
  :class 'gptel-lisp-variable
  :variable 'gptel-temperature
  :set-value #'gptel--set-with-scope
  :key "-T"
  :prompt "Temperature controls the response randomness (0.0-2.0, leave empty for API default): "
  :reader 'gptel--transient-read-number)

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
  :key "-R")

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
(declare-function gptel-context-add-current-kill "gptel-context")

(transient-define-suffix gptel--infix-context-add-current-kill (&optional arg)
  "Add current kill to gptel's context."
  :transient 'transient--do-stay
  :key "C-y"
  :if (lambda () gptel-expert-commands)
  :description
  "Yank to context"
  (interactive "P")
  (gptel-context-add-current-kill arg)
  (transient-setup))

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
  (gptel-context-remove-all t)
  (transient-setup))

;; ** Infix for additional directive

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
  :prompt (concat "Add instructions for next request only ("
                  gptel--read-with-prefix-help ") ")
  ;; TODO: Add the ability to edit this in a separate buffer, with
  ;; `gptel--edit-directive'.  This requires setting up gptel-menu with the
  ;; result as the :scope.
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

;; ** Infix for reasoning block control

(transient-define-infix gptel--infix-include-reasoning ()
  "How to handle reasoning/thinking response blocks.

Some LLMs include in their response a \"thinking\" section.  This
text improves the quality of the LLM's final output, but may not
be interesting to you by itself.

You can control how gptel should handle the thinking blocks via
this option, or by setting the variable `gptel-include-reasoning'
via elisp, which see.

Available behaviors are
- to include thinking blocks with the response,
- to omit them entirely,
- to include them but ignore them in consequent conversation turns, and
- to append them to a buffer of your choosing."
  :description "Include reasoning"
  :class 'gptel-lisp-variable
  :variable 'gptel-include-reasoning
  :format " %k %d %v"
  :set-value #'gptel--set-with-scope
  :display-nil "No"
  :display-map '((nil    . "No")
                 (ignore . "and ignore")
                 (t      . "with response"))
  :key "-v"
  :prompt "Include reasoning: "
  :reader (lambda (prompt &rest _)
            (let* ((choices '(("no"     . nil)
                              ("ignore" . ignore)
                              ("yes"    . t)
                              ("other buffer" . buffer)))
                   (destination
                    (completing-read prompt choices nil t)))
              (if (equal destination "other buffer")
                  (read-buffer "Append reasoning to buffer: ")
                (cdr (assoc destination choices))))))

;; ** Infixes for tool use

(transient-define-infix gptel--infix-use-tools ()
  "Whether LLM tool use with gptel is enabled.

This is a three-way toggle.  Assuming one or more tools to be
sent with requests have been selected, tool use can be

- disabled,
- enabled, where the LLM may choose to respond with tool calls
- forced, where the LLM must respond with one or more tool calls.

You can set this here or by customizing `gptel-use-tools', which
see."
  :description "Use tools"
  :class 'gptel-lisp-variable
  :variable 'gptel-use-tools
  :set-value (lambda (sym value scope)
               (gptel--set-with-scope sym value scope)
               (transient-setup))
  :display-nil "off"
  :display-map '((nil   . "off")
                 (t     . "on")
                 (force . "force"))
  :prompt "Use tools? "
  :reader (lambda (prompt &rest _)
            (let* ((choices '(("disable" . nil)
                              ("enable"  . t)
                              ("force"   . force)))
                   (pref (completing-read prompt choices nil t)))
              (cdr (assoc pref choices))))
  :key "-t")

(transient-define-infix gptel--infix-confirm-tool-calls ()
  "Whether tool calls should wait for the user to run them.

This is a three-way toggle between these behaviors:

- All tool calls run without confirmation.
- All tool calls wait for confirmation.
- Decided per-tool, according to the value of the tool spec's
  :confirm slot.

This sets the variable `gptel-confirm-tool-calls', which see."
  :key "-c"
  :description "Confirm tool calls"
  :class 'gptel-lisp-variable
  :variable 'gptel-confirm-tool-calls
  :set-value #'gptel--set-with-scope
  :display-nil "never"
  :display-map '((nil . "never")
                 (t   . "always")
                 (auto . "auto"))
  :prompt "Tool calls require confirmation? "
  :reader (lambda (prompt &rest _)
            (let* ((choices '(("no"   . nil)
                              ("always" . t)
                              ("tool decides" . auto)))
                   (pref (completing-read prompt choices nil t)))
              (cdr (assoc pref choices)))))

(transient-define-infix gptel--infix-include-tool-results ()
  "Whether tool call results should be included in the response.

This is a three-way toggle between these behaviors:

- All tool results are included.
- No tool results are included.
- Decided per-tool, according to the value of the tool spec's
  :include slot.

This sets the variable `gptel-include-tool-results', which see."
  :key "-i"
  :description "Include results   "
  :class 'gptel-lisp-variable
  :variable 'gptel-include-tool-results
  :set-value #'gptel--set-with-scope
  :display-nil "never"
  :display-map '((nil . "never")
                 (t   . "always")
                 (auto . "auto"))
  :prompt "Include tool results in LLM response? "
  :reader (lambda (prompt &rest _)
            (let* ((choices '(("never"   . nil)
                              ("always" . t)
                              ("tool decides" . auto)))
                   (pref (completing-read prompt choices nil t)))
              (cdr (assoc pref choices)))))


;; * Transient Suffixes

;; ** Suffix to send prompt

(transient-define-suffix gptel--suffix-send (args)
  "Send ARGS."
  :key "RET"
  :description #'gptel--describe-suffix-send
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
          ((member "m" args) (gptel--read-minibuffer-prompt))
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
            (lambda (resp info &optional _raw)
              (pcase resp
                ((pred stringp) (message "%s response: %s" backend-name resp))
                (`(tool-call . ,tool-calls) (gptel--display-tool-calls tool-calls info 'minibuffer))
                (`(tool-result . ,tool-results) (gptel--display-tool-results tool-results info))
                (_ (when (and (null resp) (plist-get info :error))
                     (message "%s response error: %s"
                              backend-name (plist-get info :status))))))))
     ((member "k" args)
      (setq stream nil)
      (setq callback
            (lambda (resp info &optional _raw)
              (pcase resp
                ((pred stringp) (kill-new resp)
                 (message "%s response: \"%s\" copied to kill-ring." backend-name
                          (truncate-string-to-width resp 30)))
                (`(tool-call . ,tool-calls) (gptel--display-tool-calls tool-calls info 'minibuffer))
                (`(tool-result . ,tool-results) (gptel--display-tool-results tool-results info))
                (_ (when (and (null resp) (plist-get info :error))
                     (message "%s response error: %s" backend-name
                              (plist-get info :status))))))))
     ((setq gptel-buffer-name
            (cl-some (lambda (s) (and (stringp s) (string-prefix-p "g" s)
                                 (substring s 1)))
                     args))
      (setq output-to-other-buffer-p t)
      (let* ((reduced-prompt            ;For inserting into the gptel buffer as
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
                     (gptel--at-word-end (point))))))
             (gptel-buffer (get-buffer gptel-buffer-name))
             (gptel-buffer-mode
              (if (buffer-live-p gptel-buffer)
                  (buffer-local-value 'major-mode gptel-buffer)
                gptel-default-mode)))
        ;; Add code fences or Org src markers around the reduced-prompt
        (cond ((eq major-mode gptel-buffer-mode))
              ((provided-mode-derived-p gptel-buffer-mode 'org-mode)
               (setq reduced-prompt
                     (if (consp reduced-prompt);either (region . prompt) or prompt
                         (concat (and (car reduced-prompt)
                                      (concat "#+begin_src " (gptel--strip-mode-suffix major-mode)
                                              "\n" (car reduced-prompt) "\n#+end_src\n\n"))
                                 (cdr reduced-prompt))
                       (concat "#+begin_src " (gptel--strip-mode-suffix major-mode)
                               "\n" (or (cdr-safe reduced-prompt) reduced-prompt) "\n#+end_src"))))
              (t (setq reduced-prompt
                       (if (consp reduced-prompt);either (region . prompt) or prompt
                           (concat (and (car reduced-prompt)
                                        (concat  "``` " (gptel--strip-mode-suffix major-mode) "\n"
                                                 (car reduced-prompt) "\n```\n\n"))
                                   (cdr reduced-prompt))
                         (concat "``` " (gptel--strip-mode-suffix major-mode) "\n"
                                 (or (cdr-safe reduced-prompt) reduced-prompt) "\n```" )))))
        (cond
         ((buffer-live-p gptel-buffer)
          ;; Insert into existing gptel session
          (setq buffer gptel-buffer)
          (with-current-buffer buffer
            (goto-char (point-max))
            (unless (or buffer-read-only
                        (get-char-property (point) 'read-only))
              (unless (bolp) (insert "\n"))
              (insert reduced-prompt))
            (setq position (point))
            (when (and gptel-mode (not dry-run))
              (gptel--update-status " Waiting..." 'warning))))
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

    ;; MAYBE: This is no a good way to handle two-part (region + instruction) prompts
    ;; If the prompt is a cons (region-text . instructions), collapse it
    (when (consp prompt) (setq prompt (concat (car prompt) "\n\n" (cdr prompt))))

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
             :transforms gptel-prompt-transform-functions
             :fsm (gptel-make-fsm :handlers gptel-send--handlers)
             :dry-run dry-run)

      (unless dry-run
        (gptel--update-status " Waiting..." 'warning))

      ;; NOTE: Possible future race condition here if Emacs ever drops the GIL.
      ;; The HTTP request callback might modify the buffer before the in-place
      ;; text is killed below.
      (when in-place
        (if (or buffer-read-only (get-char-property (point) 'read-only))
            (message "Not replacing prompt: region is read-only")
          (let ((beg (if (use-region-p)
                         (region-beginning)
                       (max (previous-single-property-change
                             (point) 'gptel nil (point-min))
                            (previous-single-property-change
                             (point) 'read-only nil (point-min)))))
                (end (if (use-region-p) (region-end) (point))))
            (unless output-to-other-buffer-p
              ;; store the killed text in gptel-history
              (gptel--attach-response-history
               (list (buffer-substring-no-properties beg end))))
            (kill-region beg end))))

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
    (pcase-let* ((`(,beg . ,end) (gptel--get-response-bounds))
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
        (when-let* ((prompt (gethash choice gptel--crowdsourced-prompts)))
          (gptel--set-with-scope
           'gptel--system-message prompt gptel--set-buffer-locally)
          (gptel--edit-directive 'gptel--system-message
            :callback (lambda () (call-interactively #'gptel-menu)))))
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
    (gptel--edit-directive 'gptel--system-message
      :setup #'activate-mark
      :callback (lambda (_) (call-interactively #'gptel-menu)))))

;; MAYBE: Eventually can be simplified with string-edit, after we drop support
;; for Emacs 28.2.
(cl-defun gptel--edit-directive (&optional sym &key prompt initial callback setup buffer)
  "Edit a gptel directive in a dedicated buffer.

Store the result in SYM, a symbol.  PROMPT and INITIAL are the heading
and initial text.  If SETUP is a function, run it after setting up the
buffer.  If CALLBACK is specified, it is run after exiting the edit.  It
is called with one argument: the buffer text or with nil depending on
whether the action is confirmed/cancelled."
  (declare (indent 1))
  (let ((orig-buf (or buffer (current-buffer)))
        (msg-start (make-marker))
        (directive (symbol-value sym)))
    (when (functionp directive)
      (setq directive (funcall directive)))
    ;; TODO: Handle editing list-of-strings directives
    (with-current-buffer (get-buffer-create "*gptel-prompt*")
      (let ((inhibit-read-only t) (inhibit-message t))
        (erase-buffer)
        (text-mode)
        (visual-line-mode 1)
        (setq header-line-format
              (concat "Edit your instructions below and press "
                      (propertize "C-c C-c" 'face 'help-key-binding)
                      " when ready, or "
                      (propertize "C-c C-k" 'face 'help-key-binding)
                      " to abort."))
        (insert
         (or prompt
             (concat
              "# Example: You are a helpful assistant. Answer as concisely as possible.\n"
              "# Example: Reply only with shell commands and no prose.\n"
              "# Example: You are a poet. Reply only in verse."))
         "\n\n")
        (add-text-properties
         (point-min) (point)
         (list 'read-only t 'face 'font-lock-comment-face 'front-sticky t 'rear-nonsticky t))
        (set-marker msg-start (point))
        (save-excursion
          ;; If it's a list, insert only the system message part
          ;; If all is nil, insert "" at least
          (insert (or initial (car-safe (gptel--parse-directive directive 'raw)) ""))
          (push-mark nil 'nomsg))
        (and (functionp setup) (funcall setup)))
      (display-buffer (current-buffer)
                      `((display-buffer-below-selected
                         display-buffer-use-some-window)
                        (some-window   . lru)
                        (body-function . ,#'select-window)
                        (window-height . ,#'fit-window-to-buffer)))
      (let ((quit-to-menu
             (lambda () "Cancel system message update and return."
               (quit-window)
               (unless (minibufferp)
                 (display-buffer orig-buf
                                 `((display-buffer-reuse-window
                                    display-buffer-use-some-window)
                                   (body-function . ,#'select-window)))))))
        (use-local-map
         (make-composed-keymap
          (define-keymap
            "C-c C-c"
            (lambda () "Confirm system message and return."
              (interactive)
              (let ((system-message
                     (buffer-substring-no-properties msg-start (point-max))))
                (when sym
                  (with-current-buffer orig-buf
                    (gptel--set-with-scope
                     sym (if (cdr-safe directive) ;Handle list of strings
                             (prog1 directive (setcar directive system-message))
                           system-message)
                     gptel--set-buffer-locally)))
                (funcall quit-to-menu)
                (when (functionp callback) (funcall callback system-message))))
            "C-c C-k" (lambda () (interactive)
                        (funcall quit-to-menu)
                        (when (functionp callback) (funcall callback nil))))
          text-mode-map))))))

;; ** Suffix for displaying and removing context
(declare-function gptel-context--buffer-setup "gptel-context")
(declare-function gptel-context--collect "gptel-context")

(transient-define-suffix gptel--suffix-context-buffer ()
  "Display all contexts from all buffers & files."
  :transient 'transient--do-exit
  :key " C"
  :if (lambda () gptel-context--alist)
  :description "Inspect context"
  (interactive)
  (gptel-context--buffer-setup))

(provide 'gptel-transient)
;;; gptel-transient.el ends here

;; Local Variables:
;; outline-regexp: "^;; \\*+"
;; eval: (outline-minor-mode 1)
;; End:
