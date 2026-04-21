;;; gptel-org-dashboard.el --- Org-agenda dashboard for gptel AI tasks -*- lexical-binding: t -*-

;; Copyright (C) 2025  Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides an org-agenda-based dashboard for monitoring and managing
;; gptel AI task states.  AI documents register themselves via
;; `gptel-org-dashboard-register' (typically called when `gptel-org-mode'
;; is enabled), and the dashboard shows tasks from all registered files.
;;
;; Open the dashboard with `gptel-org-dashboard' or via the org-agenda
;; dispatcher with the configured key (default "D").
;;
;; The dashboard displays four sections:
;;   🔥 In Progress  — tasks the AI is actively working on (AI-DOING)
;;   ⏳ Pending       — tool calls awaiting user approval (PENDING)
;;   💬 Feedback      — AI done, awaiting user review (FEEDBACK)
;;   📋 Queued        — tasks queued for AI processing (AI-DO)
;;
;; Dashboard keybindings:
;;   SPC  Show task in other window (follow mode)
;;   RET  Go to task buffer
;;   a    Allow pending tool call
;;   d    Deny pending tool call
;;   k    Kill/abort AI task
;;   g    Refresh dashboard
;;   q    Quit (bury dashboard)
;;
;; Registration model:
;;   Files register themselves with the dashboard when `gptel-org-mode'
;;   is enabled, and unregister when it is disabled or the buffer is
;;   killed.  Only registered (live) files appear in the dashboard.
;;
;; Auto-refresh:
;;   The dashboard refreshes automatically when TODO states change in
;;   registered files and when registered files are saved.  Refreshes
;;   are debounced with `run-with-idle-timer' to avoid thrashing during
;;   rapid updates.

;;; Code:

(require 'org)
(require 'org-agenda)
(require 'cl-lib)

;; External gptel functions (may not be loaded when this file is loaded)
(declare-function gptel-abort "gptel-request" (buf))

;;; Customization

(defgroup gptel-org-dashboard nil
  "Org-agenda dashboard for gptel AI tasks."
  :group 'gptel)

(defcustom gptel-org-dashboard-key "D"
  "Key for the gptel dashboard in `org-agenda-custom-commands'.
This key is used both in the org-agenda dispatcher and passed to
`org-agenda' when opening the dashboard programmatically."
  :type 'string
  :group 'gptel-org-dashboard)

(defcustom gptel-org-dashboard-debounce-seconds 0.5
  "Idle time in seconds before auto-refreshing the dashboard.
Lower values make the dashboard more responsive but may cause
flickering during rapid state changes."
  :type 'number
  :group 'gptel-org-dashboard)

(defcustom gptel-org-dashboard-buffer-name "*gptel-dashboard*"
  "Buffer name for the gptel AI task dashboard."
  :type 'string
  :group 'gptel-org-dashboard)

;;; Internal state

(defvar gptel-org-dashboard--registered-files nil
  "List of org file paths registered with the AI dashboard.
Each entry is an absolute file path.  Only files from live buffers
should be in this list; entries are cleaned up when buffers are
killed or explicitly unregistered.")

(defvar gptel-org-dashboard--refresh-timer nil
  "Timer for debounced dashboard refresh.
Non-nil while a refresh is pending.  Cancelled and replaced on
each new refresh request to implement debouncing.")

;;; Keyword accessors
;;
;; These helper functions read gptel customization variables at runtime
;; with fallbacks, so the dashboard works even if gptel-org-tasks or
;; gptel-org-agent are not yet loaded.

(defun gptel-org-dashboard--doing-keyword ()
  "Return the keyword for AI tasks in progress."
  (or (bound-and-true-p gptel-org-tasks-doing-keyword) "AI-DOING"))

(defun gptel-org-dashboard--todo-keyword ()
  "Return the keyword for queued AI tasks."
  (or (bound-and-true-p gptel-org-tasks-todo-keyword) "AI-DO"))

(defun gptel-org-dashboard--feedback-keyword ()
  "Return the keyword for tasks awaiting user feedback."
  (or (bound-and-true-p gptel-org-user-keyword) "FEEDBACK"))

(defun gptel-org-dashboard--pending-keyword ()
  "Return the keyword for tool calls awaiting approval."
  (or (nth 0 (bound-and-true-p gptel-org-agent-tool-confirm-keywords)) "PENDING"))

(defun gptel-org-dashboard--allowed-keyword ()
  "Return the keyword for approved tool calls."
  (or (nth 1 (bound-and-true-p gptel-org-agent-tool-confirm-keywords)) "ALLOWED"))

(defun gptel-org-dashboard--denied-keyword ()
  "Return the keyword for denied tool calls."
  (or (nth 2 (bound-and-true-p gptel-org-agent-tool-confirm-keywords)) "DENIED"))

;;; Registration

;;;###autoload
(defun gptel-org-dashboard-register ()
  "Register the current buffer's file with the AI dashboard.
The file will appear in the dashboard until it is unregistered
or the buffer is killed.  Hooks are added to the current buffer
for auto-refreshing the dashboard on state changes and saves."
  (interactive)
  (when-let* ((file (buffer-file-name (buffer-base-buffer (current-buffer)))))
    (let ((abs-file (expand-file-name file)))
      (cl-pushnew abs-file gptel-org-dashboard--registered-files
                  :test #'string=)
      ;; Buffer-local hooks for auto-refresh
      (add-hook 'org-after-todo-state-change-hook
                #'gptel-org-dashboard--on-state-change nil t)
      (add-hook 'after-save-hook
                #'gptel-org-dashboard--on-save nil t)
      (add-hook 'kill-buffer-hook
                #'gptel-org-dashboard--on-kill nil t)
      (message "Registered %s with gptel dashboard"
               (file-name-nondirectory abs-file)))))

;;;###autoload
(defun gptel-org-dashboard-unregister ()
  "Unregister the current buffer's file from the AI dashboard.
Removes the file from the dashboard and cleans up buffer-local hooks."
  (interactive)
  (when-let* ((file (buffer-file-name (buffer-base-buffer (current-buffer)))))
    (let ((abs-file (expand-file-name file)))
      (setq gptel-org-dashboard--registered-files
            (delete abs-file gptel-org-dashboard--registered-files))
      (remove-hook 'org-after-todo-state-change-hook
                   #'gptel-org-dashboard--on-state-change t)
      (remove-hook 'after-save-hook
                   #'gptel-org-dashboard--on-save t)
      (remove-hook 'kill-buffer-hook
                   #'gptel-org-dashboard--on-kill t)
      (message "Unregistered %s from gptel dashboard"
               (file-name-nondirectory abs-file)))))

;;; Auto-refresh

(defun gptel-org-dashboard-refresh ()
  "Refresh the dashboard if it is visible in some window.
Does nothing if the dashboard buffer does not exist or is not
currently displayed.  Can be called interactively (bound to `g'
in the dashboard) or from the auto-refresh timer."
  (interactive)
  (when-let* ((buf (get-buffer gptel-org-dashboard-buffer-name)))
    (when (get-buffer-window buf 'visible)
      (with-current-buffer buf
        (let ((inhibit-message t))
          (org-agenda-redo-all))))))

(defun gptel-org-dashboard--schedule-refresh ()
  "Schedule a debounced dashboard refresh.
Cancels any previously scheduled refresh and sets a new idle
timer.  The refresh fires after `gptel-org-dashboard-debounce-seconds'
of idle time."
  (when gptel-org-dashboard--refresh-timer
    (cancel-timer gptel-org-dashboard--refresh-timer))
  (setq gptel-org-dashboard--refresh-timer
        (run-with-idle-timer
         gptel-org-dashboard-debounce-seconds nil
         (lambda ()
           (setq gptel-org-dashboard--refresh-timer nil)
           (gptel-org-dashboard-refresh)))))

(defun gptel-org-dashboard--on-state-change ()
  "Schedule dashboard refresh when a TODO state changes.
Only triggers if the current buffer's file is registered with
the dashboard.  Added buffer-locally to `org-after-todo-state-change-hook'."
  (when (and (derived-mode-p 'org-mode)
             (member (buffer-file-name (buffer-base-buffer))
                     gptel-org-dashboard--registered-files))
    (gptel-org-dashboard--schedule-refresh)))

(defun gptel-org-dashboard--on-save ()
  "Schedule dashboard refresh when a registered file is saved.
Added buffer-locally to `after-save-hook'."
  (when-let* ((file (buffer-file-name)))
    (when (member (expand-file-name file)
                  gptel-org-dashboard--registered-files)
      (gptel-org-dashboard--schedule-refresh))))

(defun gptel-org-dashboard--on-kill ()
  "Unregister the file when its base buffer is killed.
Added buffer-locally to `kill-buffer-hook'.
Only unregisters when the base buffer itself is being killed, not
when indirect buffers (e.g., gptel agent task buffers) derived
from it are killed.  Indirect buffers inherit buffer-local hooks
from their base buffer, so this guard prevents spurious
unregistration during normal task execution."
  (unless (buffer-base-buffer)
    (gptel-org-dashboard-unregister)))

;;; Dashboard actions

(defun gptel-org-dashboard-allow ()
  "Allow the pending tool call at point.
Changes the TODO state to the ALLOWED keyword (from
`gptel-org-agent-tool-confirm-keywords'), which triggers tool
execution via `org-after-todo-state-change-hook' in the source
buffer."
  (interactive)
  (org-agenda-todo (gptel-org-dashboard--allowed-keyword)))

(defun gptel-org-dashboard-deny ()
  "Deny the pending tool call at point.
Changes the TODO state to the DENIED keyword (from
`gptel-org-agent-tool-confirm-keywords'), which informs the LLM
that the tool call was rejected."
  (interactive)
  (org-agenda-todo (gptel-org-dashboard--denied-keyword)))

(defun gptel-org-dashboard-kill ()
  "Abort the AI task at point.
Prompts for confirmation, then calls `gptel-abort' on the buffer
containing the task.  The abort advice in gptel-org-tasks handles
the state transition.  A dashboard refresh is scheduled after a
short delay."
  (interactive)
  (when (y-or-n-p "Abort this AI task? ")
    (let* ((marker (or (org-get-at-bol 'org-hd-marker)
                       (org-get-at-bol 'org-marker)))
           (buf (and marker (marker-buffer marker))))
      (when buf
        ;; gptel-abort expects the buffer where the request is active.
        ;; It handles indirect buffers internally by checking base buffers.
        (gptel-abort buf)
        ;; Schedule a refresh to pick up state changes from the abort
        (gptel-org-dashboard--schedule-refresh)))))

;;; Keymap setup

(defun gptel-org-dashboard--setup-keys ()
  "Set up dashboard-specific keybindings in the agenda buffer.
Creates a sparse keymap with `org-agenda-mode-map' as parent and
overlays the dashboard action keys.  Only activates in the
dashboard buffer (identified by `gptel-org-dashboard-buffer-name').
Added to `org-agenda-finalize-hook'."
  (when (string= (buffer-name) gptel-org-dashboard-buffer-name)
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map org-agenda-mode-map)
      (define-key map "a" #'gptel-org-dashboard-allow)
      (define-key map "d" #'gptel-org-dashboard-deny)
      (define-key map "k" #'gptel-org-dashboard-kill)
      (define-key map "g" #'gptel-org-dashboard-refresh)
      (use-local-map map))))

;;; Agenda command construction

(defun gptel-org-dashboard--make-agenda-command ()
  "Build the org-agenda custom command for the dashboard.
Reads gptel keyword customization variables at call time to
ensure the dashboard always reflects the current configuration.
The `org-agenda-files' setting references the registration
variable directly so it is evaluated when the agenda is built."
  (let ((doing-kw (gptel-org-dashboard--doing-keyword))
        (pending-kw (gptel-org-dashboard--pending-keyword))
        (feedback-kw (gptel-org-dashboard--feedback-keyword))
        (todo-kw (gptel-org-dashboard--todo-keyword)))
    `(,gptel-org-dashboard-key
      "gptel AI Dashboard"
      ;; Block agenda: four todo-match blocks
      ((todo ,doing-kw
             ((org-agenda-overriding-header
               ,(format "🔥 In Progress [%s]\n" doing-kw))
              (org-agenda-sorting-strategy '(priority-down category-keep))
              (org-agenda-prefix-format "  %-20:c ")))
       (todo ,pending-kw
             ((org-agenda-overriding-header
               ,(format "⏳ Pending Approval [%s]\n" pending-kw))
              (org-agenda-sorting-strategy '(timestamp-up))
              (org-agenda-prefix-format "  %-20:c ")))
       (todo ,feedback-kw
             ((org-agenda-overriding-header
               ,(format "💬 Needs Feedback [%s]\n" feedback-kw))
              (org-agenda-sorting-strategy '(priority-down))
              (org-agenda-prefix-format "  %-20:c ")))
       (todo ,todo-kw
             ((org-agenda-overriding-header
               ,(format "📋 Queued [%s]\n" todo-kw))
              (org-agenda-sorting-strategy '(priority-down))
              (org-agenda-prefix-format "  %-20:c "))))
      ;; Global settings for all blocks.
      ;; NOTE: org-agenda-sticky is intentionally NOT set here.
      ;; When present in command spec gprops, org-agenda-run-series
      ;; re-binds it via cl-progv during redo, which causes
      ;; org-agenda-prepare to throw 'exit (sticky buffer detected),
      ;; silently aborting the refresh.  Sticky is set only at the
      ;; entry point via a let-binding around org-agenda.
      ((org-agenda-files gptel-org-dashboard--registered-files)
       (org-agenda-compact-blocks nil)
       (org-agenda-block-separator ?─)
       (org-agenda-buffer-name ,gptel-org-dashboard-buffer-name)
       (org-agenda-window-setup 'current-window)))))

(defun gptel-org-dashboard--install-agenda-command ()
  "Install the dashboard command into `org-agenda-custom-commands'.
Replaces any existing entry with the same key.  The command is
rebuilt each time to pick up current keyword values and the
latest registered file list."
  ;; Remove any existing entry with our key
  (let ((existing (assoc gptel-org-dashboard-key org-agenda-custom-commands)))
    (when existing
      (setq org-agenda-custom-commands
            (delete existing org-agenda-custom-commands))))
  ;; Build and install fresh command
  (push (gptel-org-dashboard--make-agenda-command)
        org-agenda-custom-commands))

;;; Entry point

;;;###autoload
(defun gptel-org-dashboard ()
  "Open the gptel AI task dashboard.
Displays an org-agenda composite view showing all active AI
tasks from registered files, organized by state.

Files must be registered first via `gptel-org-dashboard-register'
\(this is typically done automatically when `gptel-org-mode' is
enabled).

The dashboard auto-refreshes when TODO states change or files
are saved.  Use \\`g' to force a manual refresh."
  (interactive)
  ;; Prune dead files (buffers may have been killed without the hook firing)
  (setq gptel-org-dashboard--registered-files
        (cl-remove-if-not
         (lambda (f) (find-buffer-visiting f))
         gptel-org-dashboard--registered-files))
  (unless gptel-org-dashboard--registered-files
    (user-error "No files registered.  Use `gptel-org-dashboard-register' in an AI document"))
  ;; Install our keymap hook (idempotent: add-hook deduplicates)
  (add-hook 'org-agenda-finalize-hook #'gptel-org-dashboard--setup-keys)
  ;; Rebuild the agenda command to pick up current state
  (gptel-org-dashboard--install-agenda-command)
  ;; Bind sticky only for initial display.  This is NOT stored in the
  ;; command spec's gprops, so org-agenda-run-series won't re-enable
  ;; it during redo — allowing org-agenda-redo-all to work correctly.
  (let ((org-agenda-sticky t))
    (org-agenda nil gptel-org-dashboard-key)))

(provide 'gptel-org-dashboard)
;;; gptel-org-dashboard.el ends here
