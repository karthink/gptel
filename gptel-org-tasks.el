;;; gptel-org-tasks.el --- AI task workflow with org-mode TODO states -*- lexical-binding: t -*-

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

;; This module provides AI task workflow support for org-mode, integrating
;; gptel with org TODO keywords and tags.
;;
;; Features:
;; - Model profiles: map simple tag names (haiku, opus, gpt4) to actual models
;; - Auto state transition: AI-DO -> AI-DOING when gptel-send is called
;; - Tag-based model selection: apply model config from task tags
;; - Built-in support for Anthropic model aliases (haiku, sonnet, opus)
;;
;; Setup:
;; 1. Add TODO keywords to your org file:
;;
;;    #+SEQ_TODO: AI-DO(a) AI-DOING(i!) HI-FEEDBACK(h@) | AI-DONE(d!) HI-DONE(D!)
;;
;; 2. Tag tasks with model names:
;;
;;    ** AI-DO Implement feature X :haiku:
;;    ** AI-DO Complex analysis    :opus:
;;
;; Built-in model aliases (no configuration needed):
;; - :haiku: -> Latest Claude Haiku
;; - :sonnet: -> Latest Claude Sonnet
;; - :opus: -> Latest Claude Opus
;;
;; You can also define custom profiles for other models:
;;
;;    (gptel-org-tasks-define-profile 'gpt4
;;      :backend "ChatGPT"
;;      :model 'gpt-4o)
;;
;; When you call `gptel-send' inside an AI-DO task:
;; - The task state changes to AI-DOING
;; - The model profile from the tag is applied (if present)

;;; Code:

(require 'org)
(require 'gptel)

;;; Customization

(defgroup gptel-org-tasks nil
  "AI task workflow for org-mode."
  :group 'gptel)

(defcustom gptel-org-tasks-todo-keyword "AI-DO"
  "TODO keyword that indicates a task ready for AI processing.
When `gptel-send' is called inside a heading with this keyword,
the state will transition to `gptel-org-tasks-doing-keyword'."
  :type 'string
  :group 'gptel-org-tasks)

(defcustom gptel-org-tasks-doing-keyword "AI-DOING"
  "TODO keyword that indicates a task is being processed by AI.
Tasks transition to this state when `gptel-send' is called."
  :type 'string
  :group 'gptel-org-tasks)

(defcustom gptel-org-tasks-apply-profile-on-send t
  "Whether to apply model profile from tags when sending.
When non-nil, `gptel-send' will check for model profile tags
and apply the corresponding configuration before sending."
  :type 'boolean
  :group 'gptel-org-tasks)

(defcustom gptel-org-tasks-change-state-on-send t
  "Whether to change TODO state when sending from an AI-DO task.
When non-nil, calling `gptel-send' inside an AI-DO heading
will transition it to AI-DOING."
  :type 'boolean
  :group 'gptel-org-tasks)

;;; Model Profiles

(defvar gptel-org-tasks--profiles nil
  "Alist mapping profile names (symbols) to gptel configuration plists.
Each entry is (NAME . PLIST) where PLIST contains:
  :backend - backend name string
  :model   - model symbol
  :description - optional description
And any other gptel options.")

(defun gptel-org-tasks-define-profile (name &rest keys)
  "Define a model profile with NAME.

NAME is a symbol that can be used as an org tag.
KEYS is a plist of gptel options.

Required keys:
  :backend - The gptel backend name (string, e.g., \"Claude\")
  :model   - The model symbol (e.g., \\='claude-3-5-haiku-latest)

Optional keys:
  :description - Human-readable description
  :temperature - Model temperature
  :max-tokens  - Maximum response tokens
  :tools       - List of tool names to enable

Example:
  (gptel-org-tasks-define-profile \\='haiku
    :backend \"Claude\"
    :model \\='claude-3-5-haiku-latest
    :description \"Fast, efficient model for simple tasks\")

  (gptel-org-tasks-define-profile \\='opus
    :backend \"Claude\"
    :model \\='claude-opus-4-5-20251101
    :description \"Most capable model for complex reasoning\")"
  (declare (indent 1))
  (unless (symbolp name)
    (error "Profile name must be a symbol: %S" name))
  (unless (plist-get keys :backend)
    (error "Profile %s missing required :backend" name))
  (unless (plist-get keys :model)
    (error "Profile %s missing required :model" name))
  (if-let* ((existing (assq name gptel-org-tasks--profiles)))
      (setcdr existing keys)
    (push (cons name keys) gptel-org-tasks--profiles))
  name)

(defun gptel-org-tasks-get-profile (name)
  "Get the model profile with NAME.
Returns the plist of options, or nil if not found.

Also recognizes model aliases (symbols with :model-id property)
as implicit profiles."
  (or (alist-get name gptel-org-tasks--profiles)
      ;; Check if NAME is a model alias (has :model-id property)
      (when-let* ((model-id (get name :model-id)))
        ;; Find the backend that has this model
        (cl-loop for (backend-name . backend) in gptel--known-backends
                 when (memq name (gptel-backend-models backend))
                 return (list :backend backend-name
                              :model name
                              :description (get name :description))))))

(defun gptel-org-tasks-list-profiles ()
  "List all defined model profiles.
Includes both explicit profiles and model aliases."
  (append
   (mapcar #'car gptel-org-tasks--profiles)
   ;; Include model aliases from all backends
   (cl-loop for (_name . backend) in gptel--known-backends
            nconc (cl-loop for model in (gptel-backend-models backend)
                           when (get model :model-id)
                           collect model))))

;;; Profile Application

(defun gptel-org-tasks--apply-profile (profile-name)
  "Apply the model profile PROFILE-NAME to current buffer.
Returns t if profile was applied, nil otherwise."
  (when-let* ((profile (gptel-org-tasks-get-profile profile-name)))
    (let ((backend-name (plist-get profile :backend))
          (model (plist-get profile :model))
          (temperature (plist-get profile :temperature))
          (max-tokens (plist-get profile :max-tokens))
          (tools (plist-get profile :tools)))
      ;; Apply backend
      (when backend-name
        (when-let* ((backend (alist-get backend-name gptel--known-backends
                                        nil nil #'equal)))
          (setq-local gptel-backend backend)))
      ;; Apply model
      (when model
        (setq-local gptel-model model))
      ;; Apply optional settings
      (when temperature
        (setq-local gptel-temperature temperature))
      (when max-tokens
        (setq-local gptel-max-tokens max-tokens))
      (when tools
        (setq-local gptel-tools
                    (cl-loop for tname in tools
                             for tool = (gptel-get-tool tname)
                             when tool collect tool)))
      (message "gptel: Applied profile '%s' (model: %s)"
               profile-name model)
      t)))

;;; Org Integration

(defun gptel-org-tasks--get-task-info ()
  "Get information about the current AI task heading.
Returns a plist with:
  :todo-state  - Current TODO keyword
  :tags        - List of tags
  :profile-tag - First tag that matches a defined profile
  :heading     - The heading text
  :marker      - Marker to the heading

Returns nil if not in an org heading."
  (when (and (derived-mode-p 'org-mode)
             (org-back-to-heading t))
    (let* ((element (org-element-at-point))
           (todo-state (org-element-property :todo-keyword element))
           (tags (org-get-tags))
           (heading (org-element-property :raw-value element))
           ;; Find first tag that matches a profile
           (profile-tag (cl-find-if
                         (lambda (tag)
                           (gptel-org-tasks-get-profile (intern tag)))
                         tags)))
      (list :todo-state todo-state
            :tags tags
            :profile-tag (when profile-tag (intern profile-tag))
            :heading heading
            :marker (point-marker)))))

(defun gptel-org-tasks--change-todo-state (new-state)
  "Change the current heading's TODO state to NEW-STATE."
  (save-excursion
    (org-back-to-heading t)
    (org-todo new-state)))

(defun gptel-org-tasks--maybe-transition-and-apply ()
  "Handle AI task state transition and profile application.
Called before `gptel-send' to:
1. Check if inside an AI-DO task
2. Apply model profile from tag if present
3. Transition to AI-DOING state

Returns t if inside an AI task, nil otherwise."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (when-let* ((task-info (ignore-errors (gptel-org-tasks--get-task-info))))
        (let ((todo-state (plist-get task-info :todo-state))
              (profile-tag (plist-get task-info :profile-tag)))
          ;; Apply profile from tag if enabled and tag exists
          (when (and gptel-org-tasks-apply-profile-on-send
                     profile-tag)
            (gptel-org-tasks--apply-profile profile-tag))
          ;; Transition AI-DO -> AI-DOING if enabled
          (when (and gptel-org-tasks-change-state-on-send
                     (equal todo-state gptel-org-tasks-todo-keyword))
            (gptel-org-tasks--change-todo-state gptel-org-tasks-doing-keyword)
            (message "gptel: Task state changed to %s"
                     gptel-org-tasks-doing-keyword))
          ;; Return t if this was an AI task
          (string-prefix-p "AI-" (or todo-state "")))))))

;;; Advice for gptel-send

(defun gptel-org-tasks--before-send (&rest _args)
  "Advice to run before `gptel-send' for AI task handling."
  (gptel-org-tasks--maybe-transition-and-apply))

;;;###autoload
(define-minor-mode gptel-org-tasks-mode
  "Minor mode for AI task workflow in org buffers.

When enabled:
- Calling `gptel-send' inside an AI-DO task transitions it to AI-DOING
- Model profiles are applied from task tags before sending

Define profiles with `gptel-org-tasks-define-profile', then tag
your tasks with the profile name:

  ** AI-DO Write a summary :haiku:
  ** AI-DO Complex analysis :opus:"
  :lighter " AI-Tasks"
  :group 'gptel-org-tasks
  (if gptel-org-tasks-mode
      (advice-add 'gptel-send :before #'gptel-org-tasks--before-send)
    (advice-remove 'gptel-send #'gptel-org-tasks--before-send)))

;;; Convenience Commands

(defun gptel-org-tasks-set-profile (profile-name)
  "Interactively apply a model profile to the current buffer.
PROFILE-NAME is selected from defined profiles."
  (interactive
   (list (intern (completing-read "Apply profile: "
                                  (gptel-org-tasks-list-profiles)
                                  nil t))))
  (if (gptel-org-tasks--apply-profile profile-name)
      (message "Applied profile: %s" profile-name)
    (user-error "Profile not found: %s" profile-name)))

(defun gptel-org-tasks-show-profiles ()
  "Display all defined model profiles."
  (interactive)
  (with-help-window "*gptel-org-tasks-profiles*"
    (princ "gptel-org-tasks Model Profiles\n")
    (princ "==============================\n\n")
    (if (null gptel-org-tasks--profiles)
        (princ "No profiles defined.\n\nUse `gptel-org-tasks-define-profile' to add profiles.")
      (dolist (entry gptel-org-tasks--profiles)
        (let* ((name (car entry))
               (plist (cdr entry))
               (desc (plist-get plist :description))
               (backend (plist-get plist :backend))
               (model (plist-get plist :model)))
          (princ (format "%-12s %s:%s\n" name backend model))
          (when desc
            (princ (format "             %s\n" desc)))
          (princ "\n"))))))

(provide 'gptel-org-tasks)
;;; gptel-org-tasks.el ends here
