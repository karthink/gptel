;;; gptel-org-archive.el --- Archive AI task conversations with summaries -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: gptel contributors
;; Keywords: convenience, org, ai

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

;; This module provides functionality to archive AI task conversations in Org
;; mode with automatic summarization.  Instead of archiving the full verbose
;; conversation (with tool calls, intermediate steps, etc.), it generates a
;; concise summary of what was accomplished.
;;
;; The workflow is two-phase:
;; 1. Use `gptel-org-prepare-archive' to summarize a DONE task in-place
;; 2. Review the summary, then use standard `org-archive-subtree' to archive
;;
;; Archive location follows the pattern: *-ai.org → *-ai-archive.org
;;
;; Usage:
;;   M-x gptel-org-prepare-archive  - Summarize current DONE task
;;   C-c C-x C-s                    - Archive the summarized task (standard org)

;;; Code:

(require 'org)
(require 'org-archive)
(eval-when-compile (require 'cl-lib))

(declare-function gptel-request "gptel")
(declare-function gptel-backend-name "gptel")
(declare-function gptel--model-name "gptel")

(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-max-tokens)
(defvar org-state)  ; Dynamically bound by org-mode after TODO state change


;;; User options

(defcustom gptel-org-archive-summary-system-prompt
  "You are a technical documentation assistant. Your task is to create a concise summary of a completed AI-assisted task conversation.

Instructions:
- Summarize WHAT was accomplished, not HOW the conversation went
- Focus on concrete outcomes: files created/modified, problems solved, decisions made
- Omit intermediate tool calls, failed attempts, and back-and-forth discussion
- Use bullet points for multiple outcomes
- Keep the summary brief (3-7 sentences typically)
- If code was written, mention the key functions/features without full implementation details
- Preserve any important decisions or rationale that would be useful for future reference

Git Repository Tracking:
If the conversation includes git commits, include a \"Git Changes\" section at the END of your summary with this exact format:

#+begin_src
Git Changes:
- REPO_PATH: path/to/repo
  COMMITS: abc1234, def5678
#+end_src

Where:
- REPO_PATH is the relative path to the repository root (use \".\" for the working directory)
- COMMITS is a comma-separated list of short commit hashes created during this task
- Include multiple REPO_PATH entries if commits were made to different repositories
- Only include this section if actual git commits were made"
  "System prompt used when generating archive summaries."
  :type 'string
  :group 'gptel)

(defcustom gptel-org-archive-include-metadata t
  "Whether to include metadata in archived summaries.

When non-nil, the summary will include:
- Archive date/time
- Model used for the original task
- Model used for summarization"
  :type 'boolean
  :group 'gptel)

(defcustom gptel-org-archive-location-function
  #'gptel-org-archive--default-location
  "Function to determine archive file location.

The function receives the source buffer's file name and should return
the archive file path.  Default transforms *-ai.org to *-ai-archive.org."
  :type 'function
  :group 'gptel)

(defcustom gptel-org-archive-summary-max-tokens 500
  "Maximum tokens for the generated summary."
  :type 'integer
  :group 'gptel)

(defcustom gptel-org-archive-auto-on-done nil
  "When non-nil, automatically prepare archive when task is marked DONE.

This adds a hook to `org-after-todo-state-change-hook' that triggers
`gptel-org-prepare-archive' when a task transitions to a DONE state
in AI document buffers (files matching *-ai.org pattern) with
`gptel-mode' active.

The automatic archiving will:
1. Generate a summary using the LLM
2. Replace the conversation with the summary
3. Leave the task ready for `org-archive-subtree' (C-c C-x C-s)

Use `gptel-org-restore-original' to undo the summary if needed."
  :type 'boolean
  :group 'gptel)


;;; Internal variables

(defvar-local gptel-org-archive--original-content nil
  "Stores original subtree content before summarization for undo.")

(defvar-local gptel-org-archive--task-metadata nil
  "Stores metadata about the task being archived.")


;;; Archive location

(defun gptel-org-archive--default-location (source-file)
  "Return default archive location for SOURCE-FILE.

Transforms filename-ai.org to filename-ai-archive.org.
Falls back to filename_archive.org for non -ai.org files."
  (let* ((base (file-name-sans-extension source-file))
         (ext (file-name-extension source-file)))
    (if (string-suffix-p "-ai" base)
        (concat base "-archive." ext)
      (concat base "_archive." ext))))

(defun gptel-org-archive--get-location ()
  "Get the archive location for the current buffer."
  (when-let* ((file (buffer-file-name)))
    (funcall gptel-org-archive-location-function file)))


;;; Subtree content extraction

(defun gptel-org-archive--get-subtree-content ()
  "Get the content of the current subtree for summarization.

Returns a plist with :heading, :level, :content, :todo-state, and :properties."
  (save-excursion
    (org-back-to-heading t)
    (let* ((heading (org-get-heading t t t t))
           (level (org-current-level))
           (todo-state (org-get-todo-state))
           (beg (point))
           (end (save-excursion (org-end-of-subtree t t) (point)))
           (content (buffer-substring-no-properties beg end))
           ;; Extract relevant properties
           (props (org-entry-properties nil 'standard))
           (gptel-backend-prop (cdr (assoc "GPTEL_BACKEND" props)))
           (gptel-model-prop (cdr (assoc "GPTEL_MODEL" props))))
      (list :heading heading
            :level level
            :todo-state todo-state
            :content content
            :beg beg
            :end end
            :backend gptel-backend-prop
            :model gptel-model-prop))))

;;; Git metadata extraction

(defun gptel-org-archive--git-run (directory &rest args)
  "Run git with ARGS in DIRECTORY, return trimmed output or nil on error."
  (when (and directory (file-directory-p directory))
    (let ((default-directory directory))
      (condition-case nil
          (let ((output (apply #'process-lines "git" args)))
            (when output (string-trim (car output))))
        (error nil)))))

(defun gptel-org-archive--get-git-info (directory)
  "Get git repository info for DIRECTORY.

Returns a plist with :repo-url, :commit, :branch, or nil if not a git repo."
  (when-let* ((git-dir (locate-dominating-file directory ".git")))
    (let ((repo-url (gptel-org-archive--git-run
                     git-dir "config" "--get" "remote.origin.url"))
          (commit (gptel-org-archive--git-run
                   git-dir "rev-parse" "--short" "HEAD"))
          (branch (gptel-org-archive--git-run
                   git-dir "rev-parse" "--abbrev-ref" "HEAD")))
      (when (or repo-url commit branch)
        (list :repo-url repo-url
              :commit commit
              :branch branch
              :root (abbreviate-file-name git-dir))))))

(defun gptel-org-archive--collect-repo-metadata ()
  "Collect git metadata for the current task context.

Returns a list of plists, one for each unique repository involved:
- The current working directory
- Any other repositories found in the task (future expansion)."
  (let ((repos nil)
        (seen-roots nil))
    ;; Primary: current working directory's repo
    (when-let* ((info (gptel-org-archive--get-git-info default-directory)))
      (let ((root (plist-get info :root)))
        (unless (member root seen-roots)
          (push root seen-roots)
          (push (append (list :name "working-dir") info) repos))))
    (nreverse repos)))

(defun gptel-org-archive--format-repo-metadata (repos)
  "Format REPOS list as org properties."
  (when repos
    (let ((result "")
          (idx 0))
      (dolist (repo repos)
        (let* ((prefix (if (= idx 0) "" (format "_%d" idx)))
               (repo-url (plist-get repo :repo-url))
               (commit (plist-get repo :commit))
               (branch (plist-get repo :branch)))
          (when repo-url
            (setq result (concat result (format ":GIT_REPO%s: %s\n" prefix repo-url))))
          (when commit
            (setq result (concat result (format ":GIT_COMMIT%s: %s\n" prefix commit))))
          (when branch
            (setq result (concat result (format ":GIT_BRANCH%s: %s\n" prefix branch)))))
        (cl-incf idx))
      result)))

(defun gptel-org-archive--parse-git-changes (summary)
  "Parse git changes section from SUMMARY.

Returns a cons cell (CLEAN-SUMMARY . GIT-CHANGES) where:
- CLEAN-SUMMARY is the summary with the Git Changes section removed
- GIT-CHANGES is a list of plists with :path and :commits keys"
  (let ((git-changes nil)
        (clean-summary summary))
    ;; Look for Git Changes section
    (when (string-match
           "\\(?:\n\\|^\\)Git Changes:[ \t]*\n\\(\\(?:- REPO_PATH:.*\n\\(?:  COMMITS:.*\n?\\)?\\)+\\)"
           summary)
      (let ((changes-text (match-string 1 summary)))
        ;; Remove the Git Changes section from summary
        (setq clean-summary
              (string-trim
               (replace-regexp-in-string
                "\\(?:\n\\|^\\)Git Changes:[ \t]*\n\\(?:- REPO_PATH:.*\n\\(?:  COMMITS:.*\n?\\)?\\)+"
                "" summary)))
        ;; Parse each repo entry
        (with-temp-buffer
          (insert changes-text)
          (goto-char (point-min))
          (while (re-search-forward
                  "- REPO_PATH:[ \t]*\\(.+?\\)[ \t]*\n\\(?:  COMMITS:[ \t]*\\(.+?\\)[ \t]*\\)?$"
                  nil t)
            (let ((path (string-trim (match-string 1)))
                  (commits (when (match-string 2)
                             (string-trim (match-string 2)))))
              (push (list :path path :commits commits) git-changes))))))
    (cons clean-summary (nreverse git-changes))))

(defun gptel-org-archive--format-git-changes (git-changes)
  "Format GIT-CHANGES as org properties.

GIT-CHANGES is a list of plists with :path and :commits keys."
  (when git-changes
    (let ((result "")
          (idx 0))
      (dolist (change git-changes)
        (let* ((prefix (if (= idx 0) "" (format "_%d" idx)))
               (path (plist-get change :path))
               (commits (plist-get change :commits)))
          (when path
            (setq result (concat result (format ":GIT_PATH%s: %s\n" prefix path))))
          (when commits
            (setq result (concat result (format ":GIT_COMMITS%s: %s\n" prefix commits)))))
        (cl-incf idx))
      result)))


;;; Conversation extraction

(defun gptel-org-archive--extract-conversation (content)
  "Extract the conversation parts from CONTENT for summarization.

Removes property drawers and focuses on the actual conversation text."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    ;; Remove property drawers
    (while (re-search-forward org-property-drawer-re nil t)
      (replace-match ""))
    ;; Remove excessive blank lines
    (goto-char (point-min))
    (while (re-search-forward "\n\\{3,\\}" nil t)
      (replace-match "\n\n"))
    (string-trim (buffer-string))))


;;; Summary generation

(defun gptel-org-archive--generate-summary (task-info callback)
  "Generate a summary for TASK-INFO and call CALLBACK with result.

TASK-INFO is a plist from `gptel-org-archive--get-subtree-content'.
CALLBACK receives the summary string on success, or nil on failure."
  (let* ((heading (plist-get task-info :heading))
         (content (plist-get task-info :content))
         (conversation (gptel-org-archive--extract-conversation content))
         (prompt (format "Summarize the following completed task conversation.\n\nTask: %s\n\nConversation:\n%s"
                         heading conversation))
         ;; Temporarily bind max-tokens for the summary request
         (gptel-max-tokens gptel-org-archive-summary-max-tokens))
    (require 'gptel)
    (gptel-request prompt
      :system gptel-org-archive-summary-system-prompt
      :stream nil
      :callback
      (lambda (response info)
        (if (stringp response)
            (funcall callback response)
          (funcall callback nil)
          (message "gptel-org-archive: Summary generation failed: %s"
                   (plist-get info :status)))))))

(defun gptel-org-archive--format-summary (summary task-info)
  "Format SUMMARY with metadata from TASK-INFO for archival.

Returns the formatted string to replace the subtree content.
Preserves the original heading level.

If SUMMARY contains a \"Git Changes\" section (as instructed by the
system prompt), it will be parsed and included in the properties."
  (let* ((parsed (gptel-org-archive--parse-git-changes summary))
         (clean-summary (car parsed))
         (git-changes (cdr parsed))
         (heading (plist-get task-info :heading))
         (level (or (plist-get task-info :level) 1))
         (stars (make-string level ?*))
         (todo-state (plist-get task-info :todo-state))
         (original-backend (plist-get task-info :backend))
         (original-model (plist-get task-info :model))
         (current-backend (gptel-backend-name gptel-backend))
         (current-model (gptel--model-name gptel-model))
         (timestamp (format-time-string "[%Y-%m-%d %a %H:%M]")))
    (concat
     ;; Heading with TODO state, preserving original level
     (if todo-state
         (format "%s %s %s\n" stars todo-state heading)
       (format "%s %s\n" stars heading))
     ;; Metadata as properties
     (when gptel-org-archive-include-metadata
       (let ((repo-metadata (gptel-org-archive--format-repo-metadata
                             (gptel-org-archive--collect-repo-metadata)))
             (git-changes-metadata (gptel-org-archive--format-git-changes
                                    git-changes)))
         (concat
          ":PROPERTIES:\n"
          (format ":ARCHIVE_DATE: %s\n" timestamp)
          (when original-backend
            (format ":ORIGINAL_BACKEND: %s\n" original-backend))
          (when original-model
            (format ":ORIGINAL_MODEL: %s\n" original-model))
          (format ":SUMMARY_BACKEND: %s\n" current-backend)
          (format ":SUMMARY_MODEL: %s\n" current-model)
          repo-metadata
          git-changes-metadata
          ":END:\n")))
     ;; Summary content (one level deeper than heading)
     (format "\n%s* Summary\n" stars)
     clean-summary
     "\n")))


;;; Main commands

;;;###autoload
(defun gptel-org-prepare-archive ()
  "Prepare the current DONE task for archival by generating a summary.

This replaces the verbose conversation with a concise summary of what
was accomplished.  The original content is saved and can be restored
with `gptel-org-restore-original'.

After reviewing the summary, use `org-archive-subtree' (C-c C-x C-s)
to archive the task.

The archive location is determined by `gptel-org-archive-location-function',
which by default transforms *-ai.org files to *-ai-archive.org."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "This command only works in Org mode"))
  (unless (org-at-heading-p)
    (org-back-to-heading t))
  (let ((todo-state (org-get-todo-state)))
    (unless (member todo-state org-done-keywords)
      (user-error "Task is not marked as DONE (current state: %s)"
                  (or todo-state "none"))))
  ;; Set archive location for this buffer
  (when-let* ((archive-loc (gptel-org-archive--get-location)))
    (setq-local org-archive-location (concat archive-loc "::* Archived Tasks")))
  (let ((task-info (gptel-org-archive--get-subtree-content)))
    ;; Save original for undo
    (setq gptel-org-archive--original-content
          (cons (plist-get task-info :beg)
                (plist-get task-info :content)))
    (setq gptel-org-archive--task-metadata task-info)
    (message "Generating summary...")
    (gptel-org-archive--generate-summary
     task-info
     (lambda (summary)
       (if summary
           (gptel-org-archive--replace-with-summary summary task-info)
         (message "Summary generation failed. Original content preserved."))))))

(defun gptel-org-archive--replace-with-summary (summary task-info)
  "Replace subtree with SUMMARY using TASK-INFO."
  (let ((beg (plist-get task-info :beg))
        (end (plist-get task-info :end))
        (formatted (gptel-org-archive--format-summary summary task-info)))
    (save-excursion
      (goto-char beg)
      (delete-region beg end)
      (insert formatted))
    (message "Summary generated. Review and use `org-archive-subtree' (C-c C-x C-s) to archive.
Use `gptel-org-restore-original' to undo.")))

;;;###autoload
(defun gptel-org-restore-original ()
  "Restore the original conversation content before summarization.

This undoes the changes made by `gptel-org-prepare-archive'."
  (interactive)
  (unless gptel-org-archive--original-content
    (user-error "No original content to restore"))
  (let ((beg (car gptel-org-archive--original-content))
        (content (cdr gptel-org-archive--original-content)))
    (save-excursion
      (goto-char beg)
      (org-back-to-heading t)
      (let ((current-end (save-excursion (org-end-of-subtree t t) (point))))
        (delete-region (point) current-end)
        (insert content)))
    (setq gptel-org-archive--original-content nil)
    (setq gptel-org-archive--task-metadata nil)
    (message "Original content restored.")))

;;;###autoload
(defun gptel-org-archive-done-tasks ()
  "Interactively prepare and archive all DONE tasks in the buffer.

For each DONE task, this will:
1. Generate a summary
2. Show the summary for review
3. Ask whether to archive, skip, or abort"
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "This command only works in Org mode"))
  (let ((done-tasks nil))
    ;; Collect all DONE tasks
    ;; Use regexp match with TODO={regexp} syntax for custom keywords
    ;; The "/" syntax doesn't work with custom done keywords like AI-DONE
    (org-map-entries
     (lambda ()
       (push (point-marker) done-tasks))
     (format "TODO={%s}" (regexp-opt org-done-keywords))
     'file)
    (setq done-tasks (nreverse done-tasks))
    (if (null done-tasks)
        (message "No DONE tasks found.")
      (message "Found %d DONE task(s). Processing..." (length done-tasks))
      ;; Process tasks one at a time
      (gptel-org-archive--process-next-task done-tasks))))

(defun gptel-org-archive--process-next-task (task-markers)
  "Process the next task in TASK-MARKERS list."
  (if (null task-markers)
      (message "All tasks processed.")
    (let ((marker (car task-markers))
          (remaining (cdr task-markers)))
      (when (marker-buffer marker)
        (with-current-buffer (marker-buffer marker)
          (goto-char marker)
          (set-marker marker nil)       ; Clean up marker
          (let ((heading (org-get-heading t t t t)))
            (when (y-or-n-p (format "Prepare archive for: %s? " heading))
              (let ((task-info (gptel-org-archive--get-subtree-content)))
                (gptel-org-archive--generate-summary
                 task-info
                 (lambda (summary)
                   (if summary
                       (let ((task-beg (plist-get task-info :beg)))
                         (gptel-org-archive--replace-with-summary summary task-info)
                         (when (y-or-n-p "Archive this task? ")
                           ;; Move point to task heading before archiving
                           (goto-char task-beg)
                           (org-archive-subtree)))
                     (message "Summary failed for: %s" heading))
                   ;; Continue with next task
                   (gptel-org-archive--process-next-task remaining)))))))))))


;;; Integration with org-archive

;;;###autoload
(defun gptel-org-archive--setup-location ()
  "Set up archive location for AI document buffers.

This sets `org-archive-location' buffer-locally for files matching
the *-ai.org pattern.  Must be autoloaded so it runs on
`org-mode-hook' before the user explicitly loads gptel-org-archive."
  (when (and (buffer-file-name)
             (derived-mode-p 'org-mode))
    (when-let* ((file (buffer-file-name))
                (base (file-name-sans-extension file))
                (_is-ai (string-suffix-p "-ai" base))
                (archive-loc (concat base "-archive."
                                     (file-name-extension file))))
      (setq-local org-archive-location
                  (concat archive-loc "::* Archived Tasks")))))

;; Automatically set archive location for AI documents
;;;###autoload
(add-hook 'org-mode-hook #'gptel-org-archive--setup-location)


;;; Auto-archive on DONE

(defun gptel-org-archive--auto-on-done ()
  "Automatically prepare archive when a task is marked DONE in gptel buffers.

This function is designed to be added to `org-after-todo-state-change-hook'.
It only triggers when:
- `gptel-org-archive-auto-on-done' is non-nil
- The task transitions to a DONE state
- The buffer is an AI document (file matches *-ai.org pattern)
- `gptel-mode' is active

After triggering, the task conversation will be replaced with a summary.
Use `gptel-org-restore-original' to undo, or `org-archive-subtree' to archive."
  (when (and gptel-org-archive-auto-on-done
             (bound-and-true-p gptel-mode)
             (buffer-file-name)
             (string-match-p "-ai\\.org$" (buffer-file-name))
             (member org-state org-done-keywords))
    (message "gptel: Auto-preparing archive for DONE task...")
    ;; Set archive location for this buffer
    (when-let* ((archive-loc (gptel-org-archive--get-location)))
      (setq-local org-archive-location (concat archive-loc "::* Archived Tasks")))
    (let ((task-info (gptel-org-archive--get-subtree-content)))
      ;; Save original for undo
      (setq gptel-org-archive--original-content
            (cons (plist-get task-info :beg)
                  (plist-get task-info :content)))
      (setq gptel-org-archive--task-metadata task-info)
      (gptel-org-archive--generate-summary
       task-info
       (lambda (summary)
         (if summary
             (progn
               (gptel-org-archive--replace-with-summary summary task-info)
               (message "gptel: Summary generated. Use `org-archive-subtree' (C-c C-x C-s) to archive, or `gptel-org-restore-original' to undo."))
           (message "gptel: Summary generation failed. Original content preserved.")))))))

(defun gptel-org-archive--setup-auto-archive ()
  "Set up auto-archive hook for the current buffer if appropriate."
  (when (and (buffer-file-name)
             (string-match-p "-ai\\.org$" (buffer-file-name))
             (derived-mode-p 'org-mode))
    (add-hook 'org-after-todo-state-change-hook
              #'gptel-org-archive--auto-on-done nil t)))

;; Set up auto-archive hook for AI documents
(add-hook 'org-mode-hook #'gptel-org-archive--setup-auto-archive)

(provide 'gptel-org-archive)
;;; gptel-org-archive.el ends here
