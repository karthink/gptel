;;; gptel-org-cache.el --- Context caching for gptel Org mode -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: gptel contributors
;; Keywords: convenience, org, ai, cache

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

;; This module provides context caching for gptel in Org mode.
;;
;; When working with AI documents that contain file links to other documents
;; (design docs, flow configs, external dependencies), these linked files
;; are often stable and don't change frequently.  Context caching allows
;; pre-processing these linked files and storing the processed context
;; locally (not version controlled) for faster loading.
;;
;; Cache location follows the pattern: *-ai.org → *-ai-cache.org
;;
;; Two caching strategies are available:
;; - Files cache: Raw file contents (fast, deterministic, full fidelity)
;; - Summary cache: AI-generated summaries (token-efficient, semantic)
;;
;; Key features:
;; - Extract file links from heading trees (parent headings and ancestors)
;; - Hash-based cache invalidation (detect when files change)
;; - Per-heading cache storage with multiple cache types
;; - Tag-based automatic caching (cache_files, cache_summary)
;; - Integration with gptel context injection
;;
;; Usage:
;;   M-x gptel-org-cache-prepare-files   - Cache raw file contents
;;   M-x gptel-org-cache-prepare-summary - Cache AI-summarized content
;;   M-x gptel-org-cache-invalidate      - Force cache rebuild
;;   M-x gptel-org-cache-status          - Show cache status
;;
;; Tag-based automatic caching:
;;   Add :cache_files: tag to a heading for automatic file caching
;;   Add :cache_summary: tag to a heading for automatic AI summary caching

;;; Code:

(require 'org)
(require 'org-element)
(require 'org-src)
(eval-when-compile (require 'cl-lib))

(declare-function gptel-context--string "gptel-context")
(declare-function gptel-context--insert-file-string "gptel-context")
(declare-function gptel-fsm-info "gptel-request")
(declare-function gptel-request "gptel")

(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel--system-message)
(defvar gptel-prompt-transform-functions)

;; Register gptel-cache as a source block language with org-src to prevent
;; org-lint warnings about unknown source block language. Cache content blocks
;; use #+begin_src gptel-cache but don't require a Babel backend.
(eval-when-compile
  (when (boundp 'org-src-lang-modes)
    (add-to-list 'org-src-lang-modes '("gptel-cache" . fundamental))))

(with-eval-after-load 'org-src
  (unless (assoc "gptel-cache" org-src-lang-modes)
    (add-to-list 'org-src-lang-modes '("gptel-cache" . fundamental))))

(defvar org-link-bracket-re)
(defvar org-link-angle-re)


;;; User options

(defcustom gptel-org-cache-enabled t
  "Whether context caching is enabled.

When non-nil, gptel will use cached context for headings where available."
  :type 'boolean
  :group 'gptel)

(defcustom gptel-org-cache-location-function
  #'gptel-org-cache--default-location
  "Function to determine cache file location.

The function receives the source buffer's file name and should return
the cache file path.  Default transforms *-ai.org to *-ai-cache.org."
  :type 'function
  :group 'gptel)

(defcustom gptel-org-cache-auto-update t
  "Whether to automatically update stale cache entries.

When non-nil, gptel will automatically regenerate cache entries
when the underlying files have changed."
  :type 'boolean
  :group 'gptel)

(defcustom gptel-org-cache-include-property-drawers t
  "Whether to include :base-ai-context: and similar drawers in cache.

When non-nil, property drawers with names ending in '-context' will be
included in the cached context."
  :type 'boolean
  :group 'gptel)

(defcustom gptel-org-cache-summary-backend nil
  "Backend to use for generating cache summaries.

When nil, uses `gptel-backend'.  Can be set to a specific backend
for summary generation (e.g., a cheaper/faster model)."
  :type '(choice (const :tag "Use gptel-backend" nil)
                 (symbol :tag "Specific backend"))
  :group 'gptel)

(defcustom gptel-org-cache-summary-model nil
  "Model to use for generating cache summaries.

When nil, uses `gptel-model'.  Can be set to a specific model
for summary generation."
  :type '(choice (const :tag "Use gptel-model" nil)
                 (symbol :tag "Specific model"))
  :group 'gptel)

(defcustom gptel-org-cache-summary-system-prompt
  "You are a technical documentation specialist. Your task is to create a
concise but comprehensive summary of the provided file contents.

For each file, extract and preserve:
- Function/method signatures with their parameters and return types
- Class/struct definitions with key fields
- Important constants and configuration values
- Key algorithms or logic flows (summarized)
- Dependencies and imports that matter for understanding
- Any documentation strings or comments that explain purpose

Format the output as structured notes that an AI assistant can use
to understand and work with this codebase. Preserve exact names and
signatures but summarize implementation details.

Be concise but complete - the goal is to minimize tokens while
retaining all information needed to work with these files."
  "System prompt for AI-generated cache summaries."
  :type 'string
  :group 'gptel)

(defcustom gptel-org-cache-files-tag "cache_files"
  "Org tag that triggers automatic file caching for a heading."
  :type 'string
  :group 'gptel)

(defcustom gptel-org-cache-summary-tag "cache_summary"
  "Org tag that triggers automatic summary caching for a heading."
  :type 'string
  :group 'gptel)


;;; Internal variables

(defvar gptel-org-cache--index nil
  "Alist of cached file information.

Each entry is (FILE-PATH . (:hash HASH :mtime MTIME :size SIZE))
where HASH is the content hash for cache invalidation.")

(defvar-local gptel-org-cache--buffer-index nil
  "Buffer-local cache index for current session.")


;;; Cache location

(defun gptel-org-cache--default-location (source-file)
  "Return default cache location for SOURCE-FILE.

Transforms filename-ai.org to filename-ai-cache.org.
Falls back to filename_cache.org for non -ai.org files."
  (let* ((base (file-name-sans-extension source-file))
         (ext (file-name-extension source-file)))
    (if (string-suffix-p "-ai" base)
        (concat base "-cache." ext)
      (concat base "_cache." ext))))

(defun gptel-org-cache--get-location ()
  "Get the cache location for the current buffer."
  (when-let* ((file (buffer-file-name)))
    (funcall gptel-org-cache-location-function file)))


;;; File hashing and change detection

(defun gptel-org-cache--file-hash (file)
  "Compute a hash for FILE based on modification time and size.

Returns a string hash, or nil if file doesn't exist."
  (when (and file (file-readable-p file))
    (let* ((attrs (file-attributes file))
           (mtime (file-attribute-modification-time attrs))
           (size (file-attribute-size attrs)))
      (format "%s-%d" (format-time-string "%Y%m%d%H%M%S" mtime) size))))

(defun gptel-org-cache--file-content-hash (file)
  "Compute MD5 hash of FILE content.

This is more accurate but slower than `gptel-org-cache--file-hash'."
  (when (and file (file-readable-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (md5 (buffer-string)))))

(defun gptel-org-cache--files-changed-p (files hash-alist)
  "Check if any FILES have changed compared to HASH-ALIST.

HASH-ALIST is ((FILE . HASH) ...).
Returns list of changed files, or nil if all unchanged."
  (let (changed)
    (dolist (file files)
      (let* ((stored-hash (cdr (assoc file hash-alist)))
             (current-hash (gptel-org-cache--file-hash file)))
        (unless (equal stored-hash current-hash)
          (push file changed))))
    (nreverse changed)))


;;; Link extraction

(defconst gptel-org-cache--link-regex
  (concat "\\[\\[\\(?:file:\\|attachment:\\)\\([^]]+\\)\\]")
  "Regex to extract file links from Org content.")

(defun gptel-org-cache--extract-file-links (content)
  "Extract file link paths from CONTENT string.

Returns a list of absolute file paths."
  (let (links)
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (while (re-search-forward gptel-org-cache--link-regex nil t)
        (let* ((raw-path (match-string 1))
               ;; Remove any description part (after ::)
               (path (car (split-string raw-path "::")))
               ;; Expand ~ and relative paths
               (expanded (expand-file-name path)))
          (when (and (file-exists-p expanded)
                     (not (file-directory-p expanded))
                     (not (member expanded links)))
            (push expanded links)))))
    (nreverse links)))

(defun gptel-org-cache--extract-drawer-content (name)
  "Extract content of drawer with NAME from current heading.

Returns the drawer content as a string, or nil if not found."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (save-excursion (org-end-of-subtree t t) (point))))
      (when (re-search-forward
             (format "^[ \t]*:%s:[ \t]*\n\\(\\(?:.*\n\\)*?\\)[ \t]*:END:"
                     (regexp-quote name))
             end t)
        (match-string 1)))))

(defun gptel-org-cache--get-heading-path ()
  "Get the outline path for the current heading.

Returns a list of heading titles from root to current."
  (save-excursion
    (let (path)
      (org-back-to-heading t)
      (push (org-get-heading t t t t) path)
      (while (org-up-heading-safe)
        (push (org-get-heading t t t t) path))
      path)))

(defun gptel-org-cache--collect-ancestor-content ()
  "Collect content from current heading and all ancestors.

Returns a plist with:
  :content - Combined content string
  :files - List of extracted file paths
  :headings - List of heading titles from root to current"
  (save-excursion
    (let ((content-parts nil)
          (all-files nil)
          (headings nil))
      ;; Collect from current heading
      (org-back-to-heading t)
      (let* ((heading (org-get-heading t t t t))
             (beg (point))
             (end (save-excursion
                    ;; Get just this heading's content, not children
                    (outline-next-heading)
                    (or (point) (point-max))))
             (text (buffer-substring-no-properties beg end)))
        (push heading headings)
        (push text content-parts)
        (setq all-files (nconc all-files (gptel-org-cache--extract-file-links text))))
      ;; Collect from ancestors (going up the tree)
      (while (org-up-heading-safe)
        (let* ((heading (org-get-heading t t t t))
               (beg (point))
               (end (save-excursion
                      (outline-next-heading)
                      (or (point) (point-max))))
               (text (buffer-substring-no-properties beg end)))
          (push heading headings)
          (push text content-parts)
          (setq all-files (nconc all-files (gptel-org-cache--extract-file-links text)))))
      ;; Also check for top-level content before first heading
      (goto-char (point-min))
      (when (not (org-at-heading-p))
        (let* ((end (save-excursion
                      (outline-next-heading)
                      (or (point) (point-max))))
               (text (buffer-substring-no-properties (point) end)))
          (push text content-parts)
          (setq all-files (nconc all-files (gptel-org-cache--extract-file-links text)))))
      ;; headings is currently (current parent grandparent ...) - no reverse needed
      ;; because we want root-to-current order which is what push gives us when
      ;; we traverse from current up to root
      ;; Actually: push adds to front, so after collecting "Third", "Second", "First"
      ;; we have ("First" "Second" "Third") which IS root-to-current
      (list :content (mapconcat #'identity (nreverse content-parts) "\n")
            :files (delete-dups all-files)
            :headings headings))))


;;; Cache storage format

(defconst gptel-org-cache-type-files 'files
  "Cache type for raw file contents.")

(defconst gptel-org-cache-type-summary 'summary
  "Cache type for AI-summarized content.")

(defun gptel-org-cache--heading-id (headings &optional cache-type)
  "Generate a unique ID for cache entry from HEADINGS list.

Optional CACHE-TYPE distinguishes between different cache types
for the same heading (e.g., files vs summary)."
  (md5 (concat (mapconcat #'identity headings "/")
               (when cache-type (format ":%s" cache-type)))))

(defun gptel-org-cache--format-entry (heading-id data)
  "Format a cache entry for HEADING-ID with DATA.

DATA is a plist with :content, :files, :headings, :file-hashes, :cache-type."
  (let ((headings (plist-get data :headings))
        (files (plist-get data :files))
        (file-hashes (plist-get data :file-hashes))
        (content (plist-get data :content))
        (cache-type (or (plist-get data :cache-type) 'files)))
    (concat
     (format "* %s (%s)\n" (car (last headings)) cache-type)
     ":PROPERTIES:\n"
     (format ":CACHE_ID: %s\n" heading-id)
     (format ":CACHE_TYPE: %s\n" cache-type)
     (format ":CACHE_DATE: %s\n" (format-time-string "[%Y-%m-%d %a %H:%M]"))
     (format ":HEADING_PATH: %s\n" (mapconcat #'identity headings " > "))
     ":END:\n\n"
     "** File Hashes\n"
     "#+begin_src elisp\n"
     (format "%S\n" file-hashes)
     "#+end_src\n\n"
     "** Cached Files\n"
     (mapconcat (lambda (f) (format "- [[file:%s]]" f)) files "\n")
     "\n\n"
     (format "** Cached Context (%s)\n" cache-type)
     "#+begin_src gptel-cache\n"
     (org-escape-code-in-string content)
     "\n#+end_src\n\n")))


;;; Cache file operations

(defun gptel-org-cache--ensure-cache-file ()
  "Ensure cache file exists and return its path."
  (when-let* ((cache-file (gptel-org-cache--get-location)))
    (unless (file-exists-p cache-file)
      (with-temp-file cache-file
        (insert "#+TITLE: AI Context Cache\n")
        (insert "#+STARTUP: overview\n")
        (insert "# This file is auto-generated. Do not edit manually.\n")
        (insert "# Add to .gitignore - this file should not be version controlled.\n\n")))
    cache-file))

(defun gptel-org-cache--find-entry (cache-file heading-id &optional cache-type)
  "Find cache entry for HEADING-ID in CACHE-FILE.

Optional CACHE-TYPE specifies which cache type to find (files or summary).
When nil, finds any matching entry.

Returns a plist with :beg, :end, :file-hashes, :content, :cache-type,
or nil if not found."
  (when (file-exists-p cache-file)
    (with-temp-buffer
      (insert-file-contents cache-file)
      (org-mode)
      (goto-char (point-min))
      (let ((id-re (format ":CACHE_ID:[ \t]+%s" (regexp-quote heading-id))))
        (when (re-search-forward id-re nil t)
          (org-back-to-heading t)
          (let* ((beg (point))
                 (end (save-excursion (org-end-of-subtree t t) (point)))
                 file-hashes content found-type)
            ;; Extract cache type
            (save-excursion
              (when (re-search-forward ":CACHE_TYPE:[ \t]+\\([a-z]+\\)" end t)
                (setq found-type (intern (match-string 1)))))
            ;; If cache-type specified, verify it matches
            (when (or (null cache-type) (eq cache-type found-type))
              ;; Extract file hashes
              (save-excursion
                (when (re-search-forward "^\\*\\* File Hashes" end t)
                  (when (re-search-forward "#\\+begin_src elisp\n\\(\\(?:.*\n\\)*?\\)#\\+end_src" end t)
                    (setq file-hashes (ignore-errors (read (match-string 1)))))))
              ;; Extract cached context
              (save-excursion
                (when (re-search-forward "^\\*\\* Cached Context" end t)
                  (when (re-search-forward "#\\+begin_src gptel-cache\n\\(\\(?:.*\n\\)*?\\)#\\+end_src" end t)
                    (setq content (org-unescape-code-in-string (match-string 1))))))
              (list :beg beg :end end :file-hashes file-hashes
                    :content content :cache-type (or found-type 'files)))))))))

(defun gptel-org-cache--write-entry (cache-file heading-id data)
  "Write or update cache entry for HEADING-ID in CACHE-FILE.

DATA is a plist with :content, :files, :headings, :file-hashes."
  (let ((existing (gptel-org-cache--find-entry cache-file heading-id))
        (formatted (gptel-org-cache--format-entry heading-id data)))
    (with-current-buffer (find-file-noselect cache-file)
      (if existing
          ;; Replace existing entry
          (progn
            (goto-char (plist-get existing :beg))
            (delete-region (plist-get existing :beg) (plist-get existing :end))
            (insert formatted))
        ;; Append new entry
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert formatted))
      (save-buffer)
      (message "Cache entry updated for %s" (car (last (plist-get data :headings)))))))

(defun gptel-org-cache--delete-entry (cache-file heading-id &optional cache-type)
  "Delete cache entry for HEADING-ID from CACHE-FILE.

Optional CACHE-TYPE specifies which cache type to delete."
  (when-let* ((existing (gptel-org-cache--find-entry cache-file heading-id cache-type)))
    (with-current-buffer (find-file-noselect cache-file)
      (delete-region (plist-get existing :beg) (plist-get existing :end))
      (save-buffer))))


;;; Context generation

(defun gptel-org-cache--file-binary-p (file)
  "Check if FILE is binary.

Returns non-nil if FILE appears to be binary."
  (with-temp-buffer
    (insert-file-contents file nil 0 512)
    (goto-char (point-min))
    (search-forward "\0" nil t)))

(defun gptel-org-cache--generate-context (files)
  "Generate context string from FILES list.

Uses gptel's context formatting to create annotated markdown."
  (with-temp-buffer
    (dolist (file files)
      (when (and (file-readable-p file)
                 (not (gptel-org-cache--file-binary-p file)))
        (insert (format "In file `%s`:\n\n```\n" (abbreviate-file-name file)))
        (insert-file-contents file)
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert "```\n\n")))
    (buffer-string)))

(defun gptel-org-cache--generate-summary (raw-context callback)
  "Generate AI summary of RAW-CONTEXT and call CALLBACK with result.

CALLBACK is called with the summary string, or nil on failure.
Uses `gptel-org-cache-summary-backend' and `gptel-org-cache-summary-model'
if set, otherwise falls back to `gptel-backend' and `gptel-model'."
  (let ((gptel-backend (or gptel-org-cache-summary-backend gptel-backend))
        (gptel-model (or gptel-org-cache-summary-model gptel-model)))
    (gptel-request raw-context
      :system gptel-org-cache-summary-system-prompt
      :callback (lambda (response info)
                  (if (and response (not (plist-get info :error)))
                      (funcall callback response)
                    (funcall callback nil))))))


;;; Main commands

;;;###autoload
(defun gptel-org-cache-prepare-files ()
  "Build or update file cache for the current heading's context.

This extracts file links from the current heading and its ancestors,
reads the raw file contents, and stores them in the cache file for
faster access in future sessions.

See also `gptel-org-cache-prepare-summary' for AI-summarized caching."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "This command only works in Org mode"))
  (unless (org-at-heading-p)
    (org-back-to-heading t))
  (let* ((cache-file (gptel-org-cache--ensure-cache-file))
         (collected (gptel-org-cache--collect-ancestor-content))
         (files (plist-get collected :files))
         (headings (plist-get collected :headings))
         (heading-id (gptel-org-cache--heading-id headings 'files)))
    (if (null files)
        (message "No file links found in heading tree.")
      (let* ((file-hashes (mapcar (lambda (f) (cons f (gptel-org-cache--file-hash f))) files))
             (context (gptel-org-cache--generate-context files))
             (data (list :content context
                         :files files
                         :headings headings
                         :file-hashes file-hashes
                         :cache-type 'files)))
        (gptel-org-cache--write-entry cache-file heading-id data)
        (message "Cached %d file(s) for heading: %s"
                 (length files) (car (last headings)))))))

;; Keep old name as alias for compatibility
(defalias 'gptel-org-cache-prepare 'gptel-org-cache-prepare-files)

;;;###autoload
(defun gptel-org-cache-prepare-summary ()
  "Build or update AI-summarized cache for the current heading's context.

This extracts file links from the current heading and its ancestors,
sends the file contents to an LLM for summarization, and stores the
summarized context in the cache file.

The summary is more token-efficient than raw file caching but requires
an API call to generate.  Use `gptel-org-cache-summary-backend' and
`gptel-org-cache-summary-model' to configure which model to use.

See also `gptel-org-cache-prepare-files' for raw file caching."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "This command only works in Org mode"))
  (unless (org-at-heading-p)
    (org-back-to-heading t))
  (let* ((cache-file (gptel-org-cache--ensure-cache-file))
         (collected (gptel-org-cache--collect-ancestor-content))
         (files (plist-get collected :files))
         (headings (plist-get collected :headings))
         (heading-id (gptel-org-cache--heading-id headings 'summary)))
    (if (null files)
        (message "No file links found in heading tree.")
      (let* ((file-hashes (mapcar (lambda (f) (cons f (gptel-org-cache--file-hash f))) files))
             (raw-context (gptel-org-cache--generate-context files)))
        (message "Generating AI summary for %d file(s)..." (length files))
        (gptel-org-cache--generate-summary
         raw-context
         (lambda (summary)
           (if (null summary)
               (message "Failed to generate summary")
             (let ((data (list :content summary
                               :files files
                               :headings headings
                               :file-hashes file-hashes
                               :cache-type 'summary)))
               (gptel-org-cache--write-entry cache-file heading-id data)
               (message "Cached AI summary for %d file(s): %s"
                        (length files) (car (last headings)))))))))))

;;;###autoload
(defun gptel-org-cache-invalidate (&optional cache-type)
  "Force rebuild of cache for the current heading.

With prefix arg, prompt for CACHE-TYPE to invalidate (files or summary).
Without prefix, invalidates and rebuilds the files cache."
  (interactive
   (list (when current-prefix-arg
           (intern (completing-read "Cache type to invalidate: "
                                    '("files" "summary" "all") nil t)))))
  (unless (derived-mode-p 'org-mode)
    (user-error "This command only works in Org mode"))
  (unless (org-at-heading-p)
    (org-back-to-heading t))
  (let* ((cache-file (gptel-org-cache--get-location))
         (headings (plist-get (gptel-org-cache--collect-ancestor-content) :headings)))
    (when (and cache-file (file-exists-p cache-file))
      (pcase cache-type
        ('all
         (gptel-org-cache--delete-entry cache-file
                                        (gptel-org-cache--heading-id headings 'files) 'files)
         (gptel-org-cache--delete-entry cache-file
                                        (gptel-org-cache--heading-id headings 'summary) 'summary)
         (message "All caches invalidated for: %s" (car (last headings))))
        ('summary
         (gptel-org-cache--delete-entry cache-file
                                        (gptel-org-cache--heading-id headings 'summary) 'summary)
         (message "Summary cache invalidated for: %s" (car (last headings)))
         (gptel-org-cache-prepare-summary))
        (_  ; Default to files
         (gptel-org-cache--delete-entry cache-file
                                        (gptel-org-cache--heading-id headings 'files) 'files)
         (message "Files cache invalidated for: %s" (car (last headings)))
         (gptel-org-cache-prepare-files))))))

;;;###autoload
(defun gptel-org-cache-status ()
  "Show cache status for the current heading."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "This command only works in Org mode"))
  (unless (org-at-heading-p)
    (org-back-to-heading t))
  (let* ((cache-file (gptel-org-cache--get-location))
         (collected (gptel-org-cache--collect-ancestor-content))
         (files (plist-get collected :files))
         (headings (plist-get collected :headings))
         (files-id (gptel-org-cache--heading-id headings 'files))
         (summary-id (gptel-org-cache--heading-id headings 'summary))
         (files-entry (when cache-file
                        (gptel-org-cache--find-entry cache-file files-id 'files)))
         (summary-entry (when cache-file
                          (gptel-org-cache--find-entry cache-file summary-id 'summary))))
    (if (and (null files-entry) (null summary-entry))
        (message "No cache entries for: %s\nFiles in tree: %d"
                 (car (last headings)) (length files))
      (let* ((files-status
              (if files-entry
                  (let* ((stored-hashes (plist-get files-entry :file-hashes))
                         (changed (gptel-org-cache--files-changed-p files stored-hashes)))
                    (if changed "STALE" "VALID"))
                "NONE"))
             (summary-status
              (if summary-entry
                  (let* ((stored-hashes (plist-get summary-entry :file-hashes))
                         (changed (gptel-org-cache--files-changed-p files stored-hashes)))
                    (if changed "STALE" "VALID"))
                "NONE")))
        (message "Cache status for: %s\n  Files cache: %s\n  Summary cache: %s\n  Files in tree: %d"
                 (car (last headings))
                 files-status
                 summary-status
                 (length files))))))

;;;###autoload
(defun gptel-org-cache-get-context ()
  "Get cached context for the current heading, if available and valid.

Returns the cached context string, or nil if cache is missing or stale.
When `gptel-org-cache-auto-update' is non-nil, automatically rebuilds
stale cache entries.

Cache type preference:
- If heading has `gptel-org-cache-summary-tag', prefer summary cache
- If heading has `gptel-org-cache-files-tag', prefer files cache
- Otherwise, use summary if available, then files

If no cache exists for the current heading, walks up parent headings
to find a cached ancestor.  This allows task sub-headings to inherit
cache from parent topic headings."
  (when (and gptel-org-cache-enabled
             (derived-mode-p 'org-mode))
    (save-excursion
      (condition-case nil
          (progn
            (unless (org-at-heading-p)
              (org-back-to-heading t))
            (let* ((cache-file (gptel-org-cache--get-location))
                   (collected (gptel-org-cache--collect-ancestor-content))
                   (files (plist-get collected :files))
                   (headings (plist-get collected :headings))
                   (tags (gptel-org-cache--get-heading-tags))
                   (preferred-type (gptel-org-cache--preferred-cache-type tags)))
              (when cache-file
                ;; Try current heading first, then walk up parents
                (gptel-org-cache--find-valid-cache
                 cache-file headings files preferred-type))))
        (error nil)))))

(defun gptel-org-cache--get-heading-tags ()
  "Get tags for current heading and ancestors.

Returns a list of all tags found."
  (save-excursion
    (let ((tags nil))
      (org-back-to-heading t)
      (setq tags (append tags (org-get-tags)))
      (while (org-up-heading-safe)
        (setq tags (append tags (org-get-tags))))
      (delete-dups tags))))

(defun gptel-org-cache--preferred-cache-type (tags)
  "Determine preferred cache type based on TAGS.

Returns \\='summary, \\='files, or nil (no preference)."
  (cond
   ((member gptel-org-cache-summary-tag tags) 'summary)
   ((member gptel-org-cache-files-tag tags) 'files)
   (t nil)))

(defun gptel-org-cache--cached-files-changed-p (hash-alist)
  "Check if any files in HASH-ALIST have changed.

Unlike `gptel-org-cache--files-changed-p', this only checks files
that are IN the cache, not files that might be missing from it.
This is useful for parent cache inheritance where child headings
may have additional file links.

HASH-ALIST is ((FILE . HASH) ...).
Returns list of changed files, or nil if all unchanged."
  (let (changed)
    (dolist (file-hash hash-alist)
      (let* ((file (car file-hash))
             (stored-hash (cdr file-hash))
             (current-hash (gptel-org-cache--file-hash file)))
        (unless (equal stored-hash current-hash)
          (push file changed))))
    (nreverse changed)))

(defun gptel-org-cache--find-valid-cache (cache-file headings files &optional preferred-type)
  "Find valid cache for HEADINGS or any parent heading path.

CACHE-FILE is the cache file to search.
HEADINGS is the full heading path from root to current.
FILES is the list of files extracted from the heading tree.
PREFERRED-TYPE is \\='summary, \\='files, or nil.

When PREFERRED-TYPE is specified, only that cache type is considered.
When nil, tries summary first, then files.

Returns the cached content string, or nil if no valid cache found."
  (let ((current-headings headings)
        (is-exact-heading t)
        result)
    ;; Try progressively shorter heading paths (current, then parents)
    (while (and current-headings (not result))
      ;; Determine which cache types to try based on preference
      (let ((types-to-try (pcase preferred-type
                            ('summary '(summary))
                            ('files '(files))
                            (_ '(summary files)))))  ; Default: prefer summary
        (dolist (cache-type types-to-try)
          (unless result
            (let* ((heading-id (gptel-org-cache--heading-id current-headings cache-type))
                   (entry (gptel-org-cache--find-entry cache-file heading-id cache-type)))
              (when entry
                (let* ((stored-hashes (plist-get entry :file-hashes))
                       ;; For exact heading, check all files; for parents, only cached files
                       (changed (if is-exact-heading
                                    (gptel-org-cache--files-changed-p files stored-hashes)
                                  (gptel-org-cache--cached-files-changed-p stored-hashes))))
                  (if changed
                      ;; Cache is stale - only auto-update if this is the exact heading
                      (when (and gptel-org-cache-auto-update is-exact-heading)
                        (pcase cache-type
                          ('summary (gptel-org-cache-prepare-summary))
                          ('files (gptel-org-cache-prepare-files)))
                        (when-let* ((new-entry
                                     (gptel-org-cache--find-entry cache-file heading-id cache-type)))
                          (setq result (plist-get new-entry :content))))
                    ;; Cache is valid
                    (setq result (plist-get entry :content)))))))))
      ;; Move up to parent heading path
      (setq current-headings (butlast current-headings))
      (setq is-exact-heading nil))
    result))


;;; Integration with gptel context

(defun gptel-org-cache--transform-inject (fsm)
  "Transform function to inject cached context into requests.

FSM is the request state machine.  This function retrieves cached
context from the source buffer (where the request originated) and
prepends it to the system message.

This is designed to be added to `gptel-prompt-transform-functions'."
  (when gptel-org-cache-enabled
    (when-let* ((info (gptel-fsm-info fsm))
                (source-buffer (plist-get info :buffer))
                (cached-context
                 (with-current-buffer source-buffer
                   (gptel-org-cache-get-context))))
      ;; Inject cached context into system message
      (cl-etypecase gptel--system-message
        (string
         (setq gptel--system-message
               (concat cached-context "\n\n" gptel--system-message)))
        (list
         (setcar gptel--system-message
                 (concat cached-context "\n\n" (car gptel--system-message))))
        (null
         (setq gptel--system-message cached-context))))))

;; Legacy advice-based injection (kept for backward compatibility)
(defun gptel-org-cache--inject-context (orig-fun &rest args)
  "Advice to inject cached context when available.

ORIG-FUN is the original context function, ARGS are its arguments.
Note: This advice-based approach may not work correctly when called
from a non-org buffer.  Prefer the transform-based approach via
`gptel-org-cache-enable'."
  (if-let* ((cached (gptel-org-cache-get-context)))
      ;; Prepend cached context to normal context
      (let ((normal-context (apply orig-fun args)))
        (if normal-context
            (concat cached "\n\n" normal-context)
          cached))
    (apply orig-fun args)))

;;;###autoload
(defun gptel-org-cache-enable ()
  "Enable context caching integration with gptel.

This adds a transform function to `gptel-prompt-transform-functions'
that injects cached context from org files into requests."
  (interactive)
  (add-to-list 'gptel-prompt-transform-functions
               #'gptel-org-cache--transform-inject)
  (setq gptel-org-cache-enabled t)
  (message "gptel context caching enabled."))

;;;###autoload
(defun gptel-org-cache-disable ()
  "Disable context caching integration with gptel."
  (interactive)
  (setq gptel-prompt-transform-functions
        (delq #'gptel-org-cache--transform-inject
              gptel-prompt-transform-functions))
  (setq gptel-org-cache-enabled nil)
  (message "gptel context caching disabled."))


;;; Gitignore helper

;;;###autoload
(defun gptel-org-cache-add-to-gitignore ()
  "Add cache file pattern to .gitignore in the current directory."
  (interactive)
  (let ((gitignore (expand-file-name ".gitignore" default-directory))
        (pattern "*-ai-cache.org"))
    (with-current-buffer (find-file-noselect gitignore)
      (goto-char (point-min))
      (if (search-forward pattern nil t)
          (message "Pattern already in .gitignore")
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert "# gptel context cache files\n")
        (insert pattern "\n")
        (save-buffer)
        (message "Added %s to .gitignore" pattern)))))

(provide 'gptel-org-cache)
;;; gptel-org-cache.el ends here
