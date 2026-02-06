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
;; Key features:
;; - Extract file links from heading trees (parent headings and ancestors)
;; - Hash-based cache invalidation (detect when files change)
;; - Per-heading cache storage
;; - Integration with gptel context injection
;;
;; Usage:
;;   M-x gptel-org-cache-prepare  - Build/update cache for current heading
;;   M-x gptel-org-cache-invalidate - Force cache rebuild
;;   M-x gptel-org-cache-status - Show cache status

;;; Code:

(require 'org)
(require 'org-element)
(eval-when-compile (require 'cl-lib))

(declare-function gptel-context--string "gptel-context")
(declare-function gptel-context--insert-file-string "gptel-context")

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

(defun gptel-org-cache--heading-id (headings)
  "Generate a unique ID for cache entry from HEADINGS list."
  (md5 (mapconcat #'identity headings "/")))

(defun gptel-org-cache--format-entry (heading-id data)
  "Format a cache entry for HEADING-ID with DATA.

DATA is a plist with :content, :files, :headings, :file-hashes."
  (let ((headings (plist-get data :headings))
        (files (plist-get data :files))
        (file-hashes (plist-get data :file-hashes))
        (content (plist-get data :content)))
    (concat
     (format "* %s\n" (car (last headings)))
     ":PROPERTIES:\n"
     (format ":CACHE_ID: %s\n" heading-id)
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
     "** Cached Context\n"
     "#+begin_example\n"
     content
     "\n#+end_example\n\n")))


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

(defun gptel-org-cache--find-entry (cache-file heading-id)
  "Find cache entry for HEADING-ID in CACHE-FILE.

Returns a plist with :beg, :end, :file-hashes, :content, or nil if not found."
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
                 file-hashes content)
            ;; Extract file hashes
            (save-excursion
              (when (re-search-forward "^\\*\\* File Hashes" end t)
                (when (re-search-forward "#\\+begin_src elisp\n\\(\\(?:.*\n\\)*?\\)#\\+end_src" end t)
                  (setq file-hashes (ignore-errors (read (match-string 1)))))))
            ;; Extract cached context
            (save-excursion
              (when (re-search-forward "^\\*\\* Cached Context" end t)
                (when (re-search-forward "#\\+begin_example\n\\(\\(?:.*\n\\)*?\\)#\\+end_example" end t)
                  (setq content (match-string 1)))))
            (list :beg beg :end end :file-hashes file-hashes :content content)))))))

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

(defun gptel-org-cache--delete-entry (cache-file heading-id)
  "Delete cache entry for HEADING-ID from CACHE-FILE."
  (when-let* ((existing (gptel-org-cache--find-entry cache-file heading-id)))
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
        (let ((start (point)))
          (insert-file-contents file)
          (goto-char (point-max)))
        (unless (bolp) (insert "\n"))
        (insert "```\n\n")))
    (buffer-string)))


;;; Main commands

;;;###autoload
(defun gptel-org-cache-prepare ()
  "Build or update cache for the current heading's context.

This extracts file links from the current heading and its ancestors,
reads the file contents, and stores them in the cache file for faster
access in future sessions."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "This command only works in Org mode"))
  (unless (org-at-heading-p)
    (org-back-to-heading t))
  (let* ((cache-file (gptel-org-cache--ensure-cache-file))
         (collected (gptel-org-cache--collect-ancestor-content))
         (files (plist-get collected :files))
         (headings (plist-get collected :headings))
         (heading-id (gptel-org-cache--heading-id headings)))
    (if (null files)
        (message "No file links found in heading tree.")
      (let* ((file-hashes (mapcar (lambda (f) (cons f (gptel-org-cache--file-hash f))) files))
             (context (gptel-org-cache--generate-context files))
             (data (list :content context
                         :files files
                         :headings headings
                         :file-hashes file-hashes)))
        (gptel-org-cache--write-entry cache-file heading-id data)
        (message "Cached %d file(s) for heading: %s"
                 (length files) (car (last headings)))))))

;;;###autoload
(defun gptel-org-cache-invalidate ()
  "Force rebuild of cache for the current heading."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "This command only works in Org mode"))
  (unless (org-at-heading-p)
    (org-back-to-heading t))
  (let* ((cache-file (gptel-org-cache--get-location))
         (headings (plist-get (gptel-org-cache--collect-ancestor-content) :headings))
         (heading-id (gptel-org-cache--heading-id headings)))
    (when (and cache-file (file-exists-p cache-file))
      (gptel-org-cache--delete-entry cache-file heading-id)
      (message "Cache invalidated for: %s" (car (last headings)))))
  ;; Rebuild
  (gptel-org-cache-prepare))

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
         (heading-id (gptel-org-cache--heading-id headings))
         (entry (when cache-file (gptel-org-cache--find-entry cache-file heading-id))))
    (if (null entry)
        (message "No cache entry for: %s\nFiles in tree: %d"
                 (car (last headings)) (length files))
      (let* ((stored-hashes (plist-get entry :file-hashes))
             (changed (gptel-org-cache--files-changed-p files stored-hashes)))
        (if changed
            (message "Cache STALE for: %s\nChanged files: %s"
                     (car (last headings))
                     (mapconcat #'abbreviate-file-name changed ", "))
          (message "Cache VALID for: %s\nCached files: %d"
                   (car (last headings))
                   (length stored-hashes)))))))

;;;###autoload
(defun gptel-org-cache-get-context ()
  "Get cached context for the current heading, if available and valid.

Returns the cached context string, or nil if cache is missing or stale.
When `gptel-org-cache-auto-update' is non-nil, automatically rebuilds
stale cache entries."
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
                   (heading-id (gptel-org-cache--heading-id headings))
                   (entry (when cache-file
                            (gptel-org-cache--find-entry cache-file heading-id))))
              (when entry
                (let* ((stored-hashes (plist-get entry :file-hashes))
                       (changed (gptel-org-cache--files-changed-p files stored-hashes)))
                  (if changed
                      ;; Cache is stale
                      (when gptel-org-cache-auto-update
                        (gptel-org-cache-prepare)
                        ;; Re-read after update
                        (when-let* ((new-entry
                                     (gptel-org-cache--find-entry cache-file heading-id)))
                          (plist-get new-entry :content)))
                    ;; Cache is valid
                    (plist-get entry :content))))))
        (error nil)))))


;;; Integration with gptel context

(defun gptel-org-cache--inject-context (orig-fun &rest args)
  "Advice to inject cached context when available.

ORIG-FUN is the original context function, ARGS are its arguments."
  (if-let* ((cached (gptel-org-cache-get-context)))
      ;; Prepend cached context to normal context
      (let ((normal-context (apply orig-fun args)))
        (if normal-context
            (concat cached "\n\n" normal-context)
          cached))
    (apply orig-fun args)))

;;;###autoload
(defun gptel-org-cache-enable ()
  "Enable context caching integration with gptel."
  (interactive)
  (advice-add 'gptel-context--string :around #'gptel-org-cache--inject-context)
  (setq gptel-org-cache-enabled t)
  (message "gptel context caching enabled."))

;;;###autoload
(defun gptel-org-cache-disable ()
  "Disable context caching integration with gptel."
  (interactive)
  (advice-remove 'gptel-context--string #'gptel-org-cache--inject-context)
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
