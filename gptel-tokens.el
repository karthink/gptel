;;; gptel-tokens.el --- Token estimation for gptel  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Keywords: convenience

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

;; Token estimation for gptel requests.  Provides a coarse estimate
;; of how many tokens a gptel-send request would use, based on
;; system prompt, buffer content, context, and tools.

;;; Code:

(require 'gptel-request)
(require 'cl-lib)
(require 'sha1)

(defvar-local gptel--token-estimate-cache nil
  "Cached token estimate data.

Plist with keys:
- :tokens - estimated token count
- :timestamp - when estimate was calculated
- :hash-system - SHA1 hash of system prompt
- :system-tokens - cached token count for system prompt
- :hash-tools - SHA1 hash of tools
- :tools-tokens - cached token count for tools
- :buffer-tick - `buffer-modified-tick'
- :buffer-tokens - cached token count for buffer content
- :region-active - whether region was active during estimation
- :region-hash - hash of region content (if active)
- :file-mtimes - plist mapping file paths to modification times")

;;; Helper functions
(defun gptel--estimate-tokens-from-string (str)
  "Estimate token count from string STR using words/2.5 heuristic."
  (if (or (not str) (string-empty-p str))
      0
    (with-temp-buffer
      (insert str)
      (/ (count-words (point-min) (point-max)) 2.5))))

(defun gptel--estimate-tokens-from-words (num)
  "Estimate token count from NUM words."
  (or (and (numberp num) (/ num 2.5)) 0))

(defun gptel--sha1 (obj)
  "Compute SHA1 hash of OBJ for caching.

OBJ can be a string, list, or other Lisp object."
  (sha1 (prin1-to-string obj)))

;;; System prompt estimation

(defun gptel--estimate-system-tokens (&optional use-cache-p)
  "Estimate tokens in system prompt.

If USE-CACHE-P is non-nil, return cached value if available.

System prompt can be a string, list, or function.  If a function,
it is evaluated and the result is used.  Also checks for
GPTEL_SYSTEM Org property."
  (if (not gptel--system-message)
      0
    (let* ((directive gptel--system-message)
           (hash (gptel--sha1 directive))
           (cached-hash (plist-get gptel--token-estimate-cache :hash-system))
           (cached-tokens (plist-get gptel--token-estimate-cache :system-tokens)))
      ;; Return cached value if available and using cache
      (if (and use-cache-p (equal hash cached-hash))
          (or cached-tokens 0)
        ;; Calculate fresh estimate
        (let* ((parsed (car-safe (gptel--parse-directive directive 'raw)))
               (tokens (gptel--estimate-tokens-from-string parsed)))
          (plist-put gptel--token-estimate-cache :hash-system hash)
          (plist-put gptel--token-estimate-cache :system-tokens tokens)
          tokens)))))

;;; Tools estimation

(defun gptel--estimate-tools-tokens (&optional use-cache-p)
  "Estimate tokens for tool definitions.

Only estimates if `gptel-use-tools' is non-nil.
If USE-CACHE-P is non-nil, return cached value if available."
  (if (not (and gptel-use-tools gptel-tools))
      0
    (let* ((tools gptel-tools)
           (hash (gptel--sha1 tools))
           (cached-hash (plist-get gptel--token-estimate-cache :hash-tools))
           (cached-tokens (plist-get gptel--token-estimate-cache :tools-tokens)))
      ;; Return cached value if available and using cache
      (if (and use-cache-p (equal hash cached-hash))
          (or cached-tokens 0)
        ;; Calculate fresh estimate
        (let ((token-count 0))
          (dolist (tool tools)
            (setq token-count (+ token-count
                                 (gptel--estimate-tokens-from-string
                                  (gptel-tool-description tool))
                                 (gptel--estimate-tokens-from-string
                                  (prin1-to-string (gptel-tool-args tool))))))
          (plist-put gptel--token-estimate-cache :hash-tools hash)
          (plist-put gptel--token-estimate-cache :tools-tokens token-count)
          token-count)))))

;;; Buffer text estimation

(defun gptel--estimate-buffer-tokens (&optional use-cache-p)
  "Estimate tokens from current buffer content.

Considers `gptel-org-branching-context' if in Org mode.
If USE-CACHE-P is non-nil, return cached value if available.

IMPORTANT: Also tracks whether a region is active, since
deactivating a region changes what will be sent even though
`buffer-modified-tick' doesn't change."
  (let* ((tick (buffer-modified-tick))
         (region-active-p (use-region-p))
         (region-hash (when region-active-p
                        (gptel--sha1
                         (buffer-substring-no-properties
                          (region-beginning) (region-end)))))
         (cached-tick (plist-get gptel--token-estimate-cache :buffer-tick))
         (cached-region (plist-get gptel--token-estimate-cache :region-active))
         (cached-hash (plist-get gptel--token-estimate-cache :region-hash))
         (cached-tokens (plist-get gptel--token-estimate-cache :buffer-tokens))
         (needs-update (or (not use-cache-p)
                           (not (= tick cached-tick))
                           (not (eq region-active-p cached-region))
                           (and region-active-p
                                (not (equal region-hash cached-hash))))))

    ;; Return cached value if available and using cache
    (if (and use-cache-p (not needs-update))
        (or cached-tokens 0)

      ;; Calculate fresh estimate
      (let ((prompt-buffer (gptel--create-prompt-buffer))
            (tokens 0))
        (with-current-buffer prompt-buffer
          ;; (let* ((prompts (gptel--parse-buffer
          ;;                  gptel-backend
          ;;                  (and gptel--num-messages-to-send
          ;;                       (* 2 gptel--num-messages-to-send))))
          ;;        (tokens 0))
          ;;   (dolist (prompt prompts)
          ;;     (setq tokens (+ tokens
          ;;                   (gptel--estimate-tokens-from-string
          ;;                    (or (plist-get prompt :content)
          ;;                        (plist-get prompt :text)))))))
          (setq tokens (gptel--estimate-tokens-from-words
                        (count-words (point-min) (point-max))))
          (kill-buffer prompt-buffer))
        (plist-put gptel--token-estimate-cache :buffer-tick tick)
        (plist-put gptel--token-estimate-cache :region-active region-active-p)
        (plist-put gptel--token-estimate-cache :region-hash region-hash)
        (plist-put gptel--token-estimate-cache :buffer-tokens tokens)
        tokens))))

;;; Context estimation

(declare-function gptel-context--collect "gptel-context")
(defun gptel--estimate-context-tokens (&optional use-cache-p)
  "Estimate tokens from `gptel-context'.

Only estimates if `gptel-use-context' is non-nil.
Returns estimated token count and updates file mtime cache.
If USE-CACHE-P is non-nil, use cached modification times."
  (if (not (and gptel-use-context gptel-context))
      0
    (let* ((contexts (gptel-context--collect))
           (file-mtimes (plist-get gptel--token-estimate-cache :file-mtimes))
           (total-tokens 0)
           (new-mtimes (or file-mtimes nil)))

      (dolist (ctx contexts)
        (pcase ctx
          ((and `(,buf . ,data) (pred (lambda (b) (bufferp (car b)))))
           ;; Buffer context
           (when (buffer-live-p buf)
             (dolist (ov (plist-get data :overlays))
               (with-current-buffer buf
                 (setq total-tokens
                       (+ total-tokens
                          (gptel--estimate-tokens-from-words
                           (count-words
                            (overlay-start ov) (overlay-end ov))))))))
           ;; Line ranges in buffers
           (dolist (line-spec (plist-get data :lines))
             (when (buffer-live-p buf)
               (save-excursion
                 (with-current-buffer buf
                   (goto-char (point-min))
                   (forward-line (1- (car line-spec)))
                   (let ((start (point)))
                     (forward-line (cdr line-spec))
                     (setq total-tokens
                           (+ total-tokens
                              (gptel--estimate-tokens-from-words
                               (count-words start (point))))))))))
           ;; Byte ranges in buffers
           (dolist (bound-spec (plist-get data :bounds))
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (setq total-tokens
                       (+ total-tokens
                          (gptel--estimate-tokens-from-words
                           (count-words
                            (car bound-spec) (cdr bound-spec)))))))))

          ;; File context
          ((and `(,path . ,_data) (pred (lambda (f) (stringp (car f)))))
           (when (file-readable-p path)
             (let* ((current-mtime (file-attribute-modification-time
                                    (file-attributes path)))
                    (cached-mtime (plist-get file-mtimes path))
                    (needs-update (or (not use-cache-p)
                                      (not (equal current-mtime cached-mtime)))))
               (when needs-update
                 ;; Use file size as heuristic for binary files
                 (if (gptel--file-binary-p path)
                     (let ((size-bytes (file-attribute-size
                                        (file-attributes path))))
                       (setq total-tokens (+ total-tokens (/ size-bytes 3))))
                   ;; Read text file and count words
                   (setq total-tokens
                         (+ total-tokens
                            (gptel--estimate-tokens-from-words
                             (with-temp-buffer
                               (insert-file-contents path)
                               (count-words (point-min) (point-max)))))))
                 (setq new-mtimes (plist-put new-mtimes path current-mtime))))))

          ;; Simple file string (no spec)
          ((pred stringp)
           (when (file-readable-p ctx)
             (let* ((current-mtime (file-attribute-modification-time
                                    (file-attributes ctx)))
                    (cached-mtime (plist-get file-mtimes ctx))
                    (needs-update (or (not use-cache-p)
                                      (not (equal current-mtime cached-mtime)))))
               (when needs-update
                 (if (gptel--file-binary-p ctx)
                     (let ((size-bytes (file-attribute-size
                                        (file-attributes ctx))))
                       (setq total-tokens (+ total-tokens (/ size-bytes 3))))
                   (setq total-tokens
                         (+ total-tokens
                            (gptel--estimate-tokens-from-words
                             (with-temp-buffer
                               (insert-file-contents ctx)
                               (count-words (point-min) (point-max)))))))
                 (setq new-mtimes (plist-put new-mtimes ctx current-mtime))))))))

      (plist-put gptel--token-estimate-cache :file-mtimes new-mtimes)
      total-tokens)))

;;; Main estimation function

(defun gptel--estimate-tokens (&optional force-recalculate)
  "Estimate total tokens for current gptel request.

Returns a plist with:
  :tokens - estimated token count (integer)
  :context-window - model's context window in tokens (integer or nil)
  :percentage - percentage of context window used (integer or nil)

If FORCE-RECALCULATE is non-nil, ignore cache and recalculate.

Handles sources of tokens:
1. System prompt (from `gptel-directives' or GPTEL_SYSTEM Org property)
2. In-buffer text (considering `gptel-org-branching-context')
3. Linked text/binary files from context
4. `gptel-context' (files, buffers, overlays) - only if `gptel-use-context'
5. Variable `gptel-tools' definitions - only if `gptel-use-tools'
6. Note: `gptel-prompt-transform-functions' cannot be estimated without
execution

Uses caching to avoid performance issues:
- System prompt and tools are SHA1-hashed
- Buffer text checked via `buffer-modified-tick' AND region state
- Context files cached by modification time"
  (unless gptel--token-estimate-cache
    (setq gptel--token-estimate-cache
          (list :hash-system nil :system-tokens nil
                :hash-tools nil :tools-tokens nil
                :buffer-tick 0 :buffer-tokens nil
                :region-active nil :region-hash nil
                :file-mtimes nil)))

  ;; Check if model has context window info
  (let* ((context-window-k (get gptel-model :context-window))
         (context-window (when context-window-k
                           (* context-window-k 1000))) ; Convert from k to tokens
         (use-cache-p (not force-recalculate))
         (total-tokens 0))

    ;; Estimate each source
    (setq total-tokens
          (+ (or (gptel--estimate-system-tokens use-cache-p) 0)
             (or (gptel--estimate-tools-tokens use-cache-p) 0)
             (or (gptel--estimate-buffer-tokens use-cache-p) 0)
             (or (gptel--estimate-context-tokens use-cache-p) 0)))

    ;; Calculate percentage if context window known
    (let ((percentage (when context-window
                        (floor (* 100 (/ (float total-tokens)
                                         (float context-window)))))))
      (list :tokens (floor total-tokens)
            :context-window context-window
            :percentage (min percentage 100)))))

(provide 'gptel-tokens)
;;; gptel-tokens.el ends here
