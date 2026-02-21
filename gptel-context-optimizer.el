;;; gptel-context-optimizer.el --- Context optimizer for gptel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Red Hat Inc.

;; Keywords: context, LRU, compilation, preload

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

;; All the features in this file require gptel to register into
;; intrusive emacs hooks in order to apply various optimizations to
;; the gptel-context to reduce inference times. So it's an opt-in and
;; it can be enabled with: (require 'gptel-context-optimizer)

;; By default when enabled this keeps the last recently saved/reverted
;; file at the top of gptel-context. It registers in the
;; after-save-hook/after-revert-hook to do so.

;; *compilation* buffer added to gptel-context only for the next
;; gptel-request can be optionally enabled with:
;;
;;    (setq-default gptel-context-enable-compilation-auto-add t)

;; kvcache context preload will increase energy/cost of inference, but
;; it may decrease the wait time before generation, so it's disabled
;; by default.
;;
;; It can be toggled with:
;;
;;    (gptel-context-preload-mode):
;;
;; or enabled with:
;;
;;    (gptel-context-preload-mode 1)
;;
;; or disabled with:
;;
;;    (gptel-context-preload-mode 0)

;;; Code:

(require 'gptel-context)

(defun gptel-context--move-to-bottom ()
  "Move the current buffer's file to the bottom of the context.

This is a LRU optimization to reduce kvcache invalidation: the last file
that was saved or reverted is statistically the most likely to be
change again in a subsequent edit."
  (let ((curbuf (buffer-file-name)))
    (when curbuf
      (let ((entry (gptel-context--get-path curbuf)))
        (when entry
          (unless (equal entry (car gptel-context))
            (setq gptel-context (cl-delete entry gptel-context :test 'equal))
            (push entry gptel-context)))))))
(add-hook 'after-save-hook #'gptel-context--move-to-bottom)
(add-hook 'after-revert-hook #'gptel-context--move-to-bottom)


(defcustom gptel-context-enable-compilation-auto-add nil
  "Whether to automatically add the compilation buffer to gptel context.

When non-nil, the compilation buffer is added to the bottom of the
gptel's context whenever the compilation filter runs. This allows the
last compilation output to be always in context of the next gptel
requests."
  :group 'gptel
  :type 'boolean)

(defun gptel-context--compilation-filter ()
  "Add gptel context when compilation filter runs."
  (when (and gptel-context-enable-compilation-auto-add
             (equal (buffer-name) "*compilation*"))
    (let ((curbuf (current-buffer)))
      (unless (equal curbuf (car gptel-context))
        (setq gptel-context (cl-delete curbuf gptel-context :test 'equal))
        (push curbuf gptel-context)))))
(add-hook 'compilation-filter-hook #'gptel-context--compilation-filter)

(defun gptel-context--compilation-remove ()
  "Remove gptel context after the first gptel-request."
  (when gptel-context-enable-compilation-auto-add
    (let ((buffer (get-buffer "*compilation*")))
      (when buffer
        (setq gptel-context
              (cl-delete buffer gptel-context :test 'equal))))))
(add-hook 'gptel-post-request-hook #'gptel-context--compilation-remove)

(defun gptel-context-preload--set-timer ()
  "Set the preload timer."
  (setq gptel-context-preload--timer
        (run-at-time gptel-context-preload-delay nil 'gptel-context-preload--send-message)))

(defun gptel-context-preload ()
  "Preload gptel context by sending a test message after a delay.
This function is designed to be added to save and revert hooks.
It cancels any pending timer, sets up a new timer,
and when the timer fires, sends a test message to gptel."
  ;; Cancel any existing timer
  (let ((curbuf (buffer-file-name)))
    (when curbuf
      (let ((entry (gptel-context--get-path curbuf)))
        (when entry
          ;; (message "gptel-context-preload pending: %s" current-buffer-truename)
          (gptel-context-preload--cancel)
          ;; Set up new timer with configurable delay
          (gptel-context-preload--set-timer))))))

(defun gptel-context-preload--send-message ()
  "Send a test message to gptel in a dedicated buffer."
  ;; Create or clear the buffer
  (unless gptel--request-alist
    (with-current-buffer (get-buffer-create "*gptel-context-preload*")
      (setq-local gptel-use-context 'system)
      ;; (message "gptel-context-preload")
      (read-only-mode -1)
      ;; Clear buffer contents
      (erase-buffer)
      ;; Insert the test message
      (insert "write the word yes")
      ;; Send the message
      (gptel-send)
      ;; cancel the preload if another gptel-request is sent
      (add-hook 'gptel-prompt-transform-functions #'gptel-context-preload--cancel))))

(defun gptel-context-preload-mode (&optional arg)
  (interactive)
  (gptel-context-preload--cancel)
  (setq gptel-context-preload--enabled (not gptel-context-preload--enabled))
  (when arg
    (setq gptel-context-preload--enabled (eq arg 1)))
  (message "gptel-context-preload-mode %s" (if (eq gptel-context-preload--enabled t)
                                              "enabled" "disabled"))
  (when gptel-context-preload--enabled
    (gptel-context-preload--set-timer)))

(defun gptel-context-preload--cancel ()
  (remove-hook 'gptel-prompt-transform-functions #'gptel-context-preload--cancel)
  (when gptel-context-preload--timer
    (cancel-timer gptel-context-preload--timer)
    (setq gptel-context-preload--timer nil))
  (gptel-abort (get-buffer "*gptel-context-preload*")))

;; Global variable to hold the timer
(defvar gptel-context-preload--timer nil
  "Timer for gptel context preloading.")

(defvar gptel-context-preload--enabled nil
  "gptel context preload enabled.")

(defvar gptel-context-preload-delay 2
  "Delay in seconds before sending the preload message.")

;; Add to save and revert hooks
(add-hook 'after-save-hook #'gptel-context-preload)
(add-hook 'after-revert-hook #'gptel-context-preload)

(provide 'gptel-context-optimizer)
;;; gptel-context-optimizer.el ends here.
