;;; gptel-context-optimizer.el --- Context optimizer for gptel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Red Hat Inc.

;; Keywords: context, LRU

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

(provide 'gptel-context-optimizer)
;;; gptel-context-optimizer.el ends here.
