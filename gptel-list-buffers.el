;;; gptel-list-buffers.el --- Provide a buffer list of gptel buffers

;; Copyright (C) 2025  Karthik Chikmagalur

;; Author: Brett Hutley <brett@hutley.net>
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

;; This creates a buffer containing a list of gptel buffers, with the
;; ability to kill or go to a buffer via keystrokes.
;;
;; Kinda based on the functionality of the inbuilt `list-buffers`
;; function

;; Keystrokes when buffer is active:
;;
;;  d - Mark buffer for deletion
;;  f - Visit the selected buffer
;;  x - Delete all marked buffers
;;  q - Quit the window

;;; Code:

;;; -*- lexical-binding: t -*-
(require 'gptel)

(defvar gptel-buffer-list--marked-buffers
  (make-hash-table :test 'equal)
  "Hash table of buffers (by name) currently marked for deletion.")

(define-derived-mode gptel-buffer-list-mode tabulated-list-mode "gptel Buffers"
  "Major mode for listing gptel buffers.

Keys:
\[gptel-buffer-list-mark-for-deletion] Mark buffer for deletion
\[gptel-buffer-list-unmark] Unmark buffer for deletion
\[gptel-buffer-list-unmark-all] Unmark all buffers marked for deletion
\[gptel-buffer-list-execute-deletions] Delete all marked buffers
\[gptel-buffer-list-visit-buffer] Visit the selected buffer
\[gptel-buffer-list-refresh] Refresh the buffer list
\[quit-window] Quit the window"
  :group 'gptel

  (setq tabulated-list-format [ ("M" 2 nil)
                                ("Buffer" 30 t)
                                ("Backend" 20 t)
                                ("Model" 20 t)
                                ("Temperature" 4 t)])

  (setq tabulated-list-padding 2)

  (use-local-map (copy-keymap tabulated-list-mode-map))
  ;; Define keybindings:
  (define-key (current-local-map) (kbd "d") #'gptel-buffer-list-mark-for-deletion)
  (define-key (current-local-map) (kbd "u") #'gptel-buffer-list-unmark)
  (define-key (current-local-map) (kbd "U") #'gptel-buffer-list-unmark-all)
  (define-key (current-local-map) (kbd "x") #'gptel-buffer-list-execute-deletions)
  (define-key (current-local-map) (kbd "f") #'gptel-buffer-list-visit-buffer)
  (define-key (current-local-map) (kbd "<return>") #'gptel-buffer-list-visit-buffer)
  (define-key (current-local-map) (kbd "g") #'gptel-buffer-list-refresh)
  (define-key (current-local-map) (kbd "q") #'quit-window)

  (tabulated-list-init-header))

(defun gptel-list-buffers ()
  "Display a list of gptel buffers."
  (interactive)
  (let ((gptel-buffers
         (seq-filter (lambda (buf)
                       (with-current-buffer buf
                         (and (boundp 'gptel-mode) gptel-mode)))
                     (buffer-list))))
    (with-current-buffer (get-buffer-create "*gptel Buffers*")
      (gptel-buffer-list-mode)
      (setq tabulated-list-entries
            (mapcar (lambda (buf)
                      (let ((bname (buffer-name buf)))
                        (with-current-buffer buf
                          (list bname
                                (vector
                                 (if (gethash bname gptel-buffer-list--marked-buffers) "D" " ")
                                 bname
				 (gptel-backend-name gptel-backend)
                                 (if (and (boundp 'gptel-model) gptel-model)
                                     (format "%s" gptel-model)
                                   "N/A")
                                 (if (and (boundp 'gptel-temperature) (numberp gptel-temperature))
                                     (format "%.1f" gptel-temperature)
                                   "N/A"))))))
                    gptel-buffers))
      (tabulated-list-print)
      (switch-to-buffer (current-buffer)))))

(defun gptel-buffer-list-mark-for-deletion ()
  "Mark the current line's buffer for deletion."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (entry (assoc id tabulated-list-entries)))
    (when entry
      (puthash id t gptel-buffer-list--marked-buffers)
      (let ((row (cadr entry)))
        (aset row 0 "D") ;; Mark column is index 0 in the vector
        (tabulated-list-print t)))))

(defun gptel-buffer-list-unmark ()
  "Unmark the current line's buffer."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (entry (assoc id tabulated-list-entries)))
    (when entry
      (remhash id gptel-buffer-list--marked-buffers)
      (let ((row (cadr entry)))
        (aset row 0 " ")
        (tabulated-list-print t)))))

(defun gptel-buffer-list-unmark-all ()
  "Unmark all buffers marked for deletion."
  (interactive)
  (clrhash gptel-buffer-list--marked-buffers)
  (gptel-buffer-list-refresh))

(defun gptel-buffer-list-execute-deletions ()
  "Delete all buffers that have been marked for deletion."
  (interactive)
  (dolist (entry tabulated-list-entries)
    (let ((id (car entry))
          (row (cadr entry)))
      (when (string= (aref row 0) "D")
        (when (get-buffer id)
          (kill-buffer id)
	  (remhash id gptel-buffer-list--marked-buffers)))))
  ;; Refresh the buffer list after deletions
  (gptel-list-buffers))

(defun gptel-buffer-list-visit-buffer ()
  "Open (switch to) the buffer under point."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when (get-buffer id)
      (switch-to-buffer id))))

(defun gptel-buffer-list-refresh ()
  "Refresh the gptel Buffers list."
  (interactive)
  (gptel-list-buffers))

(provide 'gptel-list-buffers)
;;; gptel-list-buffers.el ends here

