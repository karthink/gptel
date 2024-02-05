;;; gptel-complete.el --- complete text with an LLM  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Keywords: convenience, extensions

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

;; To use: open up any code buffer, move point to where you want code to be
;; completed, call `gptel-complete'.
;;
;; When the cursor is inside a response, more actions are available:
;; - Not happy with the response? Regenerate it with gptel-complete-regenerate (C-c M-RET),
;; - or delete it with gptel-complete-reject (C-c DEL).
;; - Ediff against previous responses with gptel-complete-ediff (C-c =)
;; - Mark the response with gptel-complete-mark (C-c SPC)
;; - Finalize the response with gptel-complete-accept (C-c RET)

;;; Code:
(require 'gptel)

;;;###autoload
(defun gptel-complete ()
  "Complete at point using gptel."
  (interactive)
  (gptel-complete--prepare-overlay)
  (let ((stream (and gptel-stream (gptel-backend-stream gptel-backend))))
    (gptel-request nil
      :stream stream
      :system (gptel-complete--system-message)
      :callback (and stream #'gptel-complete--stream-insert-response)
      :position (point-marker))))

(defvar-keymap gptel-complete--map
  :doc "Keymap for actions on gptel completions."
  "C-c C-g" #'gptel-abort
  "C-c =" #'gptel-complete-ediff
  "C-c DEL" #'gptel-complete-reject
  "C-c M-RET" #'gptel-complete-regenerate
  "C-c <return>" #'gptel-complete-accept
  "C-c SPC" #'gptel-complete-mark)

(defun gptel-complete--system-message ()
  ";TODO: "
  (concat
   (format
    "Complete the following %s code at point.  Return only code and (optionally) code comments, no explanations are needed.  Do not repeat code already present in the prompt."
    (replace-regexp-in-string "-mode$" "" (symbol-name major-mode)))
   ;; "For context, here are the contents of this file:\n\n"
   ;; (buffer-substring-no-properties (point-min) (point-max))
   ))

(defvar-local gptel-complete--overlay nil
  "Overlay for utility functions.")

(defun gptel-complete--prepare-overlay ()
  ";TODO: "
  (let ((ov))
    (if-let ((o (gptel-complete--overlay-at-pt)))
        (progn
          (setq ov o)
          (move-overlay ov (1- (point)) (point)))
      (setq ov (make-overlay (1- (point)) (point) nil t t)))
    (overlay-put ov 'gptel-overlay t)
    (overlay-put ov 'mouse-face 'highlight)
    (overlay-put ov 'keymap gptel-complete--map)
    (overlay-put ov 'help-echo
                 (concat
                  "\\[gptel-complete-regenerate]: Regenerate, \\[gptel-complete-ediff]: Ediff, "
                  "\\[gptel-complete-accept]: Accept, \\[gptel-complete-reject]: Reject, "
                  "\\[gptel-complete-mark]: Mark"))
    (overlay-put
     ov 'before-string (propertize "!" 'display `(left-fringe right-triangle fringe)))
    (overlay-put
     ov 'after-string (propertize "!" 'display `(left-fringe left-triangle fringe)))))

(defun gptel-complete--stream-insert-response (response info)
  "Insert streaming RESPONSE from an LLM into the gptel buffer.

INFO is a mutable plist containing information relevant to this
buffer."
  (let ((start-marker (plist-get info :position))
        (tracking-marker (plist-get info :tracking-marker))
        (transformer (plist-get info :transformer)))
    (when response
        (with-current-buffer (marker-buffer start-marker)
          (save-excursion
            (unless tracking-marker
              (gptel--update-status " Typing..." 'success)
              (goto-char start-marker)
              (setq tracking-marker (set-marker (make-marker) (point)))
              (set-marker-insertion-type tracking-marker t)
              (plist-put info :tracking-marker tracking-marker))

            (when transformer
              (setq response (funcall transformer response)))

            (add-text-properties
             0 (length response) '(gptel response rear-nonsticky t)
             response)
            (goto-char tracking-marker)
            ;; (run-hooks 'gptel-pre-stream-hook)
            (insert response)
            (run-hooks 'gptel-post-stream-hook))))))


;; Utility functions

(defun gptel-complete--overlay-at-pt ()
  "Find a gptel-complete overlay at point."
  (cdr-safe (get-char-property-and-overlay (point) 'gptel-overlay)))

(defun gptel-complete--bounds ()
  ";TODO: "
  (when-let ((ov (gptel-complete--overlay-at-pt)))
    (cons (overlay-start ov) (overlay-end ov))))


;; Response UI commands

(defun gptel-complete-reject ()
  "Reject gptel completion at point."
  (interactive)
  (when-let* ((bounds (gptel-complete--bounds))
              (start (car bounds))
              (end   (cdr bounds)))
    (kill-region start end)
    (message "Rejected response saved to kill-ring.")
    (delete-overlay gptel-complete--overlay)))

(defun gptel-complete-accept ()
  "Accept gptel completion at point."
  (interactive)
  (when-let ((ov (gptel-complete--overlay-at-pt)))
    (delete-overlay ov)))

(defun gptel-complete-regenerate ()
  "Regenerate this completion."
  (interactive)
  (when-let* ((bounds (gptel-complete--bounds))
              (start (car bounds))
              (end   (cdr bounds)))
    (gptel--attach-response-history
     (list (buffer-substring-no-properties start end)))
    (kill-region start end)
    (gptel-complete)))

(defun gptel-complete-mark ()
  "Mark gptel-completion at point."
  (interactive)
  (when-let* ((bounds (gptel-complete--bounds))
              (start (car bounds))
              (end   (cdr bounds)))
    (goto-char start) (push-mark)
    (goto-char end) (activate-mark)))

(defun gptel-complete-ediff (&optional arg)
  "Ediff gptel response at point against previous gptel responses.

If prefix ARG is non-nil, select the previous response to ediff
against interactively."
  (interactive "P")
  (gptel--ediff arg #'gptel-complete--bounds))

(provide 'gptel-complete)
;;; gptel-complete.el ends here
