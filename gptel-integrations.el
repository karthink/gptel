;;; gptel-transient.el --- Integrations for gptel  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
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

;; Integrations with related packages for gptel.  To use these, run
;;
;; (require 'gptel-integrations)
;;
;; For MCP integration:
;; - Run M-x `gptel-mcp-connect' and M-x `gptel-mcp-disconnect', OR
;; - Use gptel's tools menu, M-x `gptel-tools', OR
;; - Access tools from `gptel-menu'

;;; Code:
(require 'gptel)
(require 'cl-lib)
(eval-when-compile (require 'transient))

;;;; MCP integration - requires the mcp package
(declare-function mcp-hub-get-all-tool "mcp-hub")
(declare-function mcp-hub-get-servers "mcp-hub")
(declare-function mcp-hub "mcp-hub")
(defvar mcp-hub-servers)

(defun gptel-mcp-connect (&optional interactive)
  "Get gptel tools from MCP servers using the mcp package.

If INTERACTIVE is non-nil, guide the user through setting up mcp and
query for servers to retrieve tools from."
  (interactive (list t))
  (if (locate-library "mcp-hub")
      (unless (require 'mcp-hub nil t)
        (user-error "Could not load `mcp-hub'!  Please install\
 or configure the mcp package"))
    (user-error "Could not find mcp!  Please install or configure the mcp package"))
  (if (null mcp-hub-servers)
      (user-error "No MCP servers available!  Please configure `mcp-hub-servers'")
    (let* ((servers (mcp-hub-get-servers))
           (active (cl-remove-if-not (lambda (el) (eq (plist-get el :status) 'connected))
                                     servers)))
      (if (null active)
          (when (and interactive
                     (y-or-n-p "No MCP servers are running.  Open the MCP hub?"))
            (message (substitute-command-keys
                      "Start some MCP servers for gptel to connect to!\
 (\\`s' to start, \\`k' to kill, \\[mcp-hub] to get here)"))
            (mcp-hub))
        ;; Check which servers to connect to
        (letrec ((tools (mcp-hub-get-all-tool :asyncp t :categoryp t))
                 (connect-all-fn
                  (lambda () (mapc #'(lambda (tool) (apply #'gptel-make-tool tool))
                              tools)
                    (message "Added %d tools from %d MCP server%s"
                             (length tools) (length active)
                             (if (= (length active) 1) "" "s")))))
          (if (not interactive)
              (funcall connect-all-fn)  ; Connect to all of them
            (when-let* ((names (completing-read-multiple ; Ask for confirmation
                                "Get tools from servers (separate with \",\"): "
                                (cons "ALL" (mapcar (lambda (el) (plist-get el :name)) active))
                                nil t)))
              (if (member "ALL" names)
                  (funcall connect-all-fn)
                (let ((idx 0))
                  (dolist (name names)
                    (mapc (lambda (tool)
                            (when (equal (plist-get tool :category) (format "mcp-%s" name))
                              (apply #'gptel-make-tool tool)
                              (cl-incf idx)))
                          tools))
                  (message "Added %d tools from MCP servers: %S"
                           idx names))))))))))

(defun gptel-mcp-disconnect (&optional interactive)
  "Unregister gptel tools provided by MCP servers using the mcp package.

If INTERACTIVE is non-nil, query the user about which tools to remove."
  (interactive (list t))
  (if-let* ((names-alist
             (cl-loop
              for (category . _tools) in gptel--known-tools
              if (string-match "^mcp-\\(.*\\)" category)
              collect (cons (match-string 1 category) category))))
      (let ((remove-fn (lambda (cat-names)
                         (mapc (lambda (category) (setf (alist-get category gptel--known-tools
                                                              nil t #'equal)
                                                   nil))
                               cat-names))))
        (if interactive
            (when-let* ((categories
                         (completing-read-multiple
                          "Remove MCP server tools for (separate with \",\"): "
                          (cons '("ALL" . nil) names-alist)
                          nil t)))
              (if (member "ALL" categories)
                  (setq categories (map-values names-alist))
                (setq categories (mapcar (lambda (n) (cdr (assoc n names-alist))) categories)))
              (funcall remove-fn categories)
              (message "Removed MCP tools for: %S" (map-keys names-alist)))
          (funcall remove-fn (map-values names-alist))))
    (message "No MCP tools found!")))

(with-eval-after-load 'gptel-transient
  ;; FIXME: If `gptel-mcp-connect' opens mcp-hub, the transient stays open.  I
  ;; don't know how to fix this.
  (transient-define-suffix gptel--suffix-mcp-connect ()
    "Register tools provided by MCP servers."
    :key "M+"
    :description "Add tools from MCP servers"
    :transient t
    (interactive)
    (condition-case err
        (call-interactively #'gptel-mcp-connect)
      (user-error (message "%s" (cadr err))))
    (transient-setup))

  (transient-define-suffix gptel--suffix-mcp-disconnect ()
    "Remove tools provided by MCP servers from gptel."
    :key "M-"
    :description "Remove tools from MCP servers"
    :transient t
    :inapt-if (lambda () (or (not (boundp 'mcp-hub-servers))
                        (null mcp-hub-servers)))
    (interactive)
    (call-interactively #'gptel-mcp-disconnect)
    (transient-setup))

  (transient-remove-suffix 'gptel-tools '(0 2))
  (transient-append-suffix 'gptel-tools '(0 -1)
    [""
     (gptel--suffix-mcp-connect)
     (gptel--suffix-mcp-disconnect)]))

(provide 'gptel-integrations)
;;; gptel-integrations.el ends here
