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
(declare-function mcp-hub-start-all-server "mcp-hub")
(declare-function mcp-stop-server "mcp")
(declare-function mcp-hub "mcp-hub")
(defvar mcp-hub-servers)
(defvar mcp-server-connections)

(defun gptel-mcp-connect (&optional interactive server-callback)
  "Add gptel tools from MCP servers using the mcp package.

MCP servers are started if required.  If INTERACTIVE is non-nil (or
called interactively), guide the user through setting up mcp, and query
for servers to retrieve tools from.

Call SERVER-CALLBACK after starting MCP servers, if starting them."
  (interactive (list t))
  (if (locate-library "mcp-hub")
      (unless (require 'mcp-hub nil t)
        (user-error "Could not load `mcp-hub'!  Please install\
 or configure the mcp package"))
    (user-error "Could not find mcp!  Please install or configure the mcp package"))
  (if (null mcp-hub-servers)
      (user-error "No MCP servers available!  Please configure `mcp-hub-servers'")
    (let ((unregistered-servers ;Available servers minus servers already registered with gptel
           (cl-loop for server in mcp-hub-servers
                    with registered-names =
                    (cl-loop for (cat . _tools) in gptel--known-tools
                             if (string-prefix-p "mcp-" cat)
                             collect (substring cat 4))
                    unless (member (car server) registered-names)
                    collect server)))
      (if unregistered-servers
          (let* ((servers
                  (if interactive
                      (let ((picks
                             (completing-read-multiple
                              "Add tools from MCP servers (separate with \",\"): "
                              (cons '("ALL") unregistered-servers) nil t)))
                        (if (member "ALL" picks)
                            unregistered-servers
                          (mapcar (lambda (s) (assoc s mcp-hub-servers)) picks)))
                    unregistered-servers))
                 (server-active-p
                  (lambda (server) (gethash (car server) mcp-server-connections)))
                 (get-all-tools (lambda () (mcp-hub-get-all-tool :asyncp t :categoryp t)))
                 (inactive-servers (cl-remove-if server-active-p servers))
                 (add-all-tools
                  (lambda ()
                    "Register and add tools from servers.  Report failures."
                    (let ((tools (funcall get-all-tools))
                          (now-active (cl-remove-if-not server-active-p mcp-hub-servers)))
                      (mapc (lambda (tool) (apply #'gptel-make-tool tool)) tools)
                      (gptel-mcp--activate-tools tools)
                      (if-let* ((failed (cl-set-difference inactive-servers now-active
                                                           :test #'equal)))
                          (message "%d/%d server%s failed to start: %s.  Run \\[mcp-hub] to investigate."
                                   (length failed) (length inactive-servers)
                                   (if (= (length failed) 1) "" "s")
                                   (mapconcat #'car failed ", "))
                        (message "Added %d tools from %d MCP server%s: %s"
                                 (length tools) (length now-active)
                                 (if (= (length now-active) 1) "" "s")
                                 (mapconcat #'car now-active ", ")))
                      (when (functionp server-callback) (funcall server-callback))))))

            (if inactive-servers        ;start servers
                (mcp-hub-start-all-server
                 add-all-tools (mapcar #'car inactive-servers))
              (funcall add-all-tools)))
        (message "All MCP tools are already available to gptel!")))))

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
                         (setq gptel-tools ;Remove from gptel-tools
                          (cl-delete-if (lambda (tool) (member (gptel-tool-category tool)
                                                          cat-names))
                                        gptel-tools))
                         (mapc (lambda (category) ;Remove from registry
                                 (setf (alist-get category gptel--known-tools
                                                  nil t #'equal)
                                       nil))
                               cat-names))))
        (if interactive
            (when-let* ((server-names
                         (completing-read-multiple
                          "Remove MCP server tools for (separate with \",\"): "
                          (cons '("ALL" . nil) names-alist)
                          nil t)))
              (when (member "ALL" server-names)
                  (setq server-names (mapcar #'car names-alist)))
              (funcall remove-fn        ;remove selected tool categories
                       (mapcar (lambda (s) (cdr (assoc s names-alist))) server-names))
              (if (y-or-n-p
                   (format "Removed MCP tools from %d server%s.  Also shut down MCP servers?"
                           (length server-names)
                           (if (= (length server-names) 1) "" "s")))
                  (progn (mapc #'mcp-stop-server server-names)
                         (message "Shut down MCP servers: %S" server-names))
                (message "Removed MCP tools for: %S" server-names)))
          (funcall remove-fn (mapcar #'cdr names-alist))))
    ;; No MCP tools, ask to shut down servers
    (if (cl-loop
         for v being the hash-values of mcp-server-connections
         never v)
        (when interactive (message "No MCP servers active!"))
      (when (or (not interactive)
                (y-or-n-p "No MCP tools in gptel!  Shut down all MCP servers? "))
        (dolist (server mcp-hub-servers)
          (when (gethash (car server) mcp-server-connections)
            (mcp-stop-server (car server))))))))

(defun gptel-mcp--activate-tools (&optional tools)
  "Activate TOOLS or all MCP tools in current gptel session."
  (unless tools (setq tools (mcp-hub-get-all-tool :asyncp t :categoryp t)))
  (dolist (tool tools)
    (cl-pushnew (gptel-get-tool (list (plist-get tool :category)
                                      (plist-get tool :name)))
                gptel-tools)))

(with-eval-after-load 'gptel-transient
  ;; FIXME: If `gptel-mcp-connect' opens mcp-hub, the transient stays open.  I
  ;; don't know how to fix this.
  (transient-define-suffix gptel--suffix-mcp-connect ()
    "Register tools provided by MCP servers."
    :key "M+"
    :description "Add MCP server tools"
    :transient t
    (interactive)
    (condition-case err
        (gptel-mcp-connect
         t (lambda () (when-let* ((transient--prefix)
                             ((eq (oref transient--prefix command)
                                  'gptel-tools)))
                   (transient-setup 'gptel-tools))))
      (user-error (message "%s" (cadr err))))
    (transient-setup))

  (transient-define-suffix gptel--suffix-mcp-disconnect ()
    "Remove tools provided by MCP servers from gptel."
    :key "M-"
    :description (lambda () (if (cl-some (lambda (cat) (string-match-p "^mcp-" cat))
                                    (map-keys gptel--known-tools))
                           "Remove MCP server tools"
                         "Shut down MCP servers"))
    :transient t
    :inapt-if
    (lambda () (or (not (boundp 'mcp-hub-servers))
              (null mcp-hub-servers)
              (cl-loop
               for v being the hash-values of mcp-server-connections
               never v)))
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

;; Local Variables:
;; byte-compile-warnings: (not noruntime)
;; End:
