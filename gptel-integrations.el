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
(declare-function mcp--status "mcp-hub")
(declare-function mcp--tools "mcp-hub")
(declare-function mcp-make-text-tool "mcp-hub")
(defvar mcp-hub-servers)
(defvar mcp-server-connections)

(defun gptel-mcp-connect (&optional servers server-callback interactive)
  "Add gptel tools from MCP servers using the mcp package.

MCP servers are started if required.  SERVERS is a list of server
names (strings) to connect to.  If nil, all known servers are
considered.

If INTERACTIVE is non-nil (or called interactively), guide the user
through setting up mcp, and query for servers to retrieve tools from.

Call SERVER-CALLBACK after starting MCP servers.  If SERVER-CALLBACK is
not a function and non-nil, start SERVERS synchronously."
  (interactive (list nil nil t))
  (if (locate-library "mcp-hub")
      (unless (require 'mcp-hub nil t)
        (user-error "Could not load `mcp-hub'!  Please install\
 or configure the mcp package"))
    (user-error "Could not find mcp!  Please install or configure the mcp package"))
  (if (null mcp-hub-servers)
      (user-error "No MCP servers available!  Please configure `mcp-hub-servers'")
    (setq servers
          (if servers
              (mapcar (lambda (s) (assoc s mcp-hub-servers)) servers)
            mcp-hub-servers))
    (let ((unregistered-servers ;Available servers minus servers already registered with gptel
           (cl-loop for server in servers
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
                  (lambda (server)
                    (when-let* ((server (gethash (car server) mcp-server-connections)))
                      (equal (mcp--status server) 'connected))))
                 (inactive-servers (cl-remove-if server-active-p servers))
                 (add-all-tools
                  (lambda (&optional server-names)
                    "Register and add tools from servers.  Report failures."
                    (let ((tools (gptel-mcp--get-tools
                                  (or server-names (mapcar #'car inactive-servers))))
                          (now-active (cl-remove-if-not server-active-p mcp-hub-servers)))
                      (mapc (lambda (tool) (apply #'gptel-make-tool tool)) tools)
                      (gptel-mcp--activate-tools tools)
                      (if-let* ((failed (cl-set-difference inactive-servers now-active
                                                           :test #'equal)))
                          (progn
                            (message "Inactive-before: %S, Now-Active: %S" inactive-servers now-active)
                            (message (substitute-command-keys
                                      "%d/%d server%s failed to start: %s.  Run \\[mcp-hub] to investigate.")
                                     (length failed) (length inactive-servers)
                                     (if (= (length failed) 1) "" "s")
                                     (mapconcat #'car failed ", ")))
                        (let ((added (or server-names (mapcar #'car now-active))))
                          (message "Added %d tools from %d MCP server%s: %s"
                                   (length tools) (length added)
                                   (if (= (length added) 1) "" "s")
                                   (mapconcat #'identity added ", "))))
                      (when (functionp server-callback) (funcall server-callback))))))

            (if inactive-servers        ;start servers
                (let ((syncp (and server-callback (not (functionp server-callback)) t)))
                  (mcp-hub-start-all-server
                   add-all-tools (mapcar #'car inactive-servers) syncp))
              (funcall add-all-tools (mapcar #'car servers))))
        (when interactive (message "All MCP tools are already available to gptel!"))
        (when (functionp server-callback) (funcall server-callback))))))

(defun gptel-mcp-disconnect (&optional servers interactive)
  "Unregister gptel tools provided by MCP servers using the mcp package.

SERVERS is a list of server names (strings) to disconnect from.

If INTERACTIVE is non-nil, query the user about which tools to remove."
  (interactive (list nil t))
  (if-let* ((names-alist
             (cl-loop
              for (category . _tools) in gptel--known-tools
              if (and (string-match "^mcp-\\(.*\\)" category)
                      (or (null servers) ;Consider all if nil
                          (member (match-string 1 category) servers)))
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

(defun gptel-mcp--get-tools (&optional server-names)
  "Return tools from running MCP servers.

SERVER-NAMES is a list of server names to add tools from.  Add tools
from all connected servers if it is nil."
  (unless server-names
    (setq server-names (hash-table-keys mcp-server-connections)))
  (let ((servers (mapcar (lambda (n) (gethash n mcp-server-connections))
                         server-names)))
    (cl-mapcan
     (lambda (name server)
       (when (and server (equal (mcp--status server) 'connected))
         (when-let* ((tools (mcp--tools server))
                     (tool-names (mapcar #'(lambda (tool) (plist-get tool :name)) tools)))
           (mapcar (lambda (tool-name)
                     (plist-put (mcp-make-text-tool name tool-name t)
                                :category (format "mcp-%s" name)))
                   tool-names))))
     server-names servers)))

(defun gptel-mcp--activate-tools (&optional tools)
  "Activate TOOLS or all MCP tools in current gptel session."
  (unless tools (setq tools (gptel-mcp--get-tools)))
  (dolist (tool tools)
    (cl-pushnew (gptel-get-tool (list (plist-get tool :category)
                                      (plist-get tool :name)))
                gptel-tools)))

(with-eval-after-load 'gptel-transient
  (transient-define-suffix gptel--suffix-mcp-connect ()
    "Register tools provided by MCP servers."
    :key "M+"
    :description "Add MCP server tools"
    :transient t
    (interactive)
    ;; gptel-tools stores its state in its scope slot.  Retain the scope but
    ;; update it with the newly selected tools.  Then set up gptel-tools.
    (condition-case err
        (gptel-mcp-connect
         nil (lambda () (when-let* ((transient--prefix)
                               ((eq (oref transient--prefix command)
                                    'gptel-tools)))
                     (let ((state (transient-scope 'gptel-tools)))
                       (plist-put state :tools
                                  (delete-dups
                                   (nconc (mapcar (lambda (tool)
                                                    (list (gptel-tool-category tool)
                                                          (gptel-tool-name tool)))
                                                  gptel-tools)
                                          (plist-get state :tools))))
                       (transient-setup 'gptel-tools nil nil :scope state))))
         t)
      (user-error (message "%s" (cadr err)))))

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
    ;; gptel-tools stores its state in its scope slot.  Retain the scope but
    ;; remove tools from it that no longer exist, then set up gptel-tools
    (cl-loop with state = (transient-scope 'gptel-tools)
             with tools = (plist-get state :tools)
             for tool-spec in tools
             if (map-nested-elt gptel--known-tools tool-spec)
             collect tool-spec into valid-tools
             finally do (plist-put state :tools valid-tools)
             (transient-setup 'gptel-tools nil nil :scope state)))

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
