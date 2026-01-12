;;; gptel-chain.el --- Chain gptel requests together  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Keywords: convenience, tools

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

;; gptel-chain allows you to chain multiple gptel requests together in a
;; directed acyclic graph (DAG), with support for parallel execution,
;; branching, and joining.
;;
;; Each node in the chain executes a gptel-request, and the output (accumulated
;; in a working buffer) feeds into dependent nodes.  Nodes are executed in
;; dependency order, with independent branches running in parallel.
;;
;; Basic usage:
;;
;;   (gptel-chain
;;     '((:system "Extract claims" :prompt "Article text...")
;;       (:system "Fact check the claims")
;;       (:system "Summarize the fact checks"))
;;     :callback (lambda (result info)
;;                 (message "Done! Result: %s" result)))
;;
;; Branching and joining:
;;
;;   (gptel-chain
;;     '((extract :system "Extract claims" :prompt "Text...")
;;       (check-1 :system "Fact check claim 1" :inputs '(extract))
;;       (check-2 :system "Fact check claim 2" :inputs '(extract))
;;       (synthesize
;;        :system "Synthesize findings"
;;        :inputs '(check-1 check-2)
;;        :prompt (lambda (inputs)
;;                  (format "Claim checks:\n%s\n%s"
;;                          (with-current-buffer (alist-get 'check-1 inputs)
;;                            (buffer-string))
;;                          (with-current-buffer (alist-get 'check-2 inputs)
;;                            (buffer-string)))))))
;;
;; Node prompts can be:
;; - nil: concatenate all input buffer contents (default)
;; - String: literal prompt text
;; - Buffer: extract its contents
;; - Function: receives ((node-name . buffer) ...) alist of input buffers, where
;;   buffer contains the output of node-name.
;;
;; See `gptel-chain' for full documentation.

;;; Code:

(require 'gptel-request)
(require 'gptel)
(require 'cl-lib)

(defgroup gptel-chain nil
  "Chain gptel requests together."
  :group 'gptel)

;;; User options

(defcustom gptel-chain-default-error-policy 'abort
  "Default error handling policy for chain nodes.

Can be:
  \\='abort - Stop the entire chain on node failure
  \\='continue - Let dependent nodes handle missing inputs
  \\='retry - Retry the failed node (requires :retry-count)
  function - Custom handler receiving (chain node-name info)"
  :type '(choice (const :tag "Abort chain" abort)
                 (const :tag "Continue with error" continue)
                 (const :tag "Retry node" retry)
                 (function :tag "Custom handler"))
  :group 'gptel-chain)

(defcustom gptel-chain-keep-buffers nil
  "Whether to keep node working buffers after chain completion.

Useful for debugging.  When nil, buffers are automatically cleaned up."
  :type 'boolean
  :group 'gptel-chain)

;;; Data structures

(cl-defstruct (gptel-chain
               (:constructor gptel--make-chain)
               (:copier gptel--copy-chain))
  "Structure representing a chain of gptel requests.

NODES: Alist of (name . node-spec) after normalization and augmentation.
Node-spec is a plist containing both user-provided keys (:prompt, :system,
:inputs, :callback, etc.) and runtime keys added by the executor:
  :work-buffer - Working buffer where responses accumulate
  :fsm - The gptel-fsm object returned by `gptel-request'
  :pending - Number of unfulfilled dependencies

BUFFER: Buffer where the chain runs.  This is the buffer that was current
when gptel-chain was called, and is used as the default :buffer for node
requests.  This determines the scope for buffer-local variables and the
context for tool execution.

COMPLETED: List of completed node names.

FAILED: List of failed node names.

READY-QUEUE: List of node names ready to execute.

CALLBACK: Function called when chain completes or fails.
Receives (chain) as its only argument. The chain can be queried for
status and results via gptel-chain-status and gptel-chain-result."
  (nodes nil :documentation "Graph nodes as ((node-name . node-spec) ...)")
  (buffer nil :documentation "Buffer in which chain runs")
  (completed nil :documentation "List of completed node names")
  (failed nil :documentation "List of failed node names")
  (ready-queue nil :documentation "List of node names ready to execute")
  (callback nil :documentation "Called after chain completes or fails"))

;;; Node normalization

(defun gptel-chain--normalize-nodes (nodes)
  "Normalize NODES list, adding names and implicit inputs.

NODES can be a list of:
- (name . spec) - named node
- spec - nameless node, auto-named gptel-chain-node-N

Adds implicit :inputs to nodes without explicit dependencies:
- First node (or nodes with empty :inputs \\='()): no dependencies (root)
- Other nodes without :inputs: depend on previous node

Returns alist of (name . copied-and-normalized-spec).
Each spec plist is copied to avoid mutating user\\='s original data."
  (cl-loop for idx from 1
           for node in nodes
           for prev-name = nil then current-name
           for (current-name . original-spec) =
           (if (not (keywordp (car-safe node)))
               node                     ; Named node: (name . spec)
             ;; Nameless node: just spec
             (cons (intern (format "gptel-chain-node-%d" idx)) node))
           ;; Copy spec to avoid mutating user data
           for spec = (copy-sequence original-spec)
           for inputs = (plist-get spec :inputs)
           collect (cons current-name
                         (if (and (not inputs) prev-name)
                             ;; Add implicit dependency on previous node
                             (plist-put spec :inputs (list prev-name))
                           spec))))

;;; Prompt generation

(defun gptel-chain--default-combiner (input-buffers-alist)
  "Combine input buffer contents with headers.

INPUT-BUFFERS-ALIST is ((name . buffer) ...)."
  (mapconcat
   (lambda (input)
     (with-current-buffer (cdr input)
       (format "=== %s ===\n%s"
               (car input)
               (buffer-substring-no-properties (point-min) (point-max)))))
   input-buffers-alist
   "\n\n"))

(defun gptel-chain--generate-prompt (node-spec input-buffers-alist)
  "Generate prompt string for node from input dependencies.

NODE-SPEC is the node's plist.
INPUT-BUFFERS-ALIST is ((input-name . buffer) ...) for each dependency.

Returns a string to use as the prompt for `gptel-request'."
  (let ((prompt-spec (plist-get node-spec :prompt)))
    (cond
     ;; User-provided buffer: extract its contents
     ((bufferp prompt-spec)
      (with-current-buffer prompt-spec
        (buffer-substring-no-properties (point-min) (point-max))))
     
     ;; Function: call with alist of (name . buffer)
     ;; Function can extract text, transform, or reuse buffers
     ((functionp prompt-spec)
      (funcall prompt-spec input-buffers-alist))
     
     ;; String: use literal, ignore inputs
     ((stringp prompt-spec)
      prompt-spec)
     
     ;; Nil: default combiner, or nil if no inputs (for DWIM in first node)
     ((null prompt-spec)
      (and input-buffers-alist
          (gptel-chain--default-combiner input-buffers-alist))))))

;;; Chain initialization

(defun gptel-chain--initialize (chain)
  "Initialize dependency tracking and find root nodes in CHAIN."
  (dolist (node (gptel-chain-nodes chain))
    (let* ((name (car node))
           (spec (cdr node))
           (inputs (plist-get spec :inputs)))
      (if inputs
          ;; Has dependencies: store pending count in spec
          (plist-put spec :pending (length inputs))
        ;; No dependencies: root node, ready to run
        (push name (gptel-chain-ready-queue chain))))))

;;;###autoload
(defun gptel-chain (nodes &rest args)
  "Create and execute a graph of gptel requests.

This directed graph is composed of \"nodes\", where each node makes a
`gptel-request' query.  Unless explicitly specified,
- the prompt for the starting node(s) is obtained from the buffer as usual by
`gptel-request'.
- The prompt for other nodes' queries is obtained by combining the query
responses from nodes that feed into them.

NODES is a list of node specifications.  Each node can be:
  (name :prompt PROMPT :inputs INPUTS :system SYSTEM ...) ; name is a symbol
  (:prompt PROMPT :system SYSTEM ...)  ; nameless, auto-named

Node spec keys:
  All `gptel-request' keywords, plus
  :inputs - List of node names this depends on
  :prompt - String, function, buffer, or nil (see below)
  :on-error - \\='abort, \\='continue, \\='retry, or function
  :retry-count - Number of retries for \\='retry policy
  :preset - `gptel-preset' to apply when running this node

:prompt behavior:
  - nil: Concatenate outputs from input nodes (default)
  - String: Use as literal prompt text
  - Buffer: Extract buffer contents as prompt
  - Function: Called with ((input-name . buffer) ...) alist,
              should return prompt string

ARGS can include:
  :callback - Function called with (chain) when chain completes or
              fails.  Use `gptel-chain-status' and `gptel-chain-result'
              to query results.

Returns the chain struct for introspection."
  (let* ((callback (plist-get args :callback))
         (origin-buffer (current-buffer))
         (normalized-nodes (gptel-chain--normalize-nodes nodes))
         (chain (gptel--make-chain
                 :nodes normalized-nodes
                 :buffer origin-buffer
                 :completed nil
                 :failed nil
                 :ready-queue nil
                 :callback callback)))
    ;; Create working buffer for each node and store in spec
    (dolist (node normalized-nodes)
      (let* ((name (car node))
             (spec (cdr node))
             (buf (generate-new-buffer (format " *gptel-chain-%s*" name) t)))
        (plist-put spec :work-buffer buf)))
    ;; Initialize dependency tracking
    (gptel-chain--initialize chain)
    ;; Start execution
    (gptel-chain--schedule chain)
    chain))

;;; Node execution

(defun gptel-chain--launch-request (prompt buffer node-spec callback)
  "Launch `gptel-request' with PROMPT in BUFFER using NODE-SPEC and CALLBACK."
  (apply #'gptel-request prompt
         :buffer buffer
         :callback callback
         ;; Pass through node spec params, filtering out chain-specific keys
         (cl-loop for (key val) on node-spec by #'cddr
                  unless (memq key '( :inputs :preset :on-error :work-buffer
                                      :retry-count :callback :pending :prompt))
                  append (list key val))))

(defun gptel-chain--run-node (chain node-name)
  "Execute node NODE-NAME in CHAIN."
  (let* ((node-spec (alist-get node-name (gptel-chain-nodes chain)))
         (work-buffer (plist-get node-spec :work-buffer))
         (user-callback (plist-get node-spec :callback))
         (wrapped-callback
          (lambda (response info)
            ;; 1. Insert string responses into work-buffer
            (when (stringp response)
              (with-current-buffer work-buffer
                (goto-char (point-max))
                (insert response)))
            
            ;; 2. Call user callback if provided
            (when user-callback
              (funcall user-callback response info))
            
            ;; 3. Check for completion
            (cond
             ;; Final string response (no pending tool use)
             ((and (stringp response)
                   (not (plist-get info :tool-use)))
              (gptel-chain--complete-node chain node-name))
             
             ;; Error
             ((null response)
              (gptel-chain--handle-node-failure chain node-name info))
             
             ;; Abort
             ((eq response 'abort)
              (gptel-chain--handle-abort chain node-name)))))
         ;; :buffer for context/tools - user-provided or chain's origin buffer
         (request-buffer (or (plist-get node-spec :buffer)
                             (gptel-chain-buffer chain)))
         (inputs (plist-get node-spec :inputs))
         (preset (plist-get node-spec :preset))
         ;; Gather input buffers from input node specs
         (input-buffers-alist
          (mapcar (lambda (input-name)
                    (let ((input-spec (alist-get input-name (gptel-chain-nodes chain))))
                      (cons input-name (plist-get input-spec :work-buffer))))
                  inputs))
         ;; Generate prompt
         (prompt (gptel-chain--generate-prompt node-spec input-buffers-alist))
         ;; Launch request with request-buffer for context, optionally with preset
         (fsm (if preset
                  (gptel-with-preset preset
                    (gptel-chain--launch-request
                     prompt request-buffer node-spec wrapped-callback))
                (gptel-chain--launch-request
                 prompt request-buffer node-spec wrapped-callback))))
    ;; Store FSM in node spec
    (plist-put node-spec :fsm fsm)))

;;; Completion and scheduling

(defun gptel-chain--complete-node (chain node-name)
  "Mark NODE-NAME as complete in CHAIN and schedule dependents."
  ;; Mark complete
  (push node-name (gptel-chain-completed chain))
  ;; Update and schedule dependents
  (gptel-chain--notify-dependents chain node-name))

(defun gptel-chain--notify-dependents (chain completed-node)
  "Update CHAIN nodes depending on COMPLETED-NODE and schedule ready nodes."
  (pcase-dolist (`(,name . ,spec) (gptel-chain-nodes chain))
    (let ((inputs (plist-get spec :inputs)))
      ;; If this node depends on completed-node
      (when (memq completed-node inputs)
        (let ((pending (plist-get spec :pending)))
          (when pending
            ;; Decrement pending count in spec
            (plist-put spec :pending (1- pending))
            ;; If all inputs satisfied, mark ready
            (when (zerop (plist-get spec :pending))
              (push name (gptel-chain-ready-queue chain))))))))
  ;; Schedule newly ready nodes
  (gptel-chain--schedule chain))

(defun gptel-chain--complete-p (chain)
  "Return t if CHAIN execution is complete (all nodes done or failed)."
  (let ((total-nodes (length (gptel-chain-nodes chain)))
        (done-nodes (+ (length (gptel-chain-completed chain))
                       (length (gptel-chain-failed chain)))))
    (>= done-nodes total-nodes)))

(defun gptel-chain--finalize (chain)
  "Finalize CHAIN execution and call completion callback."
  (when-let ((callback (gptel-chain-callback chain)))
    (funcall callback chain))
  ;; Cleanup buffers unless keeping them
  (unless gptel-chain-keep-buffers
    (gptel-chain-cleanup-buffers chain)))

(defun gptel-chain--schedule (chain)
  "Launch all ready nodes in CHAIN, or finalize if complete."
  ;; Check if chain is complete
  (if (gptel-chain--complete-p chain)
      (gptel-chain--finalize chain)
    ;; Launch ready nodes
    (dolist (node-name (gptel-chain-ready-queue chain))
      (gptel-chain--run-node chain node-name))
    ;; Clear queue
    (setf (gptel-chain-ready-queue chain) nil)))

;;; Error handling

(defun gptel-chain--handle-node-failure (chain node-name info)
  "Handle failure of NODE-NAME in CHAIN according to its error policy."
  (let* ((node-spec (alist-get node-name (gptel-chain-nodes chain)))
         (policy (or (plist-get node-spec :on-error)
                     gptel-chain-default-error-policy))
         (retry-count (plist-get node-spec :retry-count)))
    (pcase policy
      ('abort
       (gptel-chain--abort-all chain node-name
                               (format "Node %s failed" node-name)))
      
      ('continue
       ;; Mark as completed (with error) so dependents can proceed
       (push node-name (gptel-chain-completed chain))
       (gptel-chain--notify-dependents chain node-name))
      
      ('retry
       (if (and retry-count (> retry-count 0))
           (progn
             ;; Decrement retry count and re-queue
             (plist-put node-spec :retry-count (1- retry-count))
             (push node-name (gptel-chain-ready-queue chain))
             (gptel-chain--schedule chain))
         ;; No retries left, abort
         (gptel-chain--abort-all chain node-name
                                 (format "Node %s failed after retries"
                                         node-name))))
      
      ((pred functionp)
       ;; Custom handler
       (funcall policy chain node-name info)))))

(defun gptel-chain--handle-abort (chain node-name)
  "Handle user abort of NODE-NAME in CHAIN."
  (let* ((node-spec (alist-get node-name (gptel-chain-nodes chain)))
         (policy (or (plist-get node-spec :on-error)
                     gptel-chain-default-error-policy)))
    (pcase policy
      ('abort
       (gptel-chain--abort-all chain node-name
                               (format "Node %s aborted by user" node-name)))
      
      ('continue
       (push node-name (gptel-chain-completed chain))
       (gptel-chain--notify-dependents chain node-name))
      
      (_
       (gptel-chain--handle-node-failure chain node-name
                                         (list :error 'user-abort))))))

(defun gptel-chain--abort-all (chain failed-node reason)
  "Abort all pending requests in CHAIN due to FAILED-NODE failure.

REASON is a string describing why the chain was aborted."
  ;; Mark as failed
  (push failed-node (gptel-chain-failed chain))
  
  ;; Find and abort active gptel requests belonging to this chain
  (dolist (proc-entry gptel--request-alist)
    (let* ((proc (car proc-entry))
           (fsm (cadr proc-entry))
           (info (gptel-fsm-info fsm))
           (request-buffer (plist-get info :buffer)))
      ;; Check if this request belongs to our chain by searching node specs
      (when (cl-some (lambda (node)
                       (eq (plist-get (cdr node) :work-buffer) request-buffer))
                     (gptel-chain-nodes chain))
        (gptel-abort proc))))
  
  ;; Finalize with error
  (gptel-chain--finalize chain))

;;; Public API

(defun gptel-chain-result (chain node-name &optional as-string)
  "Get result for NODE-NAME in CHAIN.

Returns the node's buffer by default.
If AS-STRING is non-nil, return buffer contents as a string.
Returns nil if node hasn't completed."
  (when (member node-name (gptel-chain-completed chain))
    (let* ((node-spec (alist-get node-name (gptel-chain-nodes chain)))
           (buf (plist-get node-spec :work-buffer)))
      (if as-string
          (with-current-buffer buf
            (buffer-substring-no-properties (point-min) (point-max)))
        buf))))

(defun gptel-chain-status (chain)
  "Return status summary for CHAIN as a plist.

Returns plist with keys:
  :state - \\='running, \\='complete, or \\='failed
  :total - total number of nodes
  :completed - list of completed node names
  :failed - list of failed node names
  :pending - list of pending node names"
  (let* ((all-nodes (mapcar #'car (gptel-chain-nodes chain)))
         (completed (gptel-chain-completed chain))
         (failed (gptel-chain-failed chain))
         (done (append completed failed))
         (pending (cl-set-difference all-nodes done))
         (state (cond
                 (failed 'failed)
                 ((null pending) 'complete)
                 (t 'running))))
    (list :state state
          :total (length all-nodes)
          :completed completed
          :failed failed
          :pending pending)))

(defun gptel-chain-cleanup-buffers (chain)
  "Kill all working buffers in CHAIN."
  (pcase-dolist (`(,_name . ,spec) (gptel-chain-nodes chain))
    (when-let ((buf (plist-get spec :work-buffer)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(provide 'gptel-chain)
;;; gptel-chain.el ends here
