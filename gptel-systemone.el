;;; gptel-systemone.el --- System One API backend for gptel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>

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

;; 

;;; Code:
(require 'cl-generic)
(eval-when-compile (require 'rx))
(require 'cl-lib)
(require 'map)
(require 'gptel-request)

;; System One Backend (TypeSafe, Ollaya etc)
(cl-defstruct (gptel-systemone (:constructor gptel--make-systemone)
                               (:copier nil)
                               (:include gptel-backend)))

(declare-function gptel-context--collect-media "gptel-context")
(declare-function json-read "json")
(declare-function gptel-context--wrap "gptel-context")

;; There are three ways to pass the structured response to the callback
;;
;; 1. Use a FSM state that checks for :decisions in INFO, then dispatches to a
;; handler for decisions.  Avoided since it creates a lot of churn across
;; several transition tables in several packages.
;;
;; 2. Make `gptel--parse-response' return (decisions . answers) instead of a
;; string.  This is a deep change to how response parsing should work, since
;; response can only be a string or nil until now, and all other information
;; (like tool calls and reasoning text) is transmitted through the INFO side
;; channel.  Avoided because I'm not sure this is the right call.
;;
;; 3. Hacky but lowest interface impact: Call the callback with (decisions
;; . answers) via the request's :post action.  :post is meant for cleanup and
;; not response handling, but until I figure out the right API change to support
;; systemone models this will have to do.

(defun gptel-systemone--callback (info)
  "Deliver (decisions . ANSWERS) from INFO to the request callback."
  (when-let* ((callback (plist-get info :callback)))
    (with-demoted-errors "gptel callback error: %S"
      (funcall callback (cons 'decisions (plist-get info :decisions)) info))))

(cl-defmethod gptel--parse-response ((_backend gptel-systemone) response info)
  "Record System One answers and usage in INFO.
Answers are normalized into INFO under :decisions and delivered
to the callback via INFO's :post functions at the terminal FSM
state.  Returns nil: there is no text RESPONSE."
  (when-let* ((usage (plist-get response :usage)))
    (plist-put info :tokens (list :input (or (plist-get usage :input_tokens) 0)
                                  :output (or (plist-get usage :output_tokens) 0))))
  (when-let* ((answers (plist-get response :answers)))
    (plist-put info :decisions answers)
    (plist-put info :post (append (plist-get info :post)
                                  (list #'gptel-systemone--callback))))
  nil)

(cl-defmethod gptel--request-data ((backend gptel-systemone) state)
  ""
  (let ((prompts-plist
         (list :model (gptel--model-name gptel-model)
               :state state
               :questions (gptel--parse-schema backend gptel--schema))))
    (unless gptel--schema
      (user-error "Requests for backend %s require questions to be
 specified via the `:schema' argument" (gptel-backend-name backend)))
    (gptel--merge-plists
     prompts-plist
     gptel--request-params
     (gptel-backend-request-params gptel-backend)
     (gptel--model-request-params  gptel-model))))

(cl-defmethod gptel--parse-schema ((_backend gptel-systemone) schema)
  "Convert SCHEMA to a System One API questions format spec."
  (when (stringp schema)  ;Two possibilities: serialized JSON, or shorthand form
    (with-temp-buffer     ;Parser for (possibly) shorthand forms
      (insert schema)
      (skip-chars-backward " \n\r\t")
      (delete-region (point) (point-max)) ;Remove trailing whitespace
      (goto-char (point-min)) (skip-chars-forward " \n\r\t")
      (if (= (char-after) ?{)
          (setq schema (gptel--json-read)) ;Assume serialized JSON schema, we're done
        (let (types labels descriptions criteria)
          (if (= (point-max) (line-end-position)) ; Single or multi-line?
              ;; Single line: we're looking for a pattern of one of the
              ;; following forms, where the absence of a type implies a noul:
              ;; Label1, Label2, Label3
              ;; Label1 Type1(Criterion1, Criterion2), Label2, Label3 Type3(...)
              (while (re-search-forward
                      (rx (group (+ (any alnum ?- ?_))) ;label, non-empty
                          (* " ")
                          (? (group (or "choice" "score" "noul" "box"))) ;type
                          (? (seq "(" (group (+ (not ")"))) ")")) ;criteria
                          (* " ") (or "," eos))
                      nil t)
                (push (match-string 1) labels)
                (push (concat (match-string 1) "?") descriptions)
                (push (if (or (not (match-string 2))
                              (string-empty-p (match-string 2)))
                          "noul" (match-string 2))
                      types)
                (push (unless (or (not (match-string 3))
                                  (string-empty-p (match-string 3)))
                        (split-string (match-string 3) " *, *"))
                      criteria)
                (skip-chars-forward " \t"))
            ;; Multi-line: we're looking for one of the following patterns on _each_ line
            ;; Label: Instructions
            ;; Label Type
            ;; Label Type: Instructions
            ;; Label Type(Criterion1, Criterion2): Instructions
            ;; Label Type(Criterion1=Desc1, Criterion2=Desc2, Criterion3): Instructions
            (while (re-search-forward
                    (rx (seq (group (+ (not (any " " ":")))) ;label, non-empty
                             (* " ")
                             (? (group (or "choice" "score" "noul" "box"))) ;type
                             (? (seq "(" (group (+ (not ")"))) ")")) ;criteria
                             (? ":")))  ;before instructions
                    (line-end-position) t)
              (push (match-string 1) labels)
              (skip-chars-forward " \t")
              (push (if (eolp)
                        (concat (match-string 1) "?")
                      (buffer-substring-no-properties (point) (line-end-position)))
                    descriptions)
              (push (if (or (not (match-string 2))
                            (string-empty-p (match-string 2)))
                        "noul" (match-string 2))
                    types)
              (push (unless (or (not (match-string 3))
                                (string-empty-p (match-string 3)))
                      (split-string (match-string 3) " *, *"))
                    criteria)
              (forward-line 1) (skip-chars-forward " \t\n")))
          (setq schema
                (cl-mapcan
                 (lambda (label type crit desc)
                   `(,(intern (concat ":" label))
                     ( :type ,type
                       ,@(when crit
                           ( list :criteria
                             (pcase type
                               ("choice"
                                (cl-loop for c in crit
                                         for (c-key c-desc) = (split-string c " *= *")
                                         nconc (list (intern (concat ":" c-key))
                                                     (or c-desc c-key))))
                               (_ (vconcat crit)))))
                       ,@(when desc
                           (list :instructions (string-trim desc))))))
                 (nreverse labels) (nreverse types)
                 (nreverse criteria) (nreverse descriptions)))))))
  schema)

(cl-defmethod gptel--parse-buffer ((backend gptel-systemone) &optional _max-entries)
  ""
  (goto-char (point-min))
  (if (eql major-mode 'lisp-data-mode)  ;This is Elisp-encoded JSON, send as-is
      (condition-case nil
          (read (current-buffer))
        (error (user-error "Backend %s: Prompt must be JSON-serializable"
                           (gptel-backend-name backend))))
    (buffer-substring-no-properties (point-min) (point-max))))

(cl-defmethod gptel--parse-list ((backend gptel-systemone) _prompt-list)
  ""
  (user-error "Backend %s: List prompts unsupported"
              (gptel-backend-name backend)))

(cl-defun gptel-make-systemone
    (name &key curl-args models
          key request-params
          (header
           (lambda (_info)
             (when-let* ((key (gptel--get-api-key)))
               `(("Authorization" . ,(concat "Bearer " key))))))
          (host "api.typesafe.ai")
          (protocol "https")
          (endpoint "/v1/systemone"))
  ""
  (declare (indent 1))
  (let ((backend
         (gptel--make-systemone
          :name name
          :curl-args curl-args
          :host host
          :header header
          :key key
          :models models
          :protocol protocol
          :endpoint endpoint
          :request-params request-params
          :url (if protocol
                   (concat protocol "://" host endpoint)
                 (concat host endpoint)))))
    (prog1 backend
      (setf (alist-get name gptel--known-backends
                       nil nil #'equal)
            backend))))

(provide 'gptel-systemone)
;;; gptel-systemone.el ends here

;; Local Variables:
;; byte-compile-warnings: (not docstrings)
;; End:
