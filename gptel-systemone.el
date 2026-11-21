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

;; This library adds support to gptel for "decision model" providers via the
;; System One API, including Ollaya, TypeSafe, Cloudflare (Clef) and others.
;;
;; Because decision models generate probabilities in structured outputs and not
;; text, they are not intended for chatting with `gptel-send'.  The primary use
;; case is ad hoc classification tasks using the `gptel-request' API.
;;
;; How the System One API works:
;;
;; 1. You supply the "state", which is any information (text, images or a mix)
;; that you want to ask questions about, as well as the "questions".  There are
;; four types of questions supported (see below).  The state can be freeform,
;; but the questions must be provided in a JSON schema required by the API.
;;
;; 2. The decision model responds with "answers" to the questions in a related
;; JSON schema.  The answers can include probabilities, scores or picks for
;; multiple-choice questions.
;;
;; How to use `gptel-systemone':
;;
;; 1. Register a backend with `gptel-make-systemone', or create an ad hoc
;; backend with `gptel--make-systemone'.
;;
;; 2. Call `gptel-request' with the state as the PROMPT, the questions as the
;; SCHEMA argument, and a CALLBACK.  The PROMPT can be a string (unstructured)
;; or any plist/array suitable for gptel's JSON encoding.  SCHEMA must be a
;; plist/array of questions in the API's questions schema (see examples below).
;;
;; (gptel-request STATE :schema QUESTIONS :callback CALLBACK [...])
;;
;; 3. The CALLBACK is called with the model's answers as (decisions . ANSWERS).
;; ANSWERS is as a plist in the API's answers schema.
;;
;; Examples of schema (See <https://docs.typesafe.ai/introduction> for the full
;; docs):
;;
;; STATE: This can be a string, or nil for the active region/text
;; above the cursor.  It can also be a gptel JSON encodable plist with any
;; structure.  Some decision models support images.  To include one (jpg/png),
;; use an Elisp image spec.  Plist example:
;;
;; (:photo1 (image :file "~/Pictures/mop01.png")
;;      ;or (image :data "..." :type "png" ...)
;;  :desc   "Lightweight and sturdy mop with wet/dry functions and a swiveling head.")
;;
;; QUESTIONS: A plist mapping question names to questions, where each question
;; type is one of "choice", "score", "noul" or "box".  Here is a comprehensive
;; list of questions about the above state:
;;
;; (:accuracy (:type noul
;;             :instructions "`desc` is an accurate description of `photo1`")
;;  :spray?   (:type noul
;;             :instructions "The mop in `photo1` includes a spray bottle.")
;;  :pad_type (:type choice
;;             :instructions "Type of cleaning attachment"
;;             :criteria (:detachable_pad "Flat absorbent pad, detachable"
;;                        :cloth_strips "Round head with cloth strips"))
;;  :height   (:type score
;;             :instructions "Max height of the handle in inches"
;;             :criteria ["15in" "25in" "35in" "50in"])
;;  :image_bounds (:type box :instructions "mop in `photo1`"))
;;
;; As you can see,
;; - Each question is a plist with a type, instructions and optionally criteria.
;;   The meanings of the question types will be clear from the ANSWERS below.
;; - Using a structured object as the state lets you refer to its
;;   components directly (`photo1`, `desc` etc).
;; - A "shorthand" question specification is also available, see
;;  `gptel--parse-schema' below.
;;
;; ANSWERS: A plist of answers returned by the API, one answer per question:
;;
;; ;; "noul" is a yes/no question with a score from 0 (no) to 1 (yes):
;; (:accuracy (:type "noul" :noul 0.66)
;;  :spray?   (:type "noul" :noul 0.83)
;;
;; ;; "choice" makes a choice between the provided options
;;  :pad_type (:type "choice" :choice "detachable_pad" :confidence 0.98
;;             :probabilities (:detachable_pad 0.99 :cloth_strips 0.01))
;;
;; ;; "score" assigns an index to the options, a probability distribution to the
;; ;; index and the expected value
;;  :height (:type "score" :score 2.96 :confidence 0.96
;;           :legend (:0 "15in" :1 "25in" :2 "35in" :3 "50in")
;;           :probabilities (:0 0.0 :1 0.01 :2 0.02 :3 0.97))
;;
;; ;; "box" reports the bounds of an object in an image
;;  :image_bounds (:type "box" :box (:top 0.1 :bottom 0.95 :left 0.47 :right 0.78)
;;                 :confidence 0.45))
;;
;; The System One API can be used any time you need an automated decision to be
;; made.  Besides general data classification, it can be used in the context of
;; LLM agents to decide if tool calls are safe, or which grade of model to
;; dispatch a query to.  In an Eamcs context, it can be used to sort/rank
;; completions, dynamically respond to the user's actions or Emacs state, etc.

;;; Code:
(require 'cl-generic)
(eval-when-compile (require 'rx))
(require 'cl-lib)
(require 'map)
(require 'gptel-request)

;;;; System One Backend (TypeSafe, Ollaya etc)
(cl-defstruct (gptel-systemone (:constructor gptel--make-systemone)
                               (:copier nil)
                               (:include gptel-backend)))

(declare-function gptel-context--collect-media "gptel-context")
(declare-function json-read "json")
(declare-function gptel-context--wrap "gptel-context")

(defun gptel-systemone--register-asset (spec assets)
  "Add image SPEC to ASSETS and return a reference to it.
SPEC is an image spec plist
- (:file PATH [...]) for an image file, or
- (:data STRING :type FORMAT [...]) for image data already
  in memory.  System One only supports \"png\" or \"jpg\" formats.

ASSETS is a one-element list holding the assets plist, modified
in place; see `gptel-systemone--sanitize-tree' for its use."
  (cond
   ((and-let* ((path (plist-get spec :file)) ;Image as file path
               (fmt (downcase (file-name-extension path)))
               (ref (concat "gptel-" (secure-hash 'md5 path)))
               (ref-kwd (intern (concat ":" ref))))
      (unless (plist-get (car assets) ref-kwd)
        (setcar assets
                (plist-put (car assets) ref-kwd
                           (list :type "image" :format fmt
                                 :data_base64 (gptel--base64-encode path)))))
      ref))
   ((and-let* ((data (plist-get spec :data)) ;Image as inline data
               (fmt (plist-get spec :type))
               (fmt (symbol-name fmt))
               (ref (concat "gptel-" (secure-hash 'md5 data)))
               (ref-kwd (intern (concat ":" ref))))
      (unless (plist-get (car assets) ref-kwd)
        (setcar assets
                (plist-put (car assets) ref-kwd
                           (list :type "image" :format fmt
                                 :data_base64 (base64-encode-string data t)))))
      ref))
   (t (user-error "Image spec is not valid"))))

(defun gptel-systemone--sanitize-tree (tree assets)
  "Sanitize TREE (a plist, list or vector) for sending to System One.
TREE is the request state or questions spec in the plist-based JSON
representation used by `gptel--json-encode'.  Non-keyword symbols in
TREE are replaced by strings, and image specs of the form (image . spec)
are replaced with (\"gptel-<md5>\") references.

Image data from image specs are placed in ASSETS, which should be a
\"boxed\" list like (list nil).

TREE and ASSETS are both modified in-place."
  (cond
   ((vectorp tree)
    (cl-loop
     for element across tree
     for idx upfrom 0
     do (cond
         ((eq (car-safe element) 'image)
          (aset tree idx (gptel-systemone--register-asset (cdr element) assets)))
         ((or (listp element) (vectorp element))
          (gptel-systemone--sanitize-tree element assets))
         ((and (symbolp element) (not (memq element '(t nil))))
          (aset tree idx (symbol-name element))))))
   ((listp tree)
    (let ((element))
      (while tree
        (setq element (car tree))
        (cond
         ((or (keywordp element) (stringp element))) ;simple key/value, passthrough
         ((eq (car-safe element) 'image)
          (setcar tree (gptel-systemone--register-asset (cdr element) assets)))
         ((or (listp element) (vectorp element))
          (gptel-systemone--sanitize-tree element assets))
         ((and (symbolp element) (not (memq element '(t nil))))
          (setcar tree (symbol-name element))))
        (setq tree (cdr tree)))))))

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
  "Record System One RESPONSE and usage in INFO.
Mutate state INFO with response metadata."
  (when-let* ((usage (plist-get response :usage)))
    (plist-put info :tokens (list :input (or (plist-get usage :input_tokens) 0)
                                  :output (or (plist-get usage :output_tokens) 0))))
  (when-let* ((answers (plist-get response :answers)))
    (plist-put info :decisions answers)
    (plist-put info :post (append (plist-get info :post)
                                  (list #'gptel-systemone--callback))))
  nil)

(cl-defmethod gptel--request-data ((backend gptel-systemone) state)
  "JSON encode STATE for sending to System One BACKEND."
  (let* ((assets-box (list nil))
         (questions (gptel--parse-schema backend gptel--schema))
         (prompts-plist
          (list :model (gptel--model-name gptel-model)
                :state state
                :questions questions)))
    (unless gptel--schema
      (user-error "Requests for backend %s require questions to be
 specified via the `:schema' argument" (gptel-backend-name backend)))
    (gptel-systemone--sanitize-tree questions assets-box) ;both mutated in-place
    (gptel-systemone--sanitize-tree state assets-box)
    (when (car-safe assets-box)         ;Images included
      (plist-put prompts-plist :assets (car assets-box)))
    (gptel--merge-plists
     prompts-plist
     gptel--request-params
     (gptel-backend-request-params gptel-backend)
     (gptel--model-request-params  gptel-model))))

(cl-defmethod gptel--parse-schema ((_backend gptel-systemone) schema)
  "Convert the questions SCHEMA to the System One API questions format.

The questions can be specified in three different ways:

- As a raw JSON string, passed along.
- As a plist object suitable for JSON encoding.

For convenience, as a \"shorthand\" string, where each line is of the type

question_name type(criterion1, criterion2, ...): instructions

For example, the questions schema:

 (:accuracy (:type noul
             :instructions \"`desc` is an accurate description of `photo1`\")
  :spray?   (:type noul
             :instructions \"The mop in `photo1` includes a spray bottle.\")
  :pad_type (:type choice
             :instructions \"Type of cleaning attachment\"
             :criteria (:detachable_pad \"Flat absorbent pad, detachable\"
                        :cloth_strips \"Round head with cloth strips\"))
  :height (:type score
           :instructions \"Max height of the handle in inches\"
           :criteria [\"15in\" \"25in\" \"35in\" \"50in\"])
  :image_bounds (:type box :instructions \"mop in `photo1`\"))

can be specified as a string with one question per line:

\"accuracy noul: `desc` is an accurate description of `photo1`
 spray?   noul: The mop in `photo1` includes a spray bottle.
 pad_type choice(detachable_pad=Flat absorbent pad, cloth_strips=Round head): Type of cleaning attachment
 height score(15in, 25in, 35in, 50in): Max height of the handle in inches
 image_bounds box: mop in `photo1`\"

The \":\" separator and the instructions for a question may be omitted,
as can the descriptions of choices for the choice question type.
Questions default to the noul type and may be omitted also:

\"accuracy
 spray? noul
 pad_type choice(detachable_pad, cloth_strips)
 height score(15in, 25in, 35in, 50in)
 image_bounds box: mop in `photo1`\""
  (when (stringp schema)  ;Two possibilities: serialized JSON, or shorthand form
    (with-temp-buffer     ;Parser for (possibly) shorthand forms
      (insert schema)
      (skip-chars-backward " \n\r\t")
      (delete-region (point) (point-max)) ;Remove trailing whitespace
      (goto-char (point-min)) (skip-chars-forward " \n\r\t")
      (if (= (char-after) ?{)
          (setq schema (gptel--json-read)) ;Assume serialized JSON schema, we're done
        (let (types labels descriptions criteria)
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
                           (? ":")))    ;before instructions
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
            (forward-line 1) (skip-chars-forward " \t\n"))
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

;; KLUDGE: Failed attempt to integrate a single-line, multiple question schema,
;; rejected because of parsing difficulties
;;
;; ;; Single line: we're looking for a pattern of one of the
;; ;; following forms, where the absence of a type implies a noul:
;; ;; Label1, Label2, Label3
;; ;; Label1 Type1(Criterion1, Criterion2), Label2, Label3 Type3(...)
;; (while (re-search-forward
;;         (rx (group (+ (any alnum ?- ?_))) ;label, non-empty
;;             (* " ")
;;             (? (group (or "choice" "score" "noul" "box"))) ;type
;;             (? (seq "(" (group (+ (not ")"))) ")")) ;criteria
;;             (* " ") (or "," eos))
;;         nil t)
;;   (push (match-string 1) labels)
;;   (push (concat (match-string 1) "?") descriptions)
;;   (push (if (or (not (match-string 2))
;;                 (string-empty-p (match-string 2)))
;;             "noul" (match-string 2))
;;         types)
;;   (push (unless (or (not (match-string 3))
;;                     (string-empty-p (match-string 3)))
;;           (split-string (match-string 3) " *, *"))
;;         criteria)
;;   (skip-chars-forward " \t"))

(cl-defmethod gptel--parse-buffer ((backend gptel-systemone) &optional _max-entries)
  "Parse the current buffer for a System One state for BACKEND.
The state can be a plist or array suitable for `gptel--json-encode', or
just text."
  (goto-char (point-min))
  (if (eql major-mode 'lisp-data-mode)  ;This is Elisp-encoded JSON, send as-is
      (condition-case nil
          (read (current-buffer))
        (error (user-error "Backend %s: Prompt must be JSON-serializable"
                           (gptel-backend-name backend))))
    (buffer-substring-no-properties (point-min) (point-max))))

(cl-defmethod gptel--parse-list ((backend gptel-systemone) _prompt-list)
  "Parse a list of prompts for System One BACKEND.
Currently unimplemented."
  (user-error "Backend %s: List prompts unsupported"
              (gptel-backend-name backend)))

;;;###autoload
(cl-defun gptel-make-systemone
    (name &key curl-args models
          key request-params
          (header
           (lambda (info)
             (when-let* ((key (gptel--get-api-key)))
               `(("Authorization" . ,(concat "Bearer " key))
                 ,@(when (plist-get (plist-get info :data) :assets)
                     '(("TypeSafe-Danger-Zone-Images" . "true")))))))
          (host "api.typesafe.ai")
          (protocol "https")
          (endpoint "/v1/systemone"))
  "Register a System One-compatible backend for gptel with NAME.

Keyword arguments:

CURL-ARGS (optional) is a list of additional Curl arguments.

HOST (optional) is the API host.  Common values include
- \"localhost:11435\" for Ollaya/Laya
- \"api.typesafe.ai\" for TypeSafe AI

MODELS is a list of available model names, as symbols.

PROTOCOL (optional) specifies the protocol, https by default.

ENDPOINT (optional) is the API endpoint to query, defaults to
\"v1/systemone\".

HEADER (optional) is for additional headers to send with each request.
It should be an alist or a function that returns an alist, like:
  ((\"Content-Type\" . \"application/json\"))

KEY (optional) is a variable whose value is the API key, or a function
that returns the key....

REQUEST-PARAMS (optional) is a plist of additional HTTP request
parameters (as plist keys) and values supported by the API.  Use
these to set parameters that gptel does not provide user options
for."
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

;;;; OpenAI decisions backend (inheriting from gptel-systemone)

;; The OpenAI decisions backend piggybacks on the System One backend, since the
;; API is similar except for minor differences in schema.

(cl-defstruct (gptel-openai-decisions
               (:constructor gptel--make-openai-decisions)
               (:copier nil)
               (:include gptel-systemone)))

(defun gptel-openai-decisions--encode-questions (questions)
  "Convert System One QUESTIONS to the OpenAI decisions API format.
QUESTIONS is the canonical questions plist, as returned by
`gptel--parse-schema' for a `gptel-systemone' backend.  Return a vector
of question objects with :name, :type, :instructions and :choices or
:levels keys, as required by the OpenAI Decisions API."
  (cl-loop
   for (id spec) on questions by #'cddr
   for type = (gptel--to-string (plist-get spec :type))
   when (equal type "box")
   do (user-error "The OpenAI decisions API has no \"box\" question type")
   collect
   (append
    (list :name (substring (symbol-name id) 1)
          :type (if (equal type "noul") "predicate" type))
    (when-let* ((instr (plist-get spec :instructions)))
      (list :instructions instr))
    (pcase type
      ("choice"
       (let ((crit (plist-get spec :criteria)))
         (list :choices
               (vconcat
                (if (vectorp crit)      ;Plain values, no descriptions
                    (mapcar (lambda (v) (list :value v)) crit)
                  (cl-loop
                   for (v d) on crit by #'cddr
                   for value = (if (symbolp v)
                                   (substring (symbol-name v) 1)
                                 v)
                   collect (append
                            (list :value value)
                            (unless (equal d value) ;Skip redundant descriptions
                              (list :description d)))))))))
      ("score"
       (let ((crit (plist-get spec :criteria)))
         (list :levels
               (vconcat
                (if (vectorp crit)      ;Plain labels, no descriptions
                    (mapcar (lambda (l) (list :label l)) crit)
                  ;; Plist of levels: labels with descriptions
                  (cl-loop for (l d) on crit by #'cddr
                           collect (append
                                    (list :label (if (symbolp l)
                                                     (substring (symbol-name l) 1)
                                                   l))
                                    (when (stringp d)
                                      (list :description d)))))))))))
   into qs
   finally return (vconcat qs)))

(defun gptel-openai-decisions--decode-answer (answer)
  "Convert an OpenAI decision ANSWER to the System One answer format."
  (pcase (plist-get answer :type)
    ("predicate"
     (list :type "noul" :noul (plist-get answer :probability)))
    ("choice"
     (list :type "choice"
           :choice (plist-get answer :choice)
           :confidence (plist-get answer :confidence)
           :probabilities
           (cl-loop for p across (plist-get answer :probabilities)
                    for v = (plist-get p :value)
                    nconc (list (if (stringp v)
                                    (intern (concat ":" v))
                                  v) ; booleans (only reachable via raw OpenAI JSON schemas)
                                (plist-get p :probability)))))
    ("score"
     (list :type "score"
           :score (plist-get answer :score)
           :confidence (plist-get answer :confidence)
           :legend
           (cl-loop for p across (plist-get answer :probabilities)
                    for key = (intern (concat ":" (number-to-string
                                                   (plist-get p :value))))
                    nconc (list key (plist-get p :label)))
           :probabilities
           (cl-loop for p across (plist-get answer :probabilities)
                    for key = (intern (concat ":" (number-to-string
                                                   (plist-get p :value))))
                    nconc (list key (plist-get p :probability)))))
    ("refusal" (list :type "refusal"))
    ;; Pass unknown answer types through unchanged
    (_ (list :type (plist-get answer :type)))))

(defun gptel-openai-decisions--decode-answers (answers)
  "Convert OpenAI ANSWERS to the System One answers format.
ANSWERS is the vector of answer objects returned by the OpenAI decisions
API.  Nameless refusals are keyed by `:refusal'."
  (cl-loop for answer across (or answers [])
           for name = (plist-get answer :name)
           for key = (if (stringp name) (intern (concat ":" name)) :refusal)
           nconc (list key (gptel-openai-decisions--decode-answer answer))))

(cl-defmethod gptel--request-data ((backend gptel-openai-decisions) state)
  "JSON encode STATE for sending to the OpenAI decisions BACKEND.
STATE can be a string or a plist/array as accepted by
`gptel-make-systemone'.  Image specs in STATE are extracted into
\"input_image\" parts."
  (unless gptel--schema
    (user-error "Requests for backend %s require questions to be
 specified via the `:schema' argument" (gptel-backend-name backend)))
  (let ((assets-box (list nil)))
    ;; Replace image specs in STATE with reference strings,
    ;; collecting their base64 data in ASSETS-BOX
    (gptel-systemone--sanitize-tree state assets-box)
    (let ((input
           (if (null (car assets-box))
               (if (stringp state) state (gptel--json-encode state))
             (vconcat
              (list (list :type "input_text"
                          :text (if (stringp state)
                                    state
                                  (gptel--json-encode state))))
              (cl-loop for (_ref asset) on (car assets-box) by #'cddr
                       collect (list :type "input_image"
                                     :image_url
                                     (concat "data:image/"
                                             (plist-get asset :format)
                                             ";base64,"
                                             (plist-get asset :data_base64))))))))
      (gptel--merge-plists
       (list :model (gptel--model-name gptel-model)
             :input input
             :questions (gptel-openai-decisions--encode-questions
                         (gptel--parse-schema backend gptel--schema)))
       gptel--request-params
       (gptel-backend-request-params gptel-backend)
       (gptel--model-request-params gptel-model)))))

(cl-defmethod gptel--parse-response ((backend gptel-openai-decisions) response info)
  "Convert OpenAI decisions RESPONSE to System One form and record in INFO.
The response is delegated to the `gptel-systemone' BACKEND response
parser."
  (cl-call-next-method
   backend
   (list :model (plist-get response :model)
         :answers (gptel-openai-decisions--decode-answers
                   (plist-get response :answers))
         :usage (plist-get response :usage))
   info))

;;;###autoload
(cl-defun gptel-make-openai-decisions
    (name &key curl-args
          key request-params
          (header
           (lambda (_info)
             (when-let* ((key (gptel--get-api-key)))
               `(("Authorization" . ,(concat "Bearer " key))))))
          (host "api.openai.com")
          (protocol "https")
          (endpoint "/v1/decisions")
          (models '(gpt-6-luna)))
  "Register an OpenAI decisions API backend for gptel with NAME.

Keyword arguments:

CURL-ARGS (optional) is a list of additional Curl arguments.

HOST (optional) is the API host, \"api.openai.com\" by default.

MODELS (optional) is a list of available model names, as symbols.
Only gpt-6-luna is currently available.

PROTOCOL (optional) specifies the protocol, https by default.

ENDPOINT (optional) is the API endpoint, \"v1/decisions\" by default.

HEADER (optional) is for additional headers to send with each
request.  It should be an alist or a function that returns an
alist, like:
  ((\"Content-Type\" . \"application/json\"))

KEY (optional) is a variable whose value is the API key, or a
function that returns the key.

REQUEST-PARAMS (optional) is a plist of additional API parameters,
such as \"safety_identifier\"."
  (declare (indent 1))
  (let ((backend
         (gptel--make-openai-decisions
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
