;;; gptel-bedrock.el --- AWS Bedrock support for gptel  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Karthik Chikmagalur

;; Keywords: comm, convenience

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

;; This file adds support for AWS Bedrock to gptel.  Documentation for the request data and the
;; response payloads can be found at these two links:
;; * https://docs.aws.amazon.com/bedrock/latest/APIReference/API_runtime_Converse.html
;; * https://docs.aws.amazon.com/bedrock/latest/APIReference/API_runtime_ConverseStream.html

;;; Code:
(require 'cl-lib)
(require 'map)
(require 'gptel)
(require 'mail-parse)

(declare-function gptel-context--collect-media "gptel-context")
(declare-function gptel-context--wrap "gptel-context")

(cl-defstruct (gptel-bedrock (:constructor gptel--make-bedrock)
                             (:copier nil)
                             (:include gptel-backend))
  model-region)

(defconst gptel-bedrock--prompt-type
  ;; For documentation purposes only -- this describes the type of prompt objects that get passed
  ;; around. https://docs.aws.amazon.com/bedrock/latest/APIReference/API_runtime_Message.html
  '(plist
    :role (member "user" "assistant")
    :content (array (or (plist :text string)
                        (plist :image (:format (member "png" "jpeg" "gif" "webp")
                                       :source (plist :bytes string))) ; bytes is b64 encoded
                        (plist :document (:format (member "pdf" "csv" "doc" "docx" "xls" "xlsx" "html" "txt" "md")
                                          :name string
                                          :source (plist :bytes string))) ; bytes is b64 encoded
                        (plist :toolUse (plist :input any :name string :toolUseId string))
                        (plist :toolResult (plist
                                            :toolUseId string
                                            :status (member "success" "error")
                                            ;; AWS allows more result types in
                                            ;; ToolResultContentBlock, but we only send text results
                                            :content (array (plist :text string))))))))

(cl-defmethod gptel--request-data ((backend gptel-bedrock) prompts)
  "Prepare request data for AWS Bedrock BACKEND from PROMPTS."
  ;; First, build the core Bedrock request data structure.
  (let ((base-request-data
         (nconc
          `(:messages [,@prompts] :inferenceConfig (:maxTokens ,(or gptel-max-tokens 500)))
          (when gptel--system-message `(:system [(:text ,gptel--system-message)]))
          (when gptel-temperature `(:temperature ,gptel-temperature))
          (when (and gptel-use-tools gptel-tools)
            `(:toolConfig (:toolChoice ,(if (eq gptel-use-tools 'force) '(:any '()) '(:auto '()))
                           :tools ,(gptel--parse-tools backend gptel-tools)))))))

    ;; Finally, merge all potential :request-params sources.
    (gptel--merge-plists
     base-request-data               ; The core data we just built
     gptel--request-params           ; Global/buffer-local gptel--request-params
     (gptel-backend-request-params backend) ; From `gptel-make-bedrock` call
     (gptel--model-request-params gptel-model)))) ; From the model's properties

(cl-defmethod gptel--parse-tools ((_backend gptel-bedrock) tools)
  "Parse TOOLS and return a list of ToolSpecification objects.

TOOLS is a list of `gptel-tool' structs, which see."
   ;; https://docs.aws.amazon.com/bedrock/latest/APIReference/API_runtime_ToolSpecification.html
   (let ((default-outputs (cl-call-next-method))) ;; use openai tool-parse
     (cl-map 'vector
      (lambda (tool spec)
        (list :toolSpec
              (list
               :name (gptel-tool-name tool)
               :description (gptel-tool-description tool)
               :inputSchema (list :json (plist-get (plist-get spec :function) :parameters)))))
      (ensure-list tools) default-outputs)))

(cl-defmethod gptel--parse-response ((_backend gptel-bedrock) response info)
  "Parse a Bedrock (non-streaming) RESPONSE and return response text.

Mutate state INFO with response metadata."
  (plist-put info :stop-reason (plist-get response :stopReason))
  (plist-put info :input-tokens
             (map-nested-elt response '(:usage :inputTokens)))
  (plist-put info :output-tokens
             (map-nested-elt response '(:usage :outputTokens)))

  (let* ((message (map-nested-elt response '(:output :message)))
         (content-strs (thread-last (plist-get message :content)
                                    (mapcar (lambda (cblock) (plist-get cblock :text)))
                                    (delq nil))))
    (gptel-bedrock--record-tool-use message info)
    (and content-strs (apply #'concat content-strs))))

(defun gptel-bedrock--record-tool-use (message info)
  "If MESSAGE has tool use requests, save those to INFO."
  (let* ((content (plist-get message :content))
         (tool-use-blocks (cl-remove-if-not
                           (lambda (cblock) (plist-get cblock :toolUse))
                           content)))
    (when tool-use-blocks
      (cl-callf vconcat (plist-get (plist-get info :data) :messages) (list message))

      (plist-put info :tool-use
                 (mapcar (lambda (block)
                           (let ((tool-use (plist-get block :toolUse)))
                             (list
                              :name (plist-get tool-use :name)
                              :args (plist-get tool-use :input)
                              :id (plist-get tool-use :toolUseId))))
                         tool-use-blocks)))))

(cl-defmethod gptel--parse-list ((_backend gptel-bedrock) prompt-strings)
  "Create a list of prompt objects from PROMPT-STRINGS.

Assumes this is a conversation with alternating roles."
  (cl-loop for text in prompt-strings
           for role = t then (not role)
           if text collect
           (list :role (if role "user" "assistant")
                 :content `[(:text ,text)])))

(cl-defmethod gptel--inject-media ((_backend gptel-bedrock) prompts)
  "Wrap the first user prompt in PROMPTS with included media files.

Media files, if present, are placed in `gptel-context'."
  (when-let* ((media-list (gptel-context--collect-media)))
    (cl-callf2 vconcat (gptel-bedrock--parse-multipart media-list)
               (plist-get (car prompts) :content))))

(defvar-local gptel-bedrock--stream-cursor nil
  "Marker to indicate last point parsed.")

(cl-defmethod gptel-curl--parse-stream ((_backend gptel-bedrock) info)
  "Parse an AWS Bedrock streaming response from the ConverseStream API.
INFO is a plist containing the request context."
  (cl-block fn
    (save-excursion
      ;; Each streaming request uses a fresh buffer, so the cursor starts out null. We keep it unset
      ;; until we have received all the headers
      (when (null gptel-bedrock--stream-cursor)
        (goto-char (point-min))
        (unless (search-forward "\r\n\r\n" nil t) (cl-return-from fn))
        (save-restriction
          (narrow-to-region (point-min) (point)) ; Required by mail-fetch-field
          (let ((content-type (mail-header-parse-content-type
                               (mail-fetch-field "Content-Type"))))
            (cl-assert content-type nil "No Content-Type header found")
            (cl-assert (string-equal (car content-type) "application/vnd.amazon.eventstream")
                       t "Unexpected Content-Type %S, expected %S")))
        (setq gptel-bedrock--stream-cursor (point-marker))

        ;; :accumulated-events contains the events from an in-progress message, from the
        ;; messageStart onwards. With each messageStop it gets cleared
        (plist-put info :accumulated-events nil))

      ;; Start of main routine
      (let ((acc-cell (cdr (plist-member info :accumulated-events))) strings prompts)
        (goto-char gptel-bedrock--stream-cursor)
        (while-let ((event (gptel-bedrock--parse-stream-message)))
          (let ((event-type (assoc-default ":event-type" (plist-get event :headers))))
            (when (member event-type '("messageStart" "contentBlockStart" "contentBlockDelta"
                                       "contentBlockStop"))
              (push event (car acc-cell)))
            (pcase event-type
              ("metadata"
               (plist-put info :input-tokens (map-nested-elt event '(:payload :usage :inputTokens)))
               (plist-put info :output-tokens (map-nested-elt event '(:payload :usage :outputTokens))))
              ("contentBlockDelta"
               (when-let ((delta-text (map-nested-elt event '(:payload :delta :text))))
                 (push delta-text strings)))
              ("messageStop"
               (push (gptel-bedrock--assemble-content-blocks (nreverse (car acc-cell))) prompts)
               (setf (car acc-cell) nil)
               (plist-put info :stop-reason (map-nested-elt event '(:payload :stopReason)))
               (plist-put info :message-complete t)))))
        (move-marker gptel-bedrock--stream-cursor (point))

        (dolist (message prompts) (gptel-bedrock--record-tool-use message info))
        (apply #'concat (nreverse strings))))))

(defun gptel-bedrock--parse-stream-message ()
  "Parse AWS Bedrock event-stream message starting at current position.
Point should be at the beginning of an event in the `vnd.amazon.event-stream'
format.  Returns plist with :headers and :payload keys if successful, nil if
incomplete."
  ;; https://github.com/awslabs/aws-c-event-stream has documentation of this format
  ;; The format consists of three main sections: Prelude, Data, and Message CRC.
  ;; 1. Prelude (12 bytes)
  ;;    a. Total Byte Length (4 bytes): Specifies the total length of the message.
  ;;    b. Headers Byte Length (4 bytes): Indicates the length of the headers section.
  ;;    c. Prelude CRC (4 bytes): A CRC value for validating the integrity of the prelude.
  ;; 2. Data (variable length)
  ;;    a. Headers: An array of packed headers. Each header has a specific format documented in
  ;;       gptel-bedrock--parse-headers
  ;;    b. Payload: The main message content, also of variable length. Length can be computed from
  ;;       the prelude fields by subtracting the prelude length, headers length, and message CRC
  ;;       length from the total.
  ;; 3. Message CRC (4 bytes): A 4-byte CRC to validate the integrity of the entire message.

  ;; (point-max) is the position after the last character, hence the use of >= and not > below
  (when (>= (- (point-max) (point)) 12)
    (let* ((prelude-start (point))
           (prelude-length 12)
           (prelude-end (+ prelude-start prelude-length))
           (prelude (buffer-substring-no-properties prelude-start prelude-end))
           (total-length (gptel-bedrock--bytes-to-int32 (substring prelude 0 4)))
           (headers-length (gptel-bedrock--bytes-to-int32 (substring prelude 4 8)))
           (headers-start prelude-end)
           (headers-end (+ headers-start headers-length))
           headers payload)
      ;; We don't validate either CRC because isn't that what the networking stack is for?

      (when (>= (point-max) (+ prelude-start total-length))
        (goto-char headers-start)
        (setq headers (gptel-bedrock--parse-headers (buffer-substring (point) headers-end)))
        (cl-assert (equal (assoc-default ":message-type" headers) "event")
                   t "Unknown message type %S; expected %S")
        (cl-assert (equal (assoc-default ":content-type" headers) "application/json")
                   t "Unexpected content-type %S is not %S")

        (goto-char headers-end)
        (setq payload (gptel--json-read))
        (let* ((message-crc 4)
               (payload-length (- total-length headers-length prelude-length message-crc)))
          (cl-assert (= (- (point) headers-end) payload-length)
                     t "Unexpected payload length %d; expected %d."))

        (goto-char (+ prelude-start total-length))
        `(:headers ,headers :payload ,payload)))))

(defun gptel-bedrock--parse-headers (headers-data)
  "Parse HEADERS-DATA into alist of (NAME . VALUE).
Keys are string-valued, lower-cased names."
  ;; Header wire format:
  ;;   1. Header Name Byte Length (1 byte): Specifies the length of the header name.
  ;;   2. Header Name (String) (Variable length): Contains the name of the header.
  ;;   3. Header Value Type (1 byte): Identifies the type of the header value.
  ;;   4. Value String Byte Length (2 bytes): Indicates the length of the value string.
  ;;   5. Value (Variable length): Holds the actual value bytes
  (let ((pos 0) (max (length headers-data)) headers)
    (cl-flet ((pos++ () (prog1 pos (cl-incf pos)))
              (++pos (n) (cl-incf pos n))
              (utf8 (unibyte-string) (decode-coding-string unibyte-string 'utf-8 t)))
      (while (< pos max)
        (let* ((name-len (aref headers-data (pos++)))
               (name (substring headers-data pos (++pos name-len)))
               (type (aref headers-data (pos++)))
               (value-len (gptel-bedrock--bytes-to-int16 (substring headers-data pos (++pos 2))))
               (value
                (pcase type
                  ;; Header types from https://awslabs.github.io/aws-crt-python/api/eventstream.html
                  (0 t)
                  (1 :json-false)
                  (2 (let ((res (aref headers-data (pos++)))) ; int8
                       (if (> res 127) (- res 256) res)))
                  (3 (gptel-bedrock--bytes-to-int16 (substring headers-data pos (++pos 2)))) ; int16
                  (4 (gptel-bedrock--bytes-to-int32 (substring headers-data pos (++pos 4)))) ; int32
                  (5 (gptel-bedrock--bytes-to-int64 (substring headers-data pos (++pos 8)))) ; int64
                  (6 (substring headers-data pos (++pos value-len)))                         ; raw bytes
                  (7 (utf8 (substring headers-data pos (++pos value-len))))                  ; utf8 string
                  (8 (decode-time       ; 64 bit int with seconds since the Unix epoch
                      (gptel-bedrock--bytes-to-int64 (substring headers-data pos (++pos 8))) t))
                  (9 (gptel-bedrock--bytes-to-uuid ; 16 byte UUID
                      (substring headers-data pos (++pos 16))))
                  (_ (error "Unknown header type: %d" type)))))
          (push (cons (downcase (utf8 name)) value) headers)))
      (cl-assert (= pos max) t "Headers did not parse cleanly. pos=%d  header-len=%d")
      headers)))

(defun gptel-bedrock--bytes-to-int16 (bytes)
  "Convert 2-byte string BYTES to big-endian signed integer."
  (let ((b0 (logand (aref bytes 0) 255))
        (b1 (logand (aref bytes 1) 255)))
    (let ((result (+ (ash b0 8) b1)))
      (if (>= b0 #x80) (- result (ash 1 16)) result))))

(defun gptel-bedrock--bytes-to-int32 (bytes)
  "Convert 4-byte string BYTES to big-endian signed integer."
  (let ((b0 (logand (aref bytes 0) 255))
        (b1 (logand (aref bytes 1) 255))
        (b2 (logand (aref bytes 2) 255))
        (b3 (logand (aref bytes 3) 255)))
    (let ((result (+ (ash b0 24) (ash b1 16) (ash b2 8) b3)))
      (if (>= b0 #x80) (- result (ash 1 32)) result))))

(defun gptel-bedrock--bytes-to-int64 (bytes)
  "Convert 8-byte string BYTES to big-endian signed integer."
  (let ((b0 (logand (aref bytes 0) 255))
        (b1 (logand (aref bytes 1) 255))
        (b2 (logand (aref bytes 2) 255))
        (b3 (logand (aref bytes 3) 255))
        (b4 (logand (aref bytes 4) 255))
        (b5 (logand (aref bytes 5) 255))
        (b6 (logand (aref bytes 6) 255))
        (b7 (logand (aref bytes 7) 255)))
    (let ((result-u63 (+ (ash (logand b0 #x7f) 56) (ash b1 48)
                         (ash b2 40) (ash b3 32) (ash b4 24) (ash b5 16) (ash b6 8) b7)))
      (if (>= b0 #x80)
          (- result-u63 (ash 1 63))
        result-u63))))

(defun gptel-bedrock--bytes-to-uuid (bytes)
  "Convert a 16-byte unibyte BYTES to a 36 character UUID string."
  (unless (and (stringp bytes) (= (length bytes) 16))
    (error "Input must be a 16-byte unibyte string"))
  (let ((hex (mapconcat (lambda (i) (format "%02x" (aref bytes i))) (number-sequence 0 15) "")))
    (format "%s-%s-%s-%s-%s"
            (substring hex 0 8)
            (substring hex 8 12)
            (substring hex 12 16)
            (substring hex 16 20)
            (substring hex 20 32))))

(defun gptel-bedrock--assemble-content-blocks (events)
  "Build a completed prompt object contained from EVENTS.
EVENTS should be a list of messageStart, contentBlockStart,
contentBlockDelta, and contentBlockStop stream messages as
returned by `gptel-bedrock--parse-stream-message', in the order
received."
  (let ((blocks (make-hash-table :test #'eql))
        role contents)
    (dolist (event events)
      (let* ((headers (plist-get event :headers))
             (payload (plist-get event :payload))
             (event-type (assoc-default ":event-type" headers)))
        (pcase event-type
          ("messageStart" (setq role (plist-get payload :role)))
          ("contentBlockStart"
           (puthash (plist-get payload :contentBlockIndex) (list event) blocks))
          ("contentBlockDelta"
           (push event (gethash (plist-get payload :contentBlockIndex) blocks)))
          ("contentBlockStop"
           (let* ((block-index (plist-get payload :contentBlockIndex))
                  (block-events (nreverse (gethash block-index blocks)))
                  (start (car block-events))
                  (deltas (cdr block-events)))
             (when-let ((tool-use (map-nested-elt start '(:payload :start :toolUse))))
               (let ((id (plist-get tool-use :toolUseId))
                     (name (plist-get tool-use :name))
                     (input (gptel--json-read-string
                             (mapconcat
                              (lambda (delta) (map-nested-elt delta '(:payload :delta :toolUse :input)))
                              deltas))))
                 (push
                  (list :toolUse (list :input input :name name :toolUseId id))
                  contents)))
             (when-let ((texts (delq nil (mapcar (lambda (d) (map-nested-elt d '(:payload :delta :text))) deltas))))
               (push (list :text (apply #'concat texts)) contents))
             ;; Currently we discard any reasoning content but this would be the spot to handle it
             ))
          (_ (error "Unexpected event-type %S" event-type)))))
    (list :role role :content (vconcat (nreverse contents)))))

(cl-defmethod gptel--parse-buffer ((_backend gptel-bedrock) &optional max-entries)
  "Parse current buffer and return a list of prompt objects for Bedrock.

MAX-ENTRIES is the maximum number of prompts to include."
  (unless max-entries (setq max-entries most-positive-fixnum))
  (let ((prompts nil) (prev-pt (point))
        (include-media (and gptel-track-media (gptel--model-capable-p 'media))))
    (cl-flet ((capture-prompt (role beg end)
                (let* ((content (if include-media
                                 (gptel-bedrock--parse-multipart
                                  (gptel--parse-media-links major-mode beg end))
                                 `[(:text ,(gptel--trim-prefixes
                                           (buffer-substring-no-properties beg end)))]))
                       (prompt (list :role role :content content)))
                  (push prompt prompts))))

      (if (or gptel-mode gptel-track-response)
          (while (and (> max-entries 0)
                      (/= prev-pt (point-min))
                      (goto-char (previous-single-property-change
                                  (point) 'gptel nil (point-min))))
            (capture-prompt (pcase (get-char-property (point) 'gptel)
                              ('response "assistant")
                              ('nil "user"))
                            (point) prev-pt)
            (setq prev-pt (point))
            (cl-decf max-entries))
        (capture-prompt "user" (point-min) (point-max)))
      prompts)))

(defconst gptel-bedrock--image-formats
  '(("image/jpg" . "jpeg")
    ("image/jpeg" . "jpeg")
    ("image/png" . "png")
    ("image/gif" . "gif")
    ("image/webp" . "webp"))
  "Map of mime type to image formats as used in AWS's ImageBlock.")

(defconst gptel-bedrock--doc-formats
  '(("application/pdf" . "pdf")
    ("text/csv" . "csv")
    ("application/msword" . "doc")
    ("application/vnd.openxmlformats-officedocument.wordprocessingml.document" . "docx")
    ("application/vnd.ms-excel" . "xls")
    ("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" . "xlsx")
    ("text/html" . "html")
    ("text/plain" . "txt")
    ("text/markdown" . "md"))
  "Map of mime type to document formats as used in AWS's DocumentBlock.")

(defun gptel-bedrock--parse-multipart (parts)
  "Convert a multipart prompt PARTS to the AWS Bedrock API format.

The input is a list of text and media plists of the form:
 ((:text \"some text\")
  (:media \"/path/to/media.png\" :mime \"image/png\")
  (:text \"More text\")).

The output is a vector of entries in Bedrock API format."
  (thread-last parts
    (cl-maplist
     (lambda (tail)
       (let* ((part (car tail))
              (text (plist-get part :text))
              (mime (plist-get part :mime))
              (media (plist-get part :media))
              (textfile (plist-get part :textfile))
              format)
         (cond
          (text (when (or (eq part (car parts)) (null (cdr tail)))
                  (setq text (gptel--trim-prefixes text)))
                (unless (string-empty-p text)
                  `(:text ,text)))
          (media
           (cond
            ((setq format (assoc mime gptel-bedrock--image-formats))
             `(:image (:format ,(cdr format) :source (:bytes ,(gptel--base64-encode media)))))
            ((setq format (assoc mime gptel-bedrock--doc-formats))
             `(:document (:format ,(cdr format)
                          :name ,(file-name-nondirectory media)
                          :source (:bytes ,(gptel--base64-encode media)))))
            (t (error "Unsupported MIME type %s for AWS Bedrock" mime))))
          (textfile `(:text ,(with-temp-buffer
                               (gptel--insert-file-string textfile)
                               (buffer-string))))))))
    (delq nil)
    (vconcat)))

;; gptel--inject-prompt not needed since the default implementation works here

(cl-defmethod gptel--parse-tool-results ((_backend gptel-bedrock) tool-use-requests)
  "Return a backend-appropriate prompt containing tool call results.

TOOL-USE-REQUESTS is a list of request plists that have been
completed.  Returns a single prompt object to inject into the
conversation."
  (list
   :role "user"
   :content
   (vconcat
    (mapcar
     (lambda (tool-call)
       `(:toolResult (:toolUseId ,(plist-get tool-call :id)
                      :status "success"
                      :content [(:text ,(plist-get tool-call :result))])))
     tool-use-requests))))

(defvar gptel-bedrock--aws-profile-cache nil
  "Cache for AWS profile credentials in the form of (PROFILE . CREDS).")

(defun gptel-bedrock--fetch-aws-profile-credentials (profile &optional clear-cache)
  "Fetch & cache AWS credentials for PROFILE using aws-cli.

If PROFILE is the keyword ':static', then it fetches IAM credentials
from the aws-cli without any profile argument.

Non-nil CLEAR-CACHE will refresh credentials."
  (let* ((creds-json
           (let ((cell (or (assoc profile gptel-bedrock--aws-profile-cache #'string=)
                           (car (push (cons profile nil) gptel-bedrock--aws-profile-cache)))))
             (or (and (not clear-cache) (cdr cell))
                 (setf (cdr cell)
                       (with-temp-buffer
		           (unless (zerop (apply #'call-process "aws" nil t nil "configure" "export-credentials"
                                                 (unless (eql profile :static) (list (format "--profile=%s" profile)))))
		             (user-error "Failed to get AWS credentials from profile"))
		         (json-parse-string (buffer-string)))))))
	 (expiration (if-let (exp (gethash "Expiration" creds-json))
			     (date-to-time exp))))
    (cond
      ((time-less-p (current-time) expiration)
       (let ((access-key (gethash "AccessKeyId" creds-json))
	     (secret-key (gethash "SecretAccessKey" creds-json))
	     (session-token (gethash "SessionToken" creds-json)))
	 (cl-values access-key secret-key session-token)))
      ((not clear-cache)
       (gptel-bedrock--fetch-aws-profile-credentials profile t))
      (t (user-error "AWS credentials expired for profile: %s" profile)))))

(defun gptel-bedrock--get-credentials (profile)
  "Return the AWS credentials to use for the request.

If credentials are not available based on the AWS_ACCESS_KEY_ID
AWS_SECRET_ACCESS_KEY AWS_SESSION_TOKEN environment variables,
aws configure export-credentials is used to obtain credentials.
PROFILE specifies the AWS profile to use for retrieving
credentials.  If PROFILE is unset, AWS_PROFILE environment
variable is used.

Returns a list of 2-3 elements, depending on whether a session
token is needed, with this form: (AWS_ACCESS_KEY_ID AWS_SECRET_ACCESS_KEY
AWS_SESSION_TOKEN).

Convenient to use with `cl-multiple-value-bind'"
  (let ((key-id (getenv "AWS_ACCESS_KEY_ID"))
        (secret-key (getenv "AWS_SECRET_ACCESS_KEY"))
        (token (getenv "AWS_SESSION_TOKEN"))
	(profile (or profile (getenv "AWS_PROFILE"))))
    (cond
      ((and profile) (gptel-bedrock--fetch-aws-profile-credentials profile))
      ((and key-id secret-key) (cl-values key-id secret-key token))
      (t (user-error "Missing AWS credentials; provide them either via environment variables or specify PROFILE when calling gptel-make-bedrock")))))

(defvar gptel-bedrock--model-ids
  ;; https://docs.aws.amazon.com/bedrock/latest/userguide/models-supported.html
  '((claude-opus-4-6             . "anthropic.claude-opus-4-6-v1")
    (claude-sonnet-4-5-20250929  . "anthropic.claude-sonnet-4-5-20250929-v1:0")
    (claude-haiku-4-5-20251001   . "anthropic.claude-haiku-4-5-20251001-v1:0")
	(claude-opus-4-5-20251101    . "anthropic.claude-opus-4-5-20251101-v1:0")
    (claude-opus-4-1-20250805    . "anthropic.claude-opus-4-1-20250805-v1:0")
    (claude-sonnet-4-20250514    . "anthropic.claude-sonnet-4-20250514-v1:0")
    (claude-opus-4-20250514      . "anthropic.claude-opus-4-20250514-v1:0")
    (claude-3-7-sonnet-20250219  . "anthropic.claude-3-7-sonnet-20250219-v1:0")
    (claude-3-5-sonnet-20241022  . "anthropic.claude-3-5-sonnet-20241022-v2:0")
    (claude-3-5-sonnet-20240620  . "anthropic.claude-3-5-sonnet-20240620-v1:0")
    (claude-3-5-haiku-20241022   . "anthropic.claude-3-5-haiku-20241022-v1:0")
    (claude-3-opus-20240229      . "anthropic.claude-3-opus-20240229-v1:0")
    (claude-3-haiku-20240307     . "anthropic.claude-3-haiku-20240307-v1:0")
    (nova-2-lite-v1              . "amazon.nova-2-lite-v1:0")
    (mistral-7b                  . "mistral.mistral-7b-instruct-v0:2")
    (mistral-8x7b                . "mistral.mixtral-8x7b-instruct-v0:1")
    (mistral-large-2402          . "mistral.mistral-large-2402-v1:0")
    (mistral-large-2407          . "mistral.mistral-large-2407-v1:0")
    (mistral-small-2402          . "mistral.mistral-small-2402-v1:0")
    (llama-3-8b                  . "meta.llama3-8b-instruct-v1:0")
    (llama-3-70b                 . "meta.llama3-70b-instruct-v1:0")
    (llama-3-1-8b                . "meta.llama3-1-8b-instruct-v1:0")
    (llama-3-1-70b               . "meta.llama3-1-70b-instruct-v1:0")
    (llama-3-1-405b              . "meta.llama3-1-405b-instruct-v1:0")
    (llama-3-2-1b                . "meta.llama3-2-1b-instruct-v1:0")
    (llama-3-2-3b                . "meta.llama3-2-3b-instruct-v1:0")
    (llama-3-2-11b               . "meta.llama3-2-11b-instruct-v1:0")
    (llama-3-2-90b               . "meta.llama3-2-90b-instruct-v1:0")
    (llama-3-3-70b               . "meta.llama3-3-70b-instruct-v1:0"))
  "Map of model name to bedrock id.

IDs can be added or replaced by calling
\(push (model-name . \"model-id\") gptel-bedrock--model-ids).")

(defvar gptel--bedrock-models
  (mapcar #'car gptel-bedrock--model-ids)
  "List of available AWS Bedrock models and associated properties.")

(defun gptel-bedrock--get-model-id (model &optional region)
  "Return the Bedrock model ID for MODEL.

REGION is one of global, apac, eu or us."
  (concat
   (when region
     (or (member region '(global apac eu us))
	 (error "Unknown Bedrock region %s" region))
     (concat (symbol-name region) "."))
   (or (alist-get model gptel-bedrock--model-ids nil nil #'eq)
       (error "Unknown Bedrock model: %s" model))))

(defun gptel-bedrock--curl-args (region profile bearer-token)
  "Generate the curl arguments to get a bedrock request signed for use in REGION.

PROFILE specifies the aws profile to use for aws configure
export-credentials.  BEARER-TOKEN is the token used for authentication."
  (let ((bearer-token (or bearer-token (getenv "AWS_BEARER_TOKEN_BEDROCK")))
        (output-args (unless (memq system-type '(windows-nt ms-dos))
                       '("--output" "/dev/stdout"))))
    (if bearer-token
        (append
         (list "-H" (format "Authorization: Bearer %s" bearer-token))
         output-args)
      (cl-multiple-value-bind (key-id secret token) (gptel-bedrock--get-credentials profile)
        (append
         (list "--user" (format "%s:%s" key-id secret)
               "--aws-sigv4" (format "aws:amz:%s:bedrock" region))
         output-args
         (when token (list "-H" (format "x-amz-security-token: %s" token))))))))


(defun gptel-bedrock--curl-version ()
  "Check Curl version required for gptel-bedrock."
  (let* ((output (shell-command-to-string "curl --version"))
         (version (and (string-match "^curl \\([0-9.]+\\)" output)
                       (match-string 1 output))))
    version))

;;;###autoload
(cl-defun gptel-make-bedrock
    (name &key
          region
          (models gptel--bedrock-models)
	  (model-region nil)
          stream curl-args request-params
          aws-profile aws-bearer-token
          (protocol "https"))
  "Register an AWS Bedrock backend for gptel with NAME.

Keyword arguments:

REGION - AWS region name (e.g. \"us-east-1\")
MODELS - The list of models supported by this backend
MODEL-REGION - one of apac, eu, us or nil
AWS-PROFILE - the aws profile to use for aws configure export-credentials
AWS-BEARER-TOKEN - the aws bearer-token for authenticating with AWS
CURL-ARGS - additional curl args
STREAM - Whether to use streaming responses or not.
REQUEST-PARAMS - a plist of additional HTTP request
parameters (as plist keys) and values supported by the API."
  (declare (indent 1))
  (unless (or aws-bearer-token (getenv "AWS_BEARER_TOKEN_BEDROCK"))
    (unless (and gptel-use-curl (version<= "8.9" (gptel-bedrock--curl-version)))
      (error "Bedrock-backend requires curl >= 8.9, but gptel-use-curl := %s, curl-version := %s"
             gptel-use-curl (gptel-bedrock--curl-version))))
  (let ((host (format "bedrock-runtime.%s.amazonaws.com" region)))
    (setf (alist-get name gptel--known-backends nil nil #'equal)
          (gptel--make-bedrock
           :name name
           :host host
           :header nil           ; x-amz-security-token is set in curl-args if needed
           :models (gptel--process-models models)
           :model-region model-region
           :protocol protocol
           :endpoint "" ; Url is dynamically constructed based on other args
           :stream stream
           :coding-system (and stream 'binary)
           :curl-args (lambda () (append curl-args (gptel-bedrock--curl-args region aws-profile aws-bearer-token)))
           :request-params request-params
           :url
           (lambda ()
             (concat protocol "://" host
                     "/model/" (gptel-bedrock--get-model-id gptel-model model-region)
                     "/" (if stream "converse-stream" "converse")))))))

(provide 'gptel-bedrock)
;;; gptel-bedrock.el ends here
