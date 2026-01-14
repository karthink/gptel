;;; gptel-request.el --- LLM request library for gptel         -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025  Karthik Chikmagalur

;; Author: Karthik Chikmagalur;; <karthikchikmagalur@gmail.com>
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

;; LLM querying library used by gptel.  This file provides the basic data
;; structures (models, backends) and prompt construction functions used by
;; gptel, along with the `gptel-request' API.
;;
;; This is everything required to use `gptel-request' to write custom commands,
;; UIs or packages that use gptel to implement custom workflows.  To use gptel's
;; LLM querying API, you can
;;
;; (require 'gptel-request)
;;
;; and make calls to `gptel-request'.
;;
;; Note that this file does not provide any of the UI components used by gptel
;; (chat buffers, tool use prompts, transient menus), nor does it provide the
;; default response callbacks used by `gptel-request'.  You will need to provide
;; your own callback to `gptel-request' to act on the LLM response, or require
;; the larger `gptel' feature.

;;; Code:

(require 'gptel-openai)
(eval-when-compile
  (require 'subr-x))
(require 'compat nil t)
(require 'cl-lib)
(require 'url)
(require 'text-property-search)
(require 'cl-generic)
(require 'map)
(require 'mailcap)                    ;FIXME Avoid this somehow

(declare-function json-read "json" ())
(defvar json-object-type)

(declare-function gptel--stream-convert-markdown->org "gptel-org")
(declare-function gptel--convert-markdown->org "gptel-org")
(declare-function gptel-org--create-prompt-buffer "gptel-org")
(declare-function gptel-context--wrap "gptel-context")
(declare-function gptel--transform-apply-preset "gptel")
(declare-function gptel--insert-response "gptel")
(declare-function gptel-curl--stream-insert-response "gptel")


;;; User options
(defgroup gptel nil
  "Interact with LLMs from anywhere in Emacs."
  :group 'hypermedia)

(defcustom gptel-proxy ""
  "Path to a proxy to use for gptel interactions.
Passed to curl via --proxy arg, for example \"proxy.yourorg.com:80\"
Leave it empty if you don't use a proxy."
  :type 'string)

(defcustom gptel-api-key #'gptel-api-key-from-auth-source
  "An API key (string) for the default LLM backend.

OpenAI by default.

Can also be a function of no arguments that returns an API
key (more secure) for the active backend."
  :type '(choice
          (string :tag "API key")
          (function :tag "Function that returns the API key")))

(defcustom gptel-stream t
  "Stream responses from the LLM as they are received.

This option is ignored unless
- the LLM backend supports streaming, and
- Curl is in use (see `gptel-use-curl')

When set to nil, Emacs waits for the full response and inserts it
all at once.  This wait is asynchronous.

\='tis a bit silly."
  :type 'boolean)

(defcustom gptel-use-curl (and (executable-find "curl") t)
  "Whether gptel should prefer Curl when available.

Can be set to t, nil, or a string path to the curl executable."
  :type '(choice
          (const :tag "Do not use Curl" nil)
          (const :tag "Use Curl" t)
          (string :tag "Specify path to the Curl executable")))

(defcustom gptel-org-convert-response t
  "Whether gptel should convert Markdown responses to Org markup.

This only affects requests originating from Org mode buffers."
  :type 'boolean)

(defcustom gptel-curl-file-size-threshold
  (if (memq system-type '(windows-nt ms-dos)) #x6ffe 130000)
  "Size threshold for using file input with Curl.

Specifies the size threshold for when to use a temporary file to pass data to
Curl in gptel queries.  If the size of the data to be sent exceeds this
threshold, the data is written to a temporary file and passed to Curl using the
`--data-binary' option with a file reference.  Otherwise, the data is passed
directly as a command-line argument.

The value is an integer representing the number of bytes.

Adjusting this value may be necessary depending on the environment
and the typical size of the data being sent in gptel queries.
A larger value may improve performance by avoiding the overhead of creating
temporary files for small data payloads, while a smaller value may be needed
if the command-line argument size is limited by the operating system.

The default of #x8000 for windows comes from Microsoft documentation
located here:
https://learn.microsoft.com/en-us/windows/win32/api/processthreadsapi/nf-processthreadsapi-createprocessa

It is set to (#x8000 - #x1000 - 2) to account for other (non-data) Curl
command line arguments."
  :type 'natnum)

(define-obsolete-variable-alias 'gptel-prompt-filter-hook
  'gptel-prompt-transform-functions "0.9.9")

(defcustom gptel-prompt-transform-functions
  '(gptel--transform-apply-preset gptel--transform-add-context)
  "Handlers to augment or transform a query before sending it.

This hook is called in a temporary buffer containing the text to
be sent, with the cursor at the end of the prompt.  You can use
it to modify the buffer or buffer-local variables as required.

Since these functions modify the prompt construction buffer, the order
in which they run is significant!  In particular, you may want to add
your function before (the default) or after
`gptel--transform-add-context', which adds gptel's context (other
buffers, files etc) to this buffer.

Example: A typical use case might be to search for occurrences of $(cmd)
and replace it with the output of the shell command cmd, making it easy
to send the output of shell commands to the LLM.

Transform functions can be synchronous or asynchronous.

Synchronous hook functions must accept zero or one argument: the INFO
plist for the current request.

Asynchronous hook functions must accept two arguments: a callback to
call after the transformation is complete, and the INFO plist for the
current request.

Note that while this set of handlers can certainly be set with a global
value to be applied to all queries in all buffers, it meant to be set
locally for a specific buffer, or chat topic, or only the context of a
certain task."
  :type 'hook)

(defcustom gptel-post-request-hook nil
  "Hook run after sending a gptel request.

This runs (possibly) before any response is received."
  :type 'hook)

;; TODO(v1.0): Remove this.
(defvar gptel-response-filter-functions nil)
(make-obsolete-variable
 'gptel-response-filter-functions
 "Response filtering is no longer supported in gptel.  To toggle
markdown to Org conversion, see `gptel-org-convert-response'.  To
filter LLM response text, either use `gptel-request' with a
custom callback, or use `gptel-post-response-functions'."
 "0.9.7")

;; TODO: Handle `prog-mode' using the `comment-start' variable
(defcustom gptel-prompt-prefix-alist
  '((markdown-mode . "### ")
    (org-mode . "*** ")
    (text-mode . "### "))
  "String used as a prefix to the query being sent to the LLM.

This is meant for the user to distinguish between queries and
responses, and is removed from the query before it is sent.

This is an alist mapping major modes to the prefix strings.  This
is only inserted in dedicated gptel buffers."
  :type '(alist :key-type symbol :value-type string))

(defcustom gptel-response-prefix-alist
  '((markdown-mode . "")
    (org-mode . "")
    (text-mode . ""))
  "String inserted before the response from the LLM.

This is meant for the user to distinguish between queries and
responses.

This is an alist mapping major modes to the reply prefix strings.  This
is only inserted in dedicated gptel buffers before the AI's response."
  :type '(alist :key-type symbol :value-type string))

(defcustom gptel-response-separator "\n\n"
  "String inserted before responses.

Also inserted before and after non-consecutive tool calls."
  :type 'string)

;; Model and interaction parameters
(defcustom gptel-directives
  '((default     . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
    (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
    (writing     . "You are a large language model and a writing assistant. Respond concisely.")
    (chat        . "You are a large language model and a conversation partner. Respond concisely."))
  "System prompts or directives for the LLM.

Each entry in this alist maps a symbol naming the directive to
the directive itself.  By default, gptel uses the directive with
the key \\+`default'.

To set the directive for a chat session interactively call
`gptel-send' with a prefix argument, or call `gptel-menu'.

A \"directive\" is typically the system message (also called
system prompt or system instruction) sent at the beginning of
each request to the LLM.  It is used to set general instructions,
expectations and the overall tone.

gptel's idea of the directive is more general.  A directive in
`gptel-directives' can be

- A string, interpreted as the system message.

- A list of strings, whose first (possibly nil) element is
  interpreted as the system message, and the remaining elements
  as (possibly nil) alternating user prompts and LLM responses.
  This can be used to template the initial part of a conversation.

- A function that returns a string or a list of strings,
  interpreted as the above.  This can be used to dynamically
  generate a system message and/or conversation template based on
  the current context.  See the definition of
  `gptel--rewrite-directive-default' for an example."
  :safe #'always
  :type '(alist :key-type symbol :value-type string))

(defcustom gptel-max-tokens nil
  "Max tokens per response.

This is roughly the number of words in the response.  100-300 is a
reasonable range for short answers, 400 or more for longer
responses.

To set the target token count for a chat session interactively
call `gptel-send' with a prefix argument."
  :safe #'always
  :type '(choice (natnum :tag "Specify Token count")
                 (const :tag "Default" nil)))

(defcustom gptel-temperature 1.0
  "\"Temperature\" of the LLM response.

This is a number between 0.0 and 2.0 that controls the randomness
of the response, with 2.0 being the most random.

To set the temperature for a chat session interactively call
`gptel-send' with a prefix argument."
  :safe (lambda (v) (or (null v) (numberp v)))
  :type '(choice (number :tag "Temperature value")
                 (const :tag "Use default" nil)))

(defcustom gptel-cache nil
  "Whether the LLM should cache request content.

Some LLM backends can cache content sent to it by gptel, so that
only the newly included part of the text needs to be processed on
subsequent conversation turns.  This results in faster and
significantly cheaper processing.

NOTE: Manual or client-configurable caching is currently
supported only by the Anthropic API and thus the
`gptel-anthropic' backend.  This variable has no effect on the
behavior of other backends.

This variable controls which parts of the query will be cached,
and can be the symbols t or nil to cache everything or nothing
respectively.  It can also be a list of symbols:

- message: Cache conversation messages
- system: Cache the system message
- tool: Cache tool definitions

Examples:

Setting it to (message system) will cache the system message and
the conversation text.

Setting it to (message system tool) will cache everything and is
the same as t."
  :type '(choice
          (const :tag "Cache everything" t)
          (const :tag "Do not cache" nil)
          (repeat symbol))
  :group 'gptel)

(defvar gptel--known-backends)

(defconst gptel--openai-models
  '((gpt-4o
     :description "Advanced model for complex tasks; cheaper & faster than GPT-Turbo"
     :capabilities (media tool-use json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 128
     :input-cost 2.50
     :output-cost 10
     :cutoff-date "2023-10")
    (gpt-4o-mini
     :description "Cheap model for fast tasks; cheaper & more capable than GPT-3.5 Turbo"
     :capabilities (media tool-use json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 128
     :input-cost 0.15
     :output-cost 0.60
     :cutoff-date "2023-10")
    (gpt-4.1
     :description "Flagship model for complex tasks"
     :capabilities (media tool-use json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 1024
     :input-cost 2.0
     :output-cost 8.0
     :cutoff-date "2024-05")
    (gpt-4.5-preview
     :description "DEPRECATED: Use gpt-4.1 instead"
     :capabilities (media tool-use url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 128
     :input-cost 75
     :output-cost 150
     :cutoff-date "2023-10")
    (gpt-4.1-mini
     :description "Balance between intelligence, speed and cost"
     :capabilities (media tool-use json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 1024
     :input-cost 0.4
     :output-cost 1.6)
    (gpt-4.1-nano
     :description "Fastest, most cost-effective GPT-4.1 model"
     :capabilities (media tool-use json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 1024
     :input-cost 0.10
     :output-cost 0.40
     :cutoff-date "2024-05")
    (gpt-4-turbo
     :description "Previous high-intelligence model"
     :capabilities (media tool-use url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 128
     :input-cost 10
     :output-cost 30
     :cutoff-date "2023-11")
    (gpt-4
     :description "GPT-4 snapshot from June 2023 with improved function calling support"
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :capabilities (media url)
     :context-window 8.192
     :input-cost 30
     :output-cost 60
     :cutoff-date "2023-11")
    (gpt-5
     :description "Flagship model for coding, reasoning, and agentic tasks across domains"
     :capabilities (media tool-use json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 400
     :input-cost 1.25
     :output-cost 10
     :cutoff-date "2024-09")
    (gpt-5-mini
     :description "Faster, more cost-efficient version of GPT-5"
     :capabilities (media tool-use json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 400
     :input-cost 0.25
     :output-cost 2.0
     :cutoff-date "2024-09")
    (gpt-5-nano
     :description "Fastest, cheapest version of GPT-5"
     :capabilities (media tool-use json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 400
     :input-cost 0.05
     :output-cost 0.40
     :cutoff-date "2024-09")
    (gpt-5.1
     :description "The best model for coding and agentic tasks"
     :capabilities (media tool-use json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 400
     :input-cost 1.25
     :output-cost 10
     :cutoff-date "2024-09")
    (gpt-5.2
     :description "The best model for coding and agentic tasks"
     :capabilities (media tool-use json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 400
     :input-cost 1.75
     :output-cost 14
     :cutoff-date "2025-08")
    (o1
     :description "Reasoning model designed to solve hard problems across domains"
     :capabilities (media reasoning)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 200
     :input-cost 15
     :output-cost 60
     :cutoff-date "2023-10")
    (o1-mini
     :description "Faster and cheaper reasoning model good at coding, math, and science"
     :context-window 128
     :input-cost 3
     :output-cost 12
     :cutoff-date "2023-10"
     :capabilities (nosystem reasoning))
    (o3
     :description "Well-rounded and powerful model across domains"
     :capabilities (reasoning media tool-use json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 200
     :input-cost 2
     :output-cost 8
     :cutoff-date "2024-05")
    (o3-mini
     :description "High intelligence at the same cost and latency targets of o1-mini"
     :context-window 200
     :input-cost 1.10
     :output-cost 4.40
     :cutoff-date "2023-10"
     :capabilities (reasoning tool-use json))
    (o4-mini
     :description "Fast, effective reasoning with efficient performance in coding and visual tasks"
     :capabilities (reasoning media tool-use json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 200
     :input-cost 1.10
     :output-cost 4.40
     :cutoff-date "2024-05")
    (gpt-3.5-turbo
     :description "More expensive & less capable than GPT-4o-mini; use that instead"
     :capabilities (tool-use)
     :context-window 16.358
     :input-cost 0.50
     :output-cost 1.50
     :cutoff-date "2021-09")
    (gpt-3.5-turbo-16k
     :description "More expensive & less capable than GPT-4o-mini; use that instead"
     :capabilities (tool-use)
     :context-window 16.385
     :input-cost 3
     :output-cost 4
     :cutoff-date "2021-09"))
  "List of available OpenAI models and associated properties.
Keys:

- `:description': a brief description of the model.

- `:capabilities': a list of capabilities supported by the model.

- `:mime-types': a list of supported MIME types for media files.

- `:context-window': the context window size, in thousands of tokens.

- `:input-cost': the input cost, in US dollars per million tokens.

- `:output-cost': the output cost, in US dollars per million tokens.

- `:cutoff-date': the knowledge cutoff date.

- `:request-params': a plist of additional request parameters to
  include when using this model.

Information about the OpenAI models was obtained from the following
sources:

- <https://platform.openai.com/docs/pricing>
- <https://platform.openai.com/docs/models>")

(defcustom gptel-model 'gpt-4o-mini
  (concat
   "Model for chat.

The name of the model, as a symbol.  This is the name as expected
by the LLM provider's API.

To set the model for a chat session interactively call
`gptel-send' with a prefix argument.")
  :safe #'always
  :type `(choice
	  (symbol :tag "Specify model name")
	  ,@(mapcar (lambda (model)
		      (list 'const :tag (symbol-name (car model))
			    (car model)))
		    gptel--openai-models)))

(defvar gptel--openai
  (gptel-make-openai
      "ChatGPT"
    :key 'gptel-api-key
    :stream t
    :models gptel--openai-models))

(defcustom gptel-backend gptel--openai
  "LLM backend to use.

This is the default \"backend\", an object of type
`gptel-backend' containing connection, authentication and model
information.

A backend for ChatGPT is pre-defined by gptel.  Backends for
other LLM providers (local or remote) may be constructed using
one of the available backend creation functions:
- `gptel-make-openai'
- `gptel-make-azure'
- `gptel-make-ollama'
- `gptel-make-gpt4all'
- `gptel-make-gemini'
See their documentation for more information and the package
README for examples."
  :safe #'always
  :type `(choice
          (const :tag "ChatGPT" ,gptel--openai)
          (restricted-sexp :match-alternatives (gptel-backend-p 'nil)
			   :tag "Other backend")))

(defvar gptel-expert-commands nil
  "Whether experimental gptel options should be enabled.

This opens up advanced options in `gptel-menu'.")

(defvar gptel--num-messages-to-send nil)
(put 'gptel--num-messages-to-send 'safe-local-variable #'always)

(defcustom gptel-log-level nil
  "Logging level for gptel.

This is one of nil or the symbols info and debug:

nil: Don't log responses
info: Log request and response bodies
debug: Log request/response bodies, headers and all other
       connection settings.

When non-nil, information is logged to `gptel--log-buffer-name',
which see."
  :type '(choice
          (const :tag "No logging" nil)
          (const :tag "Limited" info)
          (const :tag "Full" debug)))

(defcustom gptel-track-response t
  "Distinguish between user messages and LLM responses.

When creating a prompt to send to the LLM, gptel distinguishes
between text entered by the user and past LLM responses.  This
distinction is necessary for back-and-forth conversation with an
LLM.

In regular Emacs buffers you can turn this behavior off by
setting `gptel-track-response' to nil.  All text, including
past LLM responses, is then treated as user input when sending
queries.

This variable has no effect in dedicated chat buffers (buffers
with `gptel-mode' enabled), where user prompts and responses are
always handled separately."
  :type 'boolean)

(defcustom gptel-track-media nil
  "Whether supported media in chat buffers should be sent.

When this is non-nil, gptel will send text, images or other media from
links in chat buffers to the LLM.

Sending images or other binary media from links requires the
active `gptel-model' to support it.  See `gptel-make-openai',
`gptel-make-anthropic', `gptel-make-ollama' or `gptel-make-gemini' for
details on how to specify media support for models.

This option has no effect in non-chat buffers.  To include
media (including images) more generally, use `gptel-add' or
`gptel-add-file'."
  :type 'boolean)

(defcustom gptel-use-context 'system
  "Where in the request to inject gptel's additional context.

gptel always includes the active region or the buffer up to the
cursor in the request to the LLM.  Additionally, you can add
other buffers or their regions to the context with
`gptel-add-context', or from gptel's menu.  This data will be
sent with every request.

This option controls whether and where this additional context is
included in the request.

Currently supported options are:

    nil     - Do not use the context.
    system  - Include the context with the system message.
    user    - Include the context with the user prompt."
  :group 'gptel
  :type '(choice
          (const :tag "Don't include context" nil)
          (const :tag "With system message" system)
          (const :tag "With user prompt" user)))

(defcustom gptel-include-reasoning t
  "How to handle LLM reasoning or \"thinking\" text blocks.

Some LLMs include in their response a \"thinking\" section.  This
text improves the quality of the LLM's final output, but may not
be interesting to you by itself.

Supported options are the symbols

    t       - Include with the response, the default
    nil     - Do not include
    ignore  - Include with the response but ignore on subsequent
              conversation turns

It can also be a string naming a buffer, in which case the
reasoning text will be inserted at the end of that buffer."
  :group 'gptel
  :type '(choice
          (const :tag "Include with response" t)
          (const :tag "Don't include" nil)
          (const :tag "Include but ignore" ignore)
          (string :tag "Include in buffer")))

(define-obsolete-variable-alias 'gptel-context--alist 'gptel-context
  "0.9.9.3")

(defcustom gptel-context nil
  "List of gptel's context sources.

The items in this list (file names or buffers) are included with gptel
queries as additional context.

Each entry can be a file path (string) or a buffer (object, not buffer
name):

 \\='(\"~/path/to/file1\"
   \"./file2\"
   #<buffer *scratch*>
   ...)

The above covers the most common cases.  You can also specify context
sources in a more targeted way, with entries of the form

  (<buffer> . spec)
  (\"/path/to/file\" . spec)

where spec is a plist declaring specific parts of the buffer/file to
include instead of the entire text.

For buffers, you can specify regions to include using buffer spans and
line number ranges as conses, and overlays as a list:

  (<buffer> :bounds ((start1 . end1) (start2 . end2) ...)
            :lines  ((from1 . to1) (from2 . end2) ...)
            :overlays (ov1 ov2 ...))

For files, spec can include buffer spans and line number ranges, as well as
the MIME type of the file:

  (\"/path/to/file\" :bounds ((start1 . end1) (start2 . end2) ...)
                   :lines  ((from1 . to1) (from2 . end2) ...)
                   :mime \"image/png\")

gptel tries to guess file MIME types, but is not always successful, so
it is recommended to provide it with non-text files.

Usage of context commands (such as `gptel-add' and `gptel-add-file')
will modify this variable.  You can also set this variable
buffer-locally, or let-bind it around calls to gptel queries, or via
gptel presets with the :context key."
  :type '(repeat string))

(defcustom gptel-markdown-validate-link #'always
  "Validate links to be sent as context with gptel queries.

When `gptel-track-media' is enabled, this option determines if a
supported link will be followed and its source included with gptel
queries from Markdown buffers.  Currently only links to files are
supported (along with web URLs if the model supports them).

It should be a function that accepts a Markdown link and return non-nil
if the link should be followed.  See `markdown-link-at-pos' for the
structure of a Markdown link object.

By default, all links are considered valid.

Set this to `gptel--link-standalone-p' to only follow links placed on a
line by themselves, separated from surrounding text."
  :type '(choice
          (const :tag "All links" always)
          (const :tag "Standalone links" gptel--link-standalone-p)
          (function :tag "Function"))
  :group 'gptel)

(defvar gptel--request-alist nil
  "Alist of active gptel requests.
Each entry has the form (PROCESS . (FSM ABORT-CLOSURE))
If the ABORT-CLOSURE is called, it must abort the PROCESS.")

(defvar gptel--request-params nil
  "Extra parameters sent with each gptel request.

These parameters are combined with model-specific and backend-specific
:request-params before sending a request, which see.  Warning: values
incompatible with the active backend can break gptel.  Do not use this
variable unless you know what you're doing!")

(defconst gptel--ersatz-json-tool "response_json"
  "Name of ersatz tool used to force JSON output.

Some APIs, like Anthropic, use a tool to produce structured JSON output.")

(defcustom gptel-curl-extra-args nil
  "Extra arguments to pass to Curl when sending queries.

This should be a list of strings, each one a Curl command line
argument.  Note that these should not conflict with the options
in `gptel-curl--common-args', which gptel requires for correct
functioning.

If you want to specify extra arguments only when using a specific
gptel backend, use the `:curl-args' slot of the backend instead.
See `gptel-backend'."
  :group 'gptel
  :type '(repeat string))

(defconst gptel-curl--common-args
  (if (memq system-type '(windows-nt ms-dos))
      '("--disable" "--location" "--silent" "-XPOST"
        "-y7200" "-Y1" "-D-")
    '("--disable" "--location" "--silent" "--compressed"
      "-XPOST" "-y7200" "-Y1" "-D-"))
  "Arguments always passed to Curl for gptel queries.")

(defvar gptel--link-type-cache nil
  "Cache of checks for binary files.

Each alist entry maps an absolute file path to a cons cell of the
form (t . binaryp), where binaryp is non-nil if the file is
binary-encoded.")

;; The following is derived from:
;;
;; (concat "\\(?:" markdown-regex-link-inline "\\|" markdown-regex-angle-uri "\\)")
;;
;; Since we want this known at compile time, when markdown-mode is not
;; guaranteed to be available, we have to hardcode it.
(defconst gptel-markdown--link-regex
  "\\(?:\\(?1:!\\)?\\(?2:\\[\\)\\(?3:\\^?\\(?:\\\\\\]\\|[^]]\\)*\\|\\)\\(?4:\\]\\)\\(?5:(\\)\\s-*\\(?6:[^)]*?\\)\\(?:\\s-+\\(?7:\"[^\"]*\"\\)\\)?\\s-*\\(?8:)\\)\\|\\(<\\)\\([a-z][a-z0-9.+-]\\{1,31\\}:[^]	\n<>,;()]+\\)\\(>\\)\\)"
  "Link regex for `gptel-mode' in Markdown mode.")


;;; Utility functions

(defun gptel-api-key-from-auth-source (&optional host user)
  "Lookup api key in the auth source.
By default, the LLM host for the active backend is used as HOST,
and \"apikey\" as USER."
  (if-let* ((secret
             (plist-get
              (car (auth-source-search
                    :host (or host (gptel-backend-host gptel-backend))
                    :user (or user "apikey")
                    :require '(:secret)))
              :secret)))
      (if (functionp secret)
          (encode-coding-string (funcall secret) 'utf-8)
        secret)
    (user-error "No `gptel-api-key' found in the auth source")))

;; FIXME Should we utf-8 encode the api-key here?
(defun gptel--get-api-key (&optional key)
  "Get api key from KEY, or from `gptel-api-key'."
  (when-let* ((key-sym (or key (gptel-backend-key gptel-backend))))
    (cl-typecase key-sym
      (function (string-trim-right (funcall key-sym) "[\n\r]+"))
      (string (string-trim-right key-sym "[\n\r]+"))
      (symbol (if-let* ((val (symbol-value key-sym)))
                  (gptel--get-api-key val)
                (error "`gptel-api-key' is not valid")))
      (t (error "`gptel-api-key' is not valid")))))

(defsubst gptel--to-number (val)
  "Ensure VAL is a number."
  (cond
   ((numberp val) val)
   ((stringp val) (string-to-number val))
   ((error "%S cannot be converted to a number" val))))

(defsubst gptel--to-string (s)
  "Convert S to a string, if possible."
  (cl-typecase s
    (symbol (symbol-name s))
    (string s)
    (otherwise (prin1-to-string s))))

(defsubst gptel--intern (s)
  "Intern S, if possible."
  (cl-etypecase s
    (symbol s)
    (string (intern s))))

(defun gptel--merge-plists (&rest plists)
  "Merge PLISTS, altering the first one.

Later plists in the sequence take precedence over earlier ones."
  (let (;; (rtn (copy-sequence (pop plists)))
        (rtn (pop plists))
        p v ls)
    (while plists
      (setq ls (pop plists))
      (while ls
        (setq p (pop ls) v (pop ls))
        (setq rtn (plist-put rtn p v))))
    rtn))

(defun gptel--file-binary-p (path)
  "Check if file at PATH is readable and binary."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents path nil 1 512 'replace)
        (memq buffer-file-coding-system
              '(no-conversion no-conversion-multibyte)))
    (file-missing (message "File \"%s\" is not readable." path)
                  nil)))

(defun gptel--insert-file-string (path)
  "Insert at point the contents of the file at PATH as context."
  (insert (format "In file `%s`:" (abbreviate-file-name path))
          "\n\n```\n")
  (let ((pm (point-marker)))
    (set-marker-insertion-type pm t)
    (insert-file-contents path)
    (goto-char pm))
  (insert "\n```\n"))

(defun gptel--strip-mode-suffix (mode-sym)
  "Remove the -mode suffix from MODE-SYM.

MODE-SYM is typically a major-mode symbol."
  (or (alist-get mode-sym gptel--mode-description-alist)
      (let ((mode-name (thread-last
                         (symbol-name mode-sym)
                         (string-remove-suffix "-mode")
                         (string-remove-suffix "-ts"))))
        ;; NOTE: The advertised calling convention of provided-mode-derived-p
        ;; has changed in Emacs 30, this needs to be updated eventually
        (if (provided-mode-derived-p
             mode-sym 'prog-mode 'text-mode 'tex-mode)
            mode-name ""))))

(defvar url-http-end-of-headers)
(defvar url-http-response-status)
(cl-defun gptel--url-retrieve (url &key method data headers)
  "Retrieve URL synchronously with METHOD, DATA and HEADERS."
  (declare (indent 1))
  (let ((url-request-method (if (eq method 'post) "POST" "GET"))
        (url-request-data (when (eq method 'post) (encode-coding-string (gptel--json-encode data) 'utf-8)))
        (url-mime-accept-string "application/json")
        (url-request-extra-headers
         `(("content-type" . "application/json")
           ,@headers)))
    (with-current-buffer (url-retrieve-synchronously url 'silent)
      (goto-char url-http-end-of-headers)
      (gptel--json-read))))

(defsubst gptel-prompt-prefix-string ()
  "Prefix before user prompts in `gptel-mode'."
  (declare (side-effect-free t))
  (or (alist-get major-mode gptel-prompt-prefix-alist) ""))

(defsubst gptel-response-prefix-string ()
  "Prefix before LLM responses in `gptel-mode'."
  (declare (side-effect-free t))
  (or (alist-get major-mode gptel-response-prefix-alist) ""))

(defmacro gptel--at-word-end (&rest body)
  "Execute BODY at end of the current word or punctuation."
  `(save-excursion
     (skip-syntax-forward "w.")
     ,(macroexp-progn body)))

(defmacro gptel--temp-buffer (buf)
  "Generate a temp buffer BUF.

Compatibility macro for Emacs 27.1."
  (if (< emacs-major-version 28)
      `(generate-new-buffer ,buf)
    `(generate-new-buffer ,buf t)))

;; This is defined in gptel, but we define it here as well as it's required by
;; `gptel--with-buffer-copy'.
(defvar gptel-mode nil)

(defmacro gptel--with-buffer-copy (buf start end &rest body)
  "Copy gptel's local variables from BUF to a temp buffer and run BODY.

If positions START and END are provided, insert that part of BUF first."
  (declare (indent 3))
  `(gptel--with-buffer-copy-internal ,buf ,start ,end (lambda () ,@body)))

(defun gptel--with-buffer-copy-internal (buf start end body-thunk)
  "Prepare a temp buffer for a gptel request.

For BUF, START, END and BODY-THUNK see `gptel--with-buffer-copy'."
  (let ((temp-buffer (gptel--temp-buffer " *gptel-prompt*")))
    (with-current-buffer temp-buffer
      (dolist (sym '( gptel-backend gptel--system-message gptel-model
                      gptel-mode gptel-track-response gptel-track-media
                      gptel-use-tools gptel-tools gptel-use-curl gptel--schema
                      gptel-use-context gptel-context gptel--num-messages-to-send
                      gptel-stream gptel-include-reasoning gptel--request-params
                      gptel-temperature gptel-max-tokens gptel-cache))
        (set (make-local-variable sym) (buffer-local-value sym buf)))
      (when (and start end) (insert-buffer-substring buf start end))
      (setq major-mode (buffer-local-value 'major-mode buf))
      (funcall body-thunk))))

(defsubst gptel--trim-prefixes (s)
  "Remove prompt/response prefixes from string S.

Return nil if string collapses to empty string."
  (let* ((trimmed (string-trim-left
                   s (format "[\t\r\n ]*\\(?:%s\\)?[\t\r\n ]*"
                             (regexp-quote
                              (gptel-prompt-prefix-string)))))
         (trimmed (string-trim-right
                   trimmed (format "[\t\r\n ]*\\(?:%s\\)?[\t\r\n ]*"
                                   (regexp-quote
                                    (gptel-response-prefix-string))))))
    (unless (string-empty-p trimmed)
      trimmed)))

(defun gptel--link-standalone-p (link)
  "Return non-nil if Markdown LINK is isolated.

This means the extent from the link beginning to end is the only
non-whitespace content on its line."
  (let ((beg (car link)) (end (cadr link)))
    (save-excursion
      (and (= beg (progn (goto-char beg) (beginning-of-line)
                         (skip-chars-forward "\t ")
                         (point)))
           (= end (progn (goto-char end) (end-of-line)
                         (skip-chars-backward "\t ")
                         (point)))))))

(defsubst gptel--curl-path ()
  "Curl executable to use."
  (if (stringp gptel-use-curl) gptel-use-curl "curl"))

(defun gptel--transform-add-context (callback fsm)
  (if (and gptel-use-context gptel-context)
      (gptel-context--wrap callback (plist-get (gptel-fsm-info fsm) :data))
    (funcall callback)))

;;;; Model interface
;; NOTE: This interface would be simpler to implement as a defstruct.  But then
;; users cannot set `gptel-model' to a symbol/string directly, or we'd need
;; another map from these symbols to the actual model structs.

(defsubst gptel--model-name (model)
  "Get name of gptel MODEL."
  (gptel--to-string model))

(defsubst gptel--model-capabilities (model)
  "Get MODEL capabilities."
  (get model :capabilities))

(defsubst gptel--model-mimes (model)
  "Get supported mime-types for MODEL."
  (get model :mime-types))

(defsubst gptel--model-capable-p (cap &optional model)
  "Return non-nil if MODEL supports capability CAP."
  (memq cap (gptel--model-capabilities
             (or model gptel-model))))

;; TODO Handle model mime specifications like "image/*"
(defsubst gptel--model-mime-capable-p (mime &optional model)
  "Return non nil if MODEL can understand MIME type."
  (car-safe (member mime (gptel--model-mimes
                          (or model gptel-model)))))

(defsubst gptel--model-request-params (model)
  "Get model-specific request parameters for MODEL."
  (get model :request-params))

;;;; File handling
(defun gptel--base64-encode (file)
  "Encode FILE as a base64 string.

FILE is assumed to exist and be a regular file."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (base64-encode-region (point-min) (point-max)
                          :no-line-break)
    (buffer-string)))

;;;; Directive handling

(defvar gptel--system-message
  (or (alist-get 'default gptel-directives)
      "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
  "The system message used by gptel.")
(put 'gptel--system-message 'safe-local-variable #'always)

(defun gptel--describe-directive (directive width &optional replacement)
  "Find description for DIRECTIVE, truncated  to WIDTH.

DIRECTIVE is a gptel directive, and can be a string, a function
or a list of strings.  See `gptel-directives'.

The result is a string intended for display.  Newlines are
replaced with REPLACEMENT."
  (cl-typecase directive
    (string
     (string-replace
      "\n" (or replacement " ")
      (substring directive 0 (min width (length directive)))))
    (function
     (concat
      "λ: "
      (string-replace
       "\n" (or replacement " ")
       (truncate-string-to-width
        (or (and-let* ((doc (documentation directive)))
              (substring doc nil (string-match-p "\n" doc)))
            "[Dynamically generated; no preview available]")
        width nil nil t))))
    (list (and-let* ((from-template (car directive)))
            (gptel--describe-directive
             from-template width)))
    (t "")))

(defun gptel--parse-directive (directive &optional raw)
  "Parse DIRECTIVE into a backend-appropriate form.

DIRECTIVE is a gptel directive: it can be a string, a list or a
function that returns either, see `gptel-directives'.

Return a cons cell consisting of the system message (a string)
and a template consisting of alternating user/LLM
records (a list of strings or nil).

If RAW is non-nil, the user/LLM records are not processed and are
returned as a list of strings."
  (and directive
       (cl-etypecase directive
         (string   (list directive))
         (function (gptel--parse-directive (funcall directive) raw))
         (cons     (if raw directive
                     (cons (car directive)
                           ;; FIXME(augment) do this elsewhere
                           (gptel--parse-list
                            gptel-backend (cdr directive))))))))


;;; Logging

(defconst gptel--log-buffer-name "*gptel-log*"
  "Log buffer for gptel.")

(declare-function json-pretty-print "json")

(defun gptel--log (data &optional type no-json)
  "Log DATA to `gptel--log-buffer-name'.

TYPE is a label for data being logged.  DATA is assumed to be
Valid JSON unless NO-JSON is t."
  (with-current-buffer (get-buffer-create gptel--log-buffer-name)
    (let ((p (goto-char (point-max))))
      (unless (bobp) (insert "\n"))
      (insert (format "{\"gptel\": \"%s\", " (or type "none"))
              (format-time-string "\"timestamp\": \"%Y-%m-%d %H:%M:%S\"}\n")
              data)
      (unless no-json (ignore-errors (json-pretty-print p (point)))))))


;;; Structured output
(defvar gptel--schema nil
  "Response output schema for backends that support it.")

(cl-defgeneric gptel--parse-schema (_backend _schema)
  "Parse JSON schema in a backend-appropriate way.")

(defun gptel--dispatch-schema-type (schema)
  "Convert SCHEMA to a valid elisp representation.

SCHEMA can be specified in several ways:
- As a plist readable by `gptel--json-encode'
  Ex: (:type object :properties (:key1 (:type number :description \"...\")
                                 :key2 (:type string)))

- As a serialized JSON string, which will be passed as-is.

- In shorthand form #1, a single-line comma-separated string with object
  keys and (optionally) types:
  Ex: \"key1, key2 number\"
  Ex: \"key1 string, key2 int\"
  The default type is string, and types can be shortened (integer -> int) as
  long as they match a JSON schema type uniquely.

- In shorthand form #2, a multi-line string with keys, (optionally) types and
  (optionally) descriptions
  Ex: \"key1: description 1 here
       key2 integer: description 2 here\"

- Shorthand forms can be placed inside [ and ] to specify an array of
  objects:
  Ex: \"[key1, key2 number]\"
  Ex: \"[key1: description 1 here
        key2 int: description 2 here]\""
  (when (stringp schema)  ;Two possibilities: serialized JSON, or shorthand form
    (let (wrap-in-array)  ;Flag to wrap the object type in an array
      (with-temp-buffer   ;Parser for (possibly) shorthand forms
        (insert schema)
        (goto-char (point-min)) (skip-chars-forward " \n\r\t")
        (if (= (char-after) ?{)
            (setq schema (gptel--json-read)) ;Assume serialized JSON schema, we're done
          (when (= (char-after) ?\[)    ;Shorthand: assume array top-level type
            (save-excursion
              (goto-char (point-max)) (skip-chars-backward " \n\r\t") (delete-char -1))
            (delete-char 1)             ;Delete array markers [ and ]
            (setq wrap-in-array t))
          (let ( props types descriptions ;Nested object and array types are disallowed in shorthand
                 (all-types '("number" "string" "integer" "boolean" "null")))
            (if (= (point-max) (line-end-position)) ; Single or multi-line?
                ;; Single line format (type optional): "key1 type, key2, ..."
                (while (re-search-forward ",?\\([^ ,]+\\) *\\([^ ,]*\\]?\\)" nil t)
                  (push (match-string 1) props)
                  (push (if (string-empty-p (match-string 2))
                            "string" (car (all-completions (match-string 2) all-types)))
                        types)
                  (push nil descriptions))
              ;; Multi-line format (type, description optional):
              ;; "key1 type: description1 \n key2: description2..."
              (while (re-search-forward "\\([^ :]+\\) *\\([^ :]*\\):?"
                                        (line-end-position) t)
                (push (match-string 1) props)
                (push (if (string-empty-p (match-string 2))
                          "string" (car (all-completions (match-string 2) all-types)))
                      types)
                (skip-chars-forward " \t")
                (push (if (eolp) nil (buffer-substring-no-properties
                                      (point) (line-end-position)))
                      descriptions)
                (forward-line 1)))
            (let ((object
                   (list :type "object"
                         :properties
                         (cl-mapcan
                          (lambda (prop type desc)
                            `(,(intern (concat ":" prop))
                              (:type ,type ,@(when desc
                                               (list :description (string-trim desc))))))
                          (nreverse props) (nreverse types) (nreverse descriptions)))))
              (setq schema
                    (if wrap-in-array (list :type "array" :items object) object))))))))
  ;; The OpenAI and Anthropic APIs don't allow arrays at the root of the schema.
  ;; Work around this by wrapping it in an object with the field "items".
  ;; TODO(schema): Find some way to strip this extra layer from the response.
  (if (member (plist-get schema :type) '("array" array))
      (list :type "object"
            :properties (list :items schema)
            :required ["items"]
            :additionalProperties :json-false)
    schema))

(defun gptel--preprocess-schema (spec)
  "Set additionalProperties for objects in SPEC destructively.

Convert symbol :types to strings."
  ;; NOTE: Do not use `sequencep' here, as that covers strings too and breaks
  ;; things.
  (when (or (listp spec) (vectorp spec))
    (cond
     ((vectorp spec)
      (cl-loop for element across spec
               for idx upfrom 0
               do (aset spec idx (gptel--preprocess-schema element))))
     ((keywordp (car spec))
      (let ((tail spec))
        (while tail
          (when (eq (car tail) :type)
            (when (symbolp (cadr tail)) ;Convert symbol :type to string
              (setcar (cdr tail) (symbol-name (cadr tail))))
            (when (equal (cadr tail) "object") ;Add additional object fields
              (plist-put tail :additionalProperties :json-false)
              (let ((vprops (vconcat
                             (cl-loop
                              for prop in (plist-get tail :properties) by #'cddr
                              collect (substring (symbol-name prop) 1)))))
                (plist-put tail :required vprops)
                (plist-put tail :propertyOrdering vprops))))
          (when (or (listp (cadr tail)) (vectorp (cadr tail)))
            (gptel--preprocess-schema (cadr tail)))
          (setq tail (cddr tail)))))
     ((listp spec) (dolist (element spec)
                     (when (listp element)
                       (gptel--preprocess-schema element))))))
  spec)


;;; Tool use

(defcustom gptel-use-tools t
  "Whether gptel should use tools.

Tools are capabilities provided by you to the LLM as functions an
LLM can choose to call.  gptel runs the function call on your
machine.

If set to t, any tools selected in variable `gptel-tools' will be made
available to the LLM.  This is the default.  It has no effect if no
tools are selected.

If set to force, gptel will try to force the LLM to call one or
more of the provided tools.  Support for this feature depends on
the backend/API, and gptel will fall back to the default behavior
when forcing tool use is unsupported.

If nil, tool use is turned off."
  :type '(choice
          (const :tag "Enable" t)
          (const :tag "Force tool use" force)
          (const :tag "Turn Off" nil)))

(defcustom gptel-confirm-tool-calls 'auto
  "Whether tool calls should wait for the user to run them.

If set to t or nil, tool calls always or never seek confirmation
from the user before running.

If set to the symbol auto (the default), a tool call will seek
confirmation only when the corresponding tool spec has a non-nil
:confirm slot.  See `gptel-make-tool'."
  :type '(choice
          (const :tag "Tool decides" auto)
          (const :tag "Always" t)
          (const :tag "Never" nil)))

(defcustom gptel-include-tool-results 'auto
  "Whether tool call results should be included in the buffer.

If set to t or nil, results of tool calls are always or never
included in the LLM response, respectively.

If set to the symbol auto (the default), a tool call result is
included only when the corresponding tool spec has a non-nil
:include slot.  See `gptel-make-tool'."
  :type '(choice
          (const :tag "Tool decides" auto)
          (const :tag "Always" t)
          (const :tag "Never" nil)))

(defcustom gptel-tools nil
  "A list of tools to include with gptel requests.

Each tool should be a `gptel-tool' struct, which see.  To specify
a tool, use `gptel-make-tool', which see."
  :group 'gptel
  :type '(repeat gptel-tool))

(cl-defstruct (gptel-tool (:constructor nil)
                          (:constructor gptel--make-tool-internal
                                        (&key function name description args
                                              async category confirm include
                                              &allow-other-keys))
                          (:copier gptel--copy-tool))
  "Struct to specify tools for LLMs to run.

A tool is a function specification sent to the LLM along with
a (plain language) task.  If the LLM decides to use the tool to
accomplish the task, gptel will run the tool and (optionally)
feed the LLM the results.  You can add tools via
`gptel-make-tool', which see."
  (function nil :type function :documentation "Function that runs the tool")
  (name nil :type string :documentation "Tool name, snake_case recommended")
  (description nil :type string :documentation "What the tool does, intended for the LLM")
  (args nil :type list :documentation "List of plists specifying function arguments")
  (async nil :type boolean :documentation "Whether the function runs asynchronously")
  (category nil :type string :documentation "Use to group tools by purpose")
  (confirm nil :type boolean :documentation "Seek confirmation before running tool?")
  (include nil :type boolean :documentation "Include tool results in buffer?"))

(defun gptel--preprocess-tool-args (spec)
  "Convert symbol :type values in tool SPEC to strings destructively."
  ;; NOTE: Do not use `sequencep' here, as that covers strings too and breaks
  ;; things.
  (when (or (listp spec) (vectorp spec))
    (cond
     ((vectorp spec)
      (cl-loop for element across spec
               for idx upfrom 0
               do (aset spec idx (gptel--preprocess-tool-args element))))
     ((keywordp (car spec))
      (let ((tail spec))
        (while tail
          (when (and (eq (car tail) :type) (symbolp (cadr tail)))
            (setcar (cdr tail) (symbol-name (cadr tail))))
          ;; TODO: Handle :enum ("provided" "as" "list") here, convert to
          ;; :enum ["provided" "as" "array"]
          (when (or (listp (cadr tail)) (vectorp (cadr tail)))
            (gptel--preprocess-tool-args (cadr tail)))
          (setq tail (cddr tail)))))
     ((listp spec) (dolist (element spec)
                     (when (listp element)
                       (gptel--preprocess-tool-args element))))))
  spec)

(defun gptel--make-tool (&rest spec)
  "Construct a gptel-tool according to SPEC."
  (gptel--preprocess-tool-args (plist-get spec :args))
  (apply #'gptel--make-tool-internal spec))

(defvar gptel--known-tools nil
  "Alist of gptel tools arranged by category.

A \"tool\" is a function spec (definition and description)
provided by gptel to an LLM.  See `gptel-tool'.  Each tool is
assigned a category when it is created, with a category of
\"misc\" if none is specified.

This is a two-level alist mapping categories and tool names to
the tool itself.  It is used as a global register of available
tools and in gptel's UI, see variable `gptel-tools'.

In this example structure, cat-tool and the rest are cl-structs
of type `gptel-tool':

   CATEGORY         TOOL NAME          TOOL
 ((\"filesystem\" . ((\"read_file\"      . cat-tool)
                   (\"list_directory\" . ls-tool)))
  (\"emacs\"      . ((\"read_buffer\"    . buffer-substring-tool)
                   (\"send_message\"   . message-tool))))

This variable is for internal use only, to define a tool use
`gptel-make-tool'.")

(defun gptel-get-tool (path)
  "Find tool in gptel's tool registry at PATH.

PATH can be specified
- as a string representing the tool name, like \"search_db\",
- or as a list representing a category and tool name,
  like \\='(\"emacs\" \"read_buffer\").
In both cases, the first matching gptel-tool is returned.

- as a string representing a category, like \"filesystem\".
In this case a list of all gptel-tools with this category is
returned."
  (or (cl-etypecase path
        (cons (let ((tc (map-nested-elt gptel--known-tools path)))
                (if (consp tc) (map-values tc) tc)))
        (string (if-let* ((category (assoc path gptel--known-tools)))
                    (map-values (cdr category))
                  (cl-loop for (_ . tools) in gptel--known-tools
                           if (assoc path tools)
                           return (cdr it)))))
      (error "No tool matches for %S" path)))

(defun gptel-make-tool (&rest slots)
  "Make a gptel tool for LLM use.

The following keyword arguments are available, of which the first
four SLOTS are required.

NAME: The name of the tool, recommended to be in Javascript style snake_case.

FUNCTION: The function itself (lambda or symbol) that runs the tool.

DESCRIPTION: A verbose description of what the tool does, how to
call it and what it returns.

ARGS: A list of plists specifying the arguments, or nil for a function that
takes no arguments.  Each plist in ARGS requires the following keys:
- argument :name and :description, as strings.
- argument :type, as a symbol.  Allowed types are those understood by the JSON
  schema: string, number, integer, boolean, array, object or null

The following plist keys are conditional/optional:
- :optional, boolean indicating if argument is optional
- :enum for enumerated types, whose value is a vector of strings representing
  allowed values.  Note that :type is still required for enums.
- :items, if the :type is array.  Its value must be a plist including at least
  the item's :type.
- :properties, if the type is object.  Its value must be a plist that can be
  serialized into a JSON object specification by `json-serialize'.

ASYNC: boolean indicating if the elisp function is asynchronous.
If ASYNC is t, the function should take a callback as its first
argument, along with the arguments specified in ARGS, and run the
callback with the tool call result when it's ready.  The callback
itself is an implementation detail and must not be included in
ARGS.

The following keys are optional

CATEGORY: A string indicating a category for the tool.  This is
used only for grouping in gptel's UI.  Defaults to \"misc\".

CONFIRM: Whether the tool call should wait for the user to run it.  If
true, the user will be prompted with the proposed tool call, which can
be examined, accepted, deferred or canceled.  It can also be a function
that receives the same arguments as FUNCTION and returns true if the
user should be prompted.

INCLUDE: Whether the tool results should be included as part of
the LLM output.  This is useful for logging and as context for
subsequent requests in the same buffer.  This is primarily useful
in chat buffers.

Here is an example definition:

  (gptel-make-tool
   :function (lambda (location unit)
                (url-retrieve-synchronously \"api.weather.com/...\"
                                            location unit))
   :name \"get_weather\"
   :description \"Get the current weather in a given location\"
   :args (list \\='(:name \"location\"
                 :type string
                 :description \"The city and state, e.g. San Francisco, CA\")
               \\='(:name \"unit\"
                 :type string
                 :enum [\"celsius\" \"farenheit\"]
                 :description
                 \"The unit of temperature, either \\='celsius\\=' or \\='fahrenheit\\='\"
                 :optional t)))

If the tool is asynchronous, the function is modified to take a
callback as its first argument, which it runs with the result:

   (lambda (callback location unit)
     (url-retrieve \"api.weather.com/...\"
                   (lambda (_)
                     (let ((result (parse-this-buffer)))
                       (funcall callback result)))))"
  (let* ((tool (apply #'gptel--make-tool slots))
         (category (or (gptel-tool-category tool) "misc")))
    (setf (alist-get
           (gptel-tool-name tool)
           (alist-get category gptel--known-tools nil nil #'equal)
           nil nil #'equal)
          tool)))

(cl-defgeneric gptel--parse-tools (_backend tools)
  "Parse TOOLS and return a list of prompts.

TOOLS is a list of `gptel-tool' structs, which see.

_BACKEND is the LLM backend in use.  This is the default
implementation, used by OpenAI-compatible APIs and Ollama."
  (vconcat
   (mapcar
    (lambda (tool)
      (list
       :type "function"
       :function
       (append
        (list
         :name (gptel-tool-name tool)
         :description (gptel-tool-description tool))
        (if (gptel-tool-args tool)
            (list
             :parameters
             (list :type "object"
                   ;; gptel's tool args spec is close to the JSON schema, except
                   ;; that we use (:name "argname" ...)
                   ;; instead of  (:argname (...)), and
                   ;; (:optional t) for each arg instead of (:required [...])
                   ;; for all args at once.  Handle this difference by
                   ;; modifying a copy of the gptel tool arg spec.
                   :properties
                   (cl-loop
                    for arg in (gptel-tool-args tool)
                    for argspec = (copy-sequence arg)
                    for name = (plist-get arg :name) ;handled differently
                    for newname = (or (and (keywordp name) name)
                                      (make-symbol (concat ":" name)))
                    do                ;ARGSPEC is ARG without unrecognized keys
                    (cl-remf argspec :name)
                    (cl-remf argspec :optional)
                    if (equal (plist-get arg :type) "object")
                    do (unless (plist-member argspec :required)
                         (plist-put argspec :required []))
                    (plist-put argspec :additionalProperties :json-false)
                    append (list newname argspec))
                   :required
                   (vconcat
                    (delq nil (mapcar
                               (lambda (arg) (and (not (plist-get arg :optional))
                                             (plist-get arg :name)))
                               (gptel-tool-args tool))))
                   :additionalProperties :json-false))
          (list :parameters (list :type "object" :properties nil))))))
    (ensure-list tools))))

(cl-defgeneric gptel--parse-tool-results (backend results)
  "Return a BACKEND appropriate prompt containing tool call RESULTS.

This will be injected into the messages list in the prompt to
send to the LLM.")

;; FIXME(fsm) unify this with `gptel--inject-media', which is a mess
(cl-defgeneric gptel--inject-prompt
    (_backend data new-prompt &optional _position)
  "Append NEW-PROMPT to existing prompts in query DATA.

NEW-PROMPT can be a single message or a list of messages.

Not implemented: if POSITION is
- a non-negative number, insert it at that position in PROMPTS.
- a negative number, insert it there counting from the end.

This generic implementation handles the Anthropic,
OpenAI-compatible and Ollama message formats."
  ;; ;TODO(fsm): implement _POSITION
  (when (keywordp (car-safe new-prompt)) ;Is new-prompt one or many?
    (setq new-prompt (list new-prompt)))
  (let ((prompts (plist-get data :messages)))
    (plist-put data :messages (vconcat prompts new-prompt))))


;;; State machine for driving requests
(defvar gptel-request--transitions
  `((INIT . ((t                       . WAIT)))
    (WAIT . ((t                       . TYPE)))
    (TYPE . ((,#'gptel--error-p       . ERRS)
             (,#'gptel--tool-use-p    . TOOL)
             (t                       . DONE)))
    (TOOL . ((,#'gptel--error-p       . ERRS)
             (,#'gptel--tool-result-p . WAIT)
             (t                       . DONE))))
  "Alist specifying gptel's default state transition table for requests.

Each entry is a list whose car is a request state (any symbol)
and whose cdr is an alist listing possible next states.  Each key
is either a predicate function or t.  When `gptel--fsm-next' is
called, the predicates are called in the order they appear here
to find the next state.  Each predicate is called with the state
machine's INFO, see `gptel-fsm'.  A predicate of t is
considered a success and acts as a default.")

(defvar gptel-request--handlers
  `((WAIT ,#'gptel--handle-wait)
    (TOOL ,#'gptel--handle-tool-use)
    (DONE ,#'gptel--handle-post)
    (ERRS ,#'gptel--handle-post)
    (ABRT ,#'gptel--handle-post))
  "Alist specifying handlers for gptel's default state transitions.

Each entry is a list whose car is a request state (a symbol) and
whose cdr is a list of handler functions called when
transitioning to that state.  The handlers are called in the
sequence that they appear in the list, and each function receives
the state machine as its only argument.  Information about the
request state can be retrieved via the machine's INFO slot, see
`gptel-fsm'.

Handlers are responsible for doing state-related tasks (like
logging errors or inserting responses) and transitioning to the
next state by calling `gptel--fsm-transition'.

Handlers can be asynchronous, in which case the transition call
should typically be placed in its callback.")

(cl-defstruct (gptel-fsm (:constructor gptel-make-fsm)
                         (:copier gptel-copy-fsm))
  "State machine for gptel requests.

STATE: The current state of the machine, can be any symbol.

TABLE: Alist mapping states to possible next states
along with predicates to determine the next state.  See
`gptel-request--transitions' for an example.

HANDLERS: Alist mapping states to state handler functions.
Handlers are called when entering each state.  See
`gptel-request--handlers' for an example

INFO: The state machine's current context.  This is a plist
holding all the information required for the ongoing request, and
can be used to tweak and resume a paused request.  This should be
called \"context\", but context means too many things already in
gptel's code!

Each gptel request is passed an instance of this
state machine and driven by it."
  (state 'INIT)
  (table gptel-request--transitions)
  (handlers gptel-request--handlers) info)

(defun gptel--fsm-transition (machine &optional new-state)
  "Move MACHINE to its next state.

MACHINE is an instance of `gptel-fsm'.

The next state is NEW-STATE if given.  Otherwise it is determined
automatically from MACHINE's transition table."
  (unless new-state (setq new-state (gptel--fsm-next machine)))
  (push (gptel-fsm-state machine)
        (plist-get (gptel-fsm-info machine) :history))
  (setf (gptel-fsm-state machine) new-state)
  (when-let* ((handlers (alist-get new-state (gptel-fsm-handlers machine))))
    (mapc (lambda (h) (funcall h machine)) handlers)))

(defun gptel--fsm-next (machine)
  "Determine MACHINE's next state according to its transition table.

MACHINE is an instance of `gptel-fsm'"
  (let* ((current (gptel-fsm-state machine))
         (transitions (alist-get current (gptel-fsm-table machine))))
    (cl-loop
     with info = (gptel-fsm-info machine)
     for (pred . next) in transitions
     when (or (eq pred t) (funcall pred info))
     return next)))

;;;; State machine handlers
;; The next few functions are default state handlers for gptel's state machine,
;; see `gptel-request--handlers'.

(defun gptel--handle-wait (fsm)
  "Fire the request contained in state machine FSM's info."
  ;; Reset some flags in info.  This is necessary when reusing fsm's context for
  ;; a second network request: gptel tests for the presence of these flags to
  ;; handle state transitions.  (NOTE: Don't add :token to this.)
  (let ((info (gptel-fsm-info fsm)))
    (dolist (key '(:tool-success :tool-use :error :http-status :reasoning))
      (when (plist-get info key)
        (plist-put info key nil))))
  (funcall
   (if gptel-use-curl
       #'gptel-curl-get-response
     #'gptel--url-get-response)
   fsm)
  (run-hooks 'gptel-post-request-hook))

(defun gptel--handle-tool-use (fsm)
  "Run tool calls captured in FSM, and advance the state machine with the results."
  (when-let* ((info (gptel-fsm-info fsm))
              (backend (plist-get info :backend))
              ;; This function might run many times, so only act on the remaining tool calls.
              (tool-use (cl-remove-if (lambda (tc) (plist-get tc :result))
                                      (plist-get info :tool-use)))
              (ntools (length tool-use))
              (tool-idx 0))
    (with-current-buffer (plist-get info :buffer)
      (let ((result-alist) (pending-calls))
        (mapc                           ; Construct function calls
         (lambda (tool-call)
           (letrec ((args (plist-get tool-call :args))
                    (name (plist-get tool-call :name))
                    (arg-values nil)
                    (tool-spec
                     (cl-find-if
                      (lambda (ts) (equal (gptel-tool-name ts) name))
                      (plist-get info :tools)))
                    (process-tool-result
                     (lambda (result)
                       (plist-put info :tool-success t)
                       (let ((result (gptel--to-string result)))
                         (plist-put tool-call :result result)
                         (push (list tool-spec args result) result-alist))
                       (cl-incf tool-idx)
                       (when (>= tool-idx ntools) ; All tools have run
                         (gptel--inject-prompt
                          backend (plist-get info :data)
                          (gptel--parse-tool-results
                           backend (plist-get info :tool-use)))
                         (funcall (plist-get info :callback)
                                  (cons 'tool-result result-alist) info)
                         (gptel--fsm-transition fsm)))))
             (if (null tool-spec)
                 (if (equal name gptel--ersatz-json-tool) ;Could be a JSON response
                     ;; Handle structured JSON output supplied as tool call
                     (funcall (plist-get info :callback)
                              (gptel--json-encode (plist-get tool-call :args))
                              info)
                   (message "Unknown tool called by model: %s" name))
               (setq arg-values
                     (mapcar
                      (lambda (arg)
                        (let ((key (intern (concat ":" (plist-get arg :name)))))
                          (plist-get args key)))
                      (gptel-tool-args tool-spec)))
               ;; Check if tool requires confirmation
               (if (and gptel-confirm-tool-calls
                        (or (eq gptel-confirm-tool-calls t) ;always confirm, or
                            (and-let* ((confirm (gptel-tool-confirm tool-spec)))
                              (or (not (functionp confirm)) (apply confirm arg-values)))))
                   (push (list tool-spec arg-values process-tool-result)
                         pending-calls)
                 ;; If not, run the tool
                 (if (gptel-tool-async tool-spec)
                     (apply (gptel-tool-function tool-spec)
                            process-tool-result arg-values)
                   (let ((result
                          (condition-case errdata
                              (apply (gptel-tool-function tool-spec) arg-values)
                            (error (mapconcat #'gptel--to-string errdata " ")))))
                     (funcall process-tool-result result)))))))
         tool-use)
        (when pending-calls
          (plist-put info :tool-pending t)
          (funcall (plist-get info :callback)
                   (cons 'tool-call pending-calls) info))))))

(defun gptel--handle-post (fsm)
  "Run cleanup for `gptel-request' with FSM."
  (when-let* ((info (gptel-fsm-info fsm))
              (post (plist-get info :post)))
    (mapc (lambda (f) (funcall f info)) post)))

;;;; State machine predicates
;; Predicates used to find the next state to transition to, see
;; `gptel-request--transitions'.

(defun gptel--error-p (info) (plist-get info :error))

(defun gptel--tool-use-p (info) (plist-get info :tool-use))

(defun gptel--tool-result-p (info) (plist-get info :tool-success))


;;; Send gptel requests
(cl-defun gptel-request
    (&optional prompt &key callback
               (buffer (current-buffer))
               position context dry-run
               (stream nil) (in-place nil)
               (system gptel--system-message)
               schema transforms (fsm (gptel-make-fsm)))
  "Request a response from the `gptel-backend' for PROMPT.

The request is asynchronous, this function returns immediately.

If PROMPT is
- a string, it is used to create a full prompt suitable for
  sending to the LLM.
- A list of strings, it is interpreted as a conversation, i.e. a
  series of alternating user prompts and LLM responses.
  (\"user msg 1\" \"llm msg 1\" \"user msg 2\" \"llm msg 2\" ...)
- nil but region is active, the region contents are used.
- nil, the current buffer's contents up to (point) are used.
  Previous responses from the LLM are identified as responses.

Keyword arguments:

CALLBACK, if supplied, is a function of two arguments, called
with the RESPONSE (usually a string) and INFO (a plist):

 (funcall CALLBACK RESPONSE INFO)

RESPONSE is

- A string if the request was successful
- nil if there was no response or an error.

These are the only two cases you typically need to consider,
unless you need to clean up after aborted requests, use LLM
tools, handle \"reasoning\" content specially or stream
responses (see STREAM).  In these cases, RESPONSE can be

- The symbol `abort' if the request is aborted, see `gptel-abort'.

- A cons cell of the form

  (tool-call . ((TOOL ARGS CB) ...))

  where TOOL is a gptel-tool struct, ARGS is a plist of
  arguments, and CB is a function for handling the results.  You
  can call CB with the result of calling the tool to continue the
  request.

- A cons cell of the form

  (tool-result . ((TOOL ARGS RESULT) ...))

  where TOOL is a gptel-tool struct, ARGS is a plist of
  arguments, and RESULT was returned from calling the tool
  function.

- A cons cell of the form

  (reasoning . text)

  where text is the contents of the reasoning block.  (Also see
  STREAM if you are using streaming.)

See `gptel--insert-response' for an example callback handling all
cases.

The INFO plist has (at least) the following keys:
:data         - The request data included with the query
:position     - marker at the point the request was sent, unless
                POSITION is specified.
:buffer       - The buffer current when the request was sent,
                unless BUFFER is specified.
:status       - Short string describing the result of the request,
                including possible HTTP errors.

Example of a callback that messages the user with the response
and info:

 (lambda (response info)
  (if (stringp response)
      (let ((posn (marker-position (plist-get info :position)))
            (buf  (buffer-name (plist-get info :buffer))))
        (message \"Response for request from %S at %d: %s\"
                 buf posn response))
    (message \"gptel-request failed with message: %s\"
             (plist-get info :status))))

Or, for just the response:

 (lambda (response _)
  ;; Do something with response
  (message (rot13-string response)))

If CALLBACK is omitted, the response is inserted at the point the
request was sent.

STREAM is a boolean that determines if the response should be
streamed, as in `gptel-stream'.  If the model or the backend does
not support streaming, this will be ignored.

When streaming responses

- CALLBACK will be called repeatedly with each RESPONSE text
  chunk (a string) as it is received.
- When the HTTP request ends successfully, CALLBACK will be
  called with a RESPONSE argument of t to indicate success.
- Similarly, CALLBACK will be called with
  (reasoning . text-chunk) for each reasoning chunk, and
  (reasoning . t) to indicate the end of the reasoning block.

BUFFER and POSITION are the buffer and position (integer or
marker) at which the response is inserted.  If a CALLBACK is
specified, no response is inserted and these arguments are
ignored, but they are still available in the INFO plist passed
to CALLBACK for you to use.

BUFFER defaults to the current buffer, and POSITION to the value
of (point) or (region-end), depending on whether the region is
active.

CONTEXT is any additional data needed for the callback to run. It
is included in the INFO argument to the callback.
Note: This is intended for storing Emacs state to be used by
CALLBACK, and unrelated to the context supplied to the LLM.

SYSTEM is the system message or extended chat directive sent to
the LLM.  This can be a string, a list of strings or a function
that returns either; see `gptel-directives' for more
information. If SYSTEM is omitted, the value of
`gptel--system-message' for the current buffer is used.

The following keywords are mainly for internal use:

IN-PLACE is a boolean used by the default callback when inserting
the response to determine if delimiters are needed between the
prompt and the response.

If DRY-RUN is non-nil, do not send the request.  Construct and
return a state machine object that can be introspected and
resumed.

TRANSFORMS is a list of functions used to transform the prompt or query
parameters dynamically.  Each function is called in a temporary buffer
containing the prompt to be sent, and can conditionally modify this
buffer.  This can include changing the (buffer-local) values of the
model, backend or system prompt, or augmenting the prompt with
additional information (such as from a RAG engine).

- Synchronous transformers are called with zero or one argument, the
  state machine for the request.

- Asynchronous transformers are called with two arguments, a callback
  and the state machine.  It should run the callback after finishing its
  transformation.

See `gptel-prompt-transform-functions' for more.

If provided, SCHEMA forces the LLM to generate JSON output.  Its value
is a JSON schema, which can be provided as
- an elisp object, a nested plist structure.
- A JSON schema serialized to a string
- A shorthand object/array description, see `gptel--dispatch-schema-type'.
See the manual or the wiki for examples.

Note: SCHEMA is presently experimental and subject to change, and not
all providers support structured output.

FSM is the state machine driving the request.  This can be used
to define a custom request control flow, see `gptel-fsm' for
details.  You can safely ignore this -- FSM is an unstable
feature and subject to change.

Note:

1. This function is not fully self-contained.  Consider
let-binding the parameters `gptel-backend', `gptel-model',
`gptel-use-tools' and `gptel-use-context' around calls to it as
required.

2. The return value of this function is a state machine that may
be used to rerun or continue the request at a later time."
  (declare (indent 1))
  ;; TODO Remove this check in version 1.0
  (gptel--sanitize-model)
  (let* ((start-marker
          (cond
           ((null position)
            (if (use-region-p)
                (set-marker (make-marker) (region-end))
              (gptel--at-word-end (point-marker))))
           ((markerp position) position)
           ((integerp position)
            (set-marker (make-marker) position buffer))))
         (gptel--schema schema)
         (prompt-buffer
          (cond                       ;prompt from buffer or explicitly supplied
           ((null prompt)
            (gptel--create-prompt-buffer (point)))
           ((stringp prompt)
            (gptel--with-buffer-copy buffer nil nil
              (insert prompt)
              (current-buffer)))
           ((consp prompt)
            ;; (gptel--parse-list gptel-backend prompt)
            (gptel--with-buffer-copy buffer nil nil
              ;; TEMP Decide on the annoated prompt-list format
              (gptel--parse-list-and-insert prompt)
              (current-buffer)))))
         (system-list (gptel--parse-directive system 'raw)) ;eval function-valued system prompts
         (info (list :data prompt-buffer
                     :buffer buffer
                     :position start-marker)))
    (when transforms (plist-put info :transforms transforms))
    (with-current-buffer prompt-buffer
      (setq gptel--system-message       ;guaranteed to be buffer-local
            ;; Retain single-part system messages as strings to avoid surprises
            ;; when applying presets
            (if (cdr system-list) system-list (car system-list))))
    (when stream (plist-put info :stream stream))
    ;; This context should not be confused with the context aggregation context!
    (when callback (plist-put info :callback callback))
    (when context (plist-put info :context context))
    (when in-place (plist-put info :in-place in-place))
    ;; Add info to state machine context
    (when dry-run (plist-put info :dry-run dry-run))
    (setf (gptel-fsm-info fsm) info))

  ;; TEMP: Augment in separate let block for now.  Are we overcapturing?
  ;; FIXME(augment): Call augmentors with INFO, not FSM
  (let ((info (gptel-fsm-info fsm)))
    (with-current-buffer (plist-get info :data)
      (setq-local gptel-prompt-transform-functions (plist-get info :transforms))
      ;; Preset has highest priority because it can change prompt-transform-functions
      (when (memq 'gptel--transform-apply-preset gptel-prompt-transform-functions)
        (gptel--transform-apply-preset fsm)
        (setq gptel-prompt-transform-functions ;avoid mutation, copy transforms
              (remq 'gptel--transform-apply-preset gptel-prompt-transform-functions)))
      (let ((augment-total              ;act like a hook, count total
             (if (memq t gptel-prompt-transform-functions)
                 (length
                  (setq gptel-prompt-transform-functions
                        (nconc (remq t gptel-prompt-transform-functions)
                               (default-value 'gptel-prompt-transform-functions))))
               (length gptel-prompt-transform-functions)))
            (augment-idx 0))
        (if (null gptel-prompt-transform-functions)
            (gptel--realize-query fsm)
          ;; FIXME(request-lib): Cannot use gptel--update-status from this file
          ;; (with-current-buffer (plist-get info :buffer) ;Apply prompt transformations
          ;;   (gptel--update-status " Augmenting..." 'mode-line-emphasis))

          ;; FIXME(augment): This needs to be converted into a linear callback
          ;; chain to avoid race conditions with multiple async augmentors.
          (run-hook-wrapped
           'gptel-prompt-transform-functions
           (lambda (func fsm-arg)
             (with-current-buffer (plist-get info :data)
               (goto-char (point-max))
               (if (= (car (func-arity func)) 2) ;async augmentor
                   (funcall func (lambda ()
                                   (cl-incf augment-idx)
                                   (when (>= augment-idx augment-total) ;All augmentors have run
                                     (gptel--realize-query fsm-arg)))
                            fsm-arg)
                 (if (= (car (func-arity func)) 0)
                     (funcall func)
                   (funcall func fsm-arg)) ;sync augmentor
                 (cl-incf augment-idx)
                 (when (>= augment-idx augment-total) ;All augmentors have run
                   (gptel--realize-query fsm-arg))))
             nil)           ;always return nil so run-hook-wrapped doesn't abort
           fsm)))))
  fsm)

(defun gptel--realize-query (fsm)
  "Realize the query payload for FSM from its prompt buffer.

Initiate the request when done."
  (let ((info (gptel-fsm-info fsm)))
    (with-current-buffer (plist-get info :data)
      (let* ((directive (gptel--parse-directive gptel--system-message 'raw))
             ;; DIRECTIVE contains both the system message and the template prompts
             (gptel--system-message
              (unless (gptel--model-capable-p 'nosystem) (car directive)))
             ;; TODO(tool) Limit tool use to capable models after documenting :capabilities
             ;; (gptel-use-tools (and (gptel--model-capable-p 'tool-use) gptel-use-tools))
             (stream (and (plist-get info :stream) gptel-use-curl gptel-stream
                          ;; Check model-specific request-params for streaming preference
                          (let* ((model-params (gptel--model-request-params gptel-model))
                                 (stream-spec (plist-get model-params :stream)))
                            ;; If not present, there is no model-specific preference
                            (or (not (memq :stream model-params))
                                ;; If present, it must not be :json-false or nil
                                (and stream-spec (not (eq stream-spec :json-false)))))
                          ;; Check backend-specific streaming settings
                          (gptel-backend-stream gptel-backend)))
             (gptel-stream stream)
             (full-prompt))
        (when (cdr directive)       ; prompt constructed from directive/template
          (save-excursion (goto-char (point-min))
                          (gptel--parse-list-and-insert (cdr directive))))
        (goto-char (point-max))
        (setq full-prompt (gptel--parse-buffer ;prompt from buffer or explicitly supplied
                           gptel-backend (and gptel--num-messages-to-send
                                              (* 2 gptel--num-messages-to-send))))
        ;; Inject media chunks into the first user prompt if required.  Media
        ;; chunks are always included with the first user message,
        ;; irrespective of the preference in `gptel-use-context'.  This is
        ;; because media cannot be included (in general) with system messages.
        ;; TODO(augment): Find a way to do this in the prompt-buffer?
        (when (and gptel-context gptel-use-context (gptel--model-capable-p 'media))
          (gptel--inject-media gptel-backend full-prompt))
        (unless stream (cl-remf info :stream))
        (plist-put info :backend gptel-backend)
        (plist-put info :model gptel-model)
        (when gptel-include-reasoning   ;Required for next-request-only scope
          (plist-put info :include-reasoning gptel-include-reasoning))
        (when (and gptel-use-tools gptel-tools)
          (plist-put info :tools gptel-tools))
        (plist-put info :data
                   (gptel--request-data gptel-backend full-prompt)))
      (kill-buffer (current-buffer)))
    ;; INIT -> WAIT
    (unless (plist-get info :dry-run) (gptel--fsm-transition fsm))
    fsm))

(defun gptel-abort (buf)
  "Stop any active gptel process associated with buffer BUF.

BUF defaults to the current buffer."
  (interactive (list (current-buffer)))
  (when-let* ((proc-attrs
               (cl-find-if
                (lambda (entry)
                  ;; each entry has the form (PROC . (FSM ABORT-FN))
                  (eq (thread-first (cadr entry) ; FSM
                                    (gptel-fsm-info)
                                    (plist-get :buffer))
                      buf))
                gptel--request-alist))
              (proc (car proc-attrs))
              (fsm (cadr proc-attrs))
              (info (gptel-fsm-info fsm))
              (abort-fn (cddr proc-attrs)))
    ;; Run :callback with abort signal
    (with-demoted-errors "Callback error: %S"
      (and-let* ((cb (plist-get info :callback))
                 ((functionp cb)))
        (funcall cb 'abort info)))
    (funcall abort-fn)
    (setf (alist-get proc gptel--request-alist nil 'remove) nil)
    (gptel--fsm-transition fsm 'ABRT)
    (message "Stopped gptel request in buffer %S" (buffer-name buf))))


;;; Prompt creation
(defun gptel--create-prompt-buffer (&optional prompt-end)
  "Return a buffer with the conversation prompt to be sent.

If the region is active limit the prompt text to the region contents.
Otherwise the prompt text is constructed from the contents of the
current buffer up to point, or PROMPT-END if provided."
  (save-excursion
    (save-restriction
      (let ((buf (current-buffer)))
        (cond
         ((derived-mode-p 'org-mode)
          (require 'gptel-org)
          ;; Also handles regions in Org mode
          (gptel-org--create-prompt-buffer prompt-end))
         ((use-region-p)
          (let ((rb (region-beginning)) (re (region-end)))
            (gptel--with-buffer-copy buf rb re
              (current-buffer))))
         (t (unless prompt-end (setq prompt-end (point)))
            (gptel--with-buffer-copy buf (point-min) prompt-end
              (current-buffer))))))))

(defun gptel--create-prompt (&optional prompt-end)
  "Return a full conversation prompt from the contents of this buffer.

If `gptel--num-messages-to-send' is set, limit to that many
recent exchanges.

If PROMPT-END (a marker) is provided, end the prompt contents
there.  This defaults to (point)."
  (with-current-buffer (gptel--create-prompt-buffer prompt-end)
    (unwind-protect
        (gptel--parse-buffer
         gptel-backend (and gptel--num-messages-to-send
                            (* 2 gptel--num-messages-to-send)))
      (kill-buffer (current-buffer)))))

(make-obsolete 'gptel--create-prompt 'gptel--create-prompt-buffer
               "0.9.9")

(cl-defgeneric gptel--parse-buffer (backend max-entries)
  "Parse current buffer backwards from point and return a list of prompts.

BACKEND is the LLM backend in use.

MAX-ENTRIES is the number of queries/responses to include for
contexbt.")

(defun gptel--parse-list-and-insert (prompts)
  "Insert PROMPTS, a list of messages into the current buffer.

Propertize the insertions in a format gptel can parse into a
conversation.

PROMPTS is typically the input to `gptel-request', either a list of strings
representing a conversation with alternate prompt/response turns, or a list of
lists with explicit roles (prompt/response/tool).  See the documentation of
`gptel-request' for the latter."
  (if (stringp (car prompts))           ; Simple format, list of strings
      (cl-loop for text in prompts
               for response = nil then (not response)
               when text
               if response
               do (insert gptel-response-separator
                          (propertize text 'gptel 'response)
                          gptel-response-separator)
               else do (insert text))
    (dolist (entry prompts)             ; Advanced format, list of lists
      (pcase entry
        (`(prompt . ,msg) (insert (or (car-safe msg) msg)))
        (`(response . ,msg)
         (insert gptel-response-separator
                 (propertize (or (car-safe msg) msg) 'gptel 'response)))
        (`(tool . ,call)
         (insert gptel-response-separator
                 (propertize
                  (concat
                   "(:name " (plist-get call :name) " :args "
                   (prin1-to-string (plist-get call :args)) ")\n\n"
                   (plist-get call :result))
                  'gptel `(tool . ,(plist-get call :id)))))))))

(cl-defgeneric gptel--parse-list (backend prompt-list)
  "Parse PROMPT-LIST and return a list of prompts for BACKEND.

PROMPT-LIST is interpreted as a conversation, i.e. an alternating
series of user prompts and LLM responses.  The returned structure
is suitable for including in the request payload.

BACKEND is the LLM backend in use.")

(cl-defgeneric gptel--parse-media-links (mode beg end)
  "Find media links between BEG and END.

MODE is the major-mode of the buffer.

Returns a plist where each entry is of the form
  (:text \"some text\")
or
  (:media \"media uri or file path\")."
  (ignore mode)                         ;byte-compiler
  (list `(:text ,(buffer-substring-no-properties
                  beg end))))

(declare-function markdown-link-at-pos "markdown-mode")
(declare-function mailcap-file-name-to-mime-type "mailcap")

(defsubst gptel-markdown--validate-link (link)
  "Validate a Markdown LINK as sendable under the current gptel settings.

Return a form (validp link-type path . REST), where REST is a list
explaining why sending the link is not supported by gptel.  Only the
first nil value in REST is guaranteed to be correct."
  (let ((mime))
    (if-let* ((path (nth 3 link))
              (prefix (or (string-search "://" path) 0))
              (link-type (if (= prefix 0) "file" (substring path 0 prefix)))
              (path (if (and (equal link-type "file") (> prefix 0))
                        (substring path (+ prefix 3)) path))
              (resource-type
               (or (and (equal link-type "file") 'file)
                   (and (gptel--model-capable-p 'url)
                        (member link-type '("http" "https" "ftp")) 'url)))
              (user-check (funcall gptel-markdown-validate-link link))
              (readablep (or (member link-type '("http" "https" "ftp"))
                             (file-remote-p path)
                             (file-readable-p path)))
              (mime-valid
               (if (or (eq resource-type 'url)
                       (cdr (with-memoization
                                (alist-get (expand-file-name path)
                                           gptel--link-type-cache
                                           nil nil #'string=)
                              (cons t (gptel--file-binary-p path)))))
                   (gptel--model-mime-capable-p
                    (setq mime (mailcap-file-name-to-mime-type path)))
                 t)))
        (list t link-type path resource-type user-check readablep mime-valid mime)
      (list nil link-type path resource-type user-check readablep mime-valid mime))))

(cl-defmethod gptel--parse-media-links ((_mode (eql 'markdown-mode)) beg end)
  "Parse text and actionable links between BEG and END.

Return a list of the form
 ((:text \"some text\")
  (:media \"/path/to/media.png\" :mime \"image/png\")
  (:text \"More text\"))
for inclusion into the user prompt for the gptel request."
  (let ((parts) (from-pt))
    (save-excursion
      (setq from-pt (goto-char beg))
      (while (re-search-forward gptel-markdown--link-regex end t)
        (let* ((link-at-pt (markdown-link-at-pos (point)))
               (link-status (gptel-markdown--validate-link link-at-pt)))
          (cl-destructuring-bind
              (valid type path resource-type user-check readablep mime-valid mime)
              link-status
            (cond
             ((and valid (member type '("http" "https" "ftp")))
              ;; Collect text up to this image, and collect this image url
              (let ((text (buffer-substring-no-properties from-pt (car link-at-pt))))
                (unless (string-blank-p text) (push (list :text text) parts))
                (push (list :url path :mime mime) parts)
                (setq from-pt (cadr link-at-pt))))
             (valid   ; Collect text up to this link, and collect this link data
              (let ((text (buffer-substring-no-properties from-pt (car link-at-pt))))
                (unless (string-blank-p text) (push (list :text text) parts))
                (push (if mime (list :media path :mime mime) (list :textfile path)) parts)
                (setq from-pt (cadr link-at-pt))))
             ((not resource-type)
              (message "Link source not followed for unsupported link type \"%s\"." type))
             ((not user-check)
              (message
               (if (eq gptel-markdown-validate-link 'gptel--link-standalone-p)
                   "Ignoring non-standalone link \"%s\"."
                 "Link %s failed to validate, see `gptel-markdown-validate-link'.")
               path))
             ((not readablep) (message "Ignoring inaccessible file \"%s\"." path))
             ((and (not mime-valid) (eq resource-type 'file))
              (message "Ignoring unsupported binary file \"%s\"." path)))))))
    (unless (= from-pt end)
      (push (list :text (buffer-substring-no-properties from-pt end)) parts))
    (nreverse parts)))

(cl-defgeneric gptel--inject-media (backend _prompts)
  "Wrap the last prompt in PROMPTS with gptel's context.

PROMPTS is a structure as returned by `gptel--parse-buffer'.
Typically this is a list of plists.

BACKEND is the gptel backend in use."
  (display-warning
   '(gptel context)
   (format "Context support not implemented for backend %s, ignoring context"
           (gptel-backend-name backend))))

(cl-defgeneric gptel--request-data (backend prompts)
  "Generate a plist of all data for an LLM query.

BACKEND is the LLM backend in use.

PROMPTS is the plist of previous user queries and LLM responses.")

(cl-defun gptel--sanitize-model (&key (backend gptel-backend)
                                      (model gptel-model)
                                      (shoosh t))
  "Check if MODEL is available in BACKEND, adjust accordingly.

If SHOOSH is true, don't issue a warning."
  (let ((available (gptel-backend-models backend)))
    (when (stringp model)
      (unless shoosh
        (display-warning
         'gptel
         (format "`gptel-model' expects a symbol, found string \"%s\"
   Resetting `gptel-model' to %s"
                 model model)))
      (setq gptel-model (gptel--intern model)
            model gptel-model))
    (unless (member model available)
      (let ((fallback (car available)))
        (unless shoosh
          (display-warning
           'gptel
           (format (concat "Preferred `gptel-model' \"%s\" not"
                           "supported in \"%s\", using \"%s\" instead")
                   model (gptel-backend-name backend) fallback)))
        (setq-local gptel-model fallback)))))


;;; url-retrieve response handling
(defun gptel--url-get-response (fsm)
  "Fetch response to prompt in state FSM from the LLM.

FSM is the state machine driving this request.  Its INFO slot
contains the data required for setting up the request.  INFO is a
plist with the following keys, among others:
- :data     (the data being sent)
- :buffer   (the gptel buffer)
- :position (marker at which to insert the response).
- :callback (optional, the request callback)

Call CALLBACK with the response and INFO afterwards.  If omitted
the response is inserted into the current buffer after point."
  (let* ((inhibit-message t)
         (message-log-max nil)
         (url-request-method "POST")
         (info (gptel-fsm-info fsm))
         ;; We have to let-bind the following two since their dynamic
         ;; values are used for key lookup and url resolution
         (gptel-backend (plist-get info :backend))
         (gptel-model (plist-get info :model))
         (url-request-extra-headers
          (append '(("Content-Type" . "application/json"))
                  (when-let* ((header (gptel-backend-header gptel-backend)))
                    (if (functionp header)
                        (funcall header) header))))
         (callback (or (plist-get info :callback) ;if not the first run
                       #'gptel--insert-response)) ;default callback
         ;; NOTE: We don't need the decode-coding-string dance here since we
         ;; don't pass it to the OS environment and Curl.
         (url-request-data
          (gptel--json-encode (plist-get info :data))))
    (when (with-current-buffer (plist-get info :buffer)
            (and (derived-mode-p 'org-mode)
                 gptel-org-convert-response))
      (plist-put info :transformer #'gptel--convert-markdown->org))
    (plist-put info :callback callback)
    (when gptel-log-level               ;logging
      (when (eq gptel-log-level 'debug)
        (gptel--log (gptel--json-encode
                     (mapcar (lambda (pair) (cons (intern (car pair)) (cdr pair)))
                             url-request-extra-headers))
                    "request headers"))
      (gptel--log url-request-data "request body"))
    (let ((proc-buf
           (url-retrieve (let ((backend-url (gptel-backend-url gptel-backend)))
                           (if (functionp backend-url)
                               (funcall backend-url) backend-url))
                         (lambda (_)
                           (set-buffer-multibyte t)
                           (set-buffer-file-coding-system 'utf-8-unix)
                           (pcase-let ((`(,response ,http-status ,http-msg ,error)
                                        (gptel--url-parse-response
                                         (plist-get info :backend) info))
                                       (buf (current-buffer)))
                             (plist-put info :http-status http-status)
                             (plist-put info :status http-msg)
                             (gptel--fsm-transition fsm) ;WAIT -> TYPE
                             (when error (plist-put info :error error))
                             (when response ;Look for a reasoning block
                               (if (string-match-p "^\\s-*<think>" response)
                                   (when-let* ((idx (string-search "</think>" response)))
                                     (with-demoted-errors "gptel callback error: %S"
                                       (funcall callback
                                                (cons 'reasoning
                                                      (substring response nil (+ idx 8)))
                                                info))
                                     (setq response (string-trim-left
                                                     (substring response (+ idx 8)))))
                                 (when-let* ((reasoning (plist-get info :reasoning))
                                             ((stringp reasoning)))
                                   (funcall callback (cons 'reasoning reasoning) info))))
                             (when (or response (not (member http-status '("200" "100"))))
                               (with-demoted-errors "gptel callback error: %S"
                                 (funcall callback response info)))
                             (gptel--fsm-transition fsm) ;TYPE -> next
                             (setf (alist-get buf gptel--request-alist nil 'remove) nil)
                             (kill-buffer buf)))
                         nil t nil)))
      ;; TODO: Add transformer here.
      (setf (alist-get proc-buf gptel--request-alist)
            (cons fsm
                  #'(lambda ()
                      (plist-put info :callback #'ignore)
                      (let (kill-buffer-query-functions)
                        ;;Can't stop url-retrieve process
                        (kill-buffer proc-buf))))))))

(cl-defgeneric gptel--parse-response (backend response proc-info)
  "Response extractor for LLM requests.

BACKEND is the LLM backend in use.

RESPONSE is the parsed JSON of the response, as a plist.

PROC-INFO is a plist with process information and other context.
See `gptel-curl--get-response' for its contents.")

(defun gptel--url-parse-response (backend proc-info)
  "Parse response from BACKEND with PROC-INFO."
  (when gptel-log-level                 ;logging
    (save-excursion
      (goto-char url-http-end-of-headers)
      (when (eq gptel-log-level 'debug)
        (gptel--log (gptel--json-encode (buffer-substring-no-properties (point-min) (point)))
                    "response headers"))
      (gptel--log (buffer-substring-no-properties (point) (point-max))
                  "response body")))
  (if-let* ((http-msg (string-trim (buffer-substring (line-beginning-position)
                                                     (line-end-position))))
            (http-status
             (save-match-data
               (and (string-match "HTTP/[.0-9]+ +\\([0-9]+\\)" http-msg)
                    (match-string 1 http-msg))))
            (response (progn (goto-char url-http-end-of-headers)
                             (condition-case nil
                                 (gptel--json-read)
                               (error 'json-read-error)))))
      (cond
       ;; FIXME Handle the case where HTTP 100 is followed by HTTP (not 200) BUG #194
       ((or (memq url-http-response-status '(200 100))
            (string-match-p "\\(?:1\\|2\\)00 OK" http-msg))
        (list (and-let* ((resp (gptel--parse-response backend response proc-info))
                         ((not (string-blank-p resp))))
                (string-trim resp))
              http-status http-msg))
       ((and-let* ((error-data
                    (cond ((plistp response) (plist-get response :error))
                          ((arrayp response)
                           (cl-some (lambda (el) (plist-get el :error)) response)))))
          (list nil http-status http-msg error-data)))
       ((eq response 'json-read-error)
        (list nil http-status (concat "(" http-msg ") Malformed JSON in response.") "json-read-error"))
       (t (list nil http-status (concat "(" http-msg ") Could not parse HTTP response.")
                "Could not parse HTTP response.")))
    (list nil (concat "(" http-msg ") Could not parse HTTP response.")
          "Could not parse HTTP response.")))


;;; Curl request response handling

(defun gptel-curl--get-args (info token)
  "Produce list of arguments for calling Curl.

INFO contains the request data, TOKEN is a unique identifier."
  (let* ((data (plist-get info :data))
         ;; We have to let-bind the following three since their dynamic
         ;; values are used for key lookup and url resolution
         (gptel-backend (plist-get info :backend))
         (gptel-model (plist-get info :model))
         (gptel-stream (plist-get info :stream))
         (url (let ((backend-url (gptel-backend-url gptel-backend)))
                (if (functionp backend-url)
                    (funcall backend-url) backend-url)))
         (data-json (decode-coding-string (gptel--json-encode data) 'utf-8 t))
         (headers
          (append '(("Content-Type" . "application/json"))
                  (when-let* ((header (gptel-backend-header gptel-backend)))
                    (if (functionp header)
                        (funcall header) header)))))
    (when gptel-log-level
      (when (eq gptel-log-level 'debug)
        (gptel--log (gptel--json-encode
                     (mapcar (lambda (pair) (cons (intern (car pair)) (cdr pair)))
                             headers))
                    "request headers"))
      (gptel--log data-json "request body"))
    (append
     gptel-curl--common-args
     gptel-curl-extra-args
     (and-let* ((curl-args (gptel-backend-curl-args gptel-backend)))
       (if (functionp curl-args) (funcall curl-args) curl-args))
     (list (format "-w(%s . %%{size_header})" token))
     (if (< (string-bytes data-json) gptel-curl-file-size-threshold)
         (list (format "-d%s" data-json))
       (let* ((write-region-inhibit-fsync t)
              (file-name-handler-alist nil)
              (inhibit-message t)
              (temp-filename (make-temp-file "gptel-curl-data" nil ".json" data-json))
              (cleanup-fn (lambda (&rest _) (when (file-exists-p temp-filename)
                                         (delete-file temp-filename)))))
         (plist-put info :post (cons cleanup-fn (plist-get info :post)))
         (list "--data-binary" (format "@%s" temp-filename))))
     (when (not (string-empty-p gptel-proxy))
       (list "--proxy" gptel-proxy
             "--proxy-negotiate"
             "--proxy-user" ":"))
     (cl-loop for (key . val) in headers
              collect (format "-H%s: %s" key val))
     (list url))))

;;;###autoload
(defun gptel-curl-get-response (fsm)
  "Fetch response to prompt in state FSM from the LLM using Curl.

FSM is the state machine driving this request.

FSM is the state machine driving this request.  Its INFO slot
contains the data required for setting up the request.  INFO is a
plist with the following keys, among others:
- :data     (the data being sent)
- :buffer   (the gptel buffer)
- :position (marker at which to insert the response).
- :callback (optional, the request callback)

Call CALLBACK with the response and INFO afterwards.  If omitted
the response is inserted into the current buffer after point."
  (let* ((token (md5 (format "%s%s%s%s"
                             (random) (emacs-pid) (user-full-name)
                             (recent-keys))))
         (info (gptel-fsm-info fsm))
         (backend (plist-get info :backend))
         (args (gptel-curl--get-args info token))
         (stream (plist-get info :stream))
         (process (apply #'start-process "gptel-curl"
                         (gptel--temp-buffer " *gptel-curl*") (gptel--curl-path) args)))
    (when (eq gptel-log-level 'debug)
      (gptel--log (mapconcat #'shell-quote-argument (cons (gptel--curl-path) args) " \\\n")
                  "request Curl command" 'no-json))
    (with-current-buffer (process-buffer process)
      (cond
       ((eq (gptel-backend-coding-system backend) 'binary)
        ;; set-buffer-file-coding-system is not needed since we don't save this buffer
        (set-buffer-multibyte nil)
        (set-process-coding-system process 'binary 'binary))
       (t
	;; Don't try to convert cr-lf to cr on Windows so that curl's "header size
	;; in bytes" stays correct. Explicitly set utf-8 for non-win systems too,
	;; for cases when buffer coding system is not set to utf-8.
	(set-process-coding-system process 'utf-8-unix 'utf-8-unix)))
      (set-process-query-on-exit-flag process nil)
      (if (plist-get info :token)       ;not the first run, set only the token
          (plist-put info :token token)
        (setf (gptel-fsm-info fsm)      ;fist run, set all process parameters
              (nconc (list :token token
                           :transformer
                           (when (with-current-buffer (plist-get info :buffer)
                                   (and (derived-mode-p 'org-mode)
                                        gptel-org-convert-response))
                             (gptel--stream-convert-markdown->org
                              (plist-get info :position))))
                     (unless (plist-get info :callback)
                       (list :callback (if stream
                                           #'gptel-curl--stream-insert-response
                                         #'gptel--insert-response)))
                     info)))
      (if stream
          (progn (set-process-sentinel process #'gptel-curl--stream-cleanup)
                 (set-process-filter process #'gptel-curl--stream-filter))
        (set-process-sentinel process #'gptel-curl--sentinel))
      (setf (alist-get process gptel--request-alist)
            (cons fsm
                  #'(lambda ()
                      ;; Clean up Curl process
                      (set-process-sentinel process #'ignore)
                      (delete-process process)
                      (kill-buffer (process-buffer process))))))))

;; ;; Ahead-Of-Time dispatch code for the parsers
;; :parser ; FIXME `cl--generic-*' are internal functions
;; (cl--generic-method-function
;;  (if stream
;;      (cl-loop
;;       for type in
;;       (cl--class-allparents (get (type-of backend) 'cl--class))
;;       with methods = (cl--generic-method-table
;;                       (cl--generic 'gptel-curl--parse-stream))
;;       when (cl--generic-member-method `(,type t) nil methods)
;;       return (car it))
;;    (cl-loop
;;     for type in
;;     (cl--class-allparents (get (type-of backend) 'cl--class))
;;     with methods = (cl--generic-method-table
;;                     (cl--generic 'gptel--parse-response))
;;     when (cl--generic-member-method `(,type t t) nil methods)
;;     return (car it))))

(defun gptel-curl--log-response (proc-buf proc-info)
  "Parse response buffer PROC-BUF and log response.

PROC-INFO is the plist containing process metadata."
  (with-current-buffer proc-buf
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "?\n?\n" nil t)
        (when (eq gptel-log-level 'debug)
          (gptel--log (gptel--json-encode
                       (buffer-substring-no-properties
                        (point-min) (1- (point))))
                      "response headers"))
        (let ((p (point)))
          (when (search-forward (plist-get proc-info :token) nil t)
            (goto-char (1- (match-beginning 0)))
            (gptel--log (buffer-substring-no-properties p (point))
                        "response body")))))))

;; TODO: Separate user-messaging from this function
(defun gptel-curl--stream-cleanup (process _status)
  "Process sentinel for gptel curl requests.

PROCESS and _STATUS are process parameters."
  (let ((proc-buf (process-buffer process)))
    (let* ((fsm (car (alist-get process gptel--request-alist)))
           (info (gptel-fsm-info fsm))
           (http-status (plist-get info :http-status)))
      (when gptel-log-level (gptel-curl--log-response proc-buf info)) ;logging
      (if (member http-status '("200" "100")) ;Finish handling response
          ;; Run the callback one last time to signal that the process has ended
          (with-demoted-errors "gptel callback error: %S"
            (funcall (plist-get info :callback) t info))
        (with-current-buffer proc-buf   ; Or Capture error message
          (goto-char (point-max))
          (search-backward (plist-get info :token))
          (backward-char)
          (pcase-let* ((`(,_ . ,header-size) (read (current-buffer)))
                       (response (progn (goto-char header-size)
                                        (condition-case nil (gptel--json-read)
                                          (error 'json-read-error))))
                       (error-data
                        (cond ((plistp response) (plist-get response :error))
                              ((arrayp response)
                               (cl-some (lambda (el) (plist-get el :error)) response)))))
            (cond
             (error-data
              (plist-put info :error error-data))
             ((eq response 'json-read-error)
              (plist-put info :error "Malformed JSON in response."))
             (t (plist-put info :error "Could not parse HTTP response.")))))
        (with-demoted-errors "gptel callback error: %S"
          (funcall (plist-get info :callback) nil info)))
      (gptel--fsm-transition fsm))      ; Move to next state
    (setf (alist-get process gptel--request-alist nil 'remove) nil)
    (kill-buffer proc-buf)))

(defun gptel-curl--stream-filter (process output)
  (let* ((fsm (car (alist-get process gptel--request-alist)))
         (proc-info (gptel-fsm-info fsm))
         (callback (or (plist-get proc-info :callback)
                       #'gptel-curl--stream-insert-response)))
    (with-current-buffer (process-buffer process)
      ;; Insert output
      (save-excursion
        (goto-char (process-mark process))
        (insert output)
        (set-marker (process-mark process) (point)))
      
      ;; Find HTTP status
      (unless (plist-get proc-info :http-status)
        (save-excursion
          (goto-char (point-min))
          (when-let* (((not (= (line-end-position) (point-max))))
                      (http-msg (buffer-substring (line-beginning-position)
                                                  (line-end-position)))
                      (http-status
                       (save-match-data
                         (and (string-match "HTTP/[.0-9]+ +\\([0-9]+\\)" http-msg)
                              (match-string 1 http-msg)))))
            (plist-put proc-info :http-status http-status)
            (plist-put proc-info :status (string-trim http-msg))
            (gptel--fsm-transition fsm))))
      
      (when-let* ((http-msg (plist-get proc-info :status))
                  (http-status (plist-get proc-info :http-status)))
        ;; Find data chunk(s) and run callback
        ;; FIXME Handle the case where HTTP 100 is followed by HTTP (not 200) BUG #194
        (when (member http-status '("200" "100"))
          (let ((response (gptel-curl--parse-stream
                           (plist-get proc-info :backend) proc-info))
                (reasoning-block (plist-get proc-info :reasoning-block)))
            ;; Depending on the API, there are two modes that reasoning or
            ;; chain-of-thought content appears: as part of the main response
            ;; but surrounded by <think>...</think> tags, or as a separate
            ;; JSON field in the response stream.
            ;;
            ;; These cases are handled using two PROC-INFO keys:
            ;;
            ;; :reasoning-block is nil before checking for reasoning, 'in when
            ;; in a reasoning block, t when we reach the end of the block, and
            ;; 'done afterwards or if no reasoning block is found.  This
            ;; applies to both the modes above.
            ;;
            ;; :reasoning contains the reasoning text parsed from the separate
            ;; JSON field.
            ;;
            ;; NOTE: We assume here that the reasoning block always
            ;; precedes the main response block.
            (unless (eq reasoning-block 'done)
              (let ((reasoning (plist-get proc-info :reasoning)))
                (cond
                 ((stringp reasoning)
                  ;; Obtained from separate JSON field in response
                  (funcall callback (cons 'reasoning reasoning) proc-info)
                  (unless reasoning-block ;Record that we're in a reasoning block (#709)
                    (plist-put proc-info :reasoning-block 'in))
                  (plist-put proc-info :reasoning nil)) ;Reset for next parsing round
                 ((and (string-blank-p response) ;Defer checking if response is blank
                       (not reasoning-block))) ;unless we're in a reasoning block already
                 ((and (null reasoning-block) (length> response 0))
                  ;; Obtained from main response stream: reasoning block start
                  (if-let*  ((idx (string-match-p "<think>" response)))
                      (progn
                        (when (> idx 0) ;Collect leading whitespace before <think>
                          (funcall callback (substring response 0 idx) proc-info)
                          (setq response (substring response idx)))
                        (setq response (cons 'reasoning response))
                        (plist-put proc-info :reasoning-block 'in))
                    (plist-put proc-info :reasoning-block 'done)))
                 ((and (not (eq reasoning-block t)) (length> response 0))
                  (if-let* ((idx (string-match-p "</think>" response)))
                      (progn
                        (funcall callback
                                 (cons 'reasoning (substring response nil (+ idx 8)))
                                 proc-info)
                        (setq reasoning-block t) ;Signal end of reasoning stream
                        (plist-put proc-info :reasoning-block t)
                        (setq response (substring response (+ idx 8))))
                    (setq response (cons 'reasoning response)))))
                (when (eq reasoning-block t) ;End of reasoning block
                  (funcall callback '(reasoning . t) proc-info)
                  (plist-put proc-info :reasoning-block 'done))))
            (unless (equal response "") ;Response callback
              (funcall callback response proc-info))))))))

(cl-defgeneric gptel-curl--parse-stream (backend proc-info)
  "Stream parser for gptel-curl.

Implementations of this function run as part of the process
filter for the active query, and return partial responses from
the LLM.

BACKEND is the LLM backend in use.

PROC-INFO is a plist with process information and other context.
See `gptel-curl--get-response' for its contents.")

(defun gptel-curl--sentinel (process _status)
  "Process sentinel for gptel curl requests.

PROCESS and _STATUS are process parameters."
  (let ((proc-buf (process-buffer process)))
    (when-let* (((eq (process-status process) 'exit))
                (fsm (car (alist-get process gptel--request-alist)))
                (proc-info (gptel-fsm-info fsm))
                (proc-callback (plist-get proc-info :callback)))
      (when gptel-log-level (gptel-curl--log-response proc-buf proc-info)) ;logging
      (pcase-let ((`(,response ,http-status ,http-msg ,error)
                   (with-current-buffer proc-buf
                     (gptel-curl--parse-response proc-info))))
        (plist-put proc-info :http-status http-status)
        (plist-put proc-info :status http-msg)
        (gptel--fsm-transition fsm)     ;WAIT -> TYPE
        (when error (plist-put proc-info :error error))
        ;; Look for a reasoning block
        (if (and (stringp response) (string-match-p "^\\s-*<think>" response))
            (when-let* ((idx (string-search "</think>" response)))
              (with-demoted-errors "gptel callback error: %S"
                (funcall proc-callback
                         (cons 'reasoning (substring response nil (+ idx 8)))
                         proc-info))
              (setq response
                    (string-trim-left (substring response (+ idx 8)))))
          (when-let* ((reasoning (plist-get proc-info :reasoning))
                      ((stringp reasoning)))
            (funcall proc-callback (cons 'reasoning reasoning) proc-info)))
        ;; Call callback with response text
        (when (or response (not (member http-status '("200" "100"))))
          (with-demoted-errors "gptel callback error: %S"
            (funcall proc-callback response proc-info))))
      (gptel--fsm-transition fsm))      ;TYPE -> next
    (setf (alist-get process gptel--request-alist nil 'remove) nil)
    (kill-buffer proc-buf)))

(defun gptel-curl--parse-response (proc-info)
  "Parse the buffer BUF with curl's response.

PROC-INFO is a plist with contextual information."
  (let ((token (plist-get proc-info :token)))
    (goto-char (point-max))
    (search-backward token)
    (backward-char)
    (pcase-let* ((`(,_ . ,header-size) (read (current-buffer))))
      (goto-char (point-min))

      (if-let* ((http-msg (string-trim
                           (buffer-substring (line-beginning-position)
                                             (line-end-position))))
                (http-status
                 (save-match-data
                   (and (string-match "HTTP/[.0-9]+ +\\([0-9]+\\)" http-msg)
                        (match-string 1 http-msg))))
                (response (progn (goto-char header-size)
                                 (condition-case nil
                                     (gptel--json-read)
                                   (error 'json-read-error)))))
          (cond
           ;; FIXME Handle the case where HTTP 100 is followed by HTTP (not 200) BUG #194
           ((member http-status '("200" "100"))
            (list (and-let* ((resp (gptel--parse-response
                                    (plist-get proc-info :backend) response proc-info))
                             ((not (string-blank-p resp))))
                    (string-trim resp))
                  http-status http-msg))
           ((and-let* ((error-data
                        (cond ((plistp response) (plist-get response :error))
                              ((arrayp response)
                               (cl-some (lambda (el) (plist-get el :error)) response)))))
              (list nil http-status http-msg error-data)))
           ((eq response 'json-read-error)
            (list nil http-status (concat "(" http-msg ") Malformed JSON in response.")
                  "Malformed JSON in response"))
           (t (list nil http-status (concat "(" http-msg ") Could not parse HTTP response.")
                    "Could not parse HTTP response.")))
        (list nil http-status (concat "(" http-msg ") Could not parse HTTP response.")
              "Could not parse HTTP response.")))))

(provide 'gptel-request)
;;; gptel-request.el ends here
