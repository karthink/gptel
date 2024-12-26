;;; gptel.el --- Interact with ChatGPT or other LLMs     -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthik.chikmagalur@gmail.com>
;; Version: 0.9.7
;; Package-Requires: ((emacs "27.1") (transient "0.4.0") (compat "29.1.4.1"))
;; Keywords: convenience
;; URL: https://github.com/karthink/gptel

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

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; gptel is a simple Large Language Model chat client, with support for multiple
;; models and backends.
;;
;; It works in the spirit of Emacs, available at any time and in any buffer.
;;
;; gptel supports
;;
;; - The services ChatGPT, Azure, Gemini, Anthropic AI, Anyscale, Together.ai,
;;   Perplexity, Anyscale, OpenRouter, Groq, PrivateGPT, DeepSeek, Cerebras,
;;   Github Models, xAI and Kagi (FastGPT & Summarizer)
;; - Local models via Ollama, Llama.cpp, Llamafiles or GPT4All
;;
;;  Additionally, any LLM service (local or remote) that provides an
;;  OpenAI-compatible API is supported.
;;
;; Features:
;; - It’s async and fast, streams responses.
;; - Interact with LLMs from anywhere in Emacs (any buffer, shell, minibuffer,
;;   wherever)
;; - LLM responses are in Markdown or Org markup.
;; - Supports conversations and multiple independent sessions.
;; - Supports multi-modal models (send images, documents).
;; - Save chats as regular Markdown/Org/Text files and resume them later.
;; - You can go back and edit your previous prompts or LLM responses when
;;   continuing a conversation.  These will be fed back to the model.
;; - Redirect prompts and responses easily
;; - Rewrite, refactor or fill in regions in buffers
;; - Write your own commands for custom tasks with a simple API.
;;
;; Requirements for ChatGPT, Azure, Gemini or Kagi:
;;
;; - You need an appropriate API key.  Set the variable `gptel-api-key' to the
;;   key or to a function of no arguments that returns the key.  (It tries to
;;   use `auth-source' by default)
;;
;;   ChatGPT is configured out of the box.  For the other sources:
;;
;; - For Azure: define a gptel-backend with `gptel-make-azure', which see.
;; - For Gemini: define a gptel-backend with `gptel-make-gemini', which see.
;; - For Anthropic (Claude): define a gptel-backend with `gptel-make-anthropic',
;;   which see
;; - For Together.ai, Anyscale, Perplexity, Groq, OpenRouter, DeepSeek, Cerebras or
;;   Github Models: define a gptel-backend with `gptel-make-openai', which see.
;; - For PrivateGPT: define a backend with `gptel-make-privategpt', which see.
;; - For Kagi: define a gptel-backend with `gptel-make-kagi', which see.
;;
;; For local models using Ollama, Llama.cpp or GPT4All:
;;
;; - The model has to be running on an accessible address (or localhost)
;; - Define a gptel-backend with `gptel-make-ollama' or `gptel-make-gpt4all',
;;   which see.
;; - Llama.cpp or Llamafiles: Define a gptel-backend with `gptel-make-openai',
;;
;; Consult the package README for examples and more help with configuring
;; backends.
;;
;; Usage:
;;
;; gptel can be used in any buffer or in a dedicated chat buffer.  The
;; interaction model is simple: Type in a query and the response will be
;; inserted below.  You can continue the conversation by typing below the
;; response.
;;
;; To use this in any buffer:
;;
;; - Call `gptel-send' to send the buffer's text up to the cursor.  Select a
;;   region to send only the region.
;;
;; - You can select previous prompts and responses to continue the conversation.
;;
;; - Call `gptel-send' with a prefix argument to access a menu where you can set
;;   your backend, model and other parameters, or to redirect the
;;   prompt/response.
;;
;; To use this in a dedicated buffer:
;; 
;; - M-x gptel: Start a chat session
;;
;; - In the chat session: Press `C-c RET' (`gptel-send') to send your prompt.
;;   Use a prefix argument (`C-u C-c RET') to access a menu.  In this menu you
;;   can set chat parameters like the system directives, active backend or
;;   model, or choose to redirect the input or output elsewhere (such as to the
;;   kill ring or the echo area).
;;
;; - You can save this buffer to a file.  When opening this file, turn on
;;   `gptel-mode' before editing it to restore the conversation state and
;;   continue chatting.
;;
;; - To include media files with your request, you can add them to the context
;;   (described next), or include them as links in Org or Markdown mode chat
;;   buffers.  Sending media is disabled by default, you can turn it on globally
;;   via `gptel-track-media', or locally in a chat buffer via the header line.
;; 
;; Include more context with requests:
;;
;; If you want to provide the LLM with more context, you can add arbitrary
;; regions, buffers or files to the query with `gptel-add'.  To add text or
;; media files, call `gptel-add' in Dired or use the dedicated `gptel-add-file'.
;;
;; You can also add context from gptel's menu instead (gptel-send with a prefix
;; arg), as well as examine or modify context.
;;
;; When context is available, gptel will include it with each LLM query.
;;
;; Rewrite interface
;;
;; In any buffer: with a region selected, you can rewrite prose, refactor code
;; or fill in the region.  This is accessible via `gptel-rewrite', and also from
;; the `gptel-send' menu.
;;
;; gptel in Org mode:
;;
;; gptel offers a few extra conveniences in Org mode.
;; - You can limit the conversation context to an Org heading with
;;   `gptel-org-set-topic'.
;;   
;; - You can have branching conversations in Org mode, where each hierarchical
;;   outline path through the document is a separate conversation branch.
;;   See the variable `gptel-org-branching-context'.
;;   
;; - You can declare the gptel model, backend, temperature, system message and
;;   other parameters as Org properties with the command
;;   `gptel-org-set-properties'.  gptel queries under the corresponding heading
;;   will always use these settings, allowing you to create mostly reproducible
;;   LLM chat notebooks.
;;
;; Finally, gptel offers a general purpose API for writing LLM ineractions
;; that suit your workflow, see `gptel-request'.

;;; Code:
(declare-function markdown-mode "markdown-mode")
(declare-function gptel-curl-get-response "gptel-curl")
(declare-function gptel-menu "gptel-transient")
(declare-function gptel-system-prompt "gptel-transient")
(declare-function pulse-momentary-highlight-region "pulse")

(declare-function ediff-make-cloned-buffer "ediff-util")
(declare-function ediff-regions-internal "ediff")

(declare-function gptel-org--create-prompt "gptel-org")
(declare-function gptel-org-set-topic "gptel-org")
(declare-function gptel-org--save-state "gptel-org")
(declare-function gptel-org--restore-state "gptel-org")
(declare-function gptel--stream-convert-markdown->org "gptel-org")
(declare-function gptel--convert-markdown->org "gptel-org")
(define-obsolete-function-alias
  'gptel-set-topic 'gptel-org-set-topic "0.7.5")

(eval-when-compile
  (require 'subr-x))
(require 'cl-lib)
(require 'compat nil t)
(require 'url)
(require 'map)
(require 'text-property-search)
(require 'cl-generic)
(require 'gptel-openai)

(with-eval-after-load 'org
  (require 'gptel-org))


;;; User options

(defgroup gptel nil
  "Interact with LLMs from anywhere in Emacs."
  :group 'hypermedia)

;; (defcustom gptel-host "api.openai.com"
;;   "The API host queried by gptel."
;;   :group 'gptel
;;   :type 'string)
(make-obsolete-variable
 'gptel-host
 "Use `gptel-make-openai' instead."
 "0.5.0")

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
(make-obsolete-variable 'gptel-playback 'gptel-stream "0.3.0")

(defcustom gptel-use-curl (and (executable-find "curl") t)
  "Whether gptel should prefer Curl when available."
  :type 'boolean)

(defcustom gptel-curl-file-size-threshold
  (if (memq system-type '(windows-nt ms-dos)) 32766 130000)
  "Size threshold for using file input with Curl.

Specifies the size threshold for when to use a temporary file to pass data to
Curl in GPTel queries.  If the size of the data to be sent exceeds this
threshold, the data is written to a temporary file and passed to Curl using the
`--data-binary' option with a file reference.  Otherwise, the data is passed
directly as a command-line argument.

The value is an integer representing the number of bytes.

Adjusting this value may be necessary depending on the environment
and the typical size of the data being sent in GPTel queries.
A larger value may improve performance by avoiding the overhead of creating
temporary files for small data payloads, while a smaller value may be needed
if the command-line argument size is limited by the operating system.

The default for windows comes from Microsoft documentation located here:
https://learn.microsoft.com/en-us/windows/win32/api/processthreadsapi/nf-processthreadsapi-createprocessa"
  :type 'natnum)

(defcustom gptel-response-filter-functions
  (list #'gptel--convert-org)
  "Abnormal hook for transforming the response from an LLM.

This is used to format the response in some way, such as filling
paragraphs, adding annotations or recording information in the
response like links.

Each function in this hook receives two arguments, the response
string to transform and the LLM interaction buffer.  It
should return the transformed string.

NOTE: This is only used for non-streaming responses.  To
transform streaming responses, use `gptel-post-stream-hook' and
`gptel-post-response-functions'."
  :type 'hook)

(defcustom gptel-pre-response-hook nil
  "Hook run before inserting the LLM response into the current buffer.

This hook is called in the buffer where the LLM response will be
inserted.

Note: this hook only runs if the request succeeds."
  :type 'hook)

(define-obsolete-variable-alias
  'gptel-post-response-hook 'gptel-post-response-functions
  "0.6.0"
  "Post-response functions are now called with two arguments: the
start and end buffer positions of the response.")

(defcustom gptel-post-response-functions nil
  "Abnormal hook run after inserting the LLM response into the current buffer.

This hook is called in the buffer to which the LLM response is
sent, and after the full response has been inserted.  Each
function is called with two arguments: the response beginning and
end positions.

Note: this hook runs even if the request fails.  In this case the
response beginning and end positions are both the cursor position
at the time of the request."
  :type 'hook)

;; (defcustom gptel-pre-stream-insert-hook nil
;;   "Hook run before each insertion of the LLM's streaming response.

;; This hook is called in the buffer from which the prompt was sent
;; to the LLM, immediately before text insertion."
;;   :group 'gptel
;;   :type 'hook)

(defcustom gptel-post-stream-hook nil
  "Hook run after each insertion of the LLM's streaming response.

This hook is called in the buffer from which the prompt was sent
to the LLM, and after a text insertion."
  :type 'hook)

(defcustom gptel-save-state-hook nil
  "Hook run before gptel saves model parameters to a file.

You can use this hook to store additional conversation state or
model parameters to the chat buffer, or to modify the buffer in
some other way."
  :type 'hook)

(defcustom gptel-default-mode (if (fboundp 'markdown-mode)
				  'markdown-mode
				'text-mode)
  "The default major mode for dedicated chat buffers.

If `markdown-mode' is available, it is used.  Otherwise gptel
defaults to `text-mode'."
  :type 'function)

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

(defcustom gptel-use-header-line t
  "Whether `gptel-mode' should use header-line for status information.

When set to nil, use the mode line for (minimal) status
information and the echo area for messages."
  :type 'boolean)

(defcustom gptel-display-buffer-action '(pop-to-buffer)
  "The action used to display gptel chat buffers.

The gptel buffer is displayed in a window using

  (display-buffer BUFFER gptel-display-buffer-action)

The value of this option has the form (FUNCTION . ALIST),
where FUNCTION is a function or a list of functions.  Each such
function should accept two arguments: a buffer to display and an
alist of the same form as ALIST.  See info node `(elisp)Choosing
Window' for details."
  :type display-buffer--action-custom-type)

(defcustom gptel-crowdsourced-prompts-file
  (let ((cache-dir (or (eval-when-compile
			 (require 'xdg)
			 (xdg-cache-home))
                       user-emacs-directory)))
    (expand-file-name "gptel-crowdsourced-prompts.csv" cache-dir))
  "File used to store crowdsourced system prompts.

These are prompts cached from an online source (see
`gptel--crowdsourced-prompts-url'), and can be set from the
transient menu interface provided by `gptel-menu'."
  :type 'file)

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

(defcustom gptel-model 'gpt-4o-mini
  "GPT Model for chat.

The name of the model, as a symbol.  This is the name as expected
by the LLM provider's API.

The current options for ChatGPT are
- `gpt-3.5-turbo'
- `gpt-3.5-turbo-16k'
- `gpt-4o-mini'
- `gpt-4'
- `gpt-4o'
- `gpt-4-turbo'
- `gpt-4-turbo-preview'
- `gpt-4-32k'
- `gpt-4-1106-preview'

To set the model for a chat session interactively call
`gptel-send' with a prefix argument."
  :safe #'always
  :type '(choice
          (symbol :tag "Specify model name")
          (const :tag "GPT 4 omni mini" gpt-4o-mini)
          (const :tag "GPT 3.5 turbo" gpt-3.5-turbo)
          (const :tag "GPT 3.5 turbo 16k" gpt-3.5-turbo-16k)
          (const :tag "GPT 4" gpt-4)
          (const :tag "GPT 4 omni" gpt-4o)
          (const :tag "GPT 4 turbo" gpt-4-turbo)
          (const :tag "GPT 4 turbo (preview)" gpt-4-turbo-preview)
          (const :tag "GPT 4 32k" gpt-4-32k)
          (const :tag "GPT 4 1106 (preview)" gpt-4-1106-preview)))

(defcustom gptel-temperature 1.0
  "\"Temperature\" of the LLM response.

This is a number between 0.0 and 2.0 that controls the randomness
of the response, with 2.0 being the most random.

To set the temperature for a chat session interactively call
`gptel-send' with a prefix argument."
  :safe #'always
  :type 'number)

(defvar gptel--known-backends)

(defconst gptel--openai-models
  '((gpt-4o
     :description "Advanced model for complex tasks; cheaper & faster than GPT-Turbo"
     :capabilities (media tool json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 128
     :input-cost 2.50
     :output-cost 10
     :cutoff-date "2023-10")
    (gpt-4o-mini
     :description "Cheap model for fast tasks; cheaper & more capable than GPT-3.5 Turbo"
     :capabilities (media tool json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 128
     :input-cost 0.15
     :output-cost 0.60
     :cutoff-date "2023-10")
    (gpt-4-turbo
     :description "Previous high-intelligence model"
     :capabilities (media tool url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 128
     :input-cost 10
     :output-cost 30
     :cutoff-date "2023-12")
    ;; points to gpt-4-0613
    (gpt-4
     :description "GPT-4 snapshot from June 2023 with improved function calling support"
     :context-window 8.192
     :input-cost 30
     :output-cost 60
     :cutoff-date "2023-09")
    (gpt-4-turbo-preview
     :description "Points to gpt-4-0125-preview"
     :context-window 128
     :input-cost 10
     :output-cost 30
     :cutoff-date "2023-12")
    (gpt-4-0125-preview
     :description "GPT-4 Turbo preview model intended to reduce cases of “laziness”"
     :context-window 128
     :input-cost 10
     :output-cost 30
     :cutoff-date "2023-12")
    (o1-preview
     :description "Reasoning model designed to solve hard problems across domains"
     :context-window 128
     :input-cost 15
     :output-cost 60
     :cutoff-date "2023-10"
     :capabilities (nosystem)
     :request-params (:stream :json-false))
    (o1-mini
     :description "Faster and cheaper reasoning model good at coding, math, and science"
     :context-window 128
     :input-cost 3
     :output-cost 12
     :cutoff-date "2023-10"
     :capabilities (nosystem)
     :request-params (:stream :json-false))
    ;; limited information available
    (gpt-4-32k
     :input-cost 60
     :output-cost 120)
    (gpt-4-1106-preview
     :description "Preview model with improved function calling support"
     :context-window 128
     :input-cost 10
     :output-cost 30
     :cutoff-date "2023-04")
    (gpt-3.5-turbo
     :description "More expensive & less capable than GPT-4o-mini; use that instead"
     :capabilities (tool)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 16.358
     :input-cost 0.50
     :output-cost 1.50
     :cutoff-date "2021-09")
    (gpt-3.5-turbo-16k
     :description "More expensive & less capable than GPT-4o-mini; use that instead"
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
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

- <https://openai.com/pricing>
- <https://platform.openai.com/docs/models>")

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

(defvar-local gptel--bounds nil)
(put 'gptel--bounds 'safe-local-variable #'always)

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
(make-obsolete-variable
 'gptel--debug 'gptel-log-level "0.6.5")

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

When the active `gptel-model' supports it, gptel can send images
or other media from links in chat buffers to the LLM.  To use
this, the following steps are required.

1. `gptel-track-media' (this variable) should be non-nil

2. The LLM should provide vision or document support.  Currently,
only the OpenAI, Anthropic and Ollama APIs are supported.  See
the documentation of `gptel-make-openai', `gptel-make-anthropic'
and `gptel-make-ollama' resp. for details on how to specify media
support for models.

3. Only \"standalone\" links in chat buffers are considered.
These are links on their own line with no surrounding text.
Further:

- In Org mode, only files or URLs of the form
  [[/path/to/media][bracket links]] and <angle/link/path>
  are sent.

- In Markdown mode, only files or URLS of the form
  [bracket link](/path/to/media) and <angle/link/path>
  are sent.

This option has no effect in non-chat buffers.  To include
media (including images) more generally, use `gptel-add'."
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

(defvar-local gptel--old-header-line nil)

(defvar gptel-context--alist nil
  "List of gptel's context sources.

Each entry is of the form
 (buffer . (overlay1 overlay2 ...))
or
 (\"path/to/file\").")


;;; Utility functions

(defun gptel-api-key-from-auth-source (&optional host user)
  "Lookup api key in the auth source.
By default, the LLM host for the active backend is used as HOST,
and \"apikey\" as USER."
  (if-let ((secret
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
  (when-let ((key-sym (or key (gptel-backend-key gptel-backend))))
    (cl-typecase key-sym
      (function (string-trim-right (funcall key-sym) "[\n\r]+"))
      (string (string-trim-right key-sym "[\n\r]+"))
      (symbol (if-let ((val (symbol-value key-sym)))
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
  (cl-etypecase s
    (symbol (symbol-name s))
    (string s)
    (number (number-to-string s))))

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
(defun gptel-auto-scroll ()
  "Scroll window if LLM response continues below viewport.

Note: This will move the cursor."
  (when-let ((win (get-buffer-window (current-buffer) 'visible))
             ((not (pos-visible-in-window-p (point) win)))
             (scroll-error-top-bottom t))
    (condition-case nil
        (with-selected-window win
          (scroll-up-command))
      (error nil))))

(defun gptel-beginning-of-response (&optional _ _ arg)
  "Move point to the beginning of the LLM response ARG times."
  (interactive "p")
  ;; FIXME: Only works for arg == 1
  (gptel-end-of-response nil nil (- (or arg 1))))

(defun gptel-end-of-response (&optional _ _ arg)
  "Move point to the end of the LLM response ARG times."
  (interactive (list nil nil
                     (prefix-numeric-value current-prefix-arg)))
  (unless arg (setq arg 1))
  (let ((search (if (> arg 0)
                    #'text-property-search-forward
                  #'text-property-search-backward)))
    (dotimes (_ (abs arg))
      (funcall search 'gptel 'response t)
      (if (> arg 0)
          (when (looking-at (concat "\n\\{1,2\\}"
                                    (regexp-quote
                                     (gptel-prompt-prefix-string))
                                    "?"))
            (goto-char (match-end 0)))
        (when (looking-back (concat (regexp-quote
                                     (gptel-response-prefix-string))
                                    "?")
                            (point-min))
          (goto-char (match-beginning 0)))))))

(defmacro gptel--at-word-end (&rest body)
  "Execute BODY at end of the current word or punctuation."
  `(save-excursion
     (skip-syntax-forward "w.")
     ,(macroexp-progn body)))

(defun gptel-prompt-prefix-string ()
  "Prefix before user prompts in `gptel-mode'."
  (or (alist-get major-mode gptel-prompt-prefix-alist) ""))

(defun gptel-response-prefix-string ()
  "Prefix before LLM responses in `gptel-mode'."
  (or (alist-get major-mode gptel-response-prefix-alist) ""))

(defsubst gptel--trim-prefixes (s)
  "Remove prompt/response prefixes from string S."
  (string-trim s
   (format "[\t\r\n ]*\\(?:%s\\)?[\t\r\n ]*"
             (regexp-quote (gptel-prompt-prefix-string)))
   (format "[\t\r\n ]*\\(?:%s\\)?[\t\r\n ]*"
           (regexp-quote (gptel-response-prefix-string)))))

(defsubst gptel--link-standalone-p (beg end)
  "Return non-nil if positions BEG and END are isolated.

This means the extent from BEG to END is the only non-whitespace
content on this line."
  (save-excursion
    (and (= beg (progn (goto-char beg) (beginning-of-line)
                       (skip-chars-forward "\t ")
                       (point)))
         (= end (progn (goto-char end) (end-of-line)
                       (skip-chars-backward "\t ")
                       (point))))))

(defvar-local gptel--backend-name nil
  "Store to persist backend name across Emacs sessions.

Note: Changing this variable does not affect gptel\\='s behavior
in any way.")
(put 'gptel--backend-name 'safe-local-variable #'always)

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

;;;; Response text recognition

(defun gptel--get-buffer-bounds ()
  "Return the gptel response boundaries in the buffer as an alist."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (let ((prop) (bounds))
        (while (setq prop (text-property-search-backward
                           'gptel 'response t))
          (push (cons (prop-match-beginning prop)
                      (prop-match-end prop))
                bounds))
        bounds))))

(defun gptel--get-bounds ()
  "Return the gptel response boundaries around point."
  (let (prop)
    (save-excursion
      (when (text-property-search-backward
             'gptel 'response t)
        (when (setq prop (text-property-search-forward
                          'gptel 'response t))
          (cons (prop-match-beginning prop)
                      (prop-match-end prop)))))))

(defun gptel--in-response-p (&optional pt)
  "Check if position PT is inside a gptel response."
  (get-char-property (or pt (point)) 'gptel))

(defun gptel--at-response-history-p (&optional pt)
  "Check if gptel response at position PT has variants."
  (get-char-property (or pt (point)) 'gptel-history))

(defvar gptel--mode-description-alist
  '((js2-mode      . "Javascript")
    (sh-mode       . "Shell")
    (enh-ruby-mode . "Ruby")
    (yaml-mode     . "Yaml")
    (yaml-ts-mode  . "Yaml")
    (rustic-mode   . "Rust"))
  "Mapping from unconventionally named major modes to languages.

This is used when generating system prompts for rewriting and
when including context from these major modes.")

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

;;;; Directive handling


(defvar gptel--system-message (alist-get 'default gptel-directives)
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
     (concat
      (string-replace "\n" (or replacement " ")
                      (truncate-string-to-width
                       directive width nil nil t))))
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


;;; Saving and restoring state

(defun gptel--restore-state ()
  "Restore gptel state when turning on `gptel-mode'."
  (when (buffer-file-name)
    (if (derived-mode-p 'org-mode)
        (progn
          (require 'gptel-org)
          (gptel-org--restore-state))
      (when gptel--bounds
        (mapc (pcase-lambda (`(,beg . ,end))
                (put-text-property beg end 'gptel 'response))
              gptel--bounds)
        (message "gptel chat restored."))
      (when gptel--backend-name
        (if-let ((backend (alist-get
                           gptel--backend-name gptel--known-backends
                           nil nil #'equal)))
            (setq-local gptel-backend backend)
          (message
           (substitute-command-keys
            (concat
             "Could not activate gptel backend \"%s\"!  "
             "Switch backends with \\[universal-argument] \\[gptel-send]"
             " before using gptel."))
           gptel--backend-name))))))

(defun gptel--save-state ()
  "Write the gptel state to the buffer.

This saves chat metadata when writing the buffer to disk.  To
restore a chat session, turn on `gptel-mode' after opening the
file."
  (run-hooks 'gptel-save-state-hook)
  (if (derived-mode-p 'org-mode)
      (progn
        (require 'gptel-org)
        (gptel-org--save-state))
    (let ((print-escape-newlines t))
      (save-excursion
        (save-restriction
          (add-file-local-variable 'gptel-model gptel-model)
          (add-file-local-variable 'gptel--backend-name
                                   (gptel-backend-name gptel-backend))
          (unless (equal (default-value 'gptel-temperature) gptel-temperature)
            (add-file-local-variable 'gptel-temperature gptel-temperature))
          (unless (equal (default-value 'gptel--system-message)
                           gptel--system-message)
            (add-file-local-variable
             'gptel--system-message
             (car-safe (gptel--parse-directive gptel--system-message))))
          (when gptel-max-tokens
            (add-file-local-variable 'gptel-max-tokens gptel-max-tokens))
          (when (natnump gptel--num-messages-to-send)
            (add-file-local-variable 'gptel--num-messages-to-send
                                     gptel--num-messages-to-send))
          (add-file-local-variable 'gptel--bounds (gptel--get-buffer-bounds)))))))


;;; Minor mode and UI

;; NOTE: It's not clear that this is the best strategy:
(add-to-list 'text-property-default-nonsticky '(gptel . t))

;;;###autoload
(define-minor-mode gptel-mode
  "Minor mode for interacting with LLMs."
  :lighter " GPT"
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c RET") #'gptel-send)
    map)
  (if gptel-mode
      (progn
        (unless (or (derived-mode-p 'org-mode 'markdown-mode)
                    (eq major-mode 'text-mode))
          (gptel-mode -1)
          (user-error (format "`gptel-mode' is not supported in `%s'." major-mode)))
        (add-hook 'before-save-hook #'gptel--save-state nil t)
        (gptel--restore-state)
        (if gptel-use-header-line
          (setq gptel--old-header-line header-line-format
                header-line-format
                (list '(:eval (concat (propertize " " 'display '(space :align-to 0))
                               (format "%s" (gptel-backend-name gptel-backend))))
                      (propertize " Ready" 'face 'success)
                      '(:eval
                        (let* ((model (gptel--model-name gptel-model))
                              (system
                               (propertize
                                (buttonize
                                 (format "[Prompt: %s]"
                                  (or (car-safe (rassoc gptel--system-message gptel-directives))
                                   (gptel--describe-directive gptel--system-message 15)))
                                 (lambda (&rest _) (gptel-system-prompt)))
                                'mouse-face 'highlight
                                'help-echo "System message for session"))
                              (context
                               (and gptel-context--alist
                                (cl-loop for entry in gptel-context--alist
                                 if (bufferp (car entry)) count it into bufs
                                 else count (stringp (car entry)) into files
                                 finally return
                                 (propertize
                                  (buttonize
                                   (concat "[Context: "
                                    (and (> bufs 0) (format "%d buf" bufs))
                                    (and (> bufs 1) "s")
                                    (and (> bufs 0) (> files 0) ", ")
                                    (and (> files 0) (format "%d file" files))
                                    (and (> files 1) "s")
                                    "]")
                                   (lambda (&rest _)
                                     (require 'gptel-context)
                                     (gptel-context--buffer-setup)))
                                  'mouse-face 'highlight
                                  'help-echo "Active gptel context"))))
                              (toggle-track-media
                               (lambda (&rest _)
                                 (setq-local gptel-track-media
                                  (not gptel-track-media))
                                 (if gptel-track-media
                                     (message
                                      (concat
                                       "Sending media from included links.  To include media, create "
                                       "a \"standalone\" link in a paragraph by itself, separated from surrounding text."))
                                   (message "Ignoring image links.  Only link text will be sent."))
                                 (run-at-time 0 nil #'force-mode-line-update)))
                              (track-media
                               (and (gptel--model-capable-p 'media)
                                (if gptel-track-media
                                    (propertize
                                     (buttonize "[Sending media]" toggle-track-media)
                                     'mouse-face 'highlight
                                     'help-echo
                                     "Sending media from standalone links/urls when supported.\nClick to toggle")
                                  (propertize
                                     (buttonize "[Ignoring media]" toggle-track-media)
                                     'mouse-face 'highlight
                                     'help-echo
                                     "Ignoring images from standalone links/urls.\nClick to toggle")))))
                         (concat
                          (propertize
                           " " 'display
                           `(space :align-to (- right ,(+ 5 (length model) (length system)
                                                        (length track-media) (length context)))))
                          track-media (and context " ") context " " system " "
                          (propertize
                           (buttonize (concat "[" model "]")
                            (lambda (&rest _) (gptel-menu)))
                           'mouse-face 'highlight
                           'help-echo "GPT model in use"))))))
          (setq mode-line-process
                '(:eval (concat " "
                         (buttonize (gptel--model-name gptel-model)
                            (lambda (&rest _) (gptel-menu))))))))
    (remove-hook 'before-save-hook #'gptel--save-state t)
    (if gptel-use-header-line
        (setq header-line-format gptel--old-header-line
              gptel--old-header-line nil)
      (setq mode-line-process nil))))

(defun gptel--update-status (&optional msg face)
  "Update status MSG in FACE."
  (when gptel-mode
    (if gptel-use-header-line
        (and (consp header-line-format)
           (setf (nth 1 header-line-format)
                 (propertize msg 'face face)))
      (if (member msg '(" Typing..." " Waiting..."))
          (setq mode-line-process (propertize msg 'face face))
        (setq mode-line-process
              '(:eval (concat " "
                       (buttonize (gptel--model-name gptel-model)
                            (lambda (&rest _) (gptel-menu))))))
        (message (propertize msg 'face face))))
    (force-mode-line-update)))

(declare-function gptel-context--wrap "gptel-context")


;;; Send queries, handle responses
(cl-defun gptel-request
    (&optional prompt &key callback
               (buffer (current-buffer))
               position context dry-run
               (stream nil) (in-place nil)
               (system gptel--system-message))
  "Request a response from the `gptel-backend' for PROMPT.

The request is asynchronous, the function returns immediately.

If PROMPT is
- a string, it is used to create a full prompt suitable for
  sending to the LLM.
- A list of strings, it is interpreted as a conversation, i.e. a
  series of alternating user prompts and LLM responses.
- nil but region is active, the region contents are used.
- nil, the current buffer's contents up to (point) are used.
  Previous responses from the LLM are identified as responses.

Keyword arguments:

CALLBACK, if supplied, is a function of two arguments, called
with the RESPONSE (a string) and INFO (a plist):

 (funcall CALLBACK RESPONSE INFO)

RESPONSE is

- A string if the request was successful
- nil if there was no response or an error.
- The symbol `abort' if the request was aborted, see
  `gptel-abort'.

The INFO plist has (at least) the following keys:
:data         - The request data included with the query
:position     - marker at the point the request was sent, unless
                POSITION is specified.
:buffer       - The buffer current when the request was sent,
                unless BUFFER is specified.
:status       - Short string describing the result of the request, including
                possible HTTP errors.

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

SYSTEM is the system message or extended chat directive sent to
the LLM.  This can be a string, a list of strings or a function
that returns either; see `gptel-directives' for more
information. If SYSTEM is omitted, the value of
`gptel--system-message' for the current buffer is used.

The following keywords are mainly for internal use:

IN-PLACE is a boolean used by the default callback when inserting
the response to determine if delimiters are needed between the
prompt and the response.

STREAM is a boolean that determines if the response should be
streamed, as in `gptel-stream'.  The calling convention for
streaming callbacks is slightly different:

 (funcall CALLBACK RESPONSE INFO)

- CALLBACK will be called with each response text chunk (a
  string) as it is received.

- When the HTTP request ends successfully, CALLBACK will be
  called with a RESPONSE argument of t to indicate success.

- If the HTTP request throws an error, CALLBACK will be called
  with a RESPONSE argument of nil.  You can find the error via
  (plist-get INFO :status).

- If the request is aborted, CALLBACK will be called with a
  RESPONSE argument of `abort'.

If DRY-RUN is non-nil, construct and return the full
query data as usual, but do not send the request.

Note:

1. This function is not fully self-contained.  Consider
let-binding the parameters `gptel-backend', `gptel-model' and
`gptel-use-context' around calls to it as required.

2. The return value of this function is a list of data that may
be used to rerun or continue the request at a later time."
  (declare (indent 1))
  ;; TODO Remove this check in version 1.0
  (gptel--sanitize-model)
  (let* ((directive (gptel--parse-directive system))
         ;; DIRECTIVE contains both the system message and the template prompts
         (gptel--system-message
          ;; Add context chunks to system message if required
          (if (and gptel-context--alist
                   (eq gptel-use-context 'system)
                   (not (gptel--model-capable-p 'nosystem)))
              (gptel-context--wrap (car directive))
            (car directive)))
         (gptel-stream stream)
         (start-marker
          (cond
           ((null position)
            (if (use-region-p)
                (set-marker (make-marker) (region-end))
              (gptel--at-word-end (point-marker))))
           ((markerp position) position)
           ((integerp position)
            (set-marker (make-marker) position buffer))))
         (full-prompt
          (nconc
           (cdr directive)              ;prompt constructed from directive/template
           (cond                        ;prompt from buffer or explicitly supplied
            ((null prompt)
             (gptel--create-prompt start-marker))
            ((stringp prompt)
             ;; FIXME Dear reader, welcome to Jank City:
             (with-temp-buffer
               (let ((gptel-model (buffer-local-value 'gptel-model buffer))
                     (gptel-backend (buffer-local-value 'gptel-backend buffer)))
                 (insert prompt)
                 (gptel--create-prompt))))
            ((consp prompt) (gptel--parse-list gptel-backend prompt)))))
         (info (list :data (gptel--request-data gptel-backend full-prompt)
                     :buffer buffer
                     :position start-marker)))
    ;; This context should not be confused with the context aggregation context!
    (when context (plist-put info :context context))
    (when in-place (plist-put info :in-place in-place))
    (unless dry-run
      (funcall (if gptel-use-curl
                   #'gptel-curl-get-response #'gptel--url-get-response)
               info callback))
    (list stream info callback)))

(defvar gptel--request-alist nil "Alist of active gptel requests.")

(defun gptel-abort (buf)
  "Stop any active gptel process associated with buffer BUF.

BUF defaults to the current buffer."
  (interactive (list (current-buffer)))
  (when-let* ((proc-attrs
               (cl-find-if (lambda (proc-list)
                             (eq (plist-get (cdr proc-list) :buffer) buf))
                           gptel--request-alist))
              (proc (car proc-attrs))
              (info (cdr proc-attrs)))
    ;; Run callback with abort signal
    (with-demoted-errors "Callback error: %S"
      (and-let* ((cb (plist-get info :callback))
                 ((functionp cb)))
           (funcall cb 'abort info)))
    (if gptel-use-curl
        (progn                        ;Clean up Curl process
          (setf (alist-get proc gptel--request-alist nil 'remove) nil)
          (set-process-sentinel proc #'ignore)
          (delete-process proc)
          (kill-buffer (process-buffer proc)))
      (plist-put info :callback #'ignore)
      (let (kill-buffer-query-functions)
        (kill-buffer proc)))            ;Can't stop url-retrieve process
    (with-current-buffer buf
      (when gptel-mode (gptel--update-status  " Abort" 'error)))
    (message "Stopped gptel request in buffer %S" (buffer-name buf))))

;; TODO: Handle multiple requests(#15). (Only one request from one buffer at a time?)
;;;###autoload
(defun gptel-send (&optional arg)
  "Submit this prompt to the current LLM backend.

By default, the contents of the buffer up to the cursor position
are sent.  If the region is active, its contents are sent
instead.

The response from the LLM is inserted below the cursor position
at the time of sending.  To change this behavior or model
parameters, use prefix arg ARG activate a transient menu with
more options instead.

This command is asynchronous, you can continue to use Emacs while
waiting for the response."
  (interactive "P")
  (if (and arg (require 'gptel-transient nil t))
      (call-interactively #'gptel-menu)
  (message "Querying %s..." (gptel-backend-name gptel-backend))
  (gptel--sanitize-model)
  (gptel-request nil :stream gptel-stream)
  (gptel--update-status " Waiting..." 'warning)))

(declare-function json-pretty-print-buffer "json")
(defun gptel--inspect-query (request-args &optional format)
  "Show REQUEST-ARGS, the full LLM query to be sent, in a buffer.

This functions as a dry run of `gptel-send'.  If FORMAT is
the symbol json, show the encoded JSON query instead of the Lisp
structure gptel uses.

The request data may be edited and the query continued from this
buffer."
  (with-current-buffer (get-buffer-create "*gptel-query*")
    (let* ((standard-output (current-buffer))
           (inhibit-read-only t)
           (request-info (cadr request-args))
           (request-data (plist-get request-info :data)))
      (buffer-disable-undo)
      (erase-buffer)
      (if (eq format 'json)
          (progn (fundamental-mode)
                 (insert (gptel--json-encode request-data))
                 (json-pretty-print-buffer))
        (lisp-data-mode)
        (prin1 request-data)
        (pp-buffer))
      (plist-put request-info :data nil)
      ;; HACK: Reuse `gptel--bounds' to store request args.
      ;; Not ideal, but less fragile than an overlay.
      (setq-local gptel-stream  (car request-args)
                  gptel--bounds (cdr request-args))
      (goto-char (point-min))
      (view-mode 1)
      (setq buffer-undo-list nil)
      (use-local-map
       (make-composed-keymap
        (define-keymap
          "C-c C-c" #'gptel--continue-query
          "C-c C-k" #'quit-window)
        (current-local-map)))
      (unless header-line-format
        (setq header-line-format
              (substitute-command-keys
               (concat
                "Edit request: \\[read-only-mode],"
                " Send request: \\[gptel--continue-query],"
                " Quit: \\[quit-window]"))))
      (display-buffer (current-buffer) gptel-display-buffer-action))))

(defun gptel--continue-query ()
  "Continue sending the gptel query displayed in this buffer.

The request is continued with the same parameters as originally
specified."
  (interactive nil lisp-data-mode fundamental-mode)
  (unless (equal (buffer-name) "*gptel-query*")
    (user-error "This command is meant for use in a gptel dry-run buffer."))
  (save-excursion
    (goto-char (point-min))
    (condition-case-unless-debug nil
        (when-let* ((data (if (eq major-mode 'lisp-data-mode)
                              (read (current-buffer))
                            (gptel--json-read)))
                    (info (car-safe gptel--bounds)))
          (plist-put info :data data)
          (apply (if gptel-use-curl
                     #'gptel-curl-get-response
                   #'gptel--url-get-response)
                 gptel--bounds)
          (quit-window))
      (error
       (user-error "Could not read request data from buffer!")))))

(defun gptel--insert-response (response info)
  "Insert the LLM RESPONSE into the gptel buffer.

INFO is a plist containing information relevant to this buffer.
See `gptel--url-get-response' for details."
  (let* ((status-str  (plist-get info :status))
         (gptel-buffer (plist-get info :buffer))
         (start-marker (plist-get info :position))
         response-beg response-end)
    ;; Handle read-only buffers
    (when (with-current-buffer gptel-buffer
            (or buffer-read-only
                (get-char-property start-marker 'read-only)))
      (message "Buffer is read only, displaying reply in buffer \"*LLM response*\"")
      (display-buffer
       (with-current-buffer (get-buffer-create "*LLM response*")
         (visual-line-mode 1)
         (goto-char (point-max))
         (move-marker start-marker (point) (current-buffer))
         (current-buffer))
       '((display-buffer-reuse-window
          display-buffer-pop-up-window)
         (reusable-frames . visible))))
    ;; Insert response and status message/error message
    (with-current-buffer gptel-buffer
      (if response
          (progn
            (setq response (gptel--transform-response
                               response gptel-buffer))
            (save-excursion
              (put-text-property
               0 (length response) 'gptel 'response response)
              (with-current-buffer (marker-buffer start-marker)
                (goto-char start-marker)
                (run-hooks 'gptel-pre-response-hook)
                (unless (or (bobp) (plist-get info :in-place))
                  (insert "\n\n")
                  (when gptel-mode
                    (insert (gptel-response-prefix-string))))
                (setq response-beg (point)) ;Save response start position
                (insert response)
                (setq response-end (point))
                (pulse-momentary-highlight-region response-beg response-end)
                (when gptel-mode (insert "\n\n" (gptel-prompt-prefix-string)))) ;Save response end position
              (when gptel-mode (gptel--update-status " Ready" 'success))))
        (gptel--update-status
         (format " Response Error: %s" status-str) 'error)
        (message "gptel response error: (%s) %s"
                 status-str (plist-get info :error))))
    ;; Run hook in visible window to set window-point, BUG #269
    (if-let ((gptel-window (get-buffer-window gptel-buffer 'visible)))
        (with-selected-window gptel-window
          (run-hook-with-args 'gptel-post-response-functions response-beg response-end))
      (with-current-buffer gptel-buffer
        (run-hook-with-args 'gptel-post-response-functions response-beg response-end)))))

(defun gptel--create-prompt (&optional prompt-end)
  "Return a full conversation prompt from the contents of this buffer.

If `gptel--num-messages-to-send' is set, limit to that many
recent exchanges.

If the region is active limit the prompt to the region contents
instead.

If `gptel-context--alist' is non-nil and the additional
context needs to be included with the user prompt, add it.

If PROMPT-END (a marker) is provided, end the prompt contents
there."
  (save-excursion
    (save-restriction
      (let* ((max-entries (and gptel--num-messages-to-send
                               (* 2 gptel--num-messages-to-send)))
             (prompt-end (or prompt-end (point-max)))
             (prompts
              (cond
               ((use-region-p)
                ;; Narrow to region
                (narrow-to-region (region-beginning) (region-end))
                (goto-char (point-max))
                (gptel--parse-buffer gptel-backend max-entries))
               ((derived-mode-p 'org-mode)
                (require 'gptel-org)
                (goto-char prompt-end)
                (gptel-org--create-prompt prompt-end))
               (t (goto-char prompt-end)
                  (gptel--parse-buffer gptel-backend max-entries)))))
        ;; NOTE: prompts is modified in place here
        (when gptel-context--alist
          ;; Inject context chunks into the last user prompt if required.
          ;; This is also the fallback for when `gptel-use-context' is set to
          ;; 'system but the model does not support system messages.
          (when (and gptel-use-context
                     (or (eq gptel-use-context 'user)
                         (gptel--model-capable-p 'nosystem))
                     (> (length prompts) 0)) ;FIXME context should be injected
                                             ;even when there are no prompts
            (gptel--wrap-user-prompt gptel-backend prompts))
          ;; Inject media chunks into the first user prompt if required.  Media
          ;; chunks are always included with the first user message,
          ;; irrespective of the preference in `gptel-use-context'.  This is
          ;; because media cannot be included (in general) with system messages.
          (when (and gptel-use-context gptel-track-media
                     (gptel--model-capable-p 'media))
            (gptel--wrap-user-prompt gptel-backend prompts :media)))
        prompts))))

(cl-defgeneric gptel--parse-buffer (backend max-entries)
  "Parse current buffer backwards from point and return a list of prompts.

BACKEND is the LLM backend in use.

MAX-ENTRIES is the number of queries/responses to include for
contexbt.")

(cl-defgeneric gptel--parse-list (backend prompt-list)
  "Parse PROMPT-LIST and return a list of prompts suitable for
BACKEND.

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

(defvar markdown-regex-link-inline)
(defvar markdown-regex-angle-uri)
(declare-function markdown-link-at-pos "markdown-mode")
(declare-function mailcap-file-name-to-mime-type "mailcap")

(cl-defmethod gptel--parse-media-links ((_mode (eql 'markdown-mode)) beg end)
  "Parse text and actionable links between BEG and END.

Return a list of the form
 ((:text \"some text\")
  (:media \"/path/to/media.png\" :mime \"image/png\")
  (:text \"More text\"))
for inclusion into the user prompt for the gptel request."
  (require 'mailcap)                    ;FIXME Avoid this somehow
  (let ((parts) (from-pt))
    (save-excursion
      (setq from-pt (goto-char beg))
      (while (re-search-forward
              (concat "\\(?:" markdown-regex-link-inline "\\|"
                      markdown-regex-angle-uri "\\)")
              end t)
        (when-let* ((link-at-pt (markdown-link-at-pos (point)))
                    ((gptel--link-standalone-p
                      (car link-at-pt) (cadr link-at-pt)))
                    (path (nth 3 link-at-pt))
                    (path (string-remove-prefix "file://" path))
                    (mime (mailcap-file-name-to-mime-type path))
                    ((gptel--model-mime-capable-p mime)))
          (cond
           ((seq-some (lambda (p) (string-prefix-p p path))
                      '("https:" "http:" "ftp:"))
            ;; Collect text up to this image, and collect this image url
            (when (gptel--model-capable-p 'url) ; FIXME This is not a good place
                                                ; to check for url capability!
              (push (list :text (buffer-substring-no-properties from-pt (car link-at-pt)))
                    parts)
              (push (list :url path :mime mime) parts)
              (setq from-pt (cadr link-at-pt))))
           ((file-readable-p path)
            ;; Collect text up to this image, and collect this image
            (push (list :text (buffer-substring-no-properties from-pt (car link-at-pt)))
                  parts)
            (push (list :media path :mime mime) parts)
            (setq from-pt (cadr link-at-pt)))))))
    (unless (= from-pt end)
      (push (list :text (buffer-substring-no-properties from-pt end)) parts))
    (nreverse parts)))

(cl-defgeneric gptel--wrap-user-prompt (backend _prompts)
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

;; TODO: Use `run-hook-wrapped' with an accumulator instead to handle
;; buffer-local hooks, etc.
(defun gptel--transform-response (content-str buffer)
  "Filter CONTENT-STR through `gptel-response-filter-functions`.

BUFFER is passed along with CONTENT-STR to each function in this
hook."
  (let ((filtered-str content-str))
    (dolist (filter-func gptel-response-filter-functions filtered-str)
      (condition-case nil
          (when (functionp filter-func)
            (setq filtered-str
                  (funcall filter-func filtered-str buffer)))
        (error
         (display-warning '(gptel filter-functions)
                          (format "Function %S returned an error"
                                  filter-func)))))))

(defun gptel--convert-org (content buffer)
  "Transform CONTENT according to required major-mode.

Currently only `org-mode' is handled.

BUFFER is the LLM interaction buffer."
  (if (with-current-buffer buffer (derived-mode-p 'org-mode))
      (gptel--convert-markdown->org content)
    content))

(defun gptel--url-get-response (info &optional callback)
  "Fetch response to prompt in INFO from the LLM.

INFO is a plist with the following keys:
- :data     (the data being sent)
- :buffer   (the gptel buffer)
- :position (marker at which to insert the response).

Call CALLBACK with the response and INFO afterwards.  If omitted
the response is inserted into the current buffer after point."
  (let* ((inhibit-message t)
         (message-log-max nil)
         (backend gptel-backend)
         (url-request-method "POST")
         (url-request-extra-headers
          (append '(("Content-Type" . "application/json"))
                  (when-let ((header (gptel-backend-header gptel-backend)))
                    (if (functionp header)
                        (funcall header) header))))
        (url-request-data
         (encode-coding-string
          (gptel--json-encode (plist-get info :data))
          'utf-8)))
    ;; why do these checks not occur inside of `gptel--log'?
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
                          (pcase-let ((`(,response ,http-msg ,error)
                                       (gptel--url-parse-response backend info))
                                      (buf (current-buffer)))
                            (plist-put info :status http-msg)
                            (when error (plist-put info :error error))
                            (with-demoted-errors "gptel callback error: %S"
                              (funcall (or callback #'gptel--insert-response)
                                       response info))
                            (setf (alist-get buf gptel--request-alist nil 'remove) nil)
                            (kill-buffer buf)))
                        nil t nil)))
      (setf (alist-get proc-buf gptel--request-alist)
          ;; TODO: Add transformer here.  NOTE: We need info to be mutated here.
          (nconc info (list :callback callback :backend backend))))))

(cl-defgeneric gptel--parse-response (backend response proc-info)
  "Response extractor for LLM requests.

BACKEND is the LLM backend in use.

RESPONSE is the parsed JSON of the response, as a plist.

PROC-INFO is a plist with process information and other context.
See `gptel-curl--get-response' for its contents.")

(defvar url-http-end-of-headers)
(defvar url-http-response-status)
(defun gptel--url-parse-response (backend proc-info)
  "Parse response from BACKEND in RESPONSE-BUFFER."
  (when gptel-log-level             ;logging
    (save-excursion
      (goto-char url-http-end-of-headers)
      (when (eq gptel-log-level 'debug)
        (gptel--log (gptel--json-encode (buffer-substring-no-properties (point-min) (point)))
                    "response headers"))
      (gptel--log (buffer-substring-no-properties (point) (point-max))
                  "response body")))
  (if-let* ((http-msg (string-trim (buffer-substring (line-beginning-position)
                                                     (line-end-position))))
            (response (progn (goto-char url-http-end-of-headers)
                             (condition-case nil
                                 (gptel--json-read)
                               (error 'json-read-error)))))
      (cond
       ;; FIXME Handle the case where HTTP 100 is followed by HTTP (not 200) BUG #194
       ((or (memq url-http-response-status '(200 100))
            (string-match-p "\\(?:1\\|2\\)00 OK" http-msg))
        (list (string-trim (gptel--parse-response backend response
                                                  proc-info))
              http-msg))
       ((plist-get response :error)
        (let* ((error-data (plist-get response :error))
               (error-msg (plist-get error-data :message))
               (error-type (plist-get error-data :type))
               (backend-name (gptel-backend-name backend)))
          (if (stringp error-data)
              (progn
		(message "%s error: (%s) %s" backend-name http-msg error-data)
                (setq error-msg (string-trim error-data)))
            (when (stringp error-msg)
              (message "%s error: (%s) %s" backend-name http-msg (string-trim error-msg)))
            (when error-type
	      (setq http-msg (concat "("  http-msg ") " (string-trim error-type)))))
          (list nil (concat "(" http-msg ") " (or error-msg "")))))
       ((eq response 'json-read-error)
        (list nil (concat "(" http-msg ") Malformed JSON in response.") "json-read-error"))
       (t (list nil (concat "(" http-msg ") Could not parse HTTP response.")
                "Could not parse HTTP response.")))
    (list nil (concat "(" http-msg ") Could not parse HTTP response.")
          "Could not parse HTTP response.")))

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

;;;###autoload
(defun gptel (name &optional _ initial interactivep)
  "Switch to or start a chat session with NAME.

Ask for API-KEY if `gptel-api-key' is unset.

If region is active, use it as the INITIAL prompt.  Returns the
buffer created or switched to.

INTERACTIVEP is t when gptel is called interactively."
  (interactive
   (let* ((backend (default-value 'gptel-backend))
          (backend-name
           (format "*%s*" (gptel-backend-name backend))))
     (list (read-buffer
            "Create or choose gptel buffer: "
            backend-name nil                         ; DEFAULT and REQUIRE-MATCH
            (lambda (b)                                   ; PREDICATE
              ;; NOTE: buffer check is required (#450)
              (and-let* ((buf (get-buffer (or (car-safe b) b))))
                (buffer-local-value 'gptel-mode buf))))
           (condition-case nil
               (gptel--get-api-key
                (gptel-backend-key backend))
             ((error user-error)
              (setq gptel-api-key
                    (read-passwd
                     (format "%s API key: " backend-name)))))
           (and (use-region-p)
                (buffer-substring (region-beginning)
                                  (region-end)))
           t)))
  (with-current-buffer (get-buffer-create name)
    (cond ;Set major mode
     ((eq major-mode gptel-default-mode))
     ((eq gptel-default-mode 'text-mode)
      (text-mode)
      (visual-line-mode 1))
     (t (funcall gptel-default-mode)))
    (gptel--sanitize-model :backend (default-value 'gptel-backend)
                           :model (default-value 'gptel-model)
                           :shoosh nil)
    (unless gptel-mode (gptel-mode 1))
    (goto-char (point-max))
    (skip-chars-backward "\t\r\n")
    (if (bobp) (insert (or initial (gptel-prompt-prefix-string))))
    (when interactivep
      (display-buffer (current-buffer) gptel-display-buffer-action)
      (message "Send your query with %s!"
               (substitute-command-keys "\\[gptel-send]")))
    (current-buffer)))


;;; Response tweaking commands

(defun gptel--attach-response-history (history &optional buf)
  "Attach HISTORY to the next gptel response in buffer BUF.

HISTORY is a list of strings typically containing text replaced
by gptel.  BUF is the current buffer if not specified.

This is used to maintain variants of prompts or responses to diff
against if required."
  (with-current-buffer (or buf (current-buffer))
    (letrec ((gptel--attach-after
              (lambda (b e)
                (when (and b e)
                  (put-text-property
                   b e 'gptel-history
                   (append (ensure-list history)
                           (get-char-property (1- e) 'gptel-history))))
                (remove-hook 'gptel-post-response-functions
                             gptel--attach-after 'local))))
      (add-hook 'gptel-post-response-functions gptel--attach-after
                nil 'local))))

(defun gptel--ediff (&optional arg bounds-func)
  "Ediff response at point against previous gptel responses.

If prefix ARG is non-nil, select the previous response to ediff
against interactively.

If specified, use BOUNDS-FUNC to compute the bounds of the
response at point.  This can be used to include additional
context for the ediff session."
  (interactive "P")
  (when (gptel--at-response-history-p)
    (pcase-let* ((`(,beg . ,end) (funcall (or bounds-func #'gptel--get-bounds)))
                 (prev-response
                  (if arg
                      (completing-read "Choose response variant to diff against: "
                                       (get-char-property (point) 'gptel-history)
                                       nil t)
                    (car-safe (get-char-property (point) 'gptel-history))))
                 (buffer-mode major-mode)
                 (bufname (buffer-name))
                 (`(,new-buf ,new-beg ,new-end)
                  (with-current-buffer
                      (get-buffer-create (concat bufname "-PREVIOUS-*"))
                    (let ((inhibit-read-only t))
                      (erase-buffer)
                      (delay-mode-hooks (funcall buffer-mode))
                      (visual-line-mode)
                      (insert prev-response)
                      (goto-char (point-min))
                      (list (current-buffer) (point-min) (point-max))))))
      (unless prev-response (user-error "gptel response is additive: no changes to ediff"))
      (require 'ediff)
      (letrec ((cwc (current-window-configuration))
               (gptel--ediff-restore
                (lambda ()
                  (when (window-configuration-p cwc)
                    (set-window-configuration cwc))
                  (kill-buffer (get-buffer (concat bufname "-PREVIOUS-*")))
                  (kill-buffer (get-buffer (concat bufname "-CURRENT-*")))
                  (remove-hook 'ediff-quit-hook gptel--ediff-restore))))
        (add-hook 'ediff-quit-hook gptel--ediff-restore)
        (apply
         #'ediff-regions-internal
         (get-buffer (ediff-make-cloned-buffer (current-buffer) "-CURRENT-*"))
         beg end new-buf new-beg new-end
         nil
         (list 'ediff-regions-wordwise 'word-wise nil)
         ;; (if (transient-arg-value "-w" args)
         ;;     (list 'ediff-regions-wordwise 'word-wise nil)
         ;;   (list 'ediff-regions-linewise nil nil))
         )))))

(defun gptel--mark-response ()
  "Mark gptel response at point, if any."
  (interactive)
  (unless (gptel--in-response-p) (user-error "No gptel response at point"))
  (pcase-let ((`(,beg . ,end) (gptel--get-bounds)))
    (goto-char beg) (push-mark) (goto-char end) (activate-mark)))

(defun gptel--previous-variant (&optional arg)
  "Switch to previous gptel-response at this point, if it exists."
  (interactive "p")
  (pcase-let* ((`(,beg . ,end) (gptel--get-bounds))
               (history (get-char-property (point) 'gptel-history))
               (alt-response (car-safe history))
               (offset))
    (unless (and history alt-response)
      (user-error "No variant responses available"))
    (if (> arg 0)
        (setq history (append (cdr history)
                              (list (buffer-substring-no-properties beg end))))
      (setq
       alt-response (car (last history))
       history (cons (buffer-substring-no-properties beg end)
                     (nbutlast history))))
    (add-text-properties
             0 (length alt-response)
             `(gptel response gptel-history ,history)
             alt-response)
    (setq offset (min (- (point) beg) (1- (length alt-response))))
    (delete-region beg end)
    (insert alt-response)
    (goto-char (+ beg offset))
    (pulse-momentary-highlight-region beg (+ beg (length alt-response)))))

(defun gptel--next-variant (&optional arg)
  "Switch to next gptel-response at this point, if it exists."
  (interactive "p")
  (gptel--previous-variant (- arg)))

(provide 'gptel)
;;; gptel.el ends here

;; Local Variables:
;; bug-reference-url-format: "https://github.com/karthink/gptel/issues/%s"
;; End:
