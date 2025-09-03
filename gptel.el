;;; gptel.el --- Interact with ChatGPT or other LLMs     -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthik.chikmagalur@gmail.com>
;; Version: 0.9.9
;; Package-Requires: ((emacs "27.1") (transient "0.7.4") (compat "30.1.0.0"))
;; Keywords: convenience, tools
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
;; gptel supports:
;;
;; - The services ChatGPT, Azure, Gemini, Anthropic AI, Together.ai, Perplexity,
;;   AI/ML API, Anyscale, OpenRouter, Groq, PrivateGPT, DeepSeek, Cerebras, Github Models,
;;   GitHub Copilot chat, AWS Bedrock, Novita AI, xAI, Sambanova, Mistral Le
;;   Chat and Kagi (FastGPT & Summarizer).
;; - Local models via Ollama, Llama.cpp, Llamafiles or GPT4All
;;
;; Additionally, any LLM service (local or remote) that provides an
;; OpenAI-compatible API is supported.
;;
;; Features:
;;
;; - Interact with LLMs from anywhere in Emacs (any buffer, shell, minibuffer,
;;   wherever).
;; - LLM responses are in Markdown or Org markup.
;; - Supports conversations and multiple independent sessions.
;; - Supports tool-use to equip LLMs with agentic capabilities.
;; - Supports Model Context Protocol (MCP) integration using the mcp.el package.
;; - Supports multi-modal models (send images, documents).
;; - Supports "reasoning" content in LLM responses.
;; - Save chats as regular Markdown/Org/Text files and resume them later.
;; - You can go back and edit your previous prompts or LLM responses when
;;   continuing a conversation.  These will be fed back to the model.
;; - Redirect prompts and responses easily
;; - Rewrite, refactor or fill in regions in buffers.
;; - Write your own commands for custom tasks with a simple API.
;;
;; Requirements for ChatGPT, Azure, Gemini or Kagi:
;;
;; - You need an appropriate API key.  Set the variable `gptel-api-key' to the
;;   key or to a function of no arguments that returns the key.  (It tries to
;;   use `auth-source' by default)
;;
;; ChatGPT is configured out of the box.  For the other sources:
;;
;; - For Azure: define a gptel-backend with `gptel-make-azure', which see.
;; - For Gemini: define a gptel-backend with `gptel-make-gemini', which see.
;; - For Anthropic (Claude): define a gptel-backend with `gptel-make-anthropic',
;;   which see.
;; - For AI/ML API, Together.ai, Anyscale, Groq, OpenRouter, DeepSeek, Cerebras or
;;   Github Models: define a gptel-backend with `gptel-make-openai', which see.
;; - For PrivateGPT: define a backend with `gptel-make-privategpt', which see.
;; - For Perplexity: define a backend with `gptel-make-perplexity', which see.
;; - For Deepseek: define a backend with `gptel-make-deepseek', which see.
;; - For Kagi: define a gptel-backend with `gptel-make-kagi', which see.
;;
;; For local models using Ollama, Llama.cpp or GPT4All:
;;
;; - The model has to be running on an accessible address (or localhost)
;; - Define a gptel-backend with `gptel-make-ollama' or `gptel-make-gpt4all',
;;   which see.
;; - Llama.cpp or Llamafiles: Define a gptel-backend with `gptel-make-openai'.
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
;; - M-x gptel: Start a chat session.
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
;; regions, buffers, files or directories to the query with `gptel-add'.  To add
;; text or media files, call `gptel-add' in Dired or use the dedicated
;; `gptel-add-file'.
;;
;; You can also add context from gptel's menu instead (`gptel-send' with a
;; prefix arg), as well as examine or modify context.
;;
;; When context is available, gptel will include it with each LLM query.
;;
;; LLM Tool use:
;;
;; gptel supports "tool calling" behavior, where LLMs can specify arguments with
;; which to call provided "tools" (elisp functions).  The results of running the
;; tools are fed back to the LLM, giving it capabilities and knowledge beyond
;; what is available out of the box.  For example, tools can perform web
;; searches or API lookups, modify files and directories, and so on.
;;
;; Tools can be specified via `gptel-make-tool', or obtained from other
;; repositories, or from Model Context Protocol (MCP) servers using the mcp.el
;; package.  See the README for details.
;;
;; Tools can be included with LLM queries using gptel's menu, or from
;; `gptel-tools'.
;;
;; Rewrite interface
;;
;; In any buffer: with a region selected, you can rewrite prose, refactor code
;; or fill in the region.  This is accessible via `gptel-rewrite', and also from
;; the `gptel-send' menu.
;;
;; Presets
;;
;; Define a bundle of configuration (model, backend, system message, tools etc)
;; as a "preset" that can be applied together, making it easy to switch between
;; tasks in gptel.  Presets can be saved and applied from gptel's transient
;; menu.  You can also include a cookie of the form "@preset-name" in the prompt
;; to send a request with a preset applied.  This feature works everywhere, but
;; preset cookies are also fontified in chat buffers.
;;
;; gptel in Org mode:
;;
;; gptel offers a few extra conveniences in Org mode:
;;
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
;; Finally, gptel offers a general purpose API for writing LLM ineractions that
;; suit your workflow.  See `gptel-request', and `gptel-fsm' for more advanced
;; usage.

;;; Code:
(declare-function markdown-mode "markdown-mode")
(declare-function gptel-curl-get-response "gptel-curl")
(declare-function gptel-menu "gptel-transient")
(declare-function gptel-system-prompt "gptel-transient")
(declare-function gptel-tools "gptel-transient")
(declare-function pulse-momentary-highlight-region "pulse")

(declare-function ediff-make-cloned-buffer "ediff-util")
(declare-function ediff-regions-internal "ediff")
(declare-function hl-line-highlight "hl-line")

(declare-function org-escape-code-in-string "org-src")
(declare-function gptel-org--create-prompt-buffer "gptel-org")
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

(defcustom gptel-response-separator "\n\n"
  "String inserted before responses.

Also inserted before and after non-consecutive tool calls."
  :type 'string)

(defcustom gptel-use-header-line t
  "Whether `gptel-mode' should use header-line for status information.

When set to nil, use the mode line for (minimal) status
information and the echo area for messages."
  :type 'boolean)

;; Set minimally to avoid display-buffer action alist conflicts (#533)
(defcustom gptel-display-buffer-action `(nil (body-function . ,#'select-window))
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
respectively. It can also be a list of symbols:

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

When the active `gptel-model' supports it, gptel can send text, images
or other media from links in chat buffers to the LLM.  To use this, the
following steps are required.

1. `gptel-track-media' (this variable) should be non-nil

2. The LLM should provide vision or document support.  (See
`gptel-make-openai', `gptel-make-anthropic', `gptel-make-ollama' or
`gptel-make-gemini' for details on how to specify media support for
models.)

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

(defvar-local gptel--old-header-line nil)

(defvar gptel-context--alist nil
  "List of gptel's context sources.

Each entry is of the form
 (buffer . (overlay1 overlay2 ...))
or
 (\"path/to/file\").")

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

(defvar url-http-end-of-headers)
(defvar url-http-response-status)
(cl-defun gptel--url-retrieve (url &key method data headers)
  "Retrieve URL synchronously with METHOD, DATA and HEADERS."
  (declare (indent 1))
  (let ((url-request-method (if (eq method'post) "POST" "GET"))
        (url-request-data (encode-coding-string (gptel--json-encode data) 'utf-8))
        (url-mime-accept-string "application/json")
        (url-request-extra-headers
         `(("content-type" . "application/json")
           ,@headers)))
    (with-current-buffer (url-retrieve-synchronously url 'silent)
      (goto-char url-http-end-of-headers)
      (gptel--json-read))))

(defun gptel-auto-scroll ()
  "Scroll window if LLM response continues below viewport.

Note: This will move the cursor."
  (when-let* ((win (get-buffer-window (current-buffer) 'visible))
              ((not (pos-visible-in-window-p (point) win)))
              (scroll-error-top-bottom t))
    (condition-case nil
        (with-selected-window win
          (scroll-up-command))
      (error nil))))

(defsubst gptel-prompt-prefix-string ()
  "Prefix before user prompts in `gptel-mode'."
  (declare (side-effect-free t))
  (or (alist-get major-mode gptel-prompt-prefix-alist) ""))

(defsubst gptel-response-prefix-string ()
  "Prefix before LLM responses in `gptel-mode'."
  (declare (side-effect-free t))
  (or (alist-get major-mode gptel-response-prefix-alist) ""))

(defun gptel-beginning-of-response (&optional _ _ arg)
  "Move point to the beginning of the LLM response ARG times."
  (interactive (list nil nil
                     (prefix-numeric-value current-prefix-arg)))
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
          (when-let* ((prefix (gptel-prompt-prefix-string))
                      ((not (string-empty-p prefix)))
                      ((looking-at (concat "\n\\{1,2\\}"
                                           (regexp-quote prefix) "?"))))
            (goto-char (match-end 0)))
        (when-let* ((prefix (gptel-response-prefix-string))
                    ((not (string-empty-p prefix)))
                    ((looking-back (concat (regexp-quote prefix) "?")
                                   (point-min))))
          (goto-char (match-beginning 0)))))))

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
                      gptel-use-context gptel--num-messages-to-send
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

(defsubst gptel--curl-path ()
  "Curl executable to use."
  (if (stringp gptel-use-curl) gptel-use-curl "curl"))

(defun gptel--transform-add-context (callback fsm)
  (if (and gptel-use-context gptel-context--alist)
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

;;;; Response text recognition

(defun gptel--get-buffer-bounds ()
  "Return the gptel response boundaries in the buffer as an alist."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (let ((bounds) (prev-pt (point)))
        (while (and (/= prev-pt (point-min))
                    (goto-char (previous-single-property-change
                                (point) 'gptel nil (point-min))))
          (when-let* ((prop (get-char-property (point) 'gptel)))
            (let* ((prop-name (if (symbolp prop) prop (car prop)))
                   (val (when (consp prop) (cdr prop)))
                   (bound (if val
                              (list (point) prev-pt val)
                            (list (point) prev-pt))))
              (push bound (alist-get prop-name bounds))))
          (setq prev-pt (point)))
        bounds))))

(define-obsolete-function-alias
  'gptel--get-bounds 'gptel--get-response-bounds "0.9.8")

(defun gptel--get-response-bounds ()
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
  (eq (get-char-property (or pt (point)) 'gptel) 'response))

(defun gptel--at-response-history-p (&optional pt)
  "Check if gptel response at position PT has variants."
  (get-char-property (or pt (point)) 'gptel-history))

(defvar gptel--mode-description-alist
  '((js2-mode      . "Javascript")
    (sh-mode       . "Shell")
    (enh-ruby-mode . "Ruby")
    (yaml-mode     . "Yaml")
    (yaml-ts-mode  . "Yaml")
    (rustic-mode   . "Rust")
    (tuareg-mode   . "OCaml"))
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


;;; Saving and restoring state

(defun gptel--restore-props (bounds-alist)
  "Restore text properties from BOUNDS-ALIST.
BOUNDS-ALIST is (PROP . BOUNDS).  BOUNDS is a list of BOUND.  Each BOUND
is either (BEG END VAL) or (BEG END).

For (BEG END VAL) forms, even if VAL is nil, the gptel property will be
set to (PROP . VAL).  For (BEG END) forms, except when PROP is response,
the gptel property is set to just PROP.

The legacy structure, a list of (BEG . END) is also supported and will be
applied before being re-persisted in the new structure."
  (let ((modified (buffer-modified-p)))
    (if (symbolp (caar bounds-alist))
        (mapc
         (lambda (bounds)
           (let* ((prop (pop bounds)))
             (mapc
              (lambda (bound)
                (let ((prop-has-val (> (length bound) 2)))
                  (add-text-properties
                   (pop bound) (pop bound)
                   (if (eq prop 'response)
                       '(gptel response front-sticky (gptel))
                     (list 'gptel
                           (if prop-has-val
                               (cons prop (pop bound))
                             prop))))))
              bounds)))
         bounds-alist)
      (mapc (lambda (bound)
              (add-text-properties
               (car bound) (cdr bound) '(gptel response front-sticky (gptel))))
            bounds-alist))
    (set-buffer-modified-p modified)))

(defun gptel--restore-state ()
  "Restore gptel state when turning on `gptel-mode'."
  (when (buffer-file-name)
    (if (derived-mode-p 'org-mode)
        (progn
          (require 'gptel-org)
          (gptel-org--restore-state))
      (when gptel--bounds
        (gptel--restore-props gptel--bounds)
        (message "gptel chat restored."))
      (when gptel--backend-name
        (if-let* ((backend (alist-get
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

(defun gptel--inherit-stickiness (beg end pre)
  "Mark any change to an LLM response region as a response.

Intended to be added to `after-change-functions' in gptel chat buffers,
which see for BEG, END and PRE."
  (and (= pre 0) (< end (point-max))
       (and-let* ((val (get-text-property end 'gptel)))
         (add-text-properties
          beg end `(gptel ,val front-sticky (gptel))))))

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
        (unless (derived-mode-p 'org-mode 'markdown-mode 'text-mode)
          (gptel-mode -1)
          (user-error (format "`gptel-mode' is not supported in `%s'." major-mode)))
        (add-hook 'before-save-hook #'gptel--save-state nil t)
        (add-hook 'after-change-functions 'gptel--inherit-stickiness nil t)
        (gptel--prettify-preset)
        (when (derived-mode-p 'org-mode)
          ;; Work around bug in `org-fontify-extend-region'.
          (add-hook 'gptel-post-response-functions #'font-lock-flush nil t))
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
                                   "Ignoring images from standalone links/urls.\nClick to toggle"))))
                              (toggle-tools (lambda (&rest _) (interactive)
                                              (run-at-time 0 nil
                                               (lambda () (call-interactively #'gptel-tools)))))
                              (tools (when (and gptel-use-tools gptel-tools)
                                      (propertize
                                       (buttonize (pcase (length gptel-tools)
                                                   (0 "[No tools]") (1 "[1 tool]")
                                                   (len (format "[%d tools]" len)))
                                        toggle-tools)
                                       'mouse-face 'highlight
                                       'help-echo "Select tools"))))
                         (concat
                          (propertize
                           " " 'display
                           `(space :align-to (- right
                                              ,(+ 5 (length model) (length system)
                                                (length track-media) (length context) (length tools)))))
                          tools (and track-media " ") track-media (and context " ") context " " system " "
                          (propertize
                           (buttonize (concat "[" model "]")
                            (lambda (&rest _) (gptel-menu)))
                           'mouse-face 'highlight
                           'help-echo "Model in use"))))))
          (setq mode-line-process
                '(:eval (concat " "
                         (buttonize (gptel--model-name gptel-model)
                            (lambda (&rest _) (gptel-menu))))))))
    (remove-hook 'before-save-hook #'gptel--save-state t)
    (remove-hook 'after-change-functions 'gptel--inherit-stickiness t)
    (gptel--prettify-preset)
    (if gptel-use-header-line
        (setq header-line-format gptel--old-header-line
              gptel--old-header-line nil)
      (setq mode-line-process nil))))

(defvar gptel--fsm-last)                ;Defined further below
(defun gptel--update-status (&optional msg face)
  "Update status MSG in FACE."
  (when gptel-mode
    (if gptel-use-header-line
        (and (consp header-line-format)
             (setf (nth 1 header-line-format)
                   (thread-first
                     msg
                     (buttonize (lambda (_) (gptel--inspect-fsm)))
                     (propertize 'face face 'mouse-face 'highlight))))
      (if (member msg '(" Typing..." " Waiting..."))
          (setq mode-line-process (propertize msg 'face face))
        (setq mode-line-process
              '(:eval (concat " "
                       (buttonize (gptel--model-name gptel-model)
                            (lambda (&rest _) (gptel-menu))))))
        (message (propertize msg 'face face))))
    (force-mode-line-update)))

(declare-function gptel-context--wrap "gptel-context")


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

If set to t, any tools selected in `gptel-tools' will be made
available to the LLM.  This is the default.  It has no effect if
no tools are selected.

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
tools and in gptel's UI, see `gptel-tools'.

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
four are required.

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

CONFIRM: Whether the tool call should wait for the user to run
it.  If true, the user will be prompted with the proposed tool
call, which can be examined, accepted, deferred or canceled.

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
  "Return a BACKEND-appropriate prompt containing tool call RESULTS.

This will be injected into the messages list in the prompt to
send to the LLM.")

;; FIXME(fsm) unify this with `gptel--wrap-user-prompt', which is a mess
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
is either a predicate function or `t'.  When `gptel--fsm-next' is
called, the predicates are called in the order they appear here
to find the next state.  Each predicate is called with the state
machine's INFO, see `gptel-fsm'.  A predicate of `t' is
considered a success and acts as a default.")

(defvar gptel-request--handlers
  `((WAIT ,#'gptel--handle-wait)
    (TOOL ,#'gptel--handle-tool-use))
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

(defvar gptel-send--handlers
  `((WAIT ,#'gptel--handle-wait)
    (TYPE ,#'gptel--handle-pre-insert)
    (ERRS ,#'gptel--handle-error ,#'gptel--fsm-last)
    (TOOL ,#'gptel--handle-tool-use)
    (DONE ,#'gptel--handle-post-insert ,#'gptel--fsm-last))
  "Alist specifying handlers for `gptel-send' state transitions.

See `gptel-request--handlers' for details.")

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

(defvar-local gptel--fsm-last nil
  "State machine for latest request in the buffer.")

(defun gptel--fsm-last (fsm)
    "Capture the latest request state FSM for introspection."
    (let ((info (gptel-fsm-info fsm)))
      (unless gptel-log-level
        (let ((data (plist-get info :data)))
          (dolist (key '(:messages :contents :query))
            (setf (plist-get data key) nil))))
      (setf (gptel-fsm-info fsm)
            (plist-put info :end-time (current-time-string)))
      (with-current-buffer (plist-get info :buffer)
        (setq gptel--fsm-last fsm))))

(defun gptel--inspect-fsm (&optional fsm)
  "Inspect gptel request state FSM.

FSM defaults to the state of the last request in the current
buffer."
  (unless fsm
    (setq fsm (or (cdr-safe (cl-find-if
                             (lambda (proc-list)
                               (eq (thread-first (cadr proc-list)
                                                 (gptel-fsm-info)
                                                 (plist-get :buffer))
                                   (current-buffer)))
                             gptel--request-alist))
                  gptel--fsm-last)))
  (unless (cl-typep gptel--fsm-last 'gptel-fsm)
    (user-error "No gptel request log in this buffer yet!"))
  (require 'tabulated-list)
  (with-current-buffer (get-buffer-create "*gptel-diagnostic*")
    (setq tabulated-list-format [("Request attribute" 30 t) ("Value" 30)])
    (let* ((pb (lambda (s) (propertize s 'face 'font-lock-builtin-face)))
           (ps (lambda (s) (propertize s 'face 'font-lock-string-face)))
           (fmt (lambda (s) (cond ((memq (car-safe s) '(closure lambda))
                              (format "#<lambda %#x>" (sxhash s)))
                             ((byte-code-function-p s)
                              (format "#<compiled %#x>" (sxhash s)))
                             ((stringp s) (string-replace "\n" "⮐ " s))
                             (t (prin1-to-string s)))))
           (inhibit-read-only t)
           (info (gptel-fsm-info fsm))
           (entries-info
            (cl-loop
             for idx upfrom 3
             for (key val) on info by #'cddr
             unless (memq key '(:data :history :tools
                                :partial_text :partial_json))
             collect
             (list idx `[,(funcall pb (symbol-name key))
                         ,(funcall ps (funcall fmt val))])))
           (entries-data
            (cl-loop
             for idx upfrom 50
             for (key val) on (plist-get info :data) by #'cddr
             unless (memq key '(:messages :stream :contents :query))
             collect
             (list idx `[,(funcall pb (symbol-name key))
                         ,(funcall ps (funcall fmt val))]))))
      (setq tabulated-list-entries
            (nconc (list `(2 [,(funcall pb ":state")
                              ,(funcall ps
                                (mapconcat
                                 fmt (reverse (cons (gptel-fsm-state fsm)
                                               (plist-get info :history)))
                                 " → "))]))
                   entries-info
                   entries-data))
      (tabulated-list-print)
      (tabulated-list-mode)
      (tabulated-list-init-header)
      (hl-line-mode 1)
      (display-buffer
       (current-buffer)
       '((display-buffer-in-side-window)
         (side . bottom)
         (window-height . fit-window-to-buffer)
         (slot . 10)
         (body-function . select-window))))))

;;;; State machine handlers
;; The next few functions are default state handlers for gptel's state machine,
;; see `gptel-request--handlers'.

(defun gptel--handle-wait (fsm)
  "Fire the request contained in state machine FSM's info."
  ;; Reset some flags in info.  This is necessary when reusing fsm's context for
  ;; a second network request: gptel tests for the presence of these flags to
  ;; handle state transitions.  (NOTE: Don't add :token to this.)
  (let ((info (gptel-fsm-info fsm)))
    (dolist (key '(:tool-success :tool-use :error
                   :http-status :reasoning :reasoning-block))
      (when (plist-get info key)
        (plist-put info key nil))))
  (funcall
   (if gptel-use-curl
       #'gptel-curl-get-response
     #'gptel--url-get-response)
   fsm)
  (run-hooks 'gptel-post-request-hook)
  (with-current-buffer (plist-get (gptel-fsm-info fsm) :buffer)
    (gptel--update-status " Waiting..." 'warning)))

(defun gptel--handle-pre-insert (fsm)
  "Tasks before inserting the LLM response for state FSM.

Handle read-only buffers and run pre-response hooks (but only if
the request succeeded)."
  (let* ((info (gptel-fsm-info fsm))
         (start-marker (plist-get info :position)))
    (when (and
           (memq (plist-get info :callback)
                 '(gptel--insert-response gptel-curl--stream-insert-response))
           (with-current-buffer (plist-get info :buffer)
             (or buffer-read-only
                 (get-char-property start-marker 'read-only))))
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
    (with-current-buffer (marker-buffer start-marker)
      (when (plist-get info :stream)
        (gptel--update-status " Typing..." 'success))
      (save-excursion
        (goto-char start-marker)
        (when (and (member (plist-get info :http-status) '("200" "100"))
                   gptel-pre-response-hook)
          (run-hooks 'gptel-pre-response-hook))))))

(defun gptel--handle-post-insert (fsm)
  "Tasks after successfully inserting the LLM response with state FSM.

Indicate gptel status, pulse the inserted text and run post-response hooks.

No state transition here since that's handled by the process sentinels."
  (let* ((info (gptel-fsm-info fsm))
         (start-marker (plist-get info :position))
         (tracking-marker (or (plist-get info :tracking-marker)
                              start-marker))
         ;; start-marker may have been moved if :buffer was read-only
         (gptel-buffer (marker-buffer start-marker)))
    (with-current-buffer gptel-buffer
      (if (not tracking-marker)         ;Empty response
          (when gptel-mode (gptel--update-status " Empty response" 'success))
        (pulse-momentary-highlight-region start-marker tracking-marker)
        (when gptel-mode
          (save-excursion (goto-char tracking-marker)
                          (insert gptel-response-separator
                                  (gptel-prompt-prefix-string)))
          (gptel--update-status  " Ready" 'success))))
    ;; Run hook in visible window to set window-point, BUG #269
    (if-let* ((gptel-window (get-buffer-window gptel-buffer 'visible)))
        (with-selected-window gptel-window
          (run-hook-with-args
           'gptel-post-response-functions
           (marker-position start-marker) (marker-position tracking-marker)))
      (with-current-buffer gptel-buffer
        (run-hook-with-args
         'gptel-post-response-functions
         (marker-position start-marker) (marker-position tracking-marker))))))

(defun gptel--handle-error (fsm)
  "Check for errors in request state FSM perform UI updates.

Run post-response hooks."
  (when-let* ((info (gptel-fsm-info fsm))
              (error-data (plist-get info :error))
              (http-msg   (plist-get info :status))
              (gptel-buffer (plist-get info :buffer))
              (start-marker (plist-get info :position))
              (tracking-marker (or (plist-get info :tracking-marker)
                              start-marker))
              (backend-name
               (gptel-backend-name
                (buffer-local-value 'gptel-backend gptel-buffer))))
    (if (stringp error-data)
        (message "%s error: (%s) %s" backend-name http-msg (string-trim error-data))
      (when-let* ((error-type (plist-get error-data :type)))
        (setq http-msg (concat "("  http-msg ") " (string-trim error-type))))
      (when-let* ((error-msg (plist-get error-data :message)))
        (message "%s error: (%s) %s" backend-name http-msg (string-trim error-msg))))
    (with-current-buffer gptel-buffer
      (when gptel-mode
        (gptel--update-status
         (format " Error: %s" http-msg) 'error)))
    (if-let* ((gptel-window (get-buffer-window gptel-buffer 'visible)))
        (with-selected-window gptel-window
          (run-hook-with-args
           'gptel-post-response-functions
           (marker-position start-marker) (marker-position tracking-marker)))
      (with-current-buffer gptel-buffer
        (run-hook-with-args
         'gptel-post-response-functions
         (marker-position start-marker) (marker-position tracking-marker))))))

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
      (when gptel-mode
        (gptel--update-status
         (format " Calling tool..." ) 'mode-line-emphasis))

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
               (if (and gptel-confirm-tool-calls (or (eq gptel-confirm-tool-calls t)
                                                     (gptel-tool-confirm tool-spec)))
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
          (setq gptel--fsm-last fsm)
          (when gptel-mode (gptel--update-status
                            (format " Run tools?" ) 'mode-line-emphasis))
          (funcall (plist-get info :callback)
                   (cons 'tool-call pending-calls) info))))))

;;;; State machine predicates
;; Predicates used to find the next state to transition to, see
;; `gptel-request--transitions'.

(defun gptel--error-p (info) (plist-get info :error))

(defun gptel--tool-use-p (info) (plist-get info :tool-use))

(defun gptel--tool-result-p (info) (plist-get info :tool-success))

;; TODO(prompt-list): Document new prompt input format to `gptel-request'.

;;; Send queries, handle responses
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
         (info (list :data prompt-buffer
                     :buffer buffer
                     :position start-marker)))
    (when transforms (plist-put info :transforms transforms))
    (with-current-buffer prompt-buffer (setq gptel--system-message system))
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
          (with-current-buffer (plist-get info :buffer) ;Apply prompt transformations
            (gptel--update-status " Augmenting..." 'mode-line-emphasis))
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
                          ;; HACK(tool): no stream if Ollama + tools.  Need to find a better way
                          (not (and (eq (type-of gptel-backend) 'gptel-ollama)
                                    gptel-tools gptel-use-tools))
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
        (when (and gptel-context--alist gptel-use-context
                   gptel-track-media (gptel--model-capable-p 'media))
          (gptel--wrap-user-prompt gptel-backend full-prompt 'media))
        (unless stream (cl-remf info :stream))
        (plist-put info :backend gptel-backend)
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
    (gptel--sanitize-model)
    (let ((fsm (gptel-make-fsm :handlers gptel-send--handlers)))
      (gptel-request nil
        :stream gptel-stream
        :transforms gptel-prompt-transform-functions
        :fsm fsm)
      (message "Querying %s..."
               (thread-first (gptel-fsm-info fsm)
                             (plist-get :backend)
                             (or gptel-backend)
                             (gptel-backend-name))))
    (gptel--update-status " Waiting..." 'warning)))

(declare-function json-pretty-print-buffer "json")
(defun gptel--inspect-query (&optional request-fsm format)
  "Show the full LLM query that will be sent in a buffer.

This functions as a dry run of `gptel-send'.  The request data
may be edited and the query continued from this buffer.

REQUEST-FSM is the state of the request, as returned by
`gptel-request'.  If FORMAT is the symbol json, show the encoded
JSON query instead of the Lisp structure gptel uses."
  (unless request-fsm (setq request-fsm gptel--fsm-last))
  (if (bufferp (plist-get (gptel-fsm-info request-fsm) :data))
      (letrec ((dry-run-poll
                (run-with-timer
                 0 1 (lambda (fsm form)
                       (unless (bufferp (plist-get (gptel-fsm-info fsm) :data))
                         (cancel-timer dry-run-poll)
                         (gptel--inspect-query fsm form)))
                 request-fsm format))))
    (with-current-buffer (plist-get (gptel-fsm-info request-fsm) :buffer)
      (gptel--update-status " Ready" 'success))
    (with-current-buffer (get-buffer-create "*gptel-query*")
      (let* ((standard-output (current-buffer))
             (inhibit-read-only t)
             (request-data
              (plist-get (gptel-fsm-info request-fsm) :data)))
        (buffer-disable-undo)
        (erase-buffer)
        (if (eq format 'json)
            (progn (fundamental-mode)
                   (insert (gptel--json-encode request-data))
                   (json-pretty-print-buffer))
          (lisp-data-mode)
          (prin1 request-data)
          (pp-buffer))
        (setq-local gptel--fsm-last request-fsm)
        (goto-char (point-min))
        (view-mode 1)
        (setq buffer-undo-list nil)
        (use-local-map
         (make-composed-keymap
          (define-keymap
            "C-c C-c" #'gptel--continue-query
            "C-c C-w" (lambda () "Copy Curl command for query."
                        (interactive) (gptel--continue-query 'copy))
            "C-c C-k" #'quit-window)
          (current-local-map)))
        (unless header-line-format
          (setq header-line-format
                (substitute-command-keys
                 (concat
                  "Edit request: \\[read-only-mode],"
                  " Send request: \\[gptel--continue-query],"
                  (format " Copy Curl: %s"
                          (propertize "C-c C-w" 'face 'help-key-binding))
                  " Quit: \\[quit-window]"))))
        (display-buffer (current-buffer) gptel-display-buffer-action)))))

(declare-function gptel-curl--get-args "gptel-curl")

(defun gptel--continue-query (&optional copy)
  "Continue sending the gptel query displayed in this buffer.

The request is continued with the same parameters as originally
specified.

With prefix arg COPY, copy the Curl command for the request to the
kill ring instead."
  (interactive "P" lisp-data-mode fundamental-mode)
  (unless (equal (buffer-name) "*gptel-query*")
    (user-error "This command is meant for use in a gptel dry-run buffer"))
  (save-excursion
    (goto-char (point-min))
    (condition-case-unless-debug nil
        (when-let* ((data (if (eq major-mode 'lisp-data-mode)
                              (read (current-buffer))
                            (gptel--json-read))))
          (cl-assert (cl-typep gptel--fsm-last 'gptel-fsm))
          (plist-put (gptel-fsm-info gptel--fsm-last) :data data)
          (if copy                 ;Copy Curl command instead of sending request
              (let ((args (and (require 'gptel-curl)
                               (gptel-curl--get-args (gptel-fsm-info gptel--fsm-last)
                                                     (md5 (format "%s" (random)))))))
                (kill-new
                 (mapconcat #'shell-quote-argument
                            (cons (gptel--curl-path) args) " \\\n"))
                (message "Curl command for request copied to kill-ring"))
            (gptel--fsm-transition gptel--fsm-last) ;INIT -> WAIT
            (quit-window)))
      (error
       (user-error "Can not resume request: could not read data from buffer!")))))

(defun gptel--insert-response (response info &optional raw)
  "Insert the LLM RESPONSE into the gptel buffer.

INFO is a plist containing information relevant to this buffer.
See `gptel--url-get-response' for details.

Optional RAW disables text properties and transformation."
  (let* ((gptel-buffer (plist-get info :buffer))
         (start-marker (plist-get info :position))
         (tracking-marker (plist-get info :tracking-marker)))
    (pcase response
      ((pred stringp)                ;Response text
       (with-current-buffer gptel-buffer
         (when tracking-marker           ;separate from previous response
           (setq response (concat gptel-response-separator response)))
         (save-excursion
           (with-current-buffer (marker-buffer start-marker)
             (goto-char (or tracking-marker start-marker))
             ;; (run-hooks 'gptel-pre-response-hook)
             (unless (or (bobp) (plist-get info :in-place)
                         tracking-marker)
               (insert gptel-response-separator)
               (when gptel-mode
                 (insert (gptel-response-prefix-string)))
               (move-marker start-marker (point)))
             (unless raw
               (when-let* ((transformer (plist-get info :transformer)))
                 (setq response (funcall transformer response)))
               (add-text-properties
                0 (length response) '(gptel response front-sticky (gptel)) response))
             (insert response)
             (plist-put info :tracking-marker (setq tracking-marker (point-marker)))
             ;; for uniformity with streaming responses
             (set-marker-insertion-type tracking-marker t)))))
      (`(reasoning . ,text)
       (when-let* ((include (plist-get info :include-reasoning)))
         (if (stringp include)
             (with-current-buffer (get-buffer-create
                                   (plist-get info :include-reasoning))
               (save-excursion (goto-char (point-max)) (insert text)))
           (with-current-buffer (marker-buffer start-marker)
             (let ((separator         ;Separate from response prefix if required
                    (and (not tracking-marker) gptel-mode
                         (not (string-suffix-p "\n" (gptel-response-prefix-string)))
                         "\n"))
                   (blocks (if (derived-mode-p 'org-mode)
                               `("#+begin_reasoning\n" . ,(concat "\n#+end_reasoning"
                                                           gptel-response-separator))
                             ;; TODO(reasoning) remove properties and strip instead
                             (cons (propertize "``` reasoning\n" 'gptel 'ignore)
                                   (concat (propertize "\n```" 'gptel 'ignore)
                                           gptel-response-separator)))))
               (if (eq include 'ignore)
                   (progn
                     (add-text-properties
                      0 (length text) '(gptel ignore front-sticky (gptel)) text)
                     (gptel--insert-response
                      (concat (car blocks) text (cdr blocks)) info t))
                 (gptel--insert-response (concat separator (car blocks)) info t)
                 (gptel--insert-response text info)
                 (gptel--insert-response (cdr blocks) info t))
               (when (derived-mode-p 'org-mode) ;fold block
                 (save-excursion
                   (goto-char (plist-get info :tracking-marker))
                   (search-backward "#+end_reasoning" start-marker t)
                   (when (looking-at "^#\\+end_reasoning")
                     (org-cycle)))))))))
      (`(tool-call . ,tool-calls)
       (gptel--display-tool-calls tool-calls info))
      (`(tool-result . ,tool-results)
       (gptel--display-tool-results tool-results info)))))

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
  (if (stringp (car prompts))         ; Simple format, list of strings
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
  (let ((parts) (from-pt) (mime))
    (save-excursion
      (setq from-pt (goto-char beg))
      (while (re-search-forward
              (concat "\\(?:" markdown-regex-link-inline "\\|"
                      markdown-regex-angle-uri "\\)")
              end t)
        (setq mime nil)
        (when-let* ((link-at-pt (markdown-link-at-pos (point)))
                    ((gptel--link-standalone-p
                      (car link-at-pt) (cadr link-at-pt)))
                    (path (nth 3 link-at-pt))
                    (path (string-remove-prefix "file://" path)))
          (cond
           ((seq-some (lambda (p) (string-prefix-p p path))
                      '("https:" "http:" "ftp:"))
            ;; Collect text up to this image, and collect this image url
            (when (gptel--model-capable-p 'url) ; FIXME This is not a good place
                                        ; to check for url capability!
              (let ((text (buffer-substring-no-properties from-pt (car link-at-pt))))
                (unless (string-blank-p text) (push (list :text text) parts))
                (push (list :url path :mime mime) parts)
                (setq from-pt (cadr link-at-pt)))))
           ((file-readable-p path)
            (if (or (not (gptel--file-binary-p path))
                    (and (setq mime (mailcap-file-name-to-mime-type path))
                         (gptel--model-mime-capable-p mime)))
                ;; Collect text up to this image, and collect this image
                (let ((text (buffer-substring-no-properties from-pt (car link-at-pt))))
                  (unless (string-blank-p text) (push (list :text text) parts))
                  (push (if mime (list :media path :mime mime) (list :textfile path)) parts)
                  (setq from-pt (cadr link-at-pt)))
              (message "Ignoring unsupported binary file \"%s\"." path)))))))
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
         (url-request-extra-headers
          (append '(("Content-Type" . "application/json"))
                  (when-let* ((header (gptel-backend-header gptel-backend)))
                    (if (functionp header)
                        (funcall header) header))))
         (info (gptel-fsm-info fsm))
         (backend (plist-get info :backend))
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
                               (with-current-buffer (plist-get info :buffer)
                                 (funcall backend-url))
                             backend-url))
                         (lambda (_)
                           (set-buffer-multibyte t)
                           (set-buffer-file-coding-system 'utf-8-unix)
                           (pcase-let ((`(,response ,http-status ,http-msg ,error)
                                        (gptel--url-parse-response backend info))
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


;;; Reasoning content UI
(declare-function gptel-curl--stream-insert-response "gptel-curl")

(defun gptel--display-reasoning-stream (text info)
  "Show reasoning TEXT in an appropriate location.

INFO is the request INFO, see `gptel--url-get-response'.  This is
for streaming responses only."
  (when-let* ((include (plist-get info :include-reasoning)))
    (if (stringp include)
        (unless (eq text t)
          (with-current-buffer (get-buffer-create include)
            (save-excursion (goto-char (point-max))
                            (insert text))))
      (let* ((reasoning-marker (plist-get info :reasoning-marker))
             (tracking-marker (plist-get info :tracking-marker))
             (start-marker (plist-get info :position)))
        (with-current-buffer (marker-buffer start-marker)
          (if (eq text t)               ;end of stream
              (progn
                (gptel-curl--stream-insert-response
                 (concat (if (derived-mode-p 'org-mode)
                             "\n#+end_reasoning"
                           ;; TODO(reasoning) remove properties and strip instead
                           (propertize "\n```" 'gptel 'ignore))
                         gptel-response-separator)
                 info t)
                (when (derived-mode-p 'org-mode) ;fold block
                  (ignore-errors
                    (save-excursion
                      (goto-char tracking-marker)
                      (search-backward "#+end_reasoning" start-marker t)
                      (when (looking-at "^#\\+end_reasoning")
                        (org-cycle))))))
            (unless (and reasoning-marker tracking-marker
                         (= reasoning-marker tracking-marker))
              (let ((separator        ;Separate from response prefix if required
                     (and (not tracking-marker) gptel-mode
                          (not (string-suffix-p
                                "\n" (gptel-response-prefix-string)))
                          "\n")))
                (gptel-curl--stream-insert-response
                 (concat separator
                         (if (derived-mode-p 'org-mode)
                             "#+begin_reasoning\n"
                           ;; TODO(reasoning) remove properties and strip instead
                           (propertize "``` reasoning\n" 'gptel 'ignore)))
                 info t)))
            (if (eq include 'ignore)
                (progn
                  (add-text-properties
                   0 (length text) '(gptel ignore front-sticky (gptel)) text)
                  (gptel-curl--stream-insert-response text info t))
              (gptel-curl--stream-insert-response text info)))
          (setq tracking-marker (plist-get info :tracking-marker))
          (if reasoning-marker
              (move-marker reasoning-marker tracking-marker)
            (plist-put info :reasoning-marker
                       (copy-marker tracking-marker nil))))))))


;;; Tool use UI
(defun gptel--display-tool-calls (tool-calls info &optional use-minibuffer)
  "Handle tool call confirmation.

TOOL-CALLS should be a list of tool call specifications or results,
structured as:

 ((tool args callback) ...)

for tool call specifications to be confirmed.  INFO contains the
state of the request.  To prompt for tool call confirmation, use
either an overlay in the request buffer or the minibuffer (if
USE-MINIBUFFER is non-nil)."
  (let* ((start-marker (plist-get info :position))
         (tracking-marker (plist-get info :tracking-marker)))
    ;; pending tool calls look like ((tool callback args) ...)
    (with-current-buffer (plist-get info :buffer)
      (if use-minibuffer            ;prompt for confirmation from the minibuffer
          (let* ((minibuffer-allow-text-properties t)
                 (backend-name (gptel-backend-name (plist-get info :backend)))
                 (prompt (format "%s wants to run " backend-name)))
            (map-y-or-n-p
             (lambda (tool-call-spec)
               (concat prompt (propertize (gptel-tool-name (car tool-call-spec))
                                          'face 'font-lock-keyword-face)
                       ": "))
             (lambda (tcs) (gptel--accept-tool-calls (list tcs) nil))
             tool-calls '("tool call" "tool calls" "run")
             `((?i ,(lambda (_) (save-window-excursion
                             (with-selected-window
                                 (gptel--inspect-fsm gptel--fsm-last)
                               (goto-char (point-min))
                               (when (search-forward-regexp "^:tool-use" nil t)
                                 (forward-line 0) (hl-line-highlight))
                               (use-local-map
                                (make-composed-keymap
                                 (define-keymap "q" (lambda () (interactive)
                                                      (quit-window)
                                                      (exit-recursive-edit)))
                                 (current-local-map)))
                               (recursive-edit) nil)))
                   "inspect call(s)"))))
        ;; Prompt for confirmation from the chat buffer
        (let* ((backend-name (gptel-backend-name (plist-get info :backend)))
               (actions-string
                (concat (propertize "Run tools: " 'face 'font-lock-string-face)
                        (propertize "C-c C-c" 'face 'help-key-binding)
                        (propertize ", Cancel request: " 'face 'font-lock-string-face)
                        (propertize "C-c C-k" 'face 'help-key-binding)
                        (propertize ", Inspect: " 'face 'font-lock-string-face)
                        (propertize "C-c C-i" 'face 'help-key-binding)))
               (confirm-strings
                (list (concat "\n" actions-string
                              (propertize "\n" 'face '(:inherit font-lock-string-face
                                                                :underline t :extend t))
                              (format (propertize "\n%s wants to run:\n"
                                                  'face 'font-lock-string-face)
                                      backend-name))))
               ;; FIXME(tool) use a wrapper instead of a manual text-property search,
               ;; this is fragile
               (ov-start (save-excursion
                           (goto-char start-marker)
                           (text-property-search-backward 'gptel 'response)
                           (point)))
               (ov (or (cdr-safe (get-char-property-and-overlay
                                  start-marker 'gptel-tool))
                       (make-overlay ov-start (or tracking-marker start-marker)))))
          ;; If the cursor is at the overlay-end, it ends up outside, so move it back
          (unless tracking-marker
            (when (= (point) start-marker) (ignore-errors (backward-char))))
          (pcase-dolist (`(,tool-spec ,arg-values _) tool-calls)
            (push (gptel--format-tool-call (gptel-tool-name tool-spec) arg-values)
                  confirm-strings))
          (push (concat (propertize "\n" 'face '(:inherit font-lock-string-face
                                                          :underline t :extend t)))
                confirm-strings)
          ;; Add confirmation prompt to the overlay
          (overlay-put ov 'after-string
                       (apply #'concat (nreverse confirm-strings)))
          (overlay-put ov 'mouse-face 'highlight)
          (overlay-put ov 'gptel-tool tool-calls)
          (overlay-put ov 'help-echo
                       (concat "Tool call(s) requested: " actions-string))
          (overlay-put ov 'keymap
                       (define-keymap
                         "<mouse-1>" #'gptel--dispatch-tool-calls
                         "C-c C-c" #'gptel--accept-tool-calls
                         "C-c C-k" #'gptel--reject-tool-calls
                         "C-c C-i"
                         (lambda () (interactive)
                           (with-selected-window
                               (gptel--inspect-fsm gptel--fsm-last)
                             (goto-char (point-min))
                             (when (search-forward-regexp "^:tool-use" nil t)
                               (forward-line 0)
                               (hl-line-highlight)))))))))))

(defun gptel--display-tool-results (tool-results info)
  "Insert TOOL-RESULTS into buffer.

TOOL-RESULTS is

 ((tool args result) ...)

for tool call results.  INFO contains the state of the request."
  (let* ((start-marker (plist-get info :position))
         (tool-marker (plist-get info :tool-marker))
         (tracking-marker (plist-get info :tracking-marker)))
    ;; Insert tool results
    (when gptel-include-tool-results
      (with-current-buffer (marker-buffer start-marker)
        (cl-loop
         for (tool args result) in tool-results
         with include-names =
         (mapcar #'gptel-tool-name
                 (cl-remove-if-not #'gptel-tool-include (plist-get info :tools)))
         if (or (eq gptel-include-tool-results t)
                (member (gptel-tool-name tool) include-names))
         do (funcall
             (plist-get info :callback)
             (let* ((name (gptel-tool-name tool))
                    (separator        ;Separate from response prefix if required
                     (cond ((not tracking-marker)
                            (and gptel-mode
                                 (not (string-suffix-p
                                       "\n" (gptel-response-prefix-string)))
                                 "\n"))           ;start of response
                           ((not (and tool-marker ;not consecutive tool result blocks
                                      (= tracking-marker tool-marker)))
                            gptel-response-separator)))
                    (tool-use
                     ;; TODO(tool) also check args since there may be more than
                     ;; one call/result for the same tool
                     (cl-find-if
                      (lambda (tu) (equal (plist-get tu :name) name))
                      (plist-get info :tool-use)))
                    (id (plist-get tool-use :id))
                    (display-call (format "(%s %s)" name
                                          (string-trim (prin1-to-string args) "(" ")")))
                    (call (prin1-to-string `(:name ,name :args ,args)))
                    (truncated-call (truncate-string-to-width
                                     display-call
                                     (floor (* (window-width) 0.6)) 0 nil " ...)")))
               (if (derived-mode-p 'org-mode)
                   (concat
                    separator
                    "#+begin_tool "
                    truncated-call
                    (propertize
                     (concat "\n" call "\n\n" (org-escape-code-in-string result))
                     'gptel `(tool . ,id))
                    "\n#+end_tool\n")
                 ;; TODO(tool) else branch is handling all front-ends as markdown.
                 ;; At least escape markdown.
                 (concat
                  separator
                  ;; TODO(tool) remove properties and strip instead of ignoring
                  (propertize (format "``` tool %s" truncated-call) 'gptel 'ignore)
                  (propertize
                   ;; TODO(tool) escape markdown in result
                   (concat "\n" call "\n\n" result)
                   'gptel `(tool . ,id))
                  ;; TODO(tool) remove properties and strip instead of ignoring
                  (propertize "\n```\n" 'gptel 'ignore))))
             info
             'raw)
         ;; tool-result insertion has updated the tracking marker
         (unless tracking-marker
           (setq tracking-marker (plist-get info :tracking-marker)))
         (if tool-marker
               (move-marker tool-marker tracking-marker)
             (setq tool-marker (copy-marker tracking-marker nil))
             (plist-put info :tool-marker tool-marker))
         (when (derived-mode-p 'org-mode) ;fold drawer
           (ignore-errors
             (save-excursion
               (goto-char tracking-marker)
               (forward-line -1)
               (when (looking-at "^#\\+end_tool")
                 (org-cycle))))))))))

(defun gptel--format-tool-call (name arg-values)
  "Format a tool call for display in the buffer.

NAME and ARG-VALUES are the name and arguments for the call."
  (format "(%s %s)\n"
          (propertize name 'face 'font-lock-keyword-face)
          (propertize
           (mapconcat (lambda (arg)
                        (cond ((stringp arg)
                               (prin1-to-string
                                (replace-regexp-in-string
                                 "\n" "⮐" (truncate-string-to-width
                                           arg (floor (window-width) 2)
                                           nil nil t))))
                              (t (prin1-to-string arg))))
                      arg-values " ")
           'face 'font-lock-constant-face)))

(defun gptel--accept-tool-calls (&optional response ov)
  (interactive (pcase-let ((`(,resp . ,o) (get-char-property-and-overlay
                                           (point) 'gptel-tool)))
                 (list resp o)))
  (gptel--update-status " Calling tool..." 'mode-line-emphasis)
  (message "Continuing query...")
  (cl-loop for (tool-spec arg-values process-tool-result) in response
           do
           (if (gptel-tool-async tool-spec)
               (apply (gptel-tool-function tool-spec)
                      process-tool-result arg-values)
             (let ((result
                    (condition-case errdata
                        (apply (gptel-tool-function tool-spec) arg-values)
                      (error (mapconcat #'gptel--to-string errdata " ")))))
               (funcall process-tool-result result))))
  (and (overlayp ov) (delete-overlay ov)))

(defun gptel--reject-tool-calls (&optional _response ov)
  (interactive (pcase-let ((`(,resp . ,o) (get-char-property-and-overlay
                                           (point) 'gptel-tool)))
                 (list resp o)))
  (gptel--update-status " Tools cancelled" 'error)
  (message (substitute-command-keys
            "Tool calls canceled.  \\[gptel-menu] to continue them!"))
  (and (overlayp ov) (delete-overlay ov)))

(defun gptel--dispatch-tool-calls (choice)
  (interactive
   (list
    (let ((choices '((?y "yes") (?n "do nothing")
                     (?k "cancel request") (?i "inspect call(s)"))))
      (read-multiple-choice "Run tool calls? " choices))))
  (pcase (car choice)
    (?y (call-interactively #'gptel--accept-tool-calls))
    (?k (call-interactively #'gptel--reject-tool-calls))
    (?i (gptel--inspect-fsm gptel--fsm-last))))


;;; Presets
;;;; Presets implementation
(defvar gptel--known-presets nil
  "Alist of presets for gptel.

Each entry maps a preset name (a symbol) to a plist of
specifications (see `gptel-make-preset').")

(defun gptel-make-preset (name &rest keys)
  "Define a gptel preset with NAME.

A preset is a combination of gptel options intended to be applied and
used together.  Presets can make it less tedious to change gptel
settings on the fly.

Typically this will include a model, backend, system message and perhaps
some tools, but any set of gptel options can be set this way.

NAME must be a symbol.  KEYS is a plist corresponding to the options
being set.  All KEYS are optional.

Recognized keys:

DESCRIPTION is a description of the preset, used when selecting a
preset.

PARENTS is a preset name (or list of preset names) to apply before this
one.

PRE and POST are functions to run before and after the preset is
applied.  They take no arguments.

BACKEND is the gptel-backend to set, or its name (like \"ChatGPT\").

MODEL is the gptel-model.

SYSTEM is the directive. It can be
- the system message (a string),
- a list of strings (a conversation template)
- or a function (dynamic system message).
- It can also be a symbol naming a directive in `gptel-directives'.

TOOLS is a list of gptel tools or tool names, like
\\='(\"read_url\" \"read_buffer\" ...)

Recognized keys are not limited to the above.  Any other key (like
`:foo') corresponds to the value of either `gptel-foo' (preferred) or
`gptel--foo'.
- So TOOLS corresponds to `gptel-tools',
- CONFIRM-TOOL-CALLS to `gptel-confirm-tool-calls',
- TEMPERATURE to `gptel-temperature' and so on.
See gptel's customization options for all available settings."
  (declare (indent 1))
  (if-let* ((p (assoc name gptel--known-presets)))
      (setcdr p keys)
    (setq gptel--known-presets          ;Add at end of presets for menu ordering
          (nconc gptel--known-presets (list (cons name keys))))))

(defun gptel-get-preset (name)
  "Get the gptel preset spec with NAME."
  (alist-get name gptel--known-presets nil nil #'equal))

(defun gptel--save-preset (name &optional description)
  "Save gptel's current settings as a preset with NAME.

NAME must be a symbol.  DESCRIPTION is added if provided.  In addition
to registering the preset, elisp code to do the same is copied to the
kill-ring."
  (interactive
   (list (intern (completing-read "Save gptel settings to (existing or new) preset: "
                                  gptel--known-presets))
         (read-string "Description (optional): ")))
  (let ((preset-code
         `(gptel-make-preset ',name
           :description ,(when (and description
                                (not (string-blank-p description)))
                          description)
           :backend ,(gptel-backend-name gptel-backend)
           :model ',gptel-model
           :system ,(if-let* ((directive (car-safe (rassoc gptel--system-message
                                                    gptel-directives))))
                         `',directive
                      gptel--system-message)
           :tools ',(mapcar #'gptel-tool-name gptel-tools)
           :stream ,gptel-stream
           :temperature ,gptel-temperature
           :max-tokens ,gptel-max-tokens
           :use-context ',gptel-use-context
           :track-media ,gptel-track-media
           :include-reasoning ,(let ((reasoning gptel-include-reasoning))
                                   (if (eq reasoning 'ignore)
                                       ''ignore reasoning)))))
    (kill-new (pp-to-string preset-code))
    (eval preset-code)
    (message "Preset %s saved. (Lisp expression for preset saved to kill-ring)"
             (propertize (symbol-name name) 'face 'highlight))))

(defvar gptel--rewrite-directive)
(defun gptel--apply-preset (preset &optional setter)
  "Apply gptel PRESET with SETTER.

PRESET is the name of a preset, or a spec (plist) of the form
 (:KEY1 VAL1 :KEY2 VAL2 ...).

SETTER is the function used to set the gptel options.  It must accept
two arguments, the symbol being set and the value to set it to.  It
defaults to `set', and can be set to a different function to (for
example) apply the preset buffer-locally."
  (when (memq (type-of preset) '(string symbol))
    (let ((spec (or (gptel-get-preset preset)
                    (user-error "gptel preset \"%s\": Cannot find preset."
                                preset))))
      (setq preset spec)))
  (unless setter (setq setter #'set))
  (when-let* ((func (plist-get preset :pre))) (funcall func))
  (when-let* ((parents (plist-get preset :parents)))
    (mapc #'gptel--apply-preset (ensure-list parents)))
  (map-do
   (lambda (key val)
     (pcase key
       ((or :parents :description :pre :post) nil)
       ((or :system :system-message :rewrite-directive)
        (let ((sym (if (eq key :rewrite-directive)
                       'gptel--rewrite-directive 'gptel--system-message)))
          (if (and (symbolp val) (not (functionp val)))
              (if-let* ((directive (alist-get val gptel-directives)))
                  (funcall setter sym directive)
                (user-error "gptel preset: Cannot find directive %s" val))
            (funcall setter sym val))))
       (:backend
        (setq val (cl-etypecase val
                    (gptel-backend val)
                    (string (gptel-get-backend val))))
        (unless val
          (user-error "gptel preset: Cannot find backend %s." val))
        (funcall setter 'gptel-backend val))
       (:tools                          ;TEMP Confirm this `:append' convention
        (let* ((append (when (eq (car-safe val) :append) (setq val (cdr val)) t))
               (tools
                (flatten-list
                 (cl-loop for tool-name in (ensure-list val)
                          for tool = (cl-etypecase tool-name
                                       (gptel-tool tool-name)
                                       (string (ignore-errors
                                                 (gptel-get-tool tool-name))))
                          do (unless tool
                               (user-error "gptel preset: Cannot find tool %s." val))
                          collect tool))))
          (funcall setter 'gptel-tools ;append makes a copy of gptel-tools, intentional
                   (if append (delete-dups (append gptel-tools tools)) tools))))
       ((and (let sym (or (intern-soft
                           (concat "gptel-" (substring (symbol-name key) 1)))
                          (intern-soft
                           (concat "gptel--" (substring (symbol-name key) 1)))))
             (guard (and sym (boundp sym))))
        (funcall setter sym val))
       (_ (display-warning
           '(gptel presets)
           (format "gptel preset: setting for %s not found, ignoring." key)))))
   preset)
  (when-let* ((func (plist-get preset :post))) (funcall func)))

(defun gptel--preset-syms (preset)
  "Return a list of gptel variables (symbols) set by PRESET.

PRESET is the name of a preset, or a spec (plist) of the form
 (:KEY1 VAL1 :KEY2 VAL2 ...)."
  (when (memq (type-of preset) '(string symbol))
    (let ((spec (or (gptel-get-preset preset)
                    (user-error "gptel preset \"%s\": Cannot find preset."
                                preset))))
      (setq preset spec)))
  (let* ((index preset)
         syms key val)
    (while index
      (setq key (pop index) val (pop index))
      (pcase key
        ((or :description :pre :post))
        (:parents
         (mapc (lambda (parent-preset)
                 (nconc syms (gptel--preset-syms
                              (gptel-get-preset parent-preset))))
               (ensure-list val)))
        (:system (push 'gptel--system-message syms))
        (_ (if-let* ((var (or (intern-soft
                               (concat "gptel-" (substring (symbol-name key) 1)))
                              (intern-soft
                               (concat "gptel--" (substring (symbol-name key) 1))))))
               (push var syms)
             (display-warning
              '(gptel presets)
              (format "gptel preset \"%s\": setting for %s not found, ignoring."
                      (car preset) key))))))
    (cl-delete-duplicates syms)))

;; This is identical to `cl-progv', only we let-bind symbols SYM from the preset
;; to their current values instead of evaluating the values explicitly. (#1005)
(defmacro gptel-with-preset (name &rest body)
  "Run BODY with gptel preset NAME applied.

This macro can be used to create `gptel-request' command with settings
from a gptel preset applied.

NAME is the name of a preset, or a spec (plist) of the form
 (:KEY1 VAL1 :KEY2 VAL2 ...).  It must be quoted."
  (declare (indent 1))
  (let ((syms (make-symbol "syms"))
        (binds (make-symbol "binds"))
        (bodyfun (make-symbol "body")))
    `(let* ((,syms (gptel--preset-syms ,name))
            (,bodyfun (lambda () (gptel--apply-preset ,name) ,@body))
            (,binds nil))
       (while ,syms (push (list (car ,syms) (pop ,syms)) ,binds))
       (eval (list 'let (nreverse ,binds) (list 'funcall (list 'quote ,bodyfun)))))))

;;;; Presets in-buffer UI
(defun gptel--transform-apply-preset (_fsm)
  "Apply a gptel preset to the buffer depending on the prompt.

If the last user prompt includes @foo, the preset foo is applied.
Before applying the preset, \"@foo\" is removed from the prompt and
point is placed at its position."
  (when gptel--known-presets
    (text-property-search-backward 'gptel nil t)
    (while (re-search-forward "@\\([^[:blank:]]+\\)\\_>" nil t)
      ;; The following convoluted check is because re-search is much faster if
      ;; the search pattern begins with a non-whitespace char.
      (when (or (= (match-beginning 0) (point-min))
                (memq (char-syntax (char-before (match-beginning 0))) '(32 62)))
        (when-let* ((name (match-string 1))
                    (preset (or (gptel-get-preset (intern-soft name))
                                (gptel-get-preset name))))
          (delete-region (match-beginning 0) (match-end 0))
          ;; Point must be after @foo when the preset is applied to allow for
          ;; more advanced transformations.
          (gptel--apply-preset preset
                               (lambda (sym val)
                                 (set (make-local-variable sym) val))))))))

;; ;; Alternative approach with string search
;; (search-forward "@" nil t)
;; (if (and (memq (char-syntax (char-before (1- (point)))) '(32 62))
;;          (looking-at "\\([^[:blank:]]+?\\)[[:punct:]]?\\s-+"))
;;     do-stuff)

(defun gptel--fontify-preset-keyword (end)
  "Font-lock function for preset indicators in chat buffers.

Return preset fontification info for text up to END."
  (and (re-search-forward "@\\([^[:blank:]]+\\)\\_>" end t)
       (or (= (match-beginning 0) (point-min))
           (memq (char-syntax (char-before (match-beginning 0))) '(32 62)))
       (not (plist-get (text-properties-at (match-beginning 1)) 'gptel))))

(defun gptel-preset-capf ()
  "Completion at point for gptel presets in `gptel-mode'.

Add this to `completion-at-point-functions'."
  (and gptel--known-presets
       (save-excursion
         (let ((num (- (skip-syntax-backward "w_"))))
           (when (eql (char-before) ?@)
             (list (point) (+ (point) num)
                   gptel--known-presets
                   :exclusive 'no
                   :annotation-function
                   #'(lambda (c) (thread-first
                              (intern-soft c)
                              (assq gptel--known-presets) (cdr)
                              (plist-get :description)))))))))

(defun gptel--prettify-preset ()
  "Get visual and completion help with presets in gptel buffers.

Intended to be added to `gptel-mode-hook'."
  (let ((keyword '((gptel--fontify-preset-keyword
                    ;; subexp 0 here is not required, we retain it to make it
                    ;; easy to swtich to more complex patterns in the future
                    0 (when-let* ((comps (all-completions (match-string 1)
                                          gptel--known-presets))
                                  ((member (match-string 1) comps)))
                       '(:box -1 :inherit secondary-selection))
                    prepend))))
    (cond
     (gptel-mode
      (font-lock-add-keywords nil keyword t)
      (add-hook 'completion-at-point-functions #'gptel-preset-capf nil t))
     (t (font-lock-remove-keywords nil keyword)
        (remove-hook 'completion-at-point-functions #'gptel-preset-capf t)))))


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
                  (add-text-properties
                   b e `(gptel-history
                         ,(append (ensure-list history)
                           (get-char-property (1- e) 'gptel-history))
                         front-sticky (gptel gptel-history))))
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
    (pcase-let* ((`(,beg . ,end) (funcall (or bounds-func #'gptel--get-response-bounds)))
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
  (pcase-let ((`(,beg . ,end) (gptel--get-response-bounds)))
    (goto-char beg) (push-mark) (goto-char end) (activate-mark)))

(defun gptel--previous-variant (&optional arg)
  "Switch to previous gptel-response at this point, if it exists."
  (interactive "p")
  (pcase-let* ((`(,beg . ,end) (gptel--get-response-bounds))
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
