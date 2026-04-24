;;; gptel-mistral.el --- Mistral AI backend for gptel -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025  Karthik Chikmagalur

;; Authors: Karthik Chikmagalur <karthikchikmagalur@gmail.com> and pirminj

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

;; This file adds a dedicated Mistral AI backend for gptel.  Mistral's
;; Chat Completions API (at api.mistral.ai) is OpenAI-compatible, so
;; this backend inherits from `gptel-openai' without custom parsing
;; methods.  It exposes Mistral's hosted model lineup (mistral-*,
;; magistral-*, codestral-*, devstral-*, ministral-*, pixtral-*) with
;; metadata for context window, pricing, and capabilities.

;;; Code:
(require 'cl-generic)
(eval-when-compile (require 'cl-lib))
(require 'map)
(eval-and-compile
  (require 'gptel-request)
  (require 'gptel-openai))

;;; Mistral
(cl-defstruct (gptel-mistral (:include gptel-openai)
                             (:copier nil)
                             (:constructor gptel--make-mistral)))

(defconst gptel--mistral-models
  '((mistral-large-2512
     :description "Flagship large model for complex reasoning and generation"
     :capabilities (tool-use json)
     :context-window 256
     :input-cost 0.5
     :output-cost 1.5)
    (mistral-large-latest
     :description "Flagship large model for training"
     :capabilities (tool-use json)
     :context-window 128
     :input-cost 2
     :output-cost 6)
    (mistral-medium-latest
     :description "Frontier-class multimodal model, balanced performance and cost"
     :capabilities (tool-use json media)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 128
     :input-cost 0.4
     :output-cost 2)
    (mistral-small-latest
     :description "Efficient hybrid instruct/reasoning/code model"
     :capabilities (tool-use json media)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 128
     :input-cost 0.1
     :output-cost 0.3)
    (magistral-medium-latest
     :description "Frontier-class multimodal reasoning model"
     :capabilities (tool-use json reasoning media)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 128
     :input-cost 2
     :output-cost 5)
    (magistral-small-latest
     :description "Small multimodal reasoning model"
     :capabilities (tool-use json reasoning media)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 128
     :input-cost 0.5
     :output-cost 1.5)
    (codestral-latest
     :description "Cutting-edge code completion and generation model"
     :capabilities (tool-use json)
     :context-window 256
     :input-cost 0.3
     :output-cost 0.9)
    (devstral-medium-latest
     :description "Frontier code agent model for software engineering tasks"
     :capabilities (tool-use json)
     :context-window 128
     :input-cost 0.4
     :output-cost 2)
    (devstral-small-latest
     :description "Small code agent model for software engineering tasks"
     :capabilities (tool-use json)
     :context-window 128
     :input-cost 0.1
     :output-cost 0.3)
    (ministral-8b-latest
     :description "Lightweight model with vision capabilities"
     :capabilities (tool-use json media)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 128
     :input-cost 0.1
     :output-cost 0.1)
    (ministral-3b-latest
     :description "Ultra-compact model with vision capabilities"
     :capabilities (tool-use json media)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 128
     :input-cost 0.04
     :output-cost 0.04)
    (pixtral-large-latest
     :description "Large vision-capable model"
     :capabilities (tool-use json media)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 128
     :input-cost 2
     :output-cost 6))
  "List of available Mistral AI models and associated properties.")

;;;###autoload
(cl-defun gptel-make-mistral
    (name &key curl-args stream key request-params
          (header (lambda () (when-let* ((key (gptel--get-api-key)))
                          `(("Authorization" . ,(concat "Bearer " key))))))
          (host "api.mistral.ai")
          (protocol "https")
          (endpoint "/v1/chat/completions")
          (models gptel--mistral-models))
  "Register a Mistral AI backend for gptel with NAME.

For the meanings of the keyword arguments, see `gptel-make-openai'."
  (declare (indent 1))
  (let ((backend (gptel--make-mistral
                  :name name
                  :host host
                  :header header
                  :key key
                  :models (gptel--process-models models)
                  :protocol protocol
                  :endpoint endpoint
                  :stream stream
                  :request-params request-params
                  :curl-args curl-args
                  :url (concat protocol "://" host endpoint))))
    (setf (alist-get name gptel--known-backends nil nil #'equal) backend)
    backend))

(provide 'gptel-mistral)
;;; gptel-mistral.el ends here

;; Local Variables:
;; byte-compile-warnings: (not docstrings)
;; End:
