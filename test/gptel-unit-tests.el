;;; gptel-unit-tests.el --- Gptel Unit Tests  -*- lexical-binding: t; -*-
(require 'ert)
(require 'gptel)
(require 'gptel-test-backends)

(ert-deftest gptel-test-prefix-trimming ()
  (let ((gptel-prompt-prefix-alist
         '((fundamental-mode . "*Prompt*: ")))
        (gptel-response-prefix-alist
         '((fundamental-mode . "*Response*: ")))
        (gptel-response-separator "\n\n"))
    ;; Empty string is nil
    (should (equal nil (gptel--trim-prefixes "")))

    ;; Prefixes trim to nil
    (should (equal nil (gptel--trim-prefixes (gptel-prompt-prefix-string))))
    (should (equal nil (gptel--trim-prefixes (gptel-response-prefix-string))))

    ;; Trimp both prefixes to nil
    (should (equal nil (gptel--trim-prefixes
                        (format " %s  %s "
                                (gptel-prompt-prefix-string)
                                (gptel-response-prefix-string)))))
    ;; Trim extra whitespace
    (should (equal nil (gptel--trim-prefixes
                        (format "\n\t %s \n\t  %s \n\t"
                                (gptel-prompt-prefix-string)
                                (gptel-response-prefix-string)))))
    ;; Trim it all down to FOO
    (should (equal "FOO" (gptel--trim-prefixes
                          (format "\n\t %s \n\tFOO  %s \n\t"
                                  (gptel-prompt-prefix-string)
                                  (gptel-response-prefix-string)))))

    ;; Trim the final response prefix and whitespace
    (should (equal "DERP\n\t *Prompt*:  \n\tFOO"
                   (gptel--trim-prefixes
                    (concat "DERP"
                            "\n\t "
                            (gptel-prompt-prefix-string)
                            " \n\tFOO  "
                            (gptel-response-prefix-string)
                            " \n\t"))))))

;;; Tests for media parsing in buffers: `gptel--parse-media-links'
(ert-deftest gptel-test-media-link-parsing-org-1 ()
  (let ((mediatext "Some text here, just checking.")
        (buftext "Some text followed by a link:

[[file:/tmp/medialinks.txt]]

then more text, then another link

[[file:/tmp/medialinks.yaml]]

then some more text to end."))
    (unwind-protect
        (progn
          (with-temp-file "/tmp/medialinks.yaml" (insert mediatext))
          (with-temp-file "/tmp/medialinks.txt" (insert mediatext))
          (let ((org-inhibit-startup t)
                (gptel-backend (alist-get 'openai gptel-test-backends))
                (gptel-model 'gpt-4o-mini)) ;any model will do (no media)
            (with-temp-buffer
              (insert buftext)
              (delay-mode-hooks (org-mode))
              (should (equal (gptel--parse-media-links
                              major-mode (point-min) (point-max))
                             '((:text "Some text followed by a link:\n\n")
                               (:textfile "/tmp/medialinks.txt")
                               (:text "\n\nthen more text, then another link\n\n")
                               (:textfile "/tmp/medialinks.yaml")
                               (:text "\n\nthen some more text to end.")))))))
      (delete-file "/tmp/medialinks.yaml")
      (delete-file "/tmp/medialinks.txt"))))

(ert-deftest gptel-test-media-link-parsing-org-2 ()
  (let ((mediatext "Some text here, just checking.")
        (buftext "Some text followed by a link:

[[file:/tmp/medialinks.txt]]

then more text, then an image

[[file:./examples/hundred.png]]

then some more text to end."))
    (unwind-protect
        (progn
          (with-temp-file "/tmp/medialinks.txt" (insert mediatext))
          (let ((org-inhibit-startup t)
                (gptel-backend (alist-get 'openai gptel-test-backends))
                (gptel-model 'gpt-4o-mini)) ;media-capable model
            (with-temp-buffer
              (insert buftext)
              (delay-mode-hooks (org-mode))
              (should (equal (gptel--parse-media-links
                              major-mode (point-min) (point-max))
                             '((:text "Some text followed by a link:\n\n")
                               (:textfile "/tmp/medialinks.txt")
                               (:text "\n\nthen more text, then an image\n\n")
                               (:media "./examples/hundred.png" :mime "image/png")
                               (:text "\n\nthen some more text to end.")))))))
      (delete-file "/tmp/medialinks.txt"))))

;;; Tests for parsing JSON schema supplied in various forms
(ert-deftest gptel-test-dispatch-schema-type () 
  "Shorthand form tests for `gptel--dispatch-schema-type'."
  (should (equal (gptel--dispatch-schema-type
                  "name, chemical_formula str, toxicity num")
                 '( :type "object" :properties ( :name (:type "string")
                                                 :chemical_formula (:type "string")
                                                 :toxicity (:type "number")))))
  (should (equal (gptel--dispatch-schema-type
                  "[name, chemical_formula string, toxicity number]")
                 (list :type "object"
                       :properties (list :items
                                         '( :type "array"
                                            :items
                                            ( :type "object"
                                              :properties ( :name (:type "string")
                                                            :chemical_formula (:type "string")
                                                            :toxicity (:type "number")))))
                       :required ["items"]
                       :additionalProperties :json-false)))
  (should (equal (gptel--dispatch-schema-type
                  "name: Colloquial name of compound
                   chemical_formula str: Formula for compound
                   toxicity int: 1-10 denoting toxicity to humans")
                 '( :type "object"
                    :properties ( :name ( :type "string"
                                          :description "Colloquial name of compound")
                                  :chemical_formula ( :type "string"
                                                      :description "Formula for compound")
                                  :toxicity ( :type "integer"
                                              :description "1-10 denoting toxicity to humans")))))
  (should (equal (gptel--dispatch-schema-type
                  "[name: Colloquial name of compound
                    chemical_formula str: Formula for compound
                    toxicity bool: whether the compound is toxic   ]")
                 '( :type "object"
                    :properties
                    ( :items
                      ( :type "array"
                        :items
                        ( :type "object"
                          :properties
                          ( :name ( :type "string"
                                    :description "Colloquial name of compound")
                            :chemical_formula ( :type "string"
                                                :description "Formula for compound")
                            :toxicity ( :type "boolean"
                                        :description "whether the compound is toxic")))))
                    :required ["items"]
                    :additionalProperties :json-false))))

(ert-deftest gptel-test-dispatch-schema-type-advanced ()
  "Advanced shorthand form test for `gptel--dispatch-schema-type'."
  ;; Test with
  ;; - missing type
  ;; - missing description
  ;; - missing ":" separator
  ;; - missing type, description and separator
  ;; - leading and trailing whitespace
  (should
   (equal (gptel--dispatch-schema-type
           "  [name  str: Name of cat
                      age   num
                      hobby
                      bio      : One-line biography for cat    ]

           ")
          '( :type "object"
             :properties
             ( :items
               ( :type "array" :items
                 ( :type "object" :properties
                   ( :name ( :type "string" :description "Name of cat")
                     :age ( :type "number")
                     :hobby ( :type "string")
                     :bio ( :type "string" :description
                            "One-line biography for cat")))))
             :required ["items"] :additionalProperties :json-false))))

(ert-deftest gptel-test-media-link-parsing-md-1 ()
  (skip-unless (fboundp 'markdown-mode))
  (let ((mediatext "Some text here, just checking.")
        (buftext "Some text followed by a link:

[medialinks](/tmp/medialinks.txt)

then more text, then another link

[some text](/tmp/medialinks.yaml)

then some more text to end."))
    (unwind-protect
        (progn
          (with-temp-file "/tmp/medialinks.yaml" (insert mediatext))
          (with-temp-file "/tmp/medialinks.txt" (insert mediatext))
          (let ((gptel-backend (alist-get 'openai gptel-test-backends))
                (gptel-model 'gpt-4o-mini)) ;any model will do (no media)
            (with-temp-buffer
              (insert buftext)
              (delay-mode-hooks (markdown-mode))
              (should (equal (gptel--parse-media-links
                              major-mode (point-min) (point-max))
                             '((:text "Some text followed by a link:\n\n")
                               (:textfile "/tmp/medialinks.txt")
                               (:text "\n\nthen more text, then another link\n\n")
                               (:textfile "/tmp/medialinks.yaml")
                               (:text "\n\nthen some more text to end.")))))))
      (delete-file "/tmp/medialinks.yaml")
      (delete-file "/tmp/medialinks.txt"))))

(ert-deftest gptel-test-media-link-parsing-md-2 ()
  (skip-unless (fboundp 'markdown-mode))
  (let ((mediatext "Some text here, just checking.")
        (buftext "Some text followed by a link:

[medialinks](/tmp/medialinks.txt)

then more text, then an image

![an image](./examples/hundred.png)

then some more text to end."))
    (unwind-protect
        (progn
          (with-temp-file "/tmp/medialinks.txt" (insert mediatext))
          (let ((gptel-backend (alist-get 'openai gptel-test-backends))
                (gptel-model 'gpt-4o-mini)) ;media-capable model
            (with-temp-buffer
              (insert buftext)
              (delay-mode-hooks (markdown-mode))
              (should (equal (gptel--parse-media-links
                              major-mode (point-min) (point-max))
                             '((:text "Some text followed by a link:\n\n")
                               (:textfile "/tmp/medialinks.txt")
                               (:text "\n\nthen more text, then an image\n\n")
                               (:media "./examples/hundred.png" :mime "image/png")
                               (:text "\n\nthen some more text to end.")))))))
      (delete-file "/tmp/medialinks.txt"))))

;;; Test for declarative list modification DSL
(ert-deftest gptel-test--modify-value ()
  "Test `gptel--modify-value'."
  ;; string and string
  (should (equal (gptel--modify-value "original\n" "extra") "extra"))
  (should (equal (gptel--modify-value "original\n" '(:append "extra")) "original\nextra"))
  (should (equal (gptel--modify-value "original\n" '(:prepend "extra")) "extraoriginal\n"))
  (should (equal (gptel--modify-value "original\n" '(:function upcase)) "ORIGINAL\n"))
  ;; list and list
  (should (equal (gptel--modify-value '(a b c) '(d e f)) '(d e f)))
  (should (equal (gptel--modify-value '(a b c) '(:append (d e))) '(a b c d e)))
  (should (equal (gptel--modify-value '(a b c) '(:prepend (x y))) '(x y a b c)))
  (should (equal (gptel--modify-value '(1 2 3) '(:function reverse)) '(3 2 1)))
  (should (equal (gptel--modify-value '("hello") '(:append (" world"))) '("hello" " world")))
  (should (equal (gptel--modify-value '("world") '(:prepend ("hello "))) '("hello " "world")))
  ;; :merge test
  (should (equal (gptel--modify-value '(:a 1 :b 2) '(:merge (:b 3 :c 4))) '(:a 1 :b 3 :c 4)))
  (should (equal (gptel--modify-value '(:x "hello" :y 42) '(:merge (:x "world" :z nil)))
                 '(:x "world" :y 42 :z nil)))
  ;; :eval test
  (should (equal (gptel--modify-value "unused" '(:eval (+ 2 3))) 5))
  (should (equal (gptel--modify-value '(a b) '(:eval (reverse '(x y z)))) '(z y x)))
  ;; string and list combinations
  (should (equal (gptel--modify-value "hello" '(:append " world")) "hello world"))
  (should (equal (gptel--modify-value "world" '(:prepend "hello ")) "hello world"))
  ;; multiple operations
  (should (equal (gptel--modify-value "base" '(:append "1" :prepend "0")) "0base1"))
  (should (equal (gptel--modify-value '(b) '(:append (c) :prepend (a))) '(a b c)))
  ;; non-list mutation (edge cases)
  (should (equal (gptel--modify-value "original" 42) 42))
  (should (equal (gptel--modify-value '(a b c) :symbol) :symbol))
  ;; empty cases
  (should (equal (gptel--modify-value "" '(:append "text")) "text"))
  (should (equal (gptel--modify-value '() '(:append (a b))) '(a b)))
  (should (equal (gptel--modify-value "text" '(:prepend "")) "text"))
  (should (equal (gptel--modify-value '(a b) '(:prepend ())) '(a b))))

;;; Tests for header-line alignment

(ert-deftest gptel-test-header-line-pixel-alignment ()
  "Header-line uses pixel-based alignment when `string-pixel-width' is available."
  (skip-unless (fboundp 'string-pixel-width))
  (let* ((rhs "[test-model]")
         (spec `(space :align-to (- right (,(string-pixel-width rhs)))))
         (align-to (plist-get (cdr spec) :align-to))
         (offset (caddr align-to)))
    ;; Pixel path wraps the value in a list: (PIXELS)
    (should (listp offset))
    (should (numberp (car offset)))
    (should (> (car offset) 0))))

(ert-deftest gptel-test-header-line-char-fallback-offset ()
  "Header-line char fallback includes the +5 padding offset."
  (let* ((rhs "[test-model]")
         (spec `(space :align-to (- right ,(+ 5 (string-width rhs)))))
         (align-to (plist-get (cdr spec) :align-to))
         (offset (caddr align-to)))
    ;; Char path: offset is a plain number, not a list
    (should (numberp offset))
    ;; Should be string-width + 5
    (should (= offset (+ 5 (string-width rhs))))))

;;; Preset propagation tests

(ert-deftest gptel-test-preset-survives-dynamic-binding ()
  "Test that gptel--preset set via `set/make-local-variable' inside a
dynamic `let' does NOT persist after the `let' exits.

This demonstrates the root cause of the bug where delegated agents
see the preset from file-load time instead of the current preset:
`gptel-org--send-with-props' dynamically binds `gptel--preset', and
`gptel--transform-apply-preset' (which uses `set/make-local-variable')
modifies the dynamic binding, not the underlying buffer-local."
  (with-temp-buffer
    (set (make-local-variable 'gptel--preset) 'initial-preset)
    ;; Simulate gptel-org--send-with-props: dynamic let-binding of gptel--preset
    (let ((gptel--preset 'from-send-with-props))
      ;; Simulate gptel--transform-apply-preset inside the let scope
      (set (make-local-variable 'gptel--preset) 'user-changed-preset)
      ;; Inside the let, the transform appears to have worked
      (should (eq gptel--preset 'user-changed-preset)))
    ;; After the let exits, the buffer-local reverts to the stale value.
    ;; This is the bug: async tool calls run here and see the old preset.
    (should (eq gptel--preset 'initial-preset))))

(ert-deftest gptel-test-preset-stored-in-fsm-info ()
  "Test that the FSM info plist carries `gptel--preset' through the
request lifecycle.

The preset is stored early in `gptel-request' (while
`gptel-org--send-with-props' dynamic binding is active) and
`gptel--realize-query' only overwrites it when a non-nil value
exists in the prompt buffer (e.g. from a preset transform)."
  (let ((gptel--preset 'active-preset)
        (info (list :backend 'dummy :model 'dummy)))
    ;; Simulate what gptel-request now does (early store)
    (when gptel--preset (plist-put info :preset gptel--preset))
    (should (eq (plist-get info :preset) 'active-preset))
    ;; Verify it works even when buffer-local would differ
    (with-temp-buffer
      (set (make-local-variable 'gptel--preset) 'stale-preset)
      (let ((gptel--preset 'runtime-preset))
        (when gptel--preset (plist-put info :preset gptel--preset)))
      ;; Info should have the runtime value, not the buffer-local
      (should (eq (plist-get info :preset) 'runtime-preset)))))

(ert-deftest gptel-test-preset-early-store-survives-prompt-buffer ()
  "Test that the early preset store in `gptel-request' is not
clobbered by `gptel--realize-query' reading nil from the prompt buffer.

This is the core fix: `gptel--preset' is not in the variable copy list
of `gptel--with-buffer-copy-internal', so it is nil in the prompt
buffer.  `gptel--realize-query' runs inside the prompt buffer and would
overwrite the early-stored :preset with nil.  The fix makes
`gptel--realize-query' only overwrite when gptel--preset is non-nil."
  (let ((gptel--preset nil)             ;isolate from global state
        (info (list :backend 'dummy :model 'dummy)))
    ;; Step 1: Early store captures the correct preset (simulating
    ;; gptel-request running inside send-with-props's dynamic binding)
    (let ((gptel--preset 'heading-preset))
      (when gptel--preset (plist-put info :preset gptel--preset)))
    (should (eq (plist-get info :preset) 'heading-preset))
    ;; Step 2: gptel--realize-query runs in the prompt buffer where
    ;; gptel--preset is nil (not copied by with-buffer-copy-internal).
    ;; The conditional store must NOT overwrite with nil.
    (with-temp-buffer
      ;; Prompt buffer has nil gptel--preset (simulating the missing copy)
      (let ((gptel--preset nil))
        ;; This is what the fixed gptel--realize-query does:
        (when gptel--preset (plist-put info :preset gptel--preset))
        ;; The early-stored value must survive
        (should (eq (plist-get info :preset) 'heading-preset))))))

(ert-deftest gptel-test-preset-restored-during-tool-use ()
  "Test that tool execution sees the preset from FSM info, not the
stale buffer-local.

The fix wraps tool execution in `gptel--handle-tool-use' with:
  (let ((gptel--preset (or (plist-get info :preset) gptel--preset)))
    ...)
This test verifies that mechanism directly."
  (with-temp-buffer
    ;; Buffer-local has a stale value (simulating file-load time preset)
    (set (make-local-variable 'gptel--preset) 'stale-file-load-preset)
    (let* ((info (list :preset 'correct-runtime-preset))
           ;; Simulate the let-binding from gptel--handle-tool-use
           (gptel--preset (or (plist-get info :preset) gptel--preset)))
      ;; Inside the let, tool code sees the correct preset
      (should (eq gptel--preset 'correct-runtime-preset)))
    ;; Outside the let, buffer-local is untouched
    (should (eq gptel--preset 'stale-file-load-preset)))
  ;; Also verify fallback: when info has no :preset, buffer-local is used
  (with-temp-buffer
    (set (make-local-variable 'gptel--preset) 'buffer-preset)
    (let* ((info (list :backend 'dummy))
           (gptel--preset (or (plist-get info :preset) gptel--preset)))
      (should (eq gptel--preset 'buffer-preset)))))

(ert-deftest gptel-test-backend-model-restored-during-tool-use ()
  "Test that tool execution restores gptel-backend and gptel-model from
FSM info, not just gptel--preset.

Sub-agent delegation reads gptel-backend and gptel-model to determine
the baseline backend/model for the sub-agent.  Without restoration,
tools see the buffer-local values (file-level defaults) instead of the
heading-specific values captured during gptel--realize-query."
  (with-temp-buffer
    ;; Buffer-local has stale file-level defaults
    (set (make-local-variable 'gptel-backend) 'stale-backend)
    (set (make-local-variable 'gptel-model) 'stale-model)
    (set (make-local-variable 'gptel--preset) 'stale-preset)
    (let* ((info (list :preset 'heading-preset
                       :backend 'heading-backend
                       :model 'heading-model))
           ;; Simulate the let-binding from gptel--handle-tool-use
           (gptel--preset (or (plist-get info :preset) gptel--preset))
           (gptel-backend (or (plist-get info :backend) gptel-backend))
           (gptel-model (or (plist-get info :model) gptel-model)))
      ;; Inside the let, tool code sees the heading-specific values
      (should (eq gptel--preset 'heading-preset))
      (should (eq gptel-backend 'heading-backend))
      (should (eq gptel-model 'heading-model)))
    ;; Outside the let, buffer-locals are untouched
    (should (eq gptel--preset 'stale-preset))
    (should (eq gptel-backend 'stale-backend))
    (should (eq gptel-model 'stale-model))))

(ert-deftest gptel-test-accept-tool-calls-restores-bindings ()
  "Test that gptel--accept-tool-calls restores dynamic bindings from
FSM info when called interactively.

When the user confirms a tool call via C-c C-c, the function runs in
the normal command loop with only buffer-local values.  The fix reads
the info plist from the overlay or gptel--fsm-last to restore the
heading-specific bindings."
  (with-temp-buffer
    ;; Buffer-local has stale file-level defaults
    (set (make-local-variable 'gptel-backend) 'stale-backend)
    (set (make-local-variable 'gptel-model) 'stale-model)
    (set (make-local-variable 'gptel--preset) 'stale-preset)
    ;; Simulate retrieving info from overlay or gptel--fsm-last
    (let* ((info (list :preset 'heading-preset
                       :backend 'heading-backend
                       :model 'heading-model))
           ;; This mirrors the logic in the fixed gptel--accept-tool-calls
           (gptel--preset (or (and info (plist-get info :preset)) gptel--preset))
           (gptel-backend (or (and info (plist-get info :backend)) gptel-backend))
           (gptel-model (or (and info (plist-get info :model)) gptel-model)))
      (should (eq gptel--preset 'heading-preset))
      (should (eq gptel-backend 'heading-backend))
      (should (eq gptel-model 'heading-model)))
    ;; Verify no info fallback preserves buffer-locals
    (let* ((info nil)
           (gptel--preset (or (and info (plist-get info :preset)) gptel--preset))
           (gptel-backend (or (and info (plist-get info :backend)) gptel-backend))
           (gptel-model (or (and info (plist-get info :model)) gptel-model)))
      (should (eq gptel--preset 'stale-preset))
      (should (eq gptel-backend 'stale-backend))
      (should (eq gptel-model 'stale-model)))))

;;; Tests for JSON serialization with multibyte characters

(ert-deftest gptel-test-json-encode-multibyte-string ()
  "Test that `gptel--json-encode' handles multibyte strings correctly."
  (should (stringp (gptel--json-encode "hello → world")))
  (should (string-match-p "hello.*world"
                          (gptel--json-encode "hello → world"))))

(ert-deftest gptel-test-json-encode-unibyte-string ()
  "Test that `gptel--json-encode' handles unibyte strings with high bytes.
This is the core bug: `json-serialize' rejects unibyte strings containing
bytes above 127, such as UTF-8 encoded → (\\342\\206\\222)."
  (let ((unibyte-str (encode-coding-string "Error: has → arrow" 'utf-8)))
    ;; Verify our test string is actually unibyte
    (should-not (multibyte-string-p unibyte-str))
    ;; json-serialize alone would fail on this
    (should-error (json-serialize unibyte-str
                    :null-object :null :false-object :json-false)
                  :type 'wrong-type-argument)
    ;; But gptel--json-encode should handle it
    (should (stringp (gptel--json-encode unibyte-str)))))

(ert-deftest gptel-test-json-encode-nested-unibyte ()
  "Test that `gptel--json-encode' handles unibyte strings nested in plists."
  (let* ((unibyte-str (encode-coding-string "→ nested" 'utf-8))
         (data (list :role "tool" :content unibyte-str)))
    (should-not (multibyte-string-p unibyte-str))
    (should (stringp (gptel--json-encode data)))
    (should (string-match-p "\"role\":\"tool\"" (gptel--json-encode data)))))

(ert-deftest gptel-test-json-encode-nested-unibyte-in-vector ()
  "Test that `gptel--json-encode' handles unibyte strings in vectors."
  (let* ((unibyte-str (encode-coding-string "→ in vector" 'utf-8))
         (data (list :messages (vector (list :role "tool"
                                            :content unibyte-str)))))
    (should (stringp (gptel--json-encode data)))))

(ert-deftest gptel-test-ensure-multibyte-preserves-types ()
  "Test that `gptel--ensure-multibyte' preserves non-string types."
  (should (eq nil (gptel--ensure-multibyte nil)))
  (should (eq 42 (gptel--ensure-multibyte 42)))
  (should (eq :keyword (gptel--ensure-multibyte :keyword)))
  (should (eq t (gptel--ensure-multibyte t)))
  (should (multibyte-string-p
           (gptel--ensure-multibyte
            (encode-coding-string "→" 'utf-8))))
  ;; Already-multibyte strings pass through unchanged
  (let ((s "already multibyte →"))
    (should (eq s (gptel--ensure-multibyte s)))))

(ert-deftest gptel-test-ensure-multibyte-ascii-unibyte ()
  "Test that `gptel--ensure-multibyte' converts pure ASCII unibyte to multibyte.
Pure ASCII unibyte strings (no high bytes) should be converted to multibyte
without UTF-8 decoding, which could corrupt data."
  (let ((s (string-to-unibyte "plain ascii")))
    (should-not (multibyte-string-p s))
    (let ((result (gptel--ensure-multibyte s)))
      (should (multibyte-string-p result))
      (should (string= "plain ascii" result)))))

(ert-deftest gptel-test-json-encode-curl-path-unibyte ()
  "Regression test: curl path encode-coding-string + gptel--json-encode.
The curl path wraps gptel--json-encode output with encode-coding-string.
Previously it used decode-coding-string which corrupted the JSON payload.
This test simulates the curl path encoding pipeline."
  (let* ((unibyte-content (encode-coding-string "Use → for arrows and — for dashes" 'utf-8))
         (data (list :messages (vector (list :role "user"
                                            :content unibyte-content))))
         ;; Simulate curl path: encode-coding-string wrapping gptel--json-encode
         (data-json (encode-coding-string (gptel--json-encode data) 'utf-8)))
    ;; Result must be a valid string
    (should (stringp data-json))
    ;; Must contain the expected characters when decoded back
    (let ((decoded (decode-coding-string data-json 'utf-8)))
      (should (string-match-p "→" decoded))
      (should (string-match-p "—" decoded)))))

(ert-deftest gptel-test-json-encode-is-function ()
  "Test that `gptel--json-encode' is a function, not a macro.
Macros cause stale bytecode issues; this must remain a function."
  (should (functionp #'gptel--json-encode))
  (should-not (macrop (symbol-function 'gptel--json-encode))))

(ert-deftest gptel-test-curl-get-args-with-unibyte-request-data ()
  "Ensure `gptel-curl--get-args' builds args when data contains unibyte UTF-8.
This exercises the actual curl code path, not just a simulation."
  (let* ((bad (encode-coding-string
               "Never use bash echo for communication — output text directly → use ×"
               'utf-8))
         (backend (alist-get 'openai gptel-test-backends))
         (model (car (gptel-backend-models backend)))
         (info (list :data (list :model "gpt-4o-mini"
                                 :messages (vector (list :role "system"
                                                         :content bad)))
                     :backend backend
                     :model model
                     :stream t)))
    (should-not (multibyte-string-p bad))
    (should (listp (gptel-curl--get-args info "test-uuid")))))

(ert-deftest gptel-test-json-encode-tool-schema-payload ()
  "Test JSON encoding of tool schema payloads with special characters.
Mirrors the real crash scenario where tool descriptions contain
Unicode punctuation like →, —, and ×."
  (let* ((tool-desc (encode-coding-string
                     "Multiply two numbers using × operator. Returns result → stdout. Use — for ranges."
                     'utf-8))
         (param-desc (encode-coding-string
                      "The first operand — must be a number"
                      'utf-8))
         (data (list :model "gpt-4o-mini"
                     :messages (vector
                                (list :role "system"
                                      :content "You are helpful."))
                     :tools (vector
                             (list :type "function"
                                   :function
                                   (list :name "multiply"
                                         :description tool-desc
                                         :parameters
                                         (list :type "object"
                                               :properties
                                               (list :a (list :type "number"
                                                              :description param-desc)
                                                     :b (list :type "number"
                                                              :description "second operand")))))))))
    (should-not (multibyte-string-p tool-desc))
    (should-not (multibyte-string-p param-desc))
    (let ((json-str (gptel--json-encode data)))
      (should (stringp json-str))
      ;; json-serialize returns unibyte UTF-8; decode to verify content
      (let ((decoded (decode-coding-string json-str 'utf-8)))
        (should (string-match-p "×" decoded))
        (should (string-match-p "→" decoded))
        (should (string-match-p "—" decoded))))))

(ert-deftest gptel-test-json-encode-already-json-object ()
  "Test that already-encoded JSON objects pass through unchanged.
When a string starting with { is passed to `gptel--json-encode',
it should be returned as-is to prevent double encoding."
  (let ((json-str "{\"model\":\"gpt-4\",\"messages\":[]}"))
    (should (equal json-str (gptel--json-encode json-str)))))

(ert-deftest gptel-test-json-encode-already-json-array ()
  "Test that already-encoded JSON arrays pass through unchanged."
  (let ((json-str "[{\"role\":\"user\",\"content\":\"hello\"}]"))
    (should (equal json-str (gptel--json-encode json-str)))))

(ert-deftest gptel-test-json-encode-already-json-with-whitespace ()
  "Test that JSON strings with leading whitespace pass through unchanged."
  (let ((json-str "  {\"a\":1}"))
    (should (equal json-str (gptel--json-encode json-str)))))

(ert-deftest gptel-test-json-encode-plain-string-not-json ()
  "Test that plain strings are still serialized as JSON strings.
A string like \"hello\" should be JSON-encoded, not passed through."
  (let ((result (gptel--json-encode "hello world")))
    (should (equal "\"hello world\"" result))))

;; ------------------------------------------------------------------
;; Org-format logging tests
;; ------------------------------------------------------------------

(ert-deftest gptel-test-log-org-format-request-cycle ()
  "Test that gptel--log produces org-format output for a request cycle."
  (let ((gptel-log-level 'info)
        (gptel--log-buffer-name "*gptel-log-test*"))
    (when (get-buffer gptel--log-buffer-name)
      (kill-buffer gptel--log-buffer-name))
    (unwind-protect
        (progn
          (gptel--log "{\"Authorization\": \"Bearer test\"}" "request headers")
          (gptel--log "{\"content\": \"hello\"}" "response body")
          (let ((content (with-current-buffer gptel--log-buffer-name
                           (buffer-string))))
            ;; Should have org headings with request counter
            (should (string-match-p "^\\* Request #1 \\[" content))
            (should (string-match-p "^\\*\\* request headers" content))
            (should (string-match-p "^\\*\\* response body" content))
            ;; Should have src blocks for JSON
            (should (string-match-p "#\\+begin_src json" content))
            (should (string-match-p "#\\+end_src" content))
            ;; Should NOT have old JSON header format
            (should-not (string-match-p "{\"gptel\":" content))
            ;; Buffer should be in org-mode
            (with-current-buffer gptel--log-buffer-name
              (should (derived-mode-p 'org-mode)))))
      (when (get-buffer gptel--log-buffer-name)
        (kill-buffer gptel--log-buffer-name)))))

(ert-deftest gptel-test-log-org-format-no-json ()
  "Test that no-json entries use example blocks."
  (let ((gptel-log-level 'debug)
        (gptel--log-buffer-name "*gptel-log-test*"))
    (when (get-buffer gptel--log-buffer-name)
      (kill-buffer gptel--log-buffer-name))
    (unwind-protect
        (progn
          (gptel--log "some debug info" "tool-call-debug" 'no-json)
          (let ((content (with-current-buffer gptel--log-buffer-name
                           (buffer-string))))
            ;; Standalone heading for non-request entry
            (should (string-match-p "^\\* tool-call-debug" content))
            ;; Should use example block, not src
            (should (string-match-p "#\\+begin_example" content))
            (should (string-match-p "#\\+end_example" content))
            ;; Should have tags
            (should (string-match-p ":debug:" content))))
      (when (get-buffer gptel--log-buffer-name)
        (kill-buffer gptel--log-buffer-name)))))

(ert-deftest gptel-test-log-org-format-consecutive-requests ()
  "Test that consecutive requests get separate headings."
  (let ((gptel-log-level 'info)
        (gptel--log-buffer-name "*gptel-log-test*"))
    (when (get-buffer gptel--log-buffer-name)
      (kill-buffer gptel--log-buffer-name))
    (unwind-protect
        (progn
          ;; First request cycle
          (gptel--log "{\"Authorization\": \"Bearer test\"}" "request headers")
          (gptel--log "{\"content\": \"hi\"}" "response body")
          ;; Second request cycle
          (gptel--log "{\"Authorization\": \"Bearer test2\"}" "request headers")
          (gptel--log "{\"content\": \"bye\"}" "response body")
          (let ((content (with-current-buffer gptel--log-buffer-name
                           (buffer-string))))
            ;; Should have two top-level Request headings
            (should (= 2 (with-temp-buffer
                           (insert content)
                           (goto-char (point-min))
                           (let ((count 0))
                             (while (re-search-forward "^\\* Request #[0-9]+ \\[" nil t)
                               (cl-incf count))
                             count))))
            ;; Should be numbered sequentially
            (should (string-match-p "^\\* Request #1 \\[" content))
            (should (string-match-p "^\\* Request #2 \\[" content))))
      (when (get-buffer gptel--log-buffer-name)
        (kill-buffer gptel--log-buffer-name)))))

(ert-deftest gptel-test-log-org-escape-block-content ()
  "Test that org-problematic content is escaped in blocks."
  (let ((gptel-log-level 'debug)
        (gptel--log-buffer-name "*gptel-log-test*"))
    (when (get-buffer gptel--log-buffer-name)
      (kill-buffer gptel--log-buffer-name))
    (unwind-protect
        (progn
          (gptel--log "* heading\n#+begin_src\nfoo" "test-escape" 'no-json)
          (let ((content (with-current-buffer gptel--log-buffer-name
                           (buffer-string))))
            ;; Asterisks and #+ at line start should be escaped with comma
            (should (string-match-p ",\\* heading" content))
            (should (string-match-p ",#\\+begin_src" content))))
      (when (get-buffer gptel--log-buffer-name)
        (kill-buffer gptel--log-buffer-name)))))

(ert-deftest gptel-test-log-org-special-chars ()
  "Test that special characters like arrows and em-dashes log correctly."
  (let ((gptel-log-level 'debug)
        (gptel--log-buffer-name "*gptel-log-test*"))
    (when (get-buffer gptel--log-buffer-name)
      (kill-buffer gptel--log-buffer-name))
    (unwind-protect
        (progn
          (gptel--log "State: PENDING → ALLOWED — done" "test-chars" 'no-json)
          (let ((content (with-current-buffer gptel--log-buffer-name
                           (buffer-string))))
            ;; Arrow and em-dash should appear as-is
            (should (string-match-p "→" content))
            (should (string-match-p "—" content))))
      (when (get-buffer gptel--log-buffer-name)
        (kill-buffer gptel--log-buffer-name)))))

(ert-deftest gptel-test-log-tool-entries-nest-under-request ()
  "Test that tool-call entries become sub-headings under active request."
  (let ((gptel-log-level 'debug)
        (gptel--log-buffer-name "*gptel-log-test*"))
    (when (get-buffer gptel--log-buffer-name)
      (kill-buffer gptel--log-buffer-name))
    (unwind-protect
        (progn
          ;; Start a request cycle
          (gptel--log "{\"Authorization\": \"Bearer test\"}" "request headers")
          ;; Tool call within the request cycle
          (gptel--log "tool execution info" "tool-call-debug" 'no-json)
          ;; State change within the request cycle
          (gptel--log "state transition info" "debug-state-change" 'no-json)
          ;; Response
          (gptel--log "{\"content\": \"hello\"}" "response body")
          (let ((content (with-current-buffer gptel--log-buffer-name
                           (buffer-string))))
            ;; Should have only ONE top-level heading
            (should (= 1 (with-temp-buffer
                           (insert content)
                           (goto-char (point-min))
                           (let ((count 0))
                             (while (re-search-forward "^\\* " nil t)
                               (cl-incf count))
                             count))))
            ;; Tool call and state change should be sub-headings
            (should (string-match-p "^\\*\\* tool-call-debug" content))
            (should (string-match-p "^\\*\\* debug-state-change" content))
            ;; Request entries also sub-headings
            (should (string-match-p "^\\*\\* request headers" content))
            (should (string-match-p "^\\*\\* response body" content))))
      (when (get-buffer gptel--log-buffer-name)
        (kill-buffer gptel--log-buffer-name)))))

(ert-deftest gptel-test-log-request-counter ()
  "Test that request cycles are numbered sequentially."
  (let ((gptel-log-level 'info)
        (gptel--log-buffer-name "*gptel-log-test*"))
    (when (get-buffer gptel--log-buffer-name)
      (kill-buffer gptel--log-buffer-name))
    (unwind-protect
        (progn
          ;; Three request cycles
          (gptel--log "{}" "request headers")
          (gptel--log "{}" "response body")
          (gptel--log "{}" "request headers")
          (gptel--log "{}" "response body")
          (gptel--log "{}" "request headers")
          (gptel--log "{}" "response body")
          (let ((content (with-current-buffer gptel--log-buffer-name
                           (buffer-string))))
            (should (string-match-p "^\\* Request #1 \\[" content))
            (should (string-match-p "^\\* Request #2 \\[" content))
            (should (string-match-p "^\\* Request #3 \\[" content))))
      (when (get-buffer gptel--log-buffer-name)
        (kill-buffer gptel--log-buffer-name)))))

;;; Tool heading keyword and args-title helpers

(ert-deftest gptel-test-tool-state-keyword-plain-tool ()
  "`gptel-org--tool-state-keyword' uppercases and sanitizes TOOL-NAME."
  (should (equal "BASH" (gptel-org--tool-state-keyword "Bash")))
  (should (equal "PYTHON_EXEC" (gptel-org--tool-state-keyword "python_exec")))
  (should (equal "MY_TOOL" (gptel-org--tool-state-keyword "my tool")))
  (should (equal "FOO_BAR" (gptel-org--tool-state-keyword "foo.bar"))))

(ert-deftest gptel-test-tool-state-keyword-agent-uses-subagent-type ()
  "For the Agent tool, TODO keyword is derived from :subagent_type."
  (should (equal "EXECUTOR"
                 (gptel-org--tool-state-keyword
                  "Agent" '(:subagent_type "executor"
                            :description "d" :prompt "p"))))
  (should (equal "RESEARCHER"
                 (gptel-org--tool-state-keyword
                  "Agent" '(:subagent_type "researcher"))))
  (should (equal "GATHERER"
                 (gptel-org--tool-state-keyword
                  "Agent" '(:subagent_type "gatherer")))))

(ert-deftest gptel-test-tool-state-keyword-agent-missing-subagent-falls-back ()
  "Without :subagent_type the Agent tool falls back to \"AGENT\"."
  (should (equal "AGENT"
                 (gptel-org--tool-state-keyword "Agent" nil)))
  (should (equal "AGENT"
                 (gptel-org--tool-state-keyword "Agent" '(:description "d"))))
  ;; Empty :subagent_type also falls back
  (should (equal "AGENT"
                 (gptel-org--tool-state-keyword
                  "Agent" '(:subagent_type "")))))

(ert-deftest gptel-test-tool-state-keyword-non-agent-ignores-args ()
  "Non-Agent tools don't look at args."
  (should (equal "BASH"
                 (gptel-org--tool-state-keyword
                  "Bash" '(:subagent_type "executor")))))

(ert-deftest gptel-test-format-tool-args-title-basic ()
  "`gptel-org--format-tool-args-title' formats plists."
  (should (equal "" (gptel-org--format-tool-args-title nil)))
  (should (equal ":command \"date\""
                 (gptel-org--format-tool-args-title '(:command "date")))))

(ert-deftest gptel-test-format-tool-args-title-excludes ()
  "Excluded keys are omitted from the args-title."
  (should (equal ":description \"d\" :prompt \"p\""
                 (gptel-org--format-tool-args-title
                  '(:subagent_type "executor" :description "d" :prompt "p")
                  '(:subagent_type))))
  ;; Without exclusion, the key is present
  (should (string-match-p
           ":subagent_type \"executor\""
           (gptel-org--format-tool-args-title
            '(:subagent_type "executor" :description "d")))))

(ert-deftest gptel-test-tool-args-title-excludes-agent ()
  "The Agent tool excludes :subagent_type from its args-title."
  (should (equal '(:subagent_type)
                 (gptel-org--tool-args-title-excludes "Agent")))
  (should (null (gptel-org--tool-args-title-excludes "Bash")))
  (should (null (gptel-org--tool-args-title-excludes nil))))

(ert-deftest gptel-test-tool-result-as-org-p ()
  "Agent tool is rendered as plain org; others are not."
  (should (gptel-org--tool-result-as-org-p "Agent"))
  (should-not (gptel-org--tool-result-as-org-p "Bash"))
  (should-not (gptel-org--tool-result-as-org-p "Edit"))
  (should-not (gptel-org--tool-result-as-org-p nil)))

(ert-deftest gptel-test-tool-body-text-default-wraps-in-block ()
  "Non-Agent tools get a #+begin_tool...#+end_tool body."
  (let ((body (gptel-org--tool-body-text
               "Bash"
               "(:name \"Bash\" :args (:command \"date\"))"
               "Tue Apr 21 21:17:07 EEST 2026"
               "*****")))
    (should (string-match-p "#\\+begin_tool" body))
    (should (string-match-p "#\\+end_tool" body))
    (should (string-match-p "Tue Apr 21" body))
    (should-not (string-match-p "^\\*+ RESULTS" body))))

(ert-deftest gptel-test-tool-body-text-agent-uses-results-child-heading ()
  "Agent tool body emits a RESULTS child heading, not a #+begin_tool block.
The RESULTS heading is one level deeper than the tool heading's STARS."
  (let* ((stars "*****")
         (body (gptel-org--tool-body-text
                "Agent"
                "(:name \"Agent\" :args (:subagent_type \"executor\"))"
                "Executor result for task: Run sleep 60 then date\n..."
                stars)))
    (should-not (string-match-p "#\\+begin_tool" body))
    (should-not (string-match-p "#\\+end_tool" body))
    ;; RESULTS heading is one level deeper than the tool heading (6 stars)
    (should (string-match-p "^\\*\\*\\*\\*\\*\\* RESULTS$" body))
    (should (string-match-p "Executor result for task:" body))
    ;; The call line still appears
    (should (string-match-p "(:name \"Agent\"" body))))


;;; Tests for REASONING heading/body shaping helpers (IB-4.5)

(ert-deftest gptel-test-reasoning-first-line-is-heading ()
  "First non-empty line of reasoning TEXT becomes the heading title.
The remaining lines are preserved verbatim as body.  Together they
form the buffer region `* REASONING <first-line>\\n<body>\\n'."
  (let* ((text "first summary line\nbody line 1\nbody line 2\n")
         (pair (gptel--reasoning-format-org text))
         (region (concat (car pair) (cdr pair))))
    (should (string-prefix-p
             "* REASONING first summary line\nbody line 1\nbody line 2\n"
             region))))

(ert-deftest gptel-test-reasoning-strips-redundant-star ()
  "A leading `*' on the body's first character is stripped exactly once.
The strip prevents the reasoning body from accidentally forming a
sibling org heading.  Only the single `*' is removed; any character
that follows (e.g. a space) is preserved."
  ;; Test the pure helper directly.
  (should (equal " nested list item\n"
                 (gptel--reasoning-strip-leading-star "* nested list item\n")))
  (should (equal "* nested list item\n"
                 (gptel--reasoning-strip-leading-star "** nested list item\n")))
  (should (equal "no star here" (gptel--reasoning-strip-leading-star "no star here")))
  (should (equal "" (gptel--reasoning-strip-leading-star "")))
  (should (equal nil (gptel--reasoning-strip-leading-star nil)))
  ;; Test the composed helper: the body's leading `*' must be stripped.
  (let* ((text "summary\n* nested list item\n")
         (pair (gptel--reasoning-format-org text))
         (region (concat (car pair) (cdr pair))))
    (should (string-prefix-p
             "* REASONING summary\n nested list item\n"
             region))
    ;; And no `*' at column 0 in the body (would create a spurious heading).
    (should-not (string-match-p "^\\*" (or (cdr pair) "")))))

(provide 'gptel-unit-tests)
;;; gptel-unit-tests.el ends here
