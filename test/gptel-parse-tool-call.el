;; -*- lexical-binding: t; -*-
(require 'ert)
(require 'gptel)
(require 'gptel-org)
(require 'gptel-test-backends)
(require 'cl-lib)
(require 'map)

;; Unit tests for `gptel--inject-tool-call'
(ert-deftest gptel-test-inject-tool-call ()
  "Ensure that tool argument injection into messages arrays works for all backends."
  (skip-unless (fboundp 'gptel--inject-tool-call))
  (let* ((backend (alist-get 'openai gptel-test-backends))
         (testinfo
          `( :backend ,backend
             :data (:messages
                    [( :role "system" :content
                       "You are a large language model living in Emacs and a helpful assistant.")
                     ( :role "user" :content
                       [(:type "text" :text "what are the reviews like for Hamlet (1996)?")])
                     ( :role "assistant" :content :null :tool_calls
                       [( :id "fc_bb3ceed0-6c06-49ca-8851-8bde396c85aa" :type "function"
                          :function ( :name "WebSearch"
                                      :arguments "{\"count\":5,\"query\":\"Hamlet 1996 reviews\"}")
                          :index 0)]
                       :reasoning
                       "We need up-to-date info about reviews for the film Hamlet (1996). \
We need to search the web. Use functions.WebSearch.")])))
         (tool-call '( :id "fc_bb3ceed0-6c06-49ca-8851-8bde396c85aa" :name "WebSearch"
                       :args (:count 5 :query "Hamlet 1996 reviews")))
         (new-args '(:args (:count 10 :query "Some other movie")))
         (new-name '(:name "NewSearch")))
    ;; Change args
    (gptel--inject-tool-call backend (plist-get testinfo :data) tool-call new-args)
    (should (equal (map-nested-elt testinfo '(:data :messages 2 :tool_calls 0 :function :arguments))
                   (gptel--json-encode (plist-get new-args :args))))

    ;; Change tool name
    (gptel--inject-tool-call backend (plist-get testinfo :data) tool-call new-name)
    (should (equal (map-nested-elt testinfo '(:data :messages 2 :tool_calls 0 :function :name))
                   (plist-get new-name :name)))

    ;; Change args and tool name
    (gptel--inject-tool-call backend (plist-get testinfo :data) tool-call
                             '(:name "NewSearch2" :args (:count 20 :query "Hamlet 1944")))
    (let ((new-call (map-nested-elt testinfo '(:data :messages 2 :tool_calls 0 :function))))
      (should (equal (plist-get new-call :name) "NewSearch2"))
      (should (equal (plist-get new-call :arguments)
                     (gptel--json-encode '(:count 20 :query "Hamlet 1944")))))

    ;; Delete tool call
    (let ((orig-messages (copy-tree (map-nested-elt testinfo '(:data :messages)))))
      (gptel--inject-tool-call backend (plist-get testinfo :data) tool-call nil)
      (should (equal (map-nested-elt testinfo '(:data :messages))
                     (substring orig-messages 0 -1)))))

  (let* ((backend (alist-get 'anthropic gptel-test-backends))
         (testinfo
          `( :backend ,backend
             :data (:messages
                    [( :role "user"
                       :content [(:type "text" :text
                                        "what are the reviews like for Hamlet (1996)?"
                                        :cache_control (:type "ephemeral"))])
                     ( :role "assistant"
                       :content [(:type "text" :text
                                        "I'll search for reviews of Hamlet (1996) for you.")
                                 (:type "tool_use" :id "toolu_01J1ZgM54cxXMsEwQn8zMJxH" :name
                                        "WebSearch" :input (:query "Hamlet 1996 reviews"))])])))
         (tool-call '( :id "toolu_01J1ZgM54cxXMsEwQn8zMJxH" :name "WebSearch" :input nil
                       :args (:query "Hamlet 1996 reviews")))
         (new-args '(:args (:query "Some other movie")))
         (new-name '(:name "NewSearch")))
    ;; Change args
    (gptel--inject-tool-call backend (plist-get testinfo :data) tool-call new-args)
    (should (equal (map-nested-elt testinfo '(:data :messages 1 :content 1 :input))
                   (plist-get new-args :args)))
    ;; Change tool name
    (gptel--inject-tool-call backend (plist-get testinfo :data) tool-call new-name)
    (should (equal (map-nested-elt testinfo '(:data :messages 1 :content 1 :name))
                   (plist-get new-name :name)))
    ;; Delete tool call
    (let ((expected-content
           [(:type "text" :text "I'll search for reviews of Hamlet (1996) for you.")]))
      (gptel--inject-tool-call backend (plist-get testinfo :data) tool-call nil)
      (should (equal (map-nested-elt testinfo '(:data :messages 1 :content))
                     expected-content))))

  (let* ((backend (alist-get 'gemini gptel-test-backends))
         (testinfo
          `( :backend ,backend
             :data (:contents
                    [( :role "user" :parts
                       [(:text "what are the reviews like for Hamlet (1996)?")])
                     ( :role "model" :parts
                       [(:text "")
                        (:functionCall
                         (:name "WebSearch" :args
                                (:query "Hamlet movie 1996 reviews critical reception"))
                         :thoughtSignature "EtEDCs4DAb4")])])))
         (tool-call '( :name "WebSearch"
                       :args (:query "Hamlet movie 1996 reviews critical reception")))
         (new-args '(:args (:query "Some other movie")))
         (new-name '(:name "NewSearch")))
    ;; Change args
    (gptel--inject-tool-call backend (plist-get testinfo :data) tool-call new-args)
    (should (equal (map-nested-elt testinfo '(:data :contents 1 :parts 1 :functionCall :args))
                   (plist-get new-args :args)))
    ;; Change tool name
    (setq tool-call (list :name "WebSearch" :args (plist-get new-args :args)))
    (gptel--inject-tool-call backend (plist-get testinfo :data) tool-call new-name)
    (should (equal (map-nested-elt testinfo '(:data :contents 1 :parts 1 :functionCall :name))
                   (plist-get new-name :name)))
    ;; Delete tool call
    (setq tool-call (list :name (plist-get new-name :name) :args (plist-get new-args :args)))
    (let ((expected-parts [(:text "")]))
      (gptel--inject-tool-call backend (plist-get testinfo :data) tool-call nil)
      (should (equal (map-nested-elt testinfo '(:data :contents 1 :parts))
                     expected-parts))))

  (let* ((backend (alist-get 'ollama gptel-test-backends))
         (testinfo
          `( :backend ,backend
             :model "qwen3"
             :data
             (:messages
              [( :role "user"
                 :content "What are the current weather conditions and temperature in New York and London?")
               ( :role "assistant"
                 :tool_calls [( :type "function"
                                :function ( :index 0 :name "get_temperature"
                                            :arguments (:city "New York")))
                              ( :type "function"
                                :function ( :index 1 :name "get_conditions"
                                            :arguments (:city "New York")))
                              ( :type "function"
                                :function ( :index 2 :name "get_temperature"
                                            :arguments (:city "London")))
                              ( :type "function"
                                :function ( :index 3 :name "get_conditions"
                                            :arguments (:city "London")))])]
              :stream :json-false)))
         (tool-call '( :name "get_temperature" :args (:city "London")))
         (new-args '(:args (:city "Khartoum")))
         (new-name '(:name "get_temp")))
    ;; Change args
    (gptel--inject-tool-call backend (plist-get testinfo :data) tool-call new-args)
    (should (equal (map-nested-elt testinfo '(:data :messages 1 :tool_calls 2 :function :arguments))
                   (plist-get new-args :args)))

    ;; Change tool name
    (setq tool-call (list :name "get_temperature" :args (plist-get new-args :args)))
    (gptel--inject-tool-call backend (plist-get testinfo :data) tool-call new-name)
    (should (equal (map-nested-elt testinfo '(:data :messages 1 :tool_calls 2 :function :name))
                   (plist-get new-name :name)))
    ;; Delete tool call
    (setq tool-call (list :name (plist-get new-name :name) :args (plist-get new-args :args)))
    (let ((expected-calls [( :type "function"
                             :function ( :index 0 :name "get_temperature"
                                         :arguments (:city "New York")))
                           ( :type "function"
                             :function ( :index 1 :name "get_conditions"
                                         :arguments (:city "New York")))
                           ( :type "function"
                             :function ( :index 3 :name "get_conditions"
                                         :arguments (:city "London")))]))
      (gptel--inject-tool-call backend (plist-get testinfo :data) tool-call nil)
      (should (equal (map-nested-elt testinfo '(:data :messages 1 :tool_calls))
                     expected-calls))))

  (let* ((backend (alist-get 'bedrock gptel-test-backends))
         (testinfo
          `( :backend ,backend
             :data (:messages
                    [( :role "assistant" :content
                       [(:toolUse (:name "my_tool" :input (:arg1 1) :toolUseId "123"))])])))
         (tool-call '( :id "123" :name "my_tool"))
         (new-args '(:args (:arg1 2 :arg2 3)))
         (new-name '(:name "my_tool_new")))
    ;; Change args
    (gptel--inject-tool-call backend (plist-get testinfo :data) tool-call new-args)
    (should (equal (map-nested-elt testinfo '(:data :messages 0 :content 0 :toolUse :input))
                   (plist-get new-args :args)))

    ;; Change tool name
    (gptel--inject-tool-call backend (plist-get testinfo :data) tool-call new-name)
    (should (equal (map-nested-elt testinfo '(:data :messages 0 :content 0 :toolUse :name))
                   (plist-get new-name :name)))

    ;; Change args and tool name
    (gptel--inject-tool-call backend (plist-get testinfo :data) tool-call
                             '(:name "my_tool_new_2" :args (:arg1 4)))
    (let ((new-call (map-nested-elt testinfo '(:data :messages 0 :content 0 :toolUse))))
      (should (equal (plist-get new-call :name) "my_tool_new_2"))
      (should (equal (plist-get new-call :input) '(:arg1 4))))

    ;; Delete tool call (single item in content -> deletes whole message)
    (let ((testinfo-delete (copy-tree testinfo t)))
      (gptel--inject-tool-call backend (plist-get testinfo-delete :data) tool-call nil)
      (should (equal (map-nested-elt testinfo-delete '(:data :messages)) [])))

    ;; Delete tool call (multiple items in content -> deletes just the toolUse chunk)
    (let ((testinfo-multi
           `( :backend ,backend
              :data (:messages
                     [( :role "assistant" :content
                        [(:text "Here is a tool call")
                         (:toolUse (:name "my_tool" :input (:arg1 1) :toolUseId "123"))])]))))
      (gptel--inject-tool-call backend (plist-get testinfo-multi :data) tool-call nil)
      (should (equal (map-nested-elt testinfo-multi '(:data :messages 0 :content))
                     [(:text "Here is a tool call")])))))

;; ;; Ollama sample messages array
;; ( :model "qwen3"
;;   :messages
;;   [( :role "user"
;;      :content "What are the current weather conditions and temperature in New York and London?")
;;    ( :role "assistant"
;;      :tool_calls [( :type "function"
;;                     :function ( :index 0 :name "get_temperature"
;;                                 :arguments (:city "New York")))
;;                   ( :type "function"
;;                     :function ( :index 1 :name "get_conditions"
;;                                 :arguments (:city "New York")))
;;                   ( :type "function"
;;                     :function ( :index 2 :name "get_temperature"
;;                                 :arguments (:city "London")))
;;                   ( :type "function"
;;                     :function ( :index 3 :name "get_conditions"
;;                                 :arguments (:city "London")))])
;;    ( :role "tool" :tool_name "get_temperature" :content "22°C")
;;    ( :role "tool" :tool_name "get_conditions" :content "Partly cloudy")
;;    ( :role "tool" :tool_name "get_temperature" :content "15°C")
;;    ( :role "tool" :tool_name "get_conditions" :content "Rainy")]
;;   :stream :json-false)

(ert-deftest gptel-test-handle-pre-tool ()
  "Test `gptel--handle-pre-tool' hook handling."
  (skip-unless (fboundp 'gptel--handle-pre-tool))
  (let* ((backend (alist-get 'openai gptel-test-backends))
         ;; Use mutable lists for tool calls to ensure side effects are visible
         (tool-call-1 (list :name "tool1" :args (list :arg 1)))
         (tool-call-2 (list :name "tool2" :args (list :arg 2)))
         (tool-use (list tool-call-1 tool-call-2))
         (info (list :backend backend
                     :tool-use tool-use
                     :model "test-model"
                     :data (list :messages [])
                     :buffer (current-buffer)))
         (fsm (gptel-make-fsm)))
    (setf (gptel-fsm-info fsm) info)

    ;; Case 1: Modify arguments
    (let ((injected-args nil)
          (gptel-pre-tool-call-functions
           (list (lambda (args)
                   (when (equal (plist-get args :name) "tool1")
                     (list :args '(:arg 100)))))))
      (cl-letf (((symbol-function 'gptel--inject-tool-call)
                 (lambda (_backend _data tool-call new-call)
                   (push (list (plist-get tool-call :name) (plist-get new-call :args))
                         injected-args))))
        (gptel--handle-pre-tool fsm)
        ;; Check that the args were updated in the tool-call object itself
        ;; (which is part of the info plist)
        (should (equal (plist-get (nth 0 (plist-get info :tool-use)) :args)
                       '(:arg 100)))
        ;; Check that inject-tool-args was called
        (should (equal injected-args '(("tool1" (:arg 100)))))))

    ;; Case 2: Block tool
    (let ((expected-result "<tool_call_error>\nTool tool2 blocked by user\n</tool_call_error>")
          (gptel-pre-tool-call-functions
           (list (lambda (args)
                   (when (equal (plist-get args :name) "tool2")
                     (list :block t))))))
      (cl-letf (((symbol-function 'gptel--process-tool-result)
                 (lambda (_fsm _spec tool-call result)
                   ;; Simulate side effect of gptel--process-tool-result
                   (plist-put tool-call :result result))))
        (gptel--handle-pre-tool fsm)
        ;; Verify that the tool-call in info is marked as blocked/errored
        (let ((tc2 (nth 1 (plist-get info :tool-use))))
          (should (plist-get tc2 :error))
          (should (equal (plist-get tc2 :result) expected-result)))))

    (let ((transition-state nil)
          (gptel-pre-tool-call-functions
           (list (lambda (_args)
                   (list :stop t :stop-reason "Aborted")))))
      (cl-letf (((symbol-function 'gptel--fsm-transition)
                 (lambda (_fsm state)
                   (setq transition-state state))))
        (gptel--handle-pre-tool fsm)
        (should (equal transition-state 'ERRS))
        (should (equal (plist-get info :stop-reason) "Aborted"))))

    ;; Case 4: Multiple hooks running in sequence
    (let* ((tool-call-1 (list :name "tool1" :args (list :arg 1)))
           (tool-call-2 (list :name "tool2" :args (list :arg 2)))
           (tool-use (list tool-call-1 tool-call-2))
           (injected-args nil)
           (gptel-pre-tool-call-functions
            (list
             (lambda (args)
               (when (equal (plist-get args :name) "tool1")
                 (list :args '(:step 1))))
             (lambda (args)
               (when (equal (plist-get args :name) "tool1")
                 (list :args '(:step 2))))
             (lambda (args)
               (when (equal (plist-get args :name) "tool2")
                 (list :args '(:step 3)))))))
      ;; Update info with fresh tools (previous cases modified the original tools)
      (plist-put info :tool-use tool-use)
      (cl-letf (((symbol-function 'gptel--inject-tool-call)
                 (lambda (_backend _data tool-call new-call)
                   (push (list (plist-get tool-call :name) (plist-get new-call :args))
                         injected-args))))
        (gptel--handle-pre-tool fsm)
        ;; Check that inject-tool-args was called 3 times with expected values
        (should (equal injected-args
                       '(("tool2" (:step 3))
                         ("tool1" (:step 2))
                         ("tool1" (:step 1)))))))))

(ert-deftest gptel-test-handle-post-tool ()
  "Test `gptel--handle-post-tool' hook handling."
  (skip-unless (fboundp 'gptel--handle-post-tool))
  (let* ((backend (alist-get 'openai gptel-test-backends))
         (tool-spec1 (gptel-make-tool
                      :name "tool1"
                      :func #'ignore
                      :description "Test tool1"
                      :args '(:num integer :str string)))
         (tool-spec2 (gptel-make-tool
                      :name "tool2"
                      :func #'ignore
                      :description "Test tool2"
                      :args '(:bool boolean)))
         (tool-call-1 (list :name "tool1" :args (list :num 1 :str "1") :result "result1"))
         (tool-call-2 (list :name "tool2" :args (list :bool t) :result "result2"))
         (tool-call-3 (list :name "tool1" :args (list :str "3" :num 3) :result "result3"))
         (tool-use (list tool-call-1 tool-call-2 tool-call-3))
         (tool-result-alist
          (list (list tool-spec1 (list :num 1 :str "1") "result1")
                (list tool-spec2 (list :bool t) "result3")
                (list tool-spec1 (list :num 3 :str "3") "result2"))))

    ;; Case 1: Modify result
    (let ((info (list :backend backend
                      :tool-use (copy-tree tool-use)
                      :tool-result (copy-tree tool-result-alist)
                      :model "test-model"
                      :data (list :messages [])
                      :buffer (current-buffer)))
          (fsm (gptel-make-fsm))
          (gptel-post-tool-call-functions
           (list (lambda (s) (when (and (equal (plist-get s :name) "tool1")
                                   (equal (plist-get (plist-get s :args) :str) "3"))
                          (list :result "modified-result1"))))))
      (setf (gptel-fsm-info fsm) info)

      (gptel--handle-post-tool fsm)
      ;; Check that result was updated in tool-call
      (should (equal (plist-get (nth 2 (plist-get info :tool-use)) :result)
                     "modified-result1"))
      ;; Check that result was updated in tool-result-alist
      (should (equal (caddr (nth 2 (plist-get info :tool-result)))
                     "modified-result1")))

    ;; Case 2: Block tool result
    (let ((info (list :backend backend
                      :tool-use (copy-tree tool-use)
                      :tool-result (copy-tree tool-result-alist)
                      :model "test-model"
                      :data (list :messages [])
                      :buffer (current-buffer)))
          (fsm (gptel-make-fsm))
          (expected-error "<tool_call_error>\nTool tool2 blocked by user\n</tool_call_error>")
          (gptel-post-tool-call-functions
           (list (lambda (args)
                   (when (equal (plist-get args :name) "tool2")
                     (list :block t))))))
      (setf (gptel-fsm-info fsm) info)
      (gptel--handle-post-tool fsm)

      ;; Check that tool-call is marked with error and blocked result
      (let ((tc2 (nth 1 (plist-get info :tool-use))))
        (should (plist-get tc2 :error))
        (should (equal (plist-get tc2 :result) expected-error)))
      ;; Check that tool-result-alist has the blocked result
      (should (equal (caddr (nth 1 (plist-get info :tool-result)))
                     expected-error)))

    ;; Case 3: Stop request
    (let ((info (list :backend backend
                      :tool-use (copy-tree tool-use)
                      :tool-result (copy-tree tool-result-alist)
                      :model "test-model"
                      :data (list :messages [])
                      :buffer (current-buffer)))
          (fsm (gptel-make-fsm))
          (transition-state nil)
          (gptel-post-tool-call-functions
           (list (lambda (_args)
                   (list :stop t :stop-reason "Post-tool stopped")))))

      (setf (gptel-fsm-info fsm) info)
      (cl-letf (((symbol-function 'gptel--fsm-transition)
                 (lambda (_fsm state)
                   (setq transition-state state))))
        (gptel--handle-post-tool fsm)
        (should (equal transition-state 'ERRS))
        (should (equal (plist-get info :stop-reason) "Post-tool stopped"))))

    ;; Case 4: Multiple hooks running in sequence
    (let* ((info (list :backend backend
                       :tool-use (copy-tree tool-use)
                       :tool-result (copy-tree tool-result-alist)
                       :model "test-model"
                       :data (list :messages [])
                       :buffer (current-buffer)))
           (fsm (gptel-make-fsm))
           (modified-results nil)
           (gptel-post-tool-call-functions
            (list
             (lambda (s)
               (when (equal (plist-get s :name) "tool1")
                 (let ((new-result (concat "hook1-" (plist-get s :result))))
                   (push (list (plist-get s :name) new-result) modified-results)
                   (list :result new-result))))
             (lambda (s)
               (when (equal (plist-get s :name) "tool1")
                 (let ((new-result (concat "hook2-" (plist-get s :result))))
                   (push (list (plist-get s :name) new-result) modified-results)
                   (list :result new-result))))
             (lambda (s)
               (when (equal (plist-get s :name) "tool2")
                 (let ((new-result (concat "hook3-" (plist-get s :result))))
                   (push (list (plist-get s :name) new-result) modified-results)
                   (list :result new-result)))))))

      (setf (gptel-fsm-info fsm) info)
      (gptel--handle-post-tool fsm)
      ;; Check that all three hooks ran (order is reverse due to push)
      (should (equal modified-results
                     '(("tool2" "hook3-result2")
                       ("tool1" "hook2-hook1-result3")
                       ("tool1" "hook2-hook1-result1")
                       ("tool1" "hook1-result3")
                       ("tool1" "hook1-result1"))))
      ;; Check final results in tool-calls
      (should (equal (plist-get (nth 0 (plist-get info :tool-use)) :result)
                     "hook2-hook1-result1"))
      (should (equal (plist-get (nth 1 (plist-get info :tool-use)) :result)
                     "hook3-result2"))
      (should (equal (plist-get (nth 2 (plist-get info :tool-use)) :result)
                     "hook2-hook1-result3"))
      ;; Check final results in tool-result-alist
      (should (equal (caddr (nth 0 (plist-get info :tool-result)))
                     "hook2-hook1-result1"))
      (should (equal (caddr (nth 1 (plist-get info :tool-result)))
                     "hook3-result2"))
      (should (equal (caddr (nth 2 (plist-get info :tool-result)))
                     "hook2-hook1-result3")))))

(ert-deftest gptel-test-inject-prompt ()
  "Test `gptel--inject-prompt' generic implementation."
  (skip-unless (fboundp 'gptel--inject-prompt))
  ;; Test default implementation with OpenAI backend
  (let* ((backend (alist-get 'openai gptel-test-backends))
         (msg1 '(:role "user" :content "msg1"))
         (msg2 '(:role "assistant" :content "msg2"))
         (msg3 '(:role "user" :content "msg3"))
         (new-msg '(:role "system" :content "new"))
         (base-data `(:messages [,msg1 ,msg2 ,msg3])))

    ;; Case 1: Append single message (default position nil)
    (let ((data (copy-tree base-data t)))
      (gptel--inject-prompt backend data new-msg)
      (should (equal (plist-get data :messages)
                     (vector msg1 msg2 msg3 new-msg))))

    ;; Case 2: Append list of messages
    (let ((data (copy-tree base-data t))
          (new-msgs (list new-msg msg1)))
      (gptel--inject-prompt backend data new-msgs)
      (should (equal (plist-get data :messages)
                     (vector msg1 msg2 msg3 new-msg msg1))))

    ;; Case 3: Insert at position 0
    (let ((data (copy-tree base-data t)))
      (gptel--inject-prompt backend data new-msg 0)
      (should (equal (plist-get data :messages)
                     (vector new-msg msg1 msg2 msg3))))

    ;; Case 4: Insert at position 1
    (let ((data (copy-tree base-data t)))
      (gptel--inject-prompt backend data new-msg 1)
      (should (equal (plist-get data :messages)
                     (vector msg1 new-msg msg2 msg3))))

    ;; Case 5: Insert at position -1 (before last element)
    ;; Logic check: (substring [1 2 3] 0 -1) -> [1 2]. (substring [1 2 3] -1) -> [3].
    ;; Result: [1 2 new 3]. So inserts before the last element.
    (let ((data (copy-tree base-data t)))
      (gptel--inject-prompt backend data new-msg -1)
      (should (equal (plist-get data :messages)
                     (vector msg1 msg2 new-msg msg3))))

    ;; Case 6: Insert at position -2
    ;; Logic check: (substring [1 2 3] 0 -2) -> [1]. (substring [1 2 3] -2) -> [2 3].
    ;; Result: [1 new 2 3].
    (let ((data (copy-tree base-data t)))
      (gptel--inject-prompt backend data new-msg -2)
      (should (equal (plist-get data :messages)
                     (vector msg1 new-msg msg2 msg3))))

    ;; Case 7: Empty initial messages
    (let ((data '(:messages [])))
      (gptel--inject-prompt backend data new-msg)
      (should (equal (plist-get data :messages)
                     (vector new-msg)))))

  ;; Test Gemini implementation
  (let* ((backend (alist-get 'gemini gptel-test-backends))
         (msg1 '(:role "user" :parts [(:text "msg1")]))
         (msg2 '(:role "model" :parts [(:text "msg2")]))
         (msg3 '(:role "user" :parts [(:text "msg3")]))
         (new-msg '(:role "user" :parts [(:text "new")]))
         (base-data `(:contents [,msg1 ,msg2 ,msg3])))

    ;; Case 1: Append single message (default position nil)
    (let ((data (copy-tree base-data t)))
      (gptel--inject-prompt backend data new-msg)
      (should (equal (plist-get data :contents)
                     (vector msg1 msg2 msg3 new-msg))))

    ;; Case 2: Append list of messages
    (let ((data (copy-tree base-data t))
          (new-msgs (list new-msg msg1)))
      (gptel--inject-prompt backend data new-msgs)
      (should (equal (plist-get data :contents)
                     (vector msg1 msg2 msg3 new-msg msg1))))

    ;; Case 3: Insert at position 0
    (let ((data (copy-tree base-data t)))
      (gptel--inject-prompt backend data new-msg 0)
      (should (equal (plist-get data :contents)
                     (vector new-msg msg1 msg2 msg3))))

    ;; Case 4: Insert at position -1
    (let ((data (copy-tree base-data t)))
      (gptel--inject-prompt backend data new-msg -1)
      (should (equal (plist-get data :contents)
                     (vector msg1 msg2 new-msg msg3))))))

;; Read an API output stream and construct the response and tool calls

;;; General

;;; Anthropic

(ert-deftest gptel-test-anthropic-tool-stream-parallel ()
  (let* ((backend (alist-get 'anthropic gptel-test-backends))
         (testinfo
          `(:backend ,backend
            :data (:messages [(:role "user"
                               :content
                               "My Emacs *scratch* buffer contains a list of directory names.  Create directories under \"/tmp/\" with each of these names.")])))
         (response (with-temp-buffer
                     (insert-file-contents "examples/anthropic-tool-stream-parallel.txt")
                     (goto-char (point-min))
                     (gptel-curl--parse-stream backend testinfo))))
    ;; text parsing: Is the return value of the parser correct?
    (should (string= response "\n\nI'll create these directories in /tmp/:"))
    ;; tool parsing: Is it capturing the tool calls in testinfo, the info plist?
    (let* ((tool-use (plist-get testinfo :tool-use)))
      (cl-loop for tool-call-result in ;note: prefix "toolu_" has been stripped from the ids
               '((:id "toolu_01Q7ptGyMTHtj8NTAu1q93qS" :name "make_directory" :input nil :args (:parent "/tmp" :name "testdir3"))
                 (:id "toolu_01Jqbxt5WYUt6RfpoBCHpA6X" :name "make_directory" :input nil :args (:parent "/tmp" :name "testdir2"))
                 (:id "toolu_01GwpAyin6URSPn7ZuGSjXKz" :name "make_directory" :input nil :args (:parent "/tmp" :name "testdir1")))
               for id = (plist-get tool-call-result :id)
               for tool-call-found = (cl-find-if (lambda (tool-use-each) (equal (plist-get tool-use-each :id) id)) tool-use)
               do (should (equal (plist-get tool-call-found :args)
                                 (plist-get tool-call-result :args)))))
    ;; Messages list update in prompts: Has it updated the list of prompts with the tool calls?
    (let* ((last-message (map-nested-elt testinfo '(:data :messages 1)))
           (role (plist-get last-message :role))
           (content (append (plist-get last-message :content) nil)))
      (should (equal role "assistant"))
      (dolist (tool-call '((:type "tool_use" :id "toolu_01Q7ptGyMTHtj8NTAu1q93qS" :name "make_directory" :input
                            (:parent "/tmp" :name "testdir3"))
                           (:type "tool_use" :id "toolu_01Jqbxt5WYUt6RfpoBCHpA6X" :name "make_directory" :input
                            (:parent "/tmp" :name "testdir2"))
                           (:type "tool_use" :id "toolu_01GwpAyin6URSPn7ZuGSjXKz" :name "make_directory" :input
                            (:parent "/tmp" :name "testdir1"))))
        (should (member tool-call content))))))


