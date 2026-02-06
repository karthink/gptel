;;; gptel-org-tasks-test.el --- Tests for gptel-org-tasks -*- lexical-binding: t; -*-

;; Tests for the gptel-org-tasks module which provides:
;; - Model profiles (mapping tag names to backend/model configurations)
;; - Auto state transition (AI-DO -> AI-DOING when sending)
;; - Tag-based model selection

(require 'ert)
(require 'gptel)
(require 'gptel-org-tasks)

;;; Helper macro for org buffer tests

(defmacro gptel-org-tasks-test-with-buffer (content &rest body)
  "Create a temp org buffer with CONTENT and execute BODY.
Sets up gptel-org-tasks-mode and related variables."
  (declare (indent 1))
  `(let ((org-inhibit-startup t)
         (gptel-org-tasks-todo-keyword "AI-DO")
         (gptel-org-tasks-doing-keyword "AI-DOING")
         (gptel-org-tasks-canceled-keyword "CANCELED")
         (gptel-org-tasks-apply-profile-on-send t)
         (gptel-org-tasks-change-state-on-send t)
         (org-todo-keywords '((sequence "AI-DO(a)" "AI-DOING(i)" "|" "AI-DONE(d)")))
         (gptel-org-tasks--profiles nil))  ; Start with clean profiles
     (with-temp-buffer
       (delay-mode-hooks (org-mode))
       (insert ,content)
       (goto-char (point-min))
       ,@body)))

;;; Tests for profile management

(ert-deftest gptel-org-tasks-test-define-profile ()
  "Test defining a model profile."
  (gptel-org-tasks-test-with-buffer ""
    (gptel-org-tasks-define-profile 'test-profile
      :backend "Claude"
      :model 'claude-test
      :description "Test profile")
    (let ((profile (gptel-org-tasks-get-profile 'test-profile)))
      (should profile)
      (should (equal (plist-get profile :backend) "Claude"))
      (should (equal (plist-get profile :model) 'claude-test))
      (should (equal (plist-get profile :description) "Test profile")))))

(ert-deftest gptel-org-tasks-test-define-profile-update ()
  "Test that redefining a profile updates it."
  (gptel-org-tasks-test-with-buffer ""
    (gptel-org-tasks-define-profile 'test-profile
      :backend "Claude"
      :model 'model-v1)
    (gptel-org-tasks-define-profile 'test-profile
      :backend "Claude"
      :model 'model-v2)
    (let ((profile (gptel-org-tasks-get-profile 'test-profile)))
      (should (equal (plist-get profile :model) 'model-v2)))))

(ert-deftest gptel-org-tasks-test-define-profile-requires-backend ()
  "Test that profile definition requires :backend."
  (gptel-org-tasks-test-with-buffer ""
    (should-error
     (gptel-org-tasks-define-profile 'bad-profile
       :model 'some-model)
     :type 'error)))

(ert-deftest gptel-org-tasks-test-define-profile-requires-model ()
  "Test that profile definition requires :model."
  (gptel-org-tasks-test-with-buffer ""
    (should-error
     (gptel-org-tasks-define-profile 'bad-profile
       :backend "Claude")
     :type 'error)))

(ert-deftest gptel-org-tasks-test-get-profile-nonexistent ()
  "Test getting a nonexistent profile returns nil."
  (gptel-org-tasks-test-with-buffer ""
    (should-not (gptel-org-tasks-get-profile 'nonexistent))))

(ert-deftest gptel-org-tasks-test-list-profiles ()
  "Test listing all profiles."
  (gptel-org-tasks-test-with-buffer ""
    (gptel-org-tasks-define-profile 'profile-a
      :backend "Claude"
      :model 'model-a)
    (gptel-org-tasks-define-profile 'profile-b
      :backend "OpenAI"
      :model 'model-b)
    (let ((profiles (gptel-org-tasks-list-profiles)))
      (should (memq 'profile-a profiles))
      (should (memq 'profile-b profiles)))))

;;; Tests for task info retrieval

(ert-deftest gptel-org-tasks-test-get-task-info ()
  "Test retrieving task information from a heading."
  (gptel-org-tasks-test-with-buffer
   "* AI-DO Implement feature :haiku:\nDescription"
   (org-back-to-heading t)
   (let ((info (gptel-org-tasks--get-task-info)))
     (should (equal (plist-get info :todo-state) "AI-DO"))
     (should (member "haiku" (plist-get info :tags)))
     (should (equal (plist-get info :heading) "Implement feature")))))

(ert-deftest gptel-org-tasks-test-get-task-info-nested ()
  "Test retrieving task info from nested heading."
  (gptel-org-tasks-test-with-buffer
   "* Project\n** AI-DO Sub-task :sonnet:\nWork"
   ;; Define sonnet profile so it can be found
   (gptel-org-tasks-define-profile 'sonnet
     :backend "Claude"
     :model 'claude-sonnet)
   (search-forward "Sub-task")
   (let ((info (gptel-org-tasks--get-task-info)))
     (should (equal (plist-get info :todo-state) "AI-DO"))
     (should (equal (plist-get info :profile-tag) 'sonnet)))))

(ert-deftest gptel-org-tasks-test-get-task-info-no-profile-tag ()
  "Test task info when no profile tag is present."
  (gptel-org-tasks-test-with-buffer
   "* AI-DO Task without profile\nDescription"
   (org-back-to-heading t)
   (gptel-org-tasks-define-profile 'haiku
     :backend "Claude"
     :model 'claude-haiku)
   (let ((info (gptel-org-tasks--get-task-info)))
     (should (equal (plist-get info :todo-state) "AI-DO"))
     (should-not (plist-get info :profile-tag)))))

(ert-deftest gptel-org-tasks-test-get-task-info-with-matching-tag ()
  "Test task info finds matching profile tag."
  (gptel-org-tasks-test-with-buffer
   "* AI-DO Task :haiku:work:\nDescription"
   (org-back-to-heading t)
   (gptel-org-tasks-define-profile 'haiku
     :backend "Claude"
     :model 'claude-haiku)
   (let ((info (gptel-org-tasks--get-task-info)))
     (should (equal (plist-get info :profile-tag) 'haiku)))))

;;; Tests for state transitions

(ert-deftest gptel-org-tasks-test-change-todo-state ()
  "Test changing TODO state programmatically."
  (gptel-org-tasks-test-with-buffer
   "* AI-DO Test task\nDescription"
   (org-back-to-heading t)
   (gptel-org-tasks--change-todo-state "AI-DOING")
   (should (equal (org-get-todo-state) "AI-DOING"))))

;;; Tests for model alias detection

(ert-deftest gptel-org-tasks-test-model-alias-as-profile ()
  "Test that model aliases with :model-id work as implicit profiles."
  (gptel-org-tasks-test-with-buffer ""
    ;; Create a mock model alias (simulating what gptel-anthropic does)
    (put 'test-alias :model-id "actual-model-id")
    (put 'test-alias :description "Test alias")
    ;; Register a backend that has this model
    (let ((gptel--known-backends
           (list (cons "TestBackend"
                       (gptel--make-openai
                        :name "TestBackend"
                        :host "test.example.com"
                        :models '(test-alias))))))
      (let ((profile (gptel-org-tasks-get-profile 'test-alias)))
        (should profile)
        (should (equal (plist-get profile :backend) "TestBackend"))
        (should (equal (plist-get profile :model) 'test-alias))))
    ;; Cleanup
    (put 'test-alias :model-id nil)
    (put 'test-alias :description nil)))

(provide 'gptel-org-tasks-test)
;;; gptel-org-tasks-test.el ends here
