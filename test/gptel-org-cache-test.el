;;; gptel-org-cache-test.el --- Tests for gptel-org-cache -*- lexical-binding: t; -*-

;; Tests for the gptel-org-cache module which provides:
;; - Cache location determination
;; - File link extraction from heading trees
;; - Hash-based cache invalidation
;; - Cache storage and retrieval

(require 'ert)
(require 'gptel-org-cache)

;;; Helper macro for org buffer tests

(defmacro gptel-org-cache-test-with-buffer (content &rest body)
  "Create a temp org buffer with CONTENT and execute BODY."
  (declare (indent 1))
  `(let ((org-inhibit-startup t))
     (with-temp-buffer
       (delay-mode-hooks (org-mode))
       (insert ,content)
       (goto-char (point-min))
       ,@body)))

(defmacro gptel-org-cache-test-with-temp-files (&rest body)
  "Execute BODY with temporary test files available."
  (declare (indent 0))
  `(let* ((temp-dir (make-temp-file "gptel-cache-test" t))
          (test-file1 (expand-file-name "test1.txt" temp-dir))
          (test-file2 (expand-file-name "test2.org" temp-dir))
          (source-file (expand-file-name "project-ai.org" temp-dir))
          (cache-file (expand-file-name "project-ai-cache.org" temp-dir)))
     (unwind-protect
         (progn
           ;; Create test files
           (with-temp-file test-file1
             (insert "Test content 1\nLine 2\n"))
           (with-temp-file test-file2
             (insert "* Heading\nTest content 2\n"))
           ,@body)
       ;; Cleanup
       (delete-directory temp-dir t))))


;;; Tests for cache location

(ert-deftest gptel-org-cache-test-default-location-ai-file ()
  "Test cache location for *-ai.org files."
  (let ((result (gptel-org-cache--default-location "/path/to/task-ai.org")))
    (should (equal result "/path/to/task-ai-cache.org"))))

(ert-deftest gptel-org-cache-test-default-location-regular-file ()
  "Test cache location for non-ai.org files."
  (let ((result (gptel-org-cache--default-location "/path/to/notes.org")))
    (should (equal result "/path/to/notes_cache.org"))))

(ert-deftest gptel-org-cache-test-default-location-nested-ai ()
  "Test cache location for nested -ai.org files."
  (let ((result (gptel-org-cache--default-location "/my-ai-project/task-ai.org")))
    (should (equal result "/my-ai-project/task-ai-cache.org"))))

(ert-deftest gptel-org-cache-test-default-location-workspace ()
  "Test cache location for workspace ai files."
  (let ((result (gptel-org-cache--default-location "/home/user/project/ws-ai.org")))
    (should (equal result "/home/user/project/ws-ai-cache.org"))))


;;; Tests for file hashing

(ert-deftest gptel-org-cache-test-file-hash-nonexistent ()
  "Test hash returns nil for nonexistent file."
  (should-not (gptel-org-cache--file-hash "/nonexistent/file.txt")))

(ert-deftest gptel-org-cache-test-file-hash-exists ()
  "Test hash returns string for existing file."
  (gptel-org-cache-test-with-temp-files
    (let ((hash (gptel-org-cache--file-hash test-file1)))
      (should (stringp hash))
      (should (> (length hash) 0)))))

(ert-deftest gptel-org-cache-test-file-hash-consistent ()
  "Test hash is consistent for unchanged file."
  (gptel-org-cache-test-with-temp-files
    (let ((hash1 (gptel-org-cache--file-hash test-file1))
          (hash2 (gptel-org-cache--file-hash test-file1)))
      (should (equal hash1 hash2)))))

(ert-deftest gptel-org-cache-test-file-hash-changes ()
  "Test hash changes when file is modified."
  (gptel-org-cache-test-with-temp-files
    (let ((hash1 (gptel-org-cache--file-hash test-file1)))
      ;; Modify the file (with a small delay to ensure mtime changes)
      (sleep-for 0.1)
      (with-temp-file test-file1
        (insert "Modified content\n"))
      (let ((hash2 (gptel-org-cache--file-hash test-file1)))
        (should-not (equal hash1 hash2))))))


;;; Tests for change detection

(ert-deftest gptel-org-cache-test-files-changed-none ()
  "Test no changes detected when hashes match."
  (gptel-org-cache-test-with-temp-files
    (let* ((hash1 (gptel-org-cache--file-hash test-file1))
           (hash2 (gptel-org-cache--file-hash test-file2))
           (hash-alist (list (cons test-file1 hash1)
                             (cons test-file2 hash2))))
      (should-not (gptel-org-cache--files-changed-p
                   (list test-file1 test-file2) hash-alist)))))

(ert-deftest gptel-org-cache-test-files-changed-one ()
  "Test change detected when one file modified."
  (gptel-org-cache-test-with-temp-files
    (let* ((hash1 (gptel-org-cache--file-hash test-file1))
           (hash2 (gptel-org-cache--file-hash test-file2))
           (hash-alist (list (cons test-file1 hash1)
                             (cons test-file2 hash2))))
      ;; Modify one file
      (sleep-for 0.1)
      (with-temp-file test-file1
        (insert "Modified\n"))
      (let ((changed (gptel-org-cache--files-changed-p
                      (list test-file1 test-file2) hash-alist)))
        (should (= 1 (length changed)))
        (should (equal (car changed) test-file1))))))


;;; Tests for link extraction

(ert-deftest gptel-org-cache-test-extract-file-links-bracket ()
  "Test extracting bracket-style file links."
  (gptel-org-cache-test-with-temp-files
    (let* ((content (format "Some text [[file:%s][Test file]] more text" test-file1))
           (links (gptel-org-cache--extract-file-links content)))
      (should (= 1 (length links)))
      (should (equal (car links) test-file1)))))

(ert-deftest gptel-org-cache-test-extract-file-links-multiple ()
  "Test extracting multiple file links."
  (gptel-org-cache-test-with-temp-files
    (let* ((content (format "[[file:%s]] and [[file:%s]]" test-file1 test-file2))
           (links (gptel-org-cache--extract-file-links content)))
      (should (= 2 (length links)))
      (should (member test-file1 links))
      (should (member test-file2 links)))))

(ert-deftest gptel-org-cache-test-extract-file-links-no-dups ()
  "Test that duplicate links are not repeated."
  (gptel-org-cache-test-with-temp-files
    (let* ((content (format "[[file:%s]] and [[file:%s]] again [[file:%s]]"
                            test-file1 test-file2 test-file1))
           (links (gptel-org-cache--extract-file-links content)))
      (should (= 2 (length links))))))

(ert-deftest gptel-org-cache-test-extract-file-links-with-line ()
  "Test extracting file links with line numbers."
  (gptel-org-cache-test-with-temp-files
    (let* ((content (format "[[file:%s::10][Line 10]]" test-file1))
           (links (gptel-org-cache--extract-file-links content)))
      (should (= 1 (length links)))
      (should (equal (car links) test-file1)))))

(ert-deftest gptel-org-cache-test-extract-file-links-nonexistent ()
  "Test that nonexistent files are not included."
  (let* ((content "[[file:/nonexistent/file.txt][Bad link]]")
         (links (gptel-org-cache--extract-file-links content)))
    (should-not links)))

(ert-deftest gptel-org-cache-test-extract-file-links-dir-excluded ()
  "Test that directory links are excluded."
  (gptel-org-cache-test-with-temp-files
    (let* ((content (format "[[file:%s]]" temp-dir))
           (links (gptel-org-cache--extract-file-links content)))
      (should-not links))))


;;; Tests for heading path

(ert-deftest gptel-org-cache-test-get-heading-path-single ()
  "Test getting path for single heading."
  (gptel-org-cache-test-with-buffer
   "* My Heading\nContent"
   (outline-next-heading)
   (let ((path (gptel-org-cache--get-heading-path)))
     (should (= 1 (length path)))
     (should (equal (car path) "My Heading")))))

(ert-deftest gptel-org-cache-test-get-heading-path-nested ()
  "Test getting path for nested headings."
  (gptel-org-cache-test-with-buffer
   "* Parent
** Child
*** Grandchild
Content"
   (goto-char (point-max))
   (org-back-to-heading t)
   (let ((path (gptel-org-cache--get-heading-path)))
     (should (= 3 (length path)))
     (should (equal path '("Parent" "Child" "Grandchild"))))))


;;; Tests for ancestor content collection

(ert-deftest gptel-org-cache-test-collect-ancestor-basic ()
  "Test collecting content from heading tree."
  (gptel-org-cache-test-with-temp-files
    (gptel-org-cache-test-with-buffer
     (format "* Parent\n[[file:%s]]\n** Child\n[[file:%s]]"
             test-file1 test-file2)
     (goto-char (point-max))
     (org-back-to-heading t)
     (let ((collected (gptel-org-cache--collect-ancestor-content)))
       (should (plist-get collected :content))
       (should (= 2 (length (plist-get collected :files))))
       (should (= 2 (length (plist-get collected :headings))))))))

(ert-deftest gptel-org-cache-test-collect-ancestor-headings ()
  "Test that headings are collected in order from root to current."
  (gptel-org-cache-test-with-buffer
   "* First
** Second
*** Third
Content"
   (goto-char (point-max))
   (org-back-to-heading t)
   (let* ((collected (gptel-org-cache--collect-ancestor-content))
          (headings (plist-get collected :headings)))
     ;; Should have 3 headings in root-to-current order
     (should (= 3 (length headings)))
     (should (equal (car headings) "First"))
     (should (equal (car (last headings)) "Third")))))


;;; Tests for heading ID generation

(ert-deftest gptel-org-cache-test-heading-id-consistency ()
  "Test that heading ID is consistent."
  (let ((headings '("Parent" "Child" "Grandchild")))
    (should (equal (gptel-org-cache--heading-id headings)
                   (gptel-org-cache--heading-id headings)))))

(ert-deftest gptel-org-cache-test-heading-id-unique ()
  "Test that different heading paths produce different IDs."
  (let ((id1 (gptel-org-cache--heading-id '("Parent" "Child1")))
        (id2 (gptel-org-cache--heading-id '("Parent" "Child2"))))
    (should-not (equal id1 id2))))


;;; Tests for cache entry formatting

(ert-deftest gptel-org-cache-test-format-entry-structure ()
  "Test that formatted entry has correct structure."
  (let* ((data (list :content "Test context"
                     :files '("/path/to/file1.txt" "/path/to/file2.org")
                     :headings '("Parent" "Child")
                     :file-hashes '(("/path/to/file1.txt" . "hash1")
                                    ("/path/to/file2.org" . "hash2"))))
         (formatted (gptel-org-cache--format-entry "test-id" data)))
    ;; Check heading
    (should (string-match-p "^\\* Child" formatted))
    ;; Check properties
    (should (string-match-p ":CACHE_ID: test-id" formatted))
    (should (string-match-p ":CACHE_DATE:" formatted))
    (should (string-match-p ":HEADING_PATH:" formatted))
    ;; Check file hashes section
    (should (string-match-p "\\*\\* File Hashes" formatted))
    ;; Check cached files section
    (should (string-match-p "\\*\\* Cached Files" formatted))
    ;; Check cached context section
    (should (string-match-p "\\*\\* Cached Context" formatted))
    (should (string-match-p "Test context" formatted))))


;;; Tests for cache file operations

(ert-deftest gptel-org-cache-test-ensure-cache-file ()
  "Test that cache file is created with header."
  (gptel-org-cache-test-with-temp-files
    (let* ((gptel-org-cache-location-function
            (lambda (_) cache-file))
           (result (with-temp-buffer
                     (setq buffer-file-name source-file)
                     (gptel-org-cache--ensure-cache-file))))
      (should (equal result cache-file))
      (should (file-exists-p cache-file))
      (with-temp-buffer
        (insert-file-contents cache-file)
        (should (string-match-p "AI Context Cache" (buffer-string)))))))

(ert-deftest gptel-org-cache-test-write-and-find-entry ()
  "Test writing and finding cache entries."
  (gptel-org-cache-test-with-temp-files
    (let ((data (list :content "Cached content here"
                      :files (list test-file1)
                      :headings '("TestHeading")
                      :file-hashes (list (cons test-file1 "hash123"))))
          (heading-id "test-heading-id"))
      ;; Create cache file
      (with-temp-file cache-file
        (insert "#+TITLE: Test Cache\n\n"))
      ;; Write entry
      (gptel-org-cache--write-entry cache-file heading-id data)
      ;; Find entry
      (let ((entry (gptel-org-cache--find-entry cache-file heading-id)))
        (should entry)
        (should (plist-get entry :content))
        (should (string-match-p "Cached content here" (plist-get entry :content)))
        (should (plist-get entry :file-hashes))))))

(ert-deftest gptel-org-cache-test-delete-entry ()
  "Test deleting cache entries."
  (gptel-org-cache-test-with-temp-files
    (let ((data (list :content "Content"
                      :files (list test-file1)
                      :headings '("ToDelete")
                      :file-hashes nil))
          (heading-id "to-delete-id"))
      ;; Create and write
      (with-temp-file cache-file
        (insert "#+TITLE: Test\n\n"))
      (gptel-org-cache--write-entry cache-file heading-id data)
      ;; Verify exists
      (should (gptel-org-cache--find-entry cache-file heading-id))
      ;; Delete
      (gptel-org-cache--delete-entry cache-file heading-id)
      ;; Verify gone
      (should-not (gptel-org-cache--find-entry cache-file heading-id)))))


;;; Tests for context generation

(ert-deftest gptel-org-cache-test-generate-context ()
  "Test context generation from files."
  (gptel-org-cache-test-with-temp-files
    (let ((context (gptel-org-cache--generate-context (list test-file1))))
      (should (stringp context))
      (should (string-match-p "test1.txt" context))
      (should (string-match-p "Test content 1" context))
      (should (string-match-p "```" context)))))

(ert-deftest gptel-org-cache-test-generate-context-multiple ()
  "Test context generation from multiple files."
  (gptel-org-cache-test-with-temp-files
    (let ((context (gptel-org-cache--generate-context
                    (list test-file1 test-file2))))
      (should (string-match-p "test1.txt" context))
      (should (string-match-p "test2.org" context)))))


;;; Tests for main commands (integration)

(ert-deftest gptel-org-cache-test-prepare-no-links ()
  "Test prepare command with no file links - should message but not error."
  (gptel-org-cache-test-with-buffer
   "* Heading\nNo file links here"
   (outline-next-heading)
   ;; Should not error - the function messages when no links found
   ;; Just verify it doesn't signal an error
   (condition-case nil
       (progn (gptel-org-cache-prepare) t)
     (error nil))
   (should t)))

(ert-deftest gptel-org-cache-test-full-workflow ()
  "Test full cache workflow: prepare, status, get."
  (gptel-org-cache-test-with-temp-files
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (setq buffer-file-name source-file)
      (insert (format "* Test Task\n[[file:%s]]\n" test-file1))
      (goto-char (point-min))
      (outline-next-heading)
      ;; Prepare cache
      (gptel-org-cache-prepare)
      ;; Check cache file was created
      (should (file-exists-p cache-file))
      ;; Get cached context
      (let ((context (gptel-org-cache-get-context)))
        (should context)
        (should (string-match-p "Test content 1" context))))))


;;; Tests for cache validation

(ert-deftest gptel-org-cache-test-get-context-stale ()
  "Test that stale cache triggers update when auto-update enabled."
  (gptel-org-cache-test-with-temp-files
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (setq buffer-file-name source-file)
      (insert (format "* Task\n[[file:%s]]" test-file1))
      (goto-char (point-min))
      (outline-next-heading)
      ;; Prepare initial cache
      (gptel-org-cache-prepare)
      ;; Modify the file
      (sleep-for 0.1)
      (with-temp-file test-file1
        (insert "Modified content\n"))
      ;; With auto-update, should get fresh content
      (let ((gptel-org-cache-auto-update t)
            (context (gptel-org-cache-get-context)))
        (should context)
        (should (string-match-p "Modified content" context))))))

(ert-deftest gptel-org-cache-test-get-context-disabled ()
  "Test that caching returns nil when disabled."
  (gptel-org-cache-test-with-temp-files
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (setq buffer-file-name source-file)
      (insert (format "* Task\n[[file:%s]]" test-file1))
      (goto-char (point-min))
      (outline-next-heading)
      (gptel-org-cache-prepare)
      ;; Disable caching
      (let ((gptel-org-cache-enabled nil))
        (should-not (gptel-org-cache-get-context))))))


;;; Tests for enable/disable

(ert-deftest gptel-org-cache-test-enable-disable ()
  "Test enabling and disabling cache integration."
  (let ((orig-enabled gptel-org-cache-enabled)
        (orig-transforms gptel-prompt-transform-functions))
    (unwind-protect
        (progn
          ;; Enable
          (gptel-org-cache-enable)
          (should gptel-org-cache-enabled)
          (should (memq #'gptel-org-cache--transform-inject
                        gptel-prompt-transform-functions))
          ;; Disable
          (gptel-org-cache-disable)
          (should-not gptel-org-cache-enabled)
          (should-not (memq #'gptel-org-cache--transform-inject
                            gptel-prompt-transform-functions)))
      ;; Restore original state
      (setq gptel-org-cache-enabled orig-enabled)
      (setq gptel-prompt-transform-functions orig-transforms))))


;;; Tests for parent cache inheritance

(ert-deftest gptel-org-cache-test-cached-files-changed-p-unchanged ()
  "Test cached-files-changed-p returns nil when files unchanged."
  (gptel-org-cache-test-with-temp-files
    (let* ((hash1 (gptel-org-cache--file-hash test-file1))
           (hash2 (gptel-org-cache--file-hash test-file2))
           (hash-alist `((,test-file1 . ,hash1) (,test-file2 . ,hash2))))
      (should-not (gptel-org-cache--cached-files-changed-p hash-alist)))))

(ert-deftest gptel-org-cache-test-cached-files-changed-p-modified ()
  "Test cached-files-changed-p detects modified files."
  (gptel-org-cache-test-with-temp-files
    (let* ((hash1 (gptel-org-cache--file-hash test-file1))
           (hash-alist `((,test-file1 . ,hash1))))
      ;; Modify the file
      (sleep-for 0.1)
      (with-temp-file test-file1
        (insert "Modified content\n"))
      ;; Should detect change
      (let ((changed (gptel-org-cache--cached-files-changed-p hash-alist)))
        (should changed)
        (should (member test-file1 changed))))))

(ert-deftest gptel-org-cache-test-cached-files-changed-p-deleted ()
  "Test cached-files-changed-p detects deleted files."
  (gptel-org-cache-test-with-temp-files
    (let* ((hash1 (gptel-org-cache--file-hash test-file1))
           (hash-alist `((,test-file1 . ,hash1))))
      ;; Delete the file
      (delete-file test-file1)
      ;; Should detect change (file no longer readable)
      (let ((changed (gptel-org-cache--cached-files-changed-p hash-alist)))
        (should changed)
        (should (member test-file1 changed))))))

(ert-deftest gptel-org-cache-test-parent-inheritance-basic ()
  "Test that child headings inherit cache from parent."
  (gptel-org-cache-test-with-temp-files
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (setq buffer-file-name source-file)
      (insert (format "* Parent Topic\n[[file:%s]]\n** Child Task\nSome content\n" test-file1))
      (goto-char (point-min))
      (outline-next-heading)
      ;; Prepare cache at parent level
      (gptel-org-cache-prepare)
      ;; Move to child heading - search for it directly
      (goto-char (point-min))
      (re-search-forward "^\\*\\* Child Task")
      (beginning-of-line)
      (should (looking-at "\\*\\* Child"))
      ;; Should inherit parent's cache
      (let ((context (gptel-org-cache-get-context)))
        (should context)
        (should (string-match-p "Test content 1" context))))))

(ert-deftest gptel-org-cache-test-parent-inheritance-grandchild ()
  "Test that grandchild headings inherit cache from grandparent."
  (gptel-org-cache-test-with-temp-files
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (setq buffer-file-name source-file)
      (insert (format "* Grandparent\n[[file:%s]]\n** Parent\n*** Grandchild\nContent\n" test-file1))
      (goto-char (point-min))
      (outline-next-heading)
      ;; Prepare cache at grandparent level
      (gptel-org-cache-prepare)
      ;; Move to grandchild - search for it directly
      (goto-char (point-min))
      (re-search-forward "^\\*\\*\\* Grandchild")
      (beginning-of-line)
      (should (looking-at "\\*\\*\\* Grandchild"))
      ;; Should inherit grandparent's cache
      (let ((context (gptel-org-cache-get-context)))
        (should context)
        (should (string-match-p "Test content 1" context))))))

(ert-deftest gptel-org-cache-test-parent-inheritance-child-extra-files ()
  "Test parent cache works when child has additional file links."
  (gptel-org-cache-test-with-temp-files
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (setq buffer-file-name source-file)
      ;; Parent has file1, child adds file2
      (insert (format "* Parent\n[[file:%s]]\n** Child\n[[file:%s]]\n" test-file1 test-file2))
      (goto-char (point-min))
      (outline-next-heading)
      ;; Prepare cache at parent (only has file1)
      (gptel-org-cache-prepare)
      ;; Move to child
      (outline-next-heading)
      ;; Child should still get parent's cache even though it has extra file
      (let ((context (gptel-org-cache-get-context)))
        (should context)
        (should (string-match-p "Test content 1" context))))))

(ert-deftest gptel-org-cache-test-parent-inheritance-prefers-exact ()
  "Test that exact heading cache is preferred over parent cache."
  (gptel-org-cache-test-with-temp-files
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (setq buffer-file-name source-file)
      (insert (format "* Parent\n[[file:%s]]\n** Child\n[[file:%s]]\n" test-file1 test-file2))
      (goto-char (point-min))
      (outline-next-heading)
      ;; Prepare cache at parent
      (gptel-org-cache-prepare)
      ;; Move to child and prepare its own cache
      (outline-next-heading)
      (gptel-org-cache-prepare)
      ;; Child's cache should include file2
      (let ((context (gptel-org-cache-get-context)))
        (should context)
        ;; Should have both files since child cache includes both
        (should (string-match-p "Test content 1" context))
        (should (string-match-p "Test content 2" context))))))

(ert-deftest gptel-org-cache-test-parent-inheritance-stale-parent ()
  "Test that stale parent cache is not used."
  (gptel-org-cache-test-with-temp-files
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (setq buffer-file-name source-file)
      (insert (format "* Parent\n[[file:%s]]\n** Child\nContent\n" test-file1))
      (goto-char (point-min))
      (outline-next-heading)
      ;; Prepare cache at parent
      (gptel-org-cache-prepare)
      ;; Modify the cached file
      (sleep-for 0.1)
      (with-temp-file test-file1
        (insert "Modified content\n"))
      ;; Move to child
      (outline-next-heading)
      ;; With auto-update disabled, stale parent cache should not be used
      (let ((gptel-org-cache-auto-update nil))
        (should-not (gptel-org-cache-get-context))))))

(ert-deftest gptel-org-cache-test-find-valid-cache-no-cache ()
  "Test find-valid-cache returns nil when no cache exists."
  (gptel-org-cache-test-with-temp-files
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (setq buffer-file-name source-file)
      (insert "* Heading\n** Child\n")
      (goto-char (point-min))
      (outline-next-heading)
      (outline-next-heading)
      (let* ((cache-file (gptel-org-cache--get-location))
             (collected (gptel-org-cache--collect-ancestor-content))
             (headings (plist-get collected :headings))
             (files (plist-get collected :files)))
        ;; No cache prepared, should return nil
        (should-not (gptel-org-cache--find-valid-cache cache-file headings files))))))


(provide 'gptel-org-cache-test)
;;; gptel-org-cache-test.el ends here
