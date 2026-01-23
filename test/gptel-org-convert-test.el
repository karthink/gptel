;; -*- lexical-binding: t; no-byte-compile: t; -*-
(require 'ert)
(require 'gptel)
(require 'gptel-org)
(require 'cl-generic)

(declare-function json-read "json" ())
(defvar json-object-type)

;;; Methods for collecting data from HTTP logs
(cl-defgeneric gptel-test--read-response (backend &optional from to))
(cl-defmethod gptel-test--read-response ((_backend gptel-openai) &optional from to)
  (setq from (or from (point-min))
        to (or to (point-max)))
  (save-restriction
    (narrow-to-region from to)
    (goto-char from)
    (let ((strs))
      (while (re-search-forward "^data: *" nil t)
        ;; (forward-char)
        (condition-case-unless-debug err
            (thread-first
              (gptel--json-read)
              (map-nested-elt '(:choices 0 :delta :content))
              (push strs))
          (error strs)
          (:success strs)))
      (setq strs (delq nil (nreverse strs))))))

(cl-defmethod gptel-test--read-response ((_backend gptel-gemini) &optional from to)
  (setq from (or from (point-min))
        to (or to (point-max)))
  (save-restriction
    (narrow-to-region from to)
    (goto-char from)
    (let ((strs))
      (while (re-search-forward "{" nil t)
        (backward-char)
        (thread-first
          (gptel--json-read)
          (map-nested-elt '(:candidates 0 :content :parts 0 :text))
          (push strs)))
      (setq strs (delq nil (nreverse strs))))))

(defun gptel-org--test-compare-org (md-list)
  "Given a list of markdown-formatted strings MD-LIST, covert it to
Org markup incrementally and display both in buffers."
  (let ((func (gptel--stream-convert-markdown->org (point-marker))))
    (with-current-buffer (get-buffer-create "gptel-org-test-md-input.md")
      (erase-buffer)
      (apply #'insert md-list)
      (display-buffer (current-buffer)))
    (with-current-buffer (get-buffer-create "gptel-org-test-org-output.org")
      (erase-buffer)
      (apply #'insert (mapcar func md-list))
      (display-buffer (current-buffer)))))

;; Oneshot converter test generation

(defmacro gptel-org--test-conversion (index md-list org-str)
  `(ert-deftest
       ,(intern (concat "gptel-org--test-conversion-"
                 (number-to-string index)))
       ()
     (should
      (string= (gptel--convert-markdown->org
                (apply #'concat ,md-list))
       ,org-str))))

;; Stream converter test generation

(defmacro gptel-org--test-stream-conversion (index md-list org-str)
  `(ert-deftest
       ,(intern (concat "gptel-org--test-stream-conversion-"
                 (number-to-string index)))
       ()
       (let ((func (gptel--stream-convert-markdown->org (point-marker))))
        (prog1
            (should
             (string= (apply #'concat (mapcar func ,md-list))
              ,org-str))
          (setq gptel-post-response-functions nil)))))

;; Tests

(let ((md '("" "```" "cpp" "
" "#include" " <" "cstdio" ">

" "int" " main" "()" " {
" "   " " printf" "(\"" "``" "``" "`" "\n" "\");
" "   " " return" " " "0" ";
" "}
" "```"))
      (org "#+begin_src cpp
#include <cstdio>

int main() {
    printf(\"`````\n\");
    return 0;
}
#+end_src"))
  (gptel-org--test-conversion 1 md org)
  (gptel-org--test-stream-conversion 1 md org))

(let ((md '("" "Here" " is" " a" " simple" " C" "++" " program" " that" " uses" " the" " `" "printf" "`" " function" " to" " print" " the" " string" " containing" " " "5" " back" "ticks" ":

" "```" "cpp" "
" "#include" " <" "iostream" ">

" "int" " main" "()" " {
" "   " " //" " Using" " printf" " to" " print" " " "5" " back" "ticks" "
" "   " " printf" "(\"" "``" "``" "`" "\n" "\");

" "   " " return" " " "0" ";
" "}
" "``" "`

" "In" " this" " code" " snippet" "," " `" "printf" "(\"" "``" "``" "`" "\n" "\");" "`" " is" " used" " to" " print" " the" " string" " \"" "``" "```" "\"" " followed" " by" " a" " newline" " character" "."))
      (org "Here is a simple C++ program that uses the =printf= function to print the string containing 5 backticks:

#+begin_src cpp
#include <iostream>

int main() {
    // Using printf to print 5 backticks
    printf(\"`````\n\");

    return 0;
}
#+end_src

In this code snippet, =printf(\"`````\n\");= is used to print the string \"=\" followed by a newline character."))
  (gptel-org--test-conversion 2 md org)
  (gptel-org--test-stream-conversion 2 md org))


(let ((md '("" "In" " the" " definition" " of" " the" " `" "struct" " json" "_parser" "`," " the" " line" " `" "L" "isp" "_Object" " *" "object" "_workspace" ";" "`" " declares" " a" " pointer" " named" " `" "object" "_workspace" "`" " of" " type" " `" "L" "isp" "_Object" "`.\n\n" "The" " aster" "isk" " (*)" " in" " `" "L" "isp" "_Object" " *" "object" "_workspace" ";" "`" " is" " the" " pointer" " ind" "irection" " operator" " in" " C" "." " It" " indicates" " that" " `" "object" "_workspace" "`" " is" " a" " pointer" " to" " an" " object" " of" " type" " `" "L" "isp" "_Object" "`." " This" " means" " that" " `" "object" "_workspace" "`" " will" " store" " the" " memory" " address" " (" "location" ")" " of" " a" " `" "L" "isp" "_Object" "`" " variable" " rather" " than" " storing" " the" " actual" " `" "L" "isp" "_Object" "`" " value" ".\n\n" "Therefore" "," " `" "object" "_workspace" "`" " will" " be" " used" " to" " point" " to" " or" " reference" " locations" " in" " memory" " where" " `" "L" "isp" "_Object" "`" " instances" " are" " stored" "." " This" " allows" " the" " `" "struct" " json" "_parser" "`" " to" " store" " and" " work" " with" " `" "L" "isp" "_Object" "`" " instances" " indirectly" " through" " pointers" "."))
      (org "In the definition of the =struct json_parser=, the line =Lisp_Object *object_workspace;= declares a pointer named =object_workspace= of type =Lisp_Object=.

The asterisk (*) in =Lisp_Object *object_workspace;= is the pointer indirection operator in C. It indicates that =object_workspace= is a pointer to an object of type =Lisp_Object=. This means that =object_workspace= will store the memory address (location) of a =Lisp_Object= variable rather than storing the actual =Lisp_Object= value.

Therefore, =object_workspace= will be used to point to or reference locations in memory where =Lisp_Object= instances are stored. This allows the =struct json_parser= to store and work with =Lisp_Object= instances indirectly through pointers."))
  (gptel-org--test-conversion 3 md org)
  (gptel-org--test-stream-conversion 3 md org))

(let ((md '("#" "# Advantages of"
            " Org-Mode: A Detailed Overview\n\nOrg-mode is a powerful and versatile Emacs extension that offers a wide range of features"
            " for organizing information, planning projects, and writing documents. Here's a detailed overview of its advantages:\n\n**Note Taking and Idea Management:**\n\n"
            "* **Unified platform:** Org-mode provides a single platform for capturing notes, ideas, and tasks, eliminating the need for multiple tools and ensuring everything is in one place.\n* **Flexibility:** Notes can be structured hierarchically"
            " using headlines and subheadings, allowing for easy organization and navigation.\n* **Linking and tags:** Link notes together for easy reference and connect them with tags for categorized browsing.\n* **Metadata:** Capture additional information like author, deadline, and"
            " priority for better organization and search.\n\n**Project Management and Planning:**\n\n* **Task management:** Create to-do lists with deadlines, priority levels, and tags for efficient task management.\n* **Gantt charts and time estimates:** Visualize project timelines and estimate time commitments for better planning and organization.\n* **Calendar integration:** Link tasks to your calendar for better integration and time management.\n* **"
            "Progress tracking:** Track the progress of tasks and projects with checkboxes and progress bars.\n\n**Writing and Document Creation:**\n\n* **Rich text formatting:** Org-mode supports a variety of text formatting options, including bold, italics, headings, and lists, for creating professional-looking documents.\n* **Exporting to various formats:** Easily export your notes and documents to various formats like PDF, HTML, LaTeX, and"
            " plain text.\n* **Beamer presentations:** Create Beamer presentations directly within Org-mode for academic or professional presentations.\n* **Source code blocks:** Include and highlight code blocks for easy reference and documentation.\n\n**Additional Features:**\n\n* **Org-Capture:** Quickly capture ideas, notes, and tasks from anywhere using keyboard shortcuts.\n* **Org-Agenda:** View and manage your tasks and appointments in a calendar-like format.\n* **Emacs Lisp scripting:** Add custom"
            " functionality and automate tasks with Emacs Lisp scripting.\n* *"
            "*Active development and community support:** Benefit from the active development and supportive community of Org-mode users.\n\n**Overall Advantages:**\n\n* **Increased productivity:** Organize your thoughts, tasks, and projects efficiently, leading to increased productivity.\n*"
            " **Improved focus:** Eliminate distractions and stay focused on the task at hand.\n* **Enhanced creativity:** Capture ideas quickly and easily, fostering creativity and innovation.\n* **Knowledge management:** Build a comprehensive knowledge base"
            " of notes, ideas, and projects for future reference.\n*"
            " **Personalized workflow:** Tailor Org-mode to your specific needs and preferences for a truly personalized workflow.\n\n**Limitations:**\n\n* **Learning curve:** While powerful, Org-mode has a steeper learning curve compared to simpler note-taking apps.\n* **Emacs dependency:** Org-mode requires Emacs, which may not be suitable for all users.\n* **Limited mobile support:** Mobile support for Org-mode is available but not as feature-rich as"
            " the desktop version.\n\n**Who should use Org-mode?**\n\n"
            "* Students\n* Researchers\n* Writers\n* Project managers\n* Anyone who wants to improve their personal organization and productivity\n\n**Conclusion:**\n\nOrg-mode is a powerful and versatile tool that can significantly increase your productivity and organization. Its flexibility, rich features, and active community make it an excellent choice for managing notes, projects, and documents. If you're looking for a way to streamline your workflow and unleash your creativity, Org-mode is definitely worth exploring."))
      (org "** Advantages of Org-Mode: A Detailed Overview\n\nOrg-mode is a powerful and versatile Emacs extension that offers a wide range of features for organizing information, planning projects, and writing documents. Here's a detailed overview of its advantages:\n\n*Note Taking and Idea Management:*\n\n- *Unified platform:* Org-mode provides a single platform for capturing notes, ideas, and tasks, eliminating the need for multiple tools and ensuring everything is in one place.\n- *Flexibility:* Notes can be structured hierarchically using headlines and subheadings, allowing for easy organization and navigation.\n- *Linking and tags:* Link notes together for easy reference and connect them with tags for categorized browsing.\n- *Metadata:* Capture additional information like author, deadline, and priority for better organization and search.\n\n*Project Management and Planning:*\n\n- *Task management:* Create to-do lists with deadlines, priority levels, and tags for efficient task management.\n- *Gantt charts and time estimates:* Visualize project timelines and estimate time commitments for better planning and organization.\n- *Calendar integration:* Link tasks to your calendar for better integration and time management.\n- *Progress tracking:* Track the progress of tasks and projects with checkboxes and progress bars.\n\n*Writing and Document Creation:*\n\n- *Rich text formatting:* Org-mode supports a variety of text formatting options, including bold, italics, headings, and lists, for creating professional-looking documents.\n- *Exporting to various formats:* Easily export your notes and documents to various formats like PDF, HTML, LaTeX, and plain text.\n- *Beamer presentations:* Create Beamer presentations directly within Org-mode for academic or professional presentations.\n- *Source code blocks:* Include and highlight code blocks for easy reference and documentation.\n\n*Additional Features:*\n\n- *Org-Capture:* Quickly capture ideas, notes, and tasks from anywhere using keyboard shortcuts.\n- *Org-Agenda:* View and manage your tasks and appointments in a calendar-like format.\n- *Emacs Lisp scripting:* Add custom functionality and automate tasks with Emacs Lisp scripting.\n- *Active development and community support:* Benefit from the active development and supportive community of Org-mode users.\n\n*Overall Advantages:*\n\n- *Increased productivity:* Organize your thoughts, tasks, and projects efficiently, leading to increased productivity.\n- *Improved focus:* Eliminate distractions and stay focused on the task at hand.\n- *Enhanced creativity:* Capture ideas quickly and easily, fostering creativity and innovation.\n- *Knowledge management:* Build a comprehensive knowledge base of notes, ideas, and projects for future reference.\n- *Personalized workflow:* Tailor Org-mode to your specific needs and preferences for a truly personalized workflow.\n\n*Limitations:*\n\n- *Learning curve:* While powerful, Org-mode has a steeper learning curve compared to simpler note-taking apps.\n- *Emacs dependency:* Org-mode requires Emacs, which may not be suitable for all users.\n- *Limited mobile support:* Mobile support for Org-mode is available but not as feature-rich as the desktop version.\n\n*Who should use Org-mode?*\n\n- Students\n- Researchers\n- Writers\n- Project managers\n- Anyone who wants to improve their personal organization and productivity\n\n*Conclusion:*\n\nOrg-mode is a powerful and versatile tool that can significantly increase your productivity and organization. Its flexibility, rich features, and active community make it an excellent choice for managing notes, projects, and documents. If you're looking for a way to streamline your workflow and unleash your creativity, Org-mode is definitely worth exploring."))
  (gptel-org--test-conversion 4 md org)
  (gptel-org--test-stream-conversion 4 md org))

(let ((md '("" "*" "Q" "lot" "*" " is" " a" " tool" " that" " provides" " a" " project" " management" " system" " for" " Common" " Lisp" "." " It" " offers" " functionality" " similar" " to" " that" " of" " Quick" "l" "isp" " but" " focuses" " on" " creating" " isolated" " project" " environments" "." " This" " allows" " you" " to" " specify" " dependencies" " for" " each" " project" " separately" "," " avoiding" " conflicts" " between" " different" " projects" " that" " may" " require" " different" " versions" " of" " the" " same" " library" ".\n\n" "Key" " features" " of" " *" "Q" "lot" "*" " include" ":\n\n" "1" "." " **" "Dependency" " Management" "**" ":" " Manage" " and" " install" " dependencies" " locally" " per" " project" ".\n" "2" "." " **" "Environment" " Isolation" "**" ":" " Each" " project" " can" " have" " its" " own" " set" " of" " libraries" "," " independent" " of" " other" " projects" ".\n" "3" "." " **" "Integration" " with" " Quick" "l" "isp" "**" ":" " While" " Q" "lot" " creates" " isolated" " environments" "," " it" " still" " uses" " Quick" "l" "isp" " to" " handle" " the" " actual" " library" " retrieval" " and" " installation" ".\n\n" "This" " makes" " Q" "lot" " useful" " for" " maintaining" " a" " clean" " workspace" "," " especially" " when" " working" " with" " multiple" " Common" " Lisp" " projects" "."))
      (org "/Qlot/ is a tool that provides a project management system for Common Lisp. It offers functionality similar to that of Quicklisp but focuses on creating isolated project environments. This allows you to specify dependencies for each project separately, avoiding conflicts between different projects that may require different versions of the same library.

Key features of /Qlot/ include:

1. *Dependency Management*: Manage and install dependencies locally per project.
2. *Environment Isolation*: Each project can have its own set of libraries, independent of other projects.
3. *Integration with Quicklisp*: While Qlot creates isolated environments, it still uses Quicklisp to handle the actual library retrieval and installation.

This makes Qlot useful for maintaining a clean workspace, especially when working with multiple Common Lisp projects."))
  (gptel-org--test-conversion 5 md org)
  (gptel-org--test-stream-conversion 5 md org))

(let ((md '("Okay"
            ", that's a fascinating and relevant topic, especially given the increasing accessibility of powerful personal"
            " computing hardware. To research the performance of local large language models (LLMs) on"
            " the M1 Mac Studio Ultra, I propose the following plan:\n\n1.  *Gather Search Terms:* Compile a list of relevant search terms to use with"
            " the =internet_search= tool. These terms should include specific LLMs (e.g., Llama, Alpaca, GPT-J), the M1"
            " Mac Studio Ultra, and performance metrics (e.g., inference speed, memory usage, quantization).\n2.  *Initial Search:* Perform initial searches using a broad range of search terms to identify relevant articles, blog posts, forum discussions"
            ", and GitHub repositories.\n3.  *Filter and Prioritize Results:* Analyze the search results to identify the most credible and informative sources. Prioritize results that include quantitative performance data (e.g., benchmarks, latency measurements).\n4"
            ".  *Deep Dive into Key Resources:* Use the =read_website= tool to extract and analyze the content of the most promising articles and blog posts. Summarize the key findings and performance metrics.\n5.  *GitHub Exploration:* If the search results reveal relevant GitHub repositories, examine the code and documentation to understand how LL"
            "Ms are being optimized for the **M1 Mac Studio Ultra**.\n6.  *Synthesize Findings:* Combine the information gathered from different sources to create a comprehensive overview of the performance of local LLMs on the M1 Mac Studio Ultra. Identify any trends, limitations, or areas for further *research*.\n\nDoes this plan"
            " sound good to you? If so, I'll start by generating some search terms."))
      (org "Okay, that's a fascinating and relevant topic, especially given the increasing accessibility of powerful personal computing hardware. To research the performance of local large language models (LLMs) on the M1 Mac Studio Ultra, I propose the following plan:

1.  /Gather Search Terms:/ Compile a list of relevant search terms to use with the =internet_search= tool. These terms should include specific LLMs (e.g., Llama, Alpaca, GPT-J), the M1 Mac Studio Ultra, and performance metrics (e.g., inference speed, memory usage, quantization).
2.  /Initial Search:/ Perform initial searches using a broad range of search terms to identify relevant articles, blog posts, forum discussions, and GitHub repositories.
3.  /Filter and Prioritize Results:/ Analyze the search results to identify the most credible and informative sources. Prioritize results that include quantitative performance data (e.g., benchmarks, latency measurements).
4.  /Deep Dive into Key Resources:/ Use the =read_website= tool to extract and analyze the content of the most promising articles and blog posts. Summarize the key findings and performance metrics.
5.  /GitHub Exploration:/ If the search results reveal relevant GitHub repositories, examine the code and documentation to understand how LLMs are being optimized for the *M1 Mac Studio Ultra*.
6.  /Synthesize Findings:/ Combine the information gathered from different sources to create a comprehensive overview of the performance of local LLMs on the M1 Mac Studio Ultra. Identify any trends, limitations, or areas for further /research/.

Does this plan sound good to you? If so, I'll start by generating some search terms."))
  (gptel-org--test-conversion 6 md org)
  (gptel-org--test-stream-conversion 6 md org))
