;; -*- lexical-binding: t; -*-
(require 'ert)
(require 'gptel)
(require 'cl-generic)

;;; Methods for collecting data from HTTP logs
(cl-defgeneric gptel-test--read-response (backend &optional from to))
(cl-defmethod gptel-test--read-response ((_backend gptel-openai) &optional from to)
  (setq from (or from (point-min))
        to (or to (point-max)))
  (save-restriction
    (narrow-to-region from to)
    (goto-char from)
    (let ((strs) (json-object-type 'plist))
      (while (re-search-forward "^data: *" nil t)
        ;; (forward-char)
        (condition-case-unless-debug err
            (thread-first
              (json-parse-buffer :object-type 'plist)
              ;; (json-read)
              (map-nested-elt '(:choices 0 :delta :content))
              (push strs))
          (json-readtable-error strs)
          (:success strs)))
      (setq strs (delq nil (nreverse strs))))))

;;; Basic tests for markdown to org conversion
(let ((string-sequences
       '(("" "```" "cpp" "
" "#include" " <" "cstdio" ">

" "int" " main" "()" " {
" "   " " printf" "(\"" "``" "``" "`" "\n" "\");
" "   " " return" " " "0" ";
" "}
" "```")
        ("" "Here" " is" " a" " simple" " C" "++" " program" " that" " uses" " the" " `" "printf" "`" " function" " to" " print" " the" " string" " containing" " " "5" " back" "ticks" ":

" "```" "cpp" "
" "#include" " <" "iostream" ">

" "int" " main" "()" " {
" "   " " //" " Using" " printf" " to" " print" " " "5" " back" "ticks" "
" "   " " printf" "(\"" "``" "``" "`" "\n" "\");

" "   " " return" " " "0" ";
" "}
" "``" "`

" "In" " this" " code" " snippet" "," " `" "printf" "(\"" "``" "``" "`" "\n" "\");" "`" " is" " used" " to" " print" " the" " string" " \"" "``" "```" "\"" " followed" " by" " a" " newline" " character" ".")
        ("" "In" " the" " definition" " of" " the" " `" "struct" " json" "_parser" "`," " the" " line" " `" "L" "isp" "_Object" " *" "object" "_workspace" ";" "`" " declares" " a" " pointer" " named" " `" "object" "_workspace" "`" " of" " type" " `" "L" "isp" "_Object" "`.\n\n" "The" " aster" "isk" " (*)" " in" " `" "L" "isp" "_Object" " *" "object" "_workspace" ";" "`" " is" " the" " pointer" " ind" "irection" " operator" " in" " C" "." " It" " indicates" " that" " `" "object" "_workspace" "`" " is" " a" " pointer" " to" " an" " object" " of" " type" " `" "L" "isp" "_Object" "`." " This" " means" " that" " `" "object" "_workspace" "`" " will" " store" " the" " memory" " address" " (" "location" ")" " of" " a" " `" "L" "isp" "_Object" "`" " variable" " rather" " than" " storing" " the" " actual" " `" "L" "isp" "_Object" "`" " value" ".\n\n" "Therefore" "," " `" "object" "_workspace" "`" " will" " be" " used" " to" " point" " to" " or" " reference" " locations" " in" " memory" " where" " `" "L" "isp" "_Object" "`" " instances" " are" " stored" "." " This" " allows" " the" " `" "struct" " json" "_parser" "`" " to" " store" " and" " work" " with" " `" "L" "isp" "_Object" "`" " instances" " indirectly" " through" " pointers" ".")))
      (converted-sequences
       '("#+begin_src cpp
#include <cstdio>

int main() {
    printf(\"`````\n\");
    return 0;
}
#+end_src"
         "Here is a simple C++ program that uses the =printf= function to print the string containing 5 backticks:

#+begin_src cpp
#include <iostream>

int main() {
    // Using printf to print 5 backticks
    printf(\"`````\n\");

    return 0;
}
#+end_src

In this code snippet, =printf(\"`````\n\");= is used to print the string \"=\" followed by a newline character."
         "In the definition of the =struct json_parser=, the line =Lisp_Object *object_workspace;= declares a pointer named =object_workspace= of type =Lisp_Object=.

The asterisk (*) in =Lisp_Object *object_workspace;= is the pointer indirection operator in C. It indicates that =object_workspace= is a pointer to an object of type =Lisp_Object=. This means that =object_workspace= will store the memory address (location) of a =Lisp_Object= variable rather than storing the actual =Lisp_Object= value.

Therefore, =object_workspace= will be used to point to or reference locations in memory where =Lisp_Object= instances are stored. This allows the =struct json_parser= to store and work with =Lisp_Object= instances indirectly through pointers.")))
  (ert-deftest test--gptel--convert-markdown->org ()
    (cl-loop
     for input in string-sequences
     for output in converted-sequences
     do
     (should (string= (gptel--convert-markdown->org (apply #'concat input))
                      output))))

  (ert-deftest test--gptel--stream-convert-markdown->org ()
    (cl-loop
     for input in string-sequences
     for output in converted-sequences
     for func = (gptel--stream-convert-markdown->org)
     do
     (should
      (string= (apply #'concat (mapcar func input))
               output)))))

