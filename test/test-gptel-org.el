;; -*- lexical-binding: t; -*-
(require 'ert)
(require 'gptel)

(let ((string-sequence
       '("" "```" "cpp" "
" "#include" " <" "cstdio" ">

" "int" " main" "()" " {
" "   " " printf" "(\"" "``" "``" "`" "\n" "\");
" "   " " return" " " "0" ";
" "}
" "```"))
      (converted-sequence
       "#+begin_src cpp
#include <cstdio>

int main() {
    printf(\"`````\n\");
    return 0;
}
#+end_src"))
  (ert-deftest test--gptel--convert-markdown->org ()
    (should (string= (gptel--convert-markdown->org (apply #'concat string-sequence))
                     converted-sequence)))

  (ert-deftest test--gptel--stream-convert-markdown->org ()
    (let ((func (gptel--stream-convert-markdown->org)))
      (should
       (string= (apply #'concat (mapcar func string-sequence))
                converted-sequence)))))

