(defpackage #:compiler-diagnostics
  (:use #:cl)
  (:export #:compiler-condition #:compiler-error #:redefinition #:reader-error
           #:compiler-warning #:compiler-style-warning #:compiler-note)
  (:export #:origin #:origin-file #:origin-position))
