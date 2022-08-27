This system is intended to provide a portable interface to conditions signaled by Common Lisp compilers. At the moment it is very much a work in progress, and only SBCL and Clasp have any support.

# Example usage

```lisp
;;; Say test.lisp contains (defun a () (+ 'a)).

(defvar *warning-locations* nil)

(handler-bind
    ((compiler-diagnostics:compiler-warning
       (lambda (w)
         (let ((origin (compiler-diagnostics:origin w)))
           (push (cons (compiler-diagnostics:origin-file origin)
                       (compiler-diagnostics:origin-position origin))
                 *warning-locations*)))))
  (compile-file "test.lisp"))

*warning-locations* => ((#p"test.lisp" . 17)) ; or so
```

# API

The `compiler-condition`, `compiler-error`, `reader-error`, `redefinition`, `compiler-warning`, `compiler-style-warning`, and `compiler-note` condition types are defined. When compiling, you can handle them (with `handler-bind` - `handler-case` may not work, as some implementations drop information after unwinding from the compiler). Given a compiler condition, the `origin` function gets the source location it originated from. Given an origin, `origin-file` and `origin-position` return the file and file position it indicates, respectively. Any of these three functions may return `nil` if no information is accessible.
