(in-package #:compiler-diagnostics)

(deftype compiler-condition ()
  '(or reader-error compiler-error redefinition
    compiler-warning compiler-style-warning compiler-note))

(deftype compiler-error ()
  #+sbcl '(or error sb-c:compiler-error)
  #-sbcl 'error)

(deftype redefinition ()
  #+sbcl 'sb-kernel:redefinition-warning
  #+clasp 'cmp:redefined-function-warning
  #-(or sbcl clasp) nil)

(deftype compiler-warning ()
  'warning)

(deftype compiler-style-warning ()
  'style-warning)

(deftype compiler-note ()
  #+sbcl 'sb-ext:compiler-note
  #+clasp 'ext:compiler-note
  #-(or sbcl clasp) nil)

;;; Source location stuff.
(defgeneric origin-file (origin)
  (:method (origin)
    (declare (ignore origin))
    nil))
(defgeneric origin-position (origin)
  (:method (origin)
    (declare (ignore origin))
    nil))

;;; A source location. The implementation may have additional source location classes.
;;; We use this one for cases where it doesn't.
(defclass %origin ()
  (;; The pathname or namestring of the offending file, or NIL.
   (%file :initarg :file :reader origin-file :type (or string pathname null))
   ;; The file-position within the file, or NIL.
   (%position :initarg :position :reader origin-position :type (or null (integer 0)))))

;;; Get the source location for a condition, or NIL if not available.
;;; May only work from within a handler for the condition in question, before unwinding.
;;; (i.e. handler-case may break it)
(defgeneric origin (condition)
  (:method (origin)
    (declare (ignore origin))
    nil))

(defmethod origin ((condition reader-error))
  (let* ((stream (stream-error-stream condition))
         (path (ignore-errors (pathname stream))))
    (if path
        (make-instance '%origin :file path :position (file-position stream))
        nil)))

#+sbcl
(defmethod origin ((condition condition)) (sb-c::find-error-context nil))
#+clasp
(setf (fdefinition 'origin) #'cmp:compiler-condition-origin)

#+sbcl
(defmethod origin-file ((context sb-c::compiler-error-context))
  (sb-c::compiler-error-context-file-name context))
#+sbcl
(defmethod origin-position ((context sb-c::compiler-error-context))
  ;; Swank actually has an entire 250 line file dedicated to getting a file position
  ;; out of an error context. Good god. This is simpler and apparently worse.
  (sb-c::compiler-error-context-file-position context))

#+clasp
(defmethod origin-file ((source core:source-pos-info))
  (sys:file-scope-pathname (sys:file-scope source)))
#+clasp
(defmethod origin-position ((source core:source-pos-info))
  (sys:source-pos-info-filepos source))
#+clasp
(defmethod origin-file ((source cons)) (origin-file (car source)))
#+clasp
(defmethod origin-position ((source cons)) (origin-position (car source)))
