;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

#+xcvb (module (:depends-on ("package" "base/utils")))

(in-package :fare-utils)

(def*generic call-with-output (x thunk)
  (:documentation
   "Calls FUN with an actual stream argument, behaving like FORMAT with respect to stream'ing:
If OBJ is a stream, use it as the stream.
If OBJ is NIL, use a STRING-OUTPUT-STREAM as the stream, and return the resulting string.
If OBJ is T, use *STANDARD-OUTPUT* as the stream.
If OBJ is a string with a fill-pointer, use it as a string-output-stream.
Otherwise, signal an error.")
  (:method ((x null) thunk)
    (with-output-to-string (s) (funcall thunk s)))
  (:method ((x (eql t)) thunk)
    (funcall thunk *standard-output*) nil)
  #-genera
  (:method ((x stream) thunk)
    (funcall thunk x) nil)
  (:method ((x string) thunk)
    (assert (fill-pointer x))
    (with-output-to-string (s x) (funcall thunk s)))
  (:method (x thunk)
    (declare (ignorable thunk))
    (cond
      #+genera
      ((typep x 'stream) (funcall thunk x) nil)
      (t (error "not a valid stream designator ~S" x)))))

(def*macro with-output ((x &optional (value x)) &body body)
  `(call-with-output ,value #'(lambda (,x) ,@body)))

;;; Probably something similar for with call-with-input...

(def*parameter *standard-readtable* (copy-readtable nil))
(def*parameter *safe-package* :cl)

(def*fun safe-read (&optional s (eof-error-p t) eof-value)
  (with-standard-io-syntax
    (let ((*read-eval* nil)
          (*read-default-float-format* 'single-float)
          (*readtable* *standard-readtable*)
          (*package* (find-package *safe-package*)))
      (read-preserving-whitespace s eof-error-p eof-value))))

(def*fun safe-write (x &rest r)
  (with-standard-io-syntax
    (let ((*read-eval* nil)
          (*read-default-float-format* 'single-float)
          (*print-readably* nil)
          (*print-pretty* nil)
          (*print-circle* t)
          (*package* (find-package *safe-package*)))
      (apply #'write x r))))

(defun call-with-user-output-file (f fun)
  (if (equal f "-")
    (funcall fun *standard-output*)
    (with-open-file (o f :direction :output :if-exists :supersede)
      (funcall fun o))))

(def*macro with-user-output-file ((s f) &body body)
  `(call-with-user-output-file ,f #'(lambda (,s) ,@body)))
