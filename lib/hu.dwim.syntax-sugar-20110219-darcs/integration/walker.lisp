;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.syntax-sugar)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(contextl:with-active-layers
            hu.dwim.walker:free-variable-reference-form
            hu.dwim.walker:ignore-undefined-references
            hu.dwim.walker:lambda-function-form
            hu.dwim.walker:lexical-variable-reference-form
            hu.dwim.walker:make-walk-environment
            hu.dwim.walker:map-ast
            hu.dwim.walker:name-of
            hu.dwim.walker:walk-form
            )
          :hu.dwim.syntax-sugar))

(define-syntax lambda-with-bang-args (&key dispatch-character sub-dispatch-character
                                           (start-character #\[) (end-character #\]))
  "Reader macro for simple lambdas.

This read macro reads exactly one form and serves to eliminate the 'boiler' plate text from such lambdas and write only the body of the lambda itself. If the form contains any references to variables named !1, !2, !3, !n etc. these are bound to the Nth parameter of the lambda.

Examples:

#L(foo) ==> (lambda () (foo)).
\[foo] ==> (lambda () (foo)).

#L(foo !1) ==> (lambda (!1) (foo !1))
\[foo !1] ==> (lambda (!1) (foo !1))

#L(foo (bar !2) !1) ==> (lambda (!1 !2) (foo (bar !2) !1))
\[foo (bar !2) !1] ==> (lambda (!1 !2) (foo (bar !2) !1))

All arguments are declared ignorable. So if there is a reference to an argument N but not to I < N then we still take N arguments, but the the unreferenced arguments at I < N positions are declared to be ignored. Examples (sans the ignorable declarations):

#L(foo !2) ==> (lambda (!1 !2) (foo !2))
\[foo !2] ==> (lambda (!1 !2) (foo !2))

We can specify exactly how many arguments to take by using the read macro's prefix parameter. NB: this is only neccessary if the lambda needs to accept N arguments but only uses N - 1. Example:

#2L(foo !1) ==> (lambda (!1 !2) (foo !1))

When #L forms are nested (which is not advised to keep readability and clarity), !X variables are bound to the innermost form. Example:

#L#L(+ !1 !2) ==> (lambda () (lambda (!1 !2) (+ !1 !2)))

returns a function that takes no arguments and returns a function that adds its two arguments."
  (when (and (or dispatch-character sub-dispatch-character)
             (or start-character end-character))
    (error "You may either install this syntax on a dispatching macro character or on start/end characters"))
  (assert (not (xor dispatch-character sub-dispatch-character)))
  (assert (not (xor start-character end-character)))
  (unless (or dispatch-character start-character)
    (error "You must specify on which character(s) the lambda-with-bang-args syntax should be enabled!"))
  (if dispatch-character
      (progn
        (unless (get-macro-character dispatch-character)
          (make-dispatch-macro-character dispatch-character))
        (set-dispatch-macro-character dispatch-character sub-dispatch-character
                                      (make-lambda-with-bang-args-reader nil)))
      (set-macro-character start-character
                           (make-lambda-with-bang-args-reader end-character)
                           t *readtable*)))

(define-syntax sharp-l ()
  "Enables the LAMBDA-WITH-BANG-ARGS syntax on #L(+ !1 !2)"
  (set-lambda-with-bang-args-syntax-in-readtable
   :dispatch-character #\#
   :sub-dispatch-character #\L
   :start-character nil
   :end-character nil))

(defun make-lambda-with-bang-args-reader (end-character)
  (if end-character
      (named-lambda lambda-with-bang-args-reader (stream char)
        (declare (ignore char))
        (bind ((*toplevel-readtable* (or *toplevel-readtable* *readtable*)))
          (with-local-readtable
            (set-syntax-from-char end-character #\) *readtable*)
            (bind ((body (read-delimited-list end-character stream t)))
              `(lambda-with-bang-args-expander ,(package-name *package*) ,body nil)))))
      (named-lambda lambda-with-bang-args-reader (stream subchar numeric-arg)
        (declare (ignore subchar))
        (bind ((*toplevel-readtable* (or *toplevel-readtable* *readtable*))
               (body (read stream t nil t)))
          `(lambda-with-bang-args-expander ,(package-name *package*) ,body ,numeric-arg)))))

(defmacro lambda-with-bang-args-expander (package-designator body min-args &environment env)
  (bind ((package package-designator))
    (unless (packagep package)
      (setf package (find-package package)))
    (unless package
      (error "Could not find package ~S?!" package-designator))
    (flet ((make-bang-arg (package number)
             (intern (format nil "!~D" number) package)))
      (bind ((form body)
             (lambda-args (loop
                             :for i :upfrom 1 :upto (max (or min-args 0)
                                                         (find-highest-bang-var form env))
                             :collect (make-bang-arg package i))))
        `(lambda ,lambda-args
           ,@(when lambda-args
                   `((declare (ignorable ,@lambda-args))))
           ,form)))))

(defun find-highest-bang-var (form env)
  (with-active-layers (ignore-undefined-references)
    (flet ((bang-var? (form)
             (and (starts-with #\! (symbol-name form) :test #'char=)
                  (parse-integer (subseq (symbol-name form) 1) :junk-allowed t)))
           (collect-variable-references (top-form)
             (let ((result (list)))
               (map-ast (lambda (form)
                          (when (typep form '(or free-variable-reference-form
                                                 lexical-variable-reference-form))
                            (push form result))
                          (if (typep form 'lambda-function-form)
                              nil       ; don't descent any deeper
                              form))
                        top-form)
               result)))
      (or (loop
             :for var-form :in (collect-variable-references
                                (walk-form form :environment (make-walk-environment env)))
             :for var = (name-of var-form)
             :when (bang-var? var)
             :maximize (bang-var? var))
          0))))
