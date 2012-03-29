;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.quasi-quote.test)

(defsuite* (test/bivalent :in test))

(def special-variable *test-bivalent-stream*)

(def function setup-readtable-for-bivalent-test (inline?)
  (enable-quasi-quoted-bivalent-to-binary-emitting-form-syntax
   '*test-bivalent-stream*
   :with-inline-emitting inline?))

(def syntax-test-definer bivalent-test
  (:test-function   test-bivalent-emitting-forms
   :readtable-setup (setup-readtable-for-bivalent-test #f)))

(def function read-from-string-with-bivalent-syntax (string &optional (with-inline-emitting #f))
  (with-local-readtable
    (setup-readtable-for-bivalent-test with-inline-emitting)
    (read-from-string string)))

(def function pprint-bivalent (string &optional (with-inline-emitting #f))
  (pprint (macroexpand (read-from-string-with-string-syntax string :with-inline-emitting with-inline-emitting))))

(def function test-bivalent-emitting-forms (expected ast)
  (bind ((lambda-form `(lambda ()
                         (with-output-to-sequence (*test-bivalent-stream* :external-format :utf-8)
                           (emit ,ast)))))
    ;;(print (macroexpand-all lambda-form))
    (is (equalp expected
                (funcall (compile nil lambda-form))))))

(def bivalent-test test/bivalent/simple ()
  ;; binary
  (#(1 2)
   "`biv(1 2)")

  (#(1 2 3 4)
    "`biv((1 2)
          (3 4))")

  (#(1 2 3 4 5 6 7 8)
    "`biv(1 2
          (3 4)
          (5 6)
          7 8)")

  ;; string
  (#(49 32 50)
    ｢`biv("1 2")｣)

  (#(49 32 50 32 51 32 52)
    ｢`biv("1 2"
          " 3 4")｣)

  (#(49 32 50 32 51 32 52 32 53 32 54 32 55 32 56)
    ｢`biv("1 2"
          (" 3 4" " 5 6")
          " 7 8")｣)

  ;; bivalent
  (#(1 2 49 32 50)
    ｢`biv((1 2) "1 2")｣)

  (#(1 2 3 4 53 32 54 32 55 32 56)
    ｢`biv((1 2)
          ((3 4)
           "5 6")
          " 7 8")｣))

(def bivalent-test test/bivalent/unquote ()
  (#(1 2 3 4 32 53 32 54)
    ｢`biv((1 2)
          ,#(3 4)
          " 5 6")｣)

  (#(1 2 32 51 32 52 5 6 32 55 32 56 9 10 32 49 49 32 49 50)
    ｢`biv(1 2
            ,(list
              " 3 4"
              `biv(5 6 " 7 8")
              #(9 10))
            " 11 12")｣)

  (#(1 2 32 51 32 52 5 6 7 8 32 57 32 49 48 32 49 49 32 49 50 13 14 32 49 53 32 49 54)
    ｢`biv(1 2
            ,(list
              " 3 4"
              `biv(5 6
                     ,(list #(7 8) " 9 10")
                     " 11 12")
              #(13 14))
            " 15 16")｣))

(def bivalent-test test/bivalent/reverse ()
  (#(1 2 32 51 32 52 5 6 32 55 32 56)
   ｢`biv(1 2
           ,(reverse
             (list `biv(5 6) `biv(" 3 4")))
           " 7 8")｣))
