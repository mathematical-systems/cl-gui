;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.quasi-quote.test)

(defsuite* (test/string :in test))

(def special-variable *test-string-stream*)

(def function setup-readtable-for-string-test (&key with-inline-emitting binary)
  (if binary
      (enable-quasi-quoted-string-to-binary-emitting-form-syntax
       '*test-string-stream*
       :encoding :utf-8
       :with-inline-emitting with-inline-emitting)
      (enable-quasi-quoted-string-to-string-emitting-form-syntax
       '*test-string-stream*
       :with-inline-emitting with-inline-emitting)))

(def syntax-test-definer string-test
  (:test-function   test-string-emitting-forms
   :readtable-setup (setup-readtable-for-string-test :with-inline-emitting #f))
  (:test-function   test-string-emitting-forms/binary
   :readtable-setup (setup-readtable-for-string-test :with-inline-emitting #f :binary #t)))

(def syntax-test-definer string-test/inline
  (:test-function   test-string-emitting-forms
   :readtable-setup (setup-readtable-for-string-test :with-inline-emitting #f))
  (:test-function   test-string-emitting-forms/binary
   :readtable-setup (setup-readtable-for-string-test :with-inline-emitting #f :binary #t))
  (:test-function   test-string-emitting-forms
   :readtable-setup (setup-readtable-for-string-test :with-inline-emitting #t))
  (:test-function   test-string-emitting-forms/binary
   :readtable-setup (setup-readtable-for-string-test :with-inline-emitting #t :binary #t)))

(def function read-from-string-with-string-syntax (string &key with-inline-emitting binary)
  (with-local-readtable
    (setup-readtable-for-string-test :with-inline-emitting with-inline-emitting :binary binary)
    (read-from-string string)))

(def function pprint-string (string &key with-inline-emitting binary)
  (pprint (macroexpand (read-from-string-with-string-syntax string :with-inline-emitting with-inline-emitting :binary binary))))

(def function emit/string (string &key (with-inline-emitting #f) (binary #f))
  (bind ((form (read-from-string-with-string-syntax string
                                                    :with-inline-emitting with-inline-emitting
                                                    :binary binary)))
    (if binary
        (with-output-to-sequence (*test-string-stream* :element-type '(unsigned-byte 8))
          (emit (eval form)))
        (with-output-to-string (*test-string-stream*)
          (emit (eval form))))))

(def (function d) test-string-emitting-forms (expected ast)
  (bind ((lambda-form `(lambda ()
                         (with-output-to-string (*test-string-stream*)
                           (emit ,ast)))))
    ;;(print (macroexpand-all lambda-form))
    (is (equalp expected
                (funcall (compile nil lambda-form))))))

(def (function d) test-string-emitting-forms/binary (expected ast)
  (bind ((lambda-form `(lambda ()
                         (with-output-to-sequence (*test-string-stream* :element-type '(unsigned-byte 8))
                           (emit ,ast)))))
    ;;(print (macroexpand-all lambda-form))
    (is (equalp expected
                (octets-to-string (funcall (compile nil lambda-form))
                                  :encoding :utf-8)))))

(def string-test/inline test/string/simple ()
  ("1 2"
   ｢`str("1 2")｣)

  ("1 2 3 4"
   ｢`str("1 2"
         " 3 4")｣)

  ("1 2 3 4 5 6 7 8"
   ｢`str("1 2"
         (" 3 4"
          " 5 6")
         " 7 8")｣))

(def string-test/inline test/string/inline-unquote ()
  ("1 2 3 4 5 6"
   ｢`str("1 2"
         ," 3 4 "
         "5 6")｣))

(def string-test test/string/unquote ()
  ("1 2 3 4 5 6 7 8 9 10 11 12"
   ｢`str("1 2"
         ,(list " 3 4"
                `str(" 5 6" " 7 8")
                " 9 10")
         " 11 12")｣)

  ("1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16"
   ｢`str("1 2"
         ,(list " 3 4"
                `str(" 5 6"
                     ,(list " 7 8" " 9 10")
                     " 11 12")
                " 13 14")
         " 15 16")｣))

(def string-test/inline test/string/toplevel-emit ()
  ("foo"
   ｢`str ,(progn "foo")｣))

(def string-test/inline test/string/spliced-unquote ()
  ("1 2 3 4 5 6 7"
   ｢`str("1 "
         ,(make-string 1 :initial-element #\2)
         ,(list " 3 " "4 " "5 ")
         ,(make-string 1 :initial-element #\6)
         " 7")｣))

(def string-test test/string/reverse ()
  ("1 2 3 4 5 6 7 8"
   ｢`str("1 2"
         ,(reverse (list `str(" 5 6") `str(" 3 4")))
         " 7 8")｣))
