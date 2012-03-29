;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.quasi-quote.test)

(defsuite* (test/binary :in test))

(defun set-ub8-reader-in-readtable (&optional (readtable *readtable*))
  "Enable a special reader that will read #(1 2 3) into an ub8-vector."
  (bind ((original (get-dispatch-macro-character #\# #\( readtable)))
    (set-dispatch-macro-character #\# #\(
                                  (lambda (stream char1 char2)
                                    (bind ((result (funcall original stream char1 char2)))
                                      (if (every (curry #'> 256) result)
                                          (coerce result 'ub8-vector)
                                          result)))
                                  readtable)))

(def special-variable *test-binary-stream*)

(def function setup-readtable-for-binary-test (&key with-inline-emitting)
  (enable-quasi-quoted-binary-to-binary-emitting-form-syntax
   '*test-binary-stream*
   :with-inline-emitting with-inline-emitting)
  (set-ub8-reader-in-readtable))

(def syntax-test-definer binary-test
  (:test-function test-binary-emitting-forms
   :readtable-setup (setup-readtable-for-binary-test :with-inline-emitting #f)))

(def syntax-test-definer binary-test/inline
  (:test-function   test-binary-emitting-forms
   :readtable-setup (setup-readtable-for-binary-test :with-inline-emitting #f))
  (:test-function   test-binary-emitting-forms
   :readtable-setup (setup-readtable-for-binary-test :with-inline-emitting #t)))

(def function read-from-string-with-binary-syntax (string &key with-inline-emitting)
  (with-local-readtable
    (setup-readtable-for-binary-test :with-inline-emitting with-inline-emitting)
    (read-from-string string)))

(def function pprint-binary (string &key with-inline-emitting)
  (pprint (macroexpand (read-from-string-with-binary-syntax string :with-inline-emitting with-inline-emitting))))

(def function emit/binary (string &key with-inline-emitting)
  (bind ((form (read-from-string-with-binary-syntax string :with-inline-emitting with-inline-emitting)))
    (with-output-to-sequence (*test-binary-stream* :element-type '(unsigned-byte 8))
      (emit (eval form)))))

(def (function d) test-binary-emitting-forms (expected ast)
  (bind ((lambda-form `(lambda ()
                         (with-output-to-sequence (*test-binary-stream*)
                           (emit ,ast)))))
    ;;(print (macroexpand-all lambda-form))
    (is (equalp expected
                (funcall (compile nil lambda-form))))))

(def binary-test/inline test/binary/simple ()
  (#(1 2)
    "`bin(1 2)")
  (#(1 2 3 4)
    "`bin((1 2)
          (3 4))")
  (#(1 2 3 4 5 6 7 8)
    "`bin((1 2)
          ((3 4)
           (5 6))
          (7 8))"))

(def binary-test/inline test/string/toplevel-emit ()
  (#(99 98 97)
   ｢`bin ,(progn #(99 98 97))｣))

(def binary-test/inline test/binary/inline-unquote ()
  (#(1 2 3 4 5 6)
    "`bin((1 2)
          ,#(3 4)
          (5 6))"))

(def binary-test/inline test/binary/toplevel-emit ()
  (#(42 43)
   ｢`bin ,(progn #(42 43))｣))

(def binary-test test/binary/unquote ()
  (#(1 2 3 4 5 6 7 8 9 10 11 12)
    "`bin((1 2)
          ,(list
            #(3 4)
            `bin((5 6) (7 8))
            #(9 10))
          (b c))")

  (#(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)
    "`bin(1
          2
          ,(list
            #(3 4)
            `bin((5 6)
                 ,(list #(7 8) #(9 10))
                 (b c))
            #(13 14))
          f
          10)"))

(def binary-test/inline test/binary/spliced-unquote ()
  (#(1 2 3 4 5 6 7)
    "`bin(1
          ,(make-array 1 :initial-element 2)
          ,(list #(3) #(4) #(5))
          ,(make-array 1 :initial-element 6)
          7)"))

(def binary-test test/binary/reverse ()
  (#(1 2 3 4 5 6 7 8)
   "`bin(1 2
         ,(reverse (list `bin(5 6)
                         `bin(3 4)))
         7 8)"))

(def binary-test/inline test/binary/ordered-unquote ()
  (#(1 2 3 4 5 6)
    "`bin(1 2
          ,`bin(3 4)
          5 6)")

  (#(1 2 3 4 5 6 7 8 9 10 11 12)
    "`bin(1 2
          ,(list
            `bin(3 4)
            `bin(5 6 7 8)
            `bin(9 a))
          b c)")

  (#(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)
    "`bin(1 2
          ,(list
            `bin(3 4)
            `bin(5 6
                 ,(list `bin(7 8) `bin(9 a))
                 b c)
            `bin(d e))
          f 10)"))


;;;;;;
;;; mixed non-compatibles

;; TODO this mixed stuff may be nonsense...

(def special-variable *test-binary-stream-2*)

(def function setup-readtable-for-mixed-binary-test (inline1? inline2?)
  (enable-quasi-quoted-binary-to-binary-emitting-form-syntax '*test-binary-stream*
                                                             :with-inline-emitting inline1?
                                                             :dispatched-quasi-quote-name 'bin1)
  (enable-quasi-quoted-binary-to-binary-emitting-form-syntax '*test-binary-stream-2*
                                                             :with-inline-emitting inline2?
                                                             :dispatched-quasi-quote-name 'bin2)
  (set-ub8-reader-in-readtable))

(def syntax-test-definer binary-test/mixed
  (:test-function   test-binary-emitting-forms/mixed
   :readtable-setup (setup-readtable-for-mixed-binary-test #f #f))
  (:test-function   test-binary-emitting-forms/mixed
   :readtable-setup (setup-readtable-for-mixed-binary-test #t #t))
  (:test-function   test-binary-emitting-forms/mixed
   :readtable-setup (setup-readtable-for-mixed-binary-test #t #f))
  (:test-function   test-binary-emitting-forms/mixed
   :readtable-setup (setup-readtable-for-mixed-binary-test #f #t)))

(def (function d) test-binary-emitting-forms/mixed (expected ast)
  (bind ((lambda-form `(lambda ()
                         (bind ((result))
                           (push (with-output-to-sequence (*test-binary-stream*)
                                   (push (with-output-to-sequence (*test-binary-stream-2*)
                                           (emit ,ast))
                                         result))
                                 result)
                           (values-list result)))))
    ;;(print (macroexpand-all lambda-form))
    (is (equalp expected
                (multiple-value-list (funcall (compile nil lambda-form)))))))

(def binary-test/mixed test/binary/mixed ()
  ((list #(1 2 3 4 5 6)
         #(7 8 9 10))
   "`bin1(1 2
          `bin2(7 8 ,(list `bin2(9 a)))
          ,(list `bin1(3 4))
          5 6)"))
