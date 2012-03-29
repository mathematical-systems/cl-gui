;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.syntax-sugar.test)

(defsuite* (test/lambda/sharp-l :in test/lambda))

(define-syntax sharp-l-test ()
  (set-sharp-boolean-syntax-in-readtable)
  (set-lambda-with-bang-args-syntax-in-readtable :start-character #\[ :end-character #\])
  (set-sharp-l-syntax-in-readtable))

(def function read-from-string/with-sharp-l-test-syntax (string)
  (with-local-readtable
    (set-sharp-l-test-syntax-in-readtable)
    (bind ((result (read-from-string string)))
      (if (and (consp result)
               (eq (first result) 'hu.dwim.syntax-sugar::lambda-with-bang-args-expander))
          (macroexpand-1 result)
          result))))

(def function eval/with-sharp-l-test-syntax (string)
  (eval (read-from-string/with-sharp-l-test-syntax string)))

(deftest perform-sharp-l-test (fn-string args expected)
  (bind ((fn (eval/with-sharp-l-test-syntax fn-string))
         (result (if (functionp fn)
                     (apply fn args)
                     fn)))
    (if (functionp expected)
        (funcall expected result)
        (is (equal expected result)))))

(defmacro define-sharp-l-test (name &body body)
  `(deftest ,name ()
     ,@(mapcar (lambda (entry)
                 (if (member (first entry) '(signals is))
                     entry
                     (bind (((fn args expected) entry))
                       `(perform-sharp-l-test ,fn (list ,@args) ,expected))))
               body)))

(define-sharp-l-test test/lambda/sharp-l/installed-on-square-braces
  (｢[+ !1 40]｣ (2) 42))

(define-sharp-l-test test/lambda/sharp-l/simple
  (｢#L42｣ () 42)
  (｢#L(+ !1 !2)｣ (2 40) 42))

(define-sharp-l-test test/lambda/sharp-l/minimum-args
  (｢#2L42｣ (1 2) 42)
  (｢#2L(+ !1 40)｣ (2 99) 42))

(define-sharp-l-test test/lambda/sharp-l/block-label-is-ignored
  (｢#L(block !2 (return-from !2 !1))｣ (6) 6))

(deftest test/lambda/sharp-l/block-label-comes-from-lexenv ()
  ;; this test fails on SBCL because the lexenv on SBCL does not contain block names
  ;; see https://bugs.launchpad.net/sbcl/+bug/590076
  (is (= 42 (eval/with-sharp-l-test-syntax ｢(block foo
                                              (funcall [return-from foo 42]))｣))))

(define-sharp-l-test test/lambda/sharp-l/no-variable-in-quote
  (｢#L'!1｣ () '!1))

(define-sharp-l-test test/lambda/sharp-l/not-captures-outer-bang
  (｢(let ((!1 42))
      (declare (ignore !1))
      #L!1)｣
   (69)
   69))

(define-sharp-l-test test/lambda/sharp-l/nested
  (｢#L#L1｣ () (lambda (fn)
                (is (= (funcall fn) 1))))
  (｢#L#L!1｣ () (lambda (fn)
                 (is (= (funcall fn 42) 42))))
  (｢#L#L(+ !1 !2)｣ () (lambda (fn)
                        (is (= (funcall fn 40 2) 42)))))

(deftest test/lambda/sharp-l/complex-nested ()
  (is (string=
       "BARfoo"
       (funcall (funcall (eval/with-sharp-l-test-syntax
                          ｢#L(let ((a !1))
                               #L(concatenate 'string (string-upcase !1) a))｣)
                         "foo")
                "bar"))))

(define-sharp-l-test test/lambda/sharp-l/symbol-macrolet
  (｢(symbol-macrolet ((sym !1))
      #Lsym)｣
   ('bar)
   'bar)
  (｢(symbol-macrolet ((sym !1))
      (funcall #Lsym 'foo))｣
   ()
   'foo))

(define-sharp-l-test test/lambda/sharp-l/symbol-macrolet/inner
  (｢#L(symbol-macrolet ((!2 !1))
        (+ !2 10))｣
   (5)
   15))

(define-sharp-l-test test/lambda/sharp-l/macrolet
  (｢(macrolet ((mac (arg)
                 `(+ !1 ,arg)))
      #L(mac 10))｣
   (5)
   15)
  (｢(macrolet ((mac (arg)
                 `(+ !1 ,arg)))
     (funcall #L(mac 10) 2))｣
   ()
   12))

(define-sharp-l-test test/lambda/sharp-l/macrolet/inner
  (｢#L(macrolet ((!2 ()
                   '!1))
        (!2))｣
   (15)
   15))

(deftest test/lambda/sharp-l/bang-binds-to-innermost ()
  (is (equal
       10
       (funcall (funcall (eval/with-sharp-l-test-syntax
                          ｢#L(let ((a !1))
                               #L(+ a !1))｣)
                         6)
                4))))

(deftest test/lambda/sharp-l/interposed-macrolet ()
  (is (equal
       6
       (funcall (funcall (eval/with-sharp-l-test-syntax
                          ｢#L(macrolet ((mac () '!1))
                               #L(mac))｣))
                6))))

(deftest test/lambda/sharp-l/nested-macrolet ()
  (is (equal
       21
       (funcall (funcall (eval/with-sharp-l-test-syntax
                          ｢#L(macrolet ((return-bang () ''!1))
                               (macrolet ((multiply-first-bang (arg) `(* ,arg ,(return-bang))))
                                 #L(+ (multiply-first-bang 2) 1)))｣))
                10))))

(deftest test/lambda/sharp-l/interposed-symbol-macrolet ()
  (is (equal
       10
       (funcall (funcall (eval/with-sharp-l-test-syntax
                          ｢#L(symbol-macrolet ((mac !1))
                               #Lmac)｣))
                10))))
