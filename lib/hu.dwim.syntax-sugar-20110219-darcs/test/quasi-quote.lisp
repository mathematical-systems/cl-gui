;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.syntax-sugar.test)

(defsuite* (test/quasi-quote :in test))

;; define a custom quasi-quote syntax. this way we need to write less
;; code to set up the readtable using the readtime-wrapper syntax in the tests.

(define-syntax my-quasi-quote (&key (nested-quasi-quote-wrapper ''my-quote)
                                    (start-character #\`)
                                    end-character
                                    dispatch-character
                                    (unquote-character #\,)
                                    (splice-character #\@))
  (set-quasi-quote-syntax-in-readtable 'my-quote 'my-unquote
                                       :nested-quasi-quote-wrapper nested-quasi-quote-wrapper
                                       :start-character start-character
                                       :dispatch-character dispatch-character
                                       :end-character end-character
                                       :unquote-character unquote-character
                                       :splice-character splice-character))

;; we need to define a standalone syntax for the dispatched-quasi-quote version,
;; because they can't use both the #\` character...
(define-syntax my-dispatched-quasi-quote (&key (nested-quasi-quote-wrapper ''my-quote)
                                               (unquote-character #\,)
                                               (splice-character #\@))
  (set-quasi-quote-syntax-in-readtable 'my-quote 'my-unquote
                                       :nested-quasi-quote-wrapper nested-quasi-quote-wrapper
                                       :unquote-character unquote-character
                                       :splice-character splice-character
                                       :dispatched-quasi-quote-name "my-qq"))

(deftest test/quasi-quote/simple ()
  (enable-readtime-wrapper-syntax)
  (bind ((expected '(my-quote
                     (1 2 (my-unquote 3 nil) (my-unquote 4 :splice) 5))))
    (is (equal expected
               (read-from-string
                "{with-my-quasi-quote-syntax
                  `(1 2 ,3 ,@4 5)}")))
    (is (equal expected
               (read-from-string
                "{(with-my-quasi-quote-syntax :start-character #\\$ :unquote-character #\\;)
                  $(1 2 ;3 ;@4 5)}")))
    (is (equal expected
               (read-from-string
                "{(with-my-quasi-quote-syntax :start-character #\\`
                                              :unquote-character #\\;
                                              :dispatch-character #\\#)
                  #`(1 2 ;3 ;@4 5)}")))
    (is (equal expected
               (read-from-string
                "{(with-my-quasi-quote-syntax :start-character #\\[
                                              :end-character #\\]
                                              :unquote-character #\\;
                                              :splice-character #\\!)
                  [1 2 ;3 ;!4 5]}")))
    ;; this is a bit crazy: quasi-quote is registered on the same {} chars as the wrapping
    ;; readtime-wrapper, but quasi-quote restores the syntax when its scope is left, so
    ;; everything's fine with this test.
    (is (equal expected
               (read-from-string
                "{(with-my-quasi-quote-syntax :start-character #\\{
                                              :end-character #\\}
                                              :unquote-character #\\;
                                              :splice-character #\\!)
                  {1 2 ;3 ;!4 5}}")))))

(deftest test/quasi-quote/sharp-dot-readtable-case ()
  (enable-readtime-wrapper-syntax)
  (is (equal '(my-quote (1 "FOO"))
             (read-from-string
              "{with-my-quasi-quote-syntax
                `(1 #.(symbol-name 'foo))}"))))

(deftest test/quasi-quote/nested ()
  (enable-readtime-wrapper-syntax)
  (is (equal '(my-quote (1 2 (my-unquote (list 3 (my-quote (4 (my-unquote 5 :splice)))) nil)))
             (read-from-string
              "{with-my-quasi-quote-syntax
                `(1 2 ,(list 3 `(4 ,@5)))}")))
  (is (equal '(my-quote (1 2 (my-quote (3 (my-unquote 4 :splice) (my-unquote (my-unquote 5 nil) nil)))))
             (read-from-string
              "{(with-my-quasi-quote-syntax)
                `(1 2 `(3 ,@4 ,,5))}"))))

(deftest test/quasi-quote/end-character-reader-restoration ()
  (enable-readtime-wrapper-syntax)
  (bind ((42-reader (lambda (stream char)
                      (declare (ignore stream char))
                      42)))
    (set-macro-character #\[ 42-reader)
    (set-macro-character #\] 42-reader))
  (bind ((input "[{(with-my-quasi-quote-syntax :start-character #\\[
                                               :end-character #\\]
                                               :unquote-character #\\;
                                               :splice-character #\\!)
                   [1 2 ;3 ;!4 5]]}]")
         (value nil)
         (position nil))
    (setf (values value position) (read-from-string input))
    (is (= value 42))
    (setf (values value position) (read-from-string input t nil :start position))
    (is (equal value
               '(progn
                 (my-quote (1 2 (my-unquote 3 nil) (my-unquote 4 :splice) 5))
                 42)))))

(defsuite* (test/dispatched-quasi-quote :in test))

(deftest test/dispatched-quasi-quote/simple ()
  (enable-my-dispatched-quasi-quote-syntax)
  (is (equal '(my-quote (1 2 (my-unquote 3 nil) (my-unquote 4 :splice) 5))
             (read-from-string "`my-qq(1 2 ,3 ,@4 5)")))
  (is (equal '(1 2 3)
             (eval (read-from-string "`(1 2 ,(+ 1 2))")))))
