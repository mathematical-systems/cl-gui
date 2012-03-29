;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.quasi-quote.test)

(defsuite* (test/list :in test))

(def function setup-readtable-for-list-test ()
  (enable-quasi-quoted-list-to-list-emitting-form-syntax))

#+nil ; TODO delme?
(def syntax-test-definer list-test
  (:test-function   test-list-emitting-forms
   :readtable-setup (setup-readtable-for-list-test #f))
  (:test-function   test-list-emitting-forms
   :readtable-setup (setup-readtable-for-list-test #t)))

(def definer simple-list-test (name &body strings)
  (assert (every #'stringp strings))
  `(def test ,name ()
     (map nil 'compare-with-standard-backquote
          '(,@strings))))

(def function test-list-emitting-forms (expected ast)
  (is (equalp expected (eval ast))))

(def function read-from-string-with-list-syntax (string)
  (with-local-readtable
    (enable-quasi-quoted-list-to-list-emitting-form-syntax)
    (read-from-string string)))

(def function pprint-list (string)
  (downcased-pretty-print (read-from-string-with-list-syntax string)))

(def function emit-list (string)
  (bind ((form (read-from-string-with-list-syntax string)))
    (eval form)))

(def function compare-with-standard-backquote (string)
  (bind ((expected (eval (read-from-string string)))
         (forms (read-from-string-with-list-syntax string))
         (actual (eval forms)))
    (is (equal actual expected))))

(def simple-list-test test/list/simple
  ｢`(a b)｣
  ｢`(a b (1 2))｣)

(def simple-list-test test/list/dotted
  ｢`(a . b)｣
  ｢`(a (b . c))｣)

(def simple-list-test test/list/unquote
  ｢`(a b ,(list 1 2) c)｣
  ｢`(a b ,(list 1 `("call" 'me) 3))｣
  ｢`(a b ,(list 1 `("call" 'me ,(list "Al")) 3))｣)

(def test test/list/nested-unquote ()
  (equal '(list 42 41)
         (eval
          `(bind ((bar 41))
             ,(eval
               (read-from-string-with-list-syntax
                ｢(bind ((foo 42))
                   ``(,,foo ,bar))｣))))))

(def simple-list-test test/list/unquote-splice
  ｢`(a b ,@(list 1 2))｣
  ｢`(a b ,@(list 1 `("call" 'me) 3))｣
  ｢`(a b ,@(list 1 `("call" 'me ,(list "Al")) 3))｣)

(def test test/list/nested/1 ()
  (bind ((stage1 (eval `(let ((b 43))
                          ,(read-from-string-with-list-syntax
                            ｢`(let ((a 42))
                                `(let ((x 44))
                                   (list x ,a ,,b)))｣)))))
    ;;(break "~S" stage1)
    (bind ((stage2 (eval stage1)))
      ;;(break "~S" stage2)
      (bind ((stage3 (eval stage2)))
        (is (equal (list 44 42 43) stage3))))))

(def special-variable *result-through-eval*)

(def test test/list/nested/2 ()
  (bind ((*result-through-eval* ()))
    (eval (read-from-string-with-list-syntax
           ｢(macrolet ((x (name)
                         `(macrolet ((,name (&body body)
                                       `(push '(,',name ,@body) *result-through-eval*)))
                            (,name 41 42 43))))
              (x alma)
              (x barack))｣))
    (is (equal '((barack 41 42 43)
                 (alma   41 42 43))
               *result-through-eval*))))

(def test test/list/errors ()
  (signals reader-error (read-from-string-with-list-syntax ｢`(bar `(foo x ,a ,,b ,,,c))｣)))

;;; this is adapted from sbcl's test suite

(defparameter *qq* '(*rr* *ss*))
(defparameter *rr* '(3 5))
(defparameter *ss* '(4 6))

(defun *rr* (x)
  (reduce #'* x))

(defparameter *x* '(a b))
(defparameter *y* '(c))
(defparameter *p* '(append *x* *y*))
(defparameter *q* '((append *x* *y*) (list 'sqrt 9)))
(defparameter *r* '(append *x* *y*))
(defparameter *s* '((append *x* *y*)))

(defparameter *backquote-tests*
  '(("``(,,*QQ*)" . (24))
    ("``(,@,*QQ*)" . 24)
    ("``(,,@*QQ*)" . ((3 5) (4 6)))
    ("``(FOO ,,*P*)" . (foo (a b c)))
    ("``(FOO ,,@*Q*)" . (foo (a b c) (sqrt 9)))
    ("``(FOO ,',*R*)" . (foo (append *x* *y*)))
    ("``(FOO ,',@*S*)" . (foo (append *x* *y*)))
    ("``(FOO ,@,*P*)" . (foo a b c))
    ("``(FOO ,@',*R*)" . (foo append *x* *y*))
    ;; The following expression produces different result under LW.
    ("``(FOO . ,,@*Q*)" . (foo a b c sqrt 9))
    ;; These three did not work.
    ("``(FOO ,@',@*S*)" . (foo append *x* *y*))
    ("``(FOO ,@,@*Q*)" . (foo a b c sqrt 9))
    ("``(,@,@*QQ*)" . (3 5 4 6))))

(def (test d) test-double-backquote (expression value)
  (bind ((forms (read-from-string-with-list-syntax expression)))
    (is (equal (eval (eval forms))
               value))))

(def test test/list/sbcl-backq-tests ()
  (mapc (lambda (test)
          (test-double-backquote (car test) (cdr test)))
        *backquote-tests*)
  (values))
