;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.syntax-sugar.test)

(defsuite* (test/feature-cond :in test))

(defmacro with-features (features &body body)
  `(bind ((*features* (list* ,@features *features*)))
     ,@body))

(defun read-from-string/with-feature-cond-syntax (string)
  (with-standard-readtable
    (bind ((*package* (find-package :hu.dwim.syntax-sugar.test)))
      (enable-feature-cond-syntax)
      (read-from-string string))))

(defmacro define-feature-cond-test (name test-form &body tests)
  (with-unique-names (form result position)
    `(deftest ,name ()
       (bind ((,form ,test-form))
         ,@(loop
             :for entry :in tests
             :collect `(with-features ,(first entry)
                         (bind (((:values ,result ,position) (read-from-string/with-feature-cond-syntax ,form)))
                           (is (= (length ,form) ,position))
                           (is (equal ,(second entry) ,result)))))))))

(define-feature-cond-test test/feature-cond/simple
    ｢#*((:foo "it's foo")
        (:bar "it's bar")
        (t "it's default"))｣
  ((:foo :bar) "it's foo")
  ((:bar) "it's bar")
  (() "it's default"))

(define-feature-cond-test test/feature-cond/without-default
    ｢#*((:foo "it's foo")
        (:bar "it's bar"))｣
  ((:foo :bar) "it's foo")
  ((:bar) "it's bar")
  (() nil))

(define-feature-cond-test test/feature-cond/with-and-or-not
    ｢#*(((and :foo :bar) "(and :foo :bar)")
        ((and (or :foo :bar) :baz)  "(and (or :foo :bar) :baz)")
        ((and (or :foo :bar) (not :zork)) "(and (or :foo :bar) (not :zork))")
        ((or :foo :bar) "(or :foo :bar)")
        (t "default"))｣
  ((:foo :bar) "(and :foo :bar)")
  ((:foo :zork) "(or :foo :bar)")
  ((:foo) "(and (or :foo :bar) (not :zork))")
  (() "default"))

(define-feature-cond-test test/feature-cond/with-custom-expressions
    ｢#*(((and :foo (find-symbol "DEFTEST" :hu.dwim.stefil)) "(and :foo (find-symbol '#:deftest :hu.dwim.stefil))")
        ((and (or :foo (< 5 10)) :baz)  "(and (or :foo (< 5 10)) :baz)")
        ((or :foo :bar t) "(or :foo :bar t)"))｣
  ((:foo) "(and :foo (find-symbol '#:deftest :hu.dwim.stefil))")
  ((:baz) "(and (or :foo (< 5 10)) :baz)")
  ((:bar) "(or :foo :bar t)")
  (() "(or :foo :bar t)"))

(define-feature-cond-test test/feature-cond/evaluation
    ｢#*((:foo not-evaluated))｣
  ((:foo) 'not-evaluated)
  (() nil))

(deftest test/feature-cond/auto-progn-wrapping ()
  (bind ((form ｢#*((:foo (+ 2 2)
                         (+ 40 2))
                   (:bar #. nil
                         not-evaluated))｣))
    (with-features (:foo)
      (bind ((result (read-from-string/with-feature-cond-syntax form)))
        ;; branches of mutliple forms are automatically wrapped in a `(progn ,@)
        (is (consp result))
        (is (eq (first result) 'progn))
        (is (eql (eval (second result)) 4))
        (is (eql (eval result) 42))))
    (with-features (:bar)
      (bind ((result (read-from-string/with-feature-cond-syntax form)))
        ;; NIL's are removed from branches of mutliple forms to support #.(warn "careful!") idiom
        (is (eq result 'not-evaluated))))))

(define-feature-cond-test test/feature-cond/comments
    ｢#*(; bla1
        (:foo "it's foo") ; bla2
        ; bla3
        (:bar "it's bar") ;bla4
        )｣
  ((:foo :bar) "it's foo")
  ((:bar) "it's bar")
  (() nil))

(deftest test/feature-cond/conditions ()
  (bind ((form ｢#*((:foo "it's foo")
                   (:bar "it's bar")
                   (t #.(warn "default reached")))｣))
    (with-features (:foo :bar)
      (not-signals condition
        (is (string= (read-from-string/with-feature-cond-syntax form)
                     "it's foo"))))
    (bind ((condition (signals simple-warning (read-from-string/with-feature-cond-syntax form))))
      (when condition
        (is (string= (simple-condition-format-control condition) "default reached"))))))

(define-feature-cond-test test/feature-cond/read-suppress
    ｢#+(cl:or)
     #*((:missing 'non-existent-package::zork)
        (t "default"))
     42｣
  (() 42))

(define-feature-cond-test test/feature-cond/bug/1
    ｢#*((:missing 'non-existent-package::zork)
        (t "default")
        (noise 'will-it-break?)
        ((noise and (noise)) (will-it-break?)))｣
  (() "default"))
