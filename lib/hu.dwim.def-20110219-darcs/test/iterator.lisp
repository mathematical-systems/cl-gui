;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def.test)

(defsuite* (test/iterator :in test) ()
  (with-fixture test/iterator/fixture
    (-run-child-tests-)))

(def definer iterator-test (name (iterator-name iterator-args result &optional invocation-count)
                                 &body body)
  `(def test (,name :compile-before-run t) ()
     (with-fixture test/iterator/fixture
       (bind ((-invocation-count- 0)
              (-result- '()))
         (,iterator-name ,iterator-args
           (incf -invocation-count-)
           ,@body)
         (is (equalp -result- ,result))
         ,@(when invocation-count
             `((is (= -invocation-count- ,invocation-count))))
         -result-))))

(defixture test/iterator/fixture
  (handler-bind ((style-warning #'muffle-warning))
    (macrolet ((frob (&body body)
                 `(progn
                    ,@(mapcar (lambda (el)
                                `(finishes (eval ',el)))
                              body))))
      (finishes (frob
                 (def iterator iterator/1 ()
                   (dotimes (i 4)
                     (funcall -visitor- i)))
                 (def iterator iterator/2 (arg1 &key divisor)
                   (dotimes (i (/ arg1 divisor))
                     (funcall -visitor- i)))))))
  (-body-))

(def iterator-test test/iterator/1 (do-iterator/1 (j) '(3 2 1 0))
  (push j -result-))

(def iterator-test test/iterator/2/1 (do-iterator/2 (j 8 :divisor 4) '(1 0))
  (push j -result-))

(def iterator-test test/iterator/2/2 (do-iterator/2 (j 8 :divisor 2) '(3 2 1 0))
  (push j -result-))
