;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :common-lisp-user)

(defpackage :hu.dwim.walker.test
  (:use :contextl
        :hu.dwim.common
        :hu.dwim.def
        :hu.dwim.stefil
        :hu.dwim.util
        :hu.dwim.walker))

(in-package :hu.dwim.walker.test)

(defsuite* (test :in root-suite) ()
  (handler-bind
      ((walker-warning #'muffle-warning))
    (-run-child-tests-)))
