;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(def package :hu.dwim.syntax-sugar.test
  (:use :hu.dwim.common
        :hu.dwim.def
        :hu.dwim.stefil
        :hu.dwim.syntax-sugar
        :hu.dwim.syntax-sugar.unicode
        :hu.dwim.walker)
  (:shadow #:deftest)
  (:readtable-setup
   (hu.dwim.syntax-sugar:enable-sharp-l-syntax) ; TODO convert test/lambda.lisp to use readtime-wrapper-syntax and then delme
   (hu.dwim.syntax-sugar:enable-readtime-wrapper-syntax)
   (hu.dwim.syntax-sugar:enable-string-quote-syntax)))
