;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Equality

#+xcvb (module (:depends-on ("interface/memoization")))

(in-package :cl)

(defpackage :eq
  (:use :cl)
  (:export
   #:<eq> #:<eq-simple> #:<eq-slot>
   #:<equal>
   #:== #:test-function
   #:<hashable>
   #:hash
   ))

(in-package :eq)

(defclass <eq> () ())
(defparameter <eq> (fmemo:memoized 'make-instance '<eq>))
(defgeneric == (i x y))
(defgeneric test-function (i)
  (:documentation "test function for <eq> interface"))

(defmethod == ((i <eq>) x y)
  (eql x y))
(defmethod test-function ((i <eq>))
  #'eql)

(defclass <eq-simple> (<eq>) ())
(defmethod test-function ((i <eq-simple>))
  #'(lambda (x y) (== i x y)))

(defclass <eq-slot> (<eq>)
  ((test :initform #'eql :initarg :test :reader test-function)))
(defmethod == ((i <eq-slot>) x y)
  (funcall (test-function i) x y))

(defclass <hashable> (<eq>) ())
(defgeneric hash (i x))
(defmethod hash ((i <hashable>) x)
  (sxhash x))

(defclass <equal> (<hashable>) ())
(defparameter <equal> (fmemo:memoized 'make-instance '<equal>))
(defmethod == ((i <equal>) x y)
  (equal x y))
(defmethod test-function ((i <equal>))
  #'equal)

