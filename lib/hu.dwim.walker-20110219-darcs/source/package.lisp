;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :common-lisp-user)

(defpackage :hu.dwim.walker
  (:documentation "A code walker for Common Lisp")

  (:use :alexandria
        :anaphora
        :contextl
        :hu.dwim.common-lisp
        :hu.dwim.def
        :hu.dwim.defclass-star
        :hu.dwim.util
        :metabang-bind)

  (:shadow #:eval)

  (:export
   #:-environment-
   #:-form-
   #:-parent-

   ;; these are defined by conditionally loaded platform dependent code, so export them from here
   #:make-empty-lexical-environment
   #:iterate-variables-in-lexenv
   #:iterate-functions-in-lexenv
   #:iterate-blocks-in-lexenv
   #:iterate-tags-in-lexenv
   #:proclaimed-special-variable?/global
   #:declared-variable-type/global
   ))
