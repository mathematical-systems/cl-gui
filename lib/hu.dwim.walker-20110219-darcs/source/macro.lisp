;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.walker)

;;;;;;
;;; defmacro

(def (form-class e) macro-definition-form (named-walked-form
                                           docstring-mixin)
  ())

(def walker defmacro
  (define-macro -form-)
  (with-form-object (node 'macro-definition-form -parent- :name (second -form-))))

(def unwalker macro-definition-form ()
  (cerror "Continue by returning NIL" "~S's cannot be unwalked" 'macro-definition-form))
