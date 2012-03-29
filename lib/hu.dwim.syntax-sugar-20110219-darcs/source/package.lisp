;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :common-lisp-user)

(defpackage :hu.dwim.syntax-sugar
  (:use :hu.dwim.common)

  (:export #:cl-source-file-with-readtable
           #:define-syntax
           #:*toplevel-readtable*
           #:*quasi-quote-lexical-depth*
           #:*quasi-quote-nesting-level*
           #:get-macro-character*
           #:with-local-readtable
           #:with-standard-readtable
           #:list-readers))
