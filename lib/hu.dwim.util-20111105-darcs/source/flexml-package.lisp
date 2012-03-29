;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(def package :hu.dwim.util.flexml
  (:use :hu.dwim.common
        :hu.dwim.def
        :hu.dwim.util)
  (:readtable-setup
   (hu.dwim.def:setup-readtable/same-as-package :hu.dwim.util)))

(def package :hu.dwim.util.xml
  ;; for details see http://www.w3.org/XML/1998/namespace.html
  (:export #:id
           #:base
           #:lang
           #:space))
