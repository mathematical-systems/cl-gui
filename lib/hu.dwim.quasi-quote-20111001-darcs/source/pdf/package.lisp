;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(def package :hu.dwim.quasi-quote.pdf
  (:use :hu.dwim.common
        :hu.dwim.def
        :hu.dwim.quasi-quote
        :hu.dwim.syntax-sugar)
  (:readtable-setup (hu.dwim.def:setup-readtable/same-as-package :hu.dwim.quasi-quote)))

(in-package :hu.dwim.quasi-quote.pdf)

(import-external-quasi-quote-symbols-for-extensions)
