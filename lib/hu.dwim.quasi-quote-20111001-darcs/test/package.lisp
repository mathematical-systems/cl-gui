;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(def package :hu.dwim.quasi-quote.test
  (:use :babel
        :babel-streams
        :hu.dwim.common
        :hu.dwim.def
        :hu.dwim.quasi-quote
        :hu.dwim.quasi-quote.css
        :hu.dwim.quasi-quote.js
        #+nil :hu.dwim.quasi-quote.pdf
        :hu.dwim.quasi-quote.xml
        :hu.dwim.stefil
        :hu.dwim.syntax-sugar
        :hu.dwim.util
        :hu.dwim.walker)
  (:shadowing-import-from :hu.dwim.quasi-quote #:body-of
                          #:parent-of
                          #:form
                          #:map-ast)
  (:readtable-setup
   (hu.dwim.def:setup-readtable/same-as-package :hu.dwim.quasi-quote)
   (hu.dwim.syntax-sugar:enable-string-quote-syntax)))

(in-package :hu.dwim.quasi-quote.test)

(import-external-quasi-quote-symbols-for-extensions)
