;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(def package :hu.dwim.quasi-quote.xml
  (:use :babel
        :babel-streams
        :hu.dwim.common
        :hu.dwim.def
        :hu.dwim.quasi-quote
        :hu.dwim.syntax-sugar
        :hu.dwim.util)
  (:readtable-setup (hu.dwim.def:setup-readtable/same-as-package :hu.dwim.quasi-quote)))

(in-package :hu.dwim.quasi-quote.xml)

(def (function e) import-external-quasi-quote-symbols-for-extensions/xml (&optional (package *package*))
  "Import those symbols in PACKAGE that are public to extensions of hu.dwim.quasi-quote-xml but not to its users."
  (import
   '(children-of
     name-of
     attributes-of
     )
   package))

(import-external-quasi-quote-symbols-for-extensions)
