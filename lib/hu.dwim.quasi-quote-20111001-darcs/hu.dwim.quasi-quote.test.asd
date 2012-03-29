;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.quasi-quote.test
  :class hu.dwim.test-system
  :depends-on (:cxml
               :hu.dwim.quasi-quote
               :hu.dwim.quasi-quote.css
               #+nil :hu.dwim.quasi-quote.pdf
               :hu.dwim.quasi-quote.xml+hu.dwim.quasi-quote.js
               :hu.dwim.stefil+hu.dwim.def+swank
               :parse-number
               :trivial-shell)
  :components ((:module "test"
                :components ((:file "package")
                             (:file "suite" :depends-on ("package"))
                             (:file "list" :depends-on ("suite"))
                             (:file "binary" :depends-on ("suite"))
                             (:file "string" :depends-on ("suite"))
                             (:file "bivalent" :depends-on ("suite"))
                             (:file "xml" :depends-on ("suite" "string"))
                             (:file "js" :depends-on ("suite" "string" "xml"))
                             (:file "css" :depends-on ("suite"))
                             #+nil (:file "pdf" :depends-on ("suite"))))))
