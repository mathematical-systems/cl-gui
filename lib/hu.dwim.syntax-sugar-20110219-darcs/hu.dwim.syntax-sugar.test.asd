;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.syntax-sugar.test
  :class hu.dwim.test-system
  :depends-on (:hu.dwim.stefil+hu.dwim.def+swank
               :hu.dwim.syntax-sugar+hu.dwim.walker
               :hu.dwim.syntax-sugar.unicode)
  :components ((:module "test"
                :components ((:file "feature-cond" :depends-on ("suite"))
                             (:file "lambda" :depends-on ("suite"))
                             (:file "package")
                             (:file "quasi-quote" :depends-on ("suite"))
                             (:file "readtime-wrapper" :depends-on ("suite"))
                             (:file "sharp-l" :depends-on ("lambda"))
                             (:file "string-quote" :depends-on ("suite"))
                             (:file "suite" :depends-on ("package"))))))
