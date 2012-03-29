;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.quasi-quote.documentation
  :class hu.dwim.documentation-system
  :depends-on (:hu.dwim.quasi-quote.test
               :hu.dwim.presentation)
  :components ((:module "documentation"
                :components ((:file "quasi-quote" :depends-on ("package"))
                             (:file "package")))))
