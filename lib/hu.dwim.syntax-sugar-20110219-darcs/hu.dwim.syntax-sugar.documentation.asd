;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.syntax-sugar.documentation
  :class hu.dwim.documentation-system
  :depends-on (:hu.dwim.syntax-sugar.test
               :hu.dwim.presentation)
  :components ((:module "documentation"
                :components ((:file "package")
                             (:file "syntax-sugar" :depends-on ("package"))))))
