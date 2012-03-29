;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.common.documentation
  :class hu.dwim.documentation-system
  :depends-on (:hu.dwim.common
               :hu.dwim.presentation)
  :components ((:module "documentation"
                :components ((:file "common" :depends-on ("package"))
                             (:file "package")))))
