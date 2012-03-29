;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.defclass-star.documentation
  :class hu.dwim.documentation-system
  :depends-on (:hu.dwim.defclass-star.test
               :hu.dwim.presentation)
  :components ((:module "documentation"
                :components ((:file "defclass-star" :depends-on ("package"))
                             (:file "package")))))
