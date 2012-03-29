;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.common-lisp.documentation
  :class hu.dwim.documentation-system
  :depends-on (:hu.dwim.common-lisp
               :hu.dwim.presentation)
  :components ((:module "documentation"
                :components ((:file "common-lisp" :depends-on ("package"))
                             (:file "package")))))
