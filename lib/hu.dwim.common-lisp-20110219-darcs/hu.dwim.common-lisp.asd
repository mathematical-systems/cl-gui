;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.common-lisp
  :class hu.dwim.system
  :description "A redefinition of the standard Common Lisp package that includes a number of renames and shadows."
  :components ((:module "source"
                :components ((:file "package")))))
