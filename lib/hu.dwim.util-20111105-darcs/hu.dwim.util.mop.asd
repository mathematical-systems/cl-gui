;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.util.mop
  :class hu.dwim.system
  :depends-on (:hu.dwim.util
               :closer-mop)
  :components ((:module "source"
                :components (#+sbcl(:file "compact-class" :depends-on ("mop"))
                             (:file "mop")))))
