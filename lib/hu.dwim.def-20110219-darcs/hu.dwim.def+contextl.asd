;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.def+contextl
  :class hu.dwim.system
  :depends-on (:contextl
               :hu.dwim.def)
  :components ((:module "integration"
                :components ((:file "contextl")))))
