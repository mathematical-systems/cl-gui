;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.def+hu.dwim.common
  :class hu.dwim.system
  :depends-on (:hu.dwim.def
               :hu.dwim.common)
  :components ((:module "integration"
                :components ((:file "common")))))
