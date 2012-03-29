;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.util.error-handling
  :class hu.dwim.system
  :description "Various utilities, contains code for complex error handling."
  :depends-on (:hu.dwim.defclass-star+hu.dwim.def
               :hu.dwim.logger
               :hu.dwim.util)
  :components ((:module "source"
                :components ((:file "error-handling")))
               (:module "integration"
                :components (#+sbcl (:file "backtrace-sbcl")))))
