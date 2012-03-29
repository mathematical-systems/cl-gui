;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.def+hu.dwim.delico
  :class hu.dwim.system
  :depends-on (:hu.dwim.def
               :hu.dwim.delico)
  :components ((:module "integration"
                :components ((:file "delico")))))
