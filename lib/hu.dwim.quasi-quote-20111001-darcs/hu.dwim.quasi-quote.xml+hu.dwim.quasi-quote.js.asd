;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.quasi-quote.xml+hu.dwim.quasi-quote.js
  :class hu.dwim.system
  :depends-on (:hu.dwim.quasi-quote.js
               :hu.dwim.quasi-quote.xml)
  :components ((:module "integration"
                :components ((:file "xml+js")))))
