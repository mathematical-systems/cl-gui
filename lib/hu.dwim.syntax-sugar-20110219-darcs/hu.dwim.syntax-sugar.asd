;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.syntax-sugar
  :class hu.dwim.system
  :description "Various syntax extensions."
  :depends-on (:hu.dwim.common)
  :components ((:module "source"
                :components ((:file "duplicates" :depends-on ("package"))
                             (:file "feature-cond" :depends-on ("one-liners"))
                             (:file "number" :depends-on ("syntax-sugar"))
                             (:file "one-liners" :depends-on ("syntax-sugar"))
                             (:file "package")
                             (:file "quasi-quote" :depends-on ("one-liners"))
                             (:file "readtime-wrapper" :depends-on ("one-liners"))
                             (:file "string-quote" :depends-on ("one-liners"))
                             (:file "syntax-sugar" :depends-on ("duplicates"))))))
