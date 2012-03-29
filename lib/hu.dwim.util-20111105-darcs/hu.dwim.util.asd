;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.util
  :class hu.dwim.system
  :description "Various utilities, this is the most basic system that only introduce a small number of external dependencies."
  :depends-on (:hu.dwim.def+hu.dwim.common
               :hu.dwim.defclass-star+hu.dwim.def
               :hu.dwim.syntax-sugar)
  :components ((:module "source"
                :components ((:file "anaphora" :depends-on ("package"))
                             (:file "dynamic-context" :depends-on ("miscellaneous"))
                             (:file "error-handling-early" :depends-on ("package" "miscellaneous"))
                             (:file "generic-operator" :depends-on ("package"))
                             (:file "hash-table" :depends-on ("package"))
                             (:file "integer-to-string" :depends-on ("package"))
                             (:file "iterate" :depends-on ("package"))
                             (:file "linear-mapping" :depends-on ("type"))
                             (:file "number" :depends-on ("package"))
                             (:file "package")
                             (:file "place" :depends-on ("package"))
                             (:file "sequence" :depends-on ("package"))
                             (:file "string" :depends-on ("miscellaneous"))
                             (:file "threads-early" :depends-on ("package"))
                             (:file "type" :depends-on ("package"))
                             (:file "miscellaneous" :depends-on ("package"))))))
