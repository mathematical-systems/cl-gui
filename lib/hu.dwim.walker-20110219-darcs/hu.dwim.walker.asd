;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.walker
  :class hu.dwim.system
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
           "Levente Mészáros <levente.meszaros@gmail.com>")
  :description "Common Lisp form walker and unwalker (to and from CLOS instances)."
  :depends-on (:alexandria
               :anaphora
               :contextl
               :closer-mop
               :hu.dwim.common-lisp
               :hu.dwim.def+contextl
               :hu.dwim.defclass-star+hu.dwim.def
               :hu.dwim.util
               :metabang-bind)
  :components ((:file "package" :pathname "source/package")
               (:module "integration"
                :depends-on ("package")
                :components (#+allegro(:file "allegro")
                             #+clisp(:file "clisp")
                             #+cmu(:file "cmucl")
                             #+lispworks(:file "lispworks")
                             #+openmcl(:file "openmcl")
                             #+ecl(:file "ecl")
                             #+sbcl(:file "sbcl")))
               (:module "source"
                :depends-on ("integration" "package")
                :components ((:file "api" :depends-on ("conditions" "duplicates"))
                             (:file "ast" :depends-on ("infrastructure" "handler" "progn" "function"))
                             (:file "conditions" :depends-on ("variables"))
                             (:file "duplicates")
                             (:file "function" :depends-on ("infrastructure" "progn"))
                             (:file "handler" :depends-on ("infrastructure" "function"))
                             (:file "infrastructure" :depends-on ("api" "lexenv"))
                             (:file "lexenv" :depends-on ("api"))
                             (:file "macro" :depends-on ("function" "infrastructure" "progn"))
                             (:file "progn" :depends-on ("infrastructure"))
                             (:file "variables")))))
