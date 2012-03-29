;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.common
  :class hu.dwim.system
  :description "An extended Common Lisp package to the general needs of other hu.dwim systems."
  :depends-on (:alexandria
               :anaphora
               :closer-mop
               :hu.dwim.common-lisp
               :iterate
               :metabang-bind)
  :components ((:module "source"
                :components ((:file "common" :depends-on ("package"))
                             (:file "package")))))
