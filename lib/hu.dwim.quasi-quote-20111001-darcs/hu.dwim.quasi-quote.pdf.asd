;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.quasi-quote.pdf
  :class hu.dwim.system
  :description "Quasi quote transformations for emitting PDF."
  :depends-on (:cffi
               :hu.dwim.quasi-quote)
  :components ((:module "source"
                :components ((:module "pdf"
                              :components ((:file "package")
                                           (:file "ast" :depends-on ("package"))
                                           (:file "syntax" :depends-on ("package" "ast"))
                                           (:file "transform" :depends-on ("package" "syntax" "ast"))))))))

