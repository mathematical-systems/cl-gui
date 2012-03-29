;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.util.zlib
  :class hu.dwim.system
  :description "Bindings and lisp API for zlib."
  :depends-on (:cffi
               :hu.dwim.defclass-star+hu.dwim.def
               :hu.dwim.logger
               :hu.dwim.util)
  :components ((:module "source"
                :components ((:file "zlib")))))
