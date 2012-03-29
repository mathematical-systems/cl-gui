;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.util.worker-group
  :class hu.dwim.system
  :description "Provides a worker group abstraction to do a bunch of shared tasks."
  :depends-on (:bordeaux-threads
               :hu.dwim.defclass-star+hu.dwim.def
               :hu.dwim.logger
               :hu.dwim.util.error-handling)
  :components ((:module "source"
                :components ((:file "worker-group")))))
