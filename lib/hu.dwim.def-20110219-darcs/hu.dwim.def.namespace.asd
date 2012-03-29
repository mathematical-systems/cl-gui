;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.def.namespace
  :class hu.dwim.system
  :description "Thread safe namespace (global hashtable) definer."
  :depends-on (:bordeaux-threads
               :hu.dwim.def
               :hu.dwim.util
               :trivial-garbage)
  :components ((:module "source"
                :components ((:file "namespace-late")))))
