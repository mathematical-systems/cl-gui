;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.util.test
  :class hu.dwim.test-system
  :depends-on (:hu.dwim.stefil+hu.dwim.def+swank
               :hu.dwim.util
               :hu.dwim.util.error-handling
               :hu.dwim.util.error-handling+swank
               :hu.dwim.util.finite-state-machine
               :hu.dwim.util.i18n
               :hu.dwim.util.mop
               :hu.dwim.util.production+swank
               :hu.dwim.util.source
               :hu.dwim.util.standard-process
               :hu.dwim.util.threads
               :hu.dwim.util.worker-group
               :hu.dwim.util.soap)
  :components ((:module "test"
                :components ((:file "package")
                             (:file "suite" :depends-on ("package"))
                             (:file "util" :depends-on ("suite"))
                             (:file "zlib" :depends-on ("suite"))))))
