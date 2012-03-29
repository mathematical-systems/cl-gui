;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.walker.test
  :class hu.dwim.test-system
  :depends-on (:hu.dwim.stefil+hu.dwim.def
               :hu.dwim.stefil+swank
               :hu.dwim.util.temporary-files
               :hu.dwim.walker)
  :components ((:module "test"
                :components ((:file "ast" :depends-on ("package"))
                             (:file "lexenv" :depends-on ("package"))
                             (:file "macros" :depends-on ("package" "walk-unwalk"))
                             (:file "package")
                             (:file "walk-unwalk" :depends-on ("package" "lexenv"))
                             (:file "semantics" :depends-on ("package" "lexenv"))))))
