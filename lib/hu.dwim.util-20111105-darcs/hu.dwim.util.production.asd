;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.util.production
  :class hu.dwim.system
  :description "Various utilities, contains code for producing standalone executable services."
  :depends-on (:cl-fad
               :command-line-arguments
               :hu.dwim.logger
               :hu.dwim.util.error-handling
               :hu.dwim.util.temporary-files
               :hu.dwim.util+iolib
               :iolib.syscalls)
  :components ((:module "source"
                :components ((:file "production")
                             (:file "command-line")))))
