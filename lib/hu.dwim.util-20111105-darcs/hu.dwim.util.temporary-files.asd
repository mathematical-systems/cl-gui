;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.util.temporary-files
  :class hu.dwim.system
  :description ""
  :depends-on (:hu.dwim.util+iolib
               :iolib.pathnames
               :iolib.os
               :iolib.syscalls)
  :components ((:module "source"
                :components ((:file "temporary-files")))))
