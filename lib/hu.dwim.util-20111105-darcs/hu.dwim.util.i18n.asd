;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.util.i18n
  :class hu.dwim.system
  :depends-on (:hu.dwim.util
               :cl-l10n)
  :components ((:module "source"
                :components ((:file "i18n")))))
