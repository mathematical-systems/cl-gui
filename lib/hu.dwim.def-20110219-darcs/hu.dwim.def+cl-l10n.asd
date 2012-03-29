;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.def+cl-l10n
  :class hu.dwim.system
  :depends-on (:cl-l10n
               :hu.dwim.def)
  :components ((:module "integration"
                :components ((:file "cl-l10n")))))
