;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (assert (not (eq 'hu.dwim.common:in-package 'common-lisp:in-package))))

(def function %eval-extended-package-readtable-setup-form (package-name)
  (bind ((extended-package (find-extended-package package-name :otherwise nil))
         (*package* (find-package :cl-user)))
    (awhen (and extended-package
                (readtable-setup-form-of extended-package))
      ;; external forces make sure that *readtable* is rebound, but not that it's copied. keep that in mind when using :setup-readtable for (def package ...)
      (eval it))))

(locally
    (declare #+sbcl(sb-ext:muffle-conditions sb-kernel:redefinition-warning))
  (def macro hu.dwim.common:in-package (package-name)
    `(progn
       (eval-when (:compile-toplevel :execute)
         (%eval-extended-package-readtable-setup-form ,(string package-name)))
       (common-lisp:in-package ,package-name))))
