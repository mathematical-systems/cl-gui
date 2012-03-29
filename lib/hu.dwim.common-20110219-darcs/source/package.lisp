;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :common-lisp-user)

(defpackage :hu.dwim.common
  (:use :alexandria
        :anaphora
        :closer-mop
        :hu.dwim.common-lisp
        :iterate
        :metabang-bind)
  (:shadow #:dolist
           #:in-package)
  (:shadowing-import-from :closer-mop
                          #:defgeneric
                          #:defmethod
                          #:ensure-generic-function
                          #:find-method
                          #:remove-method
                          #:standard-class
                          #:standard-method
                          #:standard-generic-function)
  (:export #:dolist
           #:export-external-symbols
           #:export-external-symbols-of-used-packages
           #:import-all-owned-symbols
           #:in-package))

(defmacro hu.dwim.common:in-package (name)
  ;; this is a placeholder macro that will be redefined when hu.dwim.def is also loaded
  `(common-lisp:in-package ,name))
