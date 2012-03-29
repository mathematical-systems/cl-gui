;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :common-lisp-user)

(defpackage :hu.dwim.asdf
  (:use :asdf
        :common-lisp)
  (:export #:find-and-load-swank-integration-systems
           #:hu.dwim.system
           #:hu.dwim.test-system
           #:hu.dwim.documentation-system
           #:system-pathname
           #:system-directory
           #:system-relative-pathname
           #:system-package-name
           #:system-test-name
           #:system-test-system-name
           #:system-documentation-system-name
           #:system-compile-output
           #:system-load-output
           #:system-test-result
           #:system-test-output
           #:develop-op
           #:develop-system
           #:*load-as-production?*
           #:debug-only
           #:debug-only*
           #:production-only
           #:production-only*
           #:optimize-declaration
           #:with-muffled-boring-compiler-warnings
           #:with-ignored-boring-compiler-warnings
           #:*workspace-directory*
           #:initialize-asdf-source-registry
           #:iterate-system-dependencies
           #:map-asdf-source-registry-directories
           #:do-system-dependencies
           #:map-system-dependencies
           #:collect-system-dependencies
           #:find-system
           #:load-system
           #:test-system
           #:run-test-suite))

(in-package :hu.dwim.asdf)

(assert (find-symbol "LOAD-SYSTEM" :asdf) () "Your asdf is too old, we need a version that exports ASDF:LOAD-SYSTEM")
