;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :common-lisp-user)

(defpackage :hu.dwim.def.test
  (:use :alexandria
        :anaphora
        :hu.dwim.asdf
        :hu.dwim.common
        :hu.dwim.def
        :hu.dwim.stefil
        :iterate
        :metabang-bind)
  (:shadow #:test))

(in-package :hu.dwim.def.test)

(import-all-owned-symbols :hu.dwim.def :hu.dwim.def.test)
