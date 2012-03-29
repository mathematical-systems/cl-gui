;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :common-lisp-user)

(defpackage :hu.dwim.def
  (:use :alexandria
        :anaphora
        :common-lisp
        :hu.dwim.asdf
        :iterate
        :metabang-bind)
  (:export #:def
           #:definer
           #:find-definer
           #:with-standard-definer-options
           #:function-like-definer-declarations
           ;; lexically visible non-hygienic names
           #:-child-
           #:-children-
           #:-content-
           #:-recurse-
           #:-whole-
           #:-environment-
           #:-definer-
           #:-options-
           #:-name-
           #:-form-
           #:-body-
           #:-with-macro/body-
           #:-visitor-
           #:-self-))
