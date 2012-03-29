;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.defclass-star)

(unless (assoc "HU.DWIM.DEFCLASS-STAR" swank:*readtable-alist* :test #'string=)
  (let ((*readtable* (copy-readtable)))
    (enable-sharp-boolean-syntax)
    (push (cons "HU.DWIM.DEFCLASS-STAR" *readtable*) swank:*readtable-alist*)
    (push (cons "HU.DWIM.DEFCLASS-STAR.TEST" *readtable*) swank:*readtable-alist*)))
