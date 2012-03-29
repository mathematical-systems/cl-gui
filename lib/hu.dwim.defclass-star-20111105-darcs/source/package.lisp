;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :common-lisp-user)

(defpackage :hu.dwim.defclass-star
  (:use :common-lisp)
  (:export #:defclass*
           #:defcondition*
           ;; transformers
           #:default-accessor-name-transformer
           #:dwim-accessor-name-transformer
           #:default-initarg-name-transformer
           #:default-slot-definition-transformer
           #:make-name-transformer
           #:*allowed-slot-definition-properties*
           ;; more or less public vars (it's discouraged to set them globally)
           #:*accessor-name-transformer*
           #:*automatic-accessors-p*
           #:*initarg-name-transformer*
           #:*automatic-initargs-p*))
