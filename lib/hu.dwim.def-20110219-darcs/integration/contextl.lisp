;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(def (definer :available-flags "eod") contextl:layered-method ()
  (function-like-definer contextl:define-layered-method))

(def (definer :available-flags "eod") contextl:layered-function ()
  (bind ((body (nthcdr 2 -whole-))
         (name (pop body))
         (outer-declarations (function-like-definer-declarations -options-)))
    `(locally
         (declare ,@outer-declarations)
       ,@(when (getf -options- :export)
               `((export ',name)))
       (contextl:define-layered-function ,name ,@body))))

(def (definer e :available-flags "eod") layered-methods ()
  (defmethods-like-definer contextl:define-layered-method))

(def (definer e :available-flags "eas") layer (name &optional supers slots &rest class-options)
  (with-class-definer-options name slots
    `(contextl::deflayer ,name ,supers
       ,slots
       ,@class-options)))

(dolist (symbol '(contextl:layered-method
                  contextl:layered-function))
  (integrated-export symbol :hu.dwim.def))
