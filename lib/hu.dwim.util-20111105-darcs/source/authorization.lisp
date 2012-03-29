;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

;;;;;;
;;; Definer

(def (namespace e) authorization)

(def (definer e) authorization (name arguments &body body)
  `(progn
     (def function ,name ,arguments
       ,@body)
     (setf (find-authorization ',name) (fdefinition ',name))))

(def (definer e) authorization/function-call (name arguments &body body)
  `(def authorization ,name (-form-)
     (bind ((-name- (car -form-))
            ,@(when arguments
               `((,arguments (cdr -form-)))))
       (declare (ignorable -name-))
       ,@body)))

;;;;;;
;;; Partial eval

(def layer authorization-layer (hu.dwim.partial-eval:standard-partial-eval-layer)
  ())

(def layered-method hu.dwim.partial-eval:inline-function-call? :in authorization-layer ((ast hu.dwim.walker:free-application-form) name arguments)
  (or (find-authorization name :otherwise #f)
      (contextl:call-next-layered-method)))

(def function partial-eval-authorization (form &rest args &key &allow-other-keys)
  (apply #'hu.dwim.partial-eval:partial-eval form
         :layer 'authorization-layer
         :inline-functions '(sb-kernel::keyword-supplied-p sb-kernel::lookup-keyword)
         args))
