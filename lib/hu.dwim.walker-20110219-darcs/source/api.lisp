;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.walker)

;;;;;;
;;; lisp forms -> AST

(def (layered-function e) walk-form (form &key parent environment)
  (:documentation "Entry point to initiate code walking of FORM using ENVIRONMENT. Returns a CLOS based AST that represents FORM.")
  (:method ((form cons) &key parent environment)
    (walk-form/compound (car form) form parent environment)))

(def (layered-function e) walk-form/compound (name form parent environment)
  (:documentation "Dispatches to a form-specific walker using the first symbol as operator name.")
  (:method ((name t) form parent environment)
    (bind (((operator &rest arguments) form))
      (when (member operator '(
                               ;; as copied from CLHS 3.1.2.1.2.1 Special Forms
                               block let* return-from catch load-time-value setq eval-when locally symbol-macrolet flet macrolet tagbody function
                               multiple-value-call the go multiple-value-prog1 throw if progn unwind-protect labels progv let quote
                               ;; some local extensions
                               declare))
        (error "No walker for the special operator ~S defined." operator))
      (walk-form/application form parent operator arguments environment))))

(def (layered-function e) walk-form/application (form parent operator arguments env))
(def (layered-function e) walk-form/lambda (form parent env))
(def (layered-function e) walk-form/lambda-like (ast-node args body env &key docstring-allowed declarations-allowed whole))

;;;;;;
;;; AST -> lisp forms

(def (layered-function e) unwalk-form (form)
  (:documentation "Unwalk FORM and return a list representation."))

(def (function e) unwalk-forms (forms)
  (mapcar #'unwalk-form forms))

;;;;;;
;;; customizing the walker

(def layered-function define-macro (defmacro-form)
  (:method (defmacro-form)
    (cerror "Ignore and continue" "Encountered a toplevel macro definition. It is not recorded by default, see ~S for details." 'define-macro)))

(def layered-function function-name? (name))
(def layered-function macro-name? (name &optional env))
(def layered-function symbol-macro-name? (name &optional env))
(def layered-function constant-name? (form &optional env))
(def layered-function lambda-form? (form &optional env))
(def layered-function walker-macroexpand-1 (form &optional env))

(def layered-function coerce-to-form (form)
  (:method (form)
    form))

;;;;;;
;;; customize warnings

(def (layer e) ignore-undefined-references ()
  ())

(def layered-function handle-undefined-reference (type name)
  (:method (type name)
    (ecase type
      (:function (warn 'undefined-function-reference :name name))
      (:variable (warn 'undefined-variable-reference :name name))))
  (:method :in ignore-undefined-references :around (type name)
    ;; well, we ignore them in this layer...
    ))
