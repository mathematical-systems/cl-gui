;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.defclass-star)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :hu.dwim.def :hu.dwim.defclass-star))

(defun build-defclass-like-hu.dwim.def-expansion (name supers slots class-options -options- expansion-builder)
  (build-defclass-like-expansion name supers slots class-options expansion-builder
                                 :export-class-name (getf -options- :export
                                                          *export-class-name-p*)
                                 :export-accessor-names (getf -options- :export-accessor-names
                                                              *export-accessor-names-p*)
                                 :export-slot-names (getf -options- :export-slot-names
                                                          *export-slot-names-p*)))

(def (definer :available-flags "eas") class* (name supers slots &rest class-options)
  (build-defclass-like-hu.dwim.def-expansion
   name supers slots class-options -options-
   (lambda (processed-slots clean-options)
     `(defclass ,name ,supers
        ,processed-slots
        ,@clean-options))))

(def (definer :available-flags "eas") condition* (name supers slots &rest class-options)
  (build-defclass-like-hu.dwim.def-expansion
   name supers slots class-options -options-
   (lambda (processed-slots clean-options)
     `(define-condition ,name ,supers
        ,processed-slots
        ,@clean-options))))

(integrated-export 'class* :hu.dwim.def)
(integrated-export 'condition* :hu.dwim.def)

#|

(def (class* eas) foo (super)
  ((slot1 42 :export :slot)))

|#
