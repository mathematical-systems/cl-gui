;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.defclass-star)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :hu.dwim.def :hu.dwim.defclass-star))

(def (definer :available-flags "eas") layer* (name supers slots &rest class-options)
  (build-defclass-like-hu.dwim.def-expansion
   name supers slots class-options -options-
   (lambda (processed-slots clean-options)
     `(contextl:deflayer ,name ,supers
        ,processed-slots
        ,@clean-options))))

(integrated-export 'layer* :hu.dwim.def)

#|
(def (layer* eas) foo (super)
  ((slot1 42 :export :slot)))
|#
