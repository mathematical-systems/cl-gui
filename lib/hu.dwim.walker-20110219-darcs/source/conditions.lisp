;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.walker)

(def (condition* e) walker-condition (condition)
  ((enclosing-code *current-form*)))

(def (condition* e) walker-error (walker-condition error)
  ())

(def (condition* e) simple-walker-error (simple-error walker-error)
  ())

(def function simple-walker-error (message &rest args)
  (error 'simple-walker-error :format-control message :format-arguments args))

(def (condition* e) walker-warning (walker-condition warning)
  ())

(def (condition* e) walker-style-warning (walker-warning
                                          style-warning)
  ())

(def (condition* e) simple-walker-style-warning (walker-style-warning
                                                 simple-style-warning)
  ())

(def function simple-walker-style-warning (format-control &rest format-arguments)
  (warn 'simple-walker-style-warning :format-control format-control :format-arguments format-arguments))

(def (condition* e) undefined-reference (walker-style-warning)
  ((name)))

(def (condition* e) undefined-variable-reference (undefined-reference)
  ()
  (:report
   (lambda (c stream)
     (if (enclosing-code-of c)
         (format stream "Reference to unknown variable ~S in ~S." (name-of c) (enclosing-code-of c))
         (format stream "Reference to unknown variable ~S." (name-of c))))))

(def (condition* e) undefined-function-reference (undefined-reference)
  ()
  (:report
   (lambda (c stream)
     (if (enclosing-code-of c)
         (format stream "Reference to unknown function ~S in ~S." (name-of c) (enclosing-code-of c))
         (format stream "Reference to unknown function ~S." (name-of c))))))
