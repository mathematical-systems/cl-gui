;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.syntax-sugar.unicode)

(defun Σ (list)
  (reduce #'+ list))

(defun Π (list)
  (reduce #'* list))

(defmacro √ (number)
  `(sqrt ,number))

(defmacro ∛ (number)
  `(expt ,number 1/3))

(defmacro ∜ (number)
  `(expt ,number 1/4))

(defmacro λ (args &body body)
  "Macro on the lambda unicode character expanding to CL:LAMBDA and ignoring arguments named by a single '_' character."
  (bind ((ignored-symbols (list))
         (modified-args (mapcar (lambda (el)
                                  (if (string-equal '_ el)
                                      (let ((el (gensym)))
                                        (push el ignored-symbols)
                                        el)
                                      el))
                                args)))
    `(lambda ,modified-args
       ,@(when ignored-symbols
           `((declare (ignore ,@ignored-symbols))))
       ,@body)))
