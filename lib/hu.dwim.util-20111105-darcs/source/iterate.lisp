;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

;;;;;;
;;; Iterate extensions

(iterate::defclause (summing* expr &optional into var)
  "Sum into a variable when expr evaluates to non nil at least once, otherwise nil"
  (iterate::return-reduction-code :identity nil
                                  :operation '(lambda (sum value)
                                               (if sum
                                                   (if value
                                                       (+ sum value)
                                                       sum)
                                                   value))
                                  :expression expr
                                  :test nil
                                  :variable var
                                  :type 'number
                                  :accum-kind :increment))

(iterate::defclause (anding expr &optional into var)
  (iterate::return-reduction-code :identity #t
                                  :operation 'and
                                  :expression expr
                                  :test nil
                                  :variable var
                                  :type 'number
                                  :accum-kind :increment))

(iterate::defclause (oring expr &optional into var)
  (iterate::return-reduction-code :identity #f
                                  :operation 'or
                                  :expression expr
                                  :test nil
                                  :variable var
                                  :type 'number
                                  :accum-kind :increment))
