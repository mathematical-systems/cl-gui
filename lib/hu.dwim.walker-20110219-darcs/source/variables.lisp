;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.walker)

(def special-variable *current-form* nil)
(def special-variable *inside-macroexpansion* nil)

(def type variable-name ()
  '(and symbol
        (not keyword)
        (not (member nil t))))
