;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.asdf)

(defun if-symbol-exists (package name)
  (if (and (find-package (string package))
                  (find-symbol (string name) (string package)))
             '(:and)
             '(:or)))
