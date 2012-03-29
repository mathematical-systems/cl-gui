;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

;;;;;;
;;; Place related

(def (macro e) notf (&rest places)
  `(setf ,@(iter (for place in places)
                 (collect place)
                 (collect `(not ,place)))))

(def (macro e) clearf (&rest places)
  `(setf ,@(iter (for place in places)
                 (collect place)
                 (collect nil))))

(def (function e) symbol-global-value (symbol)
  (check-type symbol (and symbol (not (member nil t))))
  #*((:sbcl (sb-ext:symbol-global-value symbol))
     (t #.(warn "~S is not implemented properly on your platform! This may lead to broken runtime behavior..." 'symbol-global-value)
        (symbol-value symbol))))

(def (function e) (setf symbol-global-value) (value symbol)
  (check-type symbol (and symbol (not (member nil t))))
  #*((:sbcl (setf (sb-ext:symbol-global-value symbol) value))
     (t #.(warn "~S is not implemented properly on your platform! This may lead to broken runtime behavior..." '(setf symbol-global-value))
        (setf (symbol-value symbol) vlue))))
