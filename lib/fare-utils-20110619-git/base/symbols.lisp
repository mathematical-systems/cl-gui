;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Basic Utilities about Symbols

#+xcvb (module (:depends-on ("package" "base/utils" "base/strings")))

(in-package :fare-utils)

(eval-now
(exporting-definitions

(defun conc-symbol-in (package &rest rest)
  "make a symbol by concatenating stuff"
  (intern (apply #'conc-string rest) (or package *package*)))

(defun conc-symbol (&rest rest)
  "make a symbol by concatenating stuff"
  (apply #'conc-symbol-in nil rest))

(defun conc-gensym (&rest rest)
  "make a gensym by concatenating stuff"
  (gensym (apply #'conc-string rest)))

(defun conc-keyword (&rest rest)
  "make a keyword by concatenating stuff"
  (apply #'conc-symbol-in :keyword rest))

(defun make-predicate-symbol (s)
  (let ((n (conc-string s)))
    (conc-symbol n (if (every #'alphanumericp n) :p :-p))))

))

#| ;;; I never actually used that
(defun do-begin-gensym* ()
  (make-package :tmp))
(defun do-end-gensym* ()
  (do-symbols (l :tmp) (unintern l :tmp))
  (delete-package :tmp))
(defun gensym* (&optional foo)
  (let ((s (->string (or foo '#:g))))
    (if (find-package :tmp)
	(gentemp s :tmp)
	(gensym s))))
(defmacro begin-gensym* ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (do-begin-gensym*)))
(defmacro end-gensym* ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (do-end-gensym*)))
|#
