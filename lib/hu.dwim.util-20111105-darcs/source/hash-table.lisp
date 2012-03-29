;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

;;;;;;
;;; Generic hash table access

(def (generic e) hash-key (object)
  (:method (object)
    object))

(def (function ioe) hash-value (key hashtable)
  (gethash (hash-key key) hashtable))

(def (function ioe) (setf hash-value) (value key hashtable)
  (setf (gethash (hash-key key) hashtable) value))

(def (function ioe) remove-hash-value (key hashtable)
  (remhash (hash-key key) hashtable))
