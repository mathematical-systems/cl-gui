;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.asdf)

#+sbcl
(progn
  ;; (sb-ext:restrict-compiler-policy 'safety 2)
  (pushnew :iolib-debug *features*)
  ;; ALIEN-FUNCALL-SAVES-FP-AND-PC helps to get uncut backtraces even in alien calls
  #+#.(hu.dwim.asdf::if-symbol-exists "SB-C" "ALIEN-FUNCALL-SAVES-FP-AND-PC")
  (sb-ext:restrict-compiler-policy 'sb-c::alien-funcall-saves-fp-and-pc 3))

;;;;;;
;;; Production support

(defvar *load-as-production?* nil
  "When T, load the lisp files so that it will be used in a production system. This means that debug-only blocks are dropped, some hot functions are optimized, compile time and runtime log levels and various other variables are initialized accordingly.")

(defmacro debug-only (&body body)
  (if *load-as-production?*
      (values)
      `(progn
         ,@body)))

(defmacro debug-only* (&body body)
  `(if *load-as-production?*
       (values)
       (progn
         ,@body)))

(defmacro production-only (&body body)
  (if *load-as-production?*
      `(progn
         ,@body)
      (values)))

(defmacro production-only* (&body body)
  `(if *load-as-production?*
       (progn
         ,@body)
       (values)))

(defun optimize-declaration ()
  (if *load-as-production?*
      '(optimize (speed 3) (debug 0) (safety 0))
      '(optimize (debug 3) (safety 3))))
