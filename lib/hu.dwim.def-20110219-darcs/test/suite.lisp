;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def.test)

(defsuite* (test :in root-suite))

;; we need to define a local transformer, because the default one depends on the value
;; of the environment (*LOAD-AS-PRODUCTION?*, and declaimed debug level).
(defun transform-function-definer-options (options)
  (bind ((debug-level (normalize-debug-level (getf options :debug 0))))
    (when (> debug-level 0)
      (remove-from-plistf options :inline :optimize))
    (list* :debug debug-level
           (remove-from-plist options :debug))))

(deftest test/function ()
  (is (equal '(progn
               (declaim (notinline foo))
               (locally (declare (optimize (speed 0) (debug 3)))
                 (eval-when (:compile-toplevel :load-toplevel :execute)
                   (export 'foo))
                 (defun foo (bar baz unused)
                   "documentation"
                   (declare (ignore unused))
                   (+ bar baz))))
             (macroexpand-1 '(def (function ioed) foo (bar baz unused)
                              "documentation"
                              (declare (ignore unused))
                              (+ bar baz))))))

(deftest test/method ()
  (is (equal '(progn
               (declaim (notinline foo))
               (locally (declare (optimize (speed 0) (debug 3)))
                 (eval-when (:compile-toplevel :load-toplevel :execute)
                   (export 'foo))
                 (common-lisp:defmethod foo ((bar integer) (baz string) unused)
                   "documentation"
                   (declare (ignore unused))
                   (+ bar baz))))
             (macroexpand-1 '(def (method oed) foo ((bar integer) (baz string) unused)
                              "documentation"
                              (declare (ignore unused))
                              (+ bar baz))))))

(deftest test/test ()
  (is (equal '(progn
               (locally
                   (declare)
                 (deftest foo (bar baz unused)
                   "documentation"
                   (declare (ignore unused))
                   (+ bar baz))))
             (macroexpand-1 '(def test foo (bar baz unused)
                              "documentation"
                              (declare (ignore unused))
                              (+ bar baz))))))

(deftest test/constant ()
  (is (equal '(progn
               (eval-when (:compile-toplevel :load-toplevel :execute)
                 (defconstant +foo+ (hu.dwim.def::%reevaluate-constant '+foo+ 1 :test 'equal)
                   "documentation")))
             (macroexpand-1 '(def constant +foo+ 1 "documentation")))))

(deftest test/special-variable ()
  (is (equal '(progn
               (progn
                 (setf (documentation '+foo+ 'variable) "documentation")
                 (defvar +foo+)
                 (makunbound '+foo+)
                 (setf +foo+ 1)))
             (macroexpand-1 '(def special-variable +foo+ 1 "documentation"))))
  (is (equal '(progn
               (progn
                 (setf (documentation '+foo+ 'variable) "documentation")
                 (defvar +foo+)
                 (makunbound '+foo+)))
             (macroexpand-1 '(def (special-variable :documentation "documentation") +foo+)))))
