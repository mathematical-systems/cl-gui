;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.defclass-star)

;;; THE CONTENT OF THIS FILE IS COPIED OVER FROM SOME OTHER LIBRARIES TO DECREASE THE NUMBER OF DEPENDENCIES

(defmacro enable-sharp-boolean-syntax ()
  "Copies *readtable* and enables #t and #f readers for t and nil in the copy."
  '(eval-when (:compile-toplevel :execute)
    (setf *readtable* (copy-readtable *readtable*))
    (%enable-sharp-boolean-syntax)))

(defun %enable-sharp-boolean-syntax ()
  (set-dispatch-macro-character
   #\# #\t
   (lambda (s c n)
     (declare (ignore s c n))
     t))
  (set-dispatch-macro-character
   #\# #\f
   (lambda (s c n)
     (declare (ignore s c n))
     nil))
  (values))

(defun concatenate-symbol (&rest args)
  "Args are processed as parts of the result symbol with two exceptions except when a package is encountered then it is stored as the target package at intern."
  (let* ((package nil)
         (symbol-name (string-upcase
                       (with-output-to-string (str)
                         (dolist (arg args)
                           (typecase arg
                             (string (write-string arg str))
                             (package (setf package arg))
                             (symbol (unless package
                                       (setf package (symbol-package arg)))
                                     (write-string (symbol-name arg) str))
                             (integer (write-string (princ-to-string arg) str))
                             (character (write-char arg) str)
                             (t (error "Cannot convert argument ~S to symbol" arg))))))))
    (if package
        (intern symbol-name package)
        (intern symbol-name))))

(defun strcat (&rest string-designators)
  (with-output-to-string (str)
    (dolist (s string-designators)
      (when s (princ s str)))))

(defun remove-keywords (plist &rest keywords)
  "Creates a copy of PLIST without the listed KEYWORDS."
  (declare (optimize (speed 3)))
  (loop for cell = plist :then (cddr cell)
        for el = (car cell)
        while cell
        unless (member el keywords :test #'eq)
        collect el
        and collect (cadr cell)
        and do (assert (cdr cell) () "Not a proper plist")))

(define-modify-macro remf-keywords (&rest keywords) remove-keywords
  "Removes the properties identified by KEYWORDS from PLACE.")

;; from hu.dwim.util
(defun fully-qualified-symbol-name (symbol &key separator)
  (let* ((symbol-name (symbol-name symbol))
         (package (symbol-package symbol))
         (keyword-package (load-time-value (find-package "KEYWORD"))))
    (concatenate 'string
                 (unless (eq package keyword-package)
                   (package-name package))
                 (or separator
                     (if (or (not (eq package keyword-package))
                             (not (eq (nth-value 1 (find-symbol symbol-name package)) :external)))
                         "::"
                         ":"))
                 symbol-name)))

(defun integrated-export (symbol other-package)
  (export symbol (symbol-package symbol))
  (import symbol other-package)
  (export symbol other-package))
