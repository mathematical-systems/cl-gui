;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.syntax-sugar)

(export '(with-package with-readtable-case with-preserved-readtable-case))

(define-syntax readtime-wrapper (&optional (start-character #\{) (end-character #\}))
  "A utility read macro for modifying the read table.

The syntax is:

  {SPECIFIER ...}

Where SPECIFIER is either a symbol naming a function (available at read time) or a list (SPECIFIER &rest ARGUMENTS). SPECIFIER is applied to ARGUMENTS to produce a function, this is then called and passed another function which reads until the #\} character. During the executen of the function *readtable* is bound to a copy of the current read table."
  (set-macro-character start-character (make-readtime-wrapper-reader end-character) t *readtable*))

(defun make-readtime-wrapper-reader (end-character)
  (named-lambda readtime-wrapper-reader (stream char)
    (declare (ignore char))
    (bind ((*toplevel-readtable* (or *toplevel-readtable* *readtable*)))
      (with-local-readtable
        (bind ((form (with-local-readtable
                       ;; restore the readtable case of the toplevel readtable while reading the first form
                       (setf (readtable-case *readtable*) (readtable-case *toplevel-readtable*))
                       (ensure-list (read stream t nil t)))))
          (set-syntax-from-char end-character #\) *readtable*)
          (if *read-suppress*
              (progn
                (read-delimited-list end-character stream t)
                nil)
              (funcall (bind (#+sbcl(sb-ext:*evaluator-mode* :interpret))
                         (eval form))
                       (lambda ()
                         (read-delimited-list end-character stream t)))))))))

(defun with-package (package-name)
  "When used as a specifier for the READTIME-WRAPPER syntax, it locally rebinds at read time the current package to PACKAGE-NAME.

For example, this:

  {(with-package :common-lisp) t}

Will always read COMMON-LISP:T, no matter what the current package actually is."
  (lambda (reader)
    (bind ((*package* (find-package package-name))
           (result (funcall reader)))
      (if (consp result)
          (if (rest result)
              `(progn
                 ,@result)
              (first result))
          result))))

(defun with-readtable-case (case)
  "When used as a specifier for the READTIME-WRAPPER syntax, it changes the readtable-case while reading its body. Behaves as an implicit progn.

Example: {(with-readtable-case :preserve) 'fOo} => |fOo|"
  (lambda (reader)
    (with-local-readtable
      (setf (readtable-case *readtable*) case)
      (bind ((result (funcall reader)))
        (if (consp result)
            (if (rest result)
                `(progn
                   ,@result)
                (first result))
            result)))))

(defun with-preserved-readtable-case ()
  "See WITH-READTABLE-CASE."
  (with-readtable-case :preserve))
