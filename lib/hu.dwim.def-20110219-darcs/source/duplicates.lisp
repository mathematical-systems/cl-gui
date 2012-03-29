;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

;;; THE CONTENT OF THIS FILE IS COPIED OVER FROM SOME OTHER LIBRARIES TO DECREASE DEPENDENCIES

(defmacro defprint-object (&whole whole (self class-name &key (identity t) (type t) with-package (muffle-errors t))
                           &body body)
  "Define a print-object method using print-unreadable-object.
  An example:
  (defprint-object (self parenscript-dispatcher)
    (when (cachep self)
      (princ \"cached\")
      (princ \" \"))
    (princ (parenscript-file self)))"
  (with-unique-names (stream printing)
    (multiple-value-bind (body declarations documentation) (parse-body body :documentation t :whole whole)
      `(defmethod print-object ((,self ,class-name) ,stream)
         ,@(when documentation
             (list documentation))
         ,@declarations
         (print-unreadable-object (,self ,stream :type ,type :identity ,identity)
           (let ((*standard-output* ,stream))
             (block ,printing
               (,@(if muffle-errors
                      `(handler-bind ((error (lambda (error)
                                               (declare (ignore error))
                                               (write-string "<<error printing object>>")
                                               (return-from ,printing)))))
                      `(progn))
                  (let (,@(when with-package `((*package* ,(find-package with-package)))))
                    ,@body)))))
         ;; primary PRINT-OBJECT methods are supposed to return the object
         ,self))))

(defun integrated-export (symbol other-package)
  "Export SYMBOL from both its own package and OTHER-PACKAGE"
  (dolist (symbol (ensure-list symbol))
    (export symbol (symbol-package symbol))
    (shadowing-import symbol other-package)
    (export symbol other-package)))

;; from contextl
(locally #+sbcl(declare (sb-ext:muffle-conditions style-warning sb-ext:compiler-note))
(defun make-lock (&key name)
  (or
   #+allegro (mp:make-process-lock :name name)
   #+clozure-common-lisp (ccl:make-lock name)
   #+(and cmu mp) (mp:make-lock name)
   #+ecl (mp:make-lock :name name)
   #+lispworks (mp:make-lock :name name)
   #+(and sbcl sb-thread) (sb-thread:make-mutex :name name)
   (error "Threading on your lisp is not supported in hu.dwim.def")))

(defmacro with-lock (lock &body body)
  (or
   #+allegro `(mp:with-process-lock (,lock) ,@body)
   #+clozure-common-lisp `(ccl:with-lock-grabbed (,lock) ,@body)
   #+(and cmu mp) `(mp:with-lock-held (,lock) ,@body)
   #+ecl `(mp:with-lock (,lock) ,@body)
   #+lispworks `(mp:with-lock (,lock) ,@body)
   #+(and sbcl sb-thread) `(sb-thread:with-recursive-lock (,lock) ,@body)
   (error "Threading on your lisp is not supported in hu.dwim.def")))
)
