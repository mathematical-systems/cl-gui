;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Miscellaneous small utility definitions by Fare (formerly fare.lisp)

#+xcvb (module (:depends-on ("package")))

(in-package :fare-utils)

; -----------------------------------------------------------------------------
;;; Optimization settings

;(pushnew :do-test *features*) ; enable tests

(defmacro eval-now (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    ,@body))

(eval-now
  (when (or #+(or cmu sbcl ccl ecl harlequin-common-lisp allegro) t)
    (pushnew :declare-types *features*)))
#+declare-types
(progn
  (defmacro declaim-type (&rest specs)
    `(declaim
      ,@(mapcar #'(lambda (x) `(type .,x)) specs)))
  (defmacro declare-type (&rest specs)
    `(declare
      ,@(mapcar #'(lambda (x) `(type .,x)) specs)))
  (defmacro the* (type form) `(cl:the ,type ,form)))
#-declare-types
(progn
  (defmacro declaim-type (&rest specs) (declare (ignore specs)))
  (defmacro declare-type (&rest specs) (declare (ignore specs)))
  (defmacro the* (type form) (declare (ignore type)) form))

; -----------------------------------------------------------------------------
;;; Warnings
(eval-now
(defun style-warn (string &rest args)
  #-sbcl
  (apply #'warn string args)
  #+sbcl
  (apply #'sb-int:style-warn string args)))

; -----------------------------------------------------------------------------
;;; Helper functions for package management

(eval-now
(defparameter *package-misdefinition-warning-hook* #'style-warn)

(defun package-exported-symbols-symbol (p)
  (find-symbol "%%EXPORTED-SYMBOLS" p))

(defun package-exported-symbols-symbol! (p)
  (intern "%%EXPORTED-SYMBOLS" p))

(defun package-exported-symbols (p)
  (let ((s (package-exported-symbols-symbol p)))
    (and s (symbol-value s))))

(defun (setf package-exported-symbols) (new p)
  (let ((s (package-exported-symbols-symbol! p)))
    (setf (symbol-value s) new)))

(defun package-initialize-exported-symbols (p)
  (setf (package-exported-symbols p) (make-hash-table)))

(defun package-reset-exported-symbols (p)
  (let ((h (package-exported-symbols p)))
    (if (hash-table-p h)
        (clrhash h)
        (package-initialize-exported-symbols p))))

(defun package-ensure-exported-symbols (p)
  (or (let ((h (package-exported-symbols p)))
        (and (hash-table-p h) h))
      (package-initialize-exported-symbols p)))

(defun package-symbol-declared-exported-p (s &optional (p *package*))
  (let ((h (package-exported-symbols p)))
    (and (hash-table-p h)
         (gethash s h))))

(defun package-symbol-actually-exported-p (s &optional (p *package*))
  (multiple-value-bind (x status) (find-symbol (symbol-name s) p)
    (and (eq s x)
         (eq status :external))))


(defun package-ensure-symbol-exported (s &optional (p *package*))
  (unless (package-symbol-actually-exported-p s p)
    (when *package-misdefinition-warning-hook*
      (funcall *package-misdefinition-warning-hook*
               "Symbol ~W declared as exported from package ~A but isn't"
               s (package-name (find-package p)))))
  (export s p)
  (setf (gethash s (package-ensure-exported-symbols p)) t)
  t))

(defmacro export-symbols* (p &rest symbols)
  `(eval-now
    ,@(loop :for s :in symbols :collect
            `(package-ensure-symbol-exported ',s ',p))))

(defmacro export* (s &optional (p *package*))
  `(export-symbols* ,(package-name (find-package p)) ,s))

(defmacro export-symbols (&rest symbols)
  `(export-symbols* ,(package-name *package*) ,@symbols))

(defmacro exporting-definitions (&body body)
  `(progn
    ,@(loop :for def :in body
            :collect def
            :nconc
            (block nil
              (let* ((def1 (if (and (consp def) (consp (cdr def))) (car def) (return)))
                     (d1s (if (symbolp def1) (string def1) (return)))
                     (def2 (if (and (<= 3 (length d1s)) (equal "DEF" (subseq d1s 0 3)))
                               (cadr def)))
                     (sym (cond
                            ((symbolp def2) def2)
                            ((and (consp def2) (eq 'SETF (car def2))
                                  (consp (cdr def2)) (null (cddr def2))
                                  (symbolp (cadr def2)))
                             (cadr def2))
                            (t (return)))))
                (when sym (list `(export* ,sym))))))))

(export-symbols
 eval-now
 declaim-type declare-type the*
 style-warn
 *package-misdefinition-warning-hook*
 export-symbols* export* export-symbols
 exporting-definitions define-exporter)

(exporting-definitions

(defmacro define-exporter (exporter definer)
  `(defmacro ,exporter (symbol &rest args)
    `(progn
      (,',definer ,symbol ,@args)
      (export* ,symbol))))

(define-exporter define*-exporter define-exporter)

(define-exporter def*class defclass)
(define-exporter def*constant defconstant)
(define-exporter def*generic defgeneric)
(define-exporter define*-compiler-macro define-compiler-macro)
(define-exporter define*-condition define-condition)
(define-exporter define*-method-combination define-method-combination)
(define-exporter define*-modify-macro define-modify-macro)
(define-exporter define*-setf-expander define-setf-expander)
(define-exporter define*-symbol-macro define-symbol-macro)
(define-exporter def*macro defmacro)
(define-exporter def*method defmethod)
(define-exporter def*package defpackage)
(define-exporter def*parameter defparameter)
(define-exporter def*setf defsetf)
(define-exporter def*struct defstruct)
(define-exporter def*type deftype)
(define-exporter def*fun defun)
(define-exporter def*var defvar))

(eval-now
(exporting-definitions

(defun featurify (bool)
  (if #+common-lisp bool #-common-lisp (not bool)
      'common-lisp '(not common-lisp)))
(defun unfeaturify (bool-feature)
  (equal bool-feature (featurify t)))))

(exporting-definitions

(defun nilf (&rest args)
  (declare (ignore args))
  nil)

(defun NIY (&rest args)
  "fallback function for unimplemented functionality"
  (error "Not Implemented Yet~@[: ~S~]" args))

(defun quit-lisp ()
  "Quit the lisp implementation"
  #+cmu (extensions:quit)
  #+sbcl (sb-ext:quit)
  #+clisp (ext:quit)
  #+allegro (excl:exit)
  #+lispworks (lispworks:quit)
  #+ccl (ccl:quit)
  #+genera (error "You probably don't want to Halt the Machine.")
  #-(or cmu sbcl clisp allegro lispworks ccl genera) (NIY))

)
