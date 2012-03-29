;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Magic special variables: kind of like define-symbol-macro,
;;;;; but with cacheing (by default, read but not write)

#+xcvb (module (:depends-on ("package" "base/macros" "base/hash-tables")))

(in-package :fare-utils)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (hashmacro msv-named))

(defun make-msv-cache ()
  (make-hash-table :test 'eq))
(defvar *msv-cache* (make-msv-cache))
(defstruct (magic-special-variable-cell
	    (:constructor make-msv)
	    (:conc-name :msv-))
  name
  place
  initform
  read
  write
  getter
  setter
  read-cache
  write-cache)
(defstruct (magic-special-variable-cache
	    (:constructor make-msvc)
	    (:conc-name :msvc-)
	    (:type vector))
  (value nil)
  (writtenp nil))
(defun do-write-msv (msv value)
  (funcall (msv-setter msv) value))
(defun do-read-msv (msv)
  (funcall (msv-getter msv)))
(defun msv-cache (msv)
  (gethash msv *msv-cache*))
(defun do-make-msv-cache (msv)
  (setf (gethash msv *msv-cache*) (make-msvc)))
(defun ensure-msv-cache (msv)
  (or (msv-cache msv) (do-make-msv-cache msv)))
(defun do-flush-msv-write-cache (msv)
  (let ((msvc (msv-cache msv)))
    (when (and msvc (msvc-writtenp msvc))
      (do-write-msv msv (msvc-value msvc))
      (setf (msvc-writtenp msvc) nil))))
(defun do-invalidate-msv-cache (msv)
  (remhash msv *msv-cache*))
(defun do-flush-and-invalidate-msv-cache (msv)
  (do-flush-msv-write-cache msv)
  (do-invalidate-msv-cache msv))
(defun do-fill-msv-read-cache (msv)
  (let* ((cache (ensure-msv-cache msv)))
    (setf (msvc-writtenp cache) nil
	  (msvc-value cache) (do-read-msv msv))))
(defun msv-get (msv)
  (let ((msvc (msv-cache msv)))
    (if msvc
	(msvc-value msvc)
      (if (msv-read-cache msv)
	  (do-fill-msv-read-cache msv)
	(do-read-msv msv)))))
(defun msv-set (msv value)
  (if (msv-write-cache msv)
      (let ((msvc (ensure-msv-cache msv)))
	(setf (msvc-writtenp msvc) t
	      (msvc-value msvc) value))
    (let ((msvc (msv-cache msv)))
      (when msvc
	(setf (msvc-writtenp msvc) nil
	      (msvc-value msvc) value))
      (do-write-msv msv value))))
(defun (setf msv-get) (value msv)
  (msv-set msv value))
(defmacro magic-special-variable (name)
  `(msv-get (msv-named ',name)))
#| ;;; how can this be done properly?
(defmacro (setf magic-special-variable) (value name)
  `(msv-set (msv-named ',name) ,value))
|#
(defun flush-all-msv ()
  (maphash #'(lambda (msv msvc)
	       (declare (ignore msvc))
	       (do-flush-msv-write-cache msv))
	   *msv-cache*))
(defun flush-and-invalidate-all-msv ()
  (maphash #'(lambda (msv msvc)
	       (declare (ignore msvc))
	       (do-flush-and-invalidate-msv-cache msv))
	   *msv-cache*))
(defun register-msv (name &rest rest)
  (msv-set name (apply 'make-msv :name name rest)))
(defmacro register-magic-special-variable
    (name place &key (initform '_)
		     ((:read msv-read) :cache)
		     ((:write msv-write) t))
  `(register-msv
    ',name
    :place ',place
    :initform ',initform
    :read ',msv-read
    :write ',msv-write
    :getter ,(if msv-read
		 `(symbol-macrolet ((_ ,place))
		      #'(lambda () ,initform))
	       `#'(lambda ()
		    (error ,(format nil "magic variable ~A unbound" name))))
    :setter ,(if msv-write
		 `#'(lambda (x) (setf ,place x))
	       `#'(lambda (x)
		    (declare (ignore x))
		    (error ,(format nil "magic variable ~A unbound" name))))
    :read-cache ,(eq msv-read :cache)
    :write-cache ,(eq msv-write :cache)))
(defmacro define-magic-special-variable
  (name place &rest rest)
  `(progn
     (register-magic-special-variable ,name ,place ,@rest)
     (define-symbol-macro ,name (magic-special-variable ',name))))
(defmacro with-magic-special-variables (&body body)
  `(let ((*msv-cache* (make-msv-cache)))
     (unwind-protect
	 (progn ,@body)
       (flush-all-msv))))
(defmacro with-magic-special-variables-safely (&body body)
  `(progn
     (flush-all-msv)
     (with-magic-special-variables ,@body)))
#-genera
(defmethod make-load-form ((msv magic-special-variable-cell) &optional environment)
   (declare (ignore environment))
   `(register-magic-special-variable
	,(msv-name msv) ,(msv-place msv)
      :read ,(msv-read msv)
      :write ,(msv-write msv)))
(define-abbrevs
    defmsv define-magic-special-variable
  with-msv with-magic-special-variable
  with-msv* with-magic-special-variable-safely)

#| "

A better version would perhaps
(1) individuate magic variable cacheing:
A magic variable's cache would be just the symbol-value of the variable.
(said variable being special indeed)
(2) dependency tracking would help make fresh bindings out of all
variables listed and those they depend upon
(problem: unless we have a typing system for functions being called,
we can't deduce from the syntactic content of a function
the set of all magic special variables it depends upon)
Because of that problem, we're stuck with hash-table lookup for the cache,
though if we really want shallow binding, we could reimplement
dynamic rebinding based on unwind-protect or so (ouch).

Alternatively, it could define symbol-macros that optimize away checks,
lookups and indirections at macro-expansion time.
If such optimizations were often needed, it would nice
to have a partial evaluator generate them automatically - but I digress)

" |#
