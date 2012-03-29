(in-package :molicle)

(defvar *initial-readtable* (copy-readtable))

(defclass molicle ()
  ((name :initarg :name :accessor name)
   (pathname :initarg :pathname :accessor molicle-pathname)
   (truename :initarg :truename :accessor molicle-truename)
   (fasl :initarg :fasl :accessor molicle-fasl)
   (state :initform nil :accessor state)
   (readtable :initform *readtable* :accessor molicle-readtable)
   (package :initform :cl-user :accessor molicle-package)
   (final-forms :initform nil :accessor final-forms)
   (post-compile-hook :initform nil :accessor post-compile-hook)))

(defvar *molicle* nil
  "current molicle")

(defun unnamed-molicle-p (molicle)
  (let ((name (name molicle)))
    (and (consp name) (eq :unnamed (first name)))))

(defmacro initialize-molicle (&rest keys &key name class package &allow-other-keys)
  (unless (typep *molicle* 'molicle)
    (error "INITIALIZE-MOLICLE used in improper context.
You must use #.[:BEGIN].# at the start of you molicle, and
compile or load your molicle with COMPILE-MOLICLE and LOAD-MOLICLE."))
  (when (state *molicle*)
    (error "molicle ~S already initialize" (name *molicle*)))
  (when name
    (if (unnamed-molicle-p *molicle*)
        (setf (name *molicle*) name)
        (assert (equal name (name *molicle*)) (name *molicle*))))
  (when package
    (setf (molicle-package *molicle*) (string package)))
  (when class
    (change-class *molicle* class :keys keys))
  (setf (state *molicle*) :compiling)
  `(in-package ,(molicle-package *molicle*)))

(defmacro finalize-molicle ()
  (setf (state *molicle*) :finalizing)
  `(progn
     ,@(final-forms *molicle*)
     (eval-when (:compile-toplevel)
       (setf (state *molicle*) :compiled))))
