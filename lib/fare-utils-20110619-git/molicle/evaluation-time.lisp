#+xcvb (module (:depends-on "pkgdcl"))

(in-package :molicle)

(eval-now
(defclass evaluation-time ()
  ((name :initarg :name :reader name)))

(macrolet ((def (name)
             `(defconstant* ,name
                  (make-instance 'evaluation-time :name ',name))))
  (def +read-time+)
  (def +compile-time+)
  (def +load-time+)
  (def +load-source-time+)
  (def +run-time+)
  (def +coffee-time+))

(defvar *evaluation-time* nil)
);eval-now

(defun equal-time (time1 time2)
  (check-type time1 evaluation-time)
  (check-type time2 evaluation-time)
  (cond
    ((not (equal (name time1) (name time2)))
     nil)
    ((not (eq time1 time2))
     (warn "interestingly there or two different same-named time ~S and ~S" time1 time2)
     t)
    (t t)))

(defmacro check-evaluation-time (&optional time)
  `(progn
     (check-type *evaluation-time* evaluation-time)
     ,(when time `(assert (equal-time ,time *evaluation-time*)))))

(defun call-with-evaluation-time (time thunk)
  (let ((*evaluation-time* time))
    (check-evaluation-time)
    (funcall thunk)))

(defmacro with-evaluation-time ((time) &body body)
  `(call-with-evaluation-time ,time #'(lambda () ,@body)))

(defmacro initialize-evaluation-time ()
  (check-evaluation-time +coffee-time+)
  (setf *evaluation-time* +read-time+)
  `(progn
#|     (eval-when (:execute)
       (warn "Thou shall compile molicles with COMPILE-MOLICLE. Thou shall not LOAD them directly as Lisp files"))|#
     (eval-when (:compile-toplevel)
       (setf *evaluation-time* +compile-time+))
     (eval-when (:load-toplevel)
       (setf *evaluation-time* +load-time+))))

(defmacro finalize-evaluation-time ()
  (check-evaluation-time)
  (setf *evaluation-time* +coffee-time+)
  '())

(defmethod print-object ((time evaluation-time) stream)
  (format stream "~S" (name time)))
