(in-package :cl-gui)

;;;
(defmacro 1st (&rest args)
  (assert (> (length args) 0))
  (first args))

(defmacro defglobal (name value &optional doc)
  (1st #+sbcl `(sb-ext:defglobal ,name ,value ,doc)
       `(defvar ,name ,value ,doc)))

(defmacro ctime-error (&rest args)
  (apply #'error args))

(defmacro with-not-implemented-error (&body body)
  `(1st ,@body
        (ctime-error "Not yet ported for this Lisp implementation")))

