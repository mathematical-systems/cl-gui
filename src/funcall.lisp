(in-package :cl-gui)

(defglobal .functions. (make-hash-table :test 'equal))
(defglobal .no-such-function-handler.
    (lambda (name args)
      (warn "No such function: ~a, args: ~a" name args)))

(defun register-function (name function)
  (check-type name string)
  (setf (gethash name .functions.) function))

(defun unregister-function (name)
  (check-type name string)
  (remhash name .functions.))

(defun unregister-all-functions ()
  (clrhash .functions.))

;;;
(defun handle-function (function args)
  (check-type function string)
  (let ((fn (gethash function .functions.)))
    (if (null fn)
        (funcall .no-such-function-handler. function args)
        (apply fn function args))))

