(in-package :cl-gui)

(defglobal .events. (make-hash-table :test 'equal))
(defglobal .no-such-event-handler.
    (lambda (name args)
      (warn "No such event: ~a, args: ~a" name args)))

(defun register-event (name function)
  (check-type name string)
  (setf (gethash name .events.) function))

(defun unregister-event (name)
  (check-type name string)
  (remhash name .events.))

(defun unregister-all-events ()
  (clrhash .events.))

;;;
(defun handle-event (event args)
  (check-type event string)
  (let ((fn (gethash event .events.)))
    (if (null fn)
        (funcall .no-such-event-handler. event args)
        (apply fn event args))))

