(in-package :cl-gui)

(defglobal .js-object-counter. 0)
(defglobal .js-objects. nil)

(defun init-js-object-pool (&key (size 100))
  (setf .js-object-counter. 0
        .js-objects. (make-hash-table :size size :test 'equal)))

(eval-when (:load-toplevel :execute)
  (init-js-object-pool))

(defun add-js-object (js-obj)
  (let ((id (_id-of js-obj)))
    (setf (gethash id .js-objects.) js-obj)))

(defun del-js-object (id)
  (remhash id .js-objects.))

(defun get-js-object (id)
  (gethash id .js-objects.))

(defun map-js-objects (fn)
  (maphash-values fn .js-objects.))

(defun update-js-object-slots (data)
  (let* ((id (get-value-in-data :--id data))
         (js-obj (when id (get-js-object id))))
    (if js-obj
        (mapc-data (lambda (k v)
                     (setf (slot-value js-obj (symbolicate (symbol-name k))) v))
                   data)
        (error "Cannot find js-object of id: ~a" id))))

