(in-package :cl-gui)

(defun ensure-pathname (thing)
  (if (typep thing 'logical-pathname)
      (translate-logical-pathname thing)
      (pathname thing)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass ps-file (asdf:source-file)
    ((type :initform "ps"))))

(defmethod asdf:perform ((op asdf:compile-op) (c ps-file))
  (let* ((source-file (ensure-pathname (asdf:component-pathname c)))
         (output-file (merge-pathnames (make-pathname :type "js") source-file)))
    (compile-ps-file (asdf:component-pathname c) output-file)))

(defmethod asdf:perform ((op asdf:load-op) (c ps-file))
  (declare (ignorable op c)))

;; Allow for naked :ps-file in asdf definitions.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (find-class 'asdf::ps-file) (find-class 'ps-file)))

