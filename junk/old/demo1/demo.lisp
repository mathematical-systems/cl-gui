(in-package :cl-gui)


(defclass nodes-vis ()
  ((graph :initarg :graph :initform nil :accessor graph-of))
  (:metaclass js-class))

(defun load-data ()
  (make-instance 'nodes-vis
                 :graph (with-open-file (f "data.lisp")
                          (read f))
                 :_id "vis-data"))


;; TODO: change object
