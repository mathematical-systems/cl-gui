;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;; Generic iteration

(in-package :stateful)

(defclass iterator ()
  ())

(defgeneric iterator-end-p (iterator))
(defgeneric iterator-next (iterator))


(defclass list-iterator (iterator)
  ((list :type list :accessor iterator-list)))
(defmethod iterator-end-p ((x list-iterator))
  (null (iterator-list x)))
(defmethod iterator-next ((x list-iterator))
  (pop (iterator-list x)))


(defclass vector-iterator (iterator)
  (vector :type vector)
  (start :type fixnum)
  (end :type fixnum))
(defmethod iterator-end-p ((x forward-vector-iterator))
  (with-slots (start end) x
    (>= start end)))
(defclass forward-vector-iterator (vector-iterator)
  ())
(defmethod iterator-next ((x forward-vector-iterator))
  (with-slots (vector start end) x
    (when (< start end)
      (prog1 (aref vector start)
        (incf start)))))
(defclass backward-vector-iterator (vector-iterator)
  ())
(defmethod iterator-next ((x backward-vector-iterator))
  (with-slots (vector start end) x
    (when (< start end)
      (decf end)
      (aref vector end))))
