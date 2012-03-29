;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; FIFO queues

#+xcvb (module (:depends-on ("package" "base/utils" "stateful/package")))

(in-package :stateful)

(exporting-definitions

(defstruct fifo
  (head nil)
  (tail nil))

(export-symbols make-fifo fifo-head fifo-tail)

(defun fifo-empty-p (fifo)
  (null (fifo-head fifo)))

(defun fifo-enqueue (obj fifo)
  "Enqueue an object in a fifo. Return the fifo."
  (let ((last (cons obj nil)))
    (if (null (fifo-head fifo))
	(setf (fifo-head fifo) last)
      (setf (cdr (fifo-tail fifo)) last))
    (setf (fifo-tail fifo) last))
  fifo)

(defun fifo-dequeue (fifo)
  "Dequeue an object. Return the object dequeued."
  (when (fifo-head fifo)
    (prog1
	(pop (fifo-head fifo))
      (when (null (fifo-head fifo))
	(setf (fifo-tail fifo) nil)))))

(defun fifo-nconc2 (fifo1 fifo2)
  "move objects from fifo1 to head of fifo2"
  (psetf (fifo-head fifo1) nil
	 (fifo-tail fifo1) nil
	 (fifo-head fifo2) (nconc (fifo-head fifo1) (fifo-head fifo2))
	 (fifo-tail fifo2) (or (fifo-tail fifo2) (fifo-tail fifo1)))
  fifo2)

(defun fifo-dequeue-object (obj fifo)
  (loop :with buffer = (make-fifo)
    :with top = nil
    :until (fifo-empty-p fifo)
    :do (setf top (fifo-dequeue fifo))
    :until (eql obj top)
    :do (fifo-enqueue top buffer)
    :finally (return (fifo-nconc2 buffer fifo))))

(defun fifo-empty! (fifo)
  (setf (fifo-head fifo) nil
        (fifo-tail fifo) nil)
  fifo)

)

(defmethod print-object ((x fifo) stream)
  (print-unreadable-object (x stream :type t :identity t)
    (write (fifo-head x) :stream stream)))
