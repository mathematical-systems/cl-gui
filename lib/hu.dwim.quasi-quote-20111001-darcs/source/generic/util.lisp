;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.quasi-quote)

(def (function o) vector-extend (extension vector &key (start 0) (end (length extension)))
  (declare (type array-index start end))
  (bind ((original-length (length vector))
         (extension-length (- end start))
         (new-length (+ original-length extension-length))
         (original-dimension (array-dimension vector 0)))
    (when (< original-dimension new-length)
      (setf vector (adjust-array vector (max (* 2 original-dimension) new-length))))
    (setf (fill-pointer vector) new-length)
    (replace vector extension :start1 original-length :start2 start :end2 end)
    vector))

(def function reduce-subsequences (sequence reducible?-fn reducer)
  (iter (with completely-reduced? = #t)
        (with length = (length sequence))
        (for index :from 0 :below length)
        (for reducibles = (iter (while (< index length))
                                (for element = (elt sequence index))
                                (while (funcall reducible?-fn element))
                                (collect element)
                                (incf index)))
        (collect (if (zerop (length reducibles))
                     (progn
                       (setf completely-reduced? #f)
                       (elt sequence index))
                     (progn
                       (decf index)
                       (funcall reducer reducibles)))
          :into result)
        (finally (return (values result completely-reduced?)))))

(def function list-without-nils (&rest args)
  (remove nil args))

(def function map-tree (form map-function &optional (process-cons #f))
  (labels ((process (form)
             (cond ((null form)
                    nil)
                   ((consp form)
                    (bind ((result
                            (cons (process (car form))
                                  (process (cdr form)))))
                      (if process-cons
                          (funcall map-function result)
                          result)))
                   (t (funcall map-function form)))))
    (process form)))

(def function map-filtered-tree (form type map-function)
  (map-tree form
            (lambda (form)
              (if (typep form type)
                  (funcall map-function form)
                  form))))

(def (function io) princ-to-string-unless-nil (thing)
  (when thing
    (princ-to-string thing)))

(def function self-evaluating? (thing)
  (and (atom thing)
       (not (symbolp thing))))

(def function coerce-to-transformation-pipeline (thing)
  (etypecase thing
    (cons thing)
    (function (funcall thing))
    (null nil)))
