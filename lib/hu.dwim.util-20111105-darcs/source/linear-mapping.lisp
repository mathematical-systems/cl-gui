;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

;;;;;;
;;; Linear mapping

(def class linear-mapper ()
  ((predicate
    :type function
    :initarg :predicate
    :accessor predicate-of)
   (key
    :type t
    :initarg :key
    :accessor key-of)
   (value
    :type t
    :initarg :value
    :accessor value-of)))

(def class linear-mapping ()
  ((mappers
    :initform nil
    :type list
    :initarg :mappers
    :accessor mappers-of)
   (comparator
    :type function
    :initarg :comparator
    :accessor comparator-of)
   (cache
    :type hash-table
    :initform (make-hash-table :test #'equal)
    :initarg :cache
    :accessor cache-of)))

(def print-object linear-mapper
  (format t "~A : ~A" (key-of -self-) (value-of -self-)))

(def (function e) linear-mapping-value (mapping key)
  (bind (((:read-only-slots mappers cache) mapping))
    (or (gethash key cache)
        (setf (gethash key cache)
              (value-of (first (sort (iter (for mapper :in mappers)
                                           (when (funcall (predicate-of mapper) key)
                                             (collect mapper :into result))
                                           (finally
                                            (return (or result
                                                        (error "No mapper found for key ~A in ~A" key mapping)))))
                                     (lambda (mapper-1 mapper-2)
                                       (< (funcall (comparator-of mapping) mapper-1 mapper-2) 0)))))))))

(def function insert-mapper (mapping mapper)
  (bind (((:slots mappers cache) mapping)
         (old-mapper (find mapper mappers :test (lambda (mapper-1 mapper-2)
                                                  (zerop (funcall (comparator-of mapping) mapper-1 mapper-2))))))
    (when old-mapper
      (setf mappers (remove old-mapper mappers)))
    (pushnew mapper mappers)
    (clrhash cache)))

;;;;;;
;;; Linear type mapping

(def class finite-type-mapper (linear-mapper)
  ((key
    :initform nil
    :allocation :class)
   (count-limit
    :type integer
    :initarg :count-limit
    :accessor count-limit-of)))

(def function insert-subtype-mapper (mapping type value)
  (insert-mapper mapping
                 (make-instance 'linear-mapper
                                :key type
                                :value value
                                :predicate (lambda (key)
                                             (subtypep key type)))))

(def function insert-finite-type-mapper (mapping count-limit value)
  (insert-mapper mapping
                 (make-instance 'finite-type-mapper
                                :value value
                                :count-limit count-limit
                                :predicate (lambda (key)
                                             (awhen (type-instance-count-upper-bound key)
                                               (<= it count-limit))))))

(def generic type-mapper-instance-count-upper-bound (mapper)
  (:method ((mapper linear-mapper))
    (type-instance-count-upper-bound (key-of mapper)))

  (:method ((mapper finite-type-mapper))
    (count-limit-of mapper)))

(def function type-mapper-comparator (mapper-1 mapper-2)
  (bind ((upper-bound-1 (type-mapper-instance-count-upper-bound mapper-1))
         (upper-bound-2 (type-mapper-instance-count-upper-bound mapper-2))
         (key-1 (key-of mapper-1))
         (key-2 (key-of mapper-2))
         (subtypep-1-2? (subtypep key-1 key-2))
         (subtypep-2-1? (subtypep key-2 key-1)))
    (cond ((and (equal upper-bound-1 upper-bound-2)
                (and subtypep-1-2?
                     subtypep-2-1?))
           0)
          ((and upper-bound-1
                upper-bound-2
                (not (zerop (- upper-bound-1 upper-bound-2))))
           (- upper-bound-1 upper-bound-2))
          ((or (and upper-bound-1
                    (not upper-bound-2))
               subtypep-1-2?)
           -1)
          ((or (and upper-bound-2
                    (not upper-bound-1))
               subtypep-2-1?)
           1)
          (t
           ;; NOTE: unrelated types are not equal and are ordered randomly
           (- (sxhash key-1) (sxhash key-2))))))

(def (function e) make-linear-type-mapping ()
  (make-instance 'linear-mapping :comparator 'type-mapper-comparator))

(def (definer e) subtype-mapper (mapping type value)
  `(insert-subtype-mapper ,mapping ',type ',value))

(def (definer e) finite-type-mapper (mapping count value)
  `(insert-finite-type-mapper ,mapping ,count ',value))
