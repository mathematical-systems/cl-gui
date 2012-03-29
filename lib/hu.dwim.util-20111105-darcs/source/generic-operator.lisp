;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

;;;;;;
;;; Generic operators

(def (generic e) g+ (object-1 object-2)
  (:method ((object-1 number) (object-2 number))
    (+ object-1 object-2)))

(def (generic e) g- (object-1 object-2)
  (:method ((object-1 number) (object-2 number))
    (- object-1 object-2)))

(def (generic e) g* (object-1 object-2)
  (:method ((object-1 number) (object-2 number))
    (* object-1 object-2)))

(def (generic e) g/ (object-1 object-2)
  (:method ((object-1 number) (object-2 number))
    (/ object-1 object-2)))

(def (generic e) g< (object-1 object-2)
  (:method ((object-1 number) (object-2 number))
    (< object-1 object-2))

  (:method ((object-1 string) (object-2 string))
    (string< object-1 object-2)))

(def (generic e) g<= (object-1 object-2)
  (:method ((object-1 number) (object-2 number))
    (<= object-1 object-2))

  (:method ((object-1 string) (object-2 string))
    (string<= object-1 object-2)))

(def (generic e) g> (object-1 object-2)
  (:method ((object-1 number) (object-2 number))
    (> object-1 object-2))

  (:method ((object-1 string) (object-2 string))
    (string> object-1 object-2)))

(def (generic e) g>= (object-1 object-2)
  (:method ((object-1 number) (object-2 number))
    (>= object-1 object-2))

  (:method ((object-1 string) (object-2 string))
    (string>= object-1 object-2)))

(def (generic e) g= (object-1 object-2)
  (:method (object-1 object-2)
    (eq object-1 object-2))

  (:method ((object-1 number) (object-2 number))
    (= object-1 object-2))

  (:method ((object-1 string) (object-2 string))
    (string= object-1 object-2))

  (:method ((object-1 symbol) (object-2 symbol))
    (or (eq object-1 object-2)
        (and (not (symbol-package object-1))
             (not (symbol-package object-2))
             (g= (symbol-name object-1)
                 (symbol-name object-2)))))

  (:method ((object-1 list) (object-2 list))
    (labels ((%= (x y)
               (cond ((eql x y)
                      t)
                     ((and (consp x)
                           (consp y))
                      (and (g= (car x) (car y))
                           (g= (cdr x) (cdr y))))
                     (t (g= x y)))))
      (%= object-1 object-2)))

  (:method ((object-1 array) (object-2 array))
    (and (g= (array-dimensions object-1)
             (array-dimensions object-2))
         (g= (array-element-type object-1)
             (array-element-type object-2))
         (loop for index :from 0 :below (array-total-size object-1)
               always (g= (row-major-aref object-1 index) (row-major-aref object-2 index)))))

  (:method ((object-1 hash-table) (object-2 hash-table))
    (and (= (hash-table-count object-1) (hash-table-count object-2))
         (eq (hash-table-test object-1) (hash-table-test object-2))
         (block nil
           (maphash (lambda (key value)
                      (unless (g= (gethash key object-2) value)
                        (return #f)))
                    object-1)
           #t)))

  (:method ((object-1 structure-object) (object-2 structure-object))
    (let ((class-1 (class-of object-1))
          (class-2 (class-of object-2)))
      (and (eq class-1 class-2)
           (every (lambda (slot)
                    (g= (slot-value-using-class class-1 object-1 slot)
                        (slot-value-using-class class-2 object-2 slot)))
                  (class-slots (ensure-finalized class-1))))))

  (:method ((object-1 standard-object) (object-2 standard-object))
    (let ((class-1 (class-of object-1))
          (class-2 (class-of object-2)))
      (and (eq class-1 class-2)
           (every (lambda (slot)
                    (or (and (not (slot-boundp-using-class class-1 object-1 slot))
                             (not (slot-boundp-using-class class-2 object-2 slot)))
                        (g= (slot-value-using-class class-1 object-1 slot)
                            (slot-value-using-class class-2 object-2 slot))))
                  (class-slots (ensure-finalized class-1)))))))
