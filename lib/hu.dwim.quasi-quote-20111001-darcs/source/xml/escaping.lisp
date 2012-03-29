;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.quasi-quote.xml)

(def (constant :test #'equalp) +character-code->xml-escaped-entity+
  (bind ((result (make-array 256 :initial-element nil)))
    (iter (for (character entity) :in '((#\< "&lt;")
                                        (#\> "&gt;")
                                        (#\& "&amp;")
                                        (#\" "&quot;")))
          (setf (aref result (char-code character)) (coerce entity 'simple-base-string)))
    result))

(def (function eo) xml-escaped-entity-for-character (character)
  (declare (type character character))
  (bind ((character-code (char-code character)))
    (when (< character-code #.(length +character-code->xml-escaped-entity+))
      (aref #.+character-code->xml-escaped-entity+ character-code))))

(def (function eo) escape-as-xml (string &optional destination)
  (declare (type string string)
           (type (or null (array character (*))) destination))
  (bind ((result destination))
    (iter (for index :from 0 :below (length string))
          (for character = (aref string index))
          (for entity = (xml-escaped-entity-for-character character))
          (declare (type fixnum index))
          (if entity
              (progn
                (unless result
                  (setf result (make-array (floor (* 1.1 (length string)))
                                           :element-type 'character
                                           :adjustable #t
                                           :fill-pointer index))
                  (replace result string :end2 index))
                (vector-extend entity result))
              (when result
                (vector-push-extend character result))))
    (or result (shrink-vector string (length string)))))

(def function escape-quasi-quoted-string-as-xml (thing)
  (etypecase thing
    (string (escape-as-xml thing))
    (cons (map-tree thing #'escape-quasi-quoted-string-as-xml))
    (null nil)
    (character (or (xml-escaped-entity-for-character thing)
                   thing))
    (delayed-emitting thing)))
