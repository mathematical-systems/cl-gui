;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.quasi-quote.js)

(def (constant :test #'equalp) +character-code->escaped-version+
  (bind ((result (make-array 128 :initial-element t)))
    (iter (for idx :from 32 :to 126)
          (setf (aref result idx) nil))
    (iter (for (character replacement) :in '((#\'              #\')
                                             (#\\              #\\)
                                             (#\Backspace      #\b)
                                             (#.(code-char 12) #\f)
                                             (#.(code-char 0)  #\O)
                                             (#\Newline        #\n)
                                             (#\Return         #\r)
                                             (#\Tab            #\t)
                                             (#\Page           #\p)))
          (setf (aref result (char-code character)) replacement))
    result))

(def (function eo) escape-as-js-string (string &optional destination)
  (declare (type string string)
           (type (or null (array character (*))) destination))
  (bind ((result destination))
    (iter (for index :from 0 :below (length string))
          (for character = (aref string index))
          (for character-code = (char-code character))
          (for escape-character = (if (< character-code 128)
                                      (aref +character-code->escaped-version+ character-code)
                                      ;; don't escape unicode stuff, other tools should work fine
                                      nil))
          (if escape-character
              (if (eq escape-character t)
                  ;; FIXME this is ridiculously inefficient compared to the rest of this function
                  (iter (for char :in-vector (with-output-to-string (buffer)
                                               (format buffer "\\u~4,'0x" character-code)))
                        (vector-push-extend char result))
                  (progn
                    (unless result
                      (setf result (make-array (floor (* 1.1 (length string)))
                                               :element-type 'character
                                               :adjustable #t
                                               :fill-pointer index))
                      (replace result string :end2 index))
                    (vector-push-extend #\\ result)
                    (vector-push-extend escape-character result)))
              (when result
                (vector-push-extend character result))))
    (if result
        (shrink-vector result (length result))
        string)))
