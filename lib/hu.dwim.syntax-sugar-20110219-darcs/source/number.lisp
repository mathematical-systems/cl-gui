;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.syntax-sugar)

(defvar *digits* '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\, #\/))

(defun read-rational (stream)
  "read all decimals as rationals"
  (let ((chars-read 0)
        (this-char nil)
        (result 0)
        (n 0)
        (position-of-dot nil)
        (position-of-slash nil))
    (assert result)
    (iter (while (member (peek-char nil stream) *digits*))
          (setf this-char (read-char stream))
          (incf chars-read)
          (cond ((eql this-char #\,)
                 (setf position-of-dot n))
                ((eql this-char #\/)
                 (setf position-of-slash n))
                (t
                 (setf result (+ (* 10 result) (position this-char *digits*)))))
          (incf n))
    (values
     (cond ((and position-of-dot position-of-slash)
            (error "Cannot read number with both . and / characters in it"))
           (position-of-dot
            (/ result (expt 10 (- n position-of-dot 1))))
           (position-of-slash
            (multiple-value-bind (x y) (truncate result (expt 10 (- n position-of-slash 1)))
              (/ x y)))
           (t
            result))
     chars-read)))

(defun sharp-number-reader (stream c n)
  (declare (ignore c n))
  (iter (with first = t)
        (with value = 0)
        (for char = (peek-char t stream))
        (setf value
              (cond ((eq char #\))
                     (read-char stream)
                     (return value))
                    ((member char '(#\Space #\())
                     (read-char stream)
                     value)
                    ((member char *digits*)
                     (multiple-value-bind (rational read-chars)
                         (read-rational stream)
                       (assert (or first (and (= read-chars 3)
                                              (integerp rational)))
                               nil "Invalid sharp N number ~A" rational)
                       (setf first nil)
                       (+ (* value 1000) rational)))
                    (t
                     (ecase (read stream)
                       (ezer (* value 1000))
                       (millió (* value 1000000))
                       (milliárd (* value 1000000000))))))))

(define-syntax sharp-number ()
  (set-dispatch-macro-character #\# #\N #'sharp-number-reader))
