;;; -*- mode: Lisp; Syntax: Common-Lisp; coding: utf-8 -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.syntax-sugar)

;; the defaults, #xFF62 and #xFF63 are ｢ and ｣
(define-syntax string-quote (&key (start-character #.(code-char #xFF62)) (end-character #.(code-char #xFF63)) (transformer #'identity))
  "A simple string quote that unconditionally reads all characters until END-CHARACTER into a string."
  (bind ((transformer (or transformer #'identity)))
    (flet ((string-quote-reader (stream &optional char)
             (declare (ignore char))
             (%read-quoted-string stream end-character transformer)))
      (set-macro-character start-character #'string-quote-reader t *readtable*))))

(defun %read-quoted-string (stream end-character transformer)
  (check-type transformer (or function symbol))
  (bind ((*toplevel-readtable* (or *toplevel-readtable* *readtable*)))
    (loop
      :with result = (make-array 8 :element-type 'character :adjustable t :fill-pointer 0)
      :for char = (read-char stream t nil t)
      :until (char= char end-character)
      :do (vector-push-extend char result)
      :finally (return (funcall transformer result)))))
