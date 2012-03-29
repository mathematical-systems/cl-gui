;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util.test)

;; TODO:
#|
(with-open-file (input "/tmp/x.diff" :direction :input :element-type '(unsigned-byte 8))
  (with-open-file (output "/tmp/x.diff.z" :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
    (hu.dwim.wui.zlib:deflate
        (lambda (buffer start size)
          (read-sequence buffer input :start start :end size))
        (lambda (buffer start size)
          (write-sequence buffer output :start start :end size)))))

(with-open-file (input "/tmp/x.diff.z" :direction :input :element-type '(unsigned-byte 8))
  (with-open-file (output "/tmp/x.diff.extracted" :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
    (hu.dwim.wui.zlib:inflate
     (lambda (buffer start size)
       (read-sequence buffer input :start start :end size))
     (lambda (buffer start size)
       (write-sequence buffer output :start start :end size)))))
|#
