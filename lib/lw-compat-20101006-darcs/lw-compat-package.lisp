(in-package :cl-user)

#-lispworks
(defpackage #:lispworks
  (:use #:common-lisp)
  (:export #:appendf #:nconcf #:rebinding #:removef
	   #:when-let #:when-let* #:with-unique-names))
