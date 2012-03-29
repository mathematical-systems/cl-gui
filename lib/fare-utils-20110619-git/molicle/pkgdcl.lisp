(in-package :fare-utils)

(defpackage :molicle
  (:use :cl :fare-utils)
  (:export
   #:evalation-time ;;#:*evaluation-time*
   #:+read-time+ #:+compile-time+ #:+load-time+ #:+run-time+
   #:register-final-form #:register-post-compile-hook
   #:begin #:end].#))

(defpackage :[
  (:use :molicle)
  (:export
   #:begin #:end].#))

(defpackage :molicle-user
  (:use :[))

(pushnew :molicle *features*)
