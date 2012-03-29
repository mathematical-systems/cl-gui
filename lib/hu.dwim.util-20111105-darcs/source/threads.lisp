;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

;;;;;;
;;; human readable thread id's

(def (namespace :weakness :key) thread-id)
(def global-variable *thread-id-counter* (make-atomic-counter))

(def (function e) human-readable-thread-id (&optional (thread (bordeaux-threads:current-thread)))
  (assert (bordeaux-threads:threadp thread))
  (find-thread-id thread :otherwise (lambda ()
                                      (setf (find-thread-id thread)
                                            (atomic-counter/increment *thread-id-counter*)))))

(def (macro e) with-deadline ((timeout-in-seconds &key (optional #f)) &body body)
  (declare (ignorable optional))
  "As usual, TIMEOUT is in the SI unit of the scale, which is seconds. Will signal a DEADLINE-TIMEOUT when the deadline is passed."
  #*((:sbcl `(sb-sys:with-deadline (:seconds ,timeout-in-seconds)
               ,@body))
     (t (unless optional
          #.(not-yet-implemented/crucial-api 'with-deadline)))))

#*((:sbcl
     (import 'sb-sys:deadline-timeout :hu.dwim.util)
     (export 'sb-sys:deadline-timeout :hu.dwim.util)))
