;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

(def (function e) start-swank-server (port)
  (format *debug-io* "Starting Swank server on port ~A...~%" port)
  (bind (((:values started? error) (ignore-errors
                                     (with-simple-restart (continue "Ok, go on without a Swank server")
                                       (let ((swank::*loopback-interface* "127.0.0.1"))
                                         (swank:create-server :port port
                                                              :style :spawn
                                                              :dont-close #t
                                                              :coding-system "utf-8-unix")))
                                     #t)))
    (if started?
        (format *debug-io* "Swank server has been started~%")
        (warn "Swank server failed to start due to: ~A" error))))
