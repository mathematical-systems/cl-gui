;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

(def special-variable *special-variables-to-print-with-backtrace* nil)

(def special-variable *current-backtrace-special-variable-values*)

(def function %print-special-variables-for-frame ()
  (with-output-to-string (stream)
    (bind ((found-one? #f))
      (dolist (var *special-variables-to-print-with-backtrace*)
        (bind (((:values previous-value found?) (gethash var *current-backtrace-special-variable-values*))
               (current-value (if (boundp var)
                                  (symbol-value var)
                                  'unbound)))
          (when (or (not found?)
                    (not (eq previous-value current-value)))
            (setf (gethash var *current-backtrace-special-variable-values*) current-value)
            (unless found-one?
              (setf found-one? #t)
              (format stream "~%---- Special variables follow:"))
            (bind ((printed-value (or (ignore-errors
                                        (princ-to-string current-value))
                                      "<error printing value>")))
              (format stream "~%---- ~S: ~A" var printed-value))))))))

#+#.(hu.dwim.util:if-symbol-exists "SB-DEBUG" "*VERBOSITY*")
(def function collect-backtrace/impl (&key (start 0) count
                                           ((:verbosity sb-debug::*verbosity*) sb-debug::*verbosity*)
                                           (print-frame-source (> sb-debug::*verbosity* 1))
                                           &allow-other-keys)
  (unless count
    (setf count sb-debug::*default-backtrace-size-limit*))
  (setf start (+ start 10))
  (bind ((backtrace ())
         (*current-backtrace-special-variable-values* (make-hash-table :test 'eq)))
    (sb-debug::map-backtrace
     (lambda (frame)
       (bind ((frame-as-string (with-output-to-string (stream)
                                 (handler-case
                                     (progn
                                       (sb-debug::print-frame-call frame stream :number #t
                                                                   :print-frame-source print-frame-source)
                                       #+nil ; TODO eval-in-frame does not eval with the proper dynamic environment, so all this is pretty useless for now
                                       (write-string (funcall (the function
                                                                (sb-di:preprocess-for-eval '(%print-special-variables-for-frame) (sb-di:frame-code-location frame)))
                                                              frame)
                                                     stream))
                                   (serious-condition (error)
                                     ;; NOTE: the usage of ~S is important here to avoid calling
                                     ;; any custom PRINT-OBJECT methods that may error again.
                                     (format nil "<<< Error while printing frame: ~S >>>" error))))))
         (push frame-as-string backtrace)))
     :start start :count count)
    (nreverse backtrace)))

#+#.(hu.dwim.util:if-symbol-exists "SB-DEBUG" "*VERBOSITY*")
(def function collect-call-path/impl (&key (start 0) count
                                           ((:verbosity sb-debug::*verbosity*) sb-debug::*verbosity*)
                                           &allow-other-keys)
  (unless count
    (setf count sb-debug::*default-backtrace-size-limit*))
  (setf start (+ start 4))
  (bind ((call-path ()))
    (sb-debug::map-backtrace
     (lambda (frame)
       (bind (((:values name args kind) (sb-debug::frame-call frame)))
         (declare (ignore args kind))
         (push name call-path)))
     :start start :count count)
    (nreverse call-path)))
