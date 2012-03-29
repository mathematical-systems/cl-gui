;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

(def (special-variable e) *debug-on-error* #f
  "The default, system wide, value for debug-on-error.")

(def (generic e) debug-on-error? (context error)
  (:method (context error)
    *debug-on-error*))

(def (function ed) print-error-safely (&optional message &rest args)
  (when message
    (bind ((formatted (if args
                          (or (ignore-errors
                                (apply #'format nil message args))
                              (ignore-errors
                                (format nil "Error while formatting error message.~%  Format control: ~A~%  Argument types: ~A" message (mapcar #'type-of args)))
                              "Err, complete meltdown in BEST-EFFORT-LOG-ERROR. Sorry, no more clue is available...")
                          message)))
      (ignore-errors
        (write-string formatted *error-output*)
        (terpri *error-output*)
        (finish-output *error-output*)))))

(def (function ed) maybe-invoke-debugger (condition &key context)
  (when (debug-on-error? context condition)
    (when (fboundp 'invoke-slime-debugger)
      (restart-case
          (funcall 'invoke-slime-debugger condition)
        (continue-error-handling ()
          :report "Continue processing the error as if the debugger was not available"))))
  (values))

(def (function e) collect-backtrace (&key (start 0) count)
  (if (fboundp 'collect-backtrace/impl)
      (funcall 'collect-backtrace/impl :start start :count count)
      (format t "[Backtrace is not available, see ~S for details.]" 'collect-backtrace)))

(def (function e) collect-call-path (&key (start 0) count)
  (if (fboundp 'collect-call-path/impl)
      (funcall 'collect-call-path/impl :start start :count count)
      (format t "[Call path is not available, see ~S for details.]" 'collect-call-path/impl)))

;;;;;;
;;; Error log decorators

(def (special-variable :documentation "List of decorators that will be invoked when an error backtrace is logged. A decorator is a closure that will be FUNCALL'd and can print to *STANDARD-OUTPUT* (caret will not be on a new line when called).")
    *error-log-decorators* ())

(def (definer e :available-flags "eiod") error-log-decorator (name &body body)
  `(def function ,name ()
     ,@body))

(def (macro e) make-error-log-decorator (&body body)
  `(named-lambda make-error-log-decorator/body ()
     ,@body))

(def (macro e) make-special-variable-printing-error-log-decorator (&rest variables)
  `(named-lambda make-special-variable-printing-error-log-decorator/body ()
     (declare (optimize (debug 2)))
     (bind ((*package* (find-package "COMMON-LISP"))
            (*print-right-margin* 150))
       ,@(iter (for variable :in variables)
               (collect `(format t ,(bind ((*package* (find-package "COMMON-LISP")))
                                      (format nil "~~%~S ~~A" variable))
                                 (if (boundp ',variable)
                                     ,variable
                                     'unbound)))))))

(def (macro e) with-error-log-decorator (decorator &body body)
  `(bind ((*error-log-decorators* (cons ,decorator *error-log-decorators*)))
     ,@body))

(def (macro/multiple-arguments-variant e) with-error-log-decorator)
