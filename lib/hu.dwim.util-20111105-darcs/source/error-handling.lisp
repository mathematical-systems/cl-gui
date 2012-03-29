;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

;; TODO rename? to what? with-invoke-debugger-hook?
(def with-macro with-debugger-hook-for-break (hook)
  "CL:BREAK is specified to ignore CL:*DEBUGGER-HOOK*, so we need a platform dependent way to hook the debugger to be able to catch it."
  #*((:sbcl (bind ((sb-ext:*invoke-debugger-hook* hook))
              (-body-)))
     (t #.(warn "~S is not implemented for your platform. This may interfere with the behavior of CL:BREAK while the debugger is disabled..." 'with-debugger-hook-for-break)
        (-body-))))

(def (with-macro* e) with-layered-error-handlers
    (level-1-error-handler abort-unit-of-work-callback
                           &rest args &key
                           (log-to-error-output #t)
                           (ignore-condition-predicate (constantly #f))
                           (level-2-error-handler (named-lambda with-layered-error-handlers/default-level-2-handler
                                                      (error &key message &allow-other-keys)
                                                    (declare (optimize (debug 3)))
                                                    (when log-to-error-output
                                                      (format *error-output* "~A~%" (build-error-log-message :error-condition error :message message)))
                                                    (maybe-invoke-debugger error)))
                           (giving-up-callback (named-lambda with-layered-error-handlers/default-giving-up-callback
                                                   (&key reason &allow-other-keys)
                                                 (declare (optimize (debug 3)))
                                                 (when log-to-error-output
                                                   (format *error-output* "WITH-LAYERED-ERROR-HANDLERS is giving up due to: ~A~%" reason))
                                                 nil))
                           (out-of-storage-callback (named-lambda with-layered-error-handlers/default-out-of-storage-callback
                                                        (error &key &allow-other-keys)
                                                      (declare (optimize (debug 3)))
                                                      ;; TODO if/when sbcl becomes more failure tolerant with stack overflows, we could try to log this using the logger infrastructure. until then, print something to *error-output* and bail out...
                                                      (when log-to-error-output
                                                        (format *error-output* "WITH-LAYERED-ERROR-HANDLERS is bailing out due to a STORAGE-CONDITION of type ~S~%" (type-of error)))
                                                      nil))
                           &allow-other-keys)
  "Below you can find the lambda lists of the input functions. &REST means the extra arguments of WITH-LAYERED-ERROR-HANDLERS that it didn't understand. It's advised to add &key &allow-other-keys for future compatibility wherever applicable.
 - LEVEL-1-ERROR-HANDLER: (original-condition &rest)
 - LEVEL-2-ERROR-HANDLER: (nested-condition &key message &rest)
 - ABORT-UNIT-OF-WORK-CALLBACK: (&key reason &rest)
 - GIVING-UP-CALLBACK: (&key reason &rest)
 - IGNORE-CONDITION-PREDICATE: (condition &rest)
 - OUT-OF-STORAGE-CALLBACK: (oos-condition &rest)"
  (declare (optimize (debug 2)))
  (remove-from-plistf args :log-to-error-output :ignore-condition-predicate :level-2-error-handler :giving-up-callback :out-of-storage-callback)
  (bind ((level-1-error nil)
         (abort-unit-of-work/invoked? #f))
    (labels ((ignore-error? (error)
               (apply ignore-condition-predicate error args))
             (abort-unit-of-work (reason)
               (bind ((recursive? abort-unit-of-work/invoked?))
                 (setf abort-unit-of-work/invoked? #t)
                 (when recursive?
                   (warn "~&~S: ABORT-UNIT-OF-WORK-CALLBACK got called recursively, aborting the entire ~S block~%"
                         'with-layered-error-handlers 'with-layered-error-handlers)
                   ;; quietly returning from the WITH-LAYERED-ERROR-HANDLERS block is not necessarily the best solution,
                   ;; but killing the entire VM or thread is neither...
                   (return-from with-layered-error-handlers (values))))
               (apply abort-unit-of-work-callback :reason reason args))
             (handle-level-1-error (error)
               ;; first level of error handling, call around participants, give them a chance to render an error page, etc
               (setf level-1-error error)
               (handler-bind ((serious-condition #'handle-level-2-error))
                 (with-thread-activity-description ("HANDLE-LEVEL-1-ERROR")
                   (bind ((reason nil))
                     (cond
                       ((typep error 'storage-condition)
                        ;; on SBCL it includes control stack exhaustion, too
                        (setf reason "Error is a STORAGE-CONDITION")
                        (apply out-of-storage-callback error args))
                       ((ignore-error? error)
                        (setf reason (format nil "Error (of type ~S) is to be ignored according to the provided IGNORE-CONDITION-PREDICATE ~A" (type-of error) ignore-condition-predicate))
                        nil)
                       (t
                        (prog1
                            (apply level-1-error-handler error args)
                          (setf reason "Level 1 error handler finished normally"))))
                     (abort-unit-of-work reason))
                   (error "This code path must not be reached in the level 1 error handler of WITH-LAYERED-ERROR-HANDLERS"))))
             (handle-level-2-error (error)
               ;; second level of error handling quarding against errors while handling the original error
               (handler-bind ((serious-condition #'handle-level-3-error))
                 (with-thread-activity-description ("HANDLE-LEVEL-2-ERROR")
                   (unless (ignore-error? error)
                     ;; reason: when e.g. an error html page is being sent the client socket may get reset
                     (apply level-2-error-handler error
                            :message (list "Nested error while handling original error: ~A; the nested error is: ~A" level-1-error error)
                            args))
                   (abort-unit-of-work error)
                   (error "This code path must not be reached in the level 2 error handler of WITH-LAYERED-ERROR-HANDLERS"))))
             (handle-level-3-error (error)
               ;; if we get here then do as little as feasible wrapped in ignore-errors to bail out and abort processing
               ;; the request as soon as we can.
               (with-thread-activity-description ("HANDLE-LEVEL-3-ERROR")
                 (bind ((error-message (or (ignore-errors
                                             (format nil "Nested error while handling original error: ~A; the second, nested error is: ~A"
                                                     level-1-error error))
                                           (ignore-errors
                                             (format nil "Failed to log nested error message (nested print errors?). Condition type of the third nested error is ~S."
                                                     (type-of error)))
                                           "Completely failed to log error, giving up. It's probably due to some nested printer errors or the the whole VM is dying...")))
                   (ignore-errors
                     (apply giving-up-callback :reason error-message args))
                   (abort-unit-of-work error)
                   (error "This code path must not be reached in the level 3 error handler of WITH-LAYERED-ERROR-HANDLERS")))))
      (handler-bind
          ((serious-condition #'handle-level-1-error))
        (flet ((with-layered-error-handlers/debugger-hook (condition hook)
                 ;; this is only here because (break) ignores the *debugger-hook* variables, so it needs platform dependent care...
                 (declare (ignore hook))
                 (when log-to-error-output
                   (format *error-output* "~&WITH-LAYERED-ERROR-HANDLERS/DEBUGGER-HOOK is invoked, most probably because of CL:BREAK (if not, then that's a big WTF?!)~%"))
                 (maybe-invoke-debugger condition)))
          (with-debugger-hook-for-break #'with-layered-error-handlers/debugger-hook
            (-with-macro/body-)))))))

(def (function d) disabled-debugger-hook (condition &optional logger)
  (bind ((message (or (ignore-errors
                        (build-error-log-message :error-condition condition
                                                 :message "Unhandled error while debugger is disabled, quitting..."))
                      "Err, complete meltdown in DISABLED-DEBUGGER-HOOK. Sorry, no more clues...")))
    (when message
      (ignore-errors
        (write-string message *error-output*)
        (terpri *error-output*))
      (when logger
        (hu.dwim.logger:handle-log-message logger hu.dwim.logger:+fatal+ message nil))))
  (quit 42))

(def (function e) disable-debugger (&optional logger)
  (declare (ignorable logger))
  #*((:sbcl (flet ((call-disabled-debugger-hook (condition hook)
                     (declare (ignore hook))
                     (disabled-debugger-hook condition logger)))
              (sb-ext:disable-debugger) ; so that we unconditionally disable LDB
              (sb-alien:with-alien ((lose? sb-alien:boolean :extern "lose_on_corruption_p"))
                (setf lose? t))
              (setf sb-ext:*invoke-debugger-hook* #'call-disabled-debugger-hook)
              (setf *debugger-hook* #'call-disabled-debugger-hook)))
     (t #.(warn "~S is not fully implemented for your implementation which may lead to undesired consequences" 'disable-debugger)))
  (format *trace-output* "Disabled debugger~%"))


;;;;;;
;;; Backtrace extraction

(def class* stack-frame ()
  ((description)
   (local-variables)
   (source-location)))

(def function make-stack-frame (description &optional local-variables source-location)
  (make-instance 'stack-frame
                 :description description
                 :local-variables local-variables
                 :source-location source-location))

(def (with-macro e) with-backtrace-printer-bindings ()
  (bind ((bindings `((*print-pretty*           . #t)
                     (*print-level*            . 3)
                     (*print-length*           . 100)
                     (*print-circle*           . #t)
                     (*print-readably*         . #f)
                     (*print-pprint-dispatch*  . ,(copy-pprint-dispatch nil))
                     (*print-gensym*           . #t)
                     (*print-base*             . 10)
                     (*print-radix*            . #f)
                     (*print-array*            . #t)
                     (*print-lines*            . nil)
                     (*print-escape*           . #t)
                     (*print-right-margin*     . ,most-positive-fixnum)))
         (variables (mapcar #'car bindings))
         (values (mapcar #'cdr bindings)))
    (progv variables values
      (-body-))))

(def function current-thread-name-if-available ()
  ;; TODO err... it's kinda...
  (awhen (find-package "BORDEAUX-THREADS")
    (funcall (find-symbol "THREAD-NAME" it) (funcall (find-symbol "CURRENT-THREAD" it)))))

(def (function ed) build-error-log-message (&key error-condition message (timestamp (get-universal-time)) (include-backtrace #t))
  "Message may also be a list, in which case FORMAT is applied on it."
  (with-backtrace-printer-bindings
    (block building
      (with-layered-error-handlers ((lambda (nested-error)
                                      (return-from building (format nil "Failed to build backtrace due to: ~A. The orignal error was: ~A" nested-error error-condition)))
                                    (lambda (&key &allow-other-keys)
                                      (error "This should be impossible to reach in ~S" 'build-error-log-message))
                                    :level-2-error-handler (lambda (nested-error2 &key &allow-other-keys)
                                                             (declare (ignore nested-error2))
                                                             (return-from building "Failed to build backtrace due to multiple nested errors. Giving up...")))
        (with-output-to-string (*standard-output*)
          (when timestamp
            (format t "~%*** At: ~A" timestamp))
          (when message
            (format t "~&*** Message:~%")
            (apply #'format t (ensure-list message)))
          (awhen (current-thread-name-if-available)
            (format t "~&*** In thread: ~A" it))
          (when error-condition
            (format t "~&*** Error of type ~S:~%~A" (type-of error-condition) error-condition))
          (when include-backtrace
            (format t "~&*** Backtrace:~%")
            (bind ((backtrace (collect-backtrace))
                   (*print-pretty* #f))
              (iter (for stack-frame :in backtrace)
                    (for index :upfrom 0)
                    (write-string stack-frame)
                    (terpri))))
          (when *error-log-decorators*
            (format t "~&*** Backtrace decorators:")
            (with-standard-io-syntax
              (bind ((*print-readably* #f))
                (dolist (decorator *error-log-decorators*)
                  (when (symbolp decorator)
                    (bind ((*package* (find-package :keyword)))
                      (format t "~&~S:" decorator)))
                  (block invoking-decorator
                    (with-layered-error-handlers ((lambda (nested-error)
                                                    (format t "~&Error log decorator ~A signalled error: ~A." decorator nested-error))
                                                  (lambda (&key &allow-other-keys)
                                                    (return-from invoking-decorator))
                                                  :level-2-error-handler (lambda (nested-error2 &key &allow-other-keys)
                                                                           (declare (ignore nested-error2))
                                                                           (format t "Nested errors while calling error log decorator, skipping it...")))
                      (funcall decorator)))))))
          (format t "~&*** End of error details"))))))
