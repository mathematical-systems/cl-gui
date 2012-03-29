;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

;;;;;;
;;; standard-process

(def hu.dwim.logger::logger process ())

(def (special-variable e) *process*)

(def (class* e) standard-process ()
  ((process-state
    (find-state 'process-state-machine 'initializing)
    :type state
    :documentation "The current state according to the PROCESS-STATE-MACHINE.")
   (form
    :type t
    :documentation "The original form.")
   (continuation
    nil
    :type hu.dwim.delico::continuation
    :documentation "The current continuation or NIL if the process is in a START-STATE or a STOP-STATE.")
   (result
    :type t
    :documentation "The result value that was returned by the process or unbound if the process has not yet been terminated."))
  (:documentation "A STANDARD-PROCESS is a stateful activity expressed as a lisp form. When the process execution is suspended its continuation is saved. Later when the process is resumed its continuation is reused for execution."))

(def (macro e) standard-process (&body forms)
  `(make-instance 'standard-process :form '(progn ,@forms)))

(def (finite-state-machine e) process-state-machine
  (:states
    (initializing :start-state #t :documentation "The process will be in this state while setting its parameters during initalization.")
    (in-progress :documentation "The process is not yet terminated and is waiting to be continued.")
    (running :documentation "The process is currently running and using up system resources.")
    (paused :documentation "The process has been intentionally paused by the user to an indefinite extent.")
    (finished :stop-state #t :documentation "The process terminated execution normally, the result value is stored for later use.")
    (failed :stop-state #t :documentation "The process terminated execution abnormally on its own decision.")
    (cancelled :stop-state #t :documentation "The process has been intentionally cancelled by the user.")
    (broken :stop-state #t :documentation "The process ended too many times with raising an error and it has been aborted to save system resources."))
  (:transitions
    (start initializing running)
    (wait initializing in-progress)
    (wait running in-progress)
    (continue in-progress running)
    (pause in-progress paused)
    (pause paused in-progress)
    (cancel in-progress cancelled)
    (cancel paused cancelled)
    (finish running finished)
    (fail running failed)
    ;; KLUDGE: this is only needed because the error handler needs to run in a new transaction to avoid deadlocks
    ;; TODO: send die message from the same transaction
    (die in-progress broken)
    (die running broken)))

(def (function e) make-process-closure (form)
  (hu.dwim.delico::make-closure/cc
    (hu.dwim.walker:walk-form
     `(lambda ()
        (finish-process *process* ,form)))))

;;;;;;
;;; Process state

(def function process-state-name (process)
  (name-of (process-state-of process)))

(def (function e) process-initializing? (process)
  (eq (process-state-name process) 'initializing))

(def (function e) process-in-progress? (process)
  (eq (process-state-name process) 'in-progress))

(def (function e) process-running? (process)
  (eq (process-state-name process) 'running))

(def (function e) process-in-running? (process)
  (eq (process-state-name process) 'running))

(def (function e) process-paused? (process)
  (eq (process-state-name process) 'paused))

(def (function e) process-finished? (process)
  (eq (process-state-name process) 'finished))

(def (function e) process-failed? (process)
  (eq (process-state-name process) 'failed))

(def (function e) process-cancelled? (process)
  (eq (process-state-name process) 'cancelled))

(def (function e) process-broken? (process)
  (eq (process-state-name process) 'broken))

(def (function e) process-in-start-state? (process)
  (typep (process-state-of process) 'start-state))

(def (function e) process-in-stop-state? (process)
  (typep (process-state-of process) 'stop-state))

;;;;;;
;;; Process transitions

(def function %finish-process (process &optional (result nil result?))
  (setf (continuation-of process) nil)
  (when result?
    (setf (result-of process) result)))

(def (function e) pause-process (process)
  (process.debug "Pausing process ~A" process)
  (process-event process 'process-state 'pause))

(def (function e) finish-process (process &optional result)
  (process.debug "Finishing process ~A" process)
  (process-event process 'process-state 'finish)
  (%finish-process process result))

(def (function e) fail-process (process &optional result)
  (process.debug "Failing process ~A" process)
  (process-event process 'process-state 'fail)
  (%finish-process process result))

(def (function e) cancel-process (process &optional result)
  (process.debug "Cancelling process ~A" process)
  (process-event process 'process-state 'cancel)
  (%finish-process process result))

(def (function e) die-process (process)
  (process.debug "Dying process ~A" process)
  (process-event process 'process-state 'die)
  (%finish-process process))
