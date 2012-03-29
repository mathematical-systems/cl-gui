;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

;;;;;;
;;; Worker threads

(def hu.dwim.logger:logger worker-group ())

(def class* worker-group ()
  ((worker-name :type string)
   (workers nil :type list)
   (worker-lock (bordeaux-threads:make-lock "worker-lock"))
   (worker-condition-variable (bordeaux-threads:make-condition-variable))
   (jobs nil :type list)
   (job-lock (bordeaux-threads:make-lock "job-lock"))
   (scheduler-condition-variable (bordeaux-threads:make-condition-variable)))
  (:documentation "Manages a set of worker threads to process a set of jobs simultanously."))

(def class* worker ()
  ((worker-group :type worker-group)
   (keep-on-running #t :type boolean :accessor keep-on-running?)
   (thread :type t)))

(def function worker-loop (worker-group worker)
  (unwind-protect
       (iter (while (keep-on-running? worker))
             (for job = (pop-job worker-group
                                 (lambda ()
                                   (not (keep-on-running? worker)))))
             (when job
               (worker-loop/run-one-job worker-group worker job)))
    (bordeaux-threads:with-lock-held ((worker-lock-of worker-group))
      (deletef (workers-of worker-group) worker))
    (bordeaux-threads:condition-notify (worker-condition-variable-of worker-group))))

(def function worker-loop/run-one-job (worker-group worker job)
  (with-layered-error-handlers ((lambda (error)
                                  (worker-group.error (build-error-log-message :error-condition error :message (format nil "Error reached toplevel in worker ~A while executing job ~A" worker job)))
                                  (maybe-invoke-debugger error :context worker-group))
                                (lambda (&key &allow-other-keys)
                                  (return-from worker-loop/run-one-job)))
    (with-thread-name " / running job"
      (funcall job))))

(def (function e) make-worker-group (name)
  (make-instance 'worker-group :worker-name name))

(def (function e) start-worker (worker-group &optional (worker-environment-function #'funcall))
  (bordeaux-threads:with-lock-held ((worker-lock-of worker-group))
    (prog1-bind worker (make-instance 'worker :worker-group worker-group)
      (worker-group.debug "Staring new worker for ~A" worker-group)
      (setf (thread-of worker)
            (bordeaux-threads:make-thread
             (lambda ()
               (funcall worker-environment-function
                        (lambda ()
                          (worker-loop worker-group worker))))
             :name (worker-name-of worker-group)))
      (push worker (workers-of worker-group)))))

(def (function e) stop-worker (worker)
  (worker-group.debug "Stopping worker ~A" worker)
  (setf (keep-on-running? worker) #f)
  (bordeaux-threads:condition-notify (worker-condition-variable-of (worker-group-of worker))))

(def (function e) stop-all-workers (worker-group)
  (bordeaux-threads:with-lock-held ((worker-lock-of worker-group))
    (prog1-bind workers (workers-of worker-group)
      (dolist (worker workers)
        (setf (keep-on-running? worker) #f))
      (bordeaux-threads:condition-notify (worker-condition-variable-of worker-group)))))

(def (function e) push-job (worker-group job)
  (bordeaux-threads:with-lock-held ((job-lock-of worker-group))
    (worker-group.debug "Pushing new job ~A into ~A" job worker-group)
    (push job (jobs-of worker-group))
    (bordeaux-threads:condition-notify (worker-condition-variable-of worker-group))))

(def (function e) pop-job (worker-group &optional (exit-condition (constantly #f)))
  (bind ((job-lock (job-lock-of worker-group)))
    (bordeaux-threads:with-lock-held (job-lock)
      (iter (until (funcall exit-condition))
            (when-bind job (pop (jobs-of worker-group))
              (worker-group.debug "Popping job ~A from ~A" job worker-group)
              (when (null (jobs-of worker-group))
                (bordeaux-threads:condition-notify (scheduler-condition-variable-of worker-group)))
              (return-from pop-job job))
            (bordeaux-threads:condition-wait (worker-condition-variable-of worker-group) job-lock)))))

(def (function e) delete-all-jobs (worker-group)
  (bordeaux-threads:with-lock-held ((job-lock-of worker-group))
    (setf (jobs-of worker-group) nil)
    (bordeaux-threads:condition-notify (scheduler-condition-variable-of worker-group))))

(def (function e) wait-until-all-jobs-are-finished (worker-group)
  (with-thread-name " / WAIT-UNTIL-ALL-JOBS-ARE-FINISHED"
    (bind ((job-lock (job-lock-of worker-group)))
      (bordeaux-threads:with-lock-held (job-lock)
        (when (jobs-of worker-group)
          (bordeaux-threads:condition-wait (scheduler-condition-variable-of worker-group) job-lock))))))
