;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.syntax-sugar)

(define-syntax feature-cond (&key (start-character #\*)
                                  (dispatch-character #\#)
                                  end-character
                                  readtable-case)
  (when (and dispatch-character end-character)
    (error "You can not install on both a dispatch character and an end character"))
  (when (member end-character '(#\Return #\Newline #\Tab #\Space) :test 'equal)
    (error "~S is an illegal END-CHARACTER for the feature-cond syntax." end-character))
  (bind ((reader (named-lambda feature-cond-reader (input-stream char &optional dispatched-char)
                   (declare (ignore char dispatched-char))
                   (%read-feture-cond input-stream end-character readtable-case))))
    (cond
      ((and dispatch-character
            start-character)
       (unless (get-macro-character dispatch-character)
         (make-dispatch-macro-character dispatch-character))
       (set-dispatch-macro-character dispatch-character start-character reader *readtable*))
      ((not (null start-character))
       (set-macro-character start-character reader t *readtable*))
      (t (error "Don't know how to install feature-cond syntax with the given parameters.")))))

(defun %read-feture-cond (*standard-input* end-character readtable-case)
  (flet ((debug (format &rest args)
           (declare (ignorable format args))
           #+nil (apply #'format *debug-io* format args)))
    ;; we either operate in [(cond1 body1) ((and cond2 cond3) body2)]
    ;; or in #*((cond1 body1) ((and cond2 cond3) body2)) in which case the end-character is effectively #\)
    (bind ((*toplevel-readtable* (or *toplevel-readtable* *readtable*)))
      (with-local-readtable
        (when readtable-case
          (setf (readtable-case *readtable*) readtable-case))
        (labels ((skip-until-next-line ()
                   (loop
                     :with eof = '#:eof
                     :for char = (read-char *standard-input* nil eof t)
                     :do (debug "Skipping until next line, at char ~S~%" char)
                     :until (or (eq char eof)
                                (char= char #\Newline))))
                 (skip-until-open-paren (error-message on-unexpected &key (error-on-eof t))
                   (loop
                     :with eof = '#:eof
                     :for char = (read-char *standard-input* nil eof t)
                     :do (debug "Skipping at char ~S~%" char)
                     :when (eq char eof)
                     :do (progn
                           (setf char nil)
                           (when error-on-eof
                             (error "Unexpected end of file on ~A while ~A" *standard-input* error-message)))
                     :if (char= char #\;)
                     :do (skip-until-next-line)
                     :else
                     :do (progn
                           (when (char= char #\( )
                             (return))
                           (unless (member char '(#\Return #\Newline #\Tab #\Space))
                             (funcall on-unexpected char)))))
                 (process-entry ()
                   (skip-until-open-paren "trying to read the condition of a feature-cond entry"
                                          (lambda (char)
                                            (debug "Entry, unexpected char ~S~%" char)
                                            (when (eq char (or end-character #\) ))
                                              (emit-result nil))))
                   (bind ((raw-condition (aprog1
                                             (read *standard-input* t nil t)
                                           (debug "Raw condition is ~S~%" it))))
                     (if *read-suppress*
                         (read-delimited-list #\) *standard-input* t)
                         (bind ((condition (aprog1
                                               (process-feature-cond-condition raw-condition)
                                             (debug "Processed condition is ~S~%" it)))
                                (evaluated-condition (bind (#+sbcl (sb-ext:*evaluator-mode* :interpret))
                                                       (eval condition))))
                           (if evaluated-condition
                               (bind ((result (read-delimited-list #\) *standard-input* t))
                                      (*read-suppress* t))
                                 (debug "Got a match, result is ~S~%" result)
                                 (loop :named skipping :do
                                   (skip-until-open-paren "reading entries with *READ-SUPPRESS* bound to T after a match"
                                                          (lambda (char)
                                                            (debug "Purging, unexpected char ~S~%" char)
                                                            (when (eq char #\) )
                                                              (return-from skipping)))
                                                          :error-on-eof t)
                                   (unread-char #\( )
                                   (debug "Skipping dead entry~%")
                                   (read))
                                 (emit-result result))
                               (bind ((*read-suppress* t))
                                 ;; read the body suppressed (i.e. ignore symbols in unknown packages)
                                 (read-delimited-list #\) *standard-input* t)))))))
                 (emit-result (body)
                   (debug "Emitting ~S~%" body)
                   (removef body nil)
                   (return-from %read-feture-cond
                     (cond
                       ((null body)
                        nil)
                       ((length= 1 body)
                        (first body))
                       (t
                        `(locally ,@body))))))
          (unless end-character
            ;; we are in dispatch mode, e.g. installed of #*, so look for an open paren for the toplevel expression
            (skip-until-open-paren "looking for the beginning of a feature-cond expression"
                                   (lambda (char)
                                     (debug "Beginning, unexpected char ~S~%" char)
                                     (error "Expecting an open paren while processing a feature-cond expression in ~A but got ~S."
                                            *standard-input* char))))
          (loop (process-entry)))))))

(defun process-feature-cond-condition (input-form)
  (labels ((recurse (form)
             (cond
               ((consp form)
                (if (member (first form) '(and or not))
                    `(,(first form) ,@(mapcar #'recurse (rest form)))
                    form))
               ((keywordp form)
                `(find ,form *features*))
               ((member form '(t otherwise))
                t)
               ((and (not (null form))
                     (symbolp form))
                (error "To be less confusing feature-cond does not read symbols automatically into the KEYWORD package, please prefix ~S" form)
                #+nil ; this would interpret "foo" as ":foo" but let's just be less confusing than the default CL readers...
                `(find ,(intern (symbol-name form) :keyword) *features*))
               (t
                (error "Don't know how to process feature-cond form ~S" input-form)))))
    (recurse input-form)))
