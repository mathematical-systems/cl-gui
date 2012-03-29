;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.syntax-sugar)

(defparameter *quasi-quote-lexical-depth* 0
  "The absolute level of read-time (lexical) nesting of the quasi-quote readers. It does not decrease at unquotes.")

(defparameter *quasi-quote-nesting-level* 0
  "The read-time (lexical) nesting level of the quasi-quote readers, decreases when going through unquotes.")

(define-syntax quasi-quote (quasi-quote-wrapper unquote-wrapper
                                                &key
                                                (nested-quasi-quote-wrapper quasi-quote-wrapper)
                                                start-character
                                                dispatch-character
                                                end-character
                                                (unquote-character #\,)
                                                (splice-character #\@)
                                                (destructive-splice-character #\.)
                                                dispatched-quasi-quote-name
                                                (toplevel-reader-wrapper #'identity)
                                                unquote-readtable-case
                                                ;; body-reader is called with the stream to read the quasi quoted body
                                                ;; when it's actually time to read the body. by default the normal
                                                ;; lisp reader is called (with possible readtable customizations).
                                                body-reader
                                                readtable-case)
  (check-type unquote-readtable-case (member nil :toplevel :parent :upcase :downcase :preserve))
  (when (and dispatch-character end-character)
    (error "You can not install on both a dispatch character and an end character"))
  (when (and dispatched-quasi-quote-name
             (eql start-character #\`))
    (error "You requested the dispatched-quasi-quote reader, which is registered on the #\` character, and also provided #\` to be the start-character. Which one should win?"))
  (bind ((reader (make-quasi-quote-reader nil nil
                                          start-character end-character
                                          quasi-quote-wrapper nested-quasi-quote-wrapper
                                          unquote-character unquote-wrapper
                                          splice-character
                                          destructive-splice-character
                                          toplevel-reader-wrapper
                                          body-reader
                                          readtable-case
                                          unquote-readtable-case)))
    (cond
      ((and dispatch-character
            start-character)
       (unless (get-macro-character dispatch-character)
         (make-dispatch-macro-character dispatch-character))
       (set-dispatch-macro-character dispatch-character start-character reader *readtable*))
      ((not (null start-character))
       (set-macro-character start-character reader t *readtable*))))
  (when dispatched-quasi-quote-name
    (bind ((previous-reader-on-backtick (get-macro-character #\`))
           (dispatching-reader (make-quasi-quote-reader dispatched-quasi-quote-name
                                                        previous-reader-on-backtick
                                                        #\` nil
                                                        quasi-quote-wrapper nested-quasi-quote-wrapper
                                                        unquote-character unquote-wrapper
                                                        splice-character
                                                        destructive-splice-character
                                                        toplevel-reader-wrapper
                                                        body-reader
                                                        readtable-case
                                                        unquote-readtable-case)))
      (set-macro-character #\` dispatching-reader t *readtable*))))

(defparameter *dispatched-quasi-quote-name* nil)

(defun make-quasi-quote-reader (dispatched-quasi-quote-name
                                previous-reader-on-backtick
                                start-character end-character
                                quasi-quote-wrapper nested-quasi-quote-wrapper
                                unquote-character unquote-wrapper
                                splice-character
                                destructive-splice-character
                                toplevel-reader-wrapper
                                body-reader
                                readtable-case
                                unquote-readtable-case)
  (when (and dispatched-quasi-quote-name
             end-character)
    (error "What?! dispatched-quasi-quote is always installed on #\` and should not have an end-character..."))
  (macrolet ((debug-log (message &rest args)
               (declare (ignorable message args))
               #+nil
               `(progn
                  (write-string (make-string (* 2 *quasi-quote-lexical-depth*) :initial-element #\Space) *debug-io*)
                  (format *debug-io* ,message ,@args)
                  (format *debug-io* " (dispatched-quasi-quote-name: ~S, start/end character: ~S ~S)" dispatched-quasi-quote-name start-character end-character)
                  (terpri *debug-io*)
                  (values))))
    (bind ((dispatched? (not (null dispatched-quasi-quote-name)))
           (previous-reader-on-comma nil))
      (labels ((toplevel-quasi-quote-reader (stream &optional char1 char2)
                 (declare (ignore char1 char2))
                 (debug-log "TOPLEVEL-QUASI-QUOTE-READER entering")
                 (setf previous-reader-on-comma (get-macro-character #\,))
                 (multiple-value-prog1
                     (values (read-quasi-quote nil stream) t)
                   (debug-log "TOPLEVEL-QUASI-QUOTE-READER leaving")))
               (read-using-previous-reader (stream char)
                 (debug-log "READ-USING-PREVIOUS-READER entering, previous-reader-on-backtick: ~A, previous-reader-on-comma: ~A" previous-reader-on-backtick previous-reader-on-comma)
                 (assert previous-reader-on-comma)
                 (if previous-reader-on-backtick
                     (with-local-readtable
                       (set-macro-character #\, previous-reader-on-comma)
                       (funcall previous-reader-on-backtick stream char))
                     (simple-reader-error stream "No dispatched quasi-quote reader with name ~S" dispatched-quasi-quote-name)))
               (dispatching-toplevel-quasi-quote-reader (stream char)
                 (debug-log "DISPATCHING-TOPLEVEL-QUASI-QUOTE-READER entering, *dispatched-quasi-quote-name* ~S" *dispatched-quasi-quote-name*)
                 (assert dispatched?)
                 (assert (char= start-character char))
                 (setf previous-reader-on-comma (get-macro-character #\,))
                 (if (or *dispatched-quasi-quote-name*
                         (char-equal (elt (string dispatched-quasi-quote-name) 0)
                                     (peek-char nil stream t nil t)))
                     (bind ((name (or *dispatched-quasi-quote-name*
                                      (read stream t nil t))))
                       (if (or (eq name dispatched-quasi-quote-name)
                               (string= (string-downcase name) (string-downcase dispatched-quasi-quote-name)))
                           (bind ((*dispatched-quasi-quote-name* nil))
                             ;; ok, we've got a match, read it in.
                             (values (read-quasi-quote t stream) t))
                           (bind ((*dispatched-quasi-quote-name* name))
                             ;; FIXME this will do something horribly wrong for a `foo()
                             ;; if there's no handler for it and the default reader is called.
                             (values (read-using-previous-reader stream char) nil))))
                     (values (read-using-previous-reader stream char) nil)))
               (read-quasi-quoted-body (stream)
                 (debug-log "READ-QUASI-QUOTED-BODY entering")
                 (if (and body-reader
                          (not dispatched?))
                     (funcall body-reader stream)
                     (if end-character
                         (with-local-readtable
                           ;; we must set the syntax on the end char to be like #\)
                           ;; until we read out our entire body. this is needed to
                           ;; make "[... 5] style inputs work where '5' is not
                           ;; separated from ']'.
                           ;; doin this is not necessary when invoked from the nested reader
                           ;; but doesn't hurt either. wastes some cycles, but...
                           (set-syntax-from-char end-character #\) *readtable*)
                           (read-delimited-list end-character stream t))
                         (read stream t nil t))))
               (read-quasi-quote (dispatched? stream)
                 (debug-log "READ-QUASI-QUOTE entering, unquote-reader on ~S is ~A" unquote-character (get-macro-character* unquote-character *readtable*))
                 (bind ((entering-readtable *readtable*)
                        (entering-quasi-quote-nesting-level *quasi-quote-nesting-level*)
                        (*quasi-quote-lexical-depth* (1+ *quasi-quote-lexical-depth*))
                        (*quasi-quote-nesting-level* (1+ *quasi-quote-nesting-level*))
                        (*toplevel-readtable* (or *toplevel-readtable* *readtable*)))
                   (with-local-readtable
                     (set-macro-character unquote-character (make-unquote-reader entering-quasi-quote-nesting-level entering-readtable))
                     (bind ((original-sharp-dot-reader (get-dispatch-macro-character #\# #\.)))
                       ;; make sure #. reader is called in the restored readtable-case.
                       ;; TODO this should be generalized to wrap all the readers installed in the readtable? probably it's not worth the trouble...
                       (set-dispatch-macro-character #\# #\. (lambda (&rest args)
                                                               (with-local-readtable
                                                                 (setf (readtable-case *readtable*) (readtable-case *toplevel-readtable*))
                                                                 (apply original-sharp-dot-reader args)))))
                     (when end-character
                       ;; there's no point in handling nesting when we are installed on a pair of parens.
                       ;; e.g. in <element <child>> child should not be qq wrapped... so, we enable a
                       ;; special nested reader when we are installed on a pair of parens.
                       (set-macro-character start-character (make-nested-quasi-quote-reader entering-quasi-quote-nesting-level)))
                     (when readtable-case
                       (setf (readtable-case *readtable*) readtable-case))
                     (bind ((body (read-quasi-quoted-body stream)))
                       (if (functionp quasi-quote-wrapper)
                           (funcall quasi-quote-wrapper body dispatched?)
                           (list quasi-quote-wrapper body))))))
               (make-nested-quasi-quote-reader (entering-quasi-quote-nesting-level)
                 (named-lambda nested-quasi-quote-reader (stream char)
                   (declare (ignore char))
                   (debug-log "NESTED-QUASI-QUOTE-READER entering")
                   (assert (>= *quasi-quote-nesting-level* entering-quasi-quote-nesting-level))
                   (bind ((body (read-quasi-quoted-body stream)))
                     (if (functionp nested-quasi-quote-wrapper)
                         (funcall nested-quasi-quote-wrapper body dispatched?)
                         (list nested-quasi-quote-wrapper body)))))
               (make-unquote-reader (entering-quasi-quote-nesting-level entering-readtable)
                 (named-lambda unquote-reader (stream char)
                   (declare (ignore char))
                   (debug-log "UNQUOTE-READER entering, *quasi-quote-nesting-level* ~A, entering-quasi-quote-nesting-level ~A"
                              *quasi-quote-nesting-level* entering-quasi-quote-nesting-level)
                   (with-local-readtable ; this is only needed when actually restoring the original readers, but then it's needed inside the recursive READ call down at the end, so wrap it all up with a copy
                     (bind ((*quasi-quote-nesting-level* (1- *quasi-quote-nesting-level*))
                            (modifier (switch ((peek-char nil stream t nil t) :test 'char=)
                                        (splice-character :splice)
                                        (destructive-splice-character :destructive-splice))))
                       (when modifier
                         (read-char stream t nil t))
                       (assert (>= *quasi-quote-nesting-level* entering-quasi-quote-nesting-level))
                       (when (= *quasi-quote-nesting-level* entering-quasi-quote-nesting-level)
                         ;; restore the original readers when we are leaving our nesting. this way it's possible
                         ;; to use the ` and , in their previous meanings when being outside our own nesting levels.
                         ;; we don't restore the reader on the end character, if there was any, because that
                         ;; would break things like [a ,b] due to a #\] being nonterminating by default.
                         (when unquote-readtable-case
                           (ecase unquote-readtable-case
                             ((:upcase :downcase :preserve)
                              (setf (readtable-case *readtable*) unquote-readtable-case))
                             (:parent
                              (setf (readtable-case *readtable*) (readtable-case entering-readtable)))
                             (:toplevel
                              (setf (readtable-case *readtable*) (readtable-case *toplevel-readtable*)))))
                         (debug-log "UNQUOTE-READER restoring original reader on the unquote char ~S, it is ~A" unquote-character (get-macro-character* unquote-character entering-readtable))
                         (apply 'set-macro-character unquote-character (multiple-value-list (get-macro-character* unquote-character entering-readtable)))
                         (when end-character
                           (set-macro-character start-character (funcall toplevel-reader-wrapper #'toplevel-quasi-quote-reader))))
                       (debug-log "UNQUOTE-READER calling READ")
                       (bind ((body (read stream t nil t)))
                         (if (functionp unquote-wrapper)
                             (funcall unquote-wrapper body modifier)
                             (list unquote-wrapper body modifier))))))))
        (funcall toplevel-reader-wrapper
                 (if dispatched?
                     #'dispatching-toplevel-quasi-quote-reader
                     #'toplevel-quasi-quote-reader))))))
