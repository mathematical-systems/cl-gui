;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

;;;;;;
;;; Help command line option

(def (constant e) +help-command-line-option+
  '(("help" #\h)
    :type boolean
    :optional #t
    :documentation "Print usage help and exit"))

(def (function e) process-help-command-line-argument (options arguments &key help-prefix help-suffix)
  (when (getf arguments :help)
    (when help-prefix
      (format *standard-output* "~A~%~%" help-prefix))
    (command-line-arguments:show-option-help options)
    (when help-suffix
      (format *standard-output* "~%~A~%" help-suffix)
      (force-output))
    (quit 0)))

;;;;;;
;;; Version command line option

(def (constant e) +version-command-line-option+
  '(("version" #\v)
    :type boolean
    :optional #t
    :documentation "Print version information and exit"))

(def (function e) process-version-command-line-argument (arguments version-string)
  (when (getf arguments :version)
    (format *standard-output* "Version: ~A~%" version-string)
    (force-output)
    (quit 0)))

;;;;;;
;;; Quiet command line option

(def (constant e) +quiet-command-line-option+
  '(("quiet" #\q)
    :type boolean
    :optional #t
    :documentation "Do not write to standard output"))

(def (function e) process-quiet-command-line-argument (arguments)
  (when (getf arguments :quiet)
    (setf *standard-output* (make-instance 'broadcast-stream))))

;;;;;;
;;; Standard command line options

(def (constant e) +standard-command-line-options+
  (list +help-command-line-option+
        +version-command-line-option+
        +quiet-command-line-option+))

(def (function e) non-vm-command-line-arguments ()
  #+sbcl
  (bind ((arguments (command-line-arguments:get-command-line-arguments))
         (last-vm-argument (position "--end-toplevel-options" arguments :test 'string=)))
    (if last-vm-argument
        (append (first arguments)
                (subseq arguments (1+ last-vm-argument)))
        arguments))
  #-sbcl
  (command-line-arguments:get-command-line-arguments))

(def (function e) parse-command-line-arguments (options)
  (command-line-arguments:process-command-line-options options (non-vm-command-line-arguments)))

(def (function e) copy-command-line-options (options &rest default-values &key &allow-other-keys)
  (mapcar (lambda (option)
            (apply #'copy-command-line-option option default-values))
          options))

(def (function e) copy-command-line-option (option &rest default-values &key &allow-other-keys)
  (bind ((option (copy-seq option))
         (properties (cdr option))
         (property-name (command-line-arguments::actual-action-from-spec (first (ensure-list (car option)))))
         (default-value (getf default-values property-name)))
    (when (and (not (getf properties :initial-value))
               (getf default-values property-name))
      (setf (getf properties :initial-value) default-value))
    (cons (car option) properties)))
