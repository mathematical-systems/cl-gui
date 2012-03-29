;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2007 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-def)

(def function annotate-function-form-with-sideffect-tracing (form &optional environment)
  (flet ((walk-form (form)
           (hu.dwim.walker:with-walker-configuration (:macro-name? (lambda (name &optional env)
                                                                     (and (not (eq name 'defun))
                                                                          (macro-function name env))))
             (hu.dwim.walker:walk-form form :environment environment))))
    (bind ((walked (walk-form form)))
      (assert (typep walked 'hu.dwim.walker:function-definition-form))
      (hu.dwim.walker:map-ast (lambda (form)
                                (if (typep form 'hu.dwim.walker:setq-form)
                                    (bind ((parent (hu.dwim.walker:parent-of form)))
                                      (assert (typep parent 'hu.dwim.walker:implicit-progn-mixin))
                                      (setf (hu.dwim.walker:body-of parent)
                                            (substitute (walk-form `(progn
                                                                      (print ',(hu.dwim.walker:unwalk-form form) *debug-io*)
                                                                      ,(hu.dwim.walker:unwalk-form form)))
                                                        form
                                                        (hu.dwim.walker:body-of parent))))
                                    form))
                              walked)
      (hu.dwim.walker:unwalk-form walked))))
