;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.quasi-quote.test)

(defsuite* (test :in root-suite))

(def (definer e) syntax-test-definer (test-definer-name &body setup-forms)
  `(def definer ,test-definer-name (name args &body forms)
     (labels ((process-test-entry (test-function entry)
                (if (eq (first entry) 'with-expected-failures)
                    `(with-expected-failures
                       ,@(mapcar (lambda (form)
                                   (process-test-entry test-function form))
                                 (rest entry)))
                    (bind (((expected ast) entry))
                      `(,test-function ,expected (macroexpand (read-from-string ,ast)))))))
       `(def (test d) ,name ,args
          ,@(iter (for el :in ',setup-forms)
                  (bind (((&key test-function readtable-setup) el))
                    (collect `(bind ((*readtable* (copy-readtable *readtable*)))
                                ,readtable-setup
                                ,@(mapcar (lambda (form)
                                            (process-test-entry test-function form))
                                          forms)))))))))

(defun downcased-pretty-print (thing)
  (bind ((*print-case* :downcase))
    (pprint thing)))
