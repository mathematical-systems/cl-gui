;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.common)

(defmacro dolist ((iterator list &optional return-value) &body body)
  "Like DOLIST except when ITERATOR is a cons, in which case it DESTRUCTURING-BIND's the elements of LIST to it."
  (if (listp iterator)
      (let ((i (gensym "DOLIST-I-")))
        `(common-lisp:dolist (,i ,list ,return-value)
           (destructuring-bind ,iterator ,i
             ,@body)))
      `(common-lisp:dolist (,iterator ,list ,return-value)
         (let ((,iterator ,iterator))
           ,@body))))

(defun export-external-symbols (source-package target-package &key filter (if-exists :ignore))
  (check-type if-exists (member :error :warn :ignore))
  (setf target-package (find-package target-package))
  (do-external-symbols (symbol source-package)
    (when (or (not filter)
              (funcall filter symbol))
      ;; do take care of the symbol nil: (list nil)!
      (block importing-one-symbol
        (let* ((symbol (or symbol (list nil)))
               (target-symbol (if (symbolp symbol)
                                  (find-symbol (symbol-name symbol) target-package))))
          (when (and target-symbol
                     (not (eq symbol target-symbol)))
            (ecase if-exists
              (:error
               ;; let the IMPORT call below signal an error for us with some useful restarts
               (import symbol target-package))
              (:warn
               (warn 'simple-style-warning "Symbol ~S already exists in package ~A. Using ~S, the already present one."
                     symbol target-package target-symbol)
               (setf symbol target-symbol))
              (:ignore
               (setf symbol target-symbol))))
          (export symbol target-package))))))

(defun export-external-symbols-of-used-packages (package &key filter)
  (dolist (used-package (package-use-list (find-package package)))
    (export-external-symbols used-package package :filter filter)))

(export-external-symbols-of-used-packages :hu.dwim.common)

(defun import-all-owned-symbols (source-package target-package &key (overwrite nil))
  (setf source-package (find-package source-package))
  (setf target-package (find-package target-package))
  (let ((count 0))
    (do-symbols (symbol source-package)
      (let ((target-symbol-with-same-name (find-symbol (symbol-name symbol) target-package)))
        (when (and (eq (symbol-package symbol) source-package)
                   (or overwrite
                       (not target-symbol-with-same-name)))
          (when (and target-symbol-with-same-name
                     (not (eq symbol target-symbol-with-same-name))
                     overwrite)
            (unintern target-symbol-with-same-name target-package))
          (import symbol target-package)
          (incf count))))
    count))
