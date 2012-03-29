;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(defun find-function-definer-option-transformer (name)
  (bind ((name (find-symbol "TRANSFORM-FUNCTION-DEFINER-OPTIONS"
                            (symbol-package (if (consp name)
                                                (second name)
                                                name)))))
    (if (and name
             (fboundp name))
        (fdefinition name)
        #'transform-function-definer-options)))

(defun normalize-debug-level (level)
  (ecase level
    ((nil) 0)
    ((t) 3)
    ((0 1 2 3) level)))

(defun transform-function-definer-options (options)
  (bind ((debug-level (normalize-debug-level
                       (getf options :debug (max #+sbcl (sb-c::policy-quality sb-c::*policy* 'debug)
                                                 (if *load-as-production?*
                                                     0
                                                     1))))))
    (when (> debug-level 0)
      (remove-from-plistf options :inline :optimize))
    (list* :debug debug-level
           (remove-from-plist options :debug))))

(defun function-like-definer-declarations (-options-)
  (bind ((debug-level (normalize-debug-level (getf -options- :debug))))
    (if (zerop debug-level)
        (when (getf -options- :optimize)
          '((optimize (speed 3) (debug 0) (safety 2))))
        (progn
          (when (getf -options- :optimize)
            (warn "Ignoring 'O'ptimize flag because 'D'ebug was also specified"))
          `((optimize (speed 0) (debug ,debug-level)))))))

(defun %function-like-definer (definer-macro-name &key whole options allow-compound-name)
  (bind ((body (nthcdr 2 whole))
         (name (pop body))
         (name-symbol (if allow-compound-name
                          (cond
                            ((and (consp name)
                                  (length= 2 name)
                                  (eq (first name) 'setf))
                             name)
                            ((consp name)
                             (first name))
                            ((symbolp name)
                             name)
                            (t (error "Don't know how to deal with definition name ~S" name)))
                          name))
         (args (pop body)))
    (awhen (find-function-definer-option-transformer name-symbol)
      (setf options (funcall it options)))
    (bind (((:values body declarations documentation) (parse-body body :documentation t :whole whole))
           (outer-declarations (function-like-definer-declarations options))
           ;; KLUDGE https://bugs.launchpad.net/sbcl/+bug/562911
           (process-inline? (not (eq definer-macro-name 'defmacro))))
      `(progn
         ,@(when (and process-inline?
                      (getf options :inline))
                 `((declaim (inline ,name-symbol))))
         ,@(when (and process-inline?
                      (< 0 (getf options :debug)))
                 `((declaim (notinline ,name-symbol))))
         (locally
             (declare ,@outer-declarations)
           ,@(when (getf options :export)
                   `((eval-when (:compile-toplevel :load-toplevel :execute)
                       (export ',name-symbol))))
           (,definer-macro-name ,name ,args
             ,@(when documentation
                     (list documentation))
             ,@declarations
             ,@body))
         ,@(when (eq (getf options :inline) :possible)
             `((declaim (notinline ,name-symbol))))))))

(defmacro function-like-definer (definer-macro-name &key allow-compound-name)
  `(%function-like-definer ',definer-macro-name :whole -whole- :options -options-
                           :allow-compound-name ,allow-compound-name))

(defun %defmethods-like-definer (definer-macro-name -whole- -options-)
  (bind ((body (nthcdr 2 -whole-))
         (name (pop body))
         (outer-declarations (function-like-definer-declarations -options-)))
    `(locally
         (declare ,@outer-declarations)
       ,@(when (getf -options- :export)
               `((export ',name)))
       ,@(iter (for entry :in body)
               (when (eq (first entry) :method)
                 (pop entry))
               (collect `(,definer-macro-name ,name ,@entry))))))

(defmacro defmethods-like-definer (definer-macro-name)
  `(%defmethods-like-definer ',definer-macro-name -whole- -options-))
