;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.walker)

;;;;;;
;;; Empty environment

(def function make-empty-lexical-environment ()
  (sb-kernel:make-null-lexenv))

;;;;;;
;;; Miscellaneous

(def function proclaimed-special-variable?/global (name lexenv)
  (declare (ignore lexenv))
  (eq (sb-int:info :variable :kind name) :special))

(def function declared-variable-type/global (name)
  (bind (((:values type found) (sb-int:info :variable :type name)))
    (if found
        (values (sbcl-unparse-type type) found)
        (values +top-type+ nil))))

(def function sbcl-unparse-type (type)
  (if (null type)
      +top-type+
      (bind ((info (sb-kernel::type-class-info type)))
        (funcall (sb-kernel::type-class-unparse info) type))))

;;;;;;
;;; Iteration

(defun iterate-variables-in-lexenv (visitor lexenv &key
                                    include-ignored? (include-specials? t) include-macros?)
  (loop
     :with types = (sb-c::lexenv-type-restrictions lexenv)
     :for entry :in (sb-c::lexenv-vars lexenv)
     :for name = (first entry)
     :for definition = (rest entry)
     :for ignored? = (and (typep definition 'sb-c::lambda-var)
                          (sb-c::lambda-var-ignorep definition))
     :for special? = (typep definition 'sb-c::global-var)
     :for type = (or (cdr (assoc definition types))
                     (typecase definition
                       (sb-c::global-var (sb-c::global-var-type definition))
                       (sb-c::lambda-var (sb-c::lambda-var-type definition))
                       (t nil)))
     :if (and (consp definition)
              (eq 'sb-sys::macro (first definition)))
     :do (when include-macros?
           (funcall visitor name :macro? t :macro-body (rest definition)))
     :else
     :do (when (and (or (not ignored?)
                        include-ignored?)
                    (or (not special?)
                        include-specials?))
           (funcall visitor name :ignored? ignored? :special? special?
                    :type (sbcl-unparse-type type)))))

(defun iterate-functions-in-lexenv (visitor lexenv &key include-macros?)
  (loop
     :for entry :in (sb-c::lexenv-funs lexenv)
     :for name = (first entry)
     :for definition = (rest entry)
     :for macro? = (and (consp definition)
                        (eq 'sb-sys::macro (first definition)))
     :do (if macro?
             (when include-macros?
               (funcall visitor name :macro? t
                        :macro-function (rest definition)))
             (funcall visitor name))))

(defun iterate-blocks-in-lexenv (visitor lexenv)
  (loop
     :for entry :in (sb-c::lexenv-blocks lexenv)
     :for name = (first entry)
     :do (funcall visitor name)))

(defun iterate-tags-in-lexenv (visitor lexenv)
  (loop
     :for entry :in (sb-c::lexenv-tags lexenv)
     :for name = (first entry)
     :do (funcall visitor name)))

;;;;;;
;;; Augmentation

(defun augment-lexenv-with-variable (name lexenv &key special ignored)
  (let ((var (if special
                 (sb-c::make-global-var :%source-name name :kind :special)
                 (sb-c::make-lambda-var :%source-name name))))
    (when ignored
      (setf (sb-c::lambda-var-ignorep var) t))
    (sb-c::make-lexenv :default lexenv
                       :vars (list (cons name var)))))

(defun augment-lexenv-with-function (name lexenv)
  (sb-c::make-lexenv :default lexenv :funs (list (cons name t))))

(defun augment-lexenv-with-macro (name def lexenv)
  (sb-c::make-lexenv :default lexenv :funs (list (list* name 'sb-sys::macro def))))

(defun augment-lexenv-with-symbol-macro (name def lexenv)
  (sb-c::make-lexenv :default lexenv :vars (list (list* name 'sb-sys::macro def))))

(defun augment-lexenv-with-block (name lexenv)
  (sb-c::make-lexenv :default lexenv :blocks (list (list name))))

(defun augment-lexenv-with-tag (name lexenv)
  (sb-c::make-lexenv :default lexenv :tags (list (list name))))
