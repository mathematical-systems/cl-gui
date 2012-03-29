;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See COPYING for details.

(in-package :hu.dwim.walker)

#-(and allegro (version>= 7 0))
(error "This old version of Allegro CL is not supported by hu.dwim.walker")

(defun make-empty-lexical-environment ()
  (sys::make-augmentable-environment-boa :compilation))

(defun iterate-variables-in-lexenv (visitor lexenv &key include-ignored?
                                    (include-specials? t) include-macros?)
  (system::map-over-environment-variables
   (lambda (symbol type rest)
     (declare (ignore type rest))
     (multiple-value-bind (type env decl) (sys:variable-information symbol lexenv)
       (when type
         (let ((ignored (eq 'ignore (cadr (assoc 'ignore decl))))
               (special (eq :special type))
               (macro (eq :symbol-macro type)))
           (when (and (if include-ignored? t (not ignored))
                      (if include-specials? t (not special))
                      (if include-macros? t (not macro)))
             (funcall visitor symbol :ignored? ignored :special? special :macro? macro
                      :macro-body (and macro (car env))))))))
   lexenv))

(defun iterate-functions-in-lexenv (visitor lexenv &key include-macros?)
  (system::map-over-environment-functions
   (lambda (symbol type rest)
     (declare (ignore type rest))
     (multiple-value-bind (type env decl) (sys:function-information symbol lexenv)
       (declare (ignore decl))
       (when type
         (let ((macro (eq :macro type)))
           (when (or (eq :function type)
                     (and include-macros? macro))
             (funcall visitor symbol
                      :macro? macro :macro-function (and macro (car env))))))))
   lexenv))

(defun iterate-blocks-in-lexenv (visitor lexenv)
  ;; Note: doesn't work using compilation environment (and probably
  ;; won't work anytime soon [spr36543])
  (system::map-over-environment-blocks
   (lambda (symbol type rest)
     (declare (ignore rest))
     (when type
       (funcall visitor symbol)))
   lexenv))

(defun iterate-tags-in-lexenv (visitor lexenv)
  ;; Note: doesn't work using compilation environment (and probably
  ;; won't work anytime soon [spr36543])
  (system::map-over-environment-tags
   (lambda (symbol type rest)
     (declare (ignore rest))
     (when type
       (funcall visitor symbol)))
   lexenv))

(defun augment-lexenv-with-variable (name env &key special ignored)
  (declare (ignore special ignored))
  (system:augment-environment env :variable (list name)))

(defun augment-lexenv-with-function (fun env)
  (system:augment-environment env :function (list fun)))

(defun augment-lexenv-with-macro (mac def env)
  (system:augment-environment env :macro (list (list mac def))))

(defun augment-lexenv-with-symbol-macro (symmac def env)
  (system:augment-environment env :symbol-macro (list (list symmac def))))

(defun augment-lexenv-with-block (name env)
  (system:augment-environment env :block (list name)))

(defun augment-lexenv-with-tag (name env)
  (system:augment-environment env :tag (list name)))

;;; other

(defun proclaimed-special-variable?/global (symbol lexenv)
  (declare (ignore lexenv))
  (multiple-value-bind (type env decl) (sys:variable-information symbol)
    (declare (ignore env decl))
    (eq :special type)))

(defun declared-variable-type/global (symbol)
  (multiple-value-bind (type env decl) (sys:variable-information symbol)
    (declare (ignore type env))
    (cddr (assoc 'type decl)))) ;; TODO THL why is decl always nil?
