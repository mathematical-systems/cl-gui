;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.walker)

(declaim (ftype (function () t) make-empty-lexical-environment))

(defparameter *lexical-environment-functions*
  '((make-empty-lexical-environment  "Returns an empty lexical environment useful for testing and playing around in the repl.")
    (iterate-variables-in-lexenv     "(funcall VISITOR name &key ignored? special? macro? macro-body) for each variable or symbol macro definition in LEXENV.")
    (iterate-functions-in-lexenv     "(funcall VISITOR name &key macro? macro-function) for each function or macro definition in LEXENV.")
    (iterate-blocks-in-lexenv        "(funcall VISITOR name) for each block in LEXENV.")
    (iterate-tags-in-lexenv          "(funcall VISITOR name) for each tag in LEXENV.")
    (augment-lexenv-with-variable)
    (augment-lexenv-with-function)
    (augment-lexenv-with-macro)
    (augment-lexenv-with-symbol-macro)
    (augment-lexenv-with-block)
    (augment-lexenv-with-tag)
    ;; err, they are not lexical...
    (proclaimed-special-variable?/global)
    (declared-variable-type/global)
    ))

;;; set up some docstrings
(loop
   :for (name documentation) :in *lexical-environment-functions*
   :do (when documentation
         (setf (documentation name 'function) documentation)))

(defun missing-lexical-environment-function ()
  (cerror "ignore and try to continue" "This is not implemented for your lisp, sorry. You may try to continue, but...")
  nil)

;;; if there was no definition provided for some of the functions in
;;; *lexical-environment-functions* then register a function that will
;;; signal an error.
(eval-when (:load-toplevel :execute)
  (loop
     :for (name) :in *lexical-environment-functions*
     :do (unless (fboundp name)
           (setf (fdefinition name)
                 (lambda (&rest args)
                   (declare (ignore args))
                   (missing-lexical-environment-function))))))

(defun augment-lexenv (kind name lexenv &rest args)
  (ecase kind
    (:variable     (progn
                     (assert (null args))
                     (augment-lexenv-with-variable name lexenv)))
    (:function     (progn
                     (assert (null args))
                     (augment-lexenv-with-function name lexenv)))
    (:macro        (destructuring-bind (macro-definition) args
                     (augment-lexenv-with-macro name macro-definition lexenv)))
    (:symbol-macro (destructuring-bind (macro-definition) args
                     (augment-lexenv-with-symbol-macro name macro-definition lexenv)))
    (:block        (progn
                     (assert (null args))
                     (augment-lexenv-with-block name lexenv)))
    (:tag          (progn
                     (assert (null args))
                     (augment-lexenv-with-tag name lexenv)))))

(defmacro augment-lexenv! (kind name env &rest other-datum)
  `(setf ,env (augment-lexenv ,kind ,name ,env ,@other-datum)))

;;;
;;; variables
;;;
(def (macro e) do-variables-in-lexenv ((lexenv name &key
                                               (ignored? (gensym) ignored-provided?)
                                               (special? (gensym) special-provided?)
                                               (macro? (gensym) macro-provided?)
                                               (macro-body (gensym))
                                               (type (gensym) type-provided?))
                                        &body body)
  `(block nil
     (iterate-variables-in-lexenv
      (lambda (,name &key ((:ignored? ,ignored?) nil) ((:special? ,special?) nil)
          ((:macro? ,macro?) nil) ((:macro-body ,macro-body) nil) ((:type ,type) nil))
        (declare (ignorable ,@(unless ignored-provided? (list ignored?))
                            ,@(unless special-provided? (list special?))
                            ,@(unless macro-provided? (list macro? macro-body))
                            ,@(unless type-provided? (list type))))
        ,@body)
      ,lexenv
      :include-macros? ,macro-provided?
      :include-ignored? ,ignored-provided?
      :include-specials? ,special-provided?)))

(def (function e) collect-variables-in-lexenv (lexenv &key include-ignored? (include-specials? t) filter)
  (let ((result (list)))
    (iterate-variables-in-lexenv
     (lambda (name &key ignored? special? &allow-other-keys)
       (when (or (not filter)
                 (funcall filter name :ignored? ignored? :special? special?))
         (push name result)))
     lexenv
     :include-ignored? include-ignored?
     :include-specials? include-specials?)
    (nreverse result)))

(def (function e) find-variable-in-lexenv (name-to-find lexenv &key include-ignored? (include-specials? t))
  (iterate-variables-in-lexenv
   (lambda (name &key ignored? &allow-other-keys)
     (when (eq name name-to-find)
       (return-from find-variable-in-lexenv (values name ignored?))))
   lexenv
   :include-ignored? include-ignored?
   :include-specials? include-specials?)
  (values nil))

;;;
;;; functions
;;;
(def (macro e) do-functions-in-lexenv ((lexenv name &optional
                                               (macro? (gensym) macro-provided?)
                                               (macro-function (gensym) macro-function-provided?))
                                       &body body)
  `(block nil
     (iterate-functions-in-lexenv
      (lambda (,name &key ((:macro? ,macro?)) ((:macro-function ,macro-function)))
        (declare (ignorable ,@(unless macro-provided? (list macro?))
                            ,@(unless macro-function-provided? (list macro-function))))
        ,@body)
      ,lexenv
      :include-macros? ,macro-provided?)))

(def (function e) collect-functions-in-lexenv (lexenv &key filter)
  (let ((result (list)))
    (iterate-functions-in-lexenv
     (lambda (name &key &allow-other-keys)
       (when (or (not filter)
                 (funcall filter name))
         (push name result)))
     lexenv)
    (nreverse result)))

(def (function e) find-function-in-lexenv (name-to-find lexenv)
  (iterate-functions-in-lexenv
   (lambda (name &key &allow-other-keys)
     (when (eq name name-to-find)
       (return-from find-function-in-lexenv (values name))))
   lexenv)
  (values nil))

;;;
;;; macros
;;;
(def (macro e) do-macros-in-lexenv ((lexenv name &optional (macro-fn (gensym) macro-fn-provided?))
                                     &body body &aux (macro? (gensym)))
  `(block nil
     (iterate-functions-in-lexenv
      (lambda (,name &key ((:macro? ,macro?)) ((:macro-function ,macro-fn)))
        ,@(unless macro-fn-provided?
                  `((declare (ignore ,macro-fn))))
        (when ,macro?
          ,@body))
      ,lexenv
      :include-macros? t)))

(def (function e) collect-macros-in-lexenv (lexenv &key filter)
  (let ((result (list)))
    (iterate-functions-in-lexenv
     (lambda (name &key macro? macro-function)
       (declare (ignore macro-function))
       (when (and macro?
                  (or (not filter)
                      (funcall filter name)))
         (push name result)))
     lexenv
     :include-macros? t)
    (nreverse result)))

(def (function e) find-macro-in-lexenv (name-to-find lexenv)
  (iterate-functions-in-lexenv
   (lambda (name &key macro? macro-function)
     (when (and macro? (eq name name-to-find))
       (return-from find-macro-in-lexenv (values name macro-function))))
   lexenv
   :include-macros? t)
  (values nil))

;;;
;;; symbol-macros
;;;
(def (macro e) do-symbol-macros-in-lexenv ((lexenv name &optional (definition (gensym) definition-provided?))
                                            &body body)
  (with-unique-names (macro?)
    `(block nil
       (iterate-variables-in-lexenv
        (lambda (,name &key ((:macro-body ,definition)) ((:macro? ,macro?)) &allow-other-keys)
          ,@(unless definition-provided?
                    `((declare (ignore ,definition))))
          (when ,macro?
            ,@body))
        ,lexenv
        :include-macros? t))))

(def (function e) collect-symbol-macros-in-lexenv (lexenv &key filter)
  (let ((result (list)))
    (iterate-variables-in-lexenv
     (lambda (name &key macro-body macro? &allow-other-keys)
       (declare (ignore macro-body))
       (when (and macro?
                  (or (not filter)
                      (funcall filter name)))
         (push name result)))
     lexenv
     :include-macros? t)
    (nreverse result)))

(def (function e) find-symbol-macro-in-lexenv (name-to-find lexenv)
  (iterate-variables-in-lexenv
   (lambda (name &key macro-body macro? &allow-other-keys)
     (when (and macro? (eq name name-to-find))
       (return-from find-symbol-macro-in-lexenv (values name macro-body))))
   lexenv
   :include-macros? t)
  (values nil))

;;;
;;; blocks
;;;
(def (macro e) do-blocks-in-lexenv ((lexenv name) &body body)
  `(block nil
     (iterate-blocks-in-lexenv
      (lambda (,name)
        ,@body)
      ,lexenv)))

(def (function e) collect-blocks-in-lexenv (lexenv &key filter)
  (let ((result (list)))
    (iterate-blocks-in-lexenv
     (lambda (name)
       (when (or (not filter)
                 (funcall filter name))
         (push name result)))
     lexenv)
    (nreverse result)))

(def (function e) find-block-in-lexenv (name-to-find lexenv)
  (iterate-blocks-in-lexenv
   (lambda (name)
     (when (eq name name-to-find)
       (return-from find-block-in-lexenv (values name))))
   lexenv)
  (values nil))

;;;
;;; tags
;;;
(def (macro e) do-tags-in-lexenv ((lexenv name) &body body)
  `(block nil
     (iterate-tags-in-lexenv
      (lambda (,name)
        ,@body)
      ,lexenv)))

(def (function e) collect-tags-in-lexenv (lexenv &key filter)
  (let ((result (list)))
    (iterate-tags-in-lexenv
     (lambda (name)
       (when (or (not filter)
                 (funcall filter name))
         (push name result)))
     lexenv)
    (nreverse result)))

(def (function e) find-tag-in-lexenv (name-to-find lexenv)
  (iterate-tags-in-lexenv
   (lambda (name)
     (when (eq name name-to-find)
       (return-from find-tag-in-lexenv (values name))))
   lexenv)
  (values nil))
