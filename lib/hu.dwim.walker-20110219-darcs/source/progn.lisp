;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.walker)

(def (form-class e) implicit-progn-mixin ()
  ((body :ast-link t)))

(def print-object implicit-progn-mixin
  (format t "~A" (body-of -self-)))

(def (form-class e) implicit-progn-with-declarations-mixin (implicit-progn-mixin)
  ((declarations nil :ast-link t)))

(def (form-class e) binder-form-mixin ()
  ((bindings :ast-link t)))

(def (form-class e) declaration-form ()
  ())

(def (form-class e) optimize-declaration-form (declaration-form)
  ((specification :accessor specification-of :initarg :specification)))

(def unwalker optimize-declaration-form (specification)
  `(optimize ,specification))

(def (form-class e) variable-declaration-form (declaration-form
                                               named-walked-form)
  ())

(def (form-class e) function-declaration-form (declaration-form
                                               named-walked-form)
  ())

(def (form-class e) dynamic-extent-declaration-form-mixin (declaration-form)
  ())

(def (form-class e) variable-dynamic-extent-declaration-form (variable-declaration-form
                                                              dynamic-extent-declaration-form-mixin)
  ())

(def unwalker variable-dynamic-extent-declaration-form (name)
  `(dynamic-extent ,name))

(def (form-class e) function-dynamic-extent-declaration-form (function-declaration-form
                                                              dynamic-extent-declaration-form-mixin)
  ())

(def unwalker function-dynamic-extent-declaration-form (name)
  `(dynamic-extent #',name))

(def (form-class e) ignorable-declaration-form-mixin (declaration-form)
  ())

(def (form-class e) variable-ignorable-declaration-form (variable-declaration-form ignorable-declaration-form-mixin)
  ())

(def unwalker variable-ignorable-declaration-form (name)
  `(ignorable ,name))

(def (form-class e) function-ignorable-declaration-form (function-declaration-form ignorable-declaration-form-mixin)
  ())

(def unwalker function-ignorable-declaration-form (name)
  `(ignorable (function ,name)))

(def (form-class e) special-variable-declaration-form (variable-declaration-form)
  ())

(def unwalker special-variable-declaration-form (name)
  `(special ,name))

(def (form-class e) type-declaration-form (variable-declaration-form)
  ((declared-type)))

(def unwalker type-declaration-form (declared-type name)
  `(type ,declared-type ,name))

(def (form-class e) ftype-declaration-form (function-declaration-form)
  ((declared-type)))

(def unwalker ftype-declaration-form (declared-type name)
  `(ftype ,declared-type ,name))

(def (form-class e) notinline-declaration-form (function-declaration-form)
  ())

(def unwalker notinline-declaration-form (name)
  `(notinline ,name))

(def (form-class e) inline-declaration-form (function-declaration-form)
  ())

(def unwalker inline-declaration-form (name)
  `(inline ,name))

(def (form-class e) unknown-declaration-form (declaration-form)
  ((declaration-form :initarg :declaration-form :accessor declaration-form-of)))

(def method name-of ((self unknown-declaration-form))
  (first (declaration-form-of self)))

(def unwalker unknown-declaration-form (declaration-form)
  declaration-form)

;;; built-in declaration walkers

(def layered-method walk-declaration (type declaration parent environment)
  (when (member type *known-direct-type-declarations*)
    (return-from walk-declaration
      (walk-declaration 'type (list* 'type declaration) parent environment)))
  (unless (or (member type *known-declaration-types* :test #'eq)
              (and (symbolp type)
                   (member (symbol-package type) *known-system-packages* :test #'eq)))
    (simple-walker-style-warning "Ignoring unknown declaration ~S while walking forms. If it's a type declaration, then use the full form to avoid this warning: `(type ,type ,@variables), or you can also (pushnew ~S ~S) or (pushnew ~S ~S)."
                                 declaration type '*known-declaration-types*
                                 type '*known-direct-type-declarations*))
  (list (make-form-object 'unknown-declaration-form parent
                          :declaration-form declaration)))

(def declaration-walker ftype (type &rest names)
  (do-list-collect (function-name names)
    (make-declaration 'ftype-declaration-form
                      :name function-name :declared-type type)))

(def declaration-walker type (type &rest vars)
  (do-list-collect (var vars)
    (make-declaration 'type-declaration-form
                      :name var :declared-type type)))

(def declaration-walker dynamic-extent (&rest vars)
  (do-list-collect (var vars)
    (aif (function-name var)
         (make-declaration 'function-dynamic-extent-declaration-form :name it)
         (make-declaration 'variable-dynamic-extent-declaration-form :name var))))

(def declaration-walker ignorable (&rest names)
  (do-list-collect (var names)
    (aif (function-name var)
         (make-declaration 'function-ignorable-declaration-form :name it)
         (make-declaration 'variable-ignorable-declaration-form :name var))))

(def declaration-walker ignore (&rest names)
  (do-list-collect (var names)
    (aif (function-name var)
         (make-declaration 'function-ignorable-declaration-form :name it)
         (make-declaration 'variable-ignorable-declaration-form :name var))))

(def declaration-walker inline (&rest functions)
  (do-list-collect (function functions)
    (make-declaration 'inline-declaration-form :name function)))

(def declaration-walker notinline (&rest functions)
  (do-list-collect (function functions)
    (make-declaration 'notinline-declaration-form :name function)))

(def declaration-walker optimize (&rest specs)
  (do-list-collect (optimize-spec specs)
    (make-declaration 'optimize-declaration-form :specification optimize-spec)))

(def declaration-walker special (&rest vars)
  (do-list-collect (var vars)
    (make-declaration 'special-variable-declaration-form :name var)))

(defun unwalk-declarations (decls)
  ;; Return a list so declarations can be easily spliced.
  (if (null decls)
      nil
      (list `(declare ,@(unwalk-forms decls)))))

(def function walk-declarations (declarations parent env)
  (loop :for declaration :in declarations
    :do (assert (eq (first declaration) 'declare))
    :append
      (loop :for entry :in (rest declaration)
        :append
          (with-current-form entry
            (walk-declaration (car entry) entry parent env)))))

(def function augment-with-special-vars (env declarations local-names)
  ;; Append special var markers
  (dolist (form declarations)
    (when (typep form 'special-variable-declaration-form)
      (let* ((name (name-of form))
             (type-form (find-form-by-name name declarations
                                           :type 'type-declaration-form))
             (type (if type-form (declared-type-of type-form))))
        (walk-environment/augment! env :unwalked-variable name (cons :special type))
        (push name local-names))))
  ;; Append floating type declarations (i.e. skip vars defined here)
  (dolist (form declarations)
    (when (and (typep form 'type-declaration-form)
               (not (member (name-of form) local-names)))
      (walk-environment/augment! env :variable-type (name-of form) (declared-type-of form))))
  ;; Done
  env)

(def (function e) walk-implict-progn (parent forms walk-env &key declarations-callback docstring-allowed declarations-allowed (whole *current-form*))
  (assert (and (typep parent 'implicit-progn-mixin)
               (or (not declarations-allowed)
                   (typep parent 'implicit-progn-with-declarations-mixin))))
  (check-type walk-env walk-environment)
  (bind (((:values body declarations docstring) (parse-body forms :documentation docstring-allowed :whole whole)))
    (when docstring-allowed
      (setf (docstring-of parent) docstring))
    (when (and declarations
               (not declarations-allowed))
      (error "Declarations are not allowed at ~S" whole))
    (when declarations-allowed
      (bind ((walked-declarations (walk-declarations declarations parent walk-env))
             (local-names nil))
        (setf (declarations-of parent) walked-declarations)
        (when declarations-callback
          ;; always call declarations-callback because some crucial sideffects may happen inside them (like in LET's walker)
          (setf (values walk-env local-names) (funcall declarations-callback walked-declarations)))
        ;; Add special declarations to the environment
        (setf walk-env (augment-with-special-vars walk-env walked-declarations local-names))))
    (setf (body-of parent) (mapcar (lambda (form)
                                     (walk-form form :parent parent :environment walk-env))
                                   (coerce-to-form body)))
    (values)))
