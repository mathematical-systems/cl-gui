;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.quasi-quote.js)

(def layer js (ignore-undefined-references))

(def special-variable *js-macros*               (make-hash-table :test 'eq))
(def special-variable *js-symbol-macros*        (make-hash-table :test 'eq))
(def special-variable *js-special-forms*        (make-hash-table :test 'eq))
(def special-variable *js-literals*             (make-hash-table :test 'eq))
(def special-variable *js-unique-counter*       (or (when (boundp '*js-unique-counter*)
                                                      *js-unique-counter*)
                                                    ;; protect it from being reseted at an accidental reload
                                                    0))
;;(def special-variable *js-operator-name->arity* (make-hash-table :test 'eq))
(def special-variable *js-reserved-keywords*
    (bind ((table (make-hash-table :test #'equal)))
      (dolist (name '(break case catch continue default delete do else false finally for function if in instanceof new
                      null return switch this throw true try typeof var void while with
                      class ; this is not reserver, buf screws up IE...
                      ))
        (setf name (string-downcase name))
        (setf (gethash name table) t))
      table))

(def (function ie) unique-js-name (&optional (prefix "g"))
  (concatenate 'string prefix (integer-to-string (incf *js-unique-counter*))))

(def (macro e) with-unique-js-names (names &body body)
  `(bind (,@(iter (for name :in names)
                  (collect `(,name (make-symbol (unique-js-name ,(string name)))))))
     ,@body))

(def (function io) js-special-form? (name)
  (nth-value 1 (gethash name *js-special-forms*)))

(def (function io) js-literal-name? (name)
  (nth-value 1 (gethash name *js-literals*)))

(def layered-method hu.dwim.walker::constant-name? :in js (form &optional env)
  (declare (ignore env))
  (or (gethash form *js-literals*)
      (keywordp form)
      (and (not (symbolp form))
           (not (consp form)))))

(def layered-method hu.dwim.walker::function-name? :in js (name)
  (declare (ignore name))
  #t)

(def layered-method hu.dwim.walker::macro-name? :in js (name &optional env)
  (declare (ignore env))
  (and (not (js-special-form? name))
       (not (null (js-macro-definition name)))))

(def layered-method hu.dwim.walker::symbol-macro-name? :in js (name &optional env)
  (and (not (js-special-form? name))
       (nth-value 1 (macroexpand-1 name env))))

(def layered-method hu.dwim.walker::lambda-form? :in js (form &optional env)
  (declare (ignore env))
  (and (consp form)
       (member (car form) '(cl:lambda |lambda|))
       #t))

(def layered-method hu.dwim.walker::walker-macroexpand-1 :in js (form &optional env)
  (declare (ignore env)) ; TODO check the env for macrolets?
  (if (consp form)
      (bind ((name (first form))
             (expander (js-macro-definition name)))
        (if expander
            (values (funcall expander form) #t)
            (values form #f)))
      (values form #f)))

(def function js-macro-definition (name)
  (gethash name *js-macros*))

(def function (setf js-macro-definition) (value name)
  (assert (and name (symbolp name)))
  (if value
      (progn
        (when (gethash name *js-macros*)
          (simple-style-warning "Redefining js macro ~S" name))
        (setf (gethash name *js-macros*) value))
      (remhash name *js-macros*))
  value)

(def (definer e :available-flags "e") js-macro (name args &rest body)
  "Define a javascript macro, and store it in the toplevel macro environment."
  ;; TODO (undefine-js-compiler-macro name)
  (with-unique-names (form)
    (with-standard-definer-options name
      `(progn
         (setf (js-macro-definition ',name)
               (lambda (,form)
                 (destructuring-bind ,args (rest ,form) ,@body)))
         ',name))))

(def (definer e :available-flags "e") js-lisp-macro-alias (lisp-name &optional (js-name (intern (string-downcase lisp-name))))
  (with-standard-definer-options js-name
    `(setf (js-macro-definition ',js-name)
           (lambda (form)
             (macroexpand `(,',lisp-name ,@(rest form)))))))

(def (definer :available-flags "e") js-literal (name string)
  (bind ((lowercase-name (intern (string-downcase name))))
    `(progn
       (setf (gethash ',name *js-literals*) ,string)
       (setf (gethash ',lowercase-name *js-literals*) ,string)
       ,@(when (getf -options- :export)
           `((export '(,name ,lowercase-name)))))))

(macrolet ((frob (&body entries)
             `(progn
                ,@(iter (for (name js-name) :in entries)
                        (collect `(def (js-literal e) ,name ,js-name))))))
  (frob
   (this      "this")
   (t         "true")
   (true      "true")
   (false     "false")
   (nil       "null")
   (undefined "undefined")))

(def macro with-lexical-transform-functions (&body body)
  `(labels ((recurse (form)
              (bind ((*in-js-statement-context* #f))
                (transform-quasi-quoted-js-to-quasi-quoted-string form)))
            (recurse-as-comma-separated (form &optional (recurse-fn #'recurse))
              (bind ((recurse-fn (ensure-function recurse-fn)))
                (iter (for el :in form)
                      (unless (first-iteration-p)
                        (collect ", "))
                      (collect (funcall recurse-fn el))))))
     (declare (ignorable #'recurse #'recurse-as-comma-separated))
     ,@body))

(def (definer :available-flags "e") js-special-form (name &body body)
  (with-standard-definer-options name
    `(setf (gethash ',name *js-special-forms*)
           (named-lambda ,(symbolicate '#:js-special-form/ name) (-node-)
             (declare (ignorable -node-))
             (with-lexical-transform-functions
               ,@body)))))

;; https://developer.mozilla.org/en/Core_JavaScript_1.5_Reference/Operators/Operator_Precedence
(def special-variable *js-operator-name->precedence*
  (bind ((result (make-hash-table :test 'eq)))
    (iter
      (for precedence :upfrom 1)
      ;; TODO whats up with: js-expression-if, js-assign, comma, js-aref, js-slot-value?
      (for operators :in '((member)
                           (new |new|)
                           (function-call)
                           (++ --)
                           (not |not| ! ~ typeof |typeof| delete |delete|)
                           (* / %)
                           (+ -)
                           (<< >> >>>)
                           (< > <= >=)
                           (in |in|)
                           (instanceof |instanceof|)
                           (== != === !== |equal| equal |eql| eql |eq| eq)
                           (&)
                           (^)
                           (\|)
                           (and |and| \&\&)
                           (or |or| \|\|)
                           (conditional)
                           (= += -= *= /= %= <<= >>= >>>= \&= ^= \|=)
                           (|comma| comma)))
      (dolist (operator operators)
        (export operator :hu.dwim.quasi-quote.js)
        (setf (gethash operator result) precedence)))
    result))

(def function operator-precedence (op)
  (gethash op *js-operator-name->precedence*))

(def function js-operator-name? (name)
  (not (null (operator-precedence name))))

(def (function o) lisp-operator-name-to-js-operator-name (op)
  (case op
    (|and|   '&&)
    (|or|    '\|\|)
    (|not|   '!)
    (|eq|    '===)
    (|equal| '==)
    (|eql|   '==)
    (=       '==)
    (t op)))
