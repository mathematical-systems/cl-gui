;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.walker.test)

(defsuite* (test/semantics :in test))

(def test test/semantics/let/1 ()
  (bind ((walked (walk-form '(let ((foo 42)
                                   (spec 101))
                               (declare (special spec))
                               foo
                               spec
                               free)))
         (body (body-of walked))
         (binding-form (first (bindings-of walked)))
         (binding-value-form (initial-value-of binding-form))
         (variable-reference-form (first body)))
    (is (= 3 (length body)))
    (is (typep binding-value-form 'constant-form))
    (is (eql (value-of binding-value-form) 42))
    (is (typep variable-reference-form 'walked-lexical-variable-reference-form))
    (is (eq binding-form (definition-of variable-reference-form)))
    (is (typep (second body) 'special-variable-reference-form))
    (is (not (typep (second body) 'free-variable-reference-form)))
    (is (typep (third body) 'free-variable-reference-form))
    walked))

(def test test/semantics/let*/1 ()
  (bind ((walked (walk-form '(let* ((foo 42)
                                    (bar foo)
                                    (spec 101))
                               (declare (special spec))
                               foo
                               bar
                               spec
                               free)))
         (body (body-of walked))
         (bindings (bindings-of walked))
         (foo-binding-form (first bindings))
         (bar-binding-form (second bindings))
         (foo-reference-form (first body))
         (bar-reference-form (second body)))
    (annotate-binding-usage walked)
    (is (= 4 (length body)))
    (is (typep foo-reference-form 'walked-lexical-variable-reference-form))
    (is (typep bar-reference-form 'walked-lexical-variable-reference-form))
    (is (typep (initial-value-of foo-binding-form) 'constant-form))
    (is (typep (initial-value-of bar-binding-form) 'walked-lexical-variable-reference-form))
    (is (eq (definition-of (initial-value-of bar-binding-form)) foo-binding-form))
    (is (eq (definition-of foo-reference-form) foo-binding-form))
    (is (set-equal (usages-of foo-binding-form)
                   (list (initial-value-of bar-binding-form)
                         foo-reference-form)))
    (is (eq (definition-of bar-reference-form) bar-binding-form))
    (is (typep (third body) 'special-variable-reference-form))
    (is (not (typep (third body) 'free-variable-reference-form)))
    (is (typep (fourth body) 'free-variable-reference-form))
    walked))

(def test test/semantics/specials/1 ()
  (bind ((walked (eval
                  '(bind ((a-var 42)
                          (b-var 43)
                          (c-var 44)
                          (d-var 45))
                    (declare (special a-var b-var)
                             (ignorable a-var b-var c-var d-var))
                    (macrolet
                        ((macro (&environment lexenv)
                           (walk-form '(let ((a-var 142)
                                             (b-var 143)
                                             (c-var 144)
                                             (d-var 145))
                                        (declare (special a-var c-var))
                                        a-var b-var c-var d-var)
                                      :environment (make-walk-environment lexenv))))
                      (macro)))))
         (body (body-of walked)))
    (is (= 4 (length body)))
    (is (typep (first body)  'special-variable-reference-form))
    ;; B-VAR is a special binding, because special declarations are only in effect until a new binding of the same name
    (is (typep (second body) 'lexical-variable-reference-form))
    (is (typep (third body)  'special-variable-reference-form))
    (is (typep (fourth body) 'lexical-variable-reference-form))
    (is (special-binding?      (first (bindings-of walked))))
    (is (not (special-binding? (second (bindings-of walked)))))
    (is (special-binding?      (third (bindings-of walked))))
    (is (not (special-binding? (fourth (bindings-of walked)))))
    walked))

(def test test/semantics/specials/reference-to-binding ()
  (bind ((walked (walk-form '(let ((a-var 43))
                               (declare (special a-var))
                               a-var)))
         (body (body-of walked))
         (variable-binding (first (bindings-of walked)))
         (variable-reference (first body)))
    (is (= 1 (length body)))
    (is (typep variable-reference 'special-variable-reference-form))
    (is (eq (name-of variable-reference) 'a-var))
    (is (special-binding? variable-binding))
    (with-expected-failures
      (is (eql (value-of (initial-value-of (definition-of variable-reference))) 43)))
    walked))

(def test test/semantics/specials/2 ()
  (with-captured-lexical-environment
      (env (let ((spec1 1)
                 (spec3 0)
                 (lex1 2))
             (declare (special spec1 spec2 lex2))
             ;; here: spec3 lexical, lex2 special
             (locally
                 (declare (special spec3 spec4 lex3))
               ;; here spec3 shadowed by a special
               (let ((lex2 3)
                     (lex3 4))
                 ;; here lex2 & lex3 are lexical because
                 ;; let is affected only by global and
                 ;; immediate special declarations.
                 -here-))))
    (bind ((walked (walk-form '(progn spec1 spec2 spec3 spec4
                                lex1 lex2 lex3)
                              :environment (make-walk-environment env)))
           (body (body-of walked)))
      (is (typep (nth 0 body) 'special-variable-reference-form))
      (is (typep (nth 1 body) 'special-variable-reference-form))
      (is (typep (nth 2 body) 'special-variable-reference-form))
      (is (typep (nth 3 body) 'special-variable-reference-form))
      (is (typep (nth 4 body) 'unwalked-lexical-variable-reference-form))
      (is (typep (nth 5 body) 'unwalked-lexical-variable-reference-form))
      (is (typep (nth 6 body) 'unwalked-lexical-variable-reference-form)))))

(def test test/semantics/specials/3 ()
  ;; Same as 2, but inside the walker
  (bind ((walked (walk-form `(let ((spec1 1)
                                   (spec3 0)
                                   (lex1 2))
                               (declare (special spec1 spec2 lex2))
                               ;; here: spec3 lexical, lex2 special
                               (locally
                                   (declare (special spec3 spec4 lex3))
                                 ;; here spec3 shadowed by a special
                                 (let ((lex2 3)
                                       (lex3 4))
                                   ;; here lex2 & lex3 are lexical because
                                   ;; let is affected only by global and
                                   ;; immediate special declarations.
                                   (progn spec1 spec2 spec3 spec4
                                          lex1 lex2 lex3))))))
         (locally-form (first (body-of walked)))
         (let-form (first (body-of locally-form)))
         (body (body-of (first (body-of let-form)))))
    (is (typep (nth 0 body) 'special-variable-reference-form))
    (is (typep (nth 1 body) 'special-variable-reference-form))
    (is (typep (nth 2 body) 'special-variable-reference-form))
    (is (typep (nth 3 body) 'special-variable-reference-form))
    (is (typep (nth 4 body) 'walked-lexical-variable-reference-form))
    (is (typep (nth 5 body) 'walked-lexical-variable-reference-form))
    (is (typep (nth 6 body) 'walked-lexical-variable-reference-form))))

(def test test/semantics/specials/compile-time-globals ()
  (with-captured-compile-environment (env var-name macro-name smacro-name)
      `(progn
         (defvar ,var-name)
         (defmacro ,macro-name () ',var-name)
         (#+ecl eval-when #+ecl (:compile-toplevel)
          #-ecl progn
           (define-symbol-macro ,smacro-name (,macro-name)))
         -here-)
    (is (typep (walk-form smacro-name :environment (make-walk-environment env))
               '(and special-variable-reference-form (not free-variable-reference-form))))))

(defvar *spec-global*)
(defvar *spec-global-2*)
(declaim (fixnum *spec-global* *spec-global-2*))

(def test test/semantics/types/1 ()
  (with-captured-lexical-environment
      (env (let ((*spec-global* 0)
                 (spec1 1)
                 (spec2 2)
                 (lex1 1)
                 (lex2 2))
             (declare (special spec1 spec2)
                      (single-float spec1)
                      (double-float lex1))
             (locally
                 (declare (type (integer -3 10) spec2)
                          (single-float lex2))
               -here-)))
    (bind ((walked (walk-form '(progn *spec-global* *spec-global-2*
                                spec1 spec2 lex1 lex2)
                              :environment (make-walk-environment env)))
           (body (body-of walked)))
      (is (typep (nth 0 body) 'special-variable-reference-form))
      (is (equal (declared-type-of (nth 0 body)) 'fixnum))
      (is (typep (nth 1 body) 'special-variable-reference-form))
      (is (equal (declared-type-of (nth 1 body)) 'fixnum))
      (is (typep (nth 2 body) 'special-variable-reference-form))
      (is (equal (declared-type-of (nth 2 body)) 'single-float))
      (is (typep (nth 3 body) 'special-variable-reference-form))
      ;; ECL has less precise type tracking
      (is (equal (declared-type-of (nth 3 body)) #-ecl '(integer -3 10) #+ecl t))
      (is (typep (nth 4 body) 'unwalked-lexical-variable-reference-form))
      (is (equal (declared-type-of (nth 4 body)) 'double-float))
      (is (typep (nth 5 body) 'unwalked-lexical-variable-reference-form))
      (is (equal (declared-type-of (nth 5 body)) #-ecl 'single-float #+ecl t)))))

(def test test/semantics/flet/1 ()
  (bind ((walked (walk-form '(flet ((foo ()
                                     1))
                              (flet ((foo ()
                                       2))
                                (funcall #'foo)))))
         (function-object-form (finishes (first (arguments-of (first (body-of (first (body-of walked)))))))))
    (is (typep function-object-form 'walked-lexical-function-object-form))
    (bind ((walked-lexical-definition (definition-of function-object-form)))
      (is (typep walked-lexical-definition 'lexical-function-form))
      (bind ((binding (find walked-lexical-definition (bindings-of (parent-of walked-lexical-definition)))))
        (is (not (null binding)))
        (is (eq (name-of binding) 'foo))
        ;; check if we looked up the innermost 'foo
        (is (eql 2 (value-of (first (body-of walked-lexical-definition)))))))
    walked))

(def test test/semantics/flet/2 ()
  (bind ((walked (walk-form '(flet ((outer ()
                                     42))
                              (flet ((inner ()
                                       (outer)))
                                (inner)))))
         ;; fetch the (inner) call
         (application-form/inner (finishes (first (body-of (first (body-of walked)))))))
    (is (typep application-form/inner 'walked-lexical-application-form))
    (bind ((walked-lexical-definition (definition-of application-form/inner)))
      (is (typep walked-lexical-definition 'lexical-function-form))
      ;; lookup the binding for #'inner
      (bind ((binding (find walked-lexical-definition (bindings-of (parent-of walked-lexical-definition)))))
        (is (not (null binding)))
        (is (eq (name-of binding) 'inner))
        ;; fetch the (outer) call inside #'inner
        (bind ((application-form/outer (first (body-of walked-lexical-definition))))
          (is (typep application-form/outer 'walked-lexical-application-form))
          (is (eql 'outer (operator-of application-form/outer)))
          (is (eql 42 (value-of (first (body-of (definition-of application-form/outer)))))))))
    walked))

(def test test/semantics/flet/bug/1 ()
  (bind ((walked (walk-form '(flet ((outer ()
                                     ;; it's key for this test that body is empty
                                     ))
                              (outer))))
         (application-form/inner (first (body-of walked))))
    (is (typep application-form/inner 'walked-lexical-application-form))
    walked))

(def test test/semantics/defun/1 ()
  (bind ((walked (walk-form '(defun foo ()
                              (foo)))))
    (is (eql (definition-of (first (body-of walked))) walked))
    walked))

(def test test/semantics/labels/1 ()
  (let* ((ast (walk-form '(labels ((foo () (foo)))
                           (flet ((bar () #'foo (bar)))
                             (foo)
                             (bar)))))
         (foo-code (first (bindings-of ast)))
         (lab-body (first (body-of ast)))
         (bar-code (first (bindings-of lab-body)))
         (flt-body (body-of lab-body)))
    (annotate-binding-usage ast)
    (is (eql (definition-of (first (body-of foo-code)))
             foo-code))
    (is (eql (definition-of (first (body-of bar-code)))
             foo-code))
    (is (set-equal (usages-of foo-code)
                   (list (first flt-body)
                         (first (body-of bar-code))
                         (first (body-of foo-code)))))
    (is (typep (second (body-of bar-code))
               'free-application-form))
    (is (eql (definition-of (first flt-body))
             foo-code))
    (is (eql (definition-of (second flt-body))
             bar-code))))

(def test test/semantics/lambda/args ()
  (let* ((ast (walk-form '(lambda (a &optional (b a) &key (c b))
                           a b c)))
         (args (bindings-of ast)))
    (is (every (lambda (x) (typep x 'name-definition-form))
               args))
    (is (every #'eql args
               (mapcar #'definition-of (body-of ast))))
    (is (eql (first args)
             (definition-of (default-value-of (second args)))))
    (is (eql (second args)
             (definition-of (default-value-of (third args)))))))

(def test test/semantics/lambda/bug/1 ()
  (bind ((walked (not-signals warning (walk-form '(lambda (a)
                                                    a))))
         (variable-reference-form (first (body-of walked))))
    (is (typep variable-reference-form 'walked-lexical-variable-reference-form))
    walked))

(def test test/semantics/lambda/bug/2 ()
  (bind ((walked (walk-form '(lambda (&optional a (b 42)) ; no default value for A
                              )))
         (optional-argument-form (second (bindings-of walked)))
         (default-value-form (default-value-of optional-argument-form)))
    (is (null (default-value-of (first (bindings-of walked)))))
    (is (typep optional-argument-form 'optional-function-argument-form))
    (is (typep default-value-form 'constant-form))
    (is (eq (value-of default-value-form) 42))
    walked))

(def test test/semantics/lambda/bug/3 ()
  (not-signals undefined-variable-reference
    (walk-form '(lambda (a &optional (b a) &key (c b) &aux (d c) e (f e))
                 (values a b c d e f)))))

(def test test/semantics/lambda/bug/4 ()
  (bind ((walked (walk-form '(lambda (&optional (o 42 o?) &key (k nil k?))
                              o?
                              k?))))
    (is (typep (first (body-of walked)) 'lexical-variable-reference-form))
    (is (typep (second (body-of walked)) 'lexical-variable-reference-form))
    walked))

(def test test/semantics/tagbody/1 ()
  (let* ((ast (walk-form '(tagbody
                           (tagbody
                              (go a) (go b) a)
                           b)))
         (body1 (body-of ast))
         (body2 (body-of (first body1)))
         (go1 (first body2))
         (go2 (second body2)))
    (annotate-binding-usage ast)
    (is (eql (tag-of go1) (third body2)))
    (is (equal (usages-of (third body2))
               (list go1)))
    (is (eql (tag-of go2) (second body1)))
    (is (equal (usages-of (second body1))
               (list go2)))
    (is (eql (enclosing-tagbody-of go1) (first body1)))
    (is (eql (enclosing-tagbody-of go2) ast))))

(def test test/semantics/setq/bug/1 ()
  (is (equal '(1 43)
             (eval (unwalk-form
                    (walk-form
                     '(let ((x 1)
                            (y 2))
                       (symbol-macrolet ((x y))
                         ;; setq must take symbol macros into account
                         (setq x 42)
                         (incf x))
                       (list x y))))))))
