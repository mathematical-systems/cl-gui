;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.walker)

;;;; Atoms

(def (form-class e) constant-form ()
  ((value)))

(def unwalker constant-form (value)
  (if (or (eq value t)
          (eq value nil)
          (keywordp value))
      value
      (typecase value
        (symbol `(quote ,value))
        (cons   `(quote ,value))
        (t value))))

(def print-object constant-form
  (format t "!~S" (value-of -self-)))

(def (form-class e) variable-reference-form (named-walked-form)
  (declared-type))

(def unwalker variable-reference-form (name)
  name)

(def print-object variable-reference-form
  (format t "!~S" (name-of -self-)))

(def (form-class e) lexical-variable-reference-form (variable-reference-form)
  ())

(def (form-class e) walked-lexical-variable-reference-form (lexical-variable-reference-form)
  ((definition :ast-link :back))
  (:documentation "A reference to a local variable defined in the lexical environment inside the form passed to walk-form."))

(def (form-class e) unwalked-lexical-variable-reference-form (lexical-variable-reference-form)
  ()
  (:documentation "A reference to a local variable defined in the lexical environment outside of the form passed to walk-form."))

;; TODO should we add/handle walked-special-variable-reference-form?
(def (form-class e) special-variable-reference-form (variable-reference-form)
  ())

(def (form-class e) free-variable-reference-form (special-variable-reference-form)
  ())

(def walker (type t) ; atom
  (bind ((lexenv (walk-environment/lexical-environment -environment-)))
    (cond
      ((or (constant-name? -form-)
           (not (symbolp -form-)))
       (make-form-object 'constant-form -parent- :value -form-))
      (t
       (bind (((:values closest-lexenv-entry-type definition decl-type) (-lookup- :variable-like -form-)))
         (ecase closest-lexenv-entry-type
           (:variable
            (make-form-object 'walked-lexical-variable-reference-form -parent-
                              :name -form- :definition definition
                              ;; non-NIL if redeclared, e.g. (let (a) (locally (fixnum a) a))
                              :declared-type decl-type))
           (:unwalked-variable
            (if (eql (car definition) :special) ; Local special declaration?
                (make-form-object 'special-variable-reference-form -parent-
                                  :name -form- :declared-type (or decl-type (cdr definition)))
                (make-form-object 'unwalked-lexical-variable-reference-form -parent-
                                  :name -form- :declared-type (or decl-type (cdr definition)))))
           (:symbol-macro
            (bind ((*inside-macroexpansion* t))
              (recurse definition)))
           ((nil)
            ;; Try expanding a symbol macro
            (bind (((:values expansion expanded?) (walker-macroexpand-1 -form- lexenv)))
              (cond (expanded?
                     (recurse expansion))
                    ((special-variable-name? -form- lexenv)
                     (make-form-object 'special-variable-reference-form -parent- :name -form-
                                       :declared-type (or decl-type
                                                          (declared-variable-type -form- lexenv))))
                    (t
                     (handle-undefined-reference :variable -form-)
                     (make-form-object 'free-variable-reference-form -parent- :name -form-
                                       :declared-type (or decl-type
                                                          (declared-variable-type -form- lexenv)))))))))))))

;;;; BLOCK/RETURN-FROM

(def (form-class e) block-form (implicit-progn-mixin name-definition-form)
  ())

(def walker block
  (bind (((block-name &rest body) (cdr -form-)))
    (with-form-object (block 'block-form -parent-
                             :name block-name)
      (walk-implict-progn block
                          body
                          (-augment- :block block-name block)))))

(def unwalker block-form (name body)
  `(block ,name ,@(recurse-on-body body)))

(def (form-class e) return-from-form ()
  ((target-block nil :ast-link :back)
   (result :ast-link t)))

(def (condition* e) return-from-unknown-block (walker-error)
  ((block-name))
  (:report (lambda (condition stream)
             (format stream "Unable to find block named ~S used in a RETURN-FROM." (block-name-of condition)))))

(def walker return-from
  (bind (((block-name &optional (value '(values))) (rest -form-)))
    (if (-lookup- :block block-name)
        (with-form-object (return-from 'return-from-form -parent-
                                       :target-block (-lookup- :block block-name))
          (setf (result-of return-from) (recurse value return-from)))
        (restart-case
            (error 'return-from-unknown-block :block-name block-name)
          (add-block ()
            :report "Add this block and continue."
            (recurse -form- -parent- (-augment- :block block-name :unknown-block)))))))

(def unwalker return-from-form (target-block result)
  (bind ((unwalked-result (recurse result))
         (result-form (if (equal unwalked-result '(values))
                          '()
                          (list unwalked-result))))
    (if (null (name-of target-block))
        `(return ,@result-form)
        `(return-from ,(name-of target-block) ,@result-form))))

;;;; CATCH/THROW

(def (form-class e) catch-form (implicit-progn-mixin)
  ((tag)))

(def walker catch
  (bind (((tag &body body) (cdr -form-)))
    (with-form-object (catch 'catch-form -parent-)
      (setf (tag-of catch) (recurse tag catch))
      (walk-implict-progn catch body -environment-))))

(def unwalker catch-form (tag body)
  `(catch ,(recurse tag) ,@(recurse-on-body body)))

(def (form-class e) throw-form ()
  ((tag)
   (value :ast-link t)))

(def walker throw
  (bind (((tag &optional (result '(values))) (cdr -form-)))
    (with-form-object (throw 'throw-form -parent-)
      (setf (tag-of throw) (recurse tag throw)
            (value-of throw) (recurse result throw)))))

(def unwalker throw-form (tag value)
  `(throw ,(recurse tag) ,(recurse value)))

;;;; EVAL-WHEN

(def (form-class e) eval-when-form (implicit-progn-mixin)
  ((eval-when-times)))

(def walker eval-when
  (bind (((times &body body) (cdr -form-)))
    (with-form-object (eval-when 'eval-when-form -parent-)
      (setf (eval-when-times-of eval-when) times)
      (walk-implict-progn eval-when body -environment- :declarations-allowed nil))))

(def unwalker eval-when-form (body eval-when-times)
  `(eval-when ,eval-when-times
     ,@(recurse-on-body body)))

;;;; IF

(def (form-class e) if-form ()
  ((condition :ast-link t)
   (then :ast-link t)
   (else :ast-link t)))

(def walker if
  (with-form-object (if 'if-form -parent-)
    (setf (condition-of if) (recurse (second -form-) if)
          (then-of if) (recurse (third -form-) if)
          (else-of if) (recurse (fourth -form-) if))))

(def unwalker if-form (condition then else)
  `(if ,(recurse condition)
       ,(recurse then)
       ,@(awhen (recurse else)
           (list it))))

;;;; LET/LET*

(def (form-class e) lexical-variable-binder-form (binder-form-mixin
                                                  implicit-progn-with-declarations-mixin)
  ())

(def (form-class e) lexical-variable-binding-form (name-definition-form)
  ((initial-value :ast-link t)
   (special-binding nil :accessor special-binding? :type boolean)))

(def unwalker lexical-variable-binding-form (name initial-value)
  (if initial-value
      (list name (unwalk-form initial-value))
      name))

(def (form-class e) let-form (lexical-variable-binder-form)
  ())

(def walker let
  (with-form-object (let 'let-form -parent-)
    (setf (bindings-of let)
          (mapcar (lambda (binding)
                    (with-current-form binding
                      (bind (((name &optional initial-value) (ensure-list (coerce-to-form binding))))
                        (with-form-object (binding 'lexical-variable-binding-form let :name name)
                          (setf (initial-value-of binding) (recurse initial-value binding))))))
                  (coerce-to-form (second -form-))))
    (walk-implict-progn
     let (cddr -form-) -environment-
     :declarations-allowed t
     :declarations-callback (lambda (declarations &aux var-names)
                              ;; extend the walkenv before we walk the body with the lexical variables introduced by this LET form
                              (loop
                                :for binding :in (bindings-of let)
                                :for name = (name-of binding)
                                :for lexenv = (walk-environment/lexical-environment -environment-)
                                :do (push name var-names)
                                ;; NOTE: only the local declarations and global proclaminations
                                ;; affect the specialness of the new binding
                                :do (if (and (not (proclaimed-special-variable? name lexenv t))
                                             (not (find-form-by-name (coerce-to-form name) declarations
                                                                     :type 'special-variable-declaration-form)))
                                        (-augment- :variable (coerce-to-form name) binding)
                                        (setf (special-binding? binding) t)))
                              ;; we've extended the env, inform WALK-IMPLICT-PROGN about it
                              (values -environment- var-names)))))

(def function let/let*-form-unwalker (name bindings body declarations)
  `(,name ,(unwalk-forms bindings)
     ,@(unwalk-declarations declarations)
     ,@(unwalk-forms body)))

(def unwalker let-form (bindings body declarations)
  (let/let*-form-unwalker 'let bindings body declarations))

(def (form-class e) let*-form (lexical-variable-binder-form)
  ())

(def walker let*
  (with-form-object (let*-form 'let*-form -parent- :bindings '())
    (walk-implict-progn
     let*-form (cddr -form-) -environment-
     :declarations-allowed t
     :declarations-callback (lambda (declarations &aux var-names)
                              ;; extend the walkenv before we walk the body with the lexical variables introduced by this LET* form
                              (setf (bindings-of let*-form)
                                    (loop
                                      :for entry :in (second -form-)
                                      :for lexenv = (walk-environment/lexical-environment -environment-)
                                      :collect (bind (((name &optional initial-value) (ensure-list entry)))
                                                 (with-form-object (binding 'lexical-variable-binding-form let*-form :name name)
                                                   (setf (initial-value-of binding) (recurse initial-value binding))
                                                   (push name var-names)
                                                   ;; NOTE: only the local declarations and global proclaminations
                                                   ;; affect the specialness of the new binding
                                                   (if (and (not (proclaimed-special-variable? name lexenv t))
                                                            (not (find-form-by-name (coerce-to-form name) declarations
                                                                                    :type 'special-variable-declaration-form)))
                                                       (-augment- :variable (coerce-to-form name) binding)
                                                       (setf (special-binding? binding) t))))))
                              ;; we've extended the env, inform WALK-IMPLICT-PROGN about it
                              (values -environment- var-names)))))

(def unwalker let*-form (bindings body declarations)
  (let/let*-form-unwalker 'let* bindings body declarations))

;;;; LOCALLY

(def (form-class e) locally-form (implicit-progn-with-declarations-mixin)
  ())

(def walker locally
  (with-form-object (locally 'locally-form -parent-)
    (walk-implict-progn locally (cdr -form-) -environment- :declarations-allowed t)))

(def unwalker locally-form (body declarations)
  `(locally
       ,@(unwalk-declarations declarations)
     ,@(recurse-on-body body)))

;;;; MACROLET

(def (form-class e) macro-binding-form (name-definition-form)
  (expansion))

(def (form-class e) macrolet-form (binder-form-mixin
                                   implicit-progn-with-declarations-mixin)
  ())

(def walker macrolet
  ;; TODO is there any point in constructing a macrolet form if we macroexpand the body anyways?
  (with-form-object (macrolet 'macrolet-form -parent-
                              :bindings '())
    (dolist* ((&whole defn name lambda-list &body body) (second -form-))
      (bind ((handler (parse-macro-definition name lambda-list body (walk-environment/lexical-environment -environment-))))
        (-augment- :macro name handler)
        ;; there's not much point in keeping the bindings when we expand the macrolet body anyway, so don't.
        ;; it would just hinder the saving of the form into fasl's for no apparent benefit.
        (push (with-current-form defn
                (make-form-object 'macro-binding-form macrolet :name name))
              (bindings-of macrolet))))
    (setf (bindings-of macrolet) (nreverse (bindings-of macrolet)))
    (walk-implict-progn macrolet (cddr -form-) -environment- :declarations-allowed t)))

(def unwalker macrolet-form (body declarations)
  ;; We ignore the bindings, because the expansion has already taken place at walk-time.
  ;; TODO either walk into a locally, or unwalk the bindings here
  `(locally ,@(unwalk-declarations declarations) ,@(recurse-on-body body)))

;;;; MULTIPLE-VALUE-CALL

(def (form-class e) multiple-value-call-form ()
  ((function-designator :ast-link t)
   (arguments :ast-link t)))

(def walker multiple-value-call
  (with-form-object (m-v-c 'multiple-value-call-form -parent-)
    (setf (function-designator-of m-v-c) (recurse (second -form-) m-v-c)
          (arguments-of m-v-c) (mapcar (lambda (f)
                                         (recurse f m-v-c))
                                       (cddr -form-)))))

(def unwalker multiple-value-call-form (function-designator arguments)
  `(multiple-value-call ,(recurse function-designator) ,@(recurse-on-body arguments)))

;;;; MULTIPLE-VALUE-PROG1

(def (form-class e) multiple-value-prog1-form ()
  ((first-form :ast-link t)
   (other-forms :ast-link t)))

(def walker multiple-value-prog1
  (with-form-object (m-v-p1 'multiple-value-prog1-form -parent-)
    (setf (first-form-of m-v-p1) (recurse (second -form-) m-v-p1)
          (other-forms-of m-v-p1) (mapcar (lambda (f)
                                            (recurse f m-v-p1))
                                       (cddr -form-)))))

(def unwalker multiple-value-prog1-form (first-form other-forms)
  `(multiple-value-prog1 ,(recurse first-form) ,@(recurse-on-body other-forms)))

;;;; PROGN

(def (form-class e) progn-form (implicit-progn-mixin)
  ())

(def walker progn
  (with-form-object (progn 'progn-form -parent-)
    (walk-implict-progn progn (cdr -form-) -environment-)))

(def unwalker progn-form (body)
  `(progn ,@(recurse-on-body body)))

(def print-object progn-form
  (bind ((body (body-of -self-)))
    (pprint-logical-block (*standard-output* body :prefix "(" :suffix ")")
      (princ "progn")
      (pprint-indent :block 1)
      (pprint-newline :mandatory)
      (pprint-exit-if-list-exhausted)
      (loop
         :with first? = t
         :for el = (pprint-pop)
         :do
         (unless first?
           (pprint-newline :mandatory))
         (princ el)
         (pprint-exit-if-list-exhausted)
         (setf first? nil)))))

;;;; PROGV

(def (form-class e) progv-form (implicit-progn-mixin)
  ((variables-form :ast-link t)
   (values-form :ast-link t)))

(def walker progv
  (with-form-object (progv 'progv-form -parent-)
    (setf (variables-form-of progv) (recurse (cadr -form-) progv))
    (setf (values-form-of progv) (recurse (caddr -form-) progv))
    (walk-implict-progn progv (cdddr -form-) -environment-)
    progv))

(def unwalker progv-form (body variables-form values-form)
  `(progv
       ,(recurse variables-form)
       ,(recurse values-form)
     ,@(recurse-on-body body)))

;;;; QUOTE

(def walker quote
  (make-form-object 'constant-form -parent-
                    :value (second -form-)))

;;;; SETQ

(def (form-class e) setq-form ()
  ((variable :ast-link t)
   (value :ast-link t)))

(def walker setq
  ;; the SETQ handler needs to be able to deal with symbol-macrolets
  ;; which haven't yet been expanded and may expand into something
  ;; requiring SETF and not SETQ.
  (let ((effective-code '()))
    (loop
       :for (name value) :on (cdr -form-) :by #'cddr
       :do (bind (((:values type expansion) (-lookup- :variable-like name)))
             (push (if (eq type :symbol-macro)
                       `(setf ,expansion ,value)
                       `(setq ,name ,value))
                   effective-code)))
    (if (= 1 (length effective-code))
        ;; only one form, the "simple case"
        (bind (((type variable value) (first effective-code)))
          (ecase type
            (setq (with-form-object (setq 'setq-form -parent-)
                    (setf (variable-of setq) (recurse variable setq))
                    (setf (value-of setq) (recurse value setq))))
            (setf (recurse (first effective-code)))))
        ;; multiple forms
        (with-form-object (progn 'progn-form -parent-)
          (walk-implict-progn progn (nreverse effective-code) -environment-)))))

(def unwalker setq-form (variable value)
  `(setq ,(recurse variable) ,(recurse value)))

;;;; SYMBOL-MACROLET

(def (form-class e) symbol-macrolet-form (binder-form-mixin
                                          implicit-progn-with-declarations-mixin)
  ())

(def walker symbol-macrolet
  (with-form-object (symbol-macrolet 'symbol-macrolet-form -parent-
                                     :bindings '())
    (dolist* ((&whole defn symbol expansion) (second -form-))
      (-augment- :symbol-macro symbol expansion)
      (push (with-current-form defn
              (make-form-object 'macro-binding-form symbol-macrolet
                                :name symbol :expansion expansion))
            (bindings-of symbol-macrolet)))
    (nreversef (bindings-of symbol-macrolet))
    (walk-implict-progn symbol-macrolet (cddr -form-) -environment- :declarations-allowed t)))

(def unwalker symbol-macrolet-form (body declarations)
  ;; We ignore the bindings, because the expansion has already taken place at walk-time.
  ;; TODO either walk into a locally, or unwalk the bindings here
  `(locally ,@(unwalk-declarations declarations) ,@(recurse-on-body body)))

;;;; TAGBODY/GO

(def (form-class e) tagbody-form (implicit-progn-mixin)
  ())

(def walker tagbody
  (with-form-object (tagbody 'tagbody-form -parent-
                             :body (cdr -form-))
    (flet ((go-tag? (form)
             (or (symbolp form)
                 (integerp form))))
      ;; the loop below destructuivly modifies the body of tagbody. since it's the same object as the source, we need to copy it.
      (setf (body-of tagbody) (copy-list (body-of tagbody)))
      (loop
         :for part :on (body-of tagbody)
         :if (go-tag? (car part))
         :do (let ((tag (with-current-form (car part)
                          (make-form-object 'go-tag-form tagbody
                                            :name (car part)
                                            :jump-target (cdr part)))))
               (-augment- :tag (car part) tag)
               (setf (car part) tag)))
      (loop
         :for part :on (body-of tagbody)
         :unless (typep (car part) 'go-tag-form)
           :do (setf (car part) (recurse (car part) tagbody))))))

(def unwalker tagbody-form (body)
  `(tagbody ,@(recurse-on-body body)))

(def (form-class e) go-tag-form (name-definition-form)
  ((jump-target :copy-with nil)))

(def unwalker go-tag-form (name)
  name)

(def (form-class e) go-form (named-walked-form)
  ((tag :ast-link :back)))

(def method jump-target-of ((form go-form))
  (jump-target-of (tag-of form)))

(def (function e) enclosing-tagbody-of (form)
  (check-type form go-form)
  (parent-of (tag-of form)))

(def walker go
  (make-form-object 'go-form -parent-
                    :name (second -form-)
                    :tag (-lookup- :tag (second -form-))))

(def unwalker go-form (name)
  `(go ,name))

;;;; THE

(def (form-class e) the-form ()
  ((declared-type)
   (value :ast-link t)))

(def walker the
  (with-form-object (the 'the-form -parent- :declared-type (second -form-))
    (setf (value-of the) (recurse (third -form-) the))))

(def unwalker the-form (declared-type value)
  `(the ,declared-type ,(recurse value)))

;;;; UNWIND-PROTECT

(def (form-class e) unwind-protect-form ()
  ((protected-form :ast-link t)
   (cleanup-form :ast-link t)))

(def walker unwind-protect
  (with-form-object (unwind-protect 'unwind-protect-form -parent-)
    (setf (protected-form-of unwind-protect) (recurse (second -form-) unwind-protect)
          (cleanup-form-of unwind-protect) (mapcar (lambda (form)
                                                     (recurse form unwind-protect))
                                                   (cddr -form-)))))

(def unwalker unwind-protect-form (protected-form cleanup-form)
  `(unwind-protect ,(recurse protected-form) ,@(recurse-on-body cleanup-form)))

;;;; LOAD-TIME-VALUE

(def (form-class e) load-time-value-form ()
  ((body :ast-link t)
   (read-only nil :accessor read-only? :type boolean)
   (value)))

(def method initialize-instance :after ((self load-time-value-form) &key &allow-other-keys)
  (setf (value-of self) (eval (body-of self))))

(def walker load-time-value
  (assert (<= (length -form-) 3))
  (with-form-object (load-time-value 'load-time-value-form -parent-
                                     :body -form-
                                     :read-only (third -form-))
    (setf (body-of load-time-value) (recurse (second -form-) load-time-value
                                             ;; intentionally walk the body in an empty environment
                                             nil))))

(def unwalker load-time-value-form (body read-only)
  `(load-time-value ,(recurse body) ,@(if read-only '(t))))
