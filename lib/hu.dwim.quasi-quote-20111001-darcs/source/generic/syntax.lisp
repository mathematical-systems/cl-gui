;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.quasi-quote)

(def special-variable *print-quasi-quote-transformation-pipelines* #f)

(def function print-object/quasi-quote (quasi-quote name &optional (stream *standard-output*))
  (bind ((*standard-output* stream)
         (body (body-of quasi-quote)))
    (princ "`")
    (princ name)
    (if *print-quasi-quote-transformation-pipelines*
        (progn
          (princ " ")
          (princ (transformation-pipeline-of quasi-quote))
          (pprint-newline :mandatory)
          (pprint-logical-block (nil body)
            (pprint-indent :block 2)
            (loop :do
               (pprint-exit-if-list-exhausted)
               (prin1 (pprint-pop)))))
        (prin1 body)))
  quasi-quote)


;;;;;;
;;; AST

(def (class* e) syntax-node ()
  ())

(def function non-syntax-node-atom? (x)
  (and (atom x)
       (not (typep x 'syntax-node))))

;; TODO ? maybe it's just a thinko that it's needed
(def layered-method hu.dwim.walker:unwalk-form ((self syntax-node))
  self)

(def (class* e) quasi-quote (syntax-node)
  ((transformation-pipeline)
   (body)))

(def special-variable *print-quasi-quote-stack* nil)

#+nil ;; makes trace noisy for compatible-transformation-pipelines?, and it's also wrong currently
(def method print-object :around ((self quasi-quote) *standard-output*)
  (bind ((*print-case* :downcase))
    (when (and *print-quasi-quote-stack*
               (not (compatible-transformation-pipelines?
                     (transformation-pipeline-of self)
                     (transformation-pipeline-of (first *print-quasi-quote-stack*)))))
      (write-string "!"))
    (bind ((*print-quasi-quote-stack* (cons self *print-quasi-quote-stack*)))
      (call-next-method))))

(deftype unquote-modifier ()
  '(member nil :splice :destructive-splice))

(def (class* e) unquote (syntax-node)
  ((form)
   (modifier nil :type unquote-modifier)))

(def function spliced? (unquote)
  (check-type (modifier-of unquote) unquote-modifier)
  (not (null (modifier-of unquote))))

(def function destructively-spliced? (unquote)
  (check-type (modifier-of unquote) unquote-modifier)
  (eq (modifier-of unquote) :destructive-splice))

(def method print-object ((self unquote) *standard-output*)
  (write-string "_")
  (cond
    ((destructively-spliced? self) (write-string "."))
    ((spliced? self)               (write-string "@")))
  (prin1 (form-of self))
  self)

(def function unquote-node-with-constant-value? (node type)
  ;; TODO this could handle '(progn (progn 42))
  (and (typep node 'unquote)
       (constantp (form-of node))
       (typep node type)))

(def function constant-value-of-unquote-node (node)
  (bind ((form (form-of node)))
    (assert (constantp form))
    (etypecase form
      (symbol (symbol-value form))
      (string form)
      (integer (integer-to-string form))
      (float (format nil "~F" form))
      (ratio (format nil "~D" form)))))

;; TODO: eliminate side effect and check for returning (values) from unqutes
;; TODO: what if the unquote returns with another function
(def (class* e) side-effect (syntax-node)
  ((form)))

(def (function e) make-side-effect (form)
  (make-instance 'side-effect :form form))

;; TODO: revise this, how to make it safe
(def (class* e) parent-mixin ()
  ((parent :type syntax-node)))

(def constructor parent-mixin
  (iter (with class = (class-of -self-))
        (for slot :in (class-slots class))
        (when (slot-boundp-using-class class -self- slot)
          (bind ((value (slot-value-using-class class -self- slot)))
            ;; TODO: this is really fragile
            (typecase value
              (parent-mixin (setf (parent-of value) -self-))
              (list
               (when (eq 'list (slot-definition-type slot))
                 (dolist (element value)
                   (setf (parent-of element) -self-))))
              (hash-table
               (iter (for (key value) :in-hashtable value)
                     (when (typep value 'parent-mixin)
                       (setf (parent-of value) -self-))
                     (when (typep key 'parent-mixin)
                       (setf (parent-of key) -self-)))))))))

(def function find-ancestor-syntax-node (node type)
  (iter (for current :initially node :then (parent-of current))
        (until (typep current type))
        (finally (return current))))

(def function ast-package (name)
  (bind ((package (symbol-package name)))
    (if (eq package (find-package :common-lisp))
        (find-package :hu.dwim.quasi-quote)
        package)))

(def (definer e) ast (name)
  (bind ((package (ast-package name)))
    (flet ((process (names)
             (apply 'format-symbol package
                    (iter (repeat (length names))
                          (collect #\~ :result-type string)
                          (collect #\A :result-type string))
                    names)))
      `(export
        ',(mapcar #'process
                  `((,name)
                    ("QUASI-QUOTED-" ,name)
                    (,name "-EMITTING-FORM")))
        ,package))))

(export '(lambda-form lambda))

(def method make-load-form ((instance syntax-node) &optional environment)
  (make-load-form-saving-slots instance :environment environment))

(def special-variable *ast-print-object-nesting-level* 0)

(def constant +ast-print-depth+ 2)

(def print-object syntax-node
  (bind ((class (class-of -self-)))
    (pprint-logical-block (nil nil :prefix "#<" :suffix ">")
      (princ (class-name class))
      (princ " ")
      (iter (for slot :in (class-slots class))
            (when (slot-boundp-using-class class -self- slot)
              (for value = (slot-value-using-class class -self- slot))
              (unless (first-iteration-p)
                (princ " "))
              (princ (first (slot-definition-initargs slot)))
              (princ " ")
              (princ value))))))

(def macro map-ast/map-accessors-unless-same-returned (fn x &body accessors)
  (once-only (fn x)
    `(bind ((new (funcall ,fn ,x)))
       (if (eq new ,x)
           (progn
             ,@(iter (for accessor :in accessors)
                     (collect `(setf (,accessor ,x) (funcall ,fn (,accessor ,x)))))
             ,x)
           new))))

(def generic map-ast (fn x)
  (:method :around (visitor form)
    (if (consp form)
        (call-next-method)
        (bind ((new (funcall visitor form)))
          (if (eq new form)
              (call-next-method)
              new)
          new)))
  (:method (visitor (form t))
    ;; a primary method with a huge NOP
    )
  (:method (fn (x syntax-node))
    (error "MAP-AST is not properly overridden for syntax node ~A" x))
  (:method (fn (x cons))
    (bind ((car (funcall fn (car x)))
           (cdr (when (cdr x)
                  (map-ast fn (cdr x)))))
      (if (and (eql car (car x))
               (eql cdr (cdr x)))
          x
          (cons car cdr))))
  (:method (fn (x unquote))
    (bind ((new (funcall fn x)))
      (if (eq x new)
          (progn
            (setf (form-of x) (funcall fn (form-of x)))
            x)
          new))))
