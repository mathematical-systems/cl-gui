;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.quasi-quote)

(def special-variable *transformation-pipeline*)
(def special-variable *transformation*)
(def special-variable *transformation-environment*)

(def function toplevel-quasi-quote-macro-call? (form)
  (and (consp form)
       (eq (first form) 'toplevel-quasi-quote-macro)
       (typep (second form) 'quasi-quote)))

(def function maybe-slurp-in-toplevel-quasi-quote (value)
  ;; when a macro is expanded, then its arguments are toplevel qq exressions like: (macro-with-xml <body>).
  ;; for example the xml transformation needs to look ahead and try to slurp in <body> for collapsing
  ;; more constant parts.
  (if (and (toplevel-quasi-quote-macro-call? value)
           (compatible-transformation-pipelines? *transformation-pipeline* (transformation-pipeline-of (second value))))
      (progn
        (assert (length= 2 value))
        (second value))
      value))

(def function wrap-runtime-delayed-transformation-form (form)
  `(bind ((*transformation* ,*transformation*))
     ,form))

(def (class* e) transformation ()
  ((transformer :type (or symbol function))))

(def (definer e :available-flags "e") transformation (name supers slots transformer &body class-options)
  `(progn
     (def (class* ,@(when (getf -options- :export) '(:export t))) ,name ,(if (find 'transformation supers)
                              supers
                              (append supers '(transformation)))
       ,slots
       ,@class-options)
     ,(when transformer
        `(def method initialize-instance :after ((self ,name) &key)
              (bind ((-transformation- self))
                (declare (ignorable -transformation-))
                (setf (transformer-of self) ,transformer))))))

(def method make-load-form ((self transformation) &optional environment)
  (make-load-form-saving-slots self :environment environment))

(def generic compatible-transformations? (a a-next a-rest b b-next b-rest)
  (:method ((a null) (a-next null) (a-rest null) (b null) (b-next null) (b-rest null))
    #t)
  (:method (a a-next a-rest b b-next b-rest)
    #f)
  (:method :around (a a-next a-rest b b-next b-rest)
    (or (and (eq a b)
             (eq a-next b-next)
             (eq a-rest b-rest))
        (call-next-method))))

(def function compatible-with-current-transformation-pipeline? (pipeline)
  (or (not (boundp '*transformation-pipeline*))
      (compatible-transformation-pipelines? *transformation-pipeline* pipeline)))

(def function compatible-transformation-pipelines? (a b)
  (or (and (null a) (null b))
      (compatible-transformations? (first a) (second a) (rest (rest a))
                                   (first b) (second b) (rest (rest b)))))

(def class* lisp-form-emitting-transformation (transformation)
  ((with-inline-emitting #f :accessor with-inline-emitting? :documentation "WITH-INLINE-EMITTING means that the order of the creation of the syntax nodes at runtime is in sync with the expected order of these nodes in the output (i.e. nothing like <a () ,@(reverse (list <b> <c>))>). It enables an optimization: in this mode the write-sequence calls are not wrapped in closures but rather everything is emitted at the place where it is in the code.")
   ;; TODO there's some confusion here: stream-variable-name and declarations is meaningless for quasi-quoted-list-to-list-emitting-form...
   (stream-variable-name)
   (declarations '() :documentation "Add these declarations to the emitted lambda forms.")))

(def method compatible-transformations? ((a lisp-form-emitting-transformation) a-next a-rest
                                         (b lisp-form-emitting-transformation) b-next b-rest)
  (and (eql (with-inline-emitting? a) (with-inline-emitting? b))
       (eql (stream-variable-name-of a) (stream-variable-name-of b))
       (equalp (declarations-of a) (declarations-of b))
       (compatible-transformations? a-next (first a-rest) (rest a-rest)
                                    b-next (first b-rest) (rest b-rest))))

(def function ensure-progn (forms)
  (if (and (consp forms)
           (eq 'progn (first forms)))
      forms
      `(progn ,@forms)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def class* delayed-emitting ()
    ()
    (:metaclass funcallable-standard-class)
    (:documentation "A distinct type to be able to assert for it while emitting.")))

;; TODO: +void+ should be replaced by using (values) in user code (somewhat difficult)
(def (special-variable e) +void+ (bind ((result (make-instance 'delayed-emitting)))
                                   (set-funcallable-instance-function result (lambda () (values)))
                                   result))

(def (function i) make-delayed-emitting (thunk)
  (bind ((result (make-instance 'delayed-emitting)))
    (set-funcallable-instance-function result thunk)
    result))

(def macro as-delayed-emitting (&body body)
  `(make-delayed-emitting (lambda () ,@body)))

(def function wrap-emitting-forms (forms)
  (bind ((with-inline-emitting (with-inline-emitting? *transformation*))
         (forms (if (eq with-inline-emitting :as-is)
                    forms
                    (append forms
                            (if with-inline-emitting
                                '(+void+)
                                '((values)))))))
    (if with-inline-emitting
        (ensure-progn forms)
        `(as-delayed-emitting
           ,@(declarations-of *transformation*)
           ,@(if (and (consp forms)
                      (eq 'progn (first forms)))
                 (cdr forms)
                 forms)))))

(def function wrap-forms-with-bindings (bindings forms)
  (if bindings
      `(bind ,bindings
         ,forms)
      forms))

(def function trace-transformation-functions ()
  (trace run-transformation-pipeline transform transform*
         compatible-transformations? compatible-transformation-pipelines?
         transform-quasi-quoted-list-to-list-emitting-form)
  (values))

(def (function d) run-transformation-pipeline (node)
  (assert (typep node 'quasi-quote))
  (bind ((*transformation* nil)
         (*transformation-pipeline* nil))
    (iter (setf node (transform node))
          (while (typep node 'quasi-quote))))
  node)

(def function macroexpand-ignoring-toplevel-quasi-quote-macro (form &optional env)
  (if (toplevel-quasi-quote-macro-call? form)
      form
      (macroexpand form env)))

(def macro toplevel-quasi-quote-macro (node &environment env)
  (bind ((*transformation-environment* env))
    (run-transformation-pipeline node)))

(def generic transform* (parent-tr parent-next-tr parent-pipeline node tr next-tr pipeline)
  (:method (parent-tr parent-next-tr parent-pipeline node tr next-tr pipeline)
    (assert (typep node 'quasi-quote))
    (assert (eq *transformation* tr))
    (funcall (transformer-of *transformation*) node)))

(def function transform (node)
  (assert (typep node 'quasi-quote))
  (bind ((parent-position (position *transformation* *transformation-pipeline*))
         (parent-tr *transformation*)
         (parent-next-tr (when parent-position
                           (car (nthcdr (1+ parent-position) *transformation-pipeline*))))
         (parent-pipeline *transformation-pipeline*)
         (*transformation-pipeline* (transformation-pipeline-of node))
         (*transformation* (first *transformation-pipeline*)))
    (transform* parent-tr
                parent-next-tr
                parent-pipeline
                node
                *transformation*
                (second *transformation-pipeline*)
                *transformation-pipeline*)))

(def macro transformation-typecase (quasi-quote-node &body cases)
  (once-only (quasi-quote-node)
    `(etypecase ,quasi-quote-node
       ,@cases
       (quasi-quote (transform ,quasi-quote-node))
       ;; TODO is this still needed here?
       (delayed-emitting ,quasi-quote-node)
       (side-effect ,quasi-quote-node))))

(def generic make-syntax-node-emitting-form (node)
  (:method ((node null))
    nil)

  (:method ((node symbol))
    (if (keywordp node)
        node
        (list 'quote node)))

  (:method ((node number))
    node)

  (:method ((node string))
    node)

  (:method ((node function))
    node)

  (:method ((node hash-table))
    (with-unique-names (table)
      `(bind ((,table (make-hash-table :test ',(hash-table-test node))))
         ,@(iter (for (key value) :in-hashtable node)
                 (collect `(setf (gethash ,(make-syntax-node-emitting-form key) ,table)
                                 ,(make-syntax-node-emitting-form value))))
         ,table)))

  (:method ((node list))
    (iter (for element :in node)
          (collect (when (typep element 'unquote)
                     ;; TODO FIXME destructively-spliced?
                     (spliced? element)) :into spliced-elements)
          (collect (make-syntax-node-emitting-form element) :into transformed-elements)
          (finally (return
                     (cond ((every #'identity spliced-elements)
                            `(append ,@transformed-elements))
                           ((notany #'identity spliced-elements)
                            `(list ,@transformed-elements))
                           (t `(append ,@(mapcar (lambda (spliced element)
                                                   (if spliced
                                                       element
                                                       `(list ,element)))
                                                 spliced-elements transformed-elements))))))))

  (:method ((node quasi-quote))
    (make-syntax-node-emitting-form (body-of node)))

  (:method ((node unquote))
    (map-filtered-tree (form-of node) 'quasi-quote #'run-transformation-pipeline))

  (:method ((node syntax-node))
    (bind ((class (class-of node)))
      `(make-instance ',(class-name class)
                      ,@(iter (for slot :in (collect-slots-for-syntax-node-emitting-form node))
                              (when (slot-boundp-using-class class node slot)
                                (appending (list (first (slot-definition-initargs slot))
                                                 (make-syntax-node-emitting-form (slot-value-using-class class node slot))))))))))

(def generic collect-slots-for-syntax-node-emitting-form (node)
  (:method ((node syntax-node))
    (remove 'parent (class-slots (class-of node)) :key #'slot-definition-name)))

(def (transformation e) quasi-quoted-syntax-node-to-syntax-node-emitting-form (lisp-form-emitting-transformation)
  ()
  'make-syntax-node-emitting-form)

(def (transformation e) generic-transformation ()
  ((quasi-quote-transformer)
   (unquote-transformer)
   (output-transformer nil))
  'transform-with-generic-transformation)

(def function transform-with-generic-transformation (input)
  (bind ((quasi-quote-transformer (quasi-quote-transformer-of *transformation*))
         (unquote-transformer (unquote-transformer-of *transformation*)))
    (labels ((recurse (node)
               (etypecase node
                 (quasi-quote (funcall quasi-quote-transformer node))
                 (unquote (funcall unquote-transformer node))
                 (cons (cons (recurse (car node))
                             (recurse (cdr node))))
                 (null nil))))
      (bind ((result (recurse input)))
        (aif (and (slot-boundp *transformation* 'output-transformer)
                  (output-transformer-of *transformation*))
             (funcall it result)
             result)))))

;;;;;;
;;; Emit

(def (macro e) emit (ast)
  (once-only (ast)
    `(progn
       (assert (typep ,ast 'delayed-emitting) () "Something went awry around the quasi quoted stuff, EMIT got a ~S." ,ast)
       (funcall ,ast))))

