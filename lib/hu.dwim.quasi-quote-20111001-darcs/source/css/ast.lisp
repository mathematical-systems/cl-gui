;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.quasi-quote.css)

;; A quasi quoted CSS AST is made of list, css-syntax-nodes, css-quasi-quote, css-unquote recursively

(def ast css)

(def class* css-syntax-node (syntax-node)
  ())

(def (class* e) css-quasi-quote (quasi-quote css-syntax-node)
  ())

(def method print-object ((self css-quasi-quote) *standard-output*)
  (print-object/quasi-quote self "css"))

(def (function e) make-css-quasi-quote (transformation-pipeline body)
  (assert (not (typep body 'quasi-quote)))
  (make-instance 'css-quasi-quote
                 :transformation-pipeline transformation-pipeline
                 :body body))

(def (class* e) css-unquote (unquote css-syntax-node)
  ())

(def (function e) make-css-unquote (form &optional modifier)
  (make-instance 'css-unquote :form form :modifier modifier))

(def (class* e) css-clause (css-syntax-node)
  ((selectors nil :documentation "An optional list of css-selector nodes.")
   (attributes nil :documentation "A list of css-attribute nodes.")))

(def (function e) make-css-clause (selectors attributes)
  (make-instance 'css-clause :selectors selectors :attributes attributes))

(def (class* e) css-selector (css-syntax-node)
  ())

(def (class* e) css-element-selector (css-selector)
  ((name nil :documentation "The name of the type of the element node")
   (attributes nil :documentation "The list of attribute selector nodes of the element node")))

(def (function e) make-css-element-selector (name attributes)
  (make-instance 'css-element-selector :name name :attributes attributes ))
   
(def (class* e) css-attribute-selector (css-syntax-node)
  ((name nil :documentation "The name node of the attribute selector node")
   (value nil :documentation "The value node of the attribute selector node")
   (operator nil :documentation "The operator (= ~= |= ?) for the attribute selector node")))

(def (function e) make-css-attribute-selector (name value &optional operator)
  (unless operator
    (if (or (equal name "class") (equal name "pseudo-class"))
        (setf operator '~=)
        (setf operator '=)))
  (make-instance 'css-attribute-selector :name name :value value :operator operator))

(def (class* e) css-relation-selector (css-selector)
  ((elements nil :documentation "The list of element nodes of the relation selector node")
   (operator nil :documentation "The type (> + -) of the relation selector node")))

(def (function e) make-css-relation-selector (operator elements)
  (make-instance 'css-relation-selector :elements elements :operator operator))

(def (class* e) css-attribute (css-syntax-node)
  ((name nil :documentation "The name node of the attribute node")
   (value nil :documentation "The value node of the attribute node")))

(def (function e) make-css-attribute (name value)
  (make-instance 'css-attribute :name name :value value))

(def (class* e) css-annotated-value (css-syntax-node)
  ((value nil :documentation "The value node of the value node")
   (annotation nil :documentation "The annotation (:rgb :color :url) of the value")))

(def (function e) make-css-annotated-value (name value)
  ;; TODO : check value for specific annotated value
  (when (symbolp name)
    (setf name (symbol-name name)))
  (make-instance 'css-annotated-value :value value :annotation name))

(def methods map-ast
    (:method (fn (x css-clause))
      (map-ast/map-accessors-unless-same-returned fn x
        selectors-of
        attributes-of))
  (:method (fn (x css-element-selector))
    (map-ast/map-accessors-unless-same-returned fn x
      name-of
      attributes-of))
  (:method (fn (x css-attribute-selector))
    (map-ast/map-accessors-unless-same-returned fn x
      name-of
      value-of
      operator-of))
  (:method (fn (x css-relation-selector))
    (map-ast/map-accessors-unless-same-returned fn x
      elements-of
      operator-of))
  (:method (fn (x css-attribute))
    (map-ast/map-accessors-unless-same-returned fn x
      name-of
      value-of))
  (:method (fn (x css-annotated-value))
    (map-ast/map-accessors-unless-same-returned fn x
      value-of
      annotation-of)))

(def methods bq-process
    (:method ((x css-quasi-quote))
      `(list 'toplevel-quasi-quote-macro
             (make-css-quasi-quote (quote ,(transformation-pipeline-of x))
                                   ,(map-ast #'bq-process (body-of x)))))

  (:method ((x css-unquote))
    (bind ((form (form-of x)))
      (if (spliced? x)
          ;; TODO this is questionable... unconditionally wrap it inside a list? what's this? think it through... `<foo () ,@,@body>
          `(make-css-unquote (list* 'list ,(bq-bracket form)) ,(modifier-of x))
          `(make-css-unquote ,(bq-process form) ,(modifier-of x)))))

  (:method ((x css-clause))
    `(make-css-clause
      ,(bq-process (selectors-of x))
      ,(bq-process (attributes-of x))))

  (:method ((x css-element-selector))
    `(make-css-element-selector
      ,(map-ast #'bq-process (name-of x))
      ,(bq-process (attributes-of x))))

  (:method ((x css-attribute-selector))
    `(make-css-attribute-selector
      ,(map-ast #'bq-process (name-of x))
      ,(bq-process (value-of x))
      ,(map-ast #'bq-process (operator-of x))))
  
  (:method ((x css-relation-selector))
    `(make-css-relation-selector
      ,(bq-process (elements-of x))
      ,(map-ast #'bq-process (operator-of x))))

  (:method ((x css-attribute))
    `(make-css-attribute
      ,(map-ast #'bq-process (name-of x))
      ,(bq-process (value-of x))))

  (:method ((x css-annotated-value))
    `(make-css-annotated-value
      ,(map-ast #'bq-process (name-of x))
      ,(bq-process (value-of x)))))



;`css( ((tag table :class (title-border))
;       (> (id root-content) (tag th))
;       (any :class box-border))
;      :background-image (url "/images/tr.gif"))
      
