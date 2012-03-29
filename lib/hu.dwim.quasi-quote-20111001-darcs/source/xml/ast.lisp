;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.quasi-quote.xml)

;; A quasi quoted XML AST is made of list, xml-syntax-nodes, xml-quasi-quote, xml-unquote recursively

(def ast xml)

(def class* xml-syntax-node (syntax-node)
  ())

(def (class* e) xml-quasi-quote (quasi-quote xml-syntax-node)
  ())

(def method print-object ((self xml-quasi-quote) *standard-output*)
  (print-object/quasi-quote self "xml"))

(def (function e) make-xml-quasi-quote (transformation-pipeline body)
  (assert (not (typep body 'quasi-quote)))
  (make-instance 'xml-quasi-quote
                 :transformation-pipeline transformation-pipeline
                 :body body))

(def (class* e) xml-unquote (unquote xml-syntax-node)
  ())

(def (function e) make-xml-unquote (form &optional modifier)
  (make-instance 'xml-unquote :form form :modifier modifier))

(def (class* e) xml-element (xml-syntax-node)
  ((name)
   (attributes nil :documentation "A list of xml-attribute nodes.")
   (children nil)))

(def method print-object ((self xml-element) *standard-output*)
  (pprint-logical-block (nil nil :prefix "<" :suffix ">")
    (princ (name-of self))
    (when (attributes-of self)
      (write-char #\Space)
      (bind ((attributes (iter (for attribute :in (attributes-of self))
                               (collect (if (typep attribute 'xml-attribute)
                                            (list (name-of attribute) (value-of attribute))
                                            attribute))))
             (width (iter (for attribute :in attributes)
                          (when (and (consp attribute)
                                     (stringp (first attribute)))
                            (maximize (length (first attribute))))))
             (format-control (concatenate 'string "~" (if width (princ-to-string width) "5") "A ~A")))
        (pprint-logical-block (nil attributes :prefix "(" :suffix ")")
          (pprint-exit-if-list-exhausted)
          (iter (for attribute = (pprint-pop))
                (unless (first-time-p)
                  (pprint-newline :mandatory))
                (if (consp attribute)
                    (apply #'format t format-control attribute)
                    (princ attribute))
                (pprint-exit-if-list-exhausted)))))
    (pprint-indent :block 2)
    (when (children-of self)
      (pprint-newline :mandatory)
      (pprint-logical-block (nil (children-of self))
        (pprint-exit-if-list-exhausted)
        (iter
          (unless (first-time-p)
            (pprint-newline :mandatory))
          (write (pprint-pop))
          (pprint-exit-if-list-exhausted))))))

(def (class* e) xml-attribute (xml-syntax-node)
  ((name)
   (value)))

(def (class* e) xml-text (xml-syntax-node)
  ((content)))

(def (function e) make-xml-element (name &optional attributes children)
  (make-instance 'xml-element :name name :attributes attributes :children children))

(def (function e) make-xml-attribute (name value)
  (make-instance 'xml-attribute :name name :value value))

(def (function eio) maybe-make-xml-attribute (name value)
  (when value
    (make-xml-attribute name value)))

(def (function e) make-xml-text (content)
  (make-instance 'xml-text :content content))

(def methods map-ast
  (:method (fn (x xml-quasi-quote))
    (map-ast/map-accessors-unless-same-returned fn x
      body-of))
  (:method (fn (x xml-element))
    (map-ast/map-accessors-unless-same-returned fn x
      name-of
      attributes-of
      children-of))
  (:method (fn (x xml-attribute))
    (map-ast/map-accessors-unless-same-returned fn x
      name-of
      value-of)))

(def methods bq-process
  (:method ((x xml-quasi-quote))
    `(list 'toplevel-quasi-quote-macro
           (make-xml-quasi-quote (quote ,(transformation-pipeline-of x))
                                 ,(map-ast #'bq-process (body-of x)))))

  (:method ((x xml-unquote))
    (bind ((form (form-of x)))
      (if (spliced? x)
          ;; TODO this is questionable... unconditionally wrap it inside a list? what's this? think it through... `<foo () ,@,@body>
          `(make-xml-unquote (list* 'list ,(bq-bracket form)) ,(modifier-of x))
          `(make-xml-unquote ,(bq-process form) ,(modifier-of x)))))

  (:method ((x xml-element))
    `(make-xml-element ,(bq-process (name-of x))
         ,(bq-process (attributes-of x))
       ,(bq-process (children-of x))))

  (:method ((x xml-text))
    ;; TODO it's not completely clear to me why it's needed. it got triggered in HU.DWIM.WUI::EMIT-ERROR-RESPONSE-FOR-AJAX-AWARE-CLIENT
    ;; TODO record a test that triggers it and include the reason here in a comment
    `(make-xml-text ,(content-of x)))

  (:method ((x xml-attribute))
    `(make-xml-attribute
      ,(map-ast #'bq-process (name-of x))
      ,(map-ast #'bq-process (value-of x)))))
