;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.quasi-quote)

(def method compatible-transformations? ((a hu.dwim.quasi-quote.xml::quasi-quoted-xml-to-quasi-quoted-string) a-next a-rest
                                         (b hu.dwim.quasi-quote.js::quasi-quoted-js-to-quasi-quoted-string) b-next b-rest)
  (compatible-transformations? a-next (first a-rest) (rest a-rest)
                               b-next (first b-rest) (rest b-rest)))

(in-package :hu.dwim.quasi-quote.js)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(hu.dwim.quasi-quote.xml::quasi-quoted-xml-to-quasi-quoted-string
            hu.dwim.quasi-quote.xml::quasi-quoted-string-to-xml-escaped-quasi-quoted-string
            hu.dwim.quasi-quote.xml::make-quasi-quoted-string-to-xml-escaped-quasi-quoted-string-transformation)))

#+nil
(
 ;; TODO what about these?
 (def method compatible-transformations? ((a quasi-quoted-xml-to-quasi-quoted-string) a-next a-rest
                                          (b quasi-quoted-js-to-quasi-quoted-string) (b-next quasi-quoted-string-to-xml-escaped-quasi-quoted-string) b-rest)
   (compatible-transformations? a-next (first a-rest) (rest a-rest)
                                (first b-rest) (second b-rest) (rest (rest b-rest))))

 (def method compatible-transformations? ((a quasi-quoted-binary-to-binary-emitting-form) a-next a-rest
                                          (b quasi-quoted-string-to-xml-escaped-quasi-quoted-string) (b-next quasi-quoted-binary-to-binary-emitting-form) b-rest)
   (compatible-transformations? a a-next a-rest
                                b-next (first b-rest) (rest b-rest))))

;; TODO rename to make-transformation-pipeline/quasi-quoted-js-to-string-emitting-form
(def (function e) make-quasi-quoted-js-to-form-emitting-transformation-pipeline
    (stream-variable-name &key binary with-inline-emitting indentation-width (encoding :utf-8)
                          output-prefix output-postfix declarations escape-as-xml)
  (bind ((xml-escape-transformation (when escape-as-xml
                                      (make-quasi-quoted-string-to-xml-escaped-quasi-quoted-string-transformation
                                       :output-prefix output-prefix
                                       :output-postfix output-postfix))))
    (if binary
        (list-without-nils
         (apply #'make-instance
                'quasi-quoted-js-to-quasi-quoted-string
                :indentation-width indentation-width
                (unless escape-as-xml
                  (list :output-prefix output-prefix
                        :output-postfix output-postfix)))
         xml-escape-transformation
         (make-instance 'quasi-quoted-string-to-quasi-quoted-binary
                        :encoding encoding)
         (make-instance 'quasi-quoted-binary-to-binary-emitting-form
                        :stream-variable-name stream-variable-name
                        :with-inline-emitting with-inline-emitting
                        :declarations declarations))
        (list-without-nils
         (make-instance 'quasi-quoted-js-to-quasi-quoted-string
                        :output-prefix output-prefix
                        :output-postfix output-postfix
                        :indentation-width indentation-width)
         xml-escape-transformation
         (make-instance 'quasi-quoted-string-to-string-emitting-form
                        :stream-variable-name stream-variable-name
                        :with-inline-emitting with-inline-emitting
                        :declarations declarations)))))

(def method transform* ((parent-tr quasi-quoted-xml-to-quasi-quoted-string)
                        (parent-next-tr quasi-quoted-string-to-string-emitting-form)
                        parent-pipeline
                        (node js-quasi-quote)
                        (tr quasi-quoted-js-to-quasi-quoted-string)
                        (next-tr quasi-quoted-string-to-xml-escaped-quasi-quoted-string)
                        pipeline)
  (bind ((unescaped-string-quasi-quote (call-next-method)))
    (assert (typep unescaped-string-quasi-quote 'string-quasi-quote))
    (transform unescaped-string-quasi-quote)))

(def method transform* ((parent-tr quasi-quoted-string-to-quasi-quoted-binary)
                        parent-next-tr
                        parent-pipeline
                        (node string-quasi-quote)
                        (tr quasi-quoted-string-to-xml-escaped-quasi-quoted-string)
                        (next-tr quasi-quoted-string-to-quasi-quoted-binary)
                        pipeline)
  (bind ((unescaped-string-quasi-quote (call-next-method)))
    (assert (typep unescaped-string-quasi-quote 'string-quasi-quote))
    (transform unescaped-string-quasi-quote)))
