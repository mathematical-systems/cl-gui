;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(def package :hu.dwim.quasi-quote
  (:use :babel
        :babel-streams
        :hu.dwim.asdf
        :hu.dwim.common
        :hu.dwim.def
        :hu.dwim.defclass-star
        :hu.dwim.syntax-sugar
        :hu.dwim.util)
  (:readtable-setup
   (hu.dwim.util:enable-standard-hu.dwim-syntaxes)
   (hu.dwim.syntax-sugar:enable-lambda-with-bang-args-syntax)))

(in-package :hu.dwim.quasi-quote)

(def (function e) import-external-quasi-quote-symbols-for-extensions (&optional (package *package*))
  "Import those symbols in PACKAGE that are public to extensions of hu.dwim.quasi-quote but not to its users."
  (import-duplicate-symbols)
  (import
   '(form-of
     form
     modifier-of
     parent-of
     spliced?
     destructively-spliced?
     body-of
     syntax-node
     unquote-node-with-constant-value?
     constant-value-of-unquote-node
     macroexpand-ignoring-toplevel-quasi-quote-macro
     map-ast
     map-ast/map-accessors-unless-same-returned
     bq-process bq-bracket bq-completely-process
     *bq-list*
     *bq-list**
     *bq-nconc*
     *bq-quote*
     *bq-append*
     *bq-simplify*
     *bq-clobberable*
     find-ancestor-syntax-node
     list-without-nils
     binary-position
     ub8-vector
     vector-extend
     append*
     not-yet-implemented
     wrap-forms-with-bindings
     quoted-symbol?
     self-evaluating?
     maybe-slurp-in-toplevel-quasi-quote
     map-tree map-filtered-tree
     princ-to-string-unless-nil integer-to-string
     self-evaluating?
     delayed-emitting
     as-delayed-emitting
     wrap-runtime-delayed-transformation-form
     transformation-typecase
     trace-transformation-functions
     trace-list-quasi-quote-functions
     output-transformer
     output-transformer-of
     transform
     transform*
     -environment-
     -transformation-
     transformation-pipeline
     coerce-to-transformation-pipeline
     transformation-pipeline-of
     *transformation*
     *transformation-pipeline*
     *transformation-environment*
     compatible-with-current-transformation-pipeline?
     compatible-transformation-pipelines?
     compatible-transformations?
     run-transformation-pipeline
     toplevel-quasi-quote-macro
     print-object/quasi-quote
     lisp-form-emitting-transformation
     make-syntax-node-emitting-form
     collect-slots-for-syntax-node-emitting-form
     )
   package))
