;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.quasi-quote.documentation)

(def project :hu.dwim.quasi-quote)

(def book user-guide (:title "User guide")
  (chapter (:title "Introduction")
    (paragraph ()
      "TODO"))
  (chapter (:title "Supported Common Lisp Implementations")
    (paragraph ()
      "SBCL"))
  (chapter (:title "Supported Operating Systems")
    (paragraph ()
      "Linux"))
  (chapter (:title "Tutorial")
    (paragraph ()
      "TODO")))

#| ;; TODO:
- subclass quasi-quote and unquote for each DSL so asserts can help
- additional DSLs: string, vector, bivalent, XML, XHTML, SQL, JS, typesetting, pdf
- continuation support?
- what about lazyness?
- what about computed-class?

- extend xml syntax to support this (?)
<p (some-attribute 42)
  this is free text that will <i not> intern anything in any package while being read>

*** js

- introduce a js-special-form that is called on the sexp and rename
  the current definer to walked-js-special-form
|#
