;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.walker.documentation)

(def project :hu.dwim.walker)

(def method make-project-tab-pages ((component project/detail/inspector) (project (eql (find-project :hu.dwim.walker))))
  (append (list (tab-page/widget (:selector (icon/widget switch-to-tab-page :label "User guide"))
                  (make-value-inspector (find-book 'user-guide)))
                (tab-page/widget (:selector (icon/widget switch-to-tab-page :label "Dictionary"))
                  (make-value-inspector (mapcar 'find-dictionary '(walker)))))
          (call-next-method)))

(def book user-guide (:title "User guide")
  (chapter (:title "Introduction")
    (paragraph ()
      "A code walker that parses a Common Lisp sexp into an AST, a tree of CLOS objects. This AST is a much more convenient representation to do various code analysis and transformations than sexps. Also features an unwalker that can turn the walked CLOS AST tree back into an sexp.")
    (chapter (:title "Known forks")
      (paragraph ()
        "An obsolete one: " (parse-uri "http://github.com/angavrilov/cl-walker/commits/master/") ". Alexander's excellent changes are incorporated regularly into the official repo."))
    (chapter (:title "History")
      (paragraph ()
        "It was originally written by Marco Baringer in his Arnesi library. With his permission it was first factored out into a standalone cl-walker library and then renamed to hu.dwim.walker as part of a broad refactoring of all our (http://dwim.hu) libs.")
      (paragraph ()
        "Contains important contribution by several people, please consult the history of the source repository for the details.")))
  (chapter (:title "Status")
    (paragraph ()
      "It's in a very good shape, used in production code. It's not completely settled down though, still receives regular patches and extensions."))
  (chapter (:title "Supported Common Lisp Implementations")
    (paragraph ()
      "SBCL"
      "Allegro"
      "ECL"
      "CCL"))
  (chapter (:title "Tutorial")
    (paragraph ()
      "TODO")))

(def dictionary walker ()
  walk-form
  unwalk-form)
