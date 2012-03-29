;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.quasi-quote.test)

(enable-quasi-quoted-pdf-syntax)

(defsuite* (test/pdf :in test))

(def special-variable *pdf-stream*)

(def function test-pdf-ast (name ast)
  (with-open-file (*pdf-stream* (string-downcase (concatenate 'string "/tmp/" (substitute #\- #\/ (symbol-name name)) ".pdf"))
                                :direction :output :element-type '(unsigned-byte 8) :if-does-not-exist :create :if-exists :supersede)
    (transform-and-emit '(quasi-quoted-pdf
                          quasi-quoted-bivalent
                          quasi-quoted-binary
                          (binary-emitting-form :stream-name *pdf-stream*)
                          lambda-form
                          lambda)
                        ast)))

(def definer pdf-test (name args &body forms)
  `(def test ,name ,args
     (finishes
       (test-pdf-ast ',name ,@forms))))

(def pdf-test test/pdf/empty ()
  [document
   [info [dictionary "Author" "levy"]]
   [root [catalog "Pages" [pages "Count" 0
                                 "MediaBox" [array 0 0 612 792]]]]])

(def pdf-test test/pdf/simple ()
  [document
   [info [dictionary "Author" "levy"]]
   [root [catalog "Pages" [indirect-object-reference pages]]]
   [indirect-object pages
                    [pages "Count" 1
                           "Kids" [array [indirect-object-reference page1]]
                           "MediaBox" [array 0 0 612 792]]]
   [indirect-object page1
                    [page "Parent" [indirect-object-reference pages]
                          "Resources" [dictionary
                                       "ProcSet" [array [name "PDF"] [name "Text"]]
                                       "Font" [dictionary
                                               "F1" [dictionary
                                                     "Type"     [name "Font"]
                                                     "Subtype"  [name "Type1"]
                                                     "Name"     [name "F1"]
                                                     "BaseFont" [name "Times-Roman"]]]]
                          "Contents" [indirect-object-reference stream1]]]
   [indirect-object stream1
                    [stream
                     [begin-text]
                     [set-font "F1" 12]
                     [move-text 72 712]
                     [string "Hello World"]
                     [display-text]
                     [end-text]]]])

(def pdf-test test/pdf/tagged ()
  [document
   [info [dictionary "Author" "levy"]]
   [root [catalog "Pages" [indirect-object-reference pages]
                  "StructTreeRoot" [indirect-object-reference structure-tree-root]]]
   [indirect-object pages
                    [pages "Count" 1
                           "Kids" [array [indirect-object-reference page1]]
                           "MediaBox" [array 0 0 612 792]]]
   [indirect-object page1
                    [page "Parent" [indirect-object-reference pages]
                          "Resources" [dictionary
                                       "ProcSet" [array [name "PDF"] [name "Text"]]
                                       "Font" [dictionary
                                               "F1" [dictionary
                                                     "Type"     [name "Font"]
                                                     "Subtype"  [name "Type1"]
                                                     "Name"     [name "F1"]
                                                     "BaseFont" [name "Times-Roman"]]]]
                          "Contents" [indirect-object-reference stream1]]]
   [indirect-object structure-tree-root
                    [structure-tree-root "K" [indirect-object-reference structure-element1]]]
   [indirect-object structure-element1
                    [dictionary "P" [indirect-object-reference structure-tree-root]
                                "S" [name "P"]
                                "K" [dictionary "Type" "MCR"
                                                "PG" [indirect-object-reference page1]
                                                "MCID" 0]]]
   [indirect-object stream1
                    [stream
                     [paragraph
                       [begin-text]
                       [set-font "F1" 12]
                       [move-text 72 712]
                       [string "Hello World Hello World Hello World Hello World Hello World Hello World Hello World Hello World Hello World Hello World Hello World Hello World Hello World Hello World Hello World Hello World Hello World Hello World Hello World Hello World Hello World Hello World Hello World Hello World "]
                       [display-text]
                       [end-text]]]]])
