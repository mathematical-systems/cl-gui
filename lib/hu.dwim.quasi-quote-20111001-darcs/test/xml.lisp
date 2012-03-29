;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.quasi-quote.test)

(defsuite* (test/xml :in test))

(def special-variable *xml-stream*)

;; TODO should use define-syntax
(def function setup-readtable-for-xml-test (&key with-inline-emitting (binary #t) indentation-width)
  (enable-quasi-quoted-list-to-list-emitting-form-syntax)
  (if binary
      (progn
        (enable-quasi-quoted-string-to-binary-emitting-form-syntax
         '*xml-stream*
         :encoding :utf-8
         :with-inline-emitting with-inline-emitting)
        (enable-quasi-quoted-xml-to-binary-emitting-form-syntax
         '*xml-stream*
         :encoding :utf-8
         :text-node-escaping-method :per-character
         :indentation-width indentation-width
         :with-inline-emitting with-inline-emitting)
        (enable-quasi-quoted-string-to-binary-emitting-form-syntax
         '*xml-stream*
         :encoding :utf-8
         :with-inline-emitting with-inline-emitting))
      (progn
        (enable-quasi-quoted-string-to-string-emitting-form-syntax
         '*xml-stream*
         :with-inline-emitting with-inline-emitting)
        (enable-quasi-quoted-xml-to-string-emitting-form-syntax
         '*xml-stream*
         :text-node-escaping-method :per-character
         :indentation-width indentation-width
         :with-inline-emitting with-inline-emitting)
        (enable-quasi-quoted-string-to-string-emitting-form-syntax
         '*xml-stream*
         :with-inline-emitting with-inline-emitting))))

(def syntax-test-definer xml-test
  (:test-function   test-xml-emitting-forms
   :readtable-setup (setup-readtable-for-xml-test :with-inline-emitting #f))
  (:test-function   test-xml-emitting-forms
   :readtable-setup (setup-readtable-for-xml-test :with-inline-emitting #t)))

(def syntax-test-definer xml-test/inline
  (:test-function   test-xml-emitting-forms
   :readtable-setup (setup-readtable-for-xml-test :with-inline-emitting #t)))

(def syntax-test-definer xml-test/normal
  (:test-function   test-xml-emitting-forms
   :readtable-setup (setup-readtable-for-xml-test :with-inline-emitting #f)))

(def function read-from-string-with-xml-syntax (string &key (with-inline-emitting #f) (binary #f) (indentation-width 2))
  (with-local-readtable
    (setup-readtable-for-xml-test :with-inline-emitting with-inline-emitting :binary binary :indentation-width indentation-width)
    (read-from-string string)))

(def function pprint-xml (string &key (with-inline-emitting #f) (binary #f) (indentation-width 2))
  (downcased-pretty-print
   (macroexpand-all
    (read-from-string-with-xml-syntax string
                                      :with-inline-emitting with-inline-emitting
                                      :binary binary
                                      :indentation-width indentation-width))))

(def function emit/xml (string &key (with-inline-emitting #f) (binary #f) (indentation-width 2))
  (bind ((form (read-from-string-with-xml-syntax string
                                                 :with-inline-emitting with-inline-emitting
                                                 :binary binary
                                                 :indentation-width indentation-width)))
    (if binary
        (with-output-to-sequence (*xml-stream* :element-type '(unsigned-byte 8))
          (emit (eval form)))
        (with-output-to-string (*xml-stream*)
          (emit (eval form))))))

(def function test-xml-emitting-forms (expected ast)
  (bind ((lambda-form `(lambda ()
                         (with-output-to-sequence (*xml-stream* :element-type '(unsigned-byte 8))
                           (emit ,ast)))))
    ;;(print (macroexpand-all lambda-form))
    (is (equalp expected
                (octets-to-string (funcall (compile nil lambda-form))
                                  :encoding :utf-8)))))

(def function parse-xml-into-sxml (string)
  (labels ((drop-whitespace-nodes (node)
             (etypecase node
               (cons
                (list*
                 (first node)
                 (second node)
                 (iter (for child :in (rest (rest node)))
                       (unless (and (stringp child)
                                    (every 'cl-ppcre::whitespacep child))
                         (collect (drop-whitespace-nodes child))))))
               (string node))))
    (drop-whitespace-nodes (cxml:parse string (cxml-xmls:make-xmls-builder)))))

(def test test/xml/escaping/1 ()
  (is (string= "&lt;1&quot;2&gt;3&lt;&amp;4&gt;"
               (escape-as-xml "<1\"2>3<&4>")))
  (let ((str "alma"))
    (is (eq str (escape-as-xml str)))))

(def xml-test test/xml/escaping/2 ()
  (｢<element attribute="&lt;1&gt;"/>｣
   ｢<element (,@(list (make-xml-attribute "attribute" "<1>")))>｣)
  (｢<element attribute="&lt;1&gt;"/>｣
   ｢<element (attribute "<1>")>｣)
  (｢<element>&lt;tunneled&gt;42&lt;/tunneled&gt;</element>｣
   ｢<element () ,(make-xml-text "<tunneled>42</tunneled>")>｣))

(def xml-test test/xml/simple ()
  (｢<element/>｣
   ｢<element>｣)
  ;; this is braindead here, but let's just test that the name of the xml element is read unconditionally until
  ;; a newline, space, start-character, end-character or the unquote-character.
  (｢<aaa$#@!]{}[]()bbb/>｣
   ｢<aaa$#@!]{}[]()bbb>｣)
  (｢<element attribute="1"/>｣
   ｢<element (:attribute 1)>｣)
  (｢<element attribute1="1" attribute2="2"/>｣
   ｢<element (:attribute1 "1" :attribute2 "2")>｣)
  (｢<element>Hello</element>｣
   ｢<element "Hello">｣)
  ;; test that attribute list is optional
  (｢<element><child/></element>｣
   ｢<element <child>>｣)
  (｢<element><child/></element>｣
   ｢<element () <child>>｣))

(def xml-test test/xml/simple-dispatched ()
  (｢<element/>｣
   ｢`xml(element)｣)
  (｢<element attribute="1"/>｣
   ｢`xml(element (:attribute 1))｣)
  (｢<element attribute1="1" attribute2="2"/>｣
   ｢`xml(element (:attribute1 "1" :attribute2 "2"))｣)
  (｢<element>Hello</element>｣
   ｢`xml(element () "Hello")｣)
  (｢<element><child/></element>｣
   ｢`xml(element () (child))｣)
  (｢<element><child/></element>｣
   ｢`xml(element () child)｣)
  (｢&lt;escaped&gt;｣
   ｢`xml "<escaped>"｣))

(def xml-test test/xml/element-unquoting-dispatched ()
  (｢<element/>｣
   ｢`xml(,"element")｣)
  (｢<element><nested/></element>｣
   ｢`xml(element () ,(make-xml-element "nested"))｣)
  (｢<element><child1/><child2/><child3/><child4 attribute1="1"/><child5/></element>｣
   ｢`xml(element ()
          (child1)
          ,(make-xml-element "child2")
          ,@(list (make-xml-element "child3")
                  (make-xml-element "child4" (list (make-xml-attribute "attribute1" "1"))))
          (child5))｣))

(def xml-test test/xml/element-unquoting ()
  (｢<element/>｣
   ｢<,"element">｣)
  (｢<element><nested/></element>｣
   ｢<element
     ,(make-xml-element "nested")>｣)
  (｢<element><child1/><child2/><child3/><child4 attribute1="1"/><child5/></element>｣
   ｢<element
     <child1>
     ,(make-xml-element "child2")
     ,@(list (make-xml-element "child3")
             (make-xml-element "child4" (list (make-xml-attribute "attribute1" "1"))))
     <child5>>｣)
  (｢<a>foobar</a>｣
   ｢<a "foo" ,"bar">｣))

(def xml-test test/xml/attribute-unquoting ()
  (｢<element attribute="1"/>｣
   ｢<element (,@(list (make-xml-attribute "attribute" "1")))>｣)
  (｢<element />｣
   ｢<element (:attribute ,nil)>｣)
  (｢<element attribute="42"/>｣
   ｢<element (:attribute ,42)>｣)
  (｢<element attribute="42.42"/>｣
   ｢<element (:attribute ,42.42)>｣)
  (｢<element attribute="42"/>｣
   ;; this one is testing that in inline emitting the returned +void+ is not princ-to-string'ed as before.
   ;; the bug was triggered in a <foo (:bar ,`js-inline(42))> situation
   ｢<element (:attribute ,`xml ,42)>｣)
  (｢<element attribute1="1" attribute2="2" attribute3="3" attribute4="4" aTTriUte5="5" attribute6="6"/>｣
   ｢<element (attribute1 1
              ,(make-xml-attribute "attribute2" "2")
              ,@(list (make-xml-attribute "attribute3" "3")
                      (make-xml-attribute "attribute4" "4"))
              aTTriUte5 "5"
              ,(make-xml-attribute "attribute6" "6"))>｣))

(def xml-test test/xml/case-sensitivity ()
   ;; the xml reader is case sensitive, but unquoted regions are returning to the toplevel readtable's readtable-case
  (｢<eLement AttributE1="1"><ElemenT/><fOOO baR="42"/></eLement>｣
   ｢<eLement (AttributE1 1)
    ,@(progn
       (list
        <ElemenT>
        <fOOO (baR 42)>))>｣))

(def xml-test test/xml/nested-through-macro-using-lisp-quasi-quote/1 ()
  (｢<taggg attribute="atttr"><foo/><bar><baz/></bar></taggg>｣
   ｢(macrolet ((nester (tag-name attribute-value &body body)
                 `<,,tag-name (attribute ,,attribute-value) ,@,@body>))
      (nester "taggg" "atttr" <foo> <bar <baz>>))｣))

(def xml-test test/xml/nested-through-macro-using-lisp-quasi-quote/2 ()
  (｢<html><body><foo/><bar/></body></html>｣
   ｢(macrolet ((nester (&body body)
                 ;; first ,@ is for xml, the ,@body is for the lisp quasi-quote
                 `<html <body ,@,@body>>))
      (nester <foo> <bar>))｣))

(def xml-test test/xml/nested-through-macro-using-lisp-quasi-quote/3 ()
  (｢<taggg attribute="atttr"><foo/><bar><baz/></bar></taggg>｣
   ｢(macrolet ((nester (tag-name attribute-value &body body)
                 `<,,tag-name (attribute ,,attribute-value) ,@,@body>))
      ;; TODO because of the NIL (any non-xml node) in there this runs RUN-TRANSFORMATION-PIPELINE, but it shouldn't!
      (nester "taggg" "atttr" <foo> nil <bar <baz>>))｣))

(def xml-test test/xml/macrolet-in-unquote ()
  (｢<wrapper><element><tag1>value1</tag1><tag2>value2</tag2></element><last/></wrapper>｣
   ｢(macrolet ((wrapper (&body body)
                 `<wrapper ,@,@body>))
      (wrapper
       <element
        ,@(macrolet ((x (tag value)
                        `<,,tag () ,,value>))
                    (list
                     (x "tag1" "value1")
                     (x "tag2" "value2")))>
       <last>))｣))

(def xml-test test/xml/binary-bug-trigger ()
  (｢<div id="43">alma</div>｣
   ｢(macrolet ((render-dojo-widget (id &body body)
                 `(progn
                    (null ,id)
                    ,@body)))
      (bind ((idd 42))
        (render-dojo-widget idd
          <div (:id ,(format nil "43"))
               ,"alma">)))｣))

(def test test/xml/sharp-plus-works ()
  (enable-quasi-quoted-xml-syntax)
  (is (eql 42 (read-from-string "#+nil(< 1 2) 42"))))

(def test test/xml/errors ()
  (enable-quasi-quoted-xml-syntax)
  (signals reader-error
    (read-from-string "<element < >>"))
  (signals reader-error
    (read-from-string "<al&ma>"))
  (signals reader-error
    (read-from-string "<alma (kor&te 42)>"))
  (signals end-of-file
    (read-from-string "<element")))

(def test test/xml/less-then-sign-at-toplevel ()
  (enable-quasi-quoted-xml-syntax)
  (is (not (equal '<a> (read-from-string "<a>"))))
  (is (equal '< (read-from-string "<")))
  (is (equal '<= (read-from-string "<=")))
  (is (equal '(< a b) (read-from-string "(< a b)"))))

(def xml-test test/xml/spliced-attribute-list ()
  (｢<element ok="1"/>｣
   ｢<element (,@(when (< 3 4) (list (make-xml-attribute "ok" "1"))))>｣))

(def xml-test/normal test/xml/nested-unquoting ()
  (｢<a><b><c><d/></c></b></a>｣
   ｢<a ,(make-xml-element "b" nil (list <c ,(make-xml-element "d")>))>｣))

(def xml-test test/xml/mixed ()
  (｢<element>Hello UN<>QUOTED World</element>｣
   ｢<element `str("Hello" ,(list " UN<>QUOTED " "World"))>｣)
  (｢<element attribute="42"/>｣
   ｢<element (:attribute `str("4" ,(list "2")))>｣)
  (｢<element attribute="fooBAR"/>｣
   ｢<element (:attribute `str("foo" ,(string-upcase "bar")))>｣))

(def xml-test/normal test/xml/reverse ()
  ("<element><child2/><child1/></element>"
   ｢<element ,@(let ((c1 <child1>)
                    (c2 <child2>))
                   (list c2 c1))>｣))
