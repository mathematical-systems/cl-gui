;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.quasi-quote.test)

(defsuite* (test/css :in test))

(def special-variable *css-stream*)

(def test test-css/1 ()
  (bind ((*transformation* (make-instance 'quasi-quoted-css-to-quasi-quoted-string :indentation-width 0)))
    (hu.dwim.quasi-quote.css::transform-quasi-quoted-css-to-quasi-quoted-string
     (make-css-quasi-quote nil (list (make-css-clause (list (make-css-element-selector "body" nil))
                                                 (list (make-css-attribute "color" "black"))))))))

(def test test-css/2 ()
  (run-transformation-pipeline (make-css-quasi-quote (make-quasi-quoted-css-to-form-emitting-transformation-pipeline '*standard-output*)
                                                     (list (make-css-clause (list (make-css-element-selector "body" nil))
                                                                       (list (make-css-attribute "color" "black")))))))




(def function setup-readtable-for-css-test (&key with-inline-emitting (binary #t) indentation-width)
  (enable-quasi-quoted-list-to-list-emitting-form-syntax)
  (if binary
      (progn
        (enable-quasi-quoted-string-to-binary-emitting-form-syntax
         '*css-stream*
         :encoding :utf-8
         :with-inline-emitting with-inline-emitting)
        (enable-quasi-quoted-css-to-binary-emitting-form-syntax
         '*css-stream*
         :encoding :utf-8
         :indentation-width indentation-width
         :with-inline-emitting with-inline-emitting)
        (enable-quasi-quoted-string-to-binary-emitting-form-syntax
         '*css-stream*
         :encoding :utf-8
         :with-inline-emitting with-inline-emitting))
      (progn
        (enable-quasi-quoted-string-to-string-emitting-form-syntax
         '*css-stream*
         :with-inline-emitting with-inline-emitting)
        (enable-quasi-quoted-css-to-string-emitting-form-syntax
         '*css-stream*
         :indentation-width indentation-width
         :with-inline-emitting with-inline-emitting)
        (enable-quasi-quoted-string-to-string-emitting-form-syntax
         '*css-stream*
         :with-inline-emitting with-inline-emitting))))

(def function test-css-emitting-forms (expected ast)
  (bind ((lambda-form `(lambda ()
                         (with-output-to-sequence (*css-stream* :element-type '(unsigned-byte 8))
                           (emit ,ast)))))
    ;;(print (macroexpand-all lambda-form))
    (is (equalp expected
                (octets-to-string (funcall (compile nil lambda-form))
                                  :encoding :utf-8)))))

(def syntax-test-definer css-test
  (:test-function   test-css-emitting-forms
   :readtable-setup (setup-readtable-for-css-test :with-inline-emitting #f))
  (:test-function   test-css-emitting-forms
   :readtable-setup (setup-readtable-for-css-test :with-inline-emitting #t)))

(def function read-from-string-with-css-syntax (string &key (with-inline-emitting #f) (binary #f) (indentation-width 0))
  (with-local-readtable
    (setup-readtable-for-css-test :with-inline-emitting with-inline-emitting :binary binary :indentation-width indentation-width)
    (read-from-string string)))

(def css-test test/css/simple-selector ()
  ("* { color: black; }"
   ｢`css (((*) :color black))｣)
  ("body { color: black; }"
   ｢`css (((body) :color black))｣)
  ("body { color: black; }"
   ｢`css ((((body)) :color black))｣)
  ("body, th { color: black; }"
   ｢`css ((((body) (th)) :color black))｣)
  ("body + th { color: black; }"
   ｢`css ((((+ (body) (th))) :color black))｣)
  ("body > th > td { color: black; }"
   ｢`css ((((> (body) (th) (td))) :color black))｣)
  ("body th > td { color: black; }"
   ｢`css ((((> (- (body) (th)) (td))) :color black))｣)
  ("body th > td { color: black; }"
   ｢`css ((((> (- body th) td)) :color black))｣))

(def css-test test/css/simple-selector-quoting ()
  ("body { color: black; }"
   ｢`css (((,"body") :color black))｣)
  ("body { color: black; }"
   ｢`css ((((,"body")) :color black))｣)
  ("body, td { color: black; }"
   ｢`css (((,@(list "body" "td")) :color black))｣)
  ("body { color: black; }"
   ｢`css (((,(make-css-element-selector "body" nil)) :color black))｣)
  ("h1, body, td, h2 { color: black; }"
   ｢`css (((h1
            ,@(list "body" "td")
            ,(make-css-element-selector "h2" nil))
           :color black))｣)
  ("h1 > body > td > h2 { color: black; }"
   ｢`css ((((>
             h1
             ,@(list "body" "td")
             ,(make-css-element-selector "h2" nil)))
           :color black))｣))

(def css-test test/css/simple-selector-attribute ()
  ("body[type=\"password\"] { color: black; }"
   ｢`css ((((body :type password)) :color black))｣)
  ("body[type=\"password\"][name=\"mypass\"] { color: black; }"
   ｢`css ((((body :type password :name mypass)) :color black))｣)
  ("body.my-class { color: black; }"
   ｢`css ((((body :class my-class)) :color black))｣)
  ("body.my-class.my-class2 { color: black; }"
   ｢`css ((((body :class my-class :class my-class2)) :color black))｣)
  ("body.my-class.my-class2 { color: black; }"
   ｢`css ((((body :class (my-class my-class2))) :color black))｣)
  ("body:my-class { color: black; }"
   ｢`css ((((body :pseudo-class my-class)) :color black))｣)
  ("body:my-class:my-class2 { color: black; }"
   ｢`css ((((body :pseudo-class my-class :pseudo-class my-class2)) :color black))｣)
  ("body:my-class:my-class2 { color: black; }"
   ｢`css ((((body :pseudo-class (my-class my-class2))) :color black))｣)
  ("body#my-id { color: black; }"
   ｢`css ((((body :id my-id)) :color black))｣)
  ("#my-id { color: black; }"
   ｢`css ((((* :id my-id)) :color black))｣)
  (".my-class { color: black; }"
   ｢`css ((((* :class my-class)) :color black))｣)
  (":my-class { color: black; }"
   ｢`css ((((* :pseudo-class my-class)) :color black))｣)  
  (":my-class { color: black; }"
   ｢`css ((((* :pseudo-class my-class)) :color black))｣)  
  ("[type=\"x\"] { color: black; }"
   ｢`css ((((* :type x)) :color black))｣))

(def css-test test/css/simple-selector-attribute-quoting ()
  ("body[type=\"10\"] { color: black; }"
   ｢`css((((body :type ,10)) :color black))｣)
  ("body[attr=\"10\"] { color: black; }"
   ｢`css(((
            (body ,(make-css-attribute-selector "attr" "10")))
           :color black))｣)
  ("body#my-id { color: black; }"
   ｢`css (((
            (body ,(make-css-attribute-selector "id" "my-id")))
           :color black))｣)
  ("body[type=\"type1\"][type=\"type2\"] { color: black; }"
   ｢`css (((
            (body ,(make-css-attribute-selector "type" (list "type1" "type2"))))
           :color black))｣)
  ("body.my-class { color: black; }"
   ｢`css (((
            (body ,(make-css-attribute-selector "class" "my-class")))
           :color black))｣)
  ("body:my-class { color: black; }"
   ｢`css (((
            (body ,(make-css-attribute-selector "pseudo-class" "my-class")))
           :color black))｣)
  ("body.my-class1.my-class2 { color: black; }"
   ｢`css (((
            (body ,(make-css-attribute-selector "class" (list "my-class1" "my-class2"))))
           :color black))｣)
  ("a#my_id.button.vertical[alt=\"download\"]:hover { color: black; }"
   ｢`css (((
            (a
             :id "my_id"
             ,@(list
                (make-css-attribute-selector "class" (list "button" "vertical"))
                (make-css-attribute-selector "alt" "download"))
             :pseudo-class "hover"))
           :color black))｣))

(def css-test test/css/simple-attribute ()
  ("body { color: black; }"
   ｢`css (((body) :color black))｣)
  ("body { color: black; z-index: 2; }"
   ｢`css (((body) :color black :z-index 2))｣)
  ("body { margin: 10px 20px; }"
   ｢`css (((body) :margin (10px 20px)))｣)
  ("body { font-family: \"Arial\"; }"
   ｢`css (((body) :font-family "Arial"))｣)
  ("body { background-image: url(\"/images/image.png\"); }"
   ｢`css (((body) :background-image (url "/images/image.png")))｣)
  ("body { background-color: #0000FF; }"
   ｢`css (((body) :background-color (color 255)))｣)
  ("body { background-color: rgb(255,255,255); }"
   ｢`css (((body) :background-color (rgb 255 255 255)))｣)
  ("body { attribute: rgb(255,255,255) #000100 url(\"img.jpg\") black 123px no-repeat \"Arial\"; }"
   ｢`css (((body) :attribute ((rgb 255 255 255)
                              (color 256 )
                              (url "img.jpg")
                              black
                              123px
                              no-repeat
                              "Arial")))｣)
  )

(def css-test test/css/simple-attribute-quoting ()
  ("body { z-index: 10; }"
   ｢`css(((body) :z-index ,10))｣)
  ("body { font-family: \"Arial\"; }"
   ｢`css(((body) :font-family ,"Arial"))｣)
  ("body { attr: 10; }"
   ｢`css(((body) ,(make-css-attribute "attr" 10)))｣)  
  ("body { attr: \"Arial\"; }"
   ｢`css(((body) ,(make-css-attribute "attr" "Arial")))｣)
  ("body { font: 12px \"Arial\"; }"
   ｢`css(((body) :font ,'(12px "Arial")))｣)
  ("body { font: 12px \"Arial\"; }"
   ｢`css(((body) :font (12px ,"Arial")))｣)
  ("body { font: 12px 12 \"Arial\"; }"
   ｢`css(((body) :font (12px ,@(list 12 "Arial"))))｣)
  ("body { background-image: url(\"img.jpg\"); }"
   ｢`css(((body) :background-image ,(make-css-annotated-value "url" "img.jpg")))｣)
  ("body { background-image: url(\"img.jpg\"); }"
   ｢`css(((body) :background-image (url ,"img.jpg")))｣)
  ("body { background-color: #000100; }"
   ｢`css(((body) :background-color (color ,(* 2 128))))｣)
  ("body { background-color: rgb(245,245,254); }"
   ｢`css(((body) :background-color (rgb 245 ,245 ,(* 2 127))))｣)
  ("body { attribute: rgb(255,255,255) #000100 url(\"img.jpg\") black 123px no-repeat \"Arial\"; }"
   ｢`css(((body) :attribute ((rgb 255 255 255)
                              ,(make-css-annotated-value "color" 256)
                              (url "img.jpg")
                              ,@(list 'black '123px)
                              no-repeat
                              ,"Arial")))｣)
  ("body { width: 15px; font: 15px \"Arial\"; border-color: #0000FF; vertical-align: top; color: black; }"
   ｢`css(((body)
           :width 15px
           ,@(list
              (make-css-attribute "font" (list '|15px| "Arial"))
              (make-css-attribute "border-color" (make-css-annotated-value "color" 255)))
           :vertical-align top
           :color ,'black))｣))

(def css-test test/css/simple-attribute-inline ()
  ("width: 15px; font: 15px \"Arial\"; border-color: #0000FF; vertical-align: top; color: black; "
   ｢`css(:width 15px
         ,@(list
            (make-css-attribute "font" (list '|15px| "Arial"))
            (make-css-attribute "border-color" (make-css-annotated-value "color" 255)))
         :vertical-align top
         :color ,'black)｣)
  )
