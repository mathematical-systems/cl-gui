;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.quasi-quote.test)

(defsuite* (test/js :in test))

;;; Running the js tests requires a js command line interpreter.
;;; On Linux you can install the package 'spidermonkey-bin' to get one.
;;; update-alternatives --list js
;;; sudo update-alternatives --set js /usr/bin/smjs

(progn
  ;; KLUDGE: ASDF grabs the Big Compiler Lock by using WITH-COMPILATION-UNIT
  ;; KLUDGE: TRIVIAL-SHELL reads the standard-output in a separate thread and PCL cache needs the same lock
  ;; KLUDGE: avoid deadlock by initializing PCL cache now
  #+sbcl
  (sb-thread::release-mutex sb-c::**world-lock**)
  (unless (search "JavaScript" (nth-value 1 (trivial-shell:shell-command "js --version")))
    (warn "You need a command line JavaScript interpreter for the hu.dwim.quasi-quote.js tests. Install the spidermonkey-bin package for one..."))
  #+sbcl
  (sb-thread::get-mutex sb-c::**world-lock**))

(def (function d) eval-js (string)
  (bind (((:values stdout nil return-code) (trivial-shell:shell-command "js" :input string)))
    (is (= 0 return-code))
    (with-input-from-string (input stdout)
      (bind ((result (read-line input #f)))
        (block nil
          ;; KLUDGE parse-number::invalid-number is not a serious-condition...
          (awhen (ignore-some-conditions (parse-number::invalid-number serious-condition)
                   (parse-number:parse-number result))
            (return it))
          #+nil
          (switch (result :test #'string=)
            ("true" (return #t))
            ("false" (return #f))
            ("undefined" (return 'undefined)))
          result)))))

(def special-variable *js-stream*)
(def special-variable *xml+js-stream*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def function setup-readtable-for-js-test (&key with-inline-emitting (indentation-width 2) (binary #f)
                                                  (output-prefix #.(format nil "~%<script>~%// <![CDATA[~%"))
                                                  (output-postfix #.(format nil "~%// ]]>~%</script>~%"))
                                                  (xml? #f) (output-stream-name (if xml? '*xml+js-stream* '*js-stream*)))
    (enable-quasi-quoted-list-to-list-emitting-form-syntax)
    (enable-quasi-quoted-js-syntax
     :transformation-pipeline (lambda ()
                                (if (= 1 *quasi-quote-lexical-depth*)
                                    (make-quasi-quoted-js-to-form-emitting-transformation-pipeline
                                     output-stream-name
                                     :binary binary
                                     :with-inline-emitting with-inline-emitting
                                     :indentation-width indentation-width)
                                    (make-quasi-quoted-js-to-form-emitting-transformation-pipeline
                                     output-stream-name
                                     :binary binary
                                     :with-inline-emitting with-inline-emitting
                                     :indentation-width indentation-width
                                     :output-prefix output-prefix
                                     :output-postfix output-postfix)))
     :dispatched-quasi-quote-name 'js)
    (enable-quasi-quoted-js-syntax
     :transformation-pipeline (make-quasi-quoted-js-to-form-emitting-transformation-pipeline
                               output-stream-name :binary binary :with-inline-emitting with-inline-emitting
                               :indentation-width indentation-width
                               :escape-as-xml #t)
     :dispatched-quasi-quote-name 'js-inline)
    (if binary
        (progn
          (when xml?
            (enable-quasi-quoted-xml-to-binary-emitting-form-syntax
             output-stream-name
             :encoding :utf-8
             :indentation-width indentation-width
             :with-inline-emitting with-inline-emitting))
          (enable-quasi-quoted-string-to-binary-emitting-form-syntax
           output-stream-name
           :encoding :utf-8
           :with-inline-emitting with-inline-emitting))
        (progn
          (when xml?
            (enable-quasi-quoted-xml-to-string-emitting-form-syntax
             output-stream-name
             :indentation-width indentation-width
             :with-inline-emitting with-inline-emitting))
          (enable-quasi-quoted-string-to-string-emitting-form-syntax
           output-stream-name
           :with-inline-emitting with-inline-emitting)))))

(def syntax-test-definer js-test
  (:test-function   test-js-emitting-forms
   :readtable-setup (setup-readtable-for-js-test :with-inline-emitting #f))
  (:test-function   test-js-emitting-forms/binary
   :readtable-setup (setup-readtable-for-js-test :with-inline-emitting #f :binary #t))
  (:test-function   test-js-emitting-forms
   :readtable-setup (setup-readtable-for-js-test :with-inline-emitting #t))
  (:test-function   test-js-emitting-forms/binary
   :readtable-setup (setup-readtable-for-js-test :with-inline-emitting #t :binary #t)))

(def syntax-test-definer xml+js-test
  (:test-function   test-xml+js-emitting-forms
   :readtable-setup (setup-readtable-for-js-test :with-inline-emitting #f :xml? #t))
  (:test-function   test-xml+js-emitting-forms/binary
   :readtable-setup (setup-readtable-for-js-test :with-inline-emitting #f :xml? #t :binary #t))
  (:test-function   test-xml+js-emitting-forms
   :readtable-setup (setup-readtable-for-js-test :with-inline-emitting #t :xml? #t))
  (:test-function   test-xml+js-emitting-forms/binary
   :readtable-setup (setup-readtable-for-js-test :with-inline-emitting #t :xml? #t :binary #t)))

(def function read-from-string-with-xml+js-syntax (string &optional (with-inline-emitting #f) (binary #f))
  (with-local-readtable
    (setup-readtable-for-js-test :with-inline-emitting with-inline-emitting :xml? #t :binary binary)
    (read-from-string string)))

(def function pprint/xml+js (string &optional (with-inline-emitting #f) (binary #f))
  (downcased-pretty-print (macroexpand (read-from-string-with-xml+js-syntax string with-inline-emitting binary))))

(def function emit/xml+js (string &optional (with-inline-emitting #f) (binary #f))
  (bind ((form (read-from-string-with-xml+js-syntax string with-inline-emitting binary)))
    (with-output-to-string (*xml+js-stream*)
      (emit (eval form)))))

(def function js-result-equal (a b)
  (if (and (typep a 'float)
           (typep b 'float))
      (< (abs (- a b)) 0.0000001)
      (equal a b)))

(def (function d) test-js-emitting-forms (expected ast)
  (bind ((lambda-form `(lambda ()
                         (with-output-to-string (*js-stream*)
                           (emit ,ast)))))
    ;;(print (macroexpand-all lambda-form))
    (is (js-result-equal expected
                         (eval-js
                          (funcall (compile nil lambda-form)))))))

(def function test-js-emitting-forms/binary (expected ast)
  (bind ((lambda-form `(lambda ()
                         (with-output-to-sequence (*js-stream* :element-type '(unsigned-byte 8))
                           (emit ,ast)))))
    ;;(print (macroexpand-all lambda-form))
    (is (js-result-equal expected
                         (eval-js
                          (octets-to-string (funcall (compile nil lambda-form))
                                            :encoding :utf-8))))))

(def function test-xml+js-emitting-forms (expected ast)
  (bind ((lambda-form `(lambda ()
                         (with-output-to-string (*xml+js-stream*)
                           (emit ,ast)))))
    (is (js-result-equal expected (eval-js
                                   (funcall (compile nil lambda-form)))))))

(def function test-xml+js-emitting-forms/binary (expected ast)
  (bind ((lambda-form `(lambda ()
                         (with-output-to-sequence (*xml+js-stream* :element-type '(unsigned-byte 8))
                           (emit ,ast)))))
    ;;(print (macroexpand-all lambda-form))
    (is (js-result-equal expected
                         (eval-js
                          (octets-to-string (funcall (compile nil lambda-form))
                                            :encoding :utf-8))))))

;;;;;;
;;; the tests finally

(def js-test test/js/simple ()
  (42
   ｢`js(print (+ 40 2))｣)
  (42
   ｢`js(let ((a 42))
         (decf a)
         (print (incf a)))｣))

(def js-test test/js/literals ()
  (1.42
   ｢`js(print 1.42)｣)
  ((coerce 1/3 'float)
   ｢`js(print 1/3)｣)
  ("null"
   ｢`js(print nil)｣)
  ("undefined"
   ｢`js(print undefined)｣)
  ("true"
   ｢`js(print t)｣)
  ("true"
   ｢`js(print true)｣)
  ("false"
   ｢`js(print false)｣))

(def js-test test/js/unquoted-literals ()
  (1.42
   ｢`js(print ,1.42)｣)
  ((coerce 1/3 'float)
   ｢`js(print ,1/3)｣)
  ("null"
   ｢`js(print ,nil)｣)
  ("true"
   ｢`js(print ,t)｣)
  ("undefined"
   ｢`js(print ,'undefined)｣)
  ("true"
   ｢`js(print ,'true)｣)
  ("false"
   ｢`js(print ,'false)｣))

(def js-test test/js/unquote ()
  (42
   ｢`js(let ((a 20))
         (print (+ a ,(+ 20 2))))｣)
  (42
   ｢`js(let ((a ,(+ 20 2)))
         (print (+ a 10 ,10)))｣)
  (14
   ｢`js(let ((x 10))
         (defun ,'alma ()
           (setf x ,(+ 2 2))
           (return 3))
         (print (setf x (+ 2 (,'alma) x 5))))｣))

(def js-test test/js/expressions ()
  ("beforexafter"
   ｢`js(let ((x "x"))
         (setf x (+ "before" x "after"))
         (print x))｣)
  (14
   ｢`js(let ((x 10))
         (defun side-effect ()
           (setf x 4)
           (return 3))
         (print (setf x (+ 2 (side-effect) x 5))))｣)
  ("foo"
   ｢`js(print (.to-string ((lambda (x) (return x)) "foo")))｣))

(def js-test test/js/dotted-call ()
  ("true"
   ｢`js(let ((x "-"))
         (.to-string (+ "" x))
         (print (not (not (.match (+ "foo" x "bar") "o-b")))))｣))

(def js-test test/js/lambda-call ()
  (42
   ｢`js(flet ((produce-lambda (a)
                (return (lambda (b)
                          (return (+ a b))))))
         (print ((produce-lambda 2) 40)))｣))

(def js-test test/js/precedence1 ()
  ("ok"
   ｢`js(print (if (and t
                       (> 3 4))
                  "wrong"
                  "ok"))｣))

(def js-test test/js/1+ ()
  (212
   ｢`js(let ((var 1))
         (print (+ ""
                   (.toString (1+ var))
                   (.toString var)
                   (.toString (incf var)))))｣))

(def js-test test/js/if ()
  ("then"
   ｢`js(if (< 2 3)
           (print "then")
           (print "else"))｣)
  ("else"
   ｢`js(if (< 3 2)
           (print "then")
           (print "else"))｣)
  ("else"
   ｢`js(if (< 3 2)
           (progn
             (print "multiple statements")
             (print "then"))
           (progn
             (progn
               (let ((output "else"))
                 (print output)))))｣)
  ("ok"
   ｢`js(if (< 3 2)
           (if (< 2 3)
               (print "then")
               (print "else"))
           (print "ok"))｣))

(def js-test test/js/if-as-expression ()
  ("else"
   ｢`js(let ((x (if (< 3 2)
                    "then"
                    "else")))
         (print x))｣)
  ("second"
   ｢`js(let ((nothing nil)
             (x (or nothing
                    "second")))
         (print x))｣)
  ("then"
   ｢`js(print (if (< 2 3)
                  "then"
                  "else"))｣)
  ("alma-barack"
   ｢`js(print (+ "alma"
                 (if (< 2 3)
                     "-"
                     "zork")
                 "barack"))｣))

(def js-test test/js/cond ()
  ("third"
   ｢`js(cond ((< 3 2)
              (print "first"))
             ((< 5 4)
              (print "second"))
             ((< 1 2)
              (print "third"))
             (t (print "default")))｣)
  ("second"
   ｢`js(cond ((< 3 2)
              (print "more statements")
              (print "first"))
             ((< 4 5)
              (print "second"))
             (t (print "default")))｣)
  ("first"
   ｢`js(cond ((< 2 3)
              (print "first"))
             (t (print "default")))｣)
  ("no default"
   ｢`js(cond ((< 2 3)
              (print "no default")))｣))

(def js-test test/js/conditionals ()
  ("ok"
   ｢`js(if (and (or (not true) true)
                (not false))
           (print "ok")
           (print "wrong"))｣))

(def js-test test/js/do ()
  (15
   ｢`js(let* ((vector #(1 2 3 4 5))
              (length (slot-value vector 'length))
              (sum 0))
         (do ((idx 0 (1+ idx)))
             ((>= idx length))
           (incf sum (aref vector idx)))
         (print sum))｣))

(def js-test test/js/unwind-protect ()
  (45
   ｢`js(let ((a 42))
         (unwind-protect
              (progn
                (incf a)
                (incf a))
           (incf a))
         (print a))｣))

(def js-test test/js/try-catch ()
  (54
   ｢`js(let ((a 42))
         (try
              (progn
                (incf a)
                (throw 10)
                (setf a 0))
           (catch (e)
             (incf a e))
           (finally
            (incf a)))
         (print a))｣))

(def js-test test/js/arrays ()
  ("10,20"
   ｢`js(print (.to-string (vector 10 20)))｣)
  (10
   ｢`js(print (aref (list 10 20) 0))｣)
  (20
   ｢`js(print (elt (vector 10 20) 1))｣))

(def test test/js/array-errors ()
  (flet ((transform (string)
           (transform (macroexpand (read-from-string-with-xml+js-syntax string)))))
    (signals js-compile-error
      (transform ｢`js(elt (vector 10 20) 1 2 3 4 5)｣))
    (signals js-compile-error
      (transform ｢`js(aref (vector 10 20))｣))))

(def js-test test/js/slot-value ()
  (1
   ｢`js(print (slot-value (create :a 1 :b 2) 'a))｣)
  (2
   ｢`js(let ((a "b"))
         (print (slot-value (create :a 1 :b 2) a)))｣))

(def js-test test/js/create ()
  (1
   ｢`js(print (slot-value (slot-value (create "a" (create "b" ,1)) 'a) 'b))｣)
  ("true"
   ｢`js(print (slot-value (create :foo true) 'foo))｣)
  (42
   ｢`js(let ((x 42))
         (print (slot-value (create :foo x) 'foo)))｣))

(def js-test test/js/create-unquote ()
  (1
   ｢`js(print (slot-value (create ,@(list "a" 1 "b" 2)) 'a))｣)
  (4
   ｢`js(let ((a "b"))
         (print (slot-value (create ,"a" ,3 "b" ,4) a)))｣))

(def test test/js/create-unquote/errors ()
  (flet ((transform (string)
           (transform (macroexpand (read-from-string-with-xml+js-syntax string)))))
    (signals js-compile-error
      (transform ｢`js(create "a" ,@1)｣))
    (signals js-compile-error
      (transform ｢`js(create ,@1 "b")｣))))

(def js-test test/js/rebind ()
  (42
   ｢`js(let* ((x 40)
              (y 2)
              (fn (rebind/expression (x y)
                    (lambda (event)
                      (print (+ x y))))))
         (setf x 0)
         (setf y 0)
         (fn))｣)
  (42
   ｢`js(let* ((x 40)
              (y 2)
              (fn ))
         (rebind (x y)
           (setf fn (lambda (event)
                      (print (+ x y)))))
         (setf x 0)
         (setf y 0)
         (fn))｣))

(def test test/js/rebind/errors ()
  (flet ((transform (string)
           (transform (macroexpand (read-from-string-with-xml+js-syntax string)))))
    (signals error (transform ｢`js(let ((fn (rebind (x y)
                                               (lambda (event)
                                                 (print (+ x y)))))))｣))
    (signals error (transform ｢`js(rebind/expression (x y)
                                    (print 1)
                                    (print 2))｣))))

(def xml+js-test test/js/macrolet/1 ()
  (42
   ｢`js(macrolet ((macro (var value &body body)
                    `(let ((,var ,value))
                       ,@body)))
         (macro a 42 (print a)))｣))

#+nil ; TODO this test is too convoluted, check if it is valid at all...
(def xml+js-test test/js/macrolet/2 ()
  (with-expected-failures
    (3
     ｢(macrolet ((macro (properties)
                   `(progn
                      `js-inline(create "a"
                                        (create ,@(list ,@(iter (for (name value) :on properties :by #'cddr)
                                                                (collect `(quote ,name))
                                                                (collect (bind ((value value))
                                                                           ;; TODO this could work if `str was not in inline-emitting mode
                                                                           ;; or if list quasi quoting were used on the ` character (because
                                                                           ;; then this `str were not a toplevel qq, so it would not turn
                                                                           ;; into code that tries to emit stuff at macroexpand time)
                                                                           `str ,(princ-to-string (1+ value)))))))))))
        `js(print (slot-value (slot-value ,(macro ("a" 1 "b" 2 "c" 3))
                                          "a")
                              "b")))｣)))

(def macro test/js/complex-macros/test-macro (properties)
  {(lambda (reader)
     (with-local-readtable
       (setup-readtable-for-js-test :xml? #t :with-inline-emitting #t)
       (first (funcall reader))))
   `(progn
      `js-inline(slot-value
                 (slot-value
                  (create "a"
                          (create ,@,@(iter (for (name value) :on properties :by #'cddr)
                                            (collect `(quote ,name))
                                            (collect (bind ((value value))
                                                       `str ,(princ-to-string (1+ value)))))))
                  "a")
                 "b"))})

(def test test/js/complex-macros/1 ()
  (with-expected-failures
    (bind ((result (parse-xml-into-sxml
                    (emit/xml+js
                     ｢<div `js(print ,(test/js/complex-macros/test-macro ("a" 1 "b" 2 "c" 3)))>｣
                     #t))))
      (is (string= (first result) "div"))
      (bind ((js (third (third result))))
        (is (js-result-equal (eval-js js) 3))))))

(def test test/js/complex-macros/2 ()
  (with-expected-failures
    (is (js-result-equal
         (eval-js
          (emit/xml+js
           ｢(macrolet ((macro (properties)
                         ` `js-inline(slot-value
                                      (slot-value
                                       (create "x"
                                               (create ,@,@(iter (for (name value) :on properties :by #'cddr)
                                                                 (collect name)
                                                                 (collect (bind ((value value))
                                                                            `str ,(princ-to-string (1+ value)))))))
                                       "x")
                                      "b")))
              `js(print ,(macro ("a" 1 "b" 2 "c" 3))))｣))
         3))))

(def js-test test/js/defun/key ()
  (145
   ｢`js(progn
         (defun x (a &key (b 42) c d)
           (when (=== d undefined)
             (setf d 100))
           (return (+ a b c d)))
         (print (x 1 :c 2)))｣)
  (145
   ｢`js(progn
         (defun x (a &rest foo &key (b 42) c d)
           (when (== foo.c 2)
             (setf d 100))
           (return (+ a b c d)))
         (print (x 1 :c 2)))｣)
  (48
   ｢`js(flet ((fun (a &rest foo)
                (return ((lambda (a &rest foo &key (b 42) c d)
                           (return (+ a b c d)))
                         a foo))))
         (print (fun 1 :c 2 :d 3)))｣)
  (4
   ｢`js(flet ((fun (a &key (b 3))
                (return (+ a b))))
         (print (fun 1)))｣))

(def js-test test/js/defun/optional ()
  (with-expected-failures
    (43
     ｢`js(progn
           (defun x (a &optional (b 42) c)
             (return (+ a b c)))
           (print (x 1)))｣))
  (with-expected-failures
    (301
     ｢`js(progn
           (defun x (a &optional (b 42) c)
             (return (+ a b c)))
           (print (x 1 100 200)))｣)))

(def js-test test/js/apply ()
  (45
   ｢`js(progn
         (defun x (a &key (b 42) c)
           (return (+ a b c)))
         (print (apply x 1 :c 2)))｣))

(def js-test test/js/lambda ()
  ("foobarbaz"
   ｢`js(print
        ((lambda (fn x)
           (return (fn (+ x "bar"))))
         (lambda (x)
           (return (+ x "baz")))
         "foo"))｣))

(def js-test test/js/nesting-through-unquote ()
  (with-expected-failures
    (42
     ｢`js(let ((x 40))
           ,(when (> 3 2)
                  `js(incf x 2))
           (print x))｣)))

(def test test/js/escaping ()
  (let ((str "alma"))
    ;; return the input if there's no need for escaping
    (is (eq str (escape-as-js-string str)))))

(def js-test test/js/mixed-with-string-quasi-quote ()
  (42
   ｢`js(progn
         ;; this way you can inject untransformed (not even escaped) text into the js output
         `str("a = 22")
         (print (+ `str("a") 10 ,10)))｣))

(def test test/js/mixed-with-xml/simple ()
  (bind ((emitted (emit/xml+js ｢<body `js(+ 2 2)>｣))
         (body (parse-xml-into-sxml emitted)))
    (is (string= (first body) "body"))
    (bind ((script (third body)))
      (is (string= (first script) "script"))
      (is (stringp (third script)))
      (is (search "2 + 2" (third script))))))

(def test test/js/mixed-with-xml/escaping ()
  (bind ((emitted (emit/xml+js ｢<body `js-inline "&<>" >｣))
         (body (parse-xml-into-sxml emitted)))
    (is (string= (first body) "body"))
    (bind ((script (third body)))
      (is (stringp script))
      (is (search "&<>" script)))))

;; leave it at the end, because it screws up emacs coloring
(def js-test test/js/string-quoting ()
  ("alma"
   ｢`js(print "alma")｣)
  (｢al'm"a｣
   ｢`js(print "al'm\"a")｣))

#|
REPL demos

(bind ((code-as-string ｢<body
                          ,(string-upcase "some runtime generated <escaped> text")
                          `str("***<unescaped text 1>***")
                          ;; let's insert some JavaScript here, with some unquoted runtime part:
                          `js(print (+ 2 ,(+ 20 20))
                                `str("***<unescaped text 2>***"))>｣))
  (pprint/xml+js code-as-string t)
  (emit/xml+js code-as-string))

|#
