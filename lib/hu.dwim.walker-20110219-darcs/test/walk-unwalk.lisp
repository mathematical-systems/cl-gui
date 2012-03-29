;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.walker.test)

(defsuite* (test/walk-unwalk :in test))

(defvar *foo*)

(deftest test/walk-unwalk/special-variable-name? ()
  (is (special-variable-name? '*foo*)
      "Unbound special variables are not properly detected! It needs platform dependent support..."))

(deftest check-walk-unwalk (form &optional (expected form) env)
  (declare (optimize debug))
  (unless expected
    (setf expected form))
  (unless env
    (setf env (make-empty-lexical-environment)))
  (let ((walked-form (unwalk-form (walk-form form :environment (make-walk-environment env)))))
    (is (equal walked-form expected))))

(defmacro define-walk-unwalk-test (name &body body)
  `(deftest ,name ()
     (with-active-layers (ignore-undefined-references)
       ,@(loop
           :for entry :in body
           :collect (if (and (consp entry)
                             (eq (first entry) 'with-expected-failures))
                        `(with-expected-failures
                           ,@(mapcar (lambda (entry)
                                       `(check-walk-unwalk ',entry))
                                     (rest entry)))
                        `(check-walk-unwalk ',entry))))))

(define-walk-unwalk-test test/constant
  1 'a "a" (1 2 3) #(1 2 3))

(deftest test/constant/nil-and-t ()
  (is (typep (walk-form 't) 'constant-form))
  (is (typep (walk-form 'nil) 'constant-form)))

(define-walk-unwalk-test test/variable
  var
  :var)

(deftest test/variable/bug1 ()
  (let* ((walked (walk-form '(let ((v 0))
                              (declare (type integer v))
                              v)))
         (body (body-of walked)))
    (is (length= 1 body))
    (is (typep (first body) 'walked-lexical-variable-reference-form))))

(define-walk-unwalk-test test/application
  (* 2 3)
  (+ (* 3 3) (* 4 4)))

(define-walk-unwalk-test test/lambda-application
  ((lambda (x) (x x))
   #'(lambda (x) (x x)))
  ((lambda (x k) (k x))
   (if p x y)
   id))

(define-walk-unwalk-test test/lambda-list-parsing
  #'(lambda (x &aux auxfoo))
  #'(lambda (&key &allow-other-keys))
  #'(lambda (x &rest args &key key1 (key2 nil key2?) (key3 42) &allow-other-keys)))

(define-walk-unwalk-test test/eval-when/1
  (eval-when ()
    (+ 2 2))
  (eval-when (:load-toplevel)
    )
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (+ 3 3)))

(define-walk-unwalk-test test/declare/1
  (locally (declare (zork)))
  (locally (declare (optimize speed) (optimize (debug 2))))
  (locally (declare (ignorable a) (ignorable b)))
  (locally (declare (dynamic-extent a) (ignorable b))))

(deftest test/declare/2 ()
  (signals walker-style-warning
    (walk-form '(locally (declare (zork)))))
  (signals walker-style-warning
    (walk-form '(locally (declare (integer x)))))
  ;; declare is not allowed in a tagbody
  (signals error
    (walk-form '(tagbody
                   (declare (optimize speed))
                 :tag1 (+ 1 1)
                 :tag2 (+ 2 2))))
  (signals error
    (walk-form '(eval-when (:compile-toplevel :load-toplevel :execute)
                 (declare (optimize speed))))))

(deftest test/declare/3 ()
  (check-walk-unwalk
   '(lambda () (declare))
   '#'(lambda ()))
  (check-walk-unwalk
   '(macrolet ((x (&body body)
                `(locally
                     (declare (unknown abc))
                   ,@body)))
     (x 42))
   '(locally (locally (declare (unknown abc)) 42)))
  (bind ((x 'progn))
    (declare (special x))
    (check-walk-unwalk
     '(macrolet ((mac (&body body)
                  (declare (special x))
                  `(,x ,@body)))
       (mac 42 43))
     '(locally (progn 42 43))))
  (check-walk-unwalk
   '(lambda () (declare (ignorable)))
   '#'(lambda ())))

(deftest test/macro/1 ()
  (finishes
    (walk-form '(macrolet ((foo ((some-complex-args &optional (even-more-compelx 42)))
                            `(bar ,some-complex-args ,even-more-compelx)))
                 (foo (1 2)))
               :environment (make-walk-environment))))

(define-walk-unwalk-test test/lambda-function
  #'(lambda (x y) (y x))
  #'(lambda (x &key y z) (z (y x)))
  #'(lambda (&optional x y) (list x y))
  #'(lambda (&optional (x nil x-p)) x)
  #'(lambda (x &rest args) (apply x args))
  #'(lambda (object &key (a nil a?)) (values))
  #'(lambda (object &key a b &allow-other-keys) (values))
  #'(lambda (&optional x y &rest args &key a b &allow-other-keys) 42))

(deftest test/invalid-lambda-list ()
  (signals error
    (walk-form '(lambda (&rest args &key a b &optional x y &allow-other-keys) 42))))

(define-walk-unwalk-test test/walk-unwalk/block
  (block label (get-up) (eat-food) (go-to-sleep))
  (block label ((lambda (f x) (f (f x))) #'car))
  (block label (reachable) (return-from label 'done) (unreachable)))

(define-walk-unwalk-test test/walk-unwalk/catch
  (catch 'done (with-call/cc* (* 2 3)))
  (catch 'scheduler
    (tagbody start
       (funcall thunk)
       (if (done-p) (throw 'scheduler 'done) (go start))))
  (catch 'c
    (flet ((c1 () (throw 'c 1)))
      (catch 'c (c1) (print 'unreachable))
      2)))

(define-walk-unwalk-test test/walk-unwalk/if
  (if p x y)
  (if (pred x) (f x) (f-tail y #(1 2 3))))

(define-walk-unwalk-test test/walk-unwalk/flet
  (flet ((empty ())))
  (flet ((sq (x)
           (* x x)))
    (+ (sq 3) (sq 4)))
  (flet ((prline (s)
           (princ s)
           (terpri)))
    (prline "hello")
    (prline "world"))
  (flet ((foo ()
           (declare (optimize speed))
           1))
    (funcall #'foo)))

(deftest test/walk-unwalk/flet/errors ()
  (signals error
    (walk-form '(flet ((empty))))))

(define-walk-unwalk-test test/walk-unwalk/labels
  (labels ((empty ())))
  (labels ((fac-acc (n acc)
             (if (zerop n)
                 (land acc)
                 (bounce
                  (fac-acc (1- n) (* n acc))))))
    (fac-acc (fac-acc 10 1) 1))
  (labels ((evenp (n)
             (if (zerop n) t (oddp (1- n))))
           (oddp (n)
             (if (zerop n) nil (evenp (1- n)))))
    (oddp 666)))

(deftest test/walk-unwalk/labels/errors ()
  (signals error
    (walk-form '(labels ((empty))))))

(define-walk-unwalk-test test/walk-unwalk/let
  (let ((a 2) (b 3) (c 4))
    (+ (- a b) (- b c) (- c a)))
  (let ((a b) (b a)) (format t "side-effect~%") (f a b)))

(define-walk-unwalk-test test/walk-unwalk/let*
  (let* ((a (random 100)) (b (* a a))) (- b a))
  (let* ((a b) (b a)) (equal a b)))

(define-walk-unwalk-test test/walk-unwalk/load-time-value
  (load-time-value *load-pathname* #-ecl t))

(define-walk-unwalk-test test/walk-unwalk/locally
  (locally (setq *global* (whoops))))

(define-walk-unwalk-test test/walk-unwalk/multiple-value-call
  (multiple-value-call #'list 1 '/ (values 2 3) '/ (values) '/ (floor 2.5))
  (multiple-value-call #'+ (floor 5 3) (floor 19 4)))

(define-walk-unwalk-test test/walk-unwalk/multiple-value-prog1
  (multiple-value-prog1
      (values-list temp)
    (setq temp nil)
    (values-list temp)))

(define-walk-unwalk-test test/walk-unwalk/progn
  (progn (f a) (f-tail b) c)
  (progn #'(lambda (x) (x x)) 2 'a))

(define-walk-unwalk-test test/walk-unwalk/progv
  (progv '(*x*) '(2) *x*))

(define-walk-unwalk-test test/walk-unwalk/setq
  (setq x '(2 #(3 5 7) 11 "13" '17))
  (setq *global* 'symbol))

(define-walk-unwalk-test test/walk-unwalk/tagbody
  (tagbody
     (setq val 1)
     (go point-a)
     (setq val (+ val 16))
   point-c
     (setq val (+ val 4))
     (go point-b)
     (setq val (+ val 32))
   point-a
     (setq val (+ val 2))
     (go point-c)
     (setq val (+ val 64))
   point-b
     (setq val (+ val 8)))
  (tagbody
     (setq n (f2 flag #'(lambda () (go out))))
   out
     (prin1 n)))

(define-walk-unwalk-test test/walk-unwalk/the
  (the number (reverse "naoh"))
  (the string 1))

(define-walk-unwalk-test test/walk-unwalk/unwind-protect
  (unwind-protect
       (progn (setq count (+ count 1))
              (perform-access))
    (setq count (- count 1)))
  (unwind-protect
       (progn (with-call/cc* (walk-the-plank))
              (pushed-off-the-plank))
    (save-life)))

;;; bugs

(deftest test/walk-unwalk/macrolet-over-flet ()
  (check-walk-unwalk
   '(macrolet ((foo () 'bar))
     (flet ((foo ()))
       (foo)))
   ;; Used to be:
   ;; (locally
   ;;   (flet ((foo ()))
   ;;     bar))
   '(locally
     (flet ((foo ()))
       (foo)))))

(defmacro global-macro-foo ()
  'bar)

(deftest test/walk-unwalk/macro-over-flet ()
  (check-walk-unwalk
   '(flet ((global-macro-foo ()))
     (global-macro-foo))
   ;; Used to be:
   ;; (flet ((global-macro-foo ()))
   ;;   bar)
   '(flet ((global-macro-foo ()))
     (global-macro-foo))))

(deftest test/walk-unwalk/let-over-symbol-macrolet ()
  (check-walk-unwalk
   '(let (foo)
     (symbol-macrolet ((foo 'bar))
       foo))
   ;; Used to (wrongly) be the following before a bugfix:
   ;; (LET ((FOO NIL))
   ;;   (LOCALLY FOO))
   '(let ((foo ()))
     (locally 'bar))))

(deftest test/walk-unwalk/context-macrolets/1 ()
  (with-captured-lexical-environment
      (env (symbol-macrolet ((foo 'xxx))
             (symbol-macrolet ((foo 'bar))
               -here-)))
    (check-walk-unwalk 'foo ''bar env)))

(deftest test/walk-unwalk/context-macrolets/2 ()
  (with-captured-lexical-environment
      (env (let ((foo 'xxx))
             (declare (ignorable foo))
             (symbol-macrolet ((foo 'bar))
               -here-)))
    (check-walk-unwalk 'foo ''bar env)))

(deftest test/walk-unwalk/context-macrolets/3 ()
  (with-captured-lexical-environment
      (env (symbol-macrolet ((foo 'xxx))
             (let ((foo 'bar))
               (declare (ignorable foo))
               -here-)))
    (check-walk-unwalk 'foo 'foo env)))

(deftest test/walk-unwalk/context-macrolets/4 ()
  (with-captured-lexical-environment
      (env (macrolet ((foo () 'xxx))
             (macrolet ((foo () 'bar))
               -here-)))
    (check-walk-unwalk '(foo) 'bar env)))

(deftest test/walk-unwalk/context-macrolets/5 ()
  (with-captured-lexical-environment
      (env (flet ((foo () 'xxx))
             (macrolet ((foo () 'bar))
               -here-)))
    (check-walk-unwalk '(foo) 'bar env)))

(deftest test/walk-unwalk/context-macrolets/6 ()
  (with-captured-lexical-environment
      (env (macrolet ((foo () 'xxx))
             (flet ((foo () 'bar))
               -here-)))
    (check-walk-unwalk '(foo) '(foo) env)))

(define-walk-unwalk-test test/walk-unwalk/defun ()
  (defun foo (x) x)
  (defun foo (x) (declare (type fixnum x)) x)
  (defun foo (x) "foo" (declare (type fixnum x)) x))
