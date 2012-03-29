;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Miscellaneous small utility definitions by Fare (formerly fare.lisp)

#+xcvb
(module (:depends-on ("package" "base/utils" "base/strings" "base/symbols")))

(in-package :fare-utils)


;;; Help in defining macros

(def*macro with-gensyms (syms &body body)
  "Replaces given symbols with gensyms. Useful for creating macros.
This version by Paul Graham in On Lisp.
Mostly the same as cliki's WITH-UNIQUE-NAMES."
  ;; Note: we probably should be using it from alexandria or something
  `(let ,(mapcar #'(lambda (s) `(,s (gensym ,(symbol-name s)))) syms) ,@body))

(def*macro evaluating-once (vars &body body)
  "Macro to use while defining a macro that needs to enforce that the
evaluation some of its arguments happens but once. See also PCL::ONCE-ONLY,
Genera's SCL::ONCE-ONLY or CL-UTILITIES:ONCE-ONLY.
CMUCL's EXT:ONCE-ONLY has a different interface."
  (loop :for var :in vars :for sym = (gensym)
    :collect ``(,',sym ,,var) :into rt-bindings
    :collect `(,var ',sym) :into et-bindings
    :finally (return
               ``(let (,,@rt-bindings)
                   ,(let ,et-bindings ,@body)))))


; -----------------------------------------------------------------------------
;;; Defining forms

(def*macro define-abbrevs (&rest abbrevs)
  "Declare a series of abbreviations."
  `(progn ,@(loop :for (new old) :on abbrevs :by #'cddr :collect
              `(defmacro ,new (&rest rest) `(,',old ,@rest)))))

(def*macro defun-inline (name arglist &body body)
  "Declare an inline defun."
  `(progn (declaim (inline ,name))
	  (defun ,name ,arglist ,@body)))

(define-abbrevs defsubst defun-inline)



#| ;; not very useful
(def*macro define-enclosing-macro (name args before after &key unprotected)
  `(defmacro ,name (,@args &body body)
     `(progn
	,,before
	(,,(if unprotected ''prog1 ''unwind-protect)
	,@body
	,,after))))
|#


;; Simple modify-macro's
(defun xfuncall (x f &rest args) (apply f x args))
(exporting-definitions
(define-modify-macro funcallf (f &rest args) xfuncall)
(define-modify-macro appendf (&rest args) append "Append onto list")
(define-modify-macro nconcf (&rest args) nconc "Destructively append onto list")
(defun append1 (l x) (append l (list x)))
(define-modify-macro append1f (x) append1 "Append one element onto list"))

#|
;; The following is based on code by Tim Moore
;; Tim Moore <moore@bricoworks.com> on comp.lang.lisp 2001-11-03 01:51:05 GMT

;; DEFINE-MODIFY-MACRO can be written in portable ANSI CL
(defmacro dmm (name lambda-list function)
  `(defmacro ,name (place ,@lambda-list &environment env)
     (multiple-value-bind (vars vals store-vars writer-form reader-form)
	 (get-setf-expansion place env)
       `(let* (,@(mapcar #'list vars vals)
	       ,@store-vars)
	  (multiple-value-setq ,store-vars
	    (,',function ,reader-form ,,@lambda-list))
	  ,writer-form
	  (values ,@store-vars)))))
|#


(defmacro define-values-modify-macro (name val-vars lambda-list function)
  "Multiple-values variant on define-modify macro, by Tim Moore"
  (let ((env (gensym "ENV")))
    `(defmacro ,name (,@val-vars ,@lambda-list &environment ,env)
      (multiple-value-bind (vars vals store-vars writer-form reader-form)
          (get-setf-expansion `(values ,,@val-vars) ,env)
        (let ((val-temps (mapcar #'(lambda (temp) (gensym (symbol-name temp)))
                                 ',val-vars)))
          `(let* (,@(mapcar #'list vars vals)
                  ,@store-vars)
            (multiple-value-bind ,val-temps ,reader-form
              (multiple-value-setq ,store-vars
                (,',function ,@val-temps ,,@lambda-list)))
              ,writer-form
            (values ,@store-vars)))))))

(defmacro define-values-post-modify-macro (name val-vars lambda-list function)
  "Multiple-values variant on define-modify macro,
to yield pre-modification values"
  (let ((env (gensym "ENV")))
    `(defmacro ,name (,@val-vars ,@lambda-list &environment ,env)
       (multiple-value-bind (vars vals store-vars writer-form reader-form)
           (get-setf-expansion `(values ,,@val-vars) ,env)
         (let ((val-temps (mapcar #'(lambda (temp) (gensym (symbol-name temp)))
                                  ',val-vars)))
           `(let* (,@(mapcar #'list vars vals)
                   ,@store-vars)
              (multiple-value-bind ,val-temps ,reader-form
                (multiple-value-setq ,store-vars
                  (,',function ,@val-temps ,,@lambda-list))
		,writer-form
		(values ,@val-temps))))))))

(defmacro define-post-modify-macro (name lambda-list function)
  "Variant on define-modify-macro, to yield pre-modification values"
  `(define-values-post-modify-macro ,name (,(gensym)) ,lambda-list ,function))

(define-post-modify-macro post-incf () 1+)
(define-post-modify-macro post-decf () 1-)

(defmacro multiple-value-quote (&rest args)
  `(values ,@(mapcar #'(lambda (x) `',x) args)))

(define-abbrevs
  mvbind multiple-value-bind
  mvcall multiple-value-call
  mvlist multiple-value-list
  mvprog1 multiple-value-prog1
  mvsetq multiple-value-setq
  mvquote multiple-value-quote)

#|(eval-now
(defmacro let1 (var val &body body) `(let ((,var ,val)) ,@body)))|#

(defun symbol-macro-expansion (sym &optional env)
  "Check whether SYM is defined as a symbol-macro.
If it is, return its expansion and T.
If it isn't, return its expansion (=itself) and NIL.
May return a wrong result if SYM erroneously expands to itself,
which would cause an endless loop at macro-expansion time, anyway.
"
  (let ((exp (nth-value 1 (macroexpand-1 sym env))))
    (values exp (not (eq sym exp)))))

; -----------------------------------------------------------------------------
;;; Debugging

;;; Testing macros
(defmacro mac (expr)
  `(pprint (macroexpand-1 ',expr))) ; from On Lisp

;;; Simple testing macros
(defmacro test-only (&body body)
  #-do-test (declare (ignore body)) #-do-test nil
  #+do-test `(progn ,@body))

(defmacro if-testing (transform &rest rest)
  #-do-test (declare (ignore transform)) #-do-test `(progn ,@rest)
  #+do-test `(,@transform ,@rest))

(defmacro DBG-TIME (&body body)
  `(if-testing (time) ,@body))

;; usage: (TEST-FORM tested-form expected-value)
(defun report-testing-error (condition stream)
  (format stream "Form ~A returned ~A instead of ~A."
	  (testing-error-failed-form condition)
	  (testing-error-returned-value condition)
	  (testing-error-expected-value condition)))
(define-condition testing-error (error)
  ((failed-form
    :reader testing-error-failed-form
    :initarg :failed-form)
   (returned-value
    :reader testing-error-returned-value
    :initarg :returned-value)
   (expected-value
    :reader testing-error-expected-value
    :initarg :expected-value))
  (:report report-testing-error))
(defun test-form-helper (returned-value form-source expected-value)
  (unless (tree-equal returned-value expected-value)
    (error 'testing-error
	   :failed-form form-source
	   :expected-value expected-value
	   :returned-value returned-value)))
(defmacro do-test-form (tested-form expected-value)
  `(test-form-helper ,tested-form ',tested-form ,expected-value))
(defmacro TEST-FORM (tested-form expected-value)
  `(test-only (do-test-form ,tested-form ,expected-value)))
(defmacro TEST-FORMS (&rest test-pairs)
  `(test-only (progn ,@(mapcar
			#'(lambda (pair) `(TEST-FORM ,(car pair) ,(cdr pair)))
			(plist->alist test-pairs)))))
(defmacro TTEST (expr &key result (format "~A") time)
  `(progn
     (terpri) (terpri) (write ',expr)
     (let ((res ,(if time `(time ,expr) expr)))
       (format *error-output* ,(conc-string "~% => " format) res)
       ,(if result
	    `(if (equal res ,result)
		 (progn (format *error-output* " -- GOOD!~%") t)
	       (progn
		 (format *error-output*
			 ,(conc-string " whereas " format
				       " was expected. -- BAD!~%")
			 ,result)
		 nil))
	  t))))
(defmacro TTEST* (&rest clauses)
  `(every #'identity (list ,@(mapcar #'(lambda (x) `(TTEST ,@x)) clauses))))

;;; debug messages
(defun do-debug-message (format &rest args)
  "print a debugging message"
  (apply #'format t format args))
(defmacro debug-message (&rest args)
  `(test-only (do-debug-message ,@args)))
(defmacro MSG (&rest args)
  `(test-only (debug-message ,@args)))
(defmacro DBG (tag &rest exprs)
  "simple debug statement macro:
outputs a tag plus a list of variable and their values, returns the last value"
  ;"if not in debugging mode, just compute and return last value"
  ; #-do-test (declare (ignore tag)) #-do-test (car (last exprs)) #+do-test
  (let ((res (gensym))(f (gensym)))
  `(let (,res (*print-readably* nil))
    (flet ((,f (fmt &rest args) (apply #'format *error-output* fmt args)))
      (,f "~&~A~%" ,tag)
      ,@(mapcan
         #'(lambda (x)
            `((,f "~&  ~S => " ',x)
              (,f "~{~S~^ ~}~%" (setf ,res (multiple-value-list ,x)))))
         exprs)
      (apply 'values ,res)))))
(defun package-functions (package-designator)
  (loop :for s :being :each :present-symbol :of package-designator
    :when (and (fboundp s) (not (macro-function s))) :collect s))
(defun trace-package-functions (package-designator)
  (eval `(trace ,@(package-functions package-designator))))
(defun untrace-package-functions (package-designator)
  (eval `(untrace ,@(package-functions package-designator))))

(defmacro xtime ((&rest msg) &body body)
  `(prog1 (time (progn ,@body)) (format *error-output* ,@msg)))

;;; Disabling functions
(defun NOP (&rest rest)
  (declare (ignore rest))
  (values))
(defun disable-fun (fun)
  (setf (get fun 'disabled-fun) (symbol-function fun)
	(symbol-function fun) #'NOP)
  (values))
(defun enable-fun (fun)
  (setf (symbol-function fun)
	(get fun 'disabled-fun #'NOP))
  (values))


;;; Array functions
(defun copy-array-shape (array)
  "make a new array of same shape as given array"
  (make-array (array-dimensions array)
	      :element-type (array-element-type array)))
(defun copy-array (array)
  "make a fresh (shallow) copy of an array"
  (let ((new-array (copy-array-shape array)))
    (loop :for i :below (array-total-size array) :do
      (setf (row-major-aref new-array i) (row-major-aref array i)))
    new-array))
(defun fill-array (array value)
  "fill an array with a value"
  (fill
   (make-array (array-total-size array)
	       :element-type (array-element-type array)
	       :displaced-to array)
   value))
(defun vector->list (vector)
  (loop :for x :across vector :collect x))
(defun list->vector (list)
  (apply #'vector list))

;;; Streams

(defun n-stream-has-char-p (c s)
  (and (peek-char c s) (read-char s)))

(defun n-stream-eol-p (s)
  (let ((x (n-stream-has-char-p #\return s)))
    (or (n-stream-has-char-p #\linefeed s) x)))

; -----------------------------------------------------------------------------
;;; Higher-Order Functions
(defun compose/2 (f g)
  #'(lambda (&rest rest) (multiple-value-call f (apply g rest))))
(defun compose (&rest rest)
  (reduce #'compose/2 rest :from-end t :initial-value #'identity))

;;; Basic combinators
;; they call for some combinator-defining macros, that would handle
;; all the arity and funcalling cumbersomeness
(defun I (x) x)
(defun K (x) #'(lambda (y) (declare (ignore y)) x))
(defun S (f) #'(lambda (g) #'(lambda (x)
			       (funcall (funcall f x) (funcall g x)))))
(defun C (f) #'(lambda (g) #'(lambda (x) (funcall f (funcall g x)))))

; -----------------------------------------------------------------------------
;;; Control Structures
; inspired by Paul Graham's "On Lisp"

(defmacro aif (test then &optional else)
  `(let ((it ,test)) (if it ,then ,else)))
(with-gensyms (result test)
(defmacro if2 (test2 then &optional else)
  `(mvbind (,result ,test) ,test2
     (if (or ,test ,result) ,then ,else)))
(defmacro aif2 (test2 then &optional else)
  `(mvbind (it ,test) ,test2
     (if (or ,test it) ,then ,else))))

(defun bindable-symbol-p (x)
  (and (symbolp x)
       (not (eq (symbol-package x) (load-time-value (find-package :common-lisp))))
       (not (keywordp x))))
(defun normalize-optional-binding (x)
  (etypecase x
    (symbol
	(assert (bindable-symbol-p x))
	(list x nil nil))
    (list
	(assert (and (bindable-symbol-p (car x))
		     (null (cdddr x))
		     (or (null (cddr x)) (bindable-symbol-p (caddr x)))))
	(list (car x) (cadr x) (caddr x)))))
(defun optional-binding-symbol (x)
   (car (normalize-optional-binding x)))
(defun first-binding (vars)
  (assert (listp vars))
  (let ((x (car vars)))
     (assert (symbolp x))
     (cond
       ((bindable-symbol-p x) x)
       ((eq x '&rest)
          (let ((y (cadr vars)))
	    (assert (bindable-symbol-p y))
	    `(car ,y)))
       ((eq x '&optional)
	  (optional-binding-symbol (cadr vars)))
       (t (error "unable to process lambda list")))))
(defmacro if-bind (bindings test then &optional else)
  `(multiple-value-bind ,bindings ,test
      (if ,(first-binding bindings) ,then ,else)))
(defmacro when-bind (bindings test &body body)
  `(multiple-value-bind ,bindings ,test
      (when ,(first-binding bindings) ,@body)))

(defmacro defxcond (name ifform)
  `(defmacro ,name (&rest clauses)
     (when clauses
       (let* ((cl1 (car clauses))
	      (cl* (cdr clauses))
	      (test (car cl1))
	      (then (cdr cl1)))
	 `(,',ifform ,test (progn ,@then) (,',name ,@cl*))))))

(defxcond acond aif)
(defxcond cond2 if2)
(defxcond acond2 aif2)

;;; Macro-defining macros
(eval-now
(defmacro mapmacro (fun forms &optional (head '(progn)) (tail '()))
  `(,@head ,@(mapcar #'(lambda (x) `(,fun ,x)) forms) ,@tail))

(defmacro propmacro (name)
  `(defmacro ,name (obj) `(get ,obj ',',name)))
;(defmacro propmacros (&rest names)
;  `(progn ,@(mapcar #'(lambda (x) `(propmacro ,x)) names)))
(defmacro propmacros (&rest names)
  `(mapmacro propmacro ,names))
)
; -----------------------------------------------------------------------------
;;; Manipulating Source

(defun error-behaviour (e &rest r)
  "generic way to specify behaviour in exceptional situations"
  (cond
   ((functionp e) (apply e r))
   ((stringp e) (apply #'error e r))
   (t e)))

(defun form-starting-with-p (tag x)
  (and (consp x) (equal tag (car x))))

(defun single-arg-form-p (tag x &optional on-error)
  "checks whether X is an instance of a something made with a maker
defined by (MAKE-SINGLE-ARG-FORM TAG). If ON-ERROR is defined, and
X looks like it is such an instance but is malformed, then ON-ERROR
is invoked as an ERROR-BEHAVIOUR."
  (and (form-starting-with-p tag x)
       (or (and (consp (cdr x)) (null (cddr x)))
	   (error-behaviour on-error tag x))))

(defun proper-list-p (x)
  "Returns T if X is a proper list, NIL if it isn't. Checks for circularity"
  (labels
      ((ret (b)
         (return-from proper-list-p b))
       (check (x y)
         (cond
           ((null x) (ret t))
           ((eq x y) (ret nil))
           ((not (consp x)) (ret nil))))
       (recurse (x y)
         (check x y)
         (check (cdr x) y)
         (recurse (cddr x) (cdr y))))
    (check x nil)
    (recurse (cdr x) x)))

(defun single-arg (x) (cadr x))
(defmacro make-single-arg-form (name &optional
				     (maker (conc-symbol :make- name))
				     (recognizer (make-predicate-symbol name))
				     (tag name))
  `(progn
     (defun ,maker (x) (list ',tag x))
     (defun ,recognizer (x &optional on-error)
       (single-arg-form-p ',tag x on-error))))
(make-single-arg-form quote kwote)
(defun combinationp (x &optional on-error)
  "Is the form X a combination of a head and arguments,
to be evaluated as function call, macro call, or special form?"
  (and (consp x)
       (if on-error
	   (or (proper-list-p x) (error-behaviour on-error x))
	 t)))
(defun literalp (x)
  "predicate that tells whether X is the source form for a literal expression."
  (typep x '(or boolean number character array keyword)))

(defmacro copy-symbol-value (&rest l)
  `(progn ,@(mapcar #'(lambda (x)
			`(setf (symbol-value ',(car x))
			       (symbol-value ',(cdr x))))
		    (plist->alist l))))
(defmacro copy-symbol-function (&rest l)
  `(progn ,@(mapcar #'(lambda (x)
			`(setf (symbol-function ',(car x))
			       (symbol-function ',(cdr x))))
		    (plist->alist l))))

;; "This variant of DEFCONSTANT avoids problems with being evaluated multiple
;; times with an expression that doesn't yield EQ values across evaluations."
(def*macro defconstant* (name value &optional doc)
 `(defconstant ,name
    (if (boundp ',name) (symbol-value ',name) ,value)
    ,@(when doc (list doc))))



(defmacro eval-once ((&optional guard) &body body)
  "EVAL-ONCE will evaluate its BODY only once,
as determined by a guard variable GUARD.
A proper guard variable is generated in the current package
if none is specified.
A proper guard variable is generated in the designated package
if a string or keyword is specified.

The test for a previous evaluation is robust except for
the case when two different forms with bodies that have the same SXHASH
from two different files are compiled in two different sessions
with clashing GENTEMP counters then loaded into a same session.
The hashing technique should be fairly safe on most good implementations,
but then again might not be safe enough on your implementation.
A use-case that guarantees EVAL-ONCE to work in all implementations
is that all files using EVAL-ONCE with guards generated in a same package
shall be declared with a serial dependency in system definitions.
"
  (let ((package *package*)
	(hash (sxhash body)))
    (when (typep guard '(or keyword string package))
      (setf package (or (find-package guard) (error "no package ~A" guard))
	    guard nil))
    (unless guard
      (setf guard (gentemp (format nil "*EOG*~36R-" hash) package)
	    hash t))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
      (defvar ,guard nil)
      (unless (eql ,guard ,hash)
	(prog1
	    (macrolet ((eval-once-guard () '',guard))
	      ,@body)
	  (setf ,guard ,hash))))))


;;; Nesting binding forms (from a suggestion by marco baringer)
(defmacro with-nesting ((#-ccl &optional) &rest things)
  (reduce #'(lambda (outer inner) (append outer (list inner)))
          things :from-end t))

;;; Collecting data

(defmacro while-collecting ((&rest collectors) &body body)
  (let ((vars (mapcar #'(lambda (x) (gensym (symbol-name x))) collectors))
        (initial-values (mapcar (constantly nil) collectors)))
    `(let ,(mapcar #'list vars initial-values)
       (flet ,(mapcar #'(lambda (c v) `(,c (x) (push x ,v))) collectors vars)
         ,@body
         (values ,@(mapcar #'(lambda (v) `(nreverse ,v)) vars))))))

(defmacro fluid-let* (bindings &body body)
  (cond
    (bindings
     (assert (length=-p bindings 2))
     `(fluid-let1
       (,(caar bindings) ,(cadar bindings))
       (fluid-let* ,(cdr bindings) ,@body body)))
    (t
     `(progn ,@body))))

(defmacro fluid-let1 ((place val) &body body)
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion place)
    (with-gensyms (oldvals)
      `(let (,@(mapcar 'list vars vals) ,@store-vars)
         (let ((,oldvals (multiple-value-list ,reader-form)))
           (unwind-protect
                (progn
                  (multiple-value-setq ,store-vars ,val)
                  ,writer-form
                  ,@body)
             (multiple-value-setq ,store-vars (apply #'values ,oldvals))
             ,writer-form))))))
