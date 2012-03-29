;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.quasi-quote.js)

(def special-variable *js-indent-level* 0)
(def special-variable *in-js-statement-context* #t)
(def special-variable *js-operator-precedence*)

(def function make-indent ()
  (awhen (indentation-width-of *transformation*)
    (list (make-string-of-spaces (* it *js-indent-level*)))))

(def function make-newline-and-indent ()
  (list* #\Newline (make-indent)))

(def with-macro with-increased-indent* (really?)
  (if really?
      (bind ((*js-indent-level* (1+ *js-indent-level*)))
        (-body-))
      (-body-)))

(def with-macro with-increased-indent ()
  (with-increased-indent* #t
    (-body-)))

(def function wrap-runtime-delayed-js-transformation-form (form)
  (wrap-runtime-delayed-transformation-form
   (wrap-forms-with-bindings (append `((*in-js-statement-context* ,*in-js-statement-context*)
                                       (*js-operator-precedence* ,*js-operator-precedence*))
                                     (when (indentation-width-of *transformation*)
                                       `((*js-indent-level* (+ *js-indent-level* ,*js-indent-level*)))))
                             form)))

(def (function oe) hyphened-to-camel-case (input)
  (declare (type string input))
  (bind ((pieces (cl-ppcre:split "-" input)))
    (if (rest pieces)
        (bind ((*print-pretty* #f))
          (with-output-to-string (str)
            (iter (for piece :in pieces)
                  (write-string (if (first-time-p)
                                    piece
                                    (capitalize-first-letter! piece))
                                str)
                  (collect piece))))
        input)))

(def (function oe) lisp-name-to-js-name (symbol &key operator)
  (etypecase symbol
    (js-unquote
     (make-string-unquote (wrap-runtime-delayed-js-transformation-form
                           `(lisp-name-to-js-name ,(form-of symbol)))))
    (symbol
     (if (and (not operator)
              (gethash (string-downcase symbol) *js-reserved-keywords*))
         (bind ((name (symbol-name symbol)))
           (concatenate 'string name (princ-to-string (mod (sxhash name) 10000))))
         (bind ((name (copy-seq (symbol-name symbol))))
           ;; NOTE: be careful extending the set of replaced characters: some js operators like /= go through here currently
           (nsubstitute #\_ #\? name)
           (hyphened-to-camel-case name))))))

(def function to-js-operator-name (name)
  (lisp-name-to-js-name (lisp-operator-name-to-js-operator-name name) :operator #t))

(def macro with-root-operator-precedence (&body body)
  `(bind ((*js-operator-precedence* most-positive-fixnum))
     ,@body))

(def macro with-operator-precedence (operator &body body)
  `(bind ((*js-operator-precedence* ,(if (or (numberp operator)
                                             (consp operator))
                                         (if (eq 'quote (first operator))
                                             (aprog1
                                                 (operator-precedence (lisp-operator-name-to-js-operator-name (second operator)))
                                               (assert it () "~S is not a valid JS operator name" operator))
                                             operator)
                                         `(aprog1
                                              (operator-precedence (lisp-operator-name-to-js-operator-name ,operator))
                                            (assert it () "~S is not a valid JS operator name" ,operator)))))
     ,@body))

(def macro with-wrapping-based-on-operator-precedence (operator &body body)
  (with-unique-names (needs-parens? result parent-operator-precedence)
    `(bind ((,parent-operator-precedence *js-operator-precedence*))
       (with-operator-precedence ,operator
         (bind ((,needs-parens? (> *js-operator-precedence* ,parent-operator-precedence))
                (,result (progn
                           ;; (format *debug-io* "wrapping at operator: ~S, precedence ~A -> ~A~%" ',operator ,parent-operator-precedence *js-operator-precedence*)
                           ,@body)))
           (if ,needs-parens?
               `("(" ,@,result ")")
               ,result))))))

(def (function oe) to-js-literal (value)
  (etypecase value
    (string (concatenate 'string "'" (escape-as-js-string value) "'"))
    (integer (princ-to-string value))
    (float (format nil "~F" value))
    (ratio (concatenate 'string "(" (princ-to-string (numerator value)) " / " (princ-to-string (denominator value)) ")"))
    (character (concatenate 'string "'" (escape-as-js-string (string value)) "'"))
    (symbol (bind ((js-value (gethash value *js-literals*)))
              (assert js-value () "~S is not a valid js literal" value)
              js-value))
    (vector `("["
              ,@(iter (for element :in-vector value)
                      (unless (first-time-p)
                        (collect ", "))
                      (collect (to-js-literal element)))
              "]"))
    (js-unquote (make-string-unquote
                 (wrap-runtime-delayed-js-transformation-form
                  `(to-js-literal ,(form-of value)))))))

(def (function ioe) to-js-boolean (value)
  (if value "true" "false"))

(def definer transform-function (name args &body body)
  `(def function ,name ,args
     (with-lexical-transform-functions
       ,@body)))

(def transform-function transform-incf-like (node plus-plus plus-equal)
  (bind ((arguments (arguments-of node)))
    (ecase (length arguments)
      (1 (with-wrapping-based-on-operator-precedence '=
           `(,(recurse (first arguments)) " = " ,plus-plus ,(recurse (first arguments)))))
      (2 (with-wrapping-based-on-operator-precedence '+=
           `(,(recurse (first arguments)) " " ,plus-equal " " ,(recurse (second arguments))))))))

(def transform-function transform-vector-like (node)
  `(#\[
    ,@(iter (for argument :in (arguments-of node))
            (unless (first-time-p)
              (collect ", "))
            (collect (recurse argument)))
    #\]))

(def transform-function transform-map-like (node &key destructively-into)
  (not-yet-implemented "the js 'map operator is rather buggy, consider using wui.map")
  (bind ((arguments (arguments-of node))
         (fn (pop arguments))
         (fn-processed (cond
                         ((typep fn 'walked-lexical-variable-reference-form)
                          (lisp-name-to-js-name (name-of fn)))
                         ((and (typep fn 'constant-form)
                               (symbolp (value-of fn))
                               (not (js-literal-name? (value-of fn))))
                          (lisp-name-to-js-name (value-of fn)))
                         (t (recurse fn))))
         (sequence (pop arguments))
         (idx-var (unique-js-name "_idx"))
         (array-var (unique-js-name "_src"))
         (result nil))
    (when arguments
      (js-compile-error node "TODO: js compiler doesn't support iterating multiple sequences using map constructs yet"))
    (when (eq :inplace destructively-into)
      (setf destructively-into array-var))
    (when (and (not *in-js-statement-context*)
               (not destructively-into))
      (setf destructively-into (unique-js-name "_tgt"))
      (setf result (list (format nil "var ~A = [];~%" destructively-into))))
    (bind ((result `(,(format nil "var ~A = " array-var)
                     ,(recurse sequence)
                     ,(format nil ";~%for (~A = 0; ~A < ~A.length; ~A++) {~%" idx-var idx-var array-var idx-var)
                     ,@(when destructively-into
                         `(,array-var "[" ,idx-var "] = "))
                     "("
                     ,fn-processed
                     ,(format nil ")(~A[~A])~%}" array-var idx-var))))
      (if *in-js-statement-context*
          result
          `("(function () { "
            ,result
            ,(format nil "; return ~A; })()" destructively-into))))))

(macrolet ((frob (&body entries)
             `(progn
                ,@(iter (for (name-spec . body) :in entries)
                        (dolist (name (ensure-list name-spec))
                          (collect `(def js-special-form ,name
                                        ,@body)))))))
  (frob
   (|null|   (bind ((arguments (arguments-of -node-)))
               (assert (length= 1 arguments))
               (with-wrapping-based-on-operator-precedence '==
                 `(,(recurse (first arguments)) " == null"))))
   (|incf|   (transform-incf-like -node- "++" "+="))
   (|decf|   (transform-incf-like -node- "--" "-="))
   (|vector| (transform-vector-like -node-))
   (|list|   (transform-vector-like -node-))
   (|map|    (transform-map-like -node- :destructively-into :inplace))
   ;; TODO i think the with syntax ought to be a full AST node...
   (|with|   (bind ((arguments (arguments-of -node-)))
               `("with (" ,(recurse (first arguments)) ") " ,(transform-statements (rest arguments) :wrap? #t))))
   ;; KLUDGE need to handle 'not' specially, because the one at application-form can only handle infix operators for now
   (|not|    (unless (length= 1 (arguments-of -node-))
               (js-compile-error -node- "The 'not' operator expects exactly one argument!"))
             (with-wrapping-based-on-operator-precedence 'not
               `("!" ,(recurse (first (arguments-of -node-))))))
   (|aref|   (bind ((arguments (arguments-of -node-)))
               (unless (rest arguments)
                 (js-compile-error -node- "The 'aref' operator needs at least two arguments!"))
               (with-wrapping-based-on-operator-precedence 'member
                 `(,(recurse (first arguments))
                   ,@(iter (for argument :in (rest arguments))
                           (collect `(#\[
                                      ,(recurse argument)
                                      #\])))))))
   (|elt|    (bind ((arguments (arguments-of -node-)))
               (unless (length= 2 arguments)
                 (js-compile-error -node- "An elt operator with ~A arguments?" (length arguments)))
               (with-wrapping-based-on-operator-precedence 'member
                 `(,(recurse (first arguments))
                   #\[
                   ,(recurse (second arguments))
                   #\]))))
   ((1+ 1-)  (bind ((arguments (arguments-of -node-))
                    (argument (first arguments))
                    (operator (operator-of -node-)))
               (unless (length= 1 arguments)
                 (js-compile-error -node- "More than one argument to ~S?" operator))
               (with-wrapping-based-on-operator-precedence '+
                 (ecase operator
                   (1+ `("1 + " ,(recurse argument)))
                   (1- `("1 + " ,(recurse argument)))))))))

(def transform-function transform-quasi-quoted-js-to-quasi-quoted-string/array-elements (elements)
  (iter (for element :in elements)
        (unless (first-iteration-p)
          (collect ", "))
        (collect (recurse element))))

(def (function i) in-toplevel-js-block? ()
  ;; TODO ?
  #f)

(def transform-function statement-prefix-generator/lexical-variable-binder-form (node)
  (iter (for binding :in (bindings-of node))
        (for name = (name-of binding))
        (when name
          (collect (with-root-operator-precedence
                     `(#\Newline ,@(make-indent) "var " ,(lisp-name-to-js-name name) " = " ,(recurse (initial-value-of binding)) ";"))))))

(def transform-function transform-statements (thing &key (wrap? nil wrap-provided?) prefix-statements)
  (with-root-operator-precedence
    (bind ((node (labels ((drop-progns (node)
                            (typecase node
                              (progn-form (bind ((statements (hu.dwim.walker:body-of node)))
                                            (if (and (length= 1 statements)
                                                     (typep (first statements) 'implicit-progn-mixin)) ; don't strip the last progn
                                                (drop-progns (first statements))
                                                node)))
                              (t node))))
                   ;; skip the progn when it's not needed to avoid double {} wrapping
                   (drop-progns thing)))
           (*in-js-statement-context* #t)
           (statement-prefix-generator (constantly nil))
           (statements (etypecase node
                         (lexical-variable-binder-form
                          (setf statement-prefix-generator (lambda ()
                                                             (statement-prefix-generator/lexical-variable-binder-form node)))
                          (setf wrap? (not (in-toplevel-js-block?)))
                          (setf wrap-provided? #t)
                          (hu.dwim.walker:body-of node))
                         (implicit-progn-mixin
                          (hu.dwim.walker:body-of node))
                         (list
                          node))))
      (unless wrap-provided?
        (setf wrap? (and (rest statements)
                         (not (in-toplevel-js-block?)))))
      (append
       (when wrap?
         `(#\Newline ,@(make-indent) "{"))
       (with-increased-indent
         (append
          prefix-statements
          (funcall statement-prefix-generator)
          (iter (for statement :in statements)
                (collect #\Newline)
                (awhen (make-indent)
                  (collect it))
                ;; don't use RECURSE, because it rebinds *in-js-statement-context* to #f
                (collect (transform-quasi-quoted-js-to-quasi-quoted-string statement))
                (when (requres-semicolon-postfix? statement)
                  (collect #\;)))))
       (when wrap?
         `(#\Newline ,@(make-indent) "}"))))))

(def function requres-semicolon-postfix? (statement)
  (not (typep statement '(or if-form try-form))))

(def function source-of-parent (node)
  (source-of (hu.dwim.walker:parent-of node)))

(def transform-function transform-quasi-quoted-js-to-quasi-quoted-string/lambda-arguments-with-body (node)
  (bind ((arguments (bindings-of node))
         (rest-arg-name (aif (find-if (of-type 'rest-function-argument-form) arguments)
                             (lisp-name-to-js-name (name-of it))
                             "_kArgs"))
         (rest-has-been-emitted? #f)
         (transformed-arguments ())
         (transformed-body-prefix ()))
    (with-operator-precedence 'comma
      (macrolet ((%iterate-arguments (collect-commas? &body body)
                   `(iter (while arguments)
                          (for argument = (pop arguments))
                          (for previous-argument :previous argument)
                          ,(when collect-commas?
                             `(unless (or (first-time-p)
                                          (typep previous-argument 'rest-function-argument-form))
                                (push ", " transformed-arguments)))
                          (etypecase argument
                            ,@body)))
                 (iterate-arguments (&body body)
                   `(%iterate-arguments #t ,@body))
                 (iterate-arguments-without-commas (&body body)
                   `(%iterate-arguments #f ,@body)))
        (labels ((process-required-arguments ()
                   (iterate-arguments
                    (required-function-argument-form (push (lisp-name-to-js-name (name-of argument)) transformed-arguments))
                    (optional-function-argument-form (push argument arguments)
                                                     (process-first-optional-argument))
                    (rest-function-argument-form (push (lisp-name-to-js-name (name-of argument)) transformed-arguments)
                                                 (setf rest-has-been-emitted? #t))
                    (keyword-function-argument-form (push argument arguments)
                                                    (process-first-keyword-argument))))
                 (register-optional-parser (argument)
                   (declare (ignore argument))
                   (not-yet-implemented))
                 (register-keyword-parser (argument)
                   (bind ((default-value (default-value-of argument))
                          (js-name (lisp-name-to-js-name (name-of argument)))
                          (js-keyword-name (lisp-name-to-js-name (effective-keyword-name-of argument))))
                     (push (apply #'concatenate 'string
                                  (append* " var " js-name "="
                                           (bind ((accessor (concatenate 'string rest-arg-name "[\"" js-keyword-name "\"]")))
                                             (if default-value
                                                 (list accessor " ? " accessor " : " (recurse default-value))
                                                 accessor))
                                           ";"))
                           transformed-body-prefix)))
                 (process-first-keyword-argument ()
                   (iterate-arguments
                    (keyword-function-argument-form (unless rest-has-been-emitted?
                                                      (push rest-arg-name transformed-arguments))
                                                    (push (format nil "if (!~A) ~A = new Object;~%" rest-arg-name rest-arg-name)
                                                          transformed-body-prefix)
                                                    (register-keyword-parser argument)
                                                    (process-keyword-arguments))))
                 (process-keyword-arguments ()
                   (iterate-arguments-without-commas
                    (keyword-function-argument-form (register-keyword-parser argument))))
                 (process-first-optional-argument ()
                   (iterate-arguments
                    (optional-function-argument-form (register-optional-parser argument)
                                                     (process-optional-arguments))))
                 (process-optional-arguments ()
                   (iterate-arguments-without-commas
                    (optional-function-argument-form (register-optional-parser argument)))))
          (process-required-arguments))
        `("("
          ,@(nreverse transformed-arguments)
          ")"
          ,@(transform-statements node :wrap? #t :prefix-statements (nreverse transformed-body-prefix)))))))

(def transform-function transform-quasi-quoted-js-to-quasi-quoted-string/application-arguments (arguments)
  (with-operator-precedence 'comma
    (macrolet ((iterate-arguments (&body body)
                 `(iter (while arguments)
                        (for argument = (pop arguments))
                        (unless (first-time-p)
                          (collect ", "))
                        (typecase argument
                          ,@body))))
      (labels ((process-required-arguments ()
                 (iterate-arguments
                  (free-variable-reference-form (bind ((name (name-of argument)))
                                                  (assert (not (keywordp name)))
                                                  (collect (recurse argument))))
                  (constant-form (bind ((value (value-of argument)))
                                   (if (keywordp value)
                                       (progn
                                         (push argument arguments)
                                         (nconcing (process-keyword-arguments)))
                                       (collect (recurse argument)))))
                  (t (collect (recurse argument)))))
               (process-keyword-arguments ()
                 `("{"
                   ,@(with-increased-indent
                      (iter (for name = (pop arguments))
                            (for argument = (pop arguments))
                            (while name)
                            (unless (and (typep name 'constant-form)
                                         (keywordp (value-of name)))
                              (js-compile-error (hu.dwim.walker:parent-of argument) "Don't know what to do with ~S at a &key name position" name))
                            (unless argument
                              (js-compile-error (hu.dwim.walker:parent-of argument) "Odd number of &key args for js function application form ~S" (source-of-parent argument)))
                            (unless (first-time-p)
                              (collect ", "))
                            (collect (lisp-name-to-js-name (value-of name)))
                            (collect ": ")
                            (collect (recurse argument))))
                   "}")))
        (process-required-arguments)))))

(def function transform-quasi-quoted-js-to-quasi-quoted-string/create-form/name-value-pairs (input-elements)
  (iter (with indent = `(#\, #\Newline ,@(make-indent)))
        (with elements = input-elements)

        (for name = (pop elements))
        (for name-is-a-spliced-unquote = (and (typep name 'js-unquote)
                                              (spliced? name)))
        (while name)
        (unless (first-time-p)
          ;; FIXME: this here will emit a comma even if a spliced unquote follows that at the end doesn't splice anything (breaks ie iirc)
          (collect indent))
        (collect (transform-quasi-quoted-js-to-quasi-quoted-string/create-form/name name))
        (if name-is-a-spliced-unquote
            (when elements
              (js-compile-error nil "Unexpected element(s) after a spliced unquote in a create form: ~S" elements))
            (collect ": "))
        (for value = (pop elements))
        (unless name-is-a-spliced-unquote
          (collect (transform-quasi-quoted-js-to-quasi-quoted-string/create-form/value value)))))

(def function transform-quasi-quoted-js-to-quasi-quoted-string/create-form/name (name)
  (typecase name
    (string        name)
    (symbol        (lisp-name-to-js-name name))
    (constant-form (transform-quasi-quoted-js-to-quasi-quoted-string/create-form/name (value-of name)))
    (variable-reference-form (transform-quasi-quoted-js-to-quasi-quoted-string/create-form/name (name-of name)))
    (js-unquote    (make-string-unquote
                    (wrap-runtime-delayed-js-transformation-form
                     (if (spliced? name)
                         `(transform-quasi-quoted-js-to-quasi-quoted-string/create-form/name-value-pairs ,(form-of name))
                         `(transform-quasi-quoted-js-to-quasi-quoted-string/create-form/name             ,(form-of name))))))
    (t (transform-quasi-quoted-js-to-quasi-quoted-string name))))

(def transform-function transform-quasi-quoted-js-to-quasi-quoted-string/create-form/value (value)
  (typecase value
    (symbol        (recurse value))
    (constant-form (transform-quasi-quoted-js-to-quasi-quoted-string/create-form/value (value-of value)))
    (js-unquote    (if (spliced? value)
                       (js-compile-error nil "Spliced unquoting is not supported at value position in create forms")
                       (make-string-unquote
                        (wrap-runtime-delayed-js-transformation-form
                         `(transform-quasi-quoted-js-to-quasi-quoted-string/create-form/value ,(form-of value))))))
    (t (transform-quasi-quoted-js-to-quasi-quoted-string value))))

(def transform-function emit-lambda-application-form (node)
  (bind ((operator (operator-of node))
         (arguments (arguments-of node)))
    (assert (typep operator '(or lambda-function-form application-form)))
    `("(" ,(recurse operator) ")" "(" ,@(transform-quasi-quoted-js-to-quasi-quoted-string/application-arguments arguments) ")")))

(def function drop-progn-wrapper-around-single-expression (node)
  (if (and (typep node 'progn-form)
           (length= 1 (hu.dwim.walker:body-of node)))
      (first-elt (hu.dwim.walker:body-of node))
      node))

(macrolet ((frob (&rest entries)
             `(with-lexical-transform-functions
                (def generic transform-quasi-quoted-js-to-quasi-quoted-string* (form)
                  ,@(iter (for (type . body) :in entries)
                          (collect `(:method ((-node- ,type))
                                      ,@body)))))))
  (frob
   (variable-reference-form
    (lisp-name-to-js-name (name-of -node-)))
   (progn-form
    (bind ((statements (hu.dwim.walker:body-of -node-)))
      (if (and (length= 1 statements)
               (typep (first statements) '(or lexical-variable-binder-form function-binding-form)))
          ;; skip the progn when it's not needed to avoid double {} wrapping
          (recurse (first statements))
          (transform-statements -node-))))
   (if-form
    (bind ((condition (condition-of -node-))
           (then (drop-progn-wrapper-around-single-expression (then-of -node-)))
           (else (drop-progn-wrapper-around-single-expression (else-of -node-))))
      (when (and (typep else 'constant-form)
                 (eq nil (value-of else)))
        (setf else nil))
      (if *in-js-statement-context*
          (flet ((transform-if-block (node)
                   (typecase node
                     (implicit-progn-mixin
                      (transform-statements node))
                     ((or if-form try-form for-form)
                      (with-increased-indent
                        `(#\Newline
                          ,@(make-indent)
                          ;; don't use RECURSE here, because it rebinds *in-js-statement-context* to #f
                          ,(transform-quasi-quoted-js-to-quasi-quoted-string node))))
                     (t
                      (with-increased-indent
                        `(#\Newline
                          ,@(make-indent)
                          ;; don't use RECURSE here, because it rebinds *in-js-statement-context* to #f
                          ,(transform-quasi-quoted-js-to-quasi-quoted-string node)
                          ,(when (requres-semicolon-postfix? node)
                             #\;)))))))
            `("if (" ,(recurse condition) ")"
                     ,@(transform-if-block then)
                     ,@(if else
                           `(#\Newline ,@(make-indent) "else"
                                       ,@(transform-if-block else)))))
          (with-wrapping-based-on-operator-precedence 'conditional
            (when (or (typep then 'progn-form)
                      (typep else 'progn-form))
              (js-compile-error -node- "if's may not have multiple statements in their then/else branch when they are used in expression context"))
            `(,(recurse condition) " ? "
                  ,(recurse then)
               " : "
               ,(if else
                    (recurse else)
                    "undefined"))))))
   (lambda-application-form
    (emit-lambda-application-form -node-))
   (lambda-function-form
    `("function " ,@(transform-quasi-quoted-js-to-quasi-quoted-string/lambda-arguments-with-body -node-)))
   (application-form
    (bind ((arguments (arguments-of -node-))
           (operator (operator-of -node-)))
      (cond
        ((eq operator '|apply|)
         (bind ((argument-count (length arguments)))
           `(,(recurse (first arguments))
              ".apply(this, "
              ,@(if (length= 2 argument-count)
                    (list (recurse (elt arguments (1- argument-count))))
                    `("["
                      ,@(transform-quasi-quoted-js-to-quasi-quoted-string/application-arguments (subseq arguments 1 (1- argument-count)))
                      "].concat("
                      ,(recurse (elt arguments (1- argument-count)))
                      ")"))
              ")")))
        ((typep operator 'application-form)
         (emit-lambda-application-form -node-))
        ((typep operator 'walked-lexical-function-object-form)
         (with-operator-precedence 'comma
           `(,(recurse operator) "(" ,@(transform-quasi-quoted-js-to-quasi-quoted-string/application-arguments arguments) ")")))
        ((js-special-form? operator)
         (bind ((handler (gethash operator *js-special-forms*)))
           (funcall handler -node-)))
        (t
         ;; KLUDGE this is lame here, operators should not be handled by the same code as application.
         (with-wrapping-based-on-operator-precedence (or (operator-precedence (lisp-operator-name-to-js-operator-name operator))
                                                         #.(operator-precedence 'function-call))
           ;; (format *debug-io* "application-form of ~S~%" operator)
           (bind ((js-operator-name (to-js-operator-name operator)))
             (if (js-operator-name? operator)
                 ;; TODO it can only handle infix operators. due to this |not| needs its own special-form handler
                 (iter (for el :in arguments)
                       (unless (first-time-p)
                         (collect " ")
                         (collect js-operator-name)
                         (collect " "))
                       (collect (recurse el)))
                 (bind ((dotted? (starts-with #\. js-operator-name)))
                   (if dotted?
                       (with-operator-precedence 'member
                         `(,(recurse (first arguments))
                            ,js-operator-name "(" ,@(transform-quasi-quoted-js-to-quasi-quoted-string/application-arguments (rest arguments)) ")" ))
                       (with-operator-precedence 'comma
                         `(,js-operator-name "(" ,@(transform-quasi-quoted-js-to-quasi-quoted-string/application-arguments arguments) ")")))))))))))
   (constant-form
    (to-js-literal (value-of -node-)))
   (macrolet-form
    (transform-statements -node-))
   (lexical-variable-binder-form
    (transform-statements -node-))
   (setq-form
    (with-wrapping-based-on-operator-precedence '=
      `(,(recurse (variable-of -node-)) " = " ,(recurse (value-of -node-)))))
   (function-definition-form
    `("function " ,(lisp-name-to-js-name (name-of -node-))
                  ,@(transform-quasi-quoted-js-to-quasi-quoted-string/lambda-arguments-with-body -node-)))
   (macro-definition-form
    ;; we don't have anything to do with defmacro forms at this stage anymore
    (values))
   ;; TODO check how we render stuff re flet/labels and how that behaves in js...
   (flet-form
    (flet ((collect-js-names-of-variable-references (node)
             (mapcar (compose 'lisp-name-to-js-name 'name-of)
                     (collect-variable-references node))))
      (iter (with variable-references = (collect-js-names-of-variable-references (hu.dwim.walker:body-of -node-)))
            (for binding :in (bindings-of -node-))
            (for lisp-name = (name-of binding))
            (for name = (lisp-name-to-js-name lisp-name))
            (when (some (lambda (reference)
                          (string= name reference))
                        variable-references)
              (js-compile-warning -node- "Found a variable reference to a name that names an flet definition (~S). In the JavaScript output flet definitions are in the same namespace as the variables!" name))
            (appendf variable-references (collect-js-names-of-variable-references binding))
            (collect `(,@(make-newline-and-indent) "var " ,name " = " ,(recurse binding) ";") :into result)
            (finally (return (cons result (transform-statements (hu.dwim.walker:body-of -node-) :wrap? #f)))))))
   (walked-lexical-function-object-form
    (lisp-name-to-js-name (name-of -node-)))
   (return-from-form
    `("return" ,@(awhen (result-of -node-)
                        (list #\space (recurse it)))))
   (instantiate-form
    (with-wrapping-based-on-operator-precedence 'new
      `("new " ,(lisp-name-to-js-name (type-to-instantiate-of -node-))
               "("
               ,@(recurse-as-comma-separated (arguments-of -node-))
               ")")))
   (with-form
     (unless *in-js-statement-context*
       (error "The JavaScript 'with' form can only be used as a statement (it's used in an expression context). AST node: ~A" -node-))
    `("with ("
      ,(recurse (context-of -node-))
      ") { "
      ,@(with-increased-indent
         (transform-statements -node-))
      "}"))
   (create-form
    `("{ "
      ,@(with-increased-indent
         (transform-quasi-quoted-js-to-quasi-quoted-string/create-form/name-value-pairs (elements-of -node-)))
      "}"))
   (array-form
    (bind ((elements (elements-of -node-)))
      `(#\[
        ,@(recurse-as-comma-separated elements
                                      (lambda (node)
                                        (if (and (typep node 'js-unquote)
                                                 (spliced? node))
                                            (make-string-unquote
                                             (wrap-runtime-delayed-js-transformation-form
                                              `(transform-quasi-quoted-js-to-quasi-quoted-string/array-elements
                                                ,(form-of node))))
                                            (recurse node))))
        #\])))
   (regexp-form
    `("/"
      ,(regexp-of -node-)
      "/"))
   (for-form
    `("var " ,@(recurse-as-comma-separated (variables-of -node-)) ";" #\Newline
      "for (; "
      ,(recurse (looping-condition-of -node-))
      "; "
      ,@(recurse-as-comma-separated (steps-of -node-))
      ")"
      ,@(transform-statements (body-of -node-) :wrap? #t)))
   (while-form
    `("while ("
      ,(recurse (condition-of -node-))
      ")"
      ,@(transform-statements (hu.dwim.walker:body-of -node-) :wrap? #t)))
   (slot-value-form
    (bind ((object (object-of -node-))
           (slot-name (slot-name-of -node-)))
      (with-wrapping-based-on-operator-precedence 'member
        (if (symbolp slot-name)
            `(,(recurse object)
               #\.
               ,(lisp-name-to-js-name slot-name))
            `(,(recurse object)
               #\[
               ,(recurse slot-name)
               #\])))))
   (type-of-form
    (bind ((object (object-of -node-)))
      (with-operator-precedence 'typeof
        `("typeof(" ,(recurse object) ")"))))
   (unwind-protect-form
    `(,@(make-newline-and-indent) "try"
      ,@(recurse (protected-form-of -node-))
      ,@(make-newline-and-indent) "finally"
      ,@(transform-statements (cleanup-form-of -node-) :wrap? #t)))
   (try-form
    (bind ((protected-form (protected-form-of -node-))
           (catch-clauses (catch-clauses-of -node-))
           (finally-clause (finally-clause-of -node-)))
      (awhen (find-if [not (typep !1 'catch-form)] catch-clauses)
        (js-compile-error -node- "Expecting only catch caluse here, but got ~A" it))
      (flet ((transform-catch-clause (clause)
               `(,@(make-newline-and-indent) "catch (" ,(lisp-name-to-js-name (variable-name-of clause)) ")"
                 ,@(transform-statements clause :wrap? #t))))
        `(,@(make-newline-and-indent) "try"
          ,@(transform-statements (if (typep protected-form 'implicit-progn-mixin)
                                      protected-form
                                      (list protected-form))
                                  :wrap? #t)
          ,@(mapcar #'transform-catch-clause catch-clauses)
          ,@(when finally-clause
              `(,@(make-newline-and-indent) "finally"
                ,@(transform-statements finally-clause :wrap? #t)))))))))

(def (transformation e) quasi-quoted-js-to-quasi-quoted-string ()
  ((indentation-width nil)
   (output-prefix nil)
   (output-postfix nil))
  'transform-quasi-quoted-js-to-quasi-quoted-string/toplevel)

(defmethod print-object ((self quasi-quoted-js-to-quasi-quoted-string) *standard-output*)
  (princ "[JS->String]"))

(def method compatible-transformations? ((a quasi-quoted-js-to-quasi-quoted-string) a-next a-rest
                                         (b quasi-quoted-js-to-quasi-quoted-string) b-next b-rest)
  (compatible-transformations? a-next (first a-rest) (rest a-rest)
                               b-next (first b-rest) (rest b-rest)))

(def function transform-quasi-quoted-js-to-quasi-quoted-string/toplevel (node)
  (assert (typep node 'js-quasi-quote))
  (with-root-operator-precedence
    (make-string-quasi-quote (rest (transformation-pipeline-of node))
                             `(,(awhen (output-prefix-of *transformation*)
                                       (if (or (functionp it)
                                               (symbolp it))
                                           (funcall it)
                                           it))
                                ,(transform-quasi-quoted-js-to-quasi-quoted-string (body-of node))
                                ,(awhen (output-postfix-of *transformation*)
                                        (if (or (functionp it)
                                                (symbolp it))
                                            (funcall it)
                                            it))))))

(def function transform-quasi-quoted-js-to-quasi-quoted-string/process-unquoted-form (node fn)
  (map-filtered-tree (form-of node) 'js-quasi-quote fn))

(def function transform-quasi-quoted-js-to-quasi-quoted-string (node)
  (transformation-typecase node
    ((or number string character symbol) (to-js-literal node))
    (walked-form    (transform-quasi-quoted-js-to-quasi-quoted-string* node))
    (js-unquote     (transform-quasi-quoted-js-to-quasi-quoted-string/unquote node))
    (js-quasi-quote (if (compatible-transformation-pipelines? *transformation-pipeline*
                                                              (transformation-pipeline-of node))
                        (transform-quasi-quoted-js-to-quasi-quoted-string/toplevel node)
                        (transform node)))
    (string-quasi-quote node)
    (binary-quasi-quote node)))

(def function transform-quasi-quoted-js-to-quasi-quoted-string/unquote (node)
  (check-type node js-unquote)
  (bind ((spliced? (spliced? node)))
    (make-string-unquote
     (wrap-runtime-delayed-js-transformation-form
      (if spliced?
          `(mapcar 'transform-quasi-quoted-js-to-quasi-quoted-string
                   ,(transform-quasi-quoted-js-to-quasi-quoted-string/process-unquoted-form
                     node 'transform-quasi-quoted-js-to-quasi-quoted-string))
          `(transform-quasi-quoted-js-to-quasi-quoted-string
            ,(transform-quasi-quoted-js-to-quasi-quoted-string/process-unquoted-form
              node 'transform-quasi-quoted-js-to-quasi-quoted-string)))))))

(def (transformation e) quasi-quoted-js-to-quasi-quoted-js-building-forms ()
  ((result-quasi-quote-pipeline))
  'transform-quasi-quoted-js-to-quasi-quoted-js-building-forms)

(defmethod print-object ((self quasi-quoted-js-to-quasi-quoted-js-building-forms) *standard-output*)
  (princ "[JS->JS forms]"))

(def function transform-quasi-quoted-js-to-quasi-quoted-js-building-forms (node)
  (check-type node js-syntax-node)
  (bind (((list (quote qq-macro) result) (bq-completely-process node)))
    (assert (eq list 'list))
    (assert (eq quote 'quote))
    (assert (eq qq-macro 'toplevel-quasi-quote-macro))
    (with-unique-names (result-tmp)
      `(bind ((,result-tmp ,result))
         (setf (transformation-pipeline-of ,result-tmp) ',(result-quasi-quote-pipeline-of *transformation*))
         ,result-tmp))))
