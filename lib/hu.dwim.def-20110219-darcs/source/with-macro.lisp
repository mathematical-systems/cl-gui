;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(def function compute-arguments-for-function-bridge-macro (args &optional body-invocation-arguments macro-only-arguments)
  (unless args
    (return-from compute-arguments-for-function-bridge-macro
      (values nil nil nil nil)))
  (labels ((macro-only-argument? (name)
             (member name macro-only-arguments :test #'eq))
           (maybe-quote (arg)
             (if (macro-only-argument? arg)
                 ``(quote ,,arg)
                 arg))
           (process-required-argument (arg)
             ;; leave out args that are used to input new lexical names to local vars
             (unless (member arg body-invocation-arguments
                             :key (lambda (el)
                                    (second (ensure-list el)))
                             :test #'eq)
               (maybe-quote arg))))
    (bind (((:values requireds optionals rest-variable-name keywords allow-other-keys?) (parse-ordinary-lambda-list args))
           ((:values nil raw-optionals nil raw-keywords) (parse-ordinary-lambda-list args :normalize nil))
           (macro-args requireds)
           (macro-ignored-args '())
           (function-args (remove-if #'macro-only-argument? requireds))
           (processed-required-args (remove nil (mapcar #'process-required-argument requireds)))
           (funcall-list (list `(list ,@processed-required-args)))
           (generic-args processed-required-args)
           (to-be-removed-macro-only-keywords '()))
      (when optionals
        (appendf macro-args '(&optional))
        (appendf function-args '(&optional))
        (appendf generic-args '(&optional)))
      (loop
        :for entry :in optionals
        :for raw-entry :in raw-optionals
        :for local-var = (first entry)
        :for provided? = (or (third entry)
                             (gensym (concatenate 'string (string local-var) (string '#:-provided?))))
        :do (progn
              (unless (macro-only-argument? local-var)
                (appendf funcall-list `((when ,provided? (list ,(maybe-quote local-var)))))
                (appendf function-args (list raw-entry)))
              (appendf macro-args (if rest-variable-name
                                      (list local-var)
                                      `((,local-var '#:ignore-me ,provided?))))
              (appendf generic-args (list (first (ensure-list raw-entry))))))
      (progn
        (when rest-variable-name
          (appendf macro-args `(&rest ,rest-variable-name))
          (appendf function-args `(&rest ,rest-variable-name)))
        (when keywords
          (appendf macro-args '(&key))
          (appendf function-args '(&key))
          (appendf generic-args '(&key))
          (loop
            :for entry :in keywords
            :for raw-entry :in raw-keywords
            :for keyword = (first (first entry))
            :for local-var = (second (first entry))
            :for provided? = (or (third entry)
                                 (gensym (concatenate 'string (string local-var) (string '#:-provided?))))
            :do (bind ((name-part (first (ensure-list raw-entry)))
                       (name (if (consp name-part)
                                 (intern (symbol-name (first name-part)))
                                 name-part)))
                  (if (macro-only-argument? name)
                      (push keyword to-be-removed-macro-only-keywords)
                      (progn
                        (if rest-variable-name
                            (push local-var macro-ignored-args)
                            (appendf funcall-list `((when ,provided? (list ',keyword ,(maybe-quote name))))))
                        (appendf function-args (list raw-entry))))
                  (appendf macro-args (if rest-variable-name
                                          (list local-var)
                                          `((,name '#:ignore-me ,provided?))))
                  (appendf generic-args (list name)))))
        (when allow-other-keys?
          (appendf macro-args '(&allow-other-keys))
          (appendf function-args '(&allow-other-keys))
          (appendf generic-args '(&allow-other-keys)))
        (when rest-variable-name
          (appendf funcall-list (if to-be-removed-macro-only-keywords
                                    `((remove-from-plist ,rest-variable-name ,@to-be-removed-macro-only-keywords))
                                    `(,rest-variable-name)))))
      (values macro-args
              (reverse macro-ignored-args)
              funcall-list
              function-args
              generic-args))))

(def function expand-with-macro/process-body (body-form)
  (bind ((body-invocation-arguments 'undefined))
    (labels
        ((recurse (form)
           (cond
             ((consp form)
              (cond
                ;; TODO obsolete -body- (search this file for it)
                ((member (first form) '(-body- -with-macro/body-))
                 (unless (or (member body-invocation-arguments '(undefined ignore))
                             (equal body-invocation-arguments (rest form)))
                   (error "Used -WITH-MACRO/BODY- multiple times and they have different argument lists: ~S, ~S" body-invocation-arguments (rest form)))
                 (setf body-invocation-arguments (rest form))
                 ;; use an flet instead of `(funcall ,fn ,@body-invocation-arguments) so that #'-with-macro/body- also works as expected
                 `(,(first form) ,@(mapcar (lambda (el)
                                             (first (ensure-list el)))
                                           (rest form))))
                ((and (eq (first form) 'function)
                      (member (second form) '(-body- -with-macro/body-))
                      (length= 2 form))
                 ;; shut up if there's a #'-with-macro/body- somewhere
                 (setf body-invocation-arguments nil)
                 form)
                (t
                 (iter (for entry :first form :then (cdr entry))
                       (collect (recurse (car entry)) :into result)
                       (cond
                         ((consp (cdr entry))
                          ;; nop, go on looping
                          )
                         ((cdr entry)
                          (setf (cdr (last result)) (cdr entry))
                          (return result))
                         (t (return result)))))))
             ((typep form 'standard-object)
              ;; FIXME: KLUDGE to avoid a warning when quasi-quote literal STANDARD-OBJECT AST nodes are "hiding" -with-macro/body- references
              (setf body-invocation-arguments 'ignore)
              form)
             (t form))))
      (values (recurse body-form) (if (eq body-invocation-arguments 'ignore)
                                      nil
                                      body-invocation-arguments)))))

(def function expand-with-macro (name args body -options- flat? must-have-args?)
  (flet ((simple-lambda-list? (args)
           (bind (((:values nil optionals rest keywords allow-other-keys?) (parse-ordinary-lambda-list args)))
             (and (not rest)
                  (not optionals)
                  (not keywords)
                  (not allow-other-keys?)))))
    (unless (or (not flat?)
                (simple-lambda-list? args))
      (error "Can not generate a flat with-macro when using &rest, &optional or &key in its lambda list. Use with-macro* for that.")))
  (with-unique-names (fn with-body)
    (with-standard-definer-options name
      (bind ((function-definer (getf -options- :function-definer 'function))
             (macro-definer (getf -options- :macro-definer 'macro))
             (call-with-fn/name (format-symbol *package* "CALL-~A" name))
             (ignorable-variables '())
             ((:values parsed-body declarations doc-string) (parse-body body :documentation t))
             ((:values final-body-forms body-invocation-arguments) (expand-with-macro/process-body parsed-body)))
        (when (eq body-invocation-arguments 'undefined)
          (simple-style-warning "You probably want to have at least one (-with-macro/body-) form in the body of a WITH-MACRO to invoke the user provided body...")
          (setf body-invocation-arguments nil))
        (bind ((lexically-transferred-arguments '())
               (body-lambda-arguments '()))
          (dolist (el body-invocation-arguments)
            (if (consp el)
                ;; Allegro walker (?) bug triggered by the ignorable key arg: (bind (((original-name &optional new-name &key ignorable) el))
                (bind (((original-name &optional new-name &key ignorable?) el))
                  (unless new-name
                    (setf new-name `(quote ,original-name)))
                  (when (or (not (symbolp original-name))
                            (not (or (symbolp new-name)
                                     (and (consp new-name)
                                          (eq (first new-name) 'quote)
                                          (symbolp (second new-name))
                                          (not (cddr new-name))))))
                    (error "The arguments used to invoke (-with-macro/body- foo1 foo2) may only contain symbols, or (var-name-inside-macro-body var-name-visible-for-user-forms) pairs denoting variables that are \"transferred\" from the lexical scope of the with-macro into the lexical scope of the user provided body forms (implemented by renaming the fn's argument)."))
                  (when ignorable?
                    (push new-name ignorable-variables))
                  (push new-name body-lambda-arguments)
                  (push original-name lexically-transferred-arguments))
                (progn
                  (push el lexically-transferred-arguments)
                  (push `(quote ,el) body-lambda-arguments))))
          (reversef lexically-transferred-arguments)
          (reversef body-lambda-arguments)
          (bind (((:values macro-args macro-ignored-args funcall-list function-args generic-args)
                  (compute-arguments-for-function-bridge-macro
                   args body-invocation-arguments
                   (ensure-list (getf -options- :macro-only-arguments)))))
            `(progn
               ,(when function-definer
                  `(def ,function-definer ,call-with-fn/name ,(if (eq function-definer 'generic)
                                                                  `(thunk ,@generic-args)
                                                                  `(,fn ,@function-args))
                     ,@(bind ((body `((declare (type function ,fn)
                                               ,@(function-like-definer-declarations -options-))
                                      ,@declarations
                                      (labels ((-with-macro/body- (,@lexically-transferred-arguments)
                                                 (funcall ,fn ,@lexically-transferred-arguments))
                                               (-body- (&rest args)
                                                 (apply #'-with-macro/body- args)))
                                        (declare (dynamic-extent #'-with-macro/body-))
                                        (block ,name
                                          ,@final-body-forms)))))
                         (case function-definer
                           ((function method) body)
                           (generic `((:method (,fn ,@function-args)
                                        ,@body)))
                           (t body)))))
               ,(when macro-definer
                  `(def ,macro-definer ,name (,@(when (or args must-have-args?)
                                                  (if flat? macro-args (list macro-args)))
                                        &body ,with-body)
                     ,@(when doc-string (list doc-string))
                     (declare (ignore ,@macro-ignored-args))
                     `(,',call-with-fn/name
                       (named-lambda ,',(symbolicate name '#:-body) ,(list ,@body-lambda-arguments)
                         ,@,(when ignorable-variables
                             ``((declare (ignorable ,,@ignorable-variables))))
                         ,@,with-body)
                       ;; this ,@,@ is broken on ccl due to http://trac.clozure.com/ccl/ticket/6
                       ,@,@funcall-list))))))))))

;; TODO exchange the names with-macro/with-macro* so that the starred version would be the flat one without the extra parens
(def (definer e :available-flags "eod") with-macro (name args &body body)
  "(def with-macro with-foo (arg1 arg2)
     (let ((*zyz* 42)
           (local 43))
       (do something)
       (-body- local)))
   Example:
   (with-foo arg1 arg2
     (...))"
  (expand-with-macro name args body -options- t nil))

(def (definer e :available-flags "eod") with-macro* (name args &body body)
  "(def with-macro* with-foo (arg1 arg2 &key alma)
     (let ((*zyz* 42)
           (local 43))
       (do something)
       (-body- local)))
   Example:
   (with-foo (arg1 arg2 :alma alma)
     (...))"
  (expand-with-macro name args body -options- nil t))
