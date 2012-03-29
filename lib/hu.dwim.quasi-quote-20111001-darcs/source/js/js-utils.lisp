;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.quasi-quote.js)

(macrolet ((x (&rest names)
             `(progn
                ,@(iter (for name :in names)
                        (collect `(def (js-lisp-macro-alias e) ,name))))))
  (x when unless))

(def (js-macro e) |cond| (&rest clauses)
  (if (endp clauses)
      nil
      (let ((clause (first clauses))
            (more (rest clauses)))
        (if (atom clause)
            (error "COND clause is not a list: ~S" clause)
            (let ((test (first clause))
                  (forms (rest clause)))
              (if (endp forms)
                  (with-unique-names (n-result)
                    {with-preserved-readtable-case
                     `(let ((,N-RESULT ,TEST))
                        (if ,N-RESULT
                            ,N-RESULT
                            ,@(WHEN MORE `((cond ,@MORE)))))})
                  (if (member test '(t |t|))
                      {with-preserved-readtable-case
                       `(progn ,@FORMS)}
                      {with-preserved-readtable-case
                       `(if ,TEST
                            (progn ,@FORMS)
                            ,@(WHEN MORE `((cond ,@MORE))))})))))))

(def (js-macro e) |dolist| ((var list) &body body)
  (with-unique-names (idx)
    (once-only (list)
      {with-preserved-readtable-case
        `(do ((,IDX 0 (1+ ,IDX)))
             ((>= ,IDX (slot-value ,LIST 'length)))
           (let ((,VAR (aref ,LIST ,IDX)))
             ,@BODY))})))

(def (js-macro e) |foreach| (function array &rest other-arrays)
  (when other-arrays
    (not-yet-implemented "Currently the JavaScript 'foreach macro does not support multiple sequences to iterate on"))
  (when (quoted-form? function)
    (setf function (second function)))
  (with-unique-names (el)
    {with-preserved-readtable-case
      `(dolist (,EL ,ARRAY)
         (,FUNCTION ,EL))
      #+nil ; this could work, but of course forEach is not available on IE...
      `(.forEach ,ARRAY (lambda (el index array)
                          ;; forEach calls the thunk with 3 arguments which can have surprising effects (i.e. with the silly dojo.addOnLoad), so drop the others...
                          (,FUNCTION el)))}))

#+nil ; this is an alternative rebind using 'with'. delme?
(def (js-macro e) |rebind| (variables &body body)
  {with-preserved-readtable-case
    `(with (create ,@(LOOP
                       :FOR VARIABLE :IN VARIABLES
                       :FOR JS-NAME = (LISP-NAME-TO-JS-NAME VARIABLE)
                       :COLLECT JS-NAME
                       :COLLECT (MAKE-SYMBOL JS-NAME)))
           ,@BODY)})

(def (js-macro e) |rebind| (variables &body body)
  {with-preserved-readtable-case
    `((lambda ,VARIABLES
        ,@BODY) ,@VARIABLES)})

(def (js-macro e) |rebind/expression| (variables &body body)
  (unless (length= 1 body)
    (error "~S only supports a single statement whose return value will be the result of the form" '|rebind/expression|))
  {with-preserved-readtable-case
    `((lambda ,VARIABLES
        (return ,@BODY)) ,@VARIABLES)})

(macrolet ((frob (name index)
             `(def (js-macro e) ,name (thing)
                `(|aref| ,thing ,,index))))
  (frob |first|   0)
  (frob |second|  1)
  (frob |third|   2)
  (frob |fourth|  3)
  (frob |fifth|   4)
  (frob |sixth|   5)
  (frob |seventh| 6)
  (frob |eight|   7)
  (frob |ninth|   8)
  (frob |tenth|   9))
