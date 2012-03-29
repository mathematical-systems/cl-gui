(in-package #:lispworks)

#+lispworks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "lw-compat is not needed in LispWorks."))

(define-modify-macro appendf (&rest lists)
  append "Appends lists to the end of given list.")

(define-modify-macro nconcf (&rest lists)
  nconc "Appends lists to the end of given list by NCONC.")

(defmacro rebinding (vars &body body)
  "Ensures unique names for all the variables in a groups of forms."
  (loop for var in vars
	for name = (gensym (symbol-name var))
	collect `(,name ,var) into renames
	collect ``(,,var ,,name) into temps
	finally (return `(let ,renames
			   (with-unique-names
                               ,vars
                             `(let (,,@temps)
                                ,,@body))))))

(define-modify-macro removef (item &rest keys)
  (lambda (place item &rest keys &key test test-not start end key)
    (declare (ignorable test test-not start end key))
    (apply #'remove item place keys))
  "Removes an item from a sequence.")

(defmacro when-let ((var form) &body body)
  "Executes a body of code if a form evaluates to non-nil,
   propagating the result of the form through the body of code."
  `(let ((,var ,form))
     (when ,var
       (locally
         ,@body))))

(defmacro when-let* (bindings &body body)
  "Executes a body of code if a series of forms evaluates to non-nil,
   propagating the results of the forms through the body of code."
  (loop for form = `(progn ,@body) then `(when-let (,(car binding) ,(cadr binding)) ,form)
        for binding in (reverse bindings)
        finally (return form)))

(defmacro with-unique-names (names &body body)
  "Returns a body of code with each specified name bound to a similar name."
  `(let ,(mapcar (lambda (name) `(,name (gensym ,(symbol-name name))))
                 names)
     ,@body))
