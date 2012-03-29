(in-package :closer-mop)

(defmacro removef (place item)
  `(setf ,place (remove ,item ,place)))

(defun extract-lambda-list (lambda-list)
  (loop for (arg . rest) on lambda-list
        for keyp = (member arg lambda-list-keywords)
        until keyp
        if (consp arg)
        collect (car arg) into args
        else collect arg into args
        finally (return (if keyp
                          (nconc args (cons arg rest))
                          args))))

(defun extract-specializer-names (lambda-list)
  (loop for arg in lambda-list
        until (member arg lambda-list-keywords)
        if (consp arg)
        collect (cadr arg)
        else collect 't))

(cl:defgeneric generic-function-method-class (object)
  (:method ((gf generic-function))
   (clos:generic-function-method-class gf)))

(cl:defmethod compute-discriminating-function ((gf generic-function)) 
  (declare (ignore gf))
  t)

(cl:defmethod make-method-lambda ((gf generic-function) (method method)
                                  lambda-expression environment)
  (declare (ignore environment method gf))
  (destructuring-bind
      (lambda (&rest args) &body body)
      lambda-expression
    (assert (eq lambda 'lambda))
    (when (and (member '&key args :test #'eq)
               (not (member '&allow-other-keys args :test #'eq)))
      (let ((tail (member '&aux args :test #'eq)))
        (setq lambda-expression
              `(lambda (,.(ldiff args tail) &allow-other-keys ,@tail) ,@body))))
    (values
     lambda-expression
     (let ((documentation (parse-method-body body lambda-expression)))
       (when documentation
         (list 'documentation documentation))))))

(cl:defmethod compute-effective-method-function ((gf standard-generic-function) effective-method options)
  (declare (optimize (speed 3) (space 0) (compilation-speed 0)))
  (when options
    (cerror "Ignore these options."
            "This version of compute-effective-method-function does not support method combination options: ~S"
            options))
  (if (only-standard-methods gf)
    effective-method
    (lambda (&rest args)
      (funcall effective-method args nil))))

(cl:defgeneric find-method-combination (generic-function type options)
  (:method ((gf generic-function) type options)
   (declare (ignore gf))
   (cons type options)))

(defclass standard-class (cl:standard-class)
  ((direct-methods :initform '() :reader specializer-direct-methods)))

(defun optimize-slot-access-p (class)
  (flet ((applicablep (specializer)
           (if (consp specializer)
             (eql class (eql-specializer-object specializer))
             (subclassp (class-of class) specializer))))
    (when (and (loop for method in (generic-function-methods #'slot-value-using-class)
                     never (applicablep (first (method-specializers method))))
               (loop for method in (generic-function-methods #'(setf slot-value-using-class))
                     never (applicablep (second (method-specializers method)))))
      '(t))))

(cl:defmethod initialize-instance :around ((class standard-class) &rest initargs)
  (apply #'call-next-method class
         :optimize-slot-access
         (optimize-slot-access-p class)
         initargs))

(cl:defmethod reinitialize-instance :around ((class standard-class) &rest initargs)
  (apply #'call-next-method class
         :optimize-slot-access
         (optimize-slot-access-p class)
         initargs))

(cl:defmethod initialize-instance :around ((class funcallable-standard-class) &rest initargs)
  (apply #'call-next-method class
         :optimize-slot-access
         (optimize-slot-access-p class)
         initargs))

(cl:defmethod reinitialize-instance :around ((class funcallable-standard-class) &rest initargs)
  (apply #'call-next-method class
         :optimize-slot-access
         (optimize-slot-access-p class)
         initargs))

(cl:defmethod initialize-instance :before ((class standard-class) &key direct-superclasses)
  (assert (loop for superclass in direct-superclasses
                always (validate-superclass class superclass))))

(cl:defmethod reinitialize-instance :before ((class standard-class) &key (direct-superclasses '() direct-superclasses-p))
  (when direct-superclasses-p
    (assert (loop for superclass in direct-superclasses
                  always (validate-superclass class superclass)))
    (loop for superclass in (class-direct-superclasses class)
          unless (member superclass direct-superclasses)
          do (remove-direct-subclass superclass class))))

(cl:defmethod initialize-instance :before ((class funcallable-standard-class) &key direct-superclasses)
  (assert (loop for superclass in direct-superclasses
                always (validate-superclass class superclass))))

(cl:defmethod initialize-instance :before ((class funcallable-standard-class) &key direct-superclasses)
  (assert (loop for superclass in direct-superclasses
                always (validate-superclass class superclass))))

(cl:defmethod (setf class-name) (new-value (class standard-class))
  (reinitialize-instance class :name new-value)
  new-value)

(cl:defmethod (setf generic-function-name) (new-value (gf standard-generic-function))
  (reinitialize-instance gf :name new-value)
  new-value)

(defvar *direct-methods-for-built-in-classes*
  (make-hash-table :test #'eq))

(cl:defgeneric add-direct-method (specializer method)
  (:method ((specializer class) (method method))
   (declare (ignore specializer method)))
  (:method ((specializer built-in-class) (method method))
   (pushnew method (gethash specializer *direct-methods-for-built-in-classes*)))
  (:method ((specializer standard-class) (method method))
   (pushnew method (slot-value specializer 'direct-methods)))
  (:method ((specializer funcallable-standard-class) (method method))
   (pushnew method (slot-value specializer 'direct-methods))))

(cl:defgeneric remove-direct-method (specializer method)
  (:method ((specializer class) (method method))
   (declare (ignore specializer method)))
  (:method ((specializer built-in-class) (method method))
   (removef (gethash specializer *direct-methods-for-built-in-classes*) method))
  (:method ((specializer standard-class) (method method))
   (removef (slot-value specializer 'direct-methods) method))
  (:method ((specializer funcallable-standard-class) (method method))
   (removef (slot-value specializer 'direct-methods) method)))

(defvar *dependents* (make-hash-table :test #'eq))

(cl:defgeneric add-dependent (metaobject dependent)
  (:method ((metaobject standard-class) dependent)
    (pushnew dependent (gethash metaobject *dependents*)))
  (:method ((metaobject funcallable-standard-class) dependent)
    (pushnew dependent (gethash metaobject *dependents*)))
  (:method ((metaobject standard-generic-function) dependent)
    (pushnew dependent (gethash metaobject *dependents*))))

(cl:defgeneric remove-dependent (metaobject dependent)
  (:method ((metaobject standard-class) dependent)
    (setf (gethash metaobject *dependents*)
	  (delete dependent (gethash metaobject *dependents*))))
  (:method ((metaobject funcallable-standard-class) dependent)
    (setf (gethash metaobject *dependents*)
	  (delete dependent (gethash metaobject *dependents*))))
  (:method ((metaobject standard-generic-function) dependent)
    (setf (gethash metaobject *dependents*)
	  (delete dependent (gethash metaobject *dependents*)))))

(cl:defgeneric map-dependents (metaobject function)
  (:method ((metaobject standard-class) function)
    (mapc function (gethash metaobject *dependents*)))
  (:method ((metaobject funcallable-standard-class) function)
    (mapc function (gethash metaobject *dependents*)))
  (:method ((metaobject standard-generic-function) function)
    (mapc function (gethash metaobject *dependents*))))

(cl:defgeneric update-dependent (metaobject dependent &rest initargs))

(cl:defmethod reinitialize-instance :after ((metaobject standard-class) &rest initargs)
  (map-dependents metaobject (lambda (dep) (apply #'update-dependent metaobject dep initargs))))

(cl:defmethod reinitialize-instance :after ((metaobject funcallable-standard-class) &rest initargs)
  (map-dependents metaobject (lambda (dep) (apply #'update-dependent metaobject dep initargs))))

(cl:defmethod initialize-instance :after ((gf standard-generic-function) &rest initargs)
  (declare (ignore initargs))
  (set-funcallable-instance-function gf (compute-discriminating-function gf)))

(cl:defmethod ensure-generic-function-using-class :around ((gf null) name &rest initargs)
  (declare (ignore name initargs gf))
  (let ((new-gf (call-next-method)))
    (if (typep new-gf 'standard-generic-function)
      (set-funcallable-instance-function new-gf (compute-discriminating-function new-gf))
      new-gf)))

(cl:defmethod reinitialize-instance :after ((gf standard-generic-function) &rest initargs)
  (set-funcallable-instance-function gf (compute-discriminating-function gf))
  (map-dependents gf (lambda (dep) (apply #'update-dependent gf dep initargs))))

(cl:defgeneric remove-method (gf method)
  (:method ((gf generic-function) (method method))
   (cl:remove-method gf method)))

(cl:defmethod remove-method :after ((gf standard-generic-function) (method method))
  (set-funcallable-instance-function gf (compute-discriminating-function gf))
  (loop for specializer in (method-specializers method)
        if (consp specializer) do (remove-direct-method (intern-eql-specializer*
                                                         (eql-specializer-object specializer))
                                                        method)
        else do (remove-direct-method specializer method))
  (map-dependents gf (lambda (dep) (update-dependent gf dep 'remove-method method))))

(cl:defgeneric find-method (gf qualifiers specializers &optional errorp)
  (:method ((gf generic-function) qualifiers specializers &optional (errorp t))
   (cl:find-method gf qualifiers specializers errorp)))

(cl:defmethod add-method :before ((gf standard-generic-function) (method method))
  (let ((other-gf (method-generic-function method)))
    (unless (or (null other-gf) (eq other-gf gf))
      (error "The method ~S belongs to the generic function ~S and cannot be added to ~S."
             method other-gf gf)))
  (let ((old-method (find-method gf
                                 (method-qualifiers method)
                                 (method-specializers method)
                                 nil)))
    (when old-method
      (remove-method gf old-method))))

(cl:defmethod add-method :after ((gf standard-generic-function) (method method))
  (set-funcallable-instance-function gf (compute-discriminating-function gf))
  (loop for specializer in (method-specializers method)
        if (consp specializer) do (add-direct-method (intern-eql-specializer*
                                                      (eql-specializer-object specializer))
                                                     method)
        else do (add-direct-method specializer method))
  (map-dependents gf (lambda (dep) (update-dependent gf dep 'add-method method))))

(defun eql-specializer-p (thing)
  (and (consp thing)
       (eq (car thing) 'eql)
       (null (cddr thing))))

(deftype eql-specializer ()
  '(or eql-specializer* (satisfies eql-specializer-p)))

(cl:defgeneric eql-specializer-object (eql-specializer)
  (:method ((cons cons))
   (if (eql-specializer-p cons)
     (cadr cons)
     (error "~S is not an eql-specializer." cons))))

(defun intern-eql-specializer (object)
  `(eql ,object))

(cl:defmethod specializer-direct-methods ((cons cons))
  (specializer-direct-methods (eql-specializer-object cons)))

(defclass eql-specializer* ()
  ((obj :reader eql-specializer-object
        :initarg eso
        :initform (error "Use intern-eql-specializer to create eql-specializers."))
   (direct-methods :reader specializer-direct-methods
                   :accessor es-direct-methods
                   :initform ())))

(defvar *eql-specializers* (make-hash-table))

#+threads
(defvar *eql-specializers-lock* (mp:make-lock :name 'eql-specializers))

(defmacro with-eql-specializers-lock (&body body)
  #+threads `(mp:with-lock (*eql-specializers-lock*) ,@body)
  #-threads `(progn ,@body))

(defun intern-eql-specializer* (object)
  (or #+threads (gethash object *eql-specializers*)
      (with-eql-specializers-lock
       (or (gethash object *eql-specializers*)
           (setf (gethash object *eql-specializers*)
                 (make-instance 'eql-specializer* 'eso object))))))

(cl:defmethod add-direct-method ((specializer eql-specializer*) (method method))
  (pushnew method (es-direct-methods specializer)))

(cl:defmethod remove-direct-method ((specializer eql-specializer*) (method method))
  (removef (es-direct-methods specializer) method))

(cl:defgeneric specializer-direct-generic-functions (specializer)
  (:method ((class class))
   (remove-duplicates
    (mapcar #'method-generic-function
            (specializer-direct-methods class))))
  (:method ((eql-specializer eql-specializer*))
   (remove-duplicates
    (mapcar #'method-generic-function
            (specializer-direct-methods eql-specializer))))
  (:method ((cons cons))
   (specializer-direct-generic-functions
    (intern-eql-specializer*
     (eql-specializer-object cons)))))

(cl:defgeneric validate-superclass (class superclass)
  (:method (class superclass)
   (or (eq superclass (find-class 't))
       (typep superclass (find-class 'forward-referenced-class))
       (eq (class-of class) (class-of superclass))
       (let ((compatible-classes (list (find-class 'cl:standard-class)
                                       (find-class 'standard-class)
                                       (find-class 'clos:funcallable-standard-class)
                                       (find-class 'funcallable-standard-class))))
         (and (member (class-of class) compatible-classes)
              (member (class-of superclass) compatible-classes))))))

(define-validate-superclass-method standard-class cl:standard-class)
(define-validate-superclass-method funcallable-standard-class clos:funcallable-standard-class)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :closer-mop *features*))
