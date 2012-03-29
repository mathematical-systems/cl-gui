(in-package :cl-gui)

(defclass js-class (standard-class)
  ())

(defclass js-object ()
  ((_oid :initarg :_oid :initform .js-object-counter. :accessor _oid-of)
   (_id :initarg :_id :initform (error "must specify a js object name") :accessor _id-of)))

(defmethod shared-initialize :around ((class js-class)
                                      slot-names
                                      &rest all-keys
                                      &key direct-superclasses
                                           &allow-other-keys)
  (let ((parent (find-class 'js-object)))
    (labels ((inherits-from (classes)
               (loop for class in classes
                     thereis (or (subtypep class parent)
                                 (inherits-from
                                  (closer-mop:class-direct-subclasses class))))))
      (let ((all-keys (copy-list all-keys)))
        (setf (getf all-keys :direct-superclasses)
              (if (inherits-from direct-superclasses)
                  direct-superclasses
                  ;; make sure it inherits from js-object
                  (cons parent direct-superclasses)))
        (apply #'call-next-method class slot-names all-keys)))))

(defmethod js-object-to-hashtable ((o js-object))
  (let ((slot-names
          (mapcar #'closer-mop:slot-definition-name (closer-mop:compute-slots (class-of o)))))
    (loop for s in slot-names
          with table = (make-hash-table :size (length slot-names))
          do (setf (gethash s table) (slot-value o s))
          finally (return table))))

(defmethod make-instance :around ((class js-class) &rest initargs)
  (declare (ignorable initargs))
  (let ((instance (call-next-method)))
    (incf .js-object-counter.)
    (add-js-object instance)
    (emit :update :data (js-object-to-hashtable instance))
    instance))

(defmethod closer-mop:validate-superclass ((class js-class) (super standard-class))
  t)

(defmethod closer-mop:validate-superclass ((super standard-class) (class js-class))
  nil)

(defmethod update-js-object ((o js-object))
  (emit :update :data (js-object-to-hashtable o)))

