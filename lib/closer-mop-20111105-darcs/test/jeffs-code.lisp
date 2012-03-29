;;; Jeff Caldwell 2004-04-16
;;; LWL 4.3
;;;
;;; To reproduce the issues I have come across:
;;;
;;; 1. (asdf:oos 'asdf:load-op 'closer-mop)
;;; 2. (compile-file "c2mop-attributes.lisp" :load t)
;;; 3. (in-package #:c2mop-test)
;;; C2MOP-TEST> (setq cr (make-instance 'credit-rating))
;;; => Stack overflow (stack size 16000).
;;;
;;; (In this code, I accidently took out the format statements
;;; creating the output below. You may wish to put them back
;;; in the slot-value-using-class and (setf slot-value-using-class)
;;; methods at the bottom of this file.)
;;;
;;; slot-value-using-class class #<ATTRIBUTES-CLASS CREDIT-RATING 2134BF1C>
;;;   object #<CREDIT-RATING 21732B2C> slot-name ALL-ATTRIBUTES-2382
;;; slot-value-using-class class #<ATTRIBUTES-CLASS CREDIT-RATING 2134BF1C> 
;;;   object #<CREDIT-RATING 21732B2C> slot-name LEVEL
;;; slot-value-using-class class #<ATTRIBUTES-CLASS CREDIT-RATING 2134BF1C> 
;;;  object #<CREDIT-RATING 21732B2C> 
;;;  slot-name #<ATTRIBUTES-EFFECTIVE-SLOT-DEFINITION LEVEL 2134396C>
;;; slot-value-using-class class #<ATTRIBUTES-CLASS CREDIT-RATING 2134BF1C> 
;;;  object #<CREDIT-RATING 21732B2C> slot-name LEVEL
;;; slot-value-using-class class #<ATTRIBUTES-CLASS CREDIT-RATING 2134BF1C> 
;;;  object #<CREDIT-RATING 21732B2C> 
;;;  slot-name #<ATTRIBUTES-EFFECTIVE-SLOT-DEFINITION LEVEL 2134396C>
;;; slot-value-using-class class #<ATTRIBUTES-CLASS CREDIT-RATING 2134BF1C> 
;;;  object #<CREDIT-RATING 21732B2C> slot-name LEVEL
;;; slot-value-using-class class #<ATTRIBUTES-CLASS CREDIT-RATING 2134BF1C> 
;;;  object #<CREDIT-RATING 21732B2C> 
;;;  slot-name #<ATTRIBUTES-EFFECTIVE-SLOT-DEFINITION LEVEL 2134396C>
;;; ...
;;; 
;;; Note that it alternates between slot-name LEVEL and
;;; slot-name #<ATTRIBUTES-EFFECTIVE-...
;;;
;;; In closer-mop.lisp change
;;;
;;;  (cl:defmethod slot-value-using-class
;;;             ((class standard-class) object (slot symbol))
;;;    (let ((slotd (find slot (class-slots class) :key #'slot-definition-name)))
;;;      (if slotd
;;;          (if (default-reader slotd)
;;;              (slot-value-using-class class object slotd)
;;;            (call-next-method))
;;;        (slot-missing class object slot 'slot-value))))
;;;
;;; to
;;;
;;; ...
;;;      (if slotd
;;;          (if (default-reader slotd)
;;;              (slot-value-using-class (find-class 'standard-class) 
;;;                                      object slotd)
;;; ...
;;;
;;; (I have no idea if that's a correct patch but it does stop
;;; the recursive stack overflow.)
;;;
;;; Then 
;;; (asdf:oos 'asdf:load-op 'closer-mop)
;;; (setq cr (make-instance 'credit-rating)
;;; =>
;;; The slot #<ATTRIBUTES-EFFECTIVE-SLOT-DEFINITION LEVEL 2133983C> is 
;;; missing from #<CREDIT-RATING 206F0864> (of class #<ATTRIBUTES-CLASS 
;;; CREDIT-RATING 2133B3DC>), when reading the value.
;;;
;;; At this point you also can remove the slot-value-using-class and 
;;; setf slot-value-using-class methods. They were no-ops in this
;;; example, something I had run across in other code. I left them
;;; here to show the recursive stack overflow. Now that it is "fixed",
;;; we are left with the missing slot problem above.
;;; (The problem above is somewhat different from what I reported
;;; in my first email but the error above is what I'm getting now
;;; with this example.)
;;;
;;; Simply using the LW MOP, instead of using closer-mop, 
;;; "fixes" the problem above. Quit using closer-mop and revert 
;;; to the LW-only MOP. Change the defpackage to
;;;
;;; (defpackage #:c2mop-test
;;;  (:use :cl :cl-user :clos))
;;;
;;; (cl-user::quit) ;; Make really sure everything's fresh
;;; M-x slime
;;; (compile-file "c2mop-attributes.lisp" :load t)
;;; CL-USER> (in-package #:c2mop-test)
;;; #<PACKAGE C2MOP-TEST>
;;; C2MOP-TEST> (setq cr (make-instance 'credit-rating))
;;; #<CREDIT-RATING 206EDFAC>
;;; C2MOP-TEST> (setf (level cr) 42)
;;; 42
;;; C2MOP-TEST> (level cr)
;;; 42
;;; C2MOP-TEST> (setf (slot-attribute cr 'level 'date-set) 20040416)
;;; 20040416
;;; C2MOP-TEST> (slot-attribute cr 'level 'date-set)
;;; 20040416
;;;


;;;
(defpackage #:c2mop-test
;  (:use :cl :cl-user :clos)
  (:use :cl :cl-user :closer-mop)
  (:shadowing-import-from :closer-mop
                          #:defclass #:defmethod #:standard-class 
                          #:ensure-generic-function #:defgeneric 
                          #:standard-generic-function #:class-name)
)

(in-package #:c2mop-test)

(eval-when (:compile-toplevel :load-toplevel :execute)

(defvar *all-attributes* (gensym "ALL-ATTRIBUTES-"))
(defvar *current-direct-slot-definitions* nil)

(defclass attributes-class (standard-class) ())

(defclass attributes-mixin
    (standard-slot-definition)
  ((attributes :initarg :attributes :accessor slot-definition-attributes
               :initform nil)))

(defclass attributes-direct-slot-definition
    (standard-direct-slot-definition attributes-mixin)
  ())

(defclass attributes-effective-slot-definition
    (standard-effective-slot-definition attributes-mixin)
  ())

(defmethod effective-slot-definition-class ((class attributes-class)
                                            &rest initargs)
  (find-class 'attributes-effective-slot-definition))

(defmethod compute-effective-slot-definition ((class attributes-class)
                                              name direct-slots)
  (let* ((normal-slot (call-next-method)))
    (setf (slot-definition-attributes normal-slot)
          (remove-duplicates
           (apply #'append (mapcar #'slot-definition-attributes 
                                   direct-slots))))
    normal-slot))

(defmethod direct-slot-definition-class
    ((class attributes-class) &rest initargs)
  (find-class 'attributes-direct-slot-definition))

(defmethod process-a-slot-option
    ((class attributes-class) option value
        already-processed-options slot)
  (princ "process-a-slot-option") (princ option)
  (if (eq option :attributes)
      (list* :attributes  `',value already-processed-options)
      (call-next-method)))

(defmethod compute-slots ((class attributes-class))
  (let* ((normal-slots (call-next-method))
         (alist (mapcar (lambda (slot)
                          (cons (slot-definition-name slot)
                                (mapcar (lambda (attr) (cons attr nil))
                                        (slot-definition-attributes 
                                         slot))))
                        normal-slots)))
    (cons (make-instance 'attributes-effective-slot-definition
           :name *all-attributes*
           :initform alist
           :initfunction (lambda () alist))
          normal-slots)))

(defun slot-attribute (instance slot-name attribute)
  (cdr (slot-attribute-bucket instance slot-name attribute)))

(defun (setf slot-attribute) (new-value instance slot-name attribute)
  (setf (cdr (slot-attribute-bucket instance slot-name attribute))
        new-value))

(defun slot-attribute-bucket (instance slot-name attribute)
  (let* ((all-buckets (slot-value instance *all-attributes*))
         (slot-bucket (assoc slot-name all-buckets)))
    (unless slot-bucket
      (error "Slot ~S of ~S has no attributes."
             slot-name instance))
    (let ((attr-bucket (assoc attribute (cdr slot-bucket))))
      (unless attr-bucket
        (error "Slot ~S of ~S has no attribute ~S."
               slot-name instance attribute))
      attr-bucket)))

(defmethod clos:slot-value-using-class
    ((class attributes-class) object (slot-name attributes-effective-slot-definition))
  (call-next-method))

(defmethod (setf clos:slot-value-using-class)
    (value (class attributes-class) object (slot-name attributes-effective-slot-definition))
  (call-next-method))

) ; eval-when

(defclass credit-rating ()
  ((level :attributes (date-set time-set) :accessor level)
   (desc :accessor desc))
  (:metaclass attributes-class))
