;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

;;;;;;
;;; Compact class and friends

(def (class e) compact-class (standard-class)
  ((compact-words-count
    :initform nil
    :type (or null integer)
    :accessor compact-words-count-of
    :documentation "Number of compact words used to store slot values with :allocation :compact. Each word holds a fixnum representing multiple compact slot values.")
   (compact-words-offset
    :initform nil
    :type (or null integer)
    :accessor compact-words-offset-of
    :documentation "Word offset of compact words in the slot vector. Slots with :allocation :instance go first and slots with :allocation :compact go last."))
  (:documentation "A metaclass that provides compact memory representation for various slot types. For example, a boolean type can be represented on 1 bit. Compact classes may have compact and standard slots at the same time. A compact class will pack multiple slot values into one or more compact words. There are couple of limitations though, that is a single compact slot value will not span accross multiple compact words and unbound slots are currently not supported."))

(def class compact-slot-definition (standard-slot-definition)
  ()
  (:documentation "Base class for direct and effective compact slot definitions."))

(def class compact-direct-slot-definition (compact-slot-definition standard-direct-slot-definition)
  ())

(def class compact-effective-slot-definition (compact-slot-definition standard-effective-slot-definition)
  ((compact-word-offset
    :initform nil
    :type integer
    :accessor compact-word-offset-of
    :documentation "Word offset of the compact word in the slot vector.")
   (compact-bits-offset
    :initform nil
    :type integer
    :accessor compact-bits-offset-of
    :documentation "Bit position used within the compact word.")
   (compact-bit-size
    :initform nil
    :type integer
    :accessor compact-bit-size-of
    :documentation "Bit size used within the compact word.")
   (compact-reader
    :type function
    :accessor compact-reader-of
    :documentation "A one parameter function to read the slot value from an instance.")
   (compact-writer
    :type function
    :accessor compact-writer-of
    :documentation "A two parameter function to write the slot value into an instance.")))

(def class compact-accessor-method (standard-accessor-method)
  ())

(def class compact-reader-method (compact-accessor-method standard-reader-method)
  ())

(def class compact-writer-method (compact-accessor-method standard-writer-method)
  ())

;;;;;;
;;; Meta object protocol

(def method validate-superclass ((subclass compact-class) (superclass standard-class))
  (subtypep (class-of subclass) (class-of superclass)))

(def method direct-slot-definition-class ((class compact-class) &key allocation &allow-other-keys)
  (if (eq allocation :compact)
      (find-class 'compact-direct-slot-definition)
      (call-next-method)))

(def method effective-slot-definition-class ((class compact-class) &key allocation &allow-other-keys)
  (if (eq allocation :compact)
      (find-class 'compact-effective-slot-definition)
      (call-next-method)))

(def method finalize-inheritance :after ((class compact-class))
  ;; a suboptimal eager solution for the packing problem
  (iter (with compact-words-count = 0)
        (with compact-words-offset = (setf (compact-words-offset-of class) (sb-pcl::wrapper-no-of-instance-slots (sb-pcl::class-wrapper class))))
        (with compact-slots = (remove-if-not (of-type 'compact-effective-slot-definition) (class-slots class)))
        (for compact-slot :in compact-slots)
        (for compact-type = (slot-definition-type compact-slot))
        (for compact-bit-size = (aif (type-instance-count-upper-bound compact-type)
                                     (ceiling (log it 2))
                                     (error "Unknown compact type ~A" compact-type)))
        (when (> compact-bit-size #.(integer-length most-positive-fixnum))
          (error "Compact bit size is too large for ~A" compact-slot))
        (setf (compact-bit-size-of compact-slot) compact-bit-size)
        (iter (for compact-word-offset :from compact-words-offset :to (+ 1 compact-words-offset compact-words-count))
              (for compact-bits-offset = (or (iter (for compact-slot :in compact-slots)
                                                   (when (equal compact-word-offset (compact-word-offset-of compact-slot))
                                                     (maximize (+ (compact-bits-offset-of compact-slot)
                                                                  (compact-bit-size-of compact-slot)))))
                                             0))
              (when (<= (+ compact-bits-offset compact-bit-size) #.(integer-length most-positive-fixnum))
                (finish))
              (finally
               (setf (compact-word-offset-of compact-slot) compact-word-offset
                     (compact-bits-offset-of compact-slot) compact-bits-offset)
               (when (= compact-word-offset (+ compact-words-offset compact-words-count))
                 (incf compact-words-count))))
        (finally
         (setf (compact-words-count-of class) compact-words-count)))
  ;; setup accessors
  (dolist (slot (class-slots class))
    (when (typep slot 'compact-effective-slot-definition)
      (bind ((reader-function (make-compact-slot-reader slot))
             (writer-function (make-compact-slot-writer slot)))
        (setf (compact-reader-of slot) reader-function)
        (setf (compact-writer-of slot) writer-function)
        (flet ((ensure-accessor (name-provider specializers lambda-list method-class lambda-form)
                 (bind ((name (some (lambda (class)
                                      (some (lambda (direct-slot) (first (funcall name-provider direct-slot)))
                                            (class-direct-slots class)))
                                    (class-precedence-list class))))
                   (when name
                     (bind ((accessor-generic (ensure-generic-function name :lambda-list lambda-list))
                            (accessor-method (find-method accessor-generic nil specializers nil)))
                       (when accessor-method
                         (remove-method accessor-generic accessor-method))
                       (ensure-method accessor-generic lambda-form :lambda-list lambda-list :specializers specializers :method-class method-class))))))
          (ensure-accessor #'slot-definition-readers (list class) '(instance) (ensure-finalized (find-class 'compact-reader-method))
                           `(lambda (instance)
                              (declare #.(optimize-declaration))
                              (funcall ,reader-function instance)))
          (ensure-accessor #'slot-definition-writers (list (find-class t) class) '(new-value instance) (ensure-finalized (find-class 'compact-writer-method))
                           `(lambda (new-value instance)
                              (declare #.(optimize-declaration))
                              (funcall ,writer-function new-value instance))))))))

(def method allocate-instance ((class compact-class) &rest args)
  (declare (ignore args))
  ;; mostly copied over from SBCL
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  (bind ((compact-word-count (compact-words-count-of class))
         (compact-word-offset (compact-words-offset-of class))
         (wrapper (sb-pcl::class-wrapper class))
         (instance (sb-pcl::%make-standard-instance nil (sb-pcl::get-instance-hash-code)))
         (slot-count (+ compact-word-count (sb-pcl::wrapper-no-of-instance-slots wrapper)))
         (slot-vector (make-array slot-count :initial-element sb-pcl::+slot-unbound+)))
    ;; compact slot values are initialized to zero whatever that means (i.e. there's no unbound slot support now)
    (iter (repeat compact-word-count)
          (for index :from compact-word-offset)
          (setf (aref slot-vector index) 0))
    (setf (sb-pcl::std-instance-wrapper instance) wrapper
          (sb-pcl::std-instance-slots instance) slot-vector)
    instance))

(def method slot-boundp-using-class ((class compact-class) (instance standard-object) (slot compact-effective-slot-definition))
  ;; compact slots can not be unbound yet
  #t)

(def method slot-makunbound-using-class ((class compact-class) (instance standard-object) (slot compact-effective-slot-definition))
  ;; compact slots can not be unbound yet
  (error "The slot ~A in ~A with :compact :allocation cannot be unbound" slot instance))

(def method slot-value-using-class ((class compact-class) (instance standard-object) (slot compact-effective-slot-definition))
  (funcall (compact-reader-of slot) instance))

(def method (setf slot-value-using-class) (new-value (class compact-class) (instance standard-object) (slot compact-effective-slot-definition))
  (funcall (compact-writer-of slot) new-value instance))

(def function make-compact-slot-reader (slot)
  (bind ((compact-word-offset (compact-word-offset-of slot))
         (compact-bits-offset (compact-bits-offset-of slot))
         (compact-bit-size (compact-bit-size-of slot))
         (type (slot-definition-type slot)))
    (declare (type (integer 0 #.(integer-length most-positive-fixnum)) compact-bit-size compact-bits-offset)
             (type fixnum compact-word-offset))
    (flet ((%slot-value (instance)
             (declare #.(optimize-declaration))
             (the fixnum (ldb (byte compact-bit-size compact-bits-offset)
                              (the fixnum (standard-instance-access instance compact-word-offset))))))
      (declare (inline %slot-value))
      (cond ((subtypep type 'boolean)
             (lambda (instance)
               (declare #.(optimize-declaration))
               (= (%slot-value instance) 1)))
            ((subtypep type 'integer)
             (lambda (instance)
               (declare #.(optimize-declaration))
               (%slot-value instance)))
            ((subtypep type 'base-char)
             (lambda (instance)
               (declare #.(optimize-declaration))
               (code-char (%slot-value instance))))
            ((subtypep type 'single-float)
             (lambda (instance)
               (declare #.(optimize-declaration))
               (sb-vm::make-single-float (%slot-value instance))))
            ((and (subtypep type 'simple-base-string)
                  (consp type))
             (lambda (instance)
               (declare #.(optimize-declaration))
               (iter (with value = (%slot-value instance))
                     (with string = (make-string (second type)))
                     (for index :from 0 :below (the fixnum (second type)))
                     (for position :initially 0 :then (+ 7 position))
                     (declare (type fixnum index position))
                     (setf (aref string index) (code-char (ldb (byte 7 position) value)))
                     (finally (return string)))))
            (t
             (aif (type-instance-count-upper-bound type)
                  (bind ((instance-list (type-instance-list type)))
                    (lambda (instance)
                      (elt instance-list (%slot-value instance))))
                  (error "Unknown compact type ~A" type)))))))

(def function make-compact-slot-writer (slot)
  (bind ((compact-word-offset (compact-word-offset-of slot))
         (compact-bit-size (compact-bit-size-of slot))
         (compact-bits-offset (compact-bits-offset-of slot))
         (type (slot-definition-type slot)))
    (declare (type (integer 0 #.(integer-length most-positive-fixnum)) compact-bit-size compact-bits-offset)
             (type fixnum compact-word-offset))
    (flet ((%set-slot-value (new-value instance)
             (declare #.(optimize-declaration))
             (setf (standard-instance-access instance compact-word-offset)
                   (dpb new-value (byte compact-bit-size compact-bits-offset)
                        (the fixnum (standard-instance-access instance compact-word-offset))))))
      (declare (inline %set-slot-value))
      (cond ((subtypep type 'boolean)
             (lambda (new-value instance)
               (declare #.(optimize-declaration))
               (%set-slot-value (if new-value 1 0) instance)))
            ((subtypep type 'integer)
             (lambda (new-value instance)
               (declare #.(optimize-declaration))
               (%set-slot-value new-value instance)))
            ((subtypep type 'base-char)
             (lambda (new-value instance)
               (declare #.(optimize-declaration))
               (%set-slot-value (char-code new-value) instance)))
            ((subtypep type 'single-float)
             (lambda (new-value instance)
               (declare #.(optimize-declaration))
               (%set-slot-value (sb-vm::single-float-bits new-value) instance)))
            #+nil
            ((and (subtypep type 'simple-base-string)
                  (consp type))
             (lambda (new-value instance)
               (declare #.(optimize-declaration))
               (%set-slot-value (iter (with value = 0)
                                      (for char :in-sequence (coerce new-value 'simple-base-string))
                                      (for position :initially 0 :then (+ 7 position))
                                      (setf value (dpb (char-code char) (byte 7 position) value))
                                      (finally (return value)))
                                instance)))
            (t
             (if (type-instance-count-upper-bound type)
                 (bind ((instance-list (type-instance-list type)))
                   (declare (type list instance-list))
                   (lambda (new-value instance)
                     (declare #.(optimize-declaration))
                     (%set-slot-value (position new-value instance-list) instance)))
                 (error "Unknown compact type ~A" type)))))))
