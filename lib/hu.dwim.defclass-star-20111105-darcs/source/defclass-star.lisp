;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.defclass-star)

(enable-sharp-boolean-syntax)

(defmacro make-name-transformer (&rest elements)
  `(lambda (name definition)
    (declare (ignorable definition))
    (concatenate-symbol ,@(mapcar (lambda (el)
                                    (if (and (symbolp el)
                                             (string= (symbol-name el) "NAME"))
                                        'name
                                        el))
                                  elements))))

;; more or less public vars (it's discouraged to set them globally)
(defvar *accessor-name-package* nil
  "A package, or :slot-name means the home-package of the slot-name symbol and nil means *package*")
(defvar *accessor-name-transformer* 'default-accessor-name-transformer)
(defvar *automatic-accessors-p* #t)

;; these control whether the respective names should be exported from *package* (which is samples at macroexpan time)
(defvar *export-class-name-p* nil)
(defvar *export-accessor-names-p* nil)
(defvar *export-slot-names-p* nil)

(defvar *initarg-name-transformer* 'default-initarg-name-transformer)
(defvar *automatic-initargs-p* #t)

(defvar *slot-definition-transformer* 'default-slot-definition-transformer)

(defun default-slot-definition-transformer (slot-def)
  "Converts illegal (list foo) :type declarations into simple list declarations."
  (let ((name (pop slot-def))
        (type (getf slot-def :type)))
    (when (and type (listp type) (eq (first type) 'list))
      (setf (getf slot-def :type) 'list))
    (push name slot-def)
    slot-def))

(defvar *allowed-slot-definition-properties* '(:documentation :type :reader :writer :allocation :export)
  "Holds a list of keywords that are allowed in slot definitions (:accessor and :initarg are implicitly included).")

;; expand-time temporary dynamic vars
(defvar *accessor-names*)
(defvar *slot-names*)
(defvar *symbols-to-export*)

(define-condition hu.dwim.defclass-star-style-warning (simple-condition style-warning)
  ())

(defun style-warn (datum &rest args)
  (warn 'hu.dwim.defclass-star-style-warning :format-control datum :format-arguments args))

(defun slot-name-package (name)
  (if (packagep *accessor-name-package*)
      *accessor-name-package*
      (case *accessor-name-package*
        (:slot-name (symbol-package name))
        (:default *package*)
        (t *package*))))

(defun default-accessor-name-transformer (name definition)
  (let* ((type (getf definition :type))
         (package (slot-name-package name))
         (name-string (string name))
         (last-char (elt name-string (1- (length name-string))))
         (ends-with-question-mark? (char= last-char #\?)))
    (cond
      ((and (eq type 'boolean)
            (not ends-with-question-mark?))
       (cond ((char-equal last-char #\p)
              name)
             ;; i like unconditional '-p' postfix. ymmv.
             #+nil((not (find #\- name-string))
                   (concatenate-symbol name '#:p package))
             (t (concatenate-symbol name '#:-p package))))
      (ends-with-question-mark?
       name)
      (t (concatenate-symbol name '#:-of package)))))

(defun dwim-accessor-name-transformer (name definition)
  (let* ((type (getf definition :type))
         (package (slot-name-package name))
         (name-string (string name))
         (last-char (elt name-string (1- (length name-string)))))
    (if (eq type 'boolean)
        (if (char= last-char #\?)
            name
            (concatenate-symbol name "?" package))
        (concatenate-symbol name '#:-of package))))

(defun default-initarg-name-transformer (name definition)
  (declare (ignorable definition))
  (concatenate-symbol name #.(symbol-package :asdf)))

(defun process-slot-definition (definition)
  (unless (consp definition)
    (setf definition (list definition)))
  (let ((name (pop definition))
        (initform 'missing)
        (entire-definition definition))
    (assert name)
    (push name *slot-names*)
    (if (oddp (length definition))
        (progn
          (setf initform (pop definition))
          (setf entire-definition definition)
          (when (eq initform :unbound)
            (setf initform 'missing)))
        (setf initform (getf definition :initform 'missing)))
    (assert (every #'keywordp (loop for el :in definition :by #'cddr
                                    collect el))
            () "Found non-keywords in ~S" definition)
    (destructuring-bind (&key (accessor 'missing) (initarg 'missing)
                              (reader 'missing) (writer 'missing)
                              (export 'missing)
                              &allow-other-keys)
        definition
      (remf-keywords definition :accessor :reader :writer :initform :initarg :export)
      (let ((unknown-keywords (loop for el :in definition :by #'cddr
                                    unless (or (member t *allowed-slot-definition-properties*)
                                               (member el *allowed-slot-definition-properties*))
                                    collect el))
            (slot-name-warning-triggered? #f))
        (when unknown-keywords
          (style-warn "Unexpected properties in slot definition ~S.~%~
                       The unexpected properties are ~S.~%~
                       To avoid this warning (pushnew (or T :your-custom-keyword) hu.dwim.defclass-star:*allowed-slot-definition-properties*)"
                      entire-definition unknown-keywords))
        (flet ((provided-p (value)
                 (and value
                      (not (eq value 'missing))))
               (transform-accessor ()
                 (funcall *accessor-name-transformer* name entire-definition))
               (maybe-warn-for-slot-name ()
                 (unless (or slot-name-warning-triggered?
                             (eq (symbol-package name) *package*))
                   (setf slot-name-warning-triggered? #t)
                   #+nil ;; this generates too many warnings which makes it kinda pointless
                   (style-warn "defclass* for a slot name ~A while its home package is not *package* (~A). Default generated names will be interned into *package*!"
                               (fully-qualified-symbol-name name) *package*))))
          (prog1
              (funcall *slot-definition-transformer*
                       (append (list name)
                               (unless (eq initform 'missing)
                                 (list :initform initform))
                               (if (and (eq accessor 'missing)
                                        (eq reader 'missing)
                                        (eq writer 'missing))
                                   (when *automatic-accessors-p*
                                     (maybe-warn-for-slot-name)
                                     (setf accessor (transform-accessor))
                                     (list :accessor accessor))
                                   (let ((transformed-accessor (transform-accessor)))
                                     (append (progn
                                               (when (eq accessor t)
                                                 (setf accessor transformed-accessor))
                                               (when (provided-p accessor)
                                                 (list :accessor accessor)))
                                             (progn
                                               (when (eq reader t)
                                                 (setf reader transformed-accessor))
                                               (when (provided-p reader)
                                                 (list :reader reader)))
                                             (progn
                                               (when (eq writer t)
                                                 (setf writer `(setf ,transformed-accessor)))
                                               (when (provided-p writer)
                                                 (list :writer writer))))))
                               (if (eq initarg 'missing)
                                   (when *automatic-initargs-p*
                                     (list :initarg (funcall *initarg-name-transformer* name entire-definition)))
                                   (when initarg
                                     (list :initarg initarg)))
                               definition))
            (when (provided-p accessor)
              (pushnew accessor *accessor-names*))
            (when (provided-p reader)
              (pushnew reader *accessor-names*))
            (when (provided-p writer)
              (pushnew (second writer) *accessor-names*))
            (if (not (eq export 'missing))
                (ecase export
                  (:accessor
                   (when accessor
                     (push accessor *symbols-to-export*)))
                  ((:slot :name :slot-name)
                   (push name *symbols-to-export*))
                  ((t)
                   (when accessor
                     (push accessor *symbols-to-export*))
                   (push name *symbols-to-export*))
                  ((nil)))
                (progn
                  (when *export-accessor-names-p*
                    (when accessor
                      (push accessor *symbols-to-export*)))
                  (when *export-slot-names-p*
                    (push name *symbols-to-export*))))))))))

(defun extract-options-into-bindings (options)
  (let ((binding-names)
        (binding-values)
        (clean-options))
    (macrolet ((rebinding-table (&rest args)
                 `(case (car option)
                   ,@(loop for (arg-name var-name) :on args :by #'cddr
                           collect `(,arg-name
                                     (assert (= (length option) 2))
                                     (push ',var-name binding-names)
                                     (push (second option) binding-values)))
                   (t (push option clean-options)))))
      (dolist (option options)
        (rebinding-table
         :accessor-name-package *accessor-name-package*
         :accessor-name-transformer *accessor-name-transformer*
         :automatic-accessors-p *automatic-accessors-p*
         :initarg-name-transformer *initarg-name-transformer*
         :automatic-initargs-p *automatic-initargs-p*
         :export-class-name-p *export-class-name-p*
         :export-accessor-names-p *export-accessor-names-p*
         :export-slot-names-p *export-slot-names-p*
         :slot-definition-transformer *slot-definition-transformer*)))
    (values binding-names binding-values (nreverse clean-options))))

(defun build-defclass-like-expansion (name supers slots options expansion-builder
                                      &key
                                      (export-class-name *export-class-name-p*)
                                      (export-accessor-names *export-accessor-names-p*)
                                      (export-slot-names *export-slot-names-p*))
  (declare (ignore supers))
  #+nil ;; this generates warnings where defclass would not, delme eventually?
  (unless (eq (symbol-package name) *package*)
    (style-warn "defclass* for ~A while its home package is not *package* (~A)"
                (fully-qualified-symbol-name name) *package*))
  (let ((*accessor-names* nil)
        (*slot-names* nil)
        (*symbols-to-export* nil)
        (*export-class-name-p* export-class-name)
        (*export-accessor-names-p* export-accessor-names)
        (*export-slot-names-p* export-slot-names))
    (multiple-value-bind (binding-names binding-values clean-options)
        (extract-options-into-bindings options)
      (progv binding-names (mapcar #'eval binding-values)
        (let ((result (funcall expansion-builder
                               (mapcar 'process-slot-definition slots)
                               clean-options)))
          (if (or *symbols-to-export*
                  *export-class-name-p*)
              `(progn
                 ,result
                 (eval-when (:compile-toplevel :load-toplevel :execute)
                   (export '(,@(append (when *export-class-name-p*
                                         (list name))
                                       *symbols-to-export*))
                           ,(package-name *package*)))
                 (find-class ',name nil))
              result))))))

(defmacro defclass* (name supers slots &rest options)
  (build-defclass-like-expansion
   name supers slots options
   (lambda (processed-slots clean-options)
     `(defclass ,name ,supers
        ,processed-slots
        ,@clean-options))))

(defmacro defcondition* (name supers slots &rest options)
  (build-defclass-like-expansion
   name supers slots options
   (lambda (processed-slots clean-options)
     `(define-condition ,name ,supers
        ,processed-slots
        ,@clean-options))))

