;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.asdf)

;;;;;;
;;; Package support

(defclass system-with-package (system)
  ((package-name
    :initarg :package-name
    :accessor system-package-name)))

(defmethod reinitialize-instance :after ((system system-with-package) &rest args &key &allow-other-keys)
  (declare (ignore args))
  (unless (slot-boundp system 'package-name)
    (setf (system-package-name system)
          (string-upcase (component-name system)))))

(defclass system-with-target ()
  ((target-system-name
    :initarg :target-system-name
    :accessor system-target-system-name)))

(defmethod reinitialize-instance :after ((system system-with-target) &rest args &key &allow-other-keys)
  (declare (ignore args))
  (let* ((system-name (string-downcase (component-name system)))
         (last-dot-position (position #\. system-name :from-end t)))
    (unless (slot-boundp system 'target-system-name)
      (setf (system-target-system-name system)
            (subseq system-name 0 last-dot-position)))
    (let ((target-system (find-system (system-target-system-name system) nil)))
      (when target-system
        (when (and (slot-boundp target-system 'asdf::author)
                   (not (slot-boundp system 'asdf::author)))
          (setf (system-author system)
                (system-author target-system)))
        (when (and (slot-boundp target-system 'asdf::licence)
                   (not (slot-boundp system 'asdf::licence)))
          (setf (system-licence system)
                (system-licence target-system)))
        (unless (slot-boundp system 'asdf::description)
          (setf (system-description system)
                (concatenate 'string
                             (string-capitalize (subseq system-name (1+ last-dot-position)))
                             " for "
                             (system-target-system-name system))))))))

;;;;;;
;;; DWIM system

(defvar *muffle-optimization-warnings* t)

(defclass hu.dwim.cl-source-file (cl-source-file)
  ())

(defclass system-with-output ()
  ((compile-output
    :initform nil
    :initarg :compile-output
    :accessor system-compile-output)
   (load-output
    :initform nil
    :initarg :load-output
    :accessor system-load-output)))

(defclass hu.dwim.system (system-with-output system-with-package)
  ((test-system-name
    :initarg :test-system-name
    :accessor system-test-system-name)
   (documentation-system-name
    :initarg :documentation-system-name
    :accessor system-documentation-system-name))
  (:default-initargs
   :licence "BSD / Public domain"
   :author '("Tamás Borbély <tomi.borbely@gmail.com>"
             "Attila Lendvai <attila.lendvai@gmail.com>"
             "Levente Mészáros <levente.meszaros@gmail.com>")))

(defclass hu.dwim.test-system (system-with-output system-with-target system-with-package)
  ((test-name
    :initform "TEST"
    :initarg :test-name
    :accessor system-test-name)
   (test-result
    :initform nil
    :initarg :test-result
    :accessor system-test-result)
   (test-output
    :initform nil
    :initarg :test-output
    :accessor system-test-output)))

(defclass hu.dwim.documentation-system (system-with-output system-with-target system-with-package)
  ())

(defmacro with-capturing-output (place &body forms)
  (let ((stream (gensym "STREAM")))
    `(let* ((,stream (make-string-output-stream))
            (*standard-output* (make-broadcast-stream *standard-output* ,stream))
            (*error-output* (make-broadcast-stream *error-output* ,stream)))
       ,@forms
       (setf ,place (concatenate 'string ,place (get-output-stream-string ,stream))))))

(defmethod reinitialize-instance :after ((system hu.dwim.system) &rest args &key &allow-other-keys)
  (declare (ignore args))
  (unless (slot-boundp system 'test-system-name)
    (setf (system-test-system-name system)
          (concatenate 'string (string-downcase (component-name system)) ".test")))
  (unless (slot-boundp system 'documentation-system-name)
    (setf (system-documentation-system-name system)
          (concatenate 'string (string-downcase (component-name system)) ".documentation"))))

(defmethod asdf::module-default-component-class ((class hu.dwim.test-system))
  'hu.dwim.cl-source-file)

(defmethod asdf::module-default-component-class ((class hu.dwim.system))
  'hu.dwim.cl-source-file)

(defmethod perform :around ((op operation) (component hu.dwim.cl-source-file))
  (let ((*features* *features*)
        (*readtable* (copy-readtable *readtable*))
        (*package* *package*)
        (hu.dwim.common-package (find-package :hu.dwim.common)))
    (when hu.dwim.common-package
      ;; when the hu.dwim.common package is available, then we read lisp files into that, so that hu.dwim.common:in-package can shadow cl:in-package.
      ;; see hu.dwim.def/source/extended-package.lisp for more info.
      (setf *package* hu.dwim.common-package))
    (unless hu.dwim.asdf:*load-as-production?*
      (pushnew :debug *features*))
    (call-in-system-environment op (component-system component) #'call-next-method)))

(defmethod perform :around ((op compile-op) (component hu.dwim.cl-source-file))
  (with-capturing-output (system-compile-output (component-system component))
    (call-next-method)))

(defmethod perform :around ((op load-op) (component hu.dwim.cl-source-file))
  (with-capturing-output (system-load-output (component-system component))
    (call-next-method)))

(defun call-with-muffled-boring-compiler-warnings (thunk)
  (handler-bind (#+sbcl(sb-ext:compiler-note #'muffle-warning)
                 ;; NOTE: muffle these warnings to reduce compilation noise, tests already cover interesting cases
                 #+sbcl(sb-kernel:undefined-alien-style-warning #'muffle-warning))
    (funcall thunk)))

(defmacro with-muffled-boring-compiler-warnings (&body body)
  `(call-with-muffled-boring-compiler-warnings (lambda () ,@body)))

(defmacro with-ignored-boring-compiler-warnings (&body body)
  `(locally (declare #+sbcl(sb-ext:muffle-conditions style-warning sb-ext:compiler-note))
     ,@body))

(defgeneric call-in-system-environment (operation system function)
  (:method ((op operation) (system system) function)
    (if *muffle-optimization-warnings*
        (call-with-muffled-boring-compiler-warnings function)
        (funcall function))))

(defmethod perform ((op test-op) (system hu.dwim.system))
  (let ((test-system (find-system (system-test-system-name system) nil)))
    (if (typep test-system 'hu.dwim.test-system)
        (progn
          (load-system test-system)
          (run-test-suite test-system))
        (warn "There is no test system for ~A, no tests were run." system))))

(defgeneric run-test-suite (system)
  (:method ((system system))
    (warn "Don't know how to run tests suite for ~A" system))
  (:method :around ((system hu.dwim.test-system))
    (with-capturing-output (system-test-output system)
      (setf (system-test-result system) (call-next-method))))
  (:method ((system hu.dwim.test-system))
    (if (find-package :hu.dwim.stefil)
        (let ((package-name (system-package-name system)))
          (if package-name
              (let ((test-name (find-symbol (system-test-name system) package-name)))
                ;; KLUDGE: ASDF grabs the Big Compiler Lock by using WITH-COMPILATION-UNIT
                ;; KLUDGE: this causes deadlock in (test-system :hu.dwim.rdbms.postgresql)
                (unwind-protect
                     (progn
                       #+sbcl
                       (sb-thread::release-mutex sb-c::**world-lock**)
                       (funcall (find-symbol "FUNCALL-TEST-WITH-FEEDBACK-MESSAGE" :hu.dwim.stefil) test-name))
                  (progn
                    #+sbcl
                    (sb-thread::get-mutex sb-c::**world-lock**))))
              (warn "There is no test package for ~A, no tests were run." system)))
        (call-next-method))))

;;;;;;
;;; Develop

(defvar *development-package*)

(defclass develop-op (operation)
  ())

(defmethod operation-done-p ((operation develop-op) (component component))
  nil)

(defmethod perform ((operation develop-op) (component component))
  nil)

(defmethod perform ((operation develop-op) (system system))
  (load-system system)
  (let ((package (find-package (string-upcase (component-name system)))))
    (when package
      (setf *development-package* package))))

(defmethod perform :before ((operation develop-op) (system system))
  (with-simple-restart (continue "Give up loading Swank and continue...")
    (load-system :swank)
    (set (read-from-string "swank:*globally-redirect-io*") t)))

(defmethod perform :after ((operation develop-op) (system system))
  (load-system :hu.dwim.debug)
  (use-package :hu.dwim.debug :hu.dwim.common)
  (do-external-symbols (symbol :hu.dwim.debug)
    (export symbol :hu.dwim.common))
  (find-and-load-swank-integration-systems)
  (declaim (optimize (debug 3)))
  (pushnew :debug *features*)
  (warn "Pushed :debug in *features* and issued (declaim (optimize (debug 3))) to help later C-c C-c'ing"))

(defmethod perform ((operation develop-op) (system hu.dwim.system))
  (let ((test-system (find-system (system-test-system-name system) nil)))
    (if test-system
        (load-system test-system)
        (load-system system))
    (let ((package (find-package (system-package-name (or test-system system)))))
      (when package
        (setf *development-package* package)))))

(defun develop-system (system &rest args &key force (verbose t) version)
  "Shorthand for `(operate 'asdf:develop-op system)`. See [operate][] for details."
  (declare (ignore force verbose version))
  (let ((*development-package* nil))
    (multiple-value-prog1
        (apply #'operate 'develop-op system args)
      (when *development-package*
        (setf *package* *development-package*)
        (warn "Changed *package* to ~A" *package*)))))

;;;;;;
;;; Util

(defun system-pathname (name)
  (component-pathname (find-system name)))

(defun system-directory (name)
  (make-pathname :directory (pathname-directory (system-pathname name))))

(defun system-loaded-p (system-name)
  (let ((system (find-system system-name)))
    (when system
      (gethash 'load-op (asdf::component-operation-times system)))))

(defun map-asdf-source-registry-directories (visitor)
  (loop
    :for asd-file :being :the :hash-value :of asdf::*source-registry*
    :do (funcall visitor (make-pathname :directory (pathname-directory asd-file)))))

(defun find-all-swank-integration-systems ()
  (map-asdf-source-registry-directories
   (lambda (directory)
     (dolist (file (directory (merge-pathnames directory (make-pathname :name :wild :type "asd"))))
       (let ((name (pathname-name file)))
         (when (and (search "hu.dwim" name)
                    (search "+swank" name))
           (find-system name)))))))

(defun load-swank-integration-systems ()
  (maphash (lambda (name system-specification)
             (let ((system (cdr system-specification)))
               (when (and (search "+swank" name)
                          (not (system-loaded-p name))
                          (every 'system-loaded-p (collect-system-dependencies system)))
                 (load-system system))))
           asdf::*defined-systems*))

(defun find-and-load-swank-integration-systems ()
  (find-all-swank-integration-systems)
  (load-swank-integration-systems))

(defun find-system/for-dependency-specification (spec)
  (cond
    ((and (consp spec)
          (eq (first spec) :version))
     (find-system (second spec)))
    ((and spec
          (symbolp spec))
     (find-system spec))
    (t
     (error "~S: Don't know how to deal with dependency specification ~S" 'find-system/for-dependency-specification spec))))

(defun %iterate-system-dependencies-1 (function system)
  (check-type system system)
  (dolist (spec (rest (find 'load-op (component-depends-on 'load-op system) :key 'first)))
    (funcall function (find-system/for-dependency-specification spec))))

(defun iterate-system-dependencies (function system &key (transitive nil))
  (unless (typep system 'system)
    (setf system (find-system system)))
  (if transitive
      (let ((dependencies '()))
        (labels ((recurse (system)
                   (%iterate-system-dependencies-1 (lambda (dependency)
                                                     (unless (member dependency dependencies)
                                                       (push dependency dependencies)
                                                       (recurse dependency)))
                                                   system)))
          (recurse system)
          (map nil function dependencies)))
      (%iterate-system-dependencies-1 function system))
  (values))

(defun map-system-dependencies (function system &key (transitive nil))
  (let ((result '()))
    (iterate-system-dependencies (lambda (dependency)
                                   (push (funcall function dependency) result))
                                 system :transitive transitive)
    result))

(defun collect-system-dependencies (system &key (transitive nil))
  (map-system-dependencies 'identity system :transitive transitive))

(defmacro do-system-dependencies ((variable-name system-name &key (transitive nil)) &body body)
  (let ((body-fn (gensym "DSD-BODY")))
    `(block nil
       (flet ((,body-fn (,variable-name)
                ,@body))
         (iterate-system-dependencies #',body-fn ,system-name :transitive ,transitive)))))

(reinitialize-instance (change-class (find-system :hu.dwim.asdf) 'hu.dwim.system))

#+sbcl
;; KLUDGE: TODO: this is an ugly hack to work around the bug https://bugs.launchpad.net/sbcl/+bug/501075
(sb-ext::without-package-locks
  (defun sb-impl::line-length (&optional (stream *standard-output*))
    (declare (ignore stream))
    160))
