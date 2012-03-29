;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

;;;;;;
;;; Definition source text database

(def special-variable *definition-source-texts* (make-hash-table :synchronized #t))

(def function clear-definition-source-texts ()
  (clrhash *definition-source-texts*))

;; KLUDGE: all this hassle is to workaround SBCL's bootstrapping package names
;; TODO: this is obviously non thread safe
(def with-macro with-find-package-kludge ()
  (bind ((original-find-package (fdefinition 'find-package))
         (temporary-package (make-package "TEMPORARY")))
    (unwind-protect
         (progn
           (handler-bind ((package-error #'continue))
             (setf (fdefinition 'find-package) (lambda (designator)
                                                 (cond ((equal "SB!XC" designator)
                                                        temporary-package)
                                                       ((stringp designator)
                                                        (funcall original-find-package (substitute #\- #\! designator)))
                                                       (t (funcall original-find-package designator))))))
           (-body-))
      (handler-bind ((package-error #'continue))
        (setf (fdefinition 'find-package) original-find-package)
        (delete-package temporary-package)))))

(def function read-definition-source-text (definition)
  #*((:sbcl
      (bind ((definition-source (if (typep definition 'sb-introspect:definition-source)
                                    definition
                                    (sb-introspect:find-definition-source definition)))
             (pathname (sb-introspect:definition-source-pathname definition-source))
             (form-path (sb-introspect:definition-source-form-path definition-source)))
        (handler-case
            (cond ((null pathname)
                   (values (format nil ";; cannot determine source file for ~A" definition) nil))
                  ((not (length= 1 form-path))
                   (values (format nil ";; cannot process source form path ~A for ~A in ~A" form-path definition pathname)))
                  (t (with-input-from-file (stream pathname :element-type 'character :external-format (sb-impl::default-external-format))
                       (with-find-package-kludge
                         (bind ((first-form-index (car form-path))
                                (*readtable* (swank-backend::shebang-readtable))
                                (*package* (find-package :common-lisp-user)))
                           (iter (for index :from 0)
                                 (for form = (handler-bind ((sb-int::simple-reader-package-error #'continue))
                                               (read stream #f stream)))
                                 (for position = (file-position stream))
                                 (for previous-position :previous position)
                                 (declare (ignorable index position previous-position))
                                 (until (eq form stream))
                                 (when (member (first form) '(common-lisp:in-package hu.dwim.common:in-package))
                                   (setf *package* (find-package (second form)))
                                   (awhen (cdr (assoc (package-name *package*) swank:*readtable-alist* :test 'string=))
                                     (setf *readtable* it)))
                                 (when (= first-form-index index)
                                   (file-position stream previous-position)
                                   (return-from read-definition-source-text
                                     (values (string-trim-whitespace (iter (for char = (read-char stream nil stream))
                                                                           (until (or (= position (file-position stream))
                                                                                      (eq char stream)))
                                                                           (collect char :result-type '(vector character))))
                                             *package*)))
                                 (finally
                                  (return (values (format nil ";; cannot find source form ~A for ~A in ~A" form-path definition pathname) nil)))))))))
          (serious-condition (condition)
            (return-from read-definition-source-text
              (values (format nil ";; received ~A during reading source for ~A" condition definition) nil))))))
     (t
      (values (format nil ";; cannot determine source file for ~A" definition) nil))))

(def (function e) definition-source-text (definition)
  (cdr (or (gethash definition *definition-source-texts*)
           (setf (gethash definition *definition-source-texts*)
                 (bind (((:values text package) (read-definition-source-text definition)))
                   (cons package text))))))

;;;;;;
;;; Definition source form database

(def special-variable *definition-source-forms* (make-hash-table :synchronized #t))

(def function clear-definition-source-forms ()
  (clrhash *definition-source-forms*))

(def function parse-definition-source-form (definition)
  (awhen (definition-source-text definition)
    (with-input-from-string (stream it)
      (with-find-package-kludge
        (bind ((*package* (car (gethash definition *definition-source-texts*)))
               (*readtable* (copy-readtable (swank-backend::shebang-readtable))))
          (awhen (find-extended-package (package-name *package*) :otherwise #f)
            (eval (hu.dwim.def::readtable-setup-form-of it)))
          (read stream nil nil))))))

(def (function e) definition-source-form (definition)
  (or (gethash definition *definition-source-forms*)
      (setf (gethash definition *definition-source-forms*)
            (parse-definition-source-form definition))))
