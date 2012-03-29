;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009-2011 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.asdf)

(defvar *workspace-directory*
  (truename (or (asdf:getenv "DWIM_WORKSPACE")
                (let ((dir (system-relative-pathname :hu.dwim.asdf "../")))
                  (warn "Initialized ~S using the path of the ASDF system called :hu.dwim.asdf to ~A (beware of the possibility that it's been symlinked somewhere else)"
                        '*workspace-directory* dir)
                  dir))))

(defvar *swank-directory* nil)

(defun initialize-asdf-source-registry (directories &key (excluded-directories '()) (inherit-configuration? nil) (insert-at :tail) additional-entries
                                        (swank-directory *swank-directory*))
  (check-type inherit-configuration? boolean)
  (check-type swank-directory (or null string pathname))
  (unless (consp directories)
    (setf directories (list directories)))
  (let ((entries `((:also-exclude ,@excluded-directories))))
    (labels ((collect-directories (root-directory)
               (mapcar (lambda (el)
                         (list :directory el))
                       (collect-directories-for-source-registry root-directory)))
             (extend-with (path)
               (ecase insert-at
                 (:head (setf entries (append (collect-directories path) entries)))
                 (:tail (setf entries (append entries (collect-directories path)))))))
      (map nil #'extend-with directories)
      ;; iolib has its *.asd's inside its src directory
      (extend-with (merge-pathnames* (coerce-pathname "iolib/") *workspace-directory*))
      (extend-with (merge-pathnames* (coerce-pathname "global/iolib/") *workspace-directory*))
      ;; lispbuilder has several subdirectories for each subproject
      (extend-with (merge-pathnames* (coerce-pathname "lispbuilder/") *workspace-directory*))
      (extend-with (merge-pathnames* (coerce-pathname "global/lispbuilder-dev/") *workspace-directory*))
      (initialize-source-registry (append '(:source-registry)
                                          entries
                                          additional-entries
                                          (when swank-directory
                                            `((:directory ,swank-directory)))
                                          (list (if inherit-configuration?
                                                    :inherit-configuration
                                                    :ignore-inherited-configuration)))))))

(defun collect-directories-for-source-registry (root-directory &key (process-outside-links t))
  (format *debug-io* "; Collecting directories for the source registry under ~S~%" root-directory)
  (setf root-directory (ignore-errors (truename root-directory)))
  (unless root-directory
    (return-from collect-directories-for-source-registry))
  (let ((result ()))
    (flet ((collect (directory)
             (unless (find directory result :test 'equal)
               (push directory result)
               (format *debug-io* "; Collecting ~A~%" directory)))
           (include-directory? (directory-name)
             (and (not (member (elt directory-name 0) '(#\_ #\.)))
                  ;; ignore anything with "slime" in its name. the right version was already put in the asdf source registry before us even loaded...
                  (not (search "slime" directory-name :test 'equalp))))
           (external-symlink? (directory)
             (loop for a in (pathname-directory directory)
                   for b in (pathname-directory root-directory)
                   while b do
                   (unless (equal a b)
                     (return nil))
                   finally (return t))))
      (dolist (candidate-directory (subdirectories root-directory))
        (when (directory-pathname-p candidate-directory)
          (let ((directory-name (car (last (pathname-directory candidate-directory)))))
            ;; skip dirs starting with a _ and .
            (when (and (include-directory? directory-name)
                       (or process-outside-links
                           (not (external-symlink? candidate-directory)))
                       (directory (merge-pathnames* "*.asd" candidate-directory)))
              (collect candidate-directory))))))
    result))
