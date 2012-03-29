;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

(def special-variable *temporary-file-random-state* (make-random-state t))
(def global-variable *temporary-file-unique-counter* (make-atomic-counter))

(def special-variable *directory-for-temporary-files* nil
  "Holds the runtime value of the temporary directory, which includes the PID of the process.")

(def constant +temporary-directory-name-prefix+ "hu.dwim-")

(def (function e) directory-name-for-temporary-files (&key (pid (isys:getpid)))
  ;; NOTE: unexpressed abstraction: this file name structure is assumed in CLEANUP-TEMPORARY-DIRECTORIES
  (string+ (iolib.pathnames:file-path-namestring iolib.os:*temporary-directory*)
           "/"
           +temporary-directory-name-prefix+
           (integer-to-string pid)
           "/"))

(def (function e) directory-for-temporary-files ()
  (or *directory-for-temporary-files*
      (bind ((dir (directory-name-for-temporary-files)))
        (ensure-directories-exist dir) ; TODO use something from iolib.os ?
        (setf *directory-for-temporary-files* (iolib.pathnames:file-path dir)))))

(def (function e) delete-directory-for-temporary-files ()
  (when *directory-for-temporary-files*
    (iolib.os:delete-files *directory-for-temporary-files* :recursive #t)
    (setf *directory-for-temporary-files* nil))
  (values))

(def (function e) cleanup-temporary-directories ()
  "Tries to delete all temporary directories that have been created by this library by a process not running anymore."
  (bind ((deleted ()))
    (flet ((temporary-directory-of-dead-process? (pathname kind)
             (bind ((name (iolib.pathnames:file-path-namestring pathname)))
               (when (and (eq kind :directory)
                          (starts-with-subseq +temporary-directory-name-prefix+ name))
                 (bind (((:values pid position) (ignore-errors (parse-integer name :start (length +temporary-directory-name-prefix+)))))
                   (and pid
                        (= position (length name))
                        (not (posix-process-exists? pid)))))))
           (delete-temporary-directory (pathname kind parent depth)
             (declare (ignore kind parent depth))
             (ignore-errors
               ;; we may be lacking the permission, etc...
               (iolib.os:delete-files pathname :recursive #t)
               (push pathname deleted))))
      (iolib.os:walk-directory iolib.os:*temporary-directory*
                               #'delete-temporary-directory
                               :maxdepth 1
                               :test #'temporary-directory-of-dead-process?))
    deleted))

(def (function e) temporary-file-name (&optional prefix extension)
  (apply #'string+
         (iolib.pathnames:file-path-namestring (directory-for-temporary-files))
         "/"
         prefix
         (when prefix
           "-")
         (integer-to-string (atomic-counter/increment *temporary-file-unique-counter*))
         "-"
         (integer-to-string (random 100000 *temporary-file-random-state*))
         (when extension
           (list "." extension))))

(def (function e) temporary-file-path (&optional prefix extension)
  (iolib.pathnames:file-path (temporary-file-name prefix extension)))

(def (function e) shadow-temporary-file-path (root-directory relative-path temp-subdirectory-name)
  "Returns a filename 'relocated' to the temp directory under TEMP-SUBDIRECTORY-NAME."
  (check-type root-directory (or pathname iolib.pathnames:file-path-designator))
  (check-type relative-path string)
  (assert (not (starts-with #\/ relative-path)))
  (bind ((root-directory (iolib.pathnames:file-path root-directory)))
    (iolib.pathnames:merge-file-paths relative-path
                                      (iolib.pathnames:make-file-path :components (append (iolib.pathnames:file-path-components (directory-for-temporary-files))
                                                                                          (list temp-subdirectory-name)
                                                                                          (rest (iolib.pathnames:file-path-components root-directory)))
                                                                      :defaults (directory-for-temporary-files)))))

(def (function e) shadow-temporary-filename (root-directory relative-path temp-subdirectory-name)
  (iolib.pathnames:file-path-namestring (shadow-temporary-file-path root-directory relative-path temp-subdirectory-name)))

(def (function e) open-temporary-file (&rest args &key
                                             (element-type :default) ; this might be SBCL specific, but we want bivalent by default
                                             (direction :output)
                                             file-name-prefix
                                             file-type)
  (remove-from-plistf args :file-name-prefix :file-type)
  (iter
    (for file-name = (temporary-file-name file-name-prefix file-type))
    (for file = (apply #'open
                       file-name
                       :if-exists nil
                       :direction direction
                       :element-type element-type
                       args))
    (until file)
    (finally (return (values file file-name)))))

(def (with-macro* e :macro-only-arguments variable-name) with-temporary-file (variable-name &rest args)
  (bind ((stream (apply 'open-temporary-file args)))
    (unwind-protect
         (-with-macro/body- (stream variable-name))
      (delete-file stream))))

(def (function e) substitute-illegal-characters-in-file-name (name &key (replacement #\_))
  (substitute-all "/?*\"" replacement name))
