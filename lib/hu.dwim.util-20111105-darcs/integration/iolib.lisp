;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

(with-muffled-redefinition-warnings
  (def (function o :inline :possible) get-monotonic-time ()
    "Returns such a time measure that constantly grows (it's a number in seconds, and it's unaffected by setting the system clock)."
    (iolib.os:get-monotonic-time)))

(def (function e) posix-process-exists? (pid)
  (ignore-errors
    (isys:kill pid 0)
    #t))

(def (function e) is-file-executable? (file &key
                                            (follow-symlinks #t)
                                            (effective-user-id (iolib.syscalls:getuid))
                                            (effective-group-id (iolib.syscalls:getgid)))
  (check-type file iolib.pathnames:file-path-designator)
  (check-type effective-user-id (or integer string))
  (check-type effective-group-id (or integer string))
  (flet ((group-info (id)
           ;; TODO move to iolib just like user-info?
           (multiple-value-bind (name password gid)
               (etypecase id
                 (string  (isys:getgrnam id))
                 (integer (isys:getgrgid id)))
             (declare (ignore password))
             (if name
                 (list (cons :name name)
                       (cons :group-id gid))
                 nil))))
    (bind ((permissions (iolib.os:file-permissions (iolib.pathnames:file-path file))))
      (to-boolean
       (and (eq :regular-file (iolib.os:file-kind file :follow-symlinks follow-symlinks))
            (or (find :other-exec permissions)
                (bind ((stat (iolib.syscalls:stat file))
                       ((:values nil nil owner-id) (iolib.syscalls:getpwuid (iolib.syscalls:stat-uid stat)))
                       ((:values nil nil group-id) (iolib.syscalls:getgrgid (iolib.syscalls:stat-gid stat))))
                  (or (and (find :user-exec permissions)
                           (= owner-id (assoc-value (iolib.os:user-info effective-user-id) :user-id)))
                      (and (find :group-exec permissions)
                           (= group-id (assoc-value (group-info effective-group-id) :group-id)))))))))))
