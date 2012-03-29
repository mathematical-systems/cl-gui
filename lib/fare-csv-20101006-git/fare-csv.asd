;;; -*- Lisp -*-
(in-package :cl)

(asdf:defsystem #:fare-csv
  :depends-on ()
  :licence "MIT"
  :components ((:file "package") (:file "csv"))
  :serial t)
