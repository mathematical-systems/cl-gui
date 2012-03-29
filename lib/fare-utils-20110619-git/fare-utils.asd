;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
(in-package :cl-user)

(proclaim
 #+sbcl
 '(optimize
   (sb-c::merge-tail-calls 3)
   (sb-c::insert-debug-catch 0)
   (speed 3) (space 3) (debug 2))
 #+clozure
 '(optimize (speed 3) (space 3) (debug 2))
 #-(or sbcl clozure)
 '(optimize (speed 3) (space 3) (debug 1)))

#+clozure
(setf ccl::*compile-time-evaluation-policy*
      (ccl::new-compiler-policy :allow-tail-recursion-elimination (constantly t)))

(asdf:defsystem :fare-utils
  :components
  ((:file "package")

   ;;; Utilities wrt Lisp
   (:module "base"
    :depends-on ("package")
    :components
    ((:file "utils")
     (:file "strings" :depends-on ("utils"))
     (:file "symbols" :depends-on ("strings"))
     (:file "macros" :depends-on ("symbols"))
     (:file "lists" :depends-on ("macros"))
     (:file "packages" :depends-on ("lists"))
     (:file "objects" :depends-on ("macros"))
     (:file "streams" :depends-on ("utils"))
     (:file "hash-tables" :depends-on ("macros"))
     (:file "more-strings" :depends-on ("strings" "streams"))
     (:file "parse-cl-syntax" :depends-on ("macros"))))

   ;;; Utilities wrt Lisp
   (:module "filesystem"
    :depends-on ("base")
    :components
    ((:file "pathnames")
     (:file "files")
     (:file "atomic")))

   ;;; Half-baked stuff
   (:module "unbaked"
    :depends-on ("base")
    :components
    ((:file "msv"))) ; Magic Special Variables

   ;;; Interface-Passing Style generic libraries
   (:module "interface"
    :depends-on ("base")
    :components
    ((:file "memoization")
     (:file "interface")
     (:file "eq" :depends-on ("interface" "memoization"))
     (:file "order" :depends-on ("eq"))))

   ;;; IPS pure functional datastructures
   (:module "pure"
    :depends-on ("interface")
    :components
    ((:file "package")
     (:file "map" :depends-on ("package"))
     (:file "updatef" :depends-on ("package"))
     (:file "updatef-expanders" :depends-on ("updatef"))
     (:file "alist" :depends-on ("map"))
     (:file "tree" :depends-on ("map" "alist"))
     (:file "hash-table" :depends-on ("tree"))
     (:file "fmim" :depends-on ("map" "tree"))
     (:file "encoded-key-map" :depends-on ("map"))))

   ;;; Stateful containers
   (:module "stateful"
    :depends-on ("interface")
    :components
    ((:file "package")
     (:file "container" :depends-on ("package"))
     (:file "binary-heap" :depends-on ("container"))
     (:file "binomial-heap" :depends-on ("container"))
     (:file "fifo" :depends-on ("container"))
     (:file "dllist" :depends-on ("container"))
     (:file "sorting" :depends-on ("binary-heap" "binomial-heap"))))))
