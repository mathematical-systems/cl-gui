;; -*- Mode: common-lisp; Package: cl-user -*-

(in-package :cl-user)

(asdf:defsystem :test
  :serial t
  :components
  ((:module src
    :components (;; (:file "package")
                 )
    :serial t)
   (:module client-src
    :components (;; (:ps-file "main")
                 )
    :serial t))
  :depends-on (:cl-gui))

