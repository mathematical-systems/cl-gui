;; -*- Mode: common-lisp; Package: cl-user -*-

(in-package :cl-user)

(eval-when (:compile-toplevel)
  (error "This file is not intended to be compiled!"))

(load "/huang/workspace/cl-gui/utils/..//loader.lisp")

(load "test.asd")

(asdf:oos 'asdf:load-op :test)

(in-package :cl-gui)


