(in-package :cl-gui)

(eval-when (:compile-toplevel)
  (error "This file is not intended to be compiled!"))

(load "twitter-nmf.asd")

(asdf:oos 'asdf:load-op :twitter-nmf)

