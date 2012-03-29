;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Miscellaneous object-related utilities by Fare

#+xcvb (module (:depends-on ("package" "base/utils" "base/macros")))

(in-package :fare-utils)

(def*macro accessors-equal-p ((&key (test '#'equal) (prefix "")) accessors obj1 obj2)
  (evaluating-once (test obj1 obj2)
    `(and ,@(loop :for x :in accessors
                  :for slot = (if (symbolp x) x (car x))
                  :for tst = (if (symbolp x) test (cadr x))
                  :for fun = (conc-symbol prefix slot)
                  :collect `(funcall ,tst (,fun ,obj1) (,fun ,obj2))))))
