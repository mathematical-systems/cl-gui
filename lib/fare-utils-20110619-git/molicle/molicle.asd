(in-package :asdf)

(defsystem :molicle
  :depends-on (:fare-utils)
  :components
  ((:file "pkgdcl")
   (:file "evaluation-time" :depends-on ("pkgdcl"))
   (:file "molicle" :depends-on ("evaluation-time"))
   (:file "build" :depends-on ("molicle"))
   (:file "syntax" :depends-on ("molicle"))))
