(in-package :cl-user)

(asdf:defsystem :twitter-nmf
  :serial t
  :components
  ((:module src
    :components ((:file "foo")
                 )
    :serial t)
   (:module client-src
    :components ((:ps-file "main")
                 (:ps-file "vis"))
    :serial t))
  :depends-on (:cl-gui))
