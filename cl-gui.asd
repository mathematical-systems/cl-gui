;; -*- Mode: common-lisp; Package: cl-user -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (progn
    #+sbcl (require :sb-bsd-sockets)
    #+allegro (require :sock)))

(asdf:defsystem :cl-gui
  :description "Browser-based GUI toolkit for Common Lisp"
  :author "MSI"
  :version "0.1"
  :licence "unknown"
  :serial t
  ;; add new files to this list:
  :components
  ((:module src
    :components ((:file "packages")
                 (:file "meta" :depends-on ("packages"))
                 (:file "configurations" :depends-on ("packages"))
                 (:file "utils" :depends-on ("meta"))
                 (:file "variables" :depends-on ("utils"))
                 (:file "event" :depends-on ("variables"))
                 ;; (:file "communication" :depends-on ("variables" "js-object-pool" "event"))
                 (:file "server" :depends-on ("event"))
                 ;; javascript related
                 (:file "parenscript" :depends-on ("server" "variables"))
                 (:file "parenscript-asdf" :depends-on ("parenscript"))
                 (:file "js-object-pool" :depends-on ("utils"))
                 (:file "js-class" :depends-on ("js-object-pool" "server" "variables"))
                 (:file "ext" :depends-on ("parenscript" "variables")))))
  :depends-on (:parenscript
               ;; :cl-js
               :alexandria
               :iterate
               :closer-mop
               #+(and sbcl swank) :hu.dwim.quasi-quote
               ;; #+sbcl :clws
               :cl-interpol
               :hunchentoot
               :hunchensocket
               :hunchen.io
               :cl-json
               :fare-csv
               ))


