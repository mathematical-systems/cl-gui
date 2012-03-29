;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009-2011 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.asdf
  :description "Various ASDF extensions such as attached test and documentation system, explicit development support, etc."
  ;; :depends-on (:asdf) ;  not safe. Be sure to explicitly (upgrade and) load asdf before you use hu.dwim.asdf
  :components ((:module "source"
                :components ((:file "duplicates" :depends-on ("package"))
                             (:file "package")
                             (:file "production" :depends-on ("workspace" "duplicates"))
                             (:file "system" :depends-on ("production"))
                             (:file "workspace" :depends-on ("package"))))))
