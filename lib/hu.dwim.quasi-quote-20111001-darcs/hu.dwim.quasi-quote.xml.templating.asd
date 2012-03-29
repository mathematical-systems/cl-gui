;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.quasi-quote.xml.templating
  :class hu.dwim.system
  :depends-on (:hu.dwim.quasi-quote.xml+cxml
               :hu.dwim.util.temporary-files)
  :components ((:module "source"
                :components ((:module "xml"
                                      :components ((:file "templating")))))))
