;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.common-lisp.documentation)

(def project :hu.dwim.common-lisp)

(def book user-guide (:title "User guide")
  (chapter (:title "Introduction")
    (paragraph ()
      "TODO"))
  (chapter (:title "Supported Common Lisp Implementations")
    (paragraph ()
      "This library contains portable code, so all Common Lisp implementations are supported."))
  (chapter (:title "Supported Operating Systems")
    (paragraph ()
      "This library contains portable code, so all Operating Systems are supported.")))
