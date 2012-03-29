;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def.documentation)

(def project :hu.dwim.def)

(def book user-guide (:title "User guide")
  (chapter (:title "Introduction")
    (paragraph ()
      "TODO"))
  (chapter (:title "Supported Common Lisp Implementations")
    (paragraph ()
      "SBCL"))
  (chapter (:title "Supported Operating Systems")
    (paragraph ()
      "Linux"))
  (chapter (:title "Tutorial")
    (paragraph ()
      "TODO")))

#|
usage example in your init.el:
(setq dwim-workspace (getenv "DWIM_WORKSPACE"))
(add-to-list 'load-path (expand-file-name (concat dwim-workspace "/hu.dwim.def/emacs/")))
(require 'hu.dwim.def)

You may want to add this to your emacs setup:

(let ((overrides
       '((defclass* defclass)
         (defcondition* defcondition)
         (def (4 4 (&whole 4 &rest 2) &body))
         (defresources (4 &rest (&whole 2 &lambda &body))))))
  (dolist (el overrides)
    (put (first el) 'common-lisp-indent-function
         (if (symbolp (second el))
             (get (second el) 'common-lisp-indent-function)
             (second el)))))

And maybe this to lisp-font-lock-keywords-2

("(\\(def\\)[         \n]+(\\([^)         \n]+\\).*?)[         \n(]+\\(.*?\\)[         \n)]+"
 (1 font-lock-keyword-face)
 (2 font-lock-type-face)
 (3 font-lock-function-name-face))
("(\\(def\\)[         \n]+\\(.*?\\)[         \n(]+\\(.*?\\)[         \n)]+"
 (1 font-lock-keyword-face)
 (2 font-lock-type-face)
 (3 font-lock-function-name-face))
|#
