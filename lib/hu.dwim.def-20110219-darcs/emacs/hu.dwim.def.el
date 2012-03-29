;;; -*- encoding: utf-8 -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(provide 'hu.dwim.def)

(defun hu.dwim.def.lisp-mode-hook ()
  (font-lock-add-keywords
   nil `(("(\\(def\\|hu\.dwim\.def:def\\)[ 	\n]+(\\([^) 	\n]+\\).*?)[ 	\n(]+\\(.*?\\)[ 	\n)]+"
          (1 font-lock-keyword-face)
          (2 font-lock-type-face)
          (3 font-lock-function-name-face))
         ("(\\(def\\|hu\.dwim\.def:def\\)[ 	\n]+\\(.*?\\)[ 	\n(]+\\(.*?\\)[ 	\n)]+"
          (1 font-lock-keyword-face)
          (2 font-lock-type-face)
          (3 font-lock-function-name-face)))
   t))

(add-hook 'lisp-mode-hook 'hu.dwim.def.lisp-mode-hook)
