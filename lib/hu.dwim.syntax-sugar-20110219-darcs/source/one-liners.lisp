;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.syntax-sugar)

(define-syntax sharp-boolean ()
  "This syntax reads \"#t\" as COMMON-LISP:T and \"#f\" as COMMON-LISP:NIL"
  (set-dispatch-macro-character #\# #\t (constantly t))
  (set-dispatch-macro-character #\# #\f (constantly nil)))

(defun get-macro-character* (char &optional (readtable *readtable*))
  "Just like CL:GET-MACRO-CHARACTER but its second value defaults to T even if there's no macro fn installed on CHAR."
  (bind (((:values fn nonterminating?) (get-macro-character char readtable)))
    (unless fn
      ;; the standard sais that if fn is nil then it must be nil, too. but by default
      ;; everything is non-terminating, so switch it back.
      (setf nonterminating? t))
    (values fn nonterminating?)))

(defun sharp-comment-reader (s c n)
  (declare (ignore c n))
  (read s nil nil t)
  (values))

(define-syntax sharp-comment ()
  "This syntax allows \"#;\" to be used to uncomment a whole sexp."
  (set-dispatch-macro-character #\# #\; #'sharp-comment-reader))
