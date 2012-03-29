;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.quasi-quote)

(define-syntax quasi-quoted-list (&key (start-character #\`)
                                       end-character
                                       (unquote-character #\,)
                                       (splice-character #\@)
                                       (destructive-splice-character #\.)
                                       transformation-pipeline
                                       dispatched-quasi-quote-name)
  (set-quasi-quote-syntax-in-readtable
   (lambda (body dispatched?)
     (declare (ignore dispatched?))
     (bind ((toplevel? (= 1 *quasi-quote-nesting-level*))
            (quasi-quote-node (make-list-quasi-quote (coerce-to-transformation-pipeline transformation-pipeline) body)))
       (if toplevel?
           `(toplevel-quasi-quote-macro ,quasi-quote-node)
           quasi-quote-node)))
   (lambda (form modifier)
     (make-list-unquote form modifier))
   :start-character start-character
   :end-character end-character
   :unquote-character unquote-character
   :splice-character splice-character
   :destructive-splice-character destructive-splice-character
   :dispatched-quasi-quote-name dispatched-quasi-quote-name))

(define-syntax quasi-quoted-list-to-list-emitting-form (&key (start-character #\`)
                                                             end-character
                                                             (unquote-character #\,)
                                                             (splice-character #\@)
                                                             (destructive-splice-character #\.)
                                                             dispatched-quasi-quote-name)
  (set-quasi-quoted-list-syntax-in-readtable
   :transformation-pipeline (list (make-instance 'quasi-quoted-list-to-list-emitting-form))
   :dispatched-quasi-quote-name dispatched-quasi-quote-name
   :start-character start-character
   :end-character end-character
   :unquote-character unquote-character
   :splice-character splice-character
   :destructive-splice-character destructive-splice-character))

;;;;;;
;;; AST

(def ast list)

(def class* list-syntax-node (syntax-node)
  ())

(def (class* e) list-quasi-quote (quasi-quote list-syntax-node)
  ())

(def (function e) make-list-quasi-quote (transformation-pipeline body)
  (make-instance 'list-quasi-quote
                 :transformation-pipeline transformation-pipeline
                 :body body))

(def (class* e) list-unquote (unquote list-syntax-node)
  ())

(def (function e) make-list-unquote (form &optional modifier)
  (make-instance 'list-unquote :form form :modifier modifier))

(def method print-object ((self list-quasi-quote) *standard-output*)
  (print-object/quasi-quote self "list"))

(def method map-ast (fn (x list-quasi-quote))
  (setf (body-of x) (funcall fn (body-of x)))
  x)

;;;;;;
;;; transform to list emitting form

(def (transformation e) quasi-quoted-list-to-list-emitting-form (lisp-form-emitting-transformation)
  ()
  'transform-quasi-quoted-list-to-list-emitting-form)

(defmethod print-object ((self quasi-quoted-list-to-list-emitting-form) *standard-output*)
  (princ "[List->Forms]"))

(def function transform-quasi-quoted-list-to-list-emitting-form (input)
  (transformation-typecase input
    (list-quasi-quote
     (bq-completely-process (body-of input)))))

(def function trace-list-quasi-quote-functions ()
  (trace bq-process bq-bracket bq-simplify bq-simplify-args bq-remove-tokens)
  (values))

;;; Common Lisp backquote implementation, written in Common Lisp.
;;; Author: Guy L. Steele Jr.     Date: 27 December 1985
;;; Texted under Symbolics Common Lisp and Lucid Common Lisp.
;;; This software is in the public domain.

;;; The following are unique tokens used during processing
;;; They need not be symbols; they need not even be atoms.

(def special-variable *bq-list*        (make-symbol "BQ-LIST"))
(def special-variable *bq-append*      (make-symbol "BQ-APPEND"))
(def special-variable *bq-list**       (make-symbol "BQ-LIST*"))
(def special-variable *bq-nconc*       (make-symbol "BQ-NCONC"))
(def special-variable *bq-clobberable* (make-symbol "BQ-CLOBBERABLE"))
(def special-variable *bq-quote*       (make-symbol "BQ-QUOTE"))
(def special-variable *bq-quote-nil*   (list *bq-quote* nil))

;;; if the value of *BQ-SIMPLIFY* is non-nil, then BACKQUOTE
;;; processing applies the code simplifier.  If the value is NIL,
;;; then the code resulting from BACKQUOTE is exactly that
;;; specified by the official rules.

(def special-variable *bq-simplify* t)

;;; Backquote processing proceeds in three stages:
;;;
;;; (1) BQ-PROCESS applies the rules to remove occurrences of
;;; #:COMMA, #:COMMA-ATSIGN, and #:COMMA-DOT corresponding to
;;; this level of BACKQUOTE.  (It also causes embedded calls to
;;; BACKQUOTE to be expanded so that nesting is properly handled.)
;;; Code is produced that is expressed in terms of functions
;;; #:BQ-LIST, #:BQ-APPEND, and #:BQ-CLOBBERABLE.  This is done
;;; so that the simplifier will simplify only list construction
;;; functions actually generated by backquote and will not involve
;;; any user code in the simplification.   #:BQ-LIST means LIST,
;;; #:BQ-APPEND means APPEND, and #:BQ-CLOBBERABLE means IDENTITY
;;; but indicates places where ",." was used and where NCONC may
;;; therefore be introduced by the simplifier for efficiency.
;;;
;;; (2) BQ-SIMPLIFY, if used, rewrites the code produced by
;;; BQ-PROCESS to produce equivalent but faster code.  The
;;; additional functions #:BQ-LIST* and #:BQ-NCONC may be
;;; introduced into the code.
;;;
;;; (3) BQ-REMOVE-TOKENS goes through the code and replaces
;;; #:BQ-LIST with LIST, #:BQ-APPEND with APPEND, and so on.
;;; #:BQ-CLOBBERABLE is simply eliminated (a call to it being
;;; replaced by its argument).  #:BQ-LIST* is replaced by either
;;; LIST* or CONS (the latter is used in the two-argument case,
;;; purely to make the resulting code a tad more readable).

(defun bq-completely-process (x)
  (let ((raw-result (bq-process x)))
    (bq-remove-tokens (if *bq-simplify*
                          (bq-simplify raw-result)
                          raw-result))))

(defun bq-list-to-vector (list)
  (coerce list 'vector))

(define-compiler-macro bq-list-to-vector (&whole whole form)
  (if (quoted-form? form)
      (bq-list-to-vector (second form))
      whole))

(def generic bq-process (x)
  (:method ((x list-quasi-quote))
    (bq-process (bq-completely-process (body-of x))))

  (:method ((x list-unquote))
    (cond
      ((destructively-spliced? x) (error ",.~S after `" (form-of x)))
      ((spliced? x)               (error ",@~S after `" (form-of x)))
      (t
       (form-of x))))

  (:method ((x t))
    (cond
      ((atom x)
       (assert (not (typep x 'syntax-node)))
       (if (simple-vector-p x)
           (list 'bq-list-to-vector (bq-process (coerce x 'list)))
           (list *bq-quote* x)))
      (t
       (iter (for p :first x :then (cdr p))
             (when (non-syntax-node-atom? p)
               (collect (list *bq-quote* p) :into q)
               (return (cons *bq-append* q)))
             (when (typep p 'list-unquote) ; (eq (car p) *comma*)
               (cond
                 ((destructively-spliced? p) (error "Dotted ,.~S" (form-of p)))
                 ((spliced? p)               (error "Dotted ,@~S" (form-of p))))
               (collect (form-of p) :into q)
               (return (cons *bq-append* q)))
             (collect (bq-bracket (car p)) :into q))))))

;;; This implements the bracket operator of the formal rules

(defun bq-bracket (x)
  (typecase x
    (list-unquote                       ; (eq (car x) *comma*)
     (cond
       ((destructively-spliced? x)      ; (eq (car x) *comma-dot*)
        (list *bq-clobberable* (form-of x)))
       ((spliced? x)                    ; (eq (car x) *comma-atsign*)
        (form-of x))
       (t
        (list *bq-list* (form-of x)))))
    (t
     (list *bq-list* (bq-process x)))))

;;; This predicate is true of a form that when read looked
;;; like ,@foo or ,.foo

(defun bq-splicing-frob (x)
  (and (typep x 'list-unquote)
       (spliced? x)))

;;; This predicate is true of a form that when read
;;; looked like ,@foo or just plain ,foo.

(defun bq-frob (x)
  (typep x 'list-unquote))

;;; The simplifier essentially looks for calls to #:BQ-APPEND and
;;; tries to simplify them.  The arguments to #:BQ-APPEND are
;;; processed from right to left, building up a replacement for.
;;; At each step a number of special cases are handled that,
;;; loosely speaking, look like this:
;;;
;;; (APPEND (LIST a b c) foo) => (LIST* a b c foo)
;;;   provided a, b, c are not splicing frobs
;;; (APPEND (LIST* a b c) foo) => (LIST* a b (APPEND c foo))
;;;   provided a, b, c are not splicing frobs
;;; (APPEND (QUOTE (x)) foo) => (LIST* (QUOTE x) foo)
;;; (APPEND (CLOBBERABLE x) foo) => (NCONC x foo)

(defun bq-simplify (x)
  (if (atom x)
      x
      (let ((x (if (eq (car x) *bq-quote*)
                   x
                   (map-ast #'bq-simplify x))))
        (if (not (eq (car x) *bq-append*))
            x
            (bq-simplify-args x)))))

(defun bq-simplify-args (x)
  (do ((args (reverse (cdr x)) (cdr args))
       (result
        nil
        (cond ((atom (car args))
               (bq-attach-append *bq-append* (car args) result))
              ((and (eq (caar args) *bq-list*)
                    (notany #'bq-splicing-frob (cdar args)))
               (bq-attach-conses (cdar args) result))
              ((and (eq (caar args) *bq-list**)
                    (notany #'bq-splicing-frob (cdar args)))
               (bq-attach-conses
                (reverse (cdr (reverse (cdar args))))
                (bq-attach-append *bq-append*
                                  (car (last (car args)))
                                  result)))
              ((and (eq (caar args) *bq-quote*)
                    (consp (cadar args))
                    (not (bq-frob (cadar args)))
                    (null (cddar args)))
               (bq-attach-conses (list (list *bq-quote*
                                             (caadar args)))
                                 result))
              ((eq (caar args) *bq-clobberable*)
               (bq-attach-append *bq-nconc* (cadar args) result))
              ((and (null result)
                    (bq-null-or-quoted (first args))
                    (atom (first (first args))))
               ;; this takes care of dotted lists in `(a . b)
               (first args))
              (t (bq-attach-append *bq-append*
                                   (car args)
                                   result)))))
      ((null args) result)))

(defun bq-null-or-quoted (x)
  (or (null x) (and (consp x) (eq (car x) *bq-quote*))))

;;; When BQ-ATTACH-APPEND is called, the OP should be #:BQ-APPEND
;;; or #:BQ-NCONC.  This produces a form (op item result) but
;;; some simplifications are done on the fly:
;;;
;;;  (op '(a b c) '(d e f g)) => '(a b c d e f g)
;;;  (op item 'nil) => item, provided item is not a splicable frob
;;;  (op item 'nil) => (op item), if item is a splicable frob
;;;  (op item (op a b c)) => (op item a b c)

(defun bq-attach-append (op item result)
  (assert (or (eq op *bq-nconc*) (eq op *bq-append*)))
  (cond
    ((and (bq-null-or-quoted item) (bq-null-or-quoted result))
     (list *bq-quote* (append (cadr item) (cadr result))))
    ((or (null result) (equal result *bq-quote-nil*))
     (if (bq-splicing-frob item) (list op item) item))
    ((and (consp result) (eq (car result) op))
     (list* (car result) item (cdr result)))
    (t (list op item result))))

;;; The effec tof BQ-ATTACH-CONSES is to produce a form as if by
;;; `(LIST* ,@items ,result) but some simplifications are done
;;; on the fly.
;;;
;;;  (LIST* 'a 'b 'c 'd) => '(a b c . d)
;;;  (LIST* a b c 'nil) => (LIST a b c)
;;;  (LIST* a b c (LIST* d e f g)) => (LIST* a b c d e f g)
;;;  (LIST* a b c (LIST d e f g)) => (LIST a b c d e f g)

(defun bq-attach-conses (items result)
  (cond
    ((and (every #'bq-null-or-quoted items)
          (bq-null-or-quoted result))
     (list *bq-quote*
           (append (mapcar #'cadr items) (cadr result))))
    ((or (null result) (equal result *bq-quote-nil*))
     (cons *bq-list* items))
    ((and (consp result)
          (or (eq (car result) *Bq-list*)
              (eq (car result) *bq-list**)))
     (cons (car result) (append items (cdr result))))
    (t (cons *bq-list** (append items (list result))))))

;;; Removes funny toeksn and changes (#:BQ-LIST* a b) into
;;; (CONS a b) instead of (LIST* a b), purely for readability.

(defun bq-remove-tokens (x)
  (cond
    ((eq x *bq-list*) 'list)
    ((eq x *bq-append*) 'append)
    ((eq x *bq-nconc*) 'nconc)
    ((eq x *bq-list**) 'list*)
    ((eq x *bq-quote*) 'quote)
    ((atom x) x)
    ((eq (car x) *bq-clobberable*)
     (bq-remove-tokens (cadr x)))
    ((and (eq (car x) *bq-list**)
          (consp (cddr x))
          (null (cdddr x)))
     (cons 'cons (map-ast #'bq-remove-tokens (cdr x))))
    (t (map-ast #'bq-remove-tokens x))))
