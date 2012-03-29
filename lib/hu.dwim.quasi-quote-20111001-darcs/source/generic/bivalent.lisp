;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.quasi-quote)

;;;;;;
;;; Parse

(define-syntax quasi-quoted-bivalent (&key start-character
                                           end-character
                                           (unquote-character #\,)
                                           (splice-character #\@)
                                           (destructive-splice-character #\.)
                                           (transformation-pipeline nil)
                                           (dispatched-quasi-quote-name "biv"))
  (set-quasi-quote-syntax-in-readtable
   (lambda (body dispatched?)
     (declare (ignore dispatched?))
     ;; we are checking for *quasi-quote-lexical-depth* because the transform descents into the unquote forms
     ;; and transform the quasi-quote's it can find there
     (bind ((toplevel? (= 1 *quasi-quote-lexical-depth*))
            (quasi-quote-node (make-bivalent-quasi-quote (coerce-to-transformation-pipeline transformation-pipeline) body)))
       (if toplevel?
           `(toplevel-quasi-quote-macro ,quasi-quote-node)
           quasi-quote-node)))
   (lambda (form modifier)
     (make-bivalent-unquote form modifier))
   :start-character start-character
   :end-character end-character
   :unquote-character unquote-character
   :splice-character splice-character
   :destructive-splice-character destructive-splice-character
   :dispatched-quasi-quote-name dispatched-quasi-quote-name
   :unquote-readtable-case :toplevel))

(macrolet ((x (name transformation-pipeline &optional args)
             (bind ((syntax-name (format-symbol *package* "QUASI-QUOTED-BIVALENT-TO-~A" name))
                    (&key-position (position '&key args)))
               `(define-syntax ,syntax-name (,@(subseq args 0 (or &key-position (length args)))
                                               &key
                                               (with-inline-emitting #f)
                                               (declarations '())
                                               start-character
                                               end-character
                                               (unquote-character #\,)
                                               (splice-character #\@)
                                               (destructive-splice-character #\.)
                                               (dispatched-quasi-quote-name "biv")
                                               ,@(when &key-position (subseq args (1+ &key-position))))
                  (set-quasi-quoted-bivalent-syntax-in-readtable :transformation-pipeline ,transformation-pipeline
                                                                 :dispatched-quasi-quote-name dispatched-quasi-quote-name
                                                                 :start-character start-character
                                                                 :end-character end-character
                                                                 :unquote-character unquote-character
                                                                 :splice-character splice-character
                                                                 :destructive-splice-character destructive-splice-character)))))
  (x bivalent-emitting-form (list (make-instance 'quasi-quoted-bivalent-to-bivalent-emitting-form
                                                 :stream-variable-name stream-variable-name
                                                 :with-inline-emitting with-inline-emitting
                                                 :declarations declarations))
     (stream-variable-name))
  (x binary-emitting-form (list (make-instance 'quasi-quoted-bivalent-to-quasi-quoted-binary
                                               :encoding encoding)
                                (make-instance 'quasi-quoted-binary-to-binary-emitting-form
                                               :stream-variable-name stream-variable-name
                                               :with-inline-emitting with-inline-emitting
                                               :declarations declarations))
     (stream-variable-name &key (encoding *default-character-encoding*))))


;;;;;;
;;; AST

(def ast bivalent)

(def class* bivalent-syntax-node ()
  ())

(def (class* e) bivalent-quasi-quote (quasi-quote bivalent-syntax-node)
  ())

(def method print-object ((self bivalent-quasi-quote) *standard-output*)
  (print-object/quasi-quote self "biv"))

(def (function e) make-bivalent-quasi-quote (transformation-pipeline body)
  (assert (not (typep body 'quasi-quote)))
  (make-instance 'bivalent-quasi-quote
                 :transformation-pipeline transformation-pipeline
                 :body body))

(def (class* e) bivalent-unquote (unquote bivalent-syntax-node)
  ())

(def (function e) make-bivalent-unquote (form &optional modifier)
  (make-instance 'bivalent-unquote :form form :modifier modifier))


;;;;;;
;;; Transform

(def (transformation e) quasi-quoted-bivalent-to-bivalent-emitting-form (lisp-form-emitting-transformation)
  ()
  'transform-quasi-quoted-bivalent-to-bivalent-emitting-form)

(defmethod print-object ((self quasi-quoted-bivalent-to-bivalent-emitting-form) *standard-output*)
  (princ "[Bivalent->Forms]"))

(def function write-quasi-quoted-bivalent (node stream)
  (etypecase node
    (character (write-char node stream))
    (string (write-string node stream))
    (vector (write-sequence node stream))
    (list (mapc (lambda (node) (write-quasi-quoted-bivalent node stream)) node))
    (delayed-emitting (funcall node)))
  (values))

(def function make-quasi-quoted-bivalent-emitting-form (node)
  (bind ((stream (stream-variable-name-of *transformation*)))
    (etypecase node
      (ub8-vector `(write-sequence ,node ,stream))
      (character `(write-char ,node ,stream))
      (string
       (if (= 1 (length node))
           `(write-char ,(char node 0) ,stream)
           `(write-string ,node ,stream)))
      (bivalent-unquote
       `(write-quasi-quoted-bivalent
         ,(transform-quasi-quoted-bivalent-to-bivalent-emitting-form/unquote node) ,stream))
      (side-effect (form-of node)))))

(def function transform-quasi-quoted-bivalent-to-bivalent-emitting-form (input)
  (transformation-typecase input
    (bivalent-quasi-quote
     (wrap-emitting-forms (mapcar 'make-quasi-quoted-bivalent-emitting-form
                                  (reduce-binary-subsequences
                                   (reduce-string-subsequences
                                    ;; TODO mimic transform-quasi-quoted-binary-to-binary-emitting-form/flatten-body?
                                    (flatten (body-of input)))))))
    ;; TODO delme? write test that triggers it...
    #+nil(bivalent-unquote
          (transform-quasi-quoted-bivalent-to-bivalent-emitting-form/unquote node))))

(def function transform-quasi-quoted-bivalent-to-bivalent-emitting-form/unquote (input)
  (map-filtered-tree (form-of input) 'bivalent-quasi-quote
                     'transform-quasi-quoted-bivalent-to-bivalent-emitting-form))

(def (transformation e) quasi-quoted-bivalent-to-quasi-quoted-binary (transformation)
  ((encoding *default-character-encoding*))
  'transform-quasi-quoted-bivalent-to-quasi-quoted-binary)

(def function transform-quasi-quoted-bivalent-to-quasi-quoted-binary (node)
  (transformation-typecase node
    (list
     (mapcar 'transform-quasi-quoted-bivalent-to-quasi-quoted-binary
             node))
    (character (babel:string-to-octets (string node) :encoding (encoding-of *transformation*))) ;; TODO: more efficient way
    (string (babel:string-to-octets node :encoding (encoding-of *transformation*)))
    (vector (coerce node 'ub8-vector))
    (fixnum node)
    (bivalent-quasi-quote
     (make-binary-quasi-quote (rest (transformation-pipeline-of node))
                              (transform-quasi-quoted-bivalent-to-quasi-quoted-binary (body-of node))))
    (bivalent-unquote
     (make-binary-unquote
      (wrap-runtime-delayed-transformation-form
       `(transform-quasi-quoted-bivalent-to-quasi-quoted-binary
         ,(map-filtered-tree (form-of node) 'bivalent-quasi-quote
                             'transform-quasi-quoted-bivalent-to-quasi-quoted-binary)))))
    ;; TODO add tests that trigger it or delete them
    #+nil(quasi-quote
     (if (typep node 'binary-quasi-quote)
         (body-of node)
         node))
    #+nil(unquote (transform 'quasi-quoted-binary node))
    ))

