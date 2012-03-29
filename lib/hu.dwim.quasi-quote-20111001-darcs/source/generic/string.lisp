;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.quasi-quote)

;;;;;;
;;; Parse

(define-syntax quasi-quoted-string (&key start-character
                                         end-character
                                         (unquote-character #\,)
                                         (splice-character #\@)
                                         (destructive-splice-character #\.)
                                         (dispatched-quasi-quote-name "str")
                                         (transformation-pipeline nil))
  (set-quasi-quote-syntax-in-readtable
   (lambda (body dispatched?)
     (declare (ignore dispatched?))
     ;; we are checking for *quasi-quote-lexical-depth* because the transform descents into the unquote forms
     ;; and transform the quasi-quote's it can find there
     (bind ((toplevel? (= 1 *quasi-quote-lexical-depth*))
            (quasi-quote-node (make-string-quasi-quote (coerce-to-transformation-pipeline transformation-pipeline) body)))
       (if toplevel?
           `(toplevel-quasi-quote-macro ,quasi-quote-node)
           quasi-quote-node)))
   (lambda (form modifier)
     (when modifier
       (simple-reader-error "Splicing modifier specified for string unquote at form ~S. What would that mean?" form))
     (make-string-unquote form))
   :start-character start-character
   :end-character end-character
   :unquote-character unquote-character
   :splice-character splice-character
   :destructive-splice-character destructive-splice-character
   :dispatched-quasi-quote-name dispatched-quasi-quote-name
   :unquote-readtable-case :toplevel))

(macrolet ((x (name transformation-pipeline &optional args)
             (bind ((syntax-name (format-symbol *package* "QUASI-QUOTED-STRING-TO-~A" name))
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
                                               (dispatched-quasi-quote-name "str")
                                               ,@(when &key-position (subseq args (1+ &key-position))))
                  (set-quasi-quoted-string-syntax-in-readtable :transformation-pipeline ,transformation-pipeline
                                                               :dispatched-quasi-quote-name dispatched-quasi-quote-name
                                                               :start-character start-character
                                                               :end-character end-character
                                                               :unquote-character unquote-character
                                                               :splice-character splice-character
                                                               :destructive-splice-character destructive-splice-character)))))
  (x string-emitting-form (make-quasi-quoted-string-to-form-emitting-transformation-pipeline
                           stream-variable-name
                           :with-inline-emitting with-inline-emitting
                           :declarations declarations)
     (stream-variable-name))
  (x binary-emitting-form (make-quasi-quoted-string-to-form-emitting-transformation-pipeline
                           stream-variable-name
                           :binary t
                           :encoding encoding
                           :with-inline-emitting with-inline-emitting
                           :declarations declarations)
     (stream-variable-name &key (encoding *default-character-encoding*))))

(def (function e) make-quasi-quoted-string-to-form-emitting-transformation-pipeline
    (stream-variable-name &key binary with-inline-emitting (encoding :utf-8) declarations)
  (if binary
      (list (make-instance 'quasi-quoted-string-to-quasi-quoted-binary
                           :encoding encoding)
            (make-instance 'quasi-quoted-binary-to-binary-emitting-form
                           :stream-variable-name stream-variable-name
                           :with-inline-emitting with-inline-emitting
                           :declarations declarations))
      (list (make-instance 'quasi-quoted-string-to-string-emitting-form
                           :stream-variable-name stream-variable-name
                           :with-inline-emitting with-inline-emitting
                           :declarations declarations))))


;;;;;;
;;; AST
;;;
;;; A quasi quoted string is made of character, string, list, string-quasi-quote, string-unquote nodes recursively.

(def ast string)

(def class* string-syntax-node (syntax-node)
  ())

(def (class* e) string-quasi-quote (quasi-quote string-syntax-node)
  ())

(def method print-object ((self string-quasi-quote) *standard-output*)
  (print-object/quasi-quote self "str"))

(def (function e) make-string-quasi-quote (transformation-pipeline body)
  (assert (not (typep body 'quasi-quote)))
  (make-instance 'string-quasi-quote :transformation-pipeline transformation-pipeline :body body))

(def (class* e) string-unquote (unquote string-syntax-node)
  ())

(def (function e) make-string-unquote (form)
  ;; splicing doesn't mean much for strings
  (make-instance 'string-unquote :form form))


;;;;;;
;;; transform to binary-emitting-form

(def class* quasi-quoted-string-transformation-mixin ()
  ())

(def (transformation e) quasi-quoted-string-to-string-emitting-form (quasi-quoted-string-transformation-mixin
                                                                     lisp-form-emitting-transformation)
  ()
  'transform-quasi-quoted-string-to-string-emitting-form)

(defmethod print-object ((self quasi-quoted-string-to-string-emitting-form) *standard-output*)
  (princ "[String->Forms]"))

(def function string-concatenate (elements)
  (bind ((*print-pretty* #f))
    (with-output-to-string (stream)
      (dolist (el elements)
        (etypecase el
          (string (write-string el stream))
          (character (write-char el stream)))))))

(def function write-quasi-quoted-string (node stream)
  (etypecase node
    (character (write-char node stream))
    (string (write-string node stream))
    (list (mapc (lambda (sub-node)
                  (write-quasi-quoted-string sub-node stream))
                node))
    (delayed-emitting (funcall node)))
  (values))

(def function reduce-string-subsequences (sequence)
  (reduce-subsequences sequence [or (stringp !1) (characterp !1)] #'string-concatenate))

(def function make-quasi-quoted-string-emitting-form (node)
  (bind ((stream (stream-variable-name-of *transformation*)))
    (etypecase node
      (character `(write-char ,node ,stream))
      (string `(write-string ,node ,stream))
      (string-unquote
       `(write-quasi-quoted-string ,(transform-quasi-quoted-string-to-string-emitting-form/unquote node) ,stream))
      (side-effect (form-of node)))))

(def function transform-quasi-quoted-string-to-string-emitting-form (node)
  (etypecase node
    (string-quasi-quote
     (wrap-emitting-forms (mapcar 'make-quasi-quoted-string-emitting-form
                                  (reduce-string-subsequences
                                   (transform-quasi-quoted-string-to-string-emitting-form/flatten-body node)))))
    #+nil ;; TODO delme? write test that triggers it...
    (string-unquote
     (transform-quasi-quoted-s-to-s-emitting-form/unquote node))
    (side-effect (form-of node))))

(def function transform-quasi-quoted-string-to-string-emitting-form/flatten-body (input)
  (let (flattened-elements)
    (labels ((traverse (node)
               (when node
                 (etypecase node
                   (cons
                    (traverse (car node))
                    (traverse (cdr node)))
                   ((or string character) (push node flattened-elements))
                   (string-quasi-quote (bind ((nested-node node))
                                         (if (compatible-transformation-pipelines?
                                              (transformation-pipeline-of input)
                                              (transformation-pipeline-of nested-node))
                                             ;; if the pipelines are compatible, then just skip over the qq node
                                             ;; and descend into its body as if it never was there...
                                             (traverse (body-of nested-node))
                                             (push (transform nested-node) flattened-elements))))
                   ;; it's the last
                   (syntax-node (push node flattened-elements))))))
      (traverse (body-of input)))
    (nreversef flattened-elements)
    (bind ((*print-pretty* #f))
      (iter outer
            (while flattened-elements)
            (if (typep (first flattened-elements) '(or string character))
                (collect (with-output-to-string (*standard-output*)
                           (iter (for el = (first flattened-elements))
                                 (typecase el
                                   (string
                                    (write-string el)
                                    (pop flattened-elements))
                                   (character
                                    (write-char el)
                                    (pop flattened-elements))
                                   (t (return))))))
                (iter (while flattened-elements)
                      (until (typep (first flattened-elements) '(or string character)))
                      (in outer (collect (pop flattened-elements)))))))))

(def function transform-quasi-quoted-string-to-string-emitting-form/unquote (input)
  (map-filtered-tree (form-of input) 'string-quasi-quote
                     'transform-quasi-quoted-string-to-string-emitting-form))


;;;;;;
;;; transform to binary-quasi-quote

(def (transformation e) quasi-quoted-string-to-quasi-quoted-binary (quasi-quoted-string-transformation-mixin)
  ((encoding *default-character-encoding*))
  'transform-quasi-quoted-string-to-quasi-quoted-binary)

(defmethod print-object ((self quasi-quoted-string-to-quasi-quoted-binary) *standard-output*)
  (princ "[String->Binary]"))

(def method compatible-transformations? ((a quasi-quoted-string-to-quasi-quoted-binary) a-next a-rest
                                         (b quasi-quoted-string-to-quasi-quoted-binary) b-next b-rest)
  (and (eql (encoding-of a) (encoding-of b))
       (compatible-transformations? a-next (first a-rest) (rest a-rest)
                                    b-next (first b-rest) (rest b-rest))))

(def function transform-quasi-quoted-string-to-quasi-quoted-binary (node &key (encoding (encoding-of *transformation*)))
  (transformation-typecase node
    (list
     (mapcar (lambda (child)
               (transform-quasi-quoted-string-to-quasi-quoted-binary child))
             node))
    (character (babel:string-to-octets (string node) :encoding encoding))
    (string (babel:string-to-octets node :encoding encoding))
    (string-quasi-quote
     (if (compatible-with-current-transformation-pipeline? (transformation-pipeline-of node))
         (make-binary-quasi-quote (rest (transformation-pipeline-of node))
                                  (transform-quasi-quoted-string-to-quasi-quoted-binary (body-of node)))
         (transform node)))
    (string-unquote
     (make-binary-unquote
      (wrap-runtime-delayed-transformation-form
       `(transform-quasi-quoted-string-to-quasi-quoted-binary
         ,(map-filtered-tree (form-of node) 'string-quasi-quote
                             (lambda (child)
                               (transform-quasi-quoted-string-to-quasi-quoted-binary child)))))))
    (binary-quasi-quote node)))

(def method compatible-transformations? ((a quasi-quoted-binary-to-binary-emitting-form) a-next a-rest
                                         (b quasi-quoted-string-to-quasi-quoted-binary) (b-next quasi-quoted-binary-to-binary-emitting-form) b-rest)
  (compatible-transformations? a a-next a-rest
                               b-next (first b-rest) (rest b-rest)))
