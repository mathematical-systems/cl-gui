;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.quasi-quote)

(def type ub8-vector ()
  '(vector (unsigned-byte 8)))

;;;;;;
;;; Parse

(define-syntax quasi-quoted-binary (&key start-character
                                         end-character
                                         (unquote-character #\,)
                                         (splice-character #\@)
                                         (destructive-splice-character #\.)
                                         (dispatched-quasi-quote-name "bin")
                                         (transformation-pipeline nil))
  (set-quasi-quote-syntax-in-readtable
   (lambda (body dispatched?)
     (declare (ignore dispatched?))
     ;; we are checking for *quasi-quote-lexical-depth* because the transform descents into the unquote forms
     ;; and transform the quasi-quote's it can find there
     (bind ((toplevel? (= 1 *quasi-quote-lexical-depth*))
            (quasi-quote-node (make-binary-quasi-quote (coerce-to-transformation-pipeline transformation-pipeline) (process-binary-reader-body body))))
       (if toplevel?
           `(toplevel-quasi-quote-macro ,quasi-quote-node)
           quasi-quote-node)))
   (lambda (form modifier)
     (when modifier
       (simple-reader-error "Splicing modifier specified for binary unquote at form ~S. What would that mean?" form))
     (make-binary-unquote form))
   :start-character start-character
   :end-character end-character
   :unquote-character unquote-character
   :splice-character splice-character
   :destructive-splice-character destructive-splice-character
   :dispatched-quasi-quote-name dispatched-quasi-quote-name
   :unquote-readtable-case :toplevel))

(macrolet ((x (name transformation-pipeline &optional args)
             (bind ((syntax-name (format-symbol *package* "QUASI-QUOTED-BINARY-TO-~A" name)))
               `(define-syntax ,syntax-name (,@args &key
                                                    (with-inline-emitting #f)
                                                    (declarations '())
                                                    start-character
                                                    end-character
                                                    (unquote-character #\,)
                                                    (splice-character #\@)
                                                    (destructive-splice-character #\.)
                                                    (dispatched-quasi-quote-name "bin"))
                  (set-quasi-quoted-binary-syntax-in-readtable :transformation-pipeline ,transformation-pipeline
                                                               :dispatched-quasi-quote-name dispatched-quasi-quote-name
                                                               :start-character start-character
                                                               :end-character end-character
                                                               :unquote-character unquote-character
                                                               :splice-character splice-character
                                                               :destructive-splice-character destructive-splice-character)))))
  (x binary-emitting-form        (list (make-instance 'quasi-quoted-binary-to-binary-emitting-form
                                                      :stream-variable-name stream-variable-name
                                                      :with-inline-emitting with-inline-emitting
                                                      :declarations declarations))
     (stream-variable-name)))

(def (function o) decimal-number-to-hexadecimal-number (number)
  (declare (type fixnum number))
  (iter (with result = 0)
        (with remainder)
        (for multiplier :first 1 :then (* multiplier 16))
        (setf (values number remainder) (truncate number 10))
        (incf result (* multiplier remainder))
        (until (zerop number))
        (finally (return result))))

(def (function o) hexadecimal-number-string-to-hexadecimal-number (string)
  (declare (type string string))
  (iter (with result = 0)
        (for el :in-sequence (reverse string))
        (for multiplier :first 1 :then (* multiplier 16))
        (for value = (position el #.(coerce "0123456789abcdef" 'simple-base-string)
                               :test #'char-equal))
        (incf result (* multiplier value))
        (finally (return result))))

(def (function o) process-binary-reader-body (form &optional allow-strings?)
  (etypecase form
    (cons
     (iter (with buffer = (make-adjustable-vector 8 :element-type '(unsigned-byte 8)))
           (for el :in form)
           (for value = (process-binary-reader-body el allow-strings?))
           (if (integerp value)
               (vector-push-extend value buffer)
               (progn
                 (unless (zerop (length buffer))
                   (collect (shrink-vector buffer) :into result)
                   (setf buffer (make-adjustable-vector 8 :element-type '(unsigned-byte 8))))
                 (collect value :into result)))
           (finally (return (if (zerop (length buffer))
                                result
                                (if (null result)
                                    (shrink-vector buffer)
                                    (nconc result (list (shrink-vector buffer)))))))))
    (fixnum (bind ((result (decimal-number-to-hexadecimal-number form)))
              (unless (<= 0 result 255)
                (simple-reader-error nil "~A is out of the 0-255 range" result))
              result))
    (symbol (bind ((result (hexadecimal-number-string-to-hexadecimal-number (symbol-name form))))
              (unless (<= 0 result 255)
                (simple-reader-error nil "~A is out of the 0-255 range" result))
              result))
    (string (if allow-strings?
                form
                (error "Strings are not allowed in the body of the quasi-quoted-binary reader: ~S" form)))
    (syntax-node form)))


;;;;;;
;;; AST

(def ast binary)

(def class* binary-syntax-node ()
  ())

(def (class* e) binary-quasi-quote (quasi-quote binary-syntax-node)
  ())

(def method print-object ((self binary-quasi-quote) *standard-output*)
  (print-object/quasi-quote self "bin"))

(def (function e) make-binary-quasi-quote (transformation-pipeline body)
  (assert (not (typep body 'quasi-quote)))
  (make-instance 'binary-quasi-quote :body body :transformation-pipeline transformation-pipeline))

(def (class* e) binary-unquote (unquote binary-syntax-node)
  ())

(def (function e) make-binary-unquote (form)
  (make-instance 'binary-unquote :form form))


;;;;;;
;;; Transform

(def (transformation e) quasi-quoted-binary-to-binary-emitting-form (lisp-form-emitting-transformation)
  ()
  'transform-quasi-quoted-binary-to-binary-emitting-form)

(defmethod print-object ((self quasi-quoted-binary-to-binary-emitting-form) *standard-output*)
  (princ "[Binary->Forms]"))

#+nil ; TODO
(def function binary-position ()
  (file-position *quasi-quote-stream*))

(def function binary-concatenate (elements)
  (with-output-to-sequence (stream :element-type '(unsigned-byte 8))
    (dolist (el elements)
      (write-sequence el stream))))

(def (function io) write-quasi-quoted-binary (node stream)
  (declare (notinline write-quasi-quoted-binary))
  (etypecase node
    (vector (write-sequence node stream))
    (list (mapc (lambda (node) (write-quasi-quoted-binary node stream)) node))
    (integer (write-byte node stream))
    (delayed-emitting (funcall node))
    (binary-quasi-quote (write-quasi-quoted-binary (body-of node) stream)))
  (values))

(def function reduce-binary-subsequences (sequence)
  (reduce-subsequences sequence
                       (lambda (el) (typep el 'ub8-vector))
                       #'binary-concatenate))

(def function make-quasi-quoted-binary-emitting-form  (node)
  (bind ((stream-variable-name (stream-variable-name-of *transformation*)))
    (etypecase node
      (ub8-vector `(write-sequence ,node ,stream-variable-name))
      (binary-unquote
       `(write-quasi-quoted-binary ,(transform-quasi-quoted-binary-to-binary-emitting-form/unquote node) ,stream-variable-name))
      ;; a quasi-quoted-binary at this point means that it's a nested non-compatible
      (binary-quasi-quote
       (assert (typep (first (transformation-pipeline-of node)) 'lisp-form-emitting-transformation))
       `(emit ,(transform node)))
      (side-effect (form-of node)))))

(def function transform-quasi-quoted-binary-to-binary-emitting-form (input)
  (transformation-typecase input
    (binary-quasi-quote
     (wrap-emitting-forms (mapcar 'make-quasi-quoted-binary-emitting-form
                                  (reduce-binary-subsequences
                                   (transform-quasi-quoted-binary-to-binary-emitting-form/flatten-body input)))))
    #+nil ;; TODO delme? write test that triggers it...
    (binary-unquote
     (transform-quasi-quoted-binary-to-binary-emitting-form/unquote input))))

(defun transform-quasi-quoted-binary-to-binary-emitting-form/flatten-body (input)
  (bind (flattened-elements)
    (labels ((recurse (node)
               (etypecase node
                 (cons
                  (recurse (car node))
                  (recurse (cdr node)))
                 (null nil)
                 (integer
                  (assert (<= 0 node 255))
                  (push node flattened-elements))
                 (ub8-vector
                  (iter (for el :in-vector node)
                        (push el flattened-elements)))
                 (binary-quasi-quote (if (compatible-transformation-pipelines?
                                          (transformation-pipeline-of input)
                                          (transformation-pipeline-of node))
                                         ;; if the pipelines are compatible, then just skip over the qq node
                                         ;; and descend into its body as if it wasn't even there...
                                         (recurse (body-of node))
                                         (push node flattened-elements)))
                 (syntax-node (push node flattened-elements)))))
      (recurse (body-of input)))
    (nreversef flattened-elements)
    (iter outer
          (while flattened-elements)
          (if (integerp (first flattened-elements))
              (iter (with start = flattened-elements)
                    (for count :upfrom 0)
                    (while (integerp (first flattened-elements)))
                    (pop flattened-elements)
                    (finally (in outer (collect (make-array count :element-type '(unsigned-byte 8) :initial-contents (subseq start 0 count))))))
              (iter (while flattened-elements)
                    (until (integerp (first flattened-elements)))
                    (in outer (collect (pop flattened-elements))))))))

(def function transform-quasi-quoted-binary-to-binary-emitting-form/unquote (input)
  (map-filtered-tree (form-of input) 'binary-quasi-quote
                     'transform-quasi-quoted-binary-to-binary-emitting-form))




