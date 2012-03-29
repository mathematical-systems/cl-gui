;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.quasi-quote.xml)

(def (special-variable e :documentation "The default variable that holds the XML output stream.")
  *xml-stream*)

(define-syntax (quasi-quoted-xml :readtime-wrapper-result-transformer
                                 (lambda (result)
                                   (if (rest result)
                                       (make-xml-quasi-quote (coerce-to-transformation-pipeline transformation-pipeline) (mapcar 'body-of result))
                                       (first result))))
    (&key (start-character #\<)
          (end-character #\>)
          (unquote-character #\,)
          (splice-character #\@)
          (destructive-splice-character #\.)
          (transformation-pipeline nil)
          (dispatched-quasi-quote-name "xml")
          (signal-reader-redefinition #f))
  (bind ((start-character/original-reader   (multiple-value-list (get-macro-character* start-character *readtable*)))
         (end-character/original-reader     (when end-character
                                                 (multiple-value-list (get-macro-character* end-character *readtable*))))
         (unquote-character/original-reader (multiple-value-list (get-macro-character* unquote-character *readtable*))))
    (awhen (and signal-reader-redefinition
                (first start-character/original-reader))
      (simple-style-warning "Installing the XML reader on character ~S while it already has a reader installed: ~S" start-character it))
    (set-quasi-quote-syntax-in-readtable
     (named-lambda xml-quasi-quote-wrapper (body dispatched?)
       (when (and (not (typep body 'syntax-node))
                  (< (length body) 1))
         (simple-reader-error nil "Syntax error in XML syntax: no element name is given?"))
       (bind ((toplevel? (= 1 *quasi-quote-nesting-level*))
              (quasi-quote-node (unless *read-suppress*
                                  (if dispatched?
                                      ;; dispatched `xml(element) syntax
                                      (process-dispatched-xml-reader-body body (coerce-to-transformation-pipeline transformation-pipeline))
                                      ;; <>-based syntax
                                      (process-<>-xml-reader-body body (coerce-to-transformation-pipeline transformation-pipeline))))))
         (unless *read-suppress*
           (if toplevel?
               `(toplevel-quasi-quote-macro ,quasi-quote-node)
               quasi-quote-node))))
     (named-lambda xml-unquote-wrapper (body modifier)
       (unless *read-suppress*
         (make-xml-unquote body modifier)))
     :nested-quasi-quote-wrapper (lambda (body dispatched?)
                                   (when (< (length body) 1)
                                     (simple-reader-error nil "Syntax error in XML syntax: no element name is given?"))
                                   (if dispatched?
                                       body
                                       `(xml-quasi-quote/nested ,body)))
     :start-character start-character
     :end-character end-character
     :unquote-character unquote-character
     :splice-character splice-character
     :destructive-splice-character destructive-splice-character
     :readtable-case :preserve
     :unquote-readtable-case :toplevel
     :dispatched-quasi-quote-name dispatched-quasi-quote-name
     :body-reader (make-quasi-quoted-xml-body-reader start-character end-character unquote-character)
     :toplevel-reader-wrapper (lambda (reader)
                                (declare (optimize debug))
                                (named-lambda xml-toplevel-reader-wrapper (stream char)
                                  (block nil
                                    (bind ((next-char (peek-char nil stream nil :eof t)))
                                      (if (and (char= char #\<) ; we are installed on the less-then sign...
                                               (or (eq next-char :eof)
                                                   (not (or (alphanumericp next-char)
                                                            (char= unquote-character next-char)))))
                                          (flet ((restore-macro-character (character function non-terminating?)
                                                   (if function
                                                       (set-macro-character character function non-terminating?)
                                                       (set-syntax-from-char character character)))) ; copy from the standard readtable
                                            ;; KLUDGE UNREAD-CHAR after a PEEK-CHAR is not allowed by the standard,
                                            ;; but i don't care much: it works fine on lisps with sane stream buffering,
                                            ;; which includes SBCL.
                                            (unread-char start-character stream)
                                            (bind ((*readtable* (copy-readtable)))
                                              ;; disable us and call READ recursively to read a single #\<
                                              (apply #'restore-macro-character start-character start-character/original-reader)
                                              (when end-character
                                                (apply #'restore-macro-character end-character end-character/original-reader))
                                              (apply #'restore-macro-character unquote-character unquote-character/original-reader)
                                              (return (read stream t 'eof t))))
                                          (funcall reader stream char)))))))))

(def function make-quasi-quoted-xml-body-reader (start-character end-character unquote-character)
  (named-lambda quasi-quoted-xml-body-reader (stream)
    (assert end-character)
    (bind ((first-character (peek-char nil stream #t nil #t))
           (element-name (if (char= first-character unquote-character)
                             (read stream #t nil #t)
                             (read-quasi-quoted-xml-name stream start-character end-character unquote-character)))
           (attributes (list)))
      (when (char= #\( (peek-char #t stream #t nil #t))
        (read-char stream #t nil #t)
        (setf attributes
              (iter (for next-character = (peek-char #t stream #t nil #t))
                    (until (char= next-character #\) ))
                    (switch (next-character :test #'char=)
                      (unquote-character
                       (collect (read stream #t nil #t)))
                      (#\;
                       (iter (until (char= (read-char stream #f #\Newline #t) #\Newline))))
                      (t
                       (bind ((attribute-name (read-quasi-quoted-xml-name stream start-character
                                                                          end-character unquote-character)))
                         (when (starts-with #\: attribute-name :test #'char=)
                           (setf attribute-name (subseq attribute-name 1)))
                         (assert-valid-xml-name attribute-name stream)
                         (collect attribute-name)
                         (collect (read stream #t nil #t)))))
                    (finally (read-char stream #t nil #t)))))
      (list* element-name
             attributes
             (bind ((*readtable* (copy-readtable)))
               (set-syntax-from-char end-character #\) *readtable*)
               (read-delimited-list end-character stream t))))))

(def (function o) read-quasi-quoted-xml-name (stream start-character end-character unquote-character)
  (bind ((delimiters (list start-character end-character unquote-character #\( #\) #\space #\newline #\;)))
    (labels ((maybe-signal-eof (value)
               (when (eq value 'eof)
                 (simple-reader-error stream "End of file error while reading an XML name"))
               value)
             (peek ()
               (maybe-signal-eof
                (peek-char nil stream #f 'eof #t)))
             (next-char ()
               (maybe-signal-eof
                (read-char stream #f 'eof #t)))
             (delimiter? (char)
               (member char delimiters :test #'char=)))
      (declare (inline peek next-char delimiter?))
      (iter (while (delimiter? (peek)))
            ;; let's skip delimiters
            (for char = (next-char))
            (when (char= char #\;)
              ;; and unconditionally skip ; comments until the end of line
              (iter (until (char= (next-char) #\Newline)))))
      (iter (with element-name = (make-array 8 :element-type 'character :adjustable #t :fill-pointer 0))
            (for char = (peek-char nil stream #t nil #t))
            (until (member char delimiters :test #'char=))
            (vector-push-extend (next-char) element-name)
            (finally
             (when (zerop (length element-name))
               (simple-reader-error stream "No xml element name?"))
             (assert-valid-xml-name element-name stream)
             (return element-name))))))

(def function assert-valid-xml-name (name &optional stream)
  (when (position-if (lambda (el)
                       (member el '(#\< #\> #\= #\& #\") :test #'char=))
                     name)
    ;; TODO do a proper check for valid xml names...
    (simple-reader-error stream "Illegal character in element name ~S" name)))

(macrolet ((x (name transformation-pipeline &optional args)
             (bind ((syntax-name (format-symbol *package* "QUASI-QUOTED-XML-TO-~A" name))
                    (&key-position (position '&key args)))
               `(define-syntax ,syntax-name (,@(subseq args 0 (or &key-position (length args)))
                                               &key
                                               (with-inline-emitting #f)
                                               (declarations '())
                                               (indentation-width nil)
                                               (start-character #\<)
                                               (end-character #\>)
                                               (unquote-character #\,)
                                               (splice-character #\@)
                                               (destructive-splice-character #\.)
                                               ,@(when &key-position (subseq args (1+ &key-position))))
                  (set-quasi-quoted-xml-syntax-in-readtable :transformation-pipeline ,transformation-pipeline
                                                            :start-character start-character
                                                            :end-character end-character
                                                            :unquote-character unquote-character
                                                            :splice-character splice-character
                                                            :destructive-splice-character destructive-splice-character)))))
  ;; TODO ? (x xml-emitting-form           '(xml-emitting-form))
  ;; TODO make stream-variable-name &key defaulting to *xml-stream*
  (x string-emitting-form (make-quasi-quoted-xml-to-form-emitting-transformation-pipeline
                           stream-variable-name
                           :binary #f
                           :indentation-width indentation-width
                           :text-node-escaping-method text-node-escaping-method
                           :with-inline-emitting with-inline-emitting
                           :declarations declarations)
     (stream-variable-name &key (text-node-escaping-method :per-character)))
  (x binary-emitting-form (make-quasi-quoted-xml-to-form-emitting-transformation-pipeline
                           stream-variable-name
                           :binary #t
                           :indentation-width indentation-width
                           :text-node-escaping-method text-node-escaping-method
                           :encoding encoding
                           :with-inline-emitting with-inline-emitting
                           :declarations declarations)
     (stream-variable-name &key
                           (text-node-escaping-method :per-character)
                           (encoding *default-character-encoding*))))

(def (function e) make-quasi-quoted-xml-to-form-emitting-transformation-pipeline
    (stream-variable-name &key binary with-inline-emitting indentation-width
                          (encoding :utf-8) declarations (text-node-escaping-method :per-character)
                          (disable-short-xml-element-form #f))
  (if binary
      (list (make-instance 'quasi-quoted-xml-to-quasi-quoted-string
                           :text-node-escaping-method text-node-escaping-method
                           :indentation-width indentation-width
                           :disable-short-xml-element-form disable-short-xml-element-form)
            (make-instance 'quasi-quoted-string-to-quasi-quoted-binary
                           :encoding encoding)
            (make-instance 'quasi-quoted-binary-to-binary-emitting-form
                           :stream-variable-name stream-variable-name
                           :with-inline-emitting with-inline-emitting
                           :declarations declarations))
      (list (make-instance 'quasi-quoted-xml-to-quasi-quoted-string
                           :text-node-escaping-method text-node-escaping-method
                           :indentation-width indentation-width
                           :disable-short-xml-element-form disable-short-xml-element-form)
            (make-instance 'quasi-quoted-string-to-string-emitting-form
                           :stream-variable-name stream-variable-name
                           :with-inline-emitting with-inline-emitting
                           :declarations declarations))))

(def macro unless-syntax-node (value &body forms)
  (once-only (value)
    `(if (typep ,value 'syntax-node)
         ,value
         (progn
           ,@forms))))

(def function process-dispatched-xml-reader-body (form transformation-pipeline)
  (labels
      ((expand (form)
         (typecase form
           (cons
            (case (first form)
              (xml-unquote
               (assert (= (length form) 3))
               (make-xml-unquote (second form) (third form)))
              ((or xml-quasi-quote xml-quasi-quote/nested) (error "How did this happen? Send a unit test, please!"))
              (t form)))
           (t form)))
       (recurse (form)
         (typecase form
           (string (make-xml-text form))
           (null (error "Null as an xml element?! For a better error message send a unit test or a patch, please!"))
           (symbol (make-xml-element (name-as-string form)))
           (cons
            (setf form (expand form)) ;; TODO ?
            (unless-syntax-node form
              (bind ((name (aif (pop form)
                                (expand it)
                                (simple-reader-error nil "No xml element name?")))
                     (attributes (expand (pop form)))
                     (children form))
                (assert (or (listp attributes) (typep attributes 'syntax-node)))
                (make-xml-element
                    (unless-syntax-node name
                      (name-as-string name))
                    (unless-syntax-node attributes
                      (iter (for (name value) :on attributes :by #'cddr)
                            ;; TODO cleanup attribute syntax, see below
                            (collect (make-xml-attribute
                                      (unless-syntax-node name (name-as-string name))
                                      (unless-syntax-node value value)))))
                  (mapcar #'recurse children)))))
           (t form))))
    (make-xml-quasi-quote transformation-pipeline (recurse form))))

(def function check-literal-xml-attribute-name-or-value (value)
  (when (consp value)
    (simple-style-warning "Literal list for an xml name or attribute value? Are you sure you are not missing a comma around here: ~S" value)))

(def function process-<>-xml-reader-body (form transformation-pipeline)
  (labels
      ((recurse (form)
         (typecase form
           (cons
            (case (first form)
              (xml-quasi-quote/nested
               (assert (= (length form) 2))
               (bind ((form (second form)))
                 (etypecase form
                   (syntax-node form)
                   (cons
                    (bind ((name (recurse (pop form)))
                           (attributes (recurse (pop form))))
                      (unless name
                        (simple-reader-error nil "Syntax error in XML syntax, node name is NIL!?"))
                      (when (typep attributes '(or string syntax-node))
                        ;; to make the attribute list of foo optional in <foo <bar>> we only accept
                        ;; unquoted attribute lists in the form of <foo (,@(call-some-lisp)) <bar>>.
                        (push attributes form)
                        (setf attributes nil))
                      (make-xml-element
                          (unless-syntax-node name (name-as-string name))
                          (unless-syntax-node attributes
                            (iter (generate element :in attributes)
                                  (for name = (recurse (next element)))
                                  ;; in <a (,name ,value) > we interpret ,name as a full attribute.
                                  ;; this way you can use both (:foo "bar" ,@(list (make-xml-attribute "name" "value")) :baz "alma")
                                  ;; and (:foo "bar" :name ,value :baz "alma") at the same time - although name unquoting is only
                                  ;; possible using MAKE-XML-ATTRIBUTE.
                                  (if (typep name 'syntax-node)
                                      (collect name)
                                      (bind ((value (recurse (next element))))
                                        (collect (make-xml-attribute
                                                  (unless-syntax-node name
                                                    (check-literal-xml-attribute-name-or-value name)
                                                    (name-as-string name))
                                                  (unless-syntax-node value
                                                    (check-literal-xml-attribute-name-or-value value)
                                                    value)))))))
                        (mapcar (lambda (el)
                                  (if (stringp el)
                                      (make-xml-text el)
                                      (recurse el)))
                                form))))
                   (null (simple-reader-error nil "Empty xml tag?")))))
              (xml-unquote
               (assert (= (length form) 3))
               (make-xml-unquote (recurse (second form)) (third form)))
              (t
               (iter (for entry :first form :then (cdr entry))
                     (collect (recurse (car entry)) :into result)
                     (cond
                       ((consp (cdr entry))
                        ;; nop, go on looping
                        )
                       ((cdr entry)
                        (setf (cdr (last result)) (recurse (cdr entry)))
                        (return result))
                       (t (return result)))))))
           (t form))))
    (make-xml-quasi-quote transformation-pipeline (recurse `(xml-quasi-quote/nested ,form)))))

(def function name-as-string (name)
  (etypecase name
    (string name)
    (symbol (symbol-name name))))

;;;;;;
;;; XML emitting

(def (macro e) with-xml-stream (stream &body body)
  `(bind ((*xml-stream* ,stream))
     ,@body))

(def (macro e) emit-into-xml-stream (stream &body body)
  `(bind ((*xml-stream* ,stream))
     (emit (progn ,@body))))

(def (macro e) emit-into-xml-stream-buffer ((&key (external-format *default-character-encoding*)) &body body)
  (with-unique-names (buffer)
    `(with-output-to-sequence (,buffer :external-format ,external-format)
       (bind ((*xml-stream* ,buffer))
         (emit (progn ,@body))))))

(def (function e) emit-xml-prologue (&key (encoding :utf-8) (stream *xml-stream*) (version "1.1"))
  (macrolet ((emit (string)
               `(write-string ,string stream)))
    (if (and (eq encoding :utf-8)
             (string= version "1.1"))
        (emit #.(coerce (format nil "<?xml version=\"1.1\" encoding=\"UTF-8\"?>~%") 'simple-base-string))
        (progn
          (emit "<?xml version=\"")
          (emit (or version "1.1"))
          (emit "\" encoding=\"")
          (emit (string encoding))
          (emit (format nil "\"?>~%"))))))
