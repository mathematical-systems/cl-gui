;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.quasi-quote.css)

(define-syntax (quasi-quoted-css :readtime-wrapper-result-transformer
                                 (lambda (result)
                                   (if (rest result)
                                       (make-css-quasi-quote transformation-pipeline (mapcar 'body-of result))
                                       (first result))))
  (&key   start-character
          end-character
          (unquote-character #\,)
          (splice-character #\@)
          (destructive-splice-character #\.)
          (transformation-pipeline nil)
          (dispatched-quasi-quote-name "css"))
  (bind ((original-reader-on-start-character   (when start-character
                                                 (multiple-value-list (get-macro-character* start-character *readtable*))))
         (original-reader-on-end-character     (when end-character
                                                 (multiple-value-list (get-macro-character* end-character *readtable*))))
         (original-reader-on-unquote-character (multiple-value-list (get-macro-character* unquote-character *readtable*))))
    (awhen (first original-reader-on-start-character)
      (simple-style-warning "Installing the CSS reader on character ~S while it already has a reader installed: ~S" start-character it))
    (set-quasi-quote-syntax-in-readtable
     (named-lambda css-quasi-quote-wrapper (body dispatched?)
       (bind ((toplevel? (= 1 *quasi-quote-nesting-level*))
              (quasi-quote-node (unless *read-suppress*
                                  (if dispatched?
                                      ;; dispatched `css(element) syntax
                                      (process-dispatched-css-reader-body body transformation-pipeline)
                                      ;; <>-based syntax
                                      (process-{}-css-reader-body body transformation-pipeline)))))
         (unless *read-suppress*
           (if toplevel?
               `(toplevel-quasi-quote-macro ,quasi-quote-node)
               quasi-quote-node))))
     (named-lambda css-unquote-wrapper (body modifier)
       (unless *read-suppress*
         (make-css-unquote body modifier)))
     :nested-quasi-quote-wrapper (lambda (body dispatched?)
                                   (if dispatched?
                                       body
                                       `(css-quasi-quote/nested ,body)))
     :start-character start-character
     :end-character end-character
     :unquote-character unquote-character
     :splice-character splice-character
     :destructive-splice-character destructive-splice-character
     :readtable-case :preserve
     :unquote-readtable-case :toplevel
     :dispatched-quasi-quote-name dispatched-quasi-quote-name
     :body-reader (make-quasi-quoted-css-body-reader start-character end-character unquote-character)
     :toplevel-reader-wrapper (lambda (reader)
                                (declare (optimize debug))
                                (named-lambda css-toplevel-reader-wrapper (stream char)
                                  (if start-character
                                      (block nil
                                        (bind ((next-char (peek-char nil stream nil :eof t)))
                                          (if (and (char= char start-character) ; we are installed on the less-then sign...
                                                   (or (eq next-char :eof)
                                                       (not (or (alphanumericp next-char)
                                                                (char= unquote-character next-char)))))
                                              (progn
                                                ;; KLUDGE UNREAD-CHAR after a PEEK-CHAR is not allowed by the standard,
                                                ;; but i don't care much: it works fine on lisps with sane stream buffering,
                                                ;; which includes SBCL.
                                                (unread-char start-character stream)
                                                (bind ((*readtable* (copy-readtable)))
                                                  ;; disable us and call READ recursively to make things like ({ a b) work in unquoted parts
                                                  (apply 'set-macro-character start-character original-reader-on-start-character)
                                                  (when end-character
                                                    (apply 'set-macro-character end-character original-reader-on-end-character))
                                                  (apply 'set-macro-character unquote-character original-reader-on-unquote-character)
                                                  (return (read stream t 'eof t))))
                                              (funcall reader stream char))))
                                      (funcall reader stream char)))))))

(def function read-comment (stream)
  (iter (until (char= (read-char stream #f #\Newline #t) #\Newline))))

(def function make-quasi-quoted-css-body-reader (start-character end-character unquote-character)
  (declare (ignorable start-character end-character unquote-character))
  (named-lambda quasi-quoted-css-body-reader (stream)
    (simple-reader-error stream "make-quasi-quoted-css-body-reader => Not implemented yet.")))

#|
(def function make-quasi-quoted-css-body-reader (start-character end-character unquote-character)
  (named-lambda quasi-quoted-css-body-reader (stream)
    (bind ((selectors nil)
           (attributes (list)))
      (when (char= #\( (peek-char #t stream #t nil #t))
        (read-char stream #t nil #t)
        (setf selectors
              (iter (for next-character = (peek-char #t stream #t nil #t))
                    (until (char= next-character #\)))
                    (eswitch (next-character :test #'char=)
                      (unquote-character
                       (collect (read stream #t nil #t)))
                      (#\;
                       (read-comment stream))
                      (#\(
                       (collect (read-quasi-quoted-css-selector stream start-character end-character unquote-character))))
                    (finally (read-char stream #t nil #t)))))
      (setf attributes
            (iter (for next-character = (peek-char #t stream #f :eof #t)) 
                  (until (eq next-character :eof))
                  (switch (next-character :test #'char=)
                    (unquote-character
                     (collect (read stream #t nil #t)))
                    (#\;
                     (read-comment stream))
                    (t
                     (collect (read-quasi-quoted-css-attribute stream start-character end-character unquote-character))))))
      (list selectors attributes))))

(def (function o) read-quasi-quoted-css-attribute (stream start-character end-character unquote-character)
  (bind ((attribute-name (read-quasi-quoted-css-name stream start-character end-character unquote-character)))
    (when (starts-with #\: attribute-name :test #'char=)
      (setf attribute-name (subseq attribute-name 1)))
    (assert-valid-css-name attribute-name stream)
    (list attribute-name (read stream #t nil #t))))

(def (function o) read-quasi-quoted-css-selector (stream start-character end-character unquote-character)
  (read-char stream #t nil #t)
  (bind ((root-selector (read-quasi-quoted-css-root-selector stream start-character end-character unquote-character))
         (selector-name (first root-selector))
         (arguments (list))
         (relation-selector? (or (equal selector-name ">") (equal selector-name "+") (equal selector-name "-"))))
    (setf arguments
          (iter (for next-character = (peek-char #t stream #t nil #t))
                (until (char= next-character #\) ))
                (switch (next-character :test #'char=)
                  (unquote-character
                   (collect (read stream #t nil #t)))
                  (#\;
                   (read-comment stream))
                  (#\(
                   (if relation-selector?
                       (collect (read-quasi-quoted-css-selector stream start-character end-character unquote-character))
                       (simple-reader-error stream "Invalid selector")))
                  (t
                   (if relation-selector?
                       (simple-reader-error stream "Invalid attribute")
                       (collect (read-quasi-quoted-css-attribute stream start-character end-character unquote-character)))))
                (finally (read-char stream #t nil #t))))
    (when (and relation-selector? (< (length arguments) 2))
      (simple-reader-error stream "At least two selector argument is required."))
    (list selector-name arguments)))

(def (function o) read-quasi-quoted-css-root-selector (stream start-character end-character unquote-character)
  (bind ((tag-name "*")
         (id-name nil)
         (classes (list))
         (pseudo-classes (list))
         (delimiters (list #\# #\. #\:)))
    (labels ((peek (eat-whitespace)
                (aif (and eat-whitespace
                          (char= #\; (peek-char eat-whitespace stream #t nil #t)))
                     (progn (read-comment stream)
                            (peek eat-whitespace))
                     it))
             (unquote? (char)
               (char= char unquote-character))
             (delimiter? (char)
               (member char delimiters :test #'char=))
             (read-name (signal-error)
               (bind ((char (peek #f)))
                 (cond
                  ((unquote? char)
                   (read stream #t nil #t))
                  ((not (delimiter? char))
                   (read-quasi-quoted-css-name stream start-character end-character unquote-character #t))
                  (t
                   (when signal-error
                     (simple-reader-error stream "No CSS name could be read")))))))
      (awhen (read-name #f)
        (setf tag-name it))
;       (format t "root-selector>>> ~S" tag-name)
      (iter (for char = (peek #t))
            (while (delimiter? char))
            (read-char stream #t nil #t)
            (eswitch (char :test #'char=)
              (#\#
               (when id-name
                 (simple-reader-error stream "Only one ID may be specified"))
               (when (or (not (zerop (length pseudo-classes)))
                         (not (zerop (length classes))))
                 (simple-reader-error stream "ID cannot be specified after classes or pseudo-classes"))
               (setf id-name (read-name #t)))
              (#\.
               (when (not (zerop (length pseudo-classes)))
                 (simple-reader-error stream "Class cannot be specified after pseudo-classes"))
               (collect (read-name #t) :into classes))
              (#\:
               (collect (read-name #t) :into pseudo-classes))))
      (list tag-name id-name classes pseudo-classes))))

(def (function o) read-quasi-quoted-css-name (stream start-character end-character unquote-character &optional (special-selector-attribute? nil) )
  (bind ((whitespaces (list #\space #\newline #\;))
         (delimiters (list start-character end-character unquote-character)))
    (when special-selector-attribute?
      (nconc delimiters (list #\. #\# #\:)))
    (labels ((maybe-signal-eof (value)
               (when (eq value 'eof)
                 (simple-reader-error stream "End of file error while reading a CSS name"))
               value)
             (peek ()
               (maybe-signal-eof
                (peek-char nil stream #f 'eof #t)))
             (next-char ()
               (maybe-signal-eof
                (read-char stream #f 'eof #t)))
             (delimiter? (char after)
               (or (member char whitespaces :test #'char=)
                   (and after
                        (member char delimiters :test #'char=)))))
      (declare (inline peek next-char))
      (iter (while (delimiter? (peek) #f))
            ;; let's skip whitespaces
            (for char = (next-char))
            (when (char= char #\;)
              ;; and unconditionally skip ; comments until the end of line
              (iter (until (char= (next-char) #\Newline)))))
      (iter (with element-name = (make-array 8 :element-type 'character :adjustable #t :fill-pointer 0))
            (for char = (peek-char nil stream #t nil #t))
            (until (delimiter? char #t))
            (vector-push-extend (next-char) element-name)
            (finally
             (when (zerop (length element-name))
               (simple-reader-error stream "No css element name?"))
             (assert-valid-css-name element-name stream)
             (return element-name))))))

(def function assert-valid-css-name (name &optional stream)
  (when (position-if (lambda (el)
                       (member el '(#\< #\> #\= #\& #\") :test #'char=))
                     name)
    ;; TODO do a proper check for valid css names...
    (simple-reader-error stream "Illegal character in css name ~S" name)))
|#

(def macro unless-syntax-node (value &body forms)
  (once-only (value)
    `(if (typep ,value 'syntax-node)
         ,value
         (progn
           ,@forms))))

(def (function e) make-quasi-quoted-css-to-form-emitting-transformation-pipeline
    (stream-variable-name &key binary with-inline-emitting indentation-width
                          (encoding :utf-8) declarations)
  (if binary
      (list (make-instance 'quasi-quoted-css-to-quasi-quoted-string
                           :indentation-width indentation-width)
            (make-instance 'quasi-quoted-string-to-quasi-quoted-binary
                           :encoding encoding)
            (make-instance 'quasi-quoted-binary-to-binary-emitting-form
                           :stream-variable-name stream-variable-name
                           :with-inline-emitting with-inline-emitting
                           :declarations declarations))
      (list (make-instance 'quasi-quoted-css-to-quasi-quoted-string
                           :indentation-width indentation-width)
            (make-instance 'quasi-quoted-string-to-string-emitting-form
                           :stream-variable-name stream-variable-name
                           :with-inline-emitting with-inline-emitting
                           :declarations declarations))))

(def function process-dispatched-css-reader-body/make-attribute-value (value top-level?)
  (if (consp value)
      (bind ((first-element (first value)))
        (if (symbolp first-element)
            (switch ((symbol-name first-element) :test #'equal)
              ("url"
               (make-css-annotated-value first-element (second value)))
              ("color"
               (make-css-annotated-value "color" (second value)))
              ("rgb"
               (make-css-annotated-value first-element (cdr value)))
              (t
               (if top-level?
                   (mapcar #'(lambda (value)
                               (process-dispatched-css-reader-body/make-attribute-value value #f)) value)
                   (simple-reader-error nil "Attribute value may be list only in top level"))))
            (if top-level?
                (mapcar #'(lambda (value)
                            (process-dispatched-css-reader-body/make-attribute-value value #f)) value)
                (simple-reader-error nil "Attribute value may be list only in top level"))))
      value))

(def macro process-dispatched-css-reader-body/make-attributes (form maker)
  `(progn
     (iter (generate element :in ,form)
           (for name = (next element))
           (if (typep name 'syntax-node)
               (collect name)
               (bind ((value (next element)))
                 (collect (,maker
                           (unless-syntax-node name
                             (name-as-string name))
                           (unless-syntax-node value
                             ,(if (eq maker 'make-css-attribute)
                                  `(process-dispatched-css-reader-body/make-attribute-value value #t)
                                  'value)))))))))

(def function process-dispatched-css-reader-body (form transformation-pipeline)
  (labels
      ((expand (form)
         (typecase form
           (cons
            (case (first form)
              (css-unquote
               (assert (= (length form) 3))
               (make-css-unquote (second form) (third form)))
              ((or css-quasi-quote css-quasi-quote/nested) (error "How did this happen? Send a unit test, please!"))
              (t form)))
           (t form)))
       (make-selector (form)
         (typecase form
           (string (make-css-element-selector form nil))
           (null (error "Null as a css element?! For a better error message send a unit test or a patch, please!"))
           (symbol (make-css-element-selector (name-as-string form) nil))
           (cons
            (setf form (expand form)) ;; TODO ?
            (unless-syntax-node form
              (bind ((name (aif (pop form)
                                (expand it)
                                (simple-reader-error nil "No css relation selector operator or a tag type?")))
                     (arguments (expand form))
                     (relation-selector? (or (eql name '>) (eql name '+) (eql name '-))))
                (assert (or (listp arguments) (typep arguments 'syntax-node)))
                (if relation-selector?
                    (make-css-relation-selector
                     name
                     (unless-syntax-node arguments
                       (mapcar #'make-selector arguments)))
                    (make-css-element-selector
                     (unless-syntax-node name
                       (name-as-string name))
                     (process-dispatched-css-reader-body/make-attributes arguments make-css-attribute-selector))))))
           (t form)))
       (make-clause (form)
         (bind ((selectors (expand (pop form))))
           (make-css-clause
            (unless-syntax-node selectors
              (mapcar #'make-selector selectors))
            (process-dispatched-css-reader-body/make-attributes (expand form) make-css-attribute)))))
    (make-css-quasi-quote transformation-pipeline
                          (if (consp (first form))
                              (mapcar #'make-clause form)
                              (list (make-css-clause
                                     nil
                                     (process-dispatched-css-reader-body/make-attributes (expand form) make-css-attribute)))))))

(def function process-{}-css-reader-body (form transformation-pipeline)
  (declare (ignorable form transformation-pipeline))
  (simple-reader-error nil "NOT IMPLEMENTED")
#|  (labels
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
    (make-xml-quasi-quote transformation-pipeline (recurse `(xml-quasi-quote/nested ,form))))
  |#
  )

(macrolet ((x (name transformation-pipeline &optional args)
             (bind ((syntax-name (format-symbol *package* "QUASI-QUOTED-CSS-TO-~A" name))
                    (&key-position (position '&key args)))
               `(define-syntax ,syntax-name (,@(subseq args 0 (or &key-position (length args)))
                                               &key
                                               (with-inline-emitting #f)
                                               (declarations '())
                                               (indentation-width nil)
                                               start-character
                                               end-character
                                               (unquote-character #\,)
                                               (splice-character #\@)
                                               (destructive-splice-character #\.)
                                               ,@(when &key-position (subseq args (1+ &key-position))))
                  (set-quasi-quoted-css-syntax-in-readtable :transformation-pipeline ,transformation-pipeline
                                                            :start-character start-character
                                                            :end-character end-character
                                                            :unquote-character unquote-character
                                                            :splice-character splice-character
                                                            :destructive-splice-character destructive-splice-character)))))
  ;; TODO ? (x css-emitting-form           '(css-emitting-form))
  (x string-emitting-form (make-quasi-quoted-css-to-form-emitting-transformation-pipeline
                           stream-variable-name
                           :binary #f
                           :indentation-width indentation-width
                           :with-inline-emitting with-inline-emitting
                           :declarations declarations)
     (stream-variable-name &key))
  (x binary-emitting-form (make-quasi-quoted-css-to-form-emitting-transformation-pipeline
                           stream-variable-name
                           :binary #t
                           :indentation-width indentation-width
                           :encoding encoding
                           :with-inline-emitting with-inline-emitting
                           :declarations declarations)
     (stream-variable-name &key
                           (encoding *default-character-encoding*))))

(def function name-as-string (name)
  (etypecase name
    (string name)
    (symbol (symbol-name name))))

