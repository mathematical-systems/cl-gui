;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.quasi-quote.css)

(def (transformation e) quasi-quoted-css-to-quasi-quoted-string ()
  ((class-transformation-method :short-form :type (member :short-form :standard-form))
   (omit-universal-selector #t :type boolean :accessor omit-universal-selector?)
   (indentation-width nil))
  'transform-quasi-quoted-css-to-quasi-quoted-string)

(def print-object quasi-quoted-css-to-quasi-quoted-string
  (princ "[CSS->String]"))

(def function transform-quasi-quoted-css-to-quasi-quoted-string (node)
  (assert (typep node 'css-quasi-quote))
  (make-string-quasi-quote (rest (transformation-pipeline-of node))
                           (transform-quasi-quoted-css-to-quasi-quoted-string/root (body-of node))))

(def special-variable *css-indent-level* 0)

(def macro with-increased-css-indent-level (&body body)
  `(bind ((*css-indent-level* (1+ *css-indent-level*)))
     ,@body))

(def macro with-runtime-css-indent-level (&body body)
  `(wrap-forms-with-bindings
    (when (indentation-width-of *transformation*)
      `((*css-indent-level* (+ *css-indent-level* ,*css-indent-level*))))
    ,@body))

(def macro process-unquote (unquote-node fn &optional (declare? #f) (spliceable? #f) (splice-infix nil))
  (bind ((node-var (gensym "node"))
         (spliced-var (gensym "spliced"))
         (fn-var (gensym "fn"))
         (quoted-form
          (if spliceable?
              ``(if ,,spliced-var 
                    (iter (for child-node :in-sequence ,',node-var)
                          ,',(when splice-infix
                                   `(unless (first-iteration-p)
                                      (collect ,splice-infix)))
                          (collect (funcall ,,fn-var child-node)))
                    (funcall ,,fn-var ,',node-var))
              ``(funcall ,',fn-var ,',node-var))))
    (when declare?
      (setf quoted-form
            ``(locally
                  (declare #+sbcl(sb-ext:muffle-conditions sb-ext:compiler-note))
                ,,quoted-form)))
    (bind ((wrapped-form
            `(make-string-unquote
              (wrap-runtime-delayed-transformation-form
               `(bind ((,',node-var ,(form-of ,node-var))
                       (,',fn-var ,,fn-var))
                  ,,quoted-form)))))
      (when spliceable?
        (setf wrapped-form
              `(bind ((,spliced-var (spliced? ,node-var)))
                 ,wrapped-form)))
      `(bind ((,node-var ,unquote-node)
              (,fn-var ,fn))
         ,wrapped-form))))

(def macro with-unquote-support (node fn &optional (declare? #f) (spliceable? #f) (splice-infix nil))
  (bind ((node-var (gensym "node"))
         (fn-var (gensym "fn")))
    `(bind ((,node-var ,node)
            (,fn-var ,fn))
       (typecase ,node-var
         (css-unquote
          (process-unquote ,node-var ,fn-var ,declare? ,spliceable? ,splice-infix))
         (t
          (funcall ,fn-var ,node-var))))))

(def function transform-quasi-quoted-css-to-quasi-quoted-string/name (node)
  (transformation-typecase node
    (string node)
    ;; TODO: Check whether this unquote is needed at all.
    (css-unquote (bind ((form (form-of node)))
                   (if (self-evaluating? form)
                       (transform-quasi-quoted-css-to-quasi-quoted-string/name form)
                       (make-string-unquote
                        (wrap-runtime-delayed-transformation-form form)))))
    (unquote node)
    (integer node)
    (string-quasi-quote node)))

(def function transform-quasi-quoted-css-to-quasi-quoted-string/root (node)
  (bind ((indent-level (awhen (indentation-width-of *transformation*)
                         (list (make-string-of-spaces (* it *css-indent-level*)))))
         (indent-new-line (when indent-level
                            '(#\NewLine))))
    (transformation-typecase node
      (cons
       (iter (for clause-node :in-sequence node)
             (unless (first-iteration-p)
               (collect `(,@indent-new-line ,@indent-new-line)))
             (collect (with-unquote-support clause-node
                        #'transform-quasi-quoted-css-to-quasi-quoted-string/clause #t #t `(,@indent-new-line ,@indent-new-line))))))))

(def function transform-quasi-quoted-css-to-quasi-quoted-string/clause (node)
  (bind ((indent-level (awhen (indentation-width-of *transformation*)
                         (list (make-string-of-spaces (* it *css-indent-level*)))))
         (indent-new-line (when indent-level
                            '(#\NewLine))))
    (transformation-typecase node
      (css-clause
       (bind ((selectors (selectors-of node))
              (attributes (attributes-of node)))
         `(,@(when selectors
                   (transform-quasi-quoted-css-to-quasi-quoted-string/selector-list selectors ", " indent-new-line))
             ,@(when selectors
                     `(" { " ,@indent-new-line))
             ,@(iter (for attribute :in-sequence attributes)
                     (if selectors
                         (progn
                           (collect (with-increased-css-indent-level
                                      (with-unquote-support
                                          attribute
                                        #'transform-quasi-quoted-css-to-quasi-quoted-string/attribute #t #t)))
                           (collect indent-new-line))
                         (collect (with-unquote-support
                                      attribute
                                    #'transform-quasi-quoted-css-to-quasi-quoted-string/attribute #t #t))))
             ,@(when selectors
                     `("}" ,@indent-new-line)))))
      (css-quasi-quote
       (if (compatible-transformation-pipelines? *transformation-pipeline*
                                                 (transformation-pipeline-of node))
           (make-string-quasi-quote (rest (transformation-pipeline-of node))
                                    (transform-quasi-quoted-css-to-quasi-quoted-string/clause (body-of node)))
           (transform node)))
      (string-quasi-quote node)
      (cons
       (map 'list 'transform-quasi-quoted-css-to-quasi-quoted-string/clause node))
      (null (values)))
    ))

(def function transform-quasi-quoted-css-to-quasi-quoted-string/selector-list (node operator indent-new-line)
  (transformation-typecase node
    (css-unquote
     (make-string-unquote
      (wrap-runtime-delayed-transformation-form
       `(transform-quasi-quoted-css-to-quasi-quoted-string/selector-list ,(form-of node) ,operator ,indent-new-line))))
    (cons
     (iter (for selector :in-sequence node)
           (unless (first-iteration-p)
             (collect operator))
           (when (equal operator ", ")
             (collect indent-new-line))
           (collect (transform-quasi-quoted-css-to-quasi-quoted-string/selector selector operator indent-new-line))))
    (css-selector
     (transform-quasi-quoted-css-to-quasi-quoted-string/selector node operator indent-new-line))
    (string-quasi-quote node)))

(def function transform-quasi-quoted-css-to-quasi-quoted-string/selector (node operator indent-new-line)
  (transformation-typecase node
    (string node) ;; Only for unquote
    (css-unquote
     (bind ((spliced? (spliced? node)))
       (make-string-unquote
        (wrap-runtime-delayed-transformation-form
         (if spliced?
             `(transform-quasi-quoted-css-to-quasi-quoted-string/selector-list ,(form-of node) ,operator ,indent-new-line)
             `(transform-quasi-quoted-css-to-quasi-quoted-string/selector ,(form-of node) ,operator ,indent-new-line))))))
    (string-quasi-quote node)
    (css-element-selector
     (bind ((name (transform-quasi-quoted-css-to-quasi-quoted-string/name (name-of node)))
            (attributes (attributes-of node)))
       `(,(when (or (not (omit-universal-selector? *transformation*))
                    (not (equal name "*"))
                    (zerop (length attributes)))
                name)
         ,@(when attributes
                 (iter (for attribute :in-sequence attributes)
                       (collect (with-unquote-support
                                          attribute
                                  #'transform-quasi-quoted-css-to-quasi-quoted-string/attribute-selector #f #t)))))))
    (css-relation-selector
     (bind ((elements (elements-of node))
            (operator (ecase (operator-of node)
                        (> " > ")
                        (- " ")
                        (+ " + "))))
       `(,@(transform-quasi-quoted-css-to-quasi-quoted-string/selector-list elements operator indent-new-line))))))

(def function transform-quasi-quoted-css-to-quasi-quoted-string/attribute-selector (node)
  (transformation-typecase node
    (css-attribute-selector
     (bind ((name (name-of node)))
       (if (typep name 'css-unquote)
           (process-unquote name
                            #'transform-quasi-quoted-css-to-quasi-quoted-string/attribute-selector #f #t)
           (bind ((operator (operator-of node)))
             (with-unquote-support (value-of node)
               #'(lambda (node)
                   (transform-quasi-quoted-css-to-quasi-quoted-string/attribute-selector-value name operator node))))))))) 

(def function transform-quasi-quoted-css-to-quasi-quoted-string/attribute-selector-value (name operator node)
  (transformation-typecase node
    (cons
     (iter (for child-node :in-sequence node)
           (collect (with-unquote-support child-node
                      #'(lambda (node)
                          (transform-quasi-quoted-css-to-quasi-quoted-string/attribute-selector-atomic-value name operator node))))))
    (t
     (transform-quasi-quoted-css-to-quasi-quoted-string/attribute-selector-atomic-value name operator node))))

(def function transform-quasi-quoted-css-to-quasi-quoted-string/attribute-selector-atomic-value (name operator value)
  (setf value
        (transform-quasi-quoted-css-to-quasi-quoted-string/atomic-value value #t))
  (cond
    ((and (equal name "class")
          (eq operator '~=)
          (eq (class-transformation-method-of *transformation*) :short-form))
     `("." ,value))
    ((equal name "pseudo-class")
     `(":" ,value))
    ((equal name "id")
     `("#" ,value))
    (t
     `("["
       ,name
       ,(symbol-name operator)
       "\""
       ,value
       "\"]"))))

(def function transform-quasi-quoted-css-to-quasi-quoted-string/attribute (node)
  (transformation-typecase node
    (css-attribute
     (bind ((name (name-of node)))
       (if (typep name 'css-unquote)
           (process-unquote name
                            #'transform-quasi-quoted-css-to-quasi-quoted-string/attribute #f #t) ; TODO: new-line
           `(,name
             ": "
             ,(with-unquote-support (value-of node)
                                    #'transform-quasi-quoted-css-to-quasi-quoted-string/attribute-value)
             "; "))))))

(def function transform-quasi-quoted-css-to-quasi-quoted-string/attribute-value (node)
  (transformation-typecase node
    (cons
     (iter (for child-node :in-sequence node)
           (unless (first-iteration-p)
             (collect " "))
           (collect (with-unquote-support child-node
                      #'transform-quasi-quoted-css-to-quasi-quoted-string/simple-value #t #t " "))))
    (t
     (transform-quasi-quoted-css-to-quasi-quoted-string/simple-value node))))

(def function transform-quasi-quoted-css-to-quasi-quoted-string/simple-value (node)
  (transformation-typecase node
    (css-annotated-value
     (eswitch ((annotation-of node) :test #'equal)
       ("url"
        `("url("
          ,(with-unquote-support (value-of node)
                                 #'transform-quasi-quoted-css-to-quasi-quoted-string/atomic-value)
          ")"))
       ("color"
        `(,(with-unquote-support (value-of node)
                                 #'(lambda (node)
                                     (format nil "#~6,'0x"
                                             (parse-integer (transform-quasi-quoted-css-to-quasi-quoted-string/atomic-value node)))))))
       ("rgb"
        `("rgb("
          ,@(with-unquote-support (value-of node)
                                  #'(lambda (node)
                                      (iter (for component-node :in-sequence node)
                                            (for pos :from 1 :to 3)
                                            (unless (first-iteration-p)
                                              (collect ","))
                                            (collect (with-unquote-support component-node
                                                       #'transform-quasi-quoted-css-to-quasi-quoted-string/atomic-value)))))
          ")"))))
    (t
     (transform-quasi-quoted-css-to-quasi-quoted-string/atomic-value node))))

(def function transform-quasi-quoted-css-to-quasi-quoted-string/atomic-value (node &optional (pure-string? #f))
  (transformation-typecase node
    (integer (integer-to-string node))
    (float (format nil "~F" node))
    (symbol (if (constantp node)
                (symbol-value node)
                (symbol-name node)))
    (string-quasi-quote node) 
    (string (if pure-string?
                node
                `("\"" ,node "\"")))))