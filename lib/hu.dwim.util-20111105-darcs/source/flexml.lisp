;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2010 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util.flexml)

(def (namespace e :test 'equalp :weakness :value) lisp-package-for-xml-namespace)

(def (function e) register-xml-namespace (xml-namespace-uri lisp-package-designator)
  (check-type lisp-package-designator (or symbol string))
  (check-type xml-namespace-uri string)
  (bind ((lisp-package (find-package lisp-package-designator)))
    (assert lisp-package () "~S: package ~S not found" 'register-xml-namespace lisp-package-designator)
    (setf (find-lisp-package-for-xml-namespace xml-namespace-uri) lisp-package)))

(register-xml-namespace +xml-namespace-uri+ :hu.dwim.util.xml)

(def (class* ea) builder (sax:default-handler)
  ((default-package)
   (id-attributes (make-hash-table :test 'equal)
                  :initarg nil
                  :documentation "Hashtable of (namespace-uri name) -> id-attribute-entry")
   (element-stack ())
   (root)
   (drop-whitespace #f
                    :type boolean
                    :accessor drop-whitespace?)
   (default-node-class nil
                       :type (or symbol class)
                       :documentation "The default value for CLASS-FOR-NODE-NAME. When NIL an error is thrown when an unknown tag is encountered, otherwise its value is used to instantiate the node.")
   (include-default-values t
                           :type boolean
                           :accessor include-default-values?)
   (id->node (make-hash-table :test #'equal))
   (cross-referencing-slots nil)))

(def class* id-attribute-entry ()
  ((id->node (make-hash-table :test 'equal))
   (namespace-uri)
   (name)))

(def constructor (builder id-attributes)
  (etypecase id-attributes
    (null)
    (cons
     (dolist (entry id-attributes)
       (let ((namespace-uri nil)
             (name nil))
         (if (consp entry)
             (progn
               (assert (= 2 (length entry)))
               (setf namespace-uri (first entry))
               (setf name (second entry)))
             (progn
               (assert (stringp entry))
               (setf name entry)))
         (setf (gethash (list namespace-uri name) (id-attributes-of -self-))
               (make-instance 'id-attribute-entry
                              :namespace-uri namespace-uri
                              :name name)))))
    (hash-table
     ;; let's assume the user knows what they want and it's fine as it is...
     (setf (id-attributes-of -self-) id-attributes))))

(def (function e) make-builder (&key (default-package nil default-package-p) (include-default-values t)
                                     (default-node-class nil default-node-class-p) id-attributes
                                     (drop-whitespace #f))
  (bind ((builder (make-instance 'builder
                                 :include-default-values include-default-values
                                 :id-attributes id-attributes)))
    (when default-package-p
      (setf (default-package-of builder) default-package))
    (setf (drop-whitespace? builder) drop-whitespace)
    (when default-node-class-p
      (setf (default-node-class-of builder) default-node-class))
    builder))

(def (class* ea) node ()
  ((hu.dwim.util.xml:id nil)
   (parent nil)
   (children nil)
   (attributes nil)
   (local-name nil)))

(def method print-object ((self node) *standard-output*)
  (pprint-logical-block (nil nil :prefix "<" :suffix ">")
    (princ (local-name-of self))
    (when (attributes-of self)
      (write-char #\Space)
      (bind ((attributes (loop
                            :for attribute :in (if (listp (attributes-of self))
                                                   (attributes-of self)
                                                   (hash-table-keys (attributes-of self)))
                            :collect (list attribute (attribute-value self attribute))))
             (width (loop
                       :for attribute :in attributes
                       :when (and (consp attribute)
                                  (stringp (first attribute)))
                       :maximize (length (first attribute))))
             (format-control (concatenate 'string "~" (princ-to-string width) "A ~A")))
        (pprint-logical-block (nil attributes :prefix "(" :suffix ")")
          (pprint-exit-if-list-exhausted)
          (loop
             :with first? = t
             :for (attribute-name attribute-value) = (pprint-pop)
             :unless first? :do (pprint-newline :mandatory)
             :do
             (progn
               (setf first? nil)
               (format *standard-output* format-control attribute-name attribute-value)
               (pprint-exit-if-list-exhausted))))))
    (pprint-indent :block 2)
    (when (children-of self)
      (pprint-newline :mandatory)
      (pprint-logical-block (nil (coerce (children-of self) 'list))
        (pprint-exit-if-list-exhausted)
        (loop
           :with first? = t
           :unless first? :do (pprint-newline :mandatory)
           :do (progn
                 (setf first? nil)
                 (write (pprint-pop))
                 (pprint-exit-if-list-exhausted)))))))

(def function attribute-value (node attribute-name)
  (let ((attributes (attributes-of node)))
    (if (listp attributes)
        (let ((entry (assoc attribute-name attributes)))
          (values (cdr entry) (not (null entry))))
        (gethash attribute-name attributes))))

(def function (setf attribute-value) (new-value node attribute-name)
  (let ((attributes (attributes-of node)))
    (if (listp attributes)
        (let ((entry (assoc attribute-name attributes)))
          (if entry
              (setf (cdr entry) new-value)
              (push (cons attribute-name new-value) (attributes-of node))))
        (if new-value
            (setf (gethash attribute-name attributes) new-value)
            (remhash attribute-name attributes))))
  new-value)

(def method slot-missing ((class t) (node node) slot-name operation &optional new-value)
  (ecase operation
    (slot-value
     (attribute-value node slot-name))
    (setf
     (setf (attribute-value node slot-name) new-value))
    (slot-boundp
     t)
    (slot-makunbound
     (error "Flexml NODE's do not support SLOT-MAKUNBOUND"))))

(def function error/missing-cross-reference (node slot id)
  (unless (symbolp slot)
    (setf slot (closer-mop:slot-definition-name slot)))
  (error "Referenced node with id ~S in slot ~S of node ~A was not found" id slot node))

(def method sax:end-document ((builder builder))
  (loop
     for (node . slot) :in (cross-referencing-slots-of builder)
     for slot-value = (closer-mop:slot-value-using-class (class-of node) node slot) do
       ;; TODO this needs to be updated to follow the id-attributes stuff
       (ecase (closer-mop:slot-definition-type slot)
         (cross-referenced-node
          (assert (stringp slot-value))
          (let ((referenced-node (gethash slot-value (id->node-of builder))))
            (unless referenced-node
              (error/missing-cross-reference node slot slot-value))
            (setf (closer-mop:slot-value-using-class (class-of node) node slot)
                  referenced-node)))
         (cross-referenced-nodes
          (assert (stringp slot-value))
          (let ((ids (cl-ppcre:split " " slot-value))
                (id->node (id->node-of builder))
                (referenced-nodes ()))
            (dolist (id ids)
              (let ((referenced-node (gethash id id->node)))
                (unless referenced-node
                  (error/missing-cross-reference node slot id))
                (push referenced-node referenced-nodes)))
            (setf (closer-mop:slot-value-using-class (class-of node) node slot)
                  (nreverse referenced-nodes))))))
  (root-of builder))

(def (type e) cross-referenced-nodes ()
  t)

(def (type e) cross-referenced-node ()
  t)

(def (generic e) class-name-for-node-name (builder namespace-uri package local-name qualified-name)
  (:method (builder namespace-uri package (local-name string) qualified-name)
    (find-symbol (string-upcase local-name) (default-package-of builder))))

(def (generic e) class-for-node-name (builder namespace-uri package local-name qualified-name)
  (:method (builder namespace-uri package (local-name string) qualified-name)
    (bind ((class-name (class-name-for-node-name builder namespace-uri package local-name qualified-name))
           (class (find-class class-name nil))
           (default-node-class (default-node-class-of builder)))
      (when (and default-node-class
                 (symbolp default-node-class))
        (setf default-node-class (find-class default-node-class)))
      (or class
          default-node-class
          (let ((*package* (find-package :cl-user)))
            (error "Did not find a class named ~S to represent the flexml node ~S in XML namespace ~S"
                   class-name local-name (or namespace-uri :default)))))))

(def method sax:start-element ((builder builder) namespace-uri local-name qname attributes)
  (bind ((include-default-values (include-default-values? builder))
         (package (if namespace-uri
                      (find-lisp-package-for-xml-namespace namespace-uri)
                      (default-package-of builder)))
         (class (progn
                  (unless package
                    (error "Could not find a lisp package for the xml namespace ~S" namespace-uri))
                  (class-for-node-name builder namespace-uri package local-name qname))))
    (assert class)
    (assert (subtypep class 'node))
    (bind ((parent (first (element-stack-of builder)))
           (node (make-instance class))
           (id-attributes (id-attributes-of builder)))
      (setf (local-name-of node) local-name)
      (loop
         for attribute in attributes
         for attribute-local-name = (sax:attribute-local-name attribute)
         for attribute-namespace-uri = (or (sax:attribute-namespace-uri attribute)
                                           (when (member attribute-local-name '("base" "id" "lang" "space")
                                                         :test #'string=)
                                             ;; KLUDGE this shouldn't be here when parsing XML 1.1
                                             +xml-namespace-uri+))
         for attribute-package = (if attribute-namespace-uri
                                     (find-lisp-package-for-xml-namespace attribute-namespace-uri)
                                     (default-package-of builder))
         for attribute-value = (sax:attribute-value attribute)
         for id-attribute-entry = (gethash (list namespace-uri attribute-local-name) id-attributes)
         when id-attribute-entry do
           (let ((id->node (id->node-of id-attribute-entry)))
             (assert (not (gethash attribute-value id->node)) () "Duplicate id found for attribute ~A:~A, value is ~S"
                     (namespace-uri-of id-attribute-entry) (name-of id-attribute-entry) attribute-value)
             (setf (gethash attribute-value id->node) node))
         when (and (or (sax:attribute-specified-p attribute)
                       include-default-values)) do
           (bind ((slot-name (intern (string-upcase attribute-local-name) attribute-package))
                  (slot (find-slot class slot-name :otherwise nil)))
             (if slot
                 ;; then grab its SLOT-DEFINITION-TYPE and process the value accordingly
                 (let ((slot-type (closer-mop:slot-definition-type slot)))
                   (when (and (consp slot-type)
                              (eq (first slot-type) 'or))
                     (unless (and (= (length slot-type) 3)
                                  (eq (second slot-type) 'null))
                       (error "Error parsing slot type ~S" slot-type))
                     (setf slot-type (third slot-type)))
                   (cond ((member slot-type '(cross-referenced-node
                                              cross-referenced-nodes))
                          (push (cons node slot) (cross-referencing-slots-of builder)))
                         ((subtypep slot-type 'integer)
                          (setf attribute-value (parse-integer attribute-value)))
                         ((subtypep slot-type 'boolean)
                          (setf attribute-value (not (null (member attribute-value
                                                                   '("1" "true" "TRUE")
                                                                   :test #'string=))))))
                   (setf (closer-mop:slot-value-using-class (class-of node) node slot)
                         attribute-value))
                 (setf (slot-value node slot-name) attribute-value))))
      (if parent
          (progn
            (setf (parent-of node) parent)
            (push node (children-of parent)))
          (setf (root-of builder) node))
      (push node (element-stack-of builder))
      node)))

(def method sax:end-element ((builder builder) namespace-uri local-name qname)
  (bind ((node (pop (element-stack-of builder))))
    (setf (children-of node)
          (loop
             with old-children = (children-of node)
             with child-count  = (length old-children)
             with new-children = (if (zerop child-count)
                                     #() ; use a single array instace to represent the no children case
                                     (make-array child-count))
             for index :from (1- child-count) :downto 0
             do (setf (aref new-children index) (pop old-children))
             finally (return new-children)))
    node))

(def method sax:characters ((builder builder) data)
  (check-type data string)
  (bind ((parent (first (element-stack-of builder)))
         (previous (first (children-of parent))))
    (when (drop-whitespace? builder)
      (setf data (string-trim-whitespace data)))
    (unless (zerop (length data))
      (if (stringp previous)
          (setf (car (children-of parent)) (concatenate 'string previous data))
          (push data (children-of parent))))))

;;;
;;; some useful helpers
;;;
(def (function e) string-content-of (node)
  (bind ((child-count (length (children-of node))))
    (unless (= 1 child-count)
      (error "Flexml node ~A has ~A children while expecting a single string content" node child-count))
    (let ((value (elt (children-of node) 0)))
      (unless (stringp value)
        (error "The single child ~S of flexml node ~A is not a string" value node))
      value)))

(def (function e) first-child (node)
  (elt (children-of node) 0))

(def (function e) first-child-of-type (node type)
  (loop
    :for child :across (children-of node)
    :do (when (typep child type)
          (return child))))

(def (function e) first-child-with-local-name (node name)
  (loop for child :across (children-of node) do
       (when (string= (local-name-of child) name)
         (return-from first-child-with-local-name child))))

(def (function e) the-only-child (node)
  (let ((children (children-of node)))
    (assert (= 1 (length children)))
    (elt children 0)))

(def generic find-node-by-id (id builder &key otherwise)
  (:method ((id string) (builder builder) &key (otherwise :error))
    (let ((result (gethash id (id->node-of builder))))
      (unless result
        (case otherwise
          (:error (error "No XML node found with id ~S" id))
          (:warn (warn "No XML node found with id ~S" id))
          (t (if (functionp otherwise)
                 (funcall otherwise)
                 otherwise))))
      result)))

(def (macro e) node-local-name-eswitch (value &body forms)
  (once-only (value)
    `(when (typep ,value 'node)
       (bind ((-children- (children-of ,value))
              (-content- (when (and (length= 1 -children-)
                                    (stringp (first-elt -children-)))
                           (string-content-of ,value))))
         (declare (ignorable -children- -content-))
         (flet ((-recurse- (function)
                  (map 'list function -children-)))
           (declare (ignorable #'-recurse-))
           (eswitch ((local-name-of ,value) :test #'string=)
             ,@forms))))))

(def (macro e) children-local-name-eswitch ((node &key (trim-whitespace #t)) &body forms)
  (with-unique-names (children)
    (once-only (node)
      ;; NOTE: iter screws up the lexenv...
      `(loop
         :for -child- :across (children-of ,node)
         :do (bind ((,children (children-of -child-))
                    (-content- (when (and (length= 1 ,children)
                                          (stringp (first-elt ,children)))
                                 (string-content-of -child-))))
               (declare (ignorable -content-))
               ,(when trim-whitespace
                  `(when -content-
                     (setf -content- (string-trim-whitespace -content-))))
               (eswitch ((local-name-of -child-) :test #'string=)
                 ,@forms))))))