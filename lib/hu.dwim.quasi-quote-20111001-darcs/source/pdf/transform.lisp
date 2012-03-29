;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.quasi-quote.pdf)

;;;;;;
;;; Transform
;;;
;;; http://www.mactech.com/articles/mactech/Vol.15/15.09/PDFIntro/

(def special-variable *pdf-environment*)

(def class* pdf-environment ()
  ((object-id-counter 0 :type integer)
   (node-identity-to-object-id (make-hash-table) :type hash-table)
   (xref-position :type integer)
   (xref (make-instance 'pdf-xref) :type pdf-xref)
   (root-reference :type pdf-indirect-object-reference)
   (info-reference nil :type pdf-indirect-object-reference)))

(def function compute-xref-size (xref)
  ;; TODO: 1+ due to the default xref
  (1+ (iter (for section :in (sections-of xref))
            (sum (length (entries-of section))))))

(def function ensure-pdf-object-id-for-node-identity (node-identity)
  (bind ((map (node-identity-to-object-id-of *pdf-environment*))
         (object-id (gethash node-identity map)))
    (or object-id
        (setf (gethash node-identity map) (incf (object-id-counter-of *pdf-environment*))))))

(def function node-binary-position ())

(def function push-xref-entry (object-id generation-number position)
  (bind ((xref (xref-of *pdf-environment*))
         (section (first (sections-of xref)))
         (entry (when section (first (entries-of section)))))
    (when (or (not entry)
              (and entry
                   (not (= object-id (1+ (object-id-of entry))))))
      (setf section (make-instance 'pdf-xref-section))
      (push section (sections-of xref)))
    (push (make-instance 'pdf-xref-entry
                         :free #f
                         :position position
                         :object-id object-id
                         :generation-number generation-number)
          (entries-of section))
    object-id))

(labels ((recurse (node)
           (transform-quasi-quoted-pdf-to-quasi-quoted-bivalent node))
         (transform-dictionary-entry (key value)
           (list (recurse key)
                 #\Space
                 (recurse value)
                 #\NewLine)))
  (declare (inline recurse))
  (defgeneric transform-quasi-quoted-pdf-to-quasi-quoted-bivalent (node)
    (:method ((node function))
      node)

    (:method ((node quasi-quote))
      (if (typep node 'bivalent-quasi-quote)
          (body-of node)
          node))
  
    (:method ((node unquote))
      (if (typep node 'bivalent-quasi-quote)
          (body-of node)
          node))

    (:method ((node side-effect))
      node)

    (:method ((node pdf-quasi-quote))
      (make-bivalent-quasi-quote (recurse (body-of node))))

    (:method ((node pdf-unquote))
      (make-bivalent-unquote
       `(transform-quasi-quoted-pdf-to-quasi-quoted-bivalent
         ,(map-filtered-tree (form-of node) 'pdf-quasi-quote #'recurse))))

    (:method ((node pdf-null))
      "null")

    (:method ((node pdf-boolean))
      (if (value-p node)
          "true"
          "false"))

    (:method ((node pdf-number))
      (bind ((value (value-of node)))
        (etypecase value
          (unquote (recurse value))
          (side-effect (recurse value))
          (integer (princ-to-string value))
          ;; rationals and such are not allowed.
          (t (format nil "~,3F" value)))))

    (:method ((node pdf-name))
      (format nil "/~A" (value-of node)))

    (:method ((node pdf-string))
      (format nil "(~A)" (value-of node)))

    (:method ((node pdf-array))
      (list "["
            (iter (for element :in-sequence (value-of node))
                  (unless (first-iteration-p)
                    (collect #\Space))
                  (collect (recurse element)))
            "]"))

    (:method ((node pdf-stream))
      (with-unique-names (position)
        (bind ((length-node (make-instance 'pdf-indirect-object :content (make-instance 'pdf-number))))
          (setf (value-of (content-of length-node)) (make-bivalent-unquote `(princ-to-string ,position)))
          (make-bivalent-unquote
           `(let (,position)
              ,(make-bivalent-quasi-quote
                (list (format nil "<<~%/Length ")
                      (make-bivalent-unquote
                       `(transform-quasi-quoted-pdf-to-quasi-quoted-bivalent
                         (make-instance 'pdf-indirect-object-reference
                                        :object-id (ensure-pdf-object-id-for-node-identity ,(node-identity-of length-node))
                                        :generation-number ,(generation-number-of length-node))))
                      (format nil "~%>>~%stream~%")
                      (hu.dwim.quasi-quote::make-side-effect `(setf ,position (binary-position)))
                      (iter (for element :in (contents-of node))
                            (unless (first-iteration-p)
                              (collect #\Space))
                            (collect (recurse element)))
                      (hu.dwim.quasi-quote::make-side-effect `(setf ,position (- (binary-position) ,position)))
                      (format nil "~%endstream~%")
                      (recurse length-node))))))))

    (:method ((node pdf-begin-text))
      "BT")

    (:method ((node pdf-end-text))
      "ET")

    (:method ((node pdf-set-font))
      (format nil "/~A ~D Tf" (name-of node) (size-of node)))

    (:method ((node pdf-move-text))
      (format nil "~8f ~8f Td" (x-of node) (y-of node)))

    (:method ((node pdf-display-text))
      "Tj")

    (:method ((node pdf-paragraph))
      (list
       (format nil "/P <</MCID 0>>~%BDC~%")
       (iter (for element :in-sequence (contents-of node))
             (unless (first-iteration-p)
               (collect #\Space))
             (collect (recurse element)))
       (format nil "~%EMC~%")))

    (:method :around ((node pdf-dictionary))
             (list (format nil "<<~%")
                   (call-next-method)
                   ">>"))

    (:method ((node pdf-dictionary))
      (iter (for (key value) :in-hashtable (map-of node))
            (assert (typep key 'pdf-name))
            (appending (transform-dictionary-entry key value))))

    (:method ((node pdf-typed-dictionary))
      (list (bind ((class-name (class-name (class-of node))))
              (unless (eq 'pdf-dictionary class-name)
                (format nil "/Type /~A~%" (string-capitalize (subseq (string-downcase (symbol-name class-name)) 4))))) 
            (call-next-method)))

    (:method ((node pdf-indirect-object))
      (list
       (make-bivalent-unquote
        `(princ-to-string
          (push-xref-entry (ensure-pdf-object-id-for-node-identity ,(node-identity-of node))
                           ,(generation-number-of node) (binary-position))))
       (format nil " ~D obj~%" (generation-number-of node))
       (recurse (content-of node))
       (format nil "~%endobj~%")))

    (:method ((node pdf-indirect-object-reference))
      (bind ((name (name-of node)))
        (when name
          (setf node
                (find name (elements-of (find-ancestor-syntax-node node 'pdf-document))
                      :key (lambda (node)
                             (when (and (typep node 'pdf-indirect-object)
                                        (slot-boundp node 'name))
                               (name-of node))))))
        (if (object-id-of node)
            (format nil "~D ~D R" (object-id-of node) (generation-number-of node))
            (list
             (make-bivalent-unquote `(princ-to-string (ensure-pdf-object-id-for-node-identity ,(node-identity-of node))))
             (format nil " ~D R" (generation-number-of node))))))

    (:method ((node pdf-root))
      (list (call-next-method)
            (hu.dwim.quasi-quote::make-side-effect
             `(setf (root-reference-of *pdf-environment*)
                    (make-instance 'pdf-indirect-object-reference
                                   :object-id (ensure-pdf-object-id-for-node-identity ,(node-identity-of node))
                                   :generation-number ,(generation-number-of node))))))

    (:method ((node pdf-info))
      (list (call-next-method)
            (hu.dwim.quasi-quote::make-side-effect
             `(setf (info-reference-of *pdf-environment*)
                    (make-instance 'pdf-indirect-object-reference
                                   :object-id (ensure-pdf-object-id-for-node-identity ,(node-identity-of node))
                                   :generation-number ,(generation-number-of node))))))

    (:method ((node pdf-xref-entry))
      (format nil "~10,'0D ~5,'0D ~A ~%" (position-of node) (generation-number-of node)
              (if (free-p node) "f" "n")))
    
    (:method ((node pdf-xref-section))
      (bind ((entries (entries-of node)))
        (list
         (format nil "~D ~D~%" (object-id-of (last-elt entries)) (length entries))
         (mapcar #'recurse (reverse entries)))))

    (:method ((node pdf-xref))
      (list (hu.dwim.quasi-quote::make-side-effect `(setf (xref-position-of *pdf-environment*) (binary-position)))
            (format nil "xref~%")
            (mapcar #'recurse (reverse (sections-of node)))
            (make-bivalent-unquote '(mapcar 'transform-quasi-quoted-pdf-to-quasi-quoted-bivalent (sections-of (xref-of *pdf-environment*))))))

    (:method ((node pdf-header))
      (format nil "%PDF-~A~%%Non ASCII marker: í~%" (version-of node)))

    (:method :around ((node pdf-trailer))
             (list (format nil "trailer~%")
                   (call-next-method)
                   (format nil "~%startxref~%")
                   (make-bivalent-unquote '(princ-to-string (xref-position-of *pdf-environment*)))
                   (format nil "~%%%EOF~%")))

    (:method ((node pdf-trailer))
      (list
       (transform-dictionary-entry
        (make-pdf-name "Root")
        (make-bivalent-unquote '(transform-quasi-quoted-pdf-to-quasi-quoted-bivalent (root-reference-of *pdf-environment*))))
       (transform-dictionary-entry
        (make-pdf-name "Info")
        (make-bivalent-unquote '(transform-quasi-quoted-pdf-to-quasi-quoted-bivalent (info-reference-of *pdf-environment*))))
       (transform-dictionary-entry
        (make-pdf-name "Size")
        (make-bivalent-unquote '(princ-to-string (compute-xref-size (xref-of *pdf-environment*)))))))

    (:method ((node pdf-document))
      (list
       (recurse (header-of node))
       (mapcar #'recurse (elements-of node))
       (recurse (xref-of node))
       (recurse (trailer-of node))))))

(def method transform ((to (eql 'quasi-quoted-bivalent)) (input pdf-syntax-node) &key &allow-other-keys)
  (transform-quasi-quoted-pdf-to-quasi-quoted-bivalent input))

(def method setup-emitting-environment ((to (eql 'quasi-quoted-pdf)) &key next-method &allow-other-keys)
  (bind ((*pdf-environment* (make-instance 'pdf-environment)))
    (funcall next-method)))
