;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.quasi-quote.xml)

;;;;;;
;;; DOM based helpers

(def (function e) format-quasi-quoted-xml/dom/attributes (node)
  (bind ((attributes (dom:items (dom:attributes node))))
    (when (zerop (length attributes))
      (setf attributes nil))
    (when attributes
      (write-string " (")
      (flet ((attribute-value (attribute)
               (bind ((children (dom:child-nodes attribute)))
                 (assert (length= 1 children))
                 (dom:node-value (first-elt children)))))
        (iter (for attribute :in attributes)
              (unless (first-time-p)
                (write-string " "))
              (format t "~A \"~A\"" (dom:name attribute) (attribute-value attribute))))
      (write-string ")"))))

(def (function e) format-quasi-quoted-xml/dom/open-tag (node)
  (check-type node dom:element)
  (bind ((name (dom:tag-name node)))
    (format t "~&~A<~A" (make-string-of-spaces (* 2 *xml-indent-level*)) name)))

(def (function e) format-quasi-quoted-xml/dom/close-tag (node)
  (check-type node dom:element)
  (write-string ">"))

;;;;;;
;;; SAX full emitter (hardly customizable at all)

(def (function e) format-quasi-quoted-xml (input)
  "Emit an xml input as quasi quoted string. Input can be anything acceptable by cxml:parse."
  (bind ((*xml-indent-level* 0))
    (cxml:parse input (make-instance 'format-quasi-quoted-xml/sax-handler)
                :entity-resolver (constantly (make-in-memory-input-stream ""))
                :validate #f)))

(def class* format-quasi-quoted-xml/sax-handler (sax:default-handler)
  ((element-handler (constantly #f))))

(defmethod sax:start-element ((builder format-quasi-quoted-xml/sax-handler) namespace-uri local-name qname attributes)
  (unless (funcall (element-handler-of builder) namespace-uri local-name qname attributes)
    (format t "~%~A<~A" (make-string-of-spaces (* 2 *xml-indent-level*)) qname)
    (incf *xml-indent-level*)
    (when (plusp (length attributes))
      (format t " ("))
    (iter (for attribute :in attributes)
          (unless (first-time-p)
            (write-string " "))
          (format t "~A \"~A\"" (sax:attribute-qname attribute) (sax:attribute-value attribute)))
    (when (plusp (length attributes))
      (write-string ")"))))

(defmethod sax:end-element ((builder format-quasi-quoted-xml/sax-handler) namespace-uri local-name qname)
  (write-string ">")
  (decf *xml-indent-level*))

(defmethod sax:characters ((builder format-quasi-quoted-xml/sax-handler) data)
  (bind ((chars (escape-as-xml (string-trim +whitespace-characters+ data))))
    (when (plusp (length chars))
      (format t " \"~A\"" chars))))
