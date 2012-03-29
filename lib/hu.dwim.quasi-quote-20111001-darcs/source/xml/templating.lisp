;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2010 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.quasi-quote.xml)

(def (special-variable e :documentation "This special variable is bound to NIL while emitting document templates. No more, no less.")
    *document-template-context*)

(define-syntax xml-template (&key (stream-variable-name '*xml-stream*) (binary #t) (with-inline-emitting #t) indentation-width)
  (set-quasi-quoted-list-to-list-emitting-form-syntax-in-readtable)
  (if binary
      (progn
        (set-quasi-quoted-string-to-binary-emitting-form-syntax-in-readtable
         stream-variable-name
         :encoding :utf-8
         :with-inline-emitting with-inline-emitting)
        (set-quasi-quoted-xml-to-binary-emitting-form-syntax-in-readtable
         stream-variable-name
         :encoding :utf-8
         :text-node-escaping-method :per-character
         :indentation-width indentation-width
         :with-inline-emitting with-inline-emitting)
        (set-quasi-quoted-string-to-binary-emitting-form-syntax-in-readtable
         stream-variable-name
         :encoding :utf-8
         :with-inline-emitting with-inline-emitting))
      (progn
        (set-quasi-quoted-string-to-string-emitting-form-syntax-in-readtable
         stream-variable-name
         :with-inline-emitting with-inline-emitting)
        (set-quasi-quoted-xml-to-string-emitting-form-syntax-in-readtable
         stream-variable-name
         :text-node-escaping-method :per-character
         :indentation-width indentation-width
         :with-inline-emitting with-inline-emitting)
        (set-quasi-quoted-string-to-string-emitting-form-syntax-in-readtable
         stream-variable-name
         :with-inline-emitting with-inline-emitting))))

(def function ensure-qq-template-for-xml-template (input-filename output-filename &key force automatic-style-emitter)
  (check-type automatic-style-emitter (or null symbol))
  (when (or force
            (not (fad:file-exists-p output-filename))
            (> (file-write-date input-filename)
               (file-write-date output-filename)))
    (with-input-from-file (input input-filename :element-type :default)
      (with-output-to-file (*standard-output* output-filename :if-exists :supersede :element-type :default)
        (bind ((*xml-indent-level* 0)
               (*node-stack* ())
               (tag-names-to-delay '())
               (delayed-nodes ()))
          (declare (special *node-stack*))
          (labels ((dom-node-with-tag-name? (node &rest tag-names)
                     (when (typep node 'dom:element)
                       (member (dom:tag-name node) tag-names :test 'equal)))
                   (dom-node-text-value (node)
                     (when (typep node 'dom:text)
                       (bind ((value (dom:node-value node)))
                         (unless (every #'whitespace? value)
                           value))))
                   (recurse (node)
                     (bind ((*node-stack* (cons node *node-stack*)))
                       (declare (special *node-stack*))
                       (etypecase node
                         (dom:element
                          (bind ((tag-name (dom:tag-name node)))
                            (if (member tag-name tag-names-to-delay :test 'equal)
                                (push node delayed-nodes)
                                (bind ((children (dom:child-nodes node))
                                       (first-child (unless (length= 0 children)
                                                      (first-elt children))))
                                  (if (and (dom-node-with-tag-name? node "text:p")
                                           (length= 1 children)
                                           (typep first-child 'dom:text)
                                           (starts-with-subseq "!,(" (string-trim-whitespace (dom-node-text-value first-child))))
                                      (recurse first-child)
                                      (progn
                                        (format-quasi-quoted-xml/dom/open-tag node)
                                        (format-quasi-quoted-xml/dom/attributes node)
                                        (bind ((*xml-indent-level* (1+ *xml-indent-level*)))
                                          (map nil #'recurse (dom:child-nodes node)))
                                        (cond
                                          ((and automatic-style-emitter
                                                (equal tag-name "office:automatic-styles"))
                                           (format t ",(~A)" (fully-qualified-symbol-name automatic-style-emitter))
                                           (map nil #'recurse (dom:child-nodes node)))
                                          ((equal tag-name "office:document")
                                           (setf tag-names-to-delay ())
                                           (map nil #'recurse (reverse delayed-nodes))))
                                        (format-quasi-quoted-xml/dom/close-tag node)))))))
                         (dom:text
                          (emit-text-node-value node)))))
                   (emit-text-node-value (node)
                     (bind ((value (dom:node-value node)))
                       (unless (every #'whitespace? value)
                         #+nil
                         (when (search "render-odt" value)
                           (break "~S, ~S" value *node-stack*))
                         (if (dom-node-with-tag-name? (second *node-stack*) "text:p" "text:span")
                             ;; TODO this regexp is dumb here... e.g. "foo ,(fn (x) (y)) bar"
                             (bind ((pieces (cl-ppcre::all-matches-as-strings ",\\(.*?\\)+|([^,]|,[^\\(])+" value)))
                               (dolist (piece pieces)
                                 (cond
                                   ((and (> (length piece) 2)
                                         (starts-with-subseq ",(" piece))
                                    (write-string piece))
                                   (t
                                    (format t " \"~A\"" (escape-as-xml piece))))))
                             (format t " \"~A\"" (escape-as-xml value)))))))
            (bind ((document (cxml:parse input (cxml-dom:make-dom-builder)
                                         :entity-resolver (constantly (make-in-memory-input-stream ""))
                                         :validate #f)))
              (map nil #'recurse (dom:child-nodes document))))))))
  output-filename)

(def (function e) emit-xml-template (root-directory template-relative-filename output
                                                    home-package &key
                                                    (stream-variable-name '*xml-stream*)
                                                    if-exists force automatic-style-emitter)
  (flet ((body (output-stream)
           (with-local-readtable
             (bind ((qq-filename (shadow-temporary-filename root-directory template-relative-filename "qq-document-templates")))
               (ensure-directories-exist qq-filename)
               (enable-xml-template-syntax :stream-variable-name stream-variable-name)
               (with-input-from-file (input-stream (ensure-qq-template-for-xml-template
                                                    (string+ root-directory template-relative-filename)
                                                    qq-filename
                                                    :force force
                                                    :automatic-style-emitter automatic-style-emitter))
                 (bind ((form (bind ((*package* (find-package home-package)))
                                (read input-stream))))
                   (progv
                       (list stream-variable-name)
                       (list output-stream)
                     (emit-xml-prologue :version "1.0" :encoding :utf-8)
                     (funcall (compile nil `(lambda ()
                                              (declare (optimize compilation-speed))
                                              (bind ((*document-template-context* (if (boundp '*document-template-context*)
                                                                                      *document-template-context*
                                                                                      nil))
                                                     (*package* (find-package ',home-package)))
                                                ,form)))))))))))
    (if (streamp output)
        (body output)
        (with-output-to-file (output-stream output :if-exists if-exists :element-type :default)
          (body output-stream))))
  output)
