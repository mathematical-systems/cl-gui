;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.walker)

(def (macro e) do-ast-links ((item form &rest flags &key rewrite &allow-other-keys) &body code)
  (with-unique-names (link-walker)
    `(block nil
       (flet ((,link-walker (-parent- -field- ,item)
                (declare (ignorable -parent- -field-))
                ,@(append code (if rewrite (list item) nil))))
         (declare (dynamic-extent #',link-walker))
         (,(if rewrite 'rewrite-ast-links 'enum-ast-links)
           ,form #',link-walker ,@(remove-from-plist flags :rewrite))))))

(def (function e) map-ast (visitor form)
  (labels ((recurse (parent field form)
             (declare (ignore parent field))
             (aprog1 (funcall visitor form)
               ;; if the visitor returns a new AST node instead of the one
               ;; being given to it, then stop descending the tree and just
               ;; return the new one giving full control to the visitor over
               ;; what to do there.
               (when (eq it form)
                 (enum-ast-links form #'recurse)))))
    (declare (dynamic-extent #'recurse))
    (recurse nil nil form)))

(def (function e) collect-variable-references (top-form &key (type 'variable-reference-form))
  (let ((result (list)))
    (map-ast (lambda (form)
               (when (typep form type)
                 (push form result))
               form)
             top-form)
    result))

(def function clear-binding-usage-annotation (top-form)
  (map-ast (lambda (form)
             (when (typep form 'name-definition-form)
               (setf (usages-of form) nil))
             form)
           top-form))

(def generic mark-binding-usage (form)
  (:method-combination progn)
  (:method progn ((form t)))
  (:method progn ((form walked-lexical-variable-reference-form))
    (push form (usages-of (definition-of form))))
  (:method progn ((form walked-lexical-function-object-form))
    (push form (usages-of (definition-of form))))
  (:method progn ((form walked-lexical-application-form))
    (push form (usages-of (definition-of form))))
  (:method progn ((form return-from-form))
    (awhen (target-block-of form)
      (push form (usages-of it))))
  (:method progn ((form go-form))
    (awhen (tag-of form)
      (push form (usages-of it)))))

(def (function e) annotate-binding-usage (top-form)
  (clear-binding-usage-annotation top-form)
  (map-ast (lambda (form)
             (mark-binding-usage form)
             form)
           top-form))

(def (function e) copy-ast-form (form &rest initargs)
  "Duplicate an AST form instance"
  (let* ((class (class-of form))
         (copy (allocate-instance class)))
    (copy-ast-slots copy form)
    (apply #'reinitialize-instance copy initargs)))

(def (function e) rewrite-ast (top-form visitor &key parent parent-field)
  (labels ((rewrite-rec (parent field form)
             (multiple-value-bind (new-form continue)
                 (funcall visitor parent field form)
               (prog1 new-form
                 (when (or (eq form new-form) continue)
                   (rewrite-ast-links new-form #'rewrite-rec))))))
    (rewrite-rec parent parent-field top-form)))

(def (function e) deep-copy-ast (top-form &key replace-cb parent parent-field
                                          (lookup-table (make-hash-table :test #'eq))
                                          upper-table)
  "Copy a form tree with proper distinction between back and forward refs."
  (labels ( ;; Used to fix back references
           (lookup-cb (parent field form)
             (declare (ignore parent field))
             (or (gethash form lookup-table) form))
           ;; Walk the main links, copying everything
           (clone-rec (parent field form)
             (if (typep form 'walked-form)
                 (let ((user-value (if replace-cb
                                       (funcall replace-cb parent field form)
                                       form)))
                   (if (not (eq user-value form))
                       user-value
                       (or (gethash form lookup-table)
                           ;; Actually copy the node
                           (let ((new-form (copy-ast-form form)))
                             (setf (parent-of new-form) parent)
                             (setf (gethash form lookup-table) new-form)
                             (rewrite-ast-links new-form #'clone-rec)
                             new-form))))
                 form)))
    (prog1
        (clone-rec parent parent-field top-form)
      ;; Rewrite backrefs to copied forms
      (maphash (lambda (key new-form)
                 (declare (ignore key))
                 (when upper-table
                   (setf (gethash new-form upper-table) new-form))
                 (rewrite-ast-links new-form #'lookup-cb
                                    :include-main-refs nil
                                    :include-back-refs t))
               lookup-table))))

(def (function e) substitute-ast-if (new test tree &key in-place first-without-copy)
  (let ((must-copy? (not first-without-copy))
        (up-table (if in-place nil (make-hash-table :test #'eq))))
    (flet ((handle-node (parent field form)
             (declare (ignore field))
             (if (funcall test form)
                 (let ((new-form (if must-copy?
                                     (deep-copy-ast new :upper-table up-table)
                                     new)))
                   (setf (parent-of new-form) parent)
                   (setf must-copy? t)
                   new-form)
                 form)))
      (if in-place
          (rewrite-ast tree #'handle-node)
          (deep-copy-ast tree :replace-cb #'handle-node :lookup-table up-table)))))

