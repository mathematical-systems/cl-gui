;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; doubly-linked lists
#+xcvb
(module (:depends-on ("package" "stateful/package" "stateful/container")))

#|
doubly-linked-mixin and doubly-linked list.

THIS CODE IS VERY LIGHTLY TESTED, AND PROBABLY NEEDS MORE WORK.

* a list is any contents between two list headers, so we can do things like the treadmill GC trick

* a header node has no contents, just identity onto which to attach further nodes.

IMPLEMENTED:

TODO:

|#

(in-package :stateful)

(defclass doubly-linked-mixin ()
  ((previous :accessor doubly-linked-previous)
   (next :accessor doubly-linked-next)))

(defgeneric isolate-node! (node))

(defmethod isolate-node! ((node doubly-linked-mixin))
  (setf (doubly-linked-next node) node
        (doubly-linked-previous node) node)
  node)

(defmethod initialize-instance ((node doubly-linked-mixin) &key)
  (isolate-node! node)
  (call-next-method))

(defmethod empty-container! ((container doubly-linked-mixin))
  (isolate-node! container)
  container)

(defgeneric unlink-node! (node))

(defmethod unlink-node! ((node doubly-linked-mixin))
  (let ((previous (doubly-linked-previous node))
        (next (doubly-linked-next node)))
    (setf (doubly-linked-next previous) next
          (doubly-linked-previous next) previous)
    (isolate-node! node)
    nil))

(defmethod delete-node! ((container doubly-linked-mixin) node)
  (declare (ignorable container))
  (unlink-node! node)
  (values))

(defun join-dl-chains (first1 last1 first2 last2)
  (setf (doubly-linked-next last1) first2
        (doubly-linked-previous first1) last2
        (doubly-linked-next last2) first1
        (doubly-linked-previous first2) last1)
  nil)

;;;; a container based on doubly-linked objects

(defclass doubly-linked-list (doubly-linked-mixin container node-container-mixin) ;...
  ((node-class :initform 'doubly-linked-list-node)))

(defclass doubly-linked-list-node (doubly-linked-mixin node-container-mixin node-mixin) ;...
  ((node-class :initform 'doubly-linked-list)))

(defmethod container-empty-p ((container doubly-linked-mixin))
  (not (typep (doubly-linked-next container) 'node-mixin)))

(defmethod empty-container! ((container doubly-linked-list))
  (isolate-node! container)
  (call-next-method))

(defmethod insert-item! ((container doubly-linked-list) item)
  ;; insert item as the next
  (with-slots (next) container
    (let ((new (make-instance 'doubly-linked-list-node :content item)))
      (join-dl-chains new new (doubly-linked-next container) container)
      new)))

(defmethod pop-item! ((container doubly-linked-list))
  (let* ((next (doubly-linked-next container)))
    (unlink-node! next)
    (node-content next)))

(defmethod container-contents ((container doubly-linked-list))
  (loop :for node = container :then next
    :for next = (doubly-linked-next node)
    :until (eq next container)
    :collect (node-content next)))
