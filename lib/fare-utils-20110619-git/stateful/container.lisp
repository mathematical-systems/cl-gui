;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Generic heap interface

#+xcvb
(module
 (:depends-on
  ("package" "base/utils" "base/macros" "interface/order" "stateful/package")))

(in-package :stateful)

(exporting-definitions


;;;; Abstract Interface
;; Base class
(defclass container () ()
  (:documentation "base virtual class for stateful container objects"))
(defgeneric copy-container (container)
  (:documentation "duplicate a container into one with same contents"))

;; Emptiness, Size
(define-condition container-empty () ())
(defgeneric container-empty-p (container))
(defgeneric empty-container! (container))
(defun check-not-empty (container)
  (when (container-empty-p container)
    (error 'container-empty)))
(defgeneric container-size (container))

;; Containers and lists
(defgeneric container-add-list! (container list)
  (:method ((container container) list)
    (dolist (item list) (insert-item! container item))
    (values)))
(defun container-from-list! (class list &rest r)
  (apply #'make-instance class :initial-contents list r))
(defgeneric list-from-container! (container)
  (:method (container)
    (loop :until (container-empty-p container)
      :collect (pop-item! container))))
(defgeneric set-container-contents-from-list! (container list)
  (:method (container list)
           (empty-container! container)
           (container-add-list! container list)))
(defgeneric container-contents (container)
  (:documentation "extract a fresh list containing the contents of the container"))

;; Insertion, Merging
(defgeneric insert-item! (container item))
(defgeneric merge-containers! (subject object))

;; Finding, Deleting
(defgeneric find-least-item (container)
  (:method :before (container)
           (check-not-empty container)))
(defgeneric least-item (container)
  (:method :before (container)
           (check-not-empty container)))
(defgeneric pop-least-item! (container)
  (:method :before (container)
           (check-not-empty container)))
(defgeneric pop-item! (container)
  (:method :before (container)
           (check-not-empty container))
  (:method :before (container)
           (pop-least-item! container)))
(defgeneric pop-last-item! (container)
  (:method :before (container)
           (check-not-empty container)))
(defgeneric push-last-item! (container item))

(defgeneric delete-item! (container item))

(defgeneric node-item (container node))
(defgeneric (setf node-item) (item container node))

(defgeneric find-node (container item))
(defgeneric insert-node! (container item))
(defgeneric delete-node! (container item))
(defgeneric prune-node! (container item))

(defgeneric find-item (container item)
  (:method (container item)
           (node-item container (find-node container item))))



;;;; Generic container mixins

;;; Heaps
(defclass heap (order:<order> container)
  ())

;;; containers with nodes
(defclass node-container-mixin ()
  ((node-class :initarg :node-class
               :accessor container-node-class
               :allocation :class)))

(defclass node-mixin ()
  ((content :initarg :content :accessor node-content)))

(defgeneric make-node (container &rest args)
  (:method ((container node-container-mixin) &rest args)
           (apply #'make-instance (container-node-class container) args)))

(defmethod node-item ((container node-container-mixin) node)
  (node-content node))
(defmethod (setf node-item) (item (container node-container-mixin) node)
  (setf (node-content node) item))

(defgeneric exchange-nodes (container n1 n2)
  (:method ((container node-container-mixin) n1 n2)
           (declare (ignorable container))
           (rotatef (node-content n1) (node-content n2))))

(defmethod initialize-instance :after ((container node-container-mixin) &key initial-contents &allow-other-keys)
  (when initial-contents
    (container-add-list! container initial-contents)))


;;; containers having an easily computed size
(defclass sized-container-mixin ()
  ())

(defmethod container-empty-p ((container sized-container-mixin))
  (zerop (container-size container)))


;;; containers caching their actual size in a slot
(defclass cached-size-mixin (sized-container-mixin)
  ((size :initarg :size :initform 0
         :accessor container-size)))

(defmethod empty-container! :after ((container cached-size-mixin))
  (setf (container-size container) 0))

#|(defmethod insert-item! :after ((container cached-size-mixin) item)
  (incf (container-size container)))
(defmethod delete-item! :after ((container cached-size-mixin) item)
  (decf (container-size container)))
(defmethod merge-containers! :after ((c1 cached-size-mixin) c2)
  (incf (container-size c1) (container-size c2)))|#


;;; containers using a simple vector as storage for their elements
(defclass vector-container-mixin (container cached-size-mixin)
  ((vector :initarg :vector
           :initform #()
           :accessor container-vector)))

(defmethod initialize-instance :after ((container vector-container-mixin)
                                       &key initial-size initial-contents &allow-other-keys)
  (when (or initial-size initial-contents)
    (let* ((contents-size (length initial-contents))
           (size (max (or initial-size 0) contents-size)))
      (setf (container-vector container) (make-array size)
            (container-size container) contents-size)
      (replace (container-vector container) initial-contents))))


(defun-inline vector-container-ref (container index)
  (svref (container-vector container) index))
(defun-inline (setf vector-container-ref) (new-value container index)
  (setf (svref (container-vector container) index) new-value))

(defmethod empty-container! :after ((container vector-container-mixin))
  (fill (container-vector container) nil))

(defgeneric adjust-size (container newsize)
  (:method ((container vector-container-mixin) newsize)
           (let ((oldvec (container-vector container))
                 (newvec (make-array newsize :initial-element nil)))
             (replace newvec oldvec :end1 (container-size container))
             (setf (container-vector container) newvec)
             container)))

(defgeneric maybe-adjust-size-down (container)
  (:method ((container vector-container-mixin))
           (let ((buffer-size (length (container-vector container)))
                 (contents-size (container-size container)))
             (when (< (* 4 contents-size) buffer-size)
               (adjust-size container (* 2 contents-size))))))

(defgeneric maybe-adjust-size-up (container needed-size)
  (:method ((container vector-container-mixin) needed-size)
           (let ((buffer-size (length (container-vector container)))
                 (contents-size (container-size container)))
             (unless (<= needed-size buffer-size)
               (adjust-size container (max needed-size (* 2 contents-size)))))))

(defmethod pop-last-item! ((container vector-container-mixin))
  (let* ((index (decf (container-size container)))
         (item (svref (container-vector container) index)))
    (setf (svref (container-vector container) index) nil)
    (maybe-adjust-size-down container)
    item))

(defmethod push-last-item! ((container vector-container-mixin) item)
  (let* ((oldsize (container-size container))
         (newsize (1+ oldsize)))
    (maybe-adjust-size-up container newsize)
    (setf (container-size container) newsize
          (vector-container-ref container oldsize) item)
    container))

)

#|
What I'd like to have, that CLOS won't provide, is some more declarative
metaprogramming facility, where you get a static optimized combination of
containers based on a specification of entities and their interrelation,
where entities include containers and contained, indices and abstract pointers,
and operations and invariants supposed as input or provided as output.
Can you change an item based on this type of pointer, and delete it, but
not insert a new node next? How do we merge existing entries with same key?
Is the container a pure datastructure or is there state? What pure abstract
view is the state local to (i.e. when we reify "the world" for
debugging/migration/specification/evolution/refactoring or whatever purpose?

And so if I want fast insert/remove from a set and adding to another one,
then doubly linked lists make a lot of sense. And if I *also* want 

|#
