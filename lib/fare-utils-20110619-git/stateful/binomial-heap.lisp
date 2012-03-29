;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;; Minimum binomial heaps
;;; 
;;; * a minimum binomial heap is a list of minimum binomial trees
;;;  sorted by size
;;; * a minimum binomial trees is a binomial tree that satisfies the heap property whereby
;;;  each node is smaller than or equal to each of its children according to the order predicate
;;;  i.e. 1 is the smallest positive integer for <
;;; * a binomial tree of rank n has a node with n children of ranks 0 to n-1 sorted by size,
;;;  with a total number of nodes 2**n

#+xcvb
(module
 (:depends-on
  ("package"
   "base/utils"
   "interface/order"
   "stateful/package"
   "stateful/container")))

(in-package :stateful)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defclass binomial-tree (node-mixin)
  ((children :type list :accessor node-children :initform () :initarg :children))))

(def*class binomial-heap (heap node-container-mixin cached-size-mixin)
  ((trees :type simple-vector :accessor heap-trees :initform #() :initarg :trees)
   (node-class :initform (find-class 'binomial-tree) :allocation :class)))

(defgeneric tree-content (container))

(defmethod tree-content ((tree binomial-tree))
  (list* (node-content tree) (mapcar #'tree-content (node-children tree))))

(defmethod print-object ((tree binomial-tree) stream)
  (print-unreadable-object (tree stream :type t :identity t)
    (format stream "~A" (tree-content tree))))

(defmethod print-object ((heap binomial-heap) stream)
  (print-unreadable-object (heap stream :type t :identity t)
    (format stream ":size ~A :children ~A"
            (container-size heap) (heap-trees heap))))



(defmethod empty-container! ((heap binomial-heap))
  (setf (heap-trees heap) #()))

(defun singleton-binomial-heap (item &rest r)
  (apply #'make-instance 'binomial-heap
         :size 1
         :trees (vector (make-instance 'binomial-tree :content item))
         r))

(defmethod merge-containers! ((h1 binomial-heap) (h2 binomial-heap))
  (loop :with t1 = (heap-trees h1)
    :with t2 = (heap-trees h2)
    :with l1 = (length t1)
    :with l2 = (length t2)
    :with sr = (+ (container-size h1) (container-size h2))
    :with lr = (integer-length sr)
    :with tr = (make-array lr :initial-element nil)
    :with carry = nil
    :with s = nil
    :for i :below lr
    :for x1 = (and (< i l1) (aref t1 i))
    :for x2 = (and (< i l2) (aref t2 i)) :do
    (if x1
        (if x2
            (setf s carry
                  carry (merge-binomial-trees! h1 x1 x2))
            (if carry
                (setf s nil
                      carry (merge-binomial-trees! h1 x1 carry))
                (setf s x1 carry nil)))
        (if x2
            (if carry
                (setf s nil
                      carry (merge-binomial-trees! h1 x2 carry))
                (setf s x2
                      carry nil))
            (setf s carry
                  carry nil)))
    (setf (svref tr i) s)
    :finally (setf (heap-trees h1) tr
                   (container-size h1) sr))
  h1)

(defgeneric merge-binomial-trees! (h t1 t2))

(defmethod merge-binomial-trees! ((h binomial-heap) (t1 binomial-tree) (t2 binomial-tree))
  (multiple-value-bind (ta tb)
      (if (order< h (node-content t1) (node-content t2))
          (values t1 t2)
          (values t2 t1))
    (push tb (node-children ta))
    ta))

(defmethod insert-item! ((heap binomial-heap) item)
  (merge-containers! heap (singleton-binomial-heap item)))

(defmethod find-least-item ((heap binomial-heap))
  (loop :with min = nil
    :with minp = nil
    :for tree :across (heap-trees heap)
    :for i :from 0 :do
    (when tree
      (let ((x (node-content tree)))
        (unless (and minp (order< heap min x))
          (setf min x minp i))))
    :finally (return (values min minp))))

(defmethod least-item ((heap binomial-heap))
  (nth-value 0 (find-least-item heap)))

(defmethod pop-least-item! ((heap binomial-heap))
  (multiple-value-bind (min minp)
      (find-least-item heap)
    (let* ((l (ash 1 minp))
           (tree (svref (heap-trees heap) minp)))
      (setf (svref (heap-trees heap) minp) nil)
      (decf (container-size heap) l)
      (unless (zerop minp)
        (merge-containers! heap
                           (make-instance 'binomial-heap
                             :size (1- l)
                             :trees (make-array minp :initial-contents
                                                (nreverse (node-children tree)))))))
    min))

#|
(defmethod delete-node! ((heap binomial-heap) index)
  ;; recursively remove subtrees, merging back a heap with all that didn't actually need be removed
  (NIY))
|#
