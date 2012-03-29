;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;; Minimum binary heaps
;;;
;;; (impure) binary-tree satisfying
;;; 1- the shape property whereby the tree is a finite subset of an infinite binary
;;;  tree as filled in a depth-first left-to-right traversal.
;;; 2- the heap property whereby each node is smaller than or equal to each of
;;;  its children according to the order predicate
;;;  i.e. 1 is the smallest positive integer for <
;;;
;;; DONE: the basic functionality is working and has been tested
;;; TODO:
;;; * document it
;;; * write a better function that gives you node handles for editing/removal purposes

#+xcvb
(module
 (:depends-on
  ("package"
   "base/utils"
   "base/macros"
   "interface/order"
   "stateful/package"
   "stateful/container")))

(in-package :stateful)

(def*class binary-heap (heap order:<order-parameter>
                        vector-container-mixin sized-container-mixin)
  ())

;; In this implementation, we use the standard trick of representing heap contents
;; as a 1-based vectors, where the bits down from the highest weight to the lowest-weight
;; describe the path (0 for left, 1 for right) that leads from the root of the tree
;; to the current node. (With negative indices, we could use 0 as root, and negative
;; for right vs positive for left.)
;; CL vector storage is always 0-based so we adjust appropriately.
(defun-inline bh-index-left (i)
   (1+ (ash i 1)))
(defun-inline bh-index-right (i)
   (ash i 1))
(defun-inline bh-index-parent (i)
   (ash i -1))
(defun-inline bhref (heap index)
  (vector-container-ref heap (1- index)))
(defun-inline (setf bhref) (new-value heap index)
  (setf (vector-container-ref heap (1- index)) new-value))

(defmethod node-item ((heap binary-heap) node)
  (bhref heap node))


(defun-inline binary-heap-exchange-nodes (heap i1 i2)
  (rotatef (bhref heap i1) (bhref heap i2))
  nil)

(defmethod exchange-nodes ((heap binary-heap) i1 i2)
  (binary-heap-exchange-nodes heap i1 i2))

(defmethod copy-container ((heap binary-heap))
  (make-instance 'binary-heap
    :size (container-size heap)
    :vector (copy-array (container-vector heap))
    :order (order:order-interface heap)))

(defun binary-heap-sift-down (heap index &aux (min index))
  ;; the heap invariant is respected on each subtree of the specified node
  ;; but was disturbed at said node -- restore the invariant for that subtree.
  ;; Time O(ln n).
  (flet ((consider (x)
           (when (and (<= x (container-size heap))
                      (order< heap (bhref heap x) (bhref heap min)))
             (setf min x))))
           (consider (bh-index-left index))
           (consider (bh-index-right index))
           (if (= min index)
               index
               (progn
                 (binary-heap-exchange-nodes heap min index)
                 (binary-heap-sift-down heap min)))))

(defun binary-heap-enforce-invariant (heap)
  ;; starting from arbitrary contents, enforce the heap invariant,
  ;; starting at the next-to-deepest level upward. Time O(n ln n)
  (loop :for i :from (ash (container-size heap) -1) :downto 1 :do
        (binary-heap-sift-down heap i)))

(defmethod initialize-instance :after ((heap binary-heap) &key)
  (binary-heap-enforce-invariant heap))

(defun binary-heap-sift-up (heap index)
  ;; the heap invariant is respected on the tree above specified leaf.
  ;; the leaf was just inserted or modified. -- restore the invariant by moving things up
  ;; Time O(ln n).
  (when (< 1 index)
    (let ((parent-index (bh-index-parent index)))
      (when (order< heap (bhref heap index) (bhref heap parent-index))
        (binary-heap-exchange-nodes heap index parent-index)
        (binary-heap-sift-up heap parent-index)))))

(defmethod least-item ((heap binary-heap))
  (bhref heap 1))

(defmethod pop-least-item! ((heap binary-heap))
  (prog1
      (least-item heap)
    (delete-node! heap 1)))

(defmethod delete-node! ((heap binary-heap) index)
  (let* ((last-index (container-size heap))
         (last-item (bhref heap last-index)))
    (pop-last-item! heap)
    (unless (= index last-index)
      (setf (node-item heap index) last-item)))
  (values))

(defmethod (setf node-item) (item (heap binary-heap) index)
  (let ((old-key (bhref heap index)))
    (setf (bhref heap index) item)
    (if (order< heap old-key item)
        (binary-heap-sift-down heap index)
        (binary-heap-sift-up heap index)))
    item)

(defmethod insert-item! ((heap binary-heap) item)
  (push-last-item! heap item)
  (binary-heap-sift-up heap (container-size heap)))

(defmethod print-object ((heap binary-heap) stream)
  (print-unreadable-object (heap stream :type t :identity t)
    (format stream ":SIZE ~A (buffer ~A) :CONTENTS ~A"
            (container-size heap)
            (length (container-vector heap))
            (container-contents heap))))

(defmethod container-contents ((heap binary-heap))
  (coerce (subseq (container-vector heap) 0 (container-size heap)) 'list))

(defmethod empty-container! ((heap binary-heap))
  ())

#|
Something with nodes, such that you can easily remove nodes
(def*class noded-binary-heap (heap vector-container-mixin sized-container-mixin)
  ...)
|#
