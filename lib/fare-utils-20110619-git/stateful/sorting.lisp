;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;; Sorting Algorithms.

#+xcvb
(module
 (:depends-on ("package" "interface/eq" "interface/order" "stateful/package")))

(in-package :stateful)

(defun stable-bucket-sort/ordinals (size vector)
  ;; in: a vector mapping its indices to numbers below size
  ;; out: a sorted list of lists of numbers having the same mapped value
  (loop
    :with buckets = (make-array size :initial-element nil)
    :for x :from (1- (length vector)) :downto 0
    :for y = (aref vector x) :do
    (push x (svref buckets y))
    :finally (return (loop :for b :across buckets :when b :collect b))))


(defun adjacency-table/ordinals (size arcs)
  ;; in: the number of nodes in the graph, as numbered from 0,
  ;;     a list of (origin . destination) pairs for arcs of the graph
  ;; out: an array giving for each node the list of destinations for that origin
  (loop
      :with table = (make-array size :initial-element nil)
      :for arc :in arcs
      :do (push (cdr arc) (aref table (car arc)))
      :finally (return table)))

(defun transpose-cons (x)
  (cons (cdr x) (car x)))

(defun reverse-adjacency-table/ordinals (size arcs)
  (adjacency-table/ordinals size (mapcar #'transpose-cons arcs)))


(defun stable-strongly-connected-components/ordinals (size arcs)
  ;; input: the number of nodes in the graph, as numbered from 0,
  ;;        a list of (origin . destination) pairs of numbers describing the (directed) graph
  ;; output: a sorted list of the strongly connected components, each a sorted list of numbers
  ;;         a vector mapping numbers to the smallest element in the component they belong to
  ;; Using Kosaraju's algorithm
  (declare (optimize (speed 0) (safety 3) (debug 3)))
  (let* ((color (make-array size :initial-element nil)) ;; nil for white, t for grey, end time for black
         (finishers (make-array size :initial-element nil))
         (current-finisher size)
         (adjacency-table (adjacency-table/ordinals size arcs))
         (reverse-adjacency-table (reverse-adjacency-table/ordinals size arcs))
         (buckets (make-array size :initial-element nil))
         (least (make-array size :initial-element nil))
         (current-least nil))
    (labels ((dfs1 (node)
               (setf (aref color node) t)
               (loop
                 :for next :in (aref adjacency-table node)
                 :unless (aref color next)
                 :do (dfs1 next))
               (setf (aref finishers (decf current-finisher)) node))
             (dfs2 (node)
               (setf current-least node)
               (dfs2-1 node node)
               (setf (aref least node) current-least))
             (dfs2-1 (node forefather)
               (setf (aref color node) nil)
               (loop
                 :for next :in (aref reverse-adjacency-table node)
                 :when (aref color next) :do
                 (progn
                   (when (< node current-least) (setf current-least node))
                   (dfs2-1 next forefather)))
               (setf (aref buckets node) forefather)))
      (loop
        :for i :below size
        :unless (aref color i) :do ;; When the node is white
        (dfs1 i))
      (loop
        :for i :below size
        :for j = (aref finishers i)
        :when (aref color j) :do ;; When the node is black
        (dfs2 j))
      (loop
        :for i :below size :do
        (setf (aref buckets i) (aref least (aref buckets i))))
      (values (stable-bucket-sort/ordinals size buckets)
              buckets))))

#|
(stable-strongly-connected-components/ordinals
 10
 '((0 . 2) (0 . 3) (0 . 1) (7 . 2) (2 . 4) (8 . 7) (4 . 8) (4 . 9) (1 . 9) (1 . 3) (3 . 6) (6 . 1)))


((0) (1 3 6) (2 4 7 8) (5) (9))
#(0 1 2 1 2 5 1 2 2 9)


|#

#|
(stable-topological-sort/ordinals
 6
 '((4 . 2) (4 . 1) (1 . 3) (2 . 3)))
|#

(defun stable-topological-sort/ordinals (size dependencies)
  (let* ((minimals (make-instance 'binary-heap :order order:<number>))
         (adjacency-table (adjacency-table/ordinals size dependencies))
         (dependency-count (make-array size :initial-element 0))
         (result nil))
    (loop
        :for dep :in dependencies :do
        (incf (aref dependency-count (cdr dep))))
    (loop
      :for i :below size
      :when (zerop (aref dependency-count i)) :do
      (insert-item! minimals i))
    (loop :repeat size :do
      (when (container-empty-p minimals)
        (error "Cyclic dependencies"))
      (let ((min (pop-least-item! minimals)))
        (push min result)
        (loop
          :for dep :in (aref adjacency-table min)
          :for count = (decf (aref dependency-count dep))
          :when (zerop count) :do
          (insert-item! minimals dep))))
    (nreverse result)))

(defun map-graph-to-ordinals (sequence arcs &key (test 'equal))
    (let* ((elements (coerce sequence 'vector))
           (size (length elements))
           (hash (make-hash-table :test test)))
      (loop :for x :across elements :for i :from 0 :do
            (setf (gethash x hash) i))
      (values
       size
       (loop :for (x . y) :in arcs
             :for xi = (gethash x hash)
             :for yi = (gethash y hash)
             :collect (cons xi yi))
       elements)))

(defun stable-topological-sort (sequence dependencies &key (test 'equal))
  (multiple-value-bind (size arcs elements) (map-graph-to-ordinals sequence dependencies :test test)
    (mapcar #'(lambda (x) (svref elements x)) (stable-topological-sort/ordinals size arcs))))

#|
(defun stable-cyclic-topological-sort (sequence dependencies &key (test 'equal))
  (labels
      ((heap-order (h1 h2)
         (order:<key> :order order:<number> :key 'find-least-item)))
    (let* ((elements (coerce sequence 'vector))
           (length (length elements))
           (hash (make-hash-table :test test))
           (equivs (make-array length :initial-element nil))
           (outgoing (make-array length :initial-element nil))
           (incoming (make-array length :initial-element nil))
           (minimals (make-instance 'binary-heap :order order))
           (nonmins (make-instance 'binary-heap :order order))
           (results nil))
      (loop :for x :across elements :for i :from 0 :do
            (setf (gethash x hash) i)
            (setf (aref equivs i) (singleton-binomial-heap i :order #'order:<number>))
            (setf (aref incoming i) (make-instance 'binomial-heap :order #'order:<number>))
            (setf (aref outgoing i) (make-instance 'binomial-heap :order #'order:<number>)))
      (loop :for (x . y) :in dependencies
            :for xi = (gethash x hash)
            :for yi = (gethash y hash) do
            (insert-item! (aref incoming xi) yi)
            (insert-item! (aref incoming yi) xi))
      (loop :for i :from 0 :for o :across outgoing :do
            (if (container-empty-p o)
                (insert-item! minimals i)
                (insert-item! nonmins i)))
      (loop
        (cond
          ((not (container-empty-p minimals))
           (let* ((min (pop-least-item! minimals))
                  (equivs (list-from-container! (aref equivs min))))
             (loop :for x :in equivs :do
               (loop :for y :in
                 (delete-item!
                  (push equivs results)))
               ((not (container-empty-p nonmins))
             ;; do some cycles detection
  ...)
|#
