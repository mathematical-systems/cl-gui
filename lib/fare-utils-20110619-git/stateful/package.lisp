;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Interfaces for Stateful Data-Structures

#+xcvb
(module
 (:depends-on
  ("package" "interface/interface" "interface/eq" "interface/order")))

(in-package :cl)

(defpackage :stateful
  (:use :cl :fare-utils :interface :eq :order)
  (:export
   #:container #:container-add-list!
   #:container-contents #:container-empty #:container-empty-p
   #:container-from-list! #:container-size
   #:container-empty-p
   #:copy-container
   #:empty-container!
   #:check-not-empty
   #:list-from-container!
   #:set-container-contents-from-list!
   #:container-contents
   #:insert-item!
   #:merge-containers!
   #:find-least-item
   #:least-item
   #:pop-least-item!
   #:pop-item!
   #:pop-last-item!
   #:push-last-item!
   #:delete-item!
   #:node-item
   #:find-node
   #:insert-node!
   #:delete-node!
   #:prune-node!
   #:find-item
   #:heap
   #:node-container-mixin
   #:node-mixin
   #:make-node
   #:exchange-nodes
   #:initialize-instance
   #:sized-container-mixin
   #:container-empty-p
   #:cached-size-mixin
   #:vector-container-mixin
   #:vector-container-ref
   #:adjust-size
   #:maybe-adjust-size-down
   #:maybe-adjust-size-up
   #:binary-heap
   #:binomial-heap
   #:fifo
   #:make-fifo
   #:fifo-head
   #:fifo-tail
   #:fifo-empty-p
   #:fifo-enqueue
   #:fifo-dequeue
   #:fifo-nconc2
   #:fifo-dequeue-object
   #:fifo-empty!
   ))
