;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

;;;;;;
;;; Sequence utilities

(def (function ioe) the-only-element (elements)
  (assert (length= 1 elements))
  (elt elements 0))

(def compiler-macro the-only-element (&whole whole elements)
  (once-only (elements)
    `(progn
       (assert (length= 1 ,elements) () "~S failed on form ~S" 'the-only-element ',whole)
       (elt ,elements 0))))

(def (function ioe) the-non-nil (value)
  (assert value)
  value)

(def compiler-macro the-non-nil (&whole whole value)
  (once-only (value)
    `(progn
       (assert ,value () "~S failed on form ~S" 'the-non-nil ',whole)
       ,value)))

(def (function e) ensure-sequence (thing)
  (if (typep thing 'sequence)
      thing
      (list thing)))

(def (function e) subseq-if-longer (limit sequence &key postfix)
  (if (> (length sequence) limit)
      (bind ((cut (subseq sequence 0 limit)))
        (values (if postfix
                    (concatenate 'string cut postfix)
                    cut)
                #t))
      (values sequence #f)))

(def (function e) all-the-same? (sequence &key (test 'eql) key)
  "Returns true if all elements in the LIST are equal using TEST."
  (check-type sequence sequence)
  (ensure-functionf test)
  (iter (with first-element)
        (for element :in-sequence sequence)
        (when key
          (setf element (funcall key element)))
        (when (first-time-p)
          (setf first-element element)
          (next-iteration))
        (unless (or (first-time-p)
                    (funcall test first-element element))
          (return #f))
        (finally (return #t))))

(def (function e) collect (element sequence &key (key #'identity) (test #'eq))
  "Collects elements equal to ELEMENT from SEQUENCE into a freshly allocated sequence."
  (remove element sequence :key key :test-not test))

(def (function e) collect-if (predicate sequence &key (key #'identity))
  "Collects elements from SEQUENCE for which the PREDICATE is true into a freshly allocated sequence."
  (remove-if-not predicate sequence :key key))

(def (function e) optional-list (&rest elements)
  (remove nil elements))

(def (function e) optional-list* (&rest elements)
  (remove nil (apply #'list* elements)))

(def (macro e) foreach (function first-sequence &rest more-sequences)
  `(map nil ,function ,first-sequence ,@more-sequences))

(def (function e) partition (sequence &rest predicates)
  (iter (with result = (make-array (length predicates) :initial-element nil))
        (for element :in-sequence sequence)
        (iter (for predicate :in predicates)
              (for index :from 0)
              (when (funcall predicate element)
                (push element (aref result index))
                (finish)))
        (finally
         (return
           (iter (for element :in-vector result)
                 (collect (nreverse element)))))))

(def (function e) split-sequence-by-partitioning (sequence &rest predicates)
  (iter outer
        (with length = (length sequence))
        (with position = 0)
        (while (< position length))
        (for element = (elt sequence position))
        (for part = (iter inner
                          (for index :from 0)
                          (for predicate = (elt predicates index))
                          (when (funcall predicate element)
                            (return-from inner (subseq sequence position
                                                       (position-if (lambda (element)
                                                                      (or (iter (for preceding-predicate-index :from 0 :below index)
                                                                                (thereis (funcall (elt predicates preceding-predicate-index) element)))
                                                                          (not (funcall predicate element))))
                                                                    sequence :start position))))))
        (if part
            (progn
              (collect part)
              (incf position (length part)))
            (incf position))))

(def (function e) substitute-all (old-elements new-element sequence &key (test #'eql) (start 0) count end key from-end)
  (bind ((sequence (copy-seq sequence)))
    (iter (for old-element :in-sequence old-elements)
          (nsubstitute new-element old-element sequence :test test :key key :start start :count count :end end :from-end from-end))
    sequence))

;;;;;;
;;; tree

(def (function e) find/tree (item sequence &key (key 'identity) (test 'eql))
  (ensure-functionf key test)
  (block searching
    (labels
        ((recurse (sequence)
           (loop
             :for el :in sequence :do
             (cond
               ((funcall test item (funcall key el))
                (return-from searching el))
               ((consp el)
                (recurse el))))))
      (recurse sequence)
      nil)))

(def (function e) substitute/tree (new old list &key from-end (test #'eql) (test-not nil) (end nil) (count nil) (key nil) (start 0))
  "Starting from LIST non-destructively replaces OLD with NEW."
  (if (consp list)
      (bind ((result (iter (for newitem in (ensure-list new))
                           (for olditem in (ensure-list old))
                           (setf list (substitute newitem olditem list :from-end from-end :test test :test-not test-not
                                                  :end end :count count :key key :start start))
                           (finally (return list)))))
        (iter (for node first result then (cdr node))
              (until (null node))
              (for el = (car node))
              (setf (car node) (substitute/tree new old el :from-end from-end :test test :test-not test-not
                                                :end end :count count :key key :start start)))
        result)
      (if (funcall test list old)
          new
          list)))

(def (function eio) shrink-vector (vector size)
  "Fast shrinking for simple vectors. It's not thread-safe, use only on local vectors!"
  #*((:sbcl      (sb-kernel:%shrink-vector vector size))
     (:allegro   (excl::.primcall 'sys::shrink-svector vector size))
     (:cmu       (lisp::shrink-vector vector size))
     (:lispworks (system::shrink-vector$vector vector size))
     (:scl       (common-lisp::shrink-vector vector size))
     (t          (subseq vector 0 size))))
