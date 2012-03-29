;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Miscellaneous small utility definitions by Fare (formerly fare.lisp)

#+xcvb
(module (:depends-on ("package" "base/strings" "base/symbols" "base/macros")))

(in-package :fare-utils)

;; Pairs to alist
(defun plist->alist (plist)
  "transform a list of consecutive pairs into an alist
   PLEASE instead use ALEXANDRIA:PLIST-ALIST"
  (assert (and (listp plist) (evenp (list-length plist))))
  (loop :for (key val) :on plist by #'cddr :collect (cons key val)))
#| (if (null plist) nil
     (acons (car plist) (cadr plist) (plist->alist (cddr plist)))) |#

(defun alist->plist (alist)
  (loop :for (key . val) :in alist :nconc (list key val)))

(defun sort-keys (keys)
  (alist->plist (sort (plist->alist keys) #'string< :key #'car)))

(defun rlist* (x &rest r)
  "reverse of LIST* of reverse of arguments"
  (append x r))
(defun rcons (x y)
  "construct a new list with Y at end of list X"
  (append x (list y)))
(defun firstn (n l)
  "N first elements of list L -- complement of (NTHCDR N L)"
  (subseq l 0 n))

(defun append/list (list)
  (reduce #'append list :from-end t))
(defun mappend (&rest rest)
  "like mapcan, but works on lists resulting from quasi-quoted expressions"
  (append/list (apply #'mapcar rest)))

(defun mapcar2 (fun list)
  "simple mapcar for functions returning 2 values,
returning the two lists of the values returned by the function."
  (if (null list) (values nil nil)
    (mvbind (x y) (funcall fun (car list))
       (mvbind (xl yl) (mapcar2 fun (cdr list))
	  (values (cons x xl) (cons y yl))))))


(defun integers-below (n)
  "list of integers from 0 to n-1, or from n to m"
  (loop :for i fixnum :below n :collect i))
(defun integers-between (n m)
  "list of integers from n to m (included)"
  (loop :for i :from (ceiling n) :to m :collect i))
(defun list-of-integers (n &optional m)
  (if m (integers-between n m) (integers-below n)))

(defun copy-list-without-nth (n list)
  "remove Nth element from LIST, yielding an entirely new list"
  (loop :for x :in list :for i fixnum :from 0 :unless (= i n) :collect x))

; same, without any extraneous consing (but with lots of mutation)
(defun remove-nth (n list)
  "remove Nth element from LIST, keeping the tail and yielding a new head"
  (loop
    :with head = (cons t nil)
    :with link = head
    :for tail :on list
    :for elt :in list
    :count n :do
    (let ((cons (cons elt nil)))
      (rplacd link cons)
      (setf link cons))
    :finally
    (progn (rplacd link (cdr tail))
	   (return (cdr head)))))
(TEST-FORM (remove-nth 3 '(a b c 3 d e f)) '(a b c d e f))

(defsubst ndolist-with-rest-fun (list fun)
  (when list
    (let ((elem (car list))
	  (rest (cdr list)))
      (tagbody
       loop
       (funcall fun elem rest)
       (setf list (cdr list))
       (when (null list) (go exit))
       (rotatef elem (car list))
       (go loop)
       exit))))
(defsubst dolist-with-rest-fun (list fun)
  (ndolist-with-rest-fun (copy-seq list) fun))
(defmacro ndolist-with-rest (formals list &body body)
  `(ndolist-with-rest-fun ,list #'(lambda ,formals ,@body)))
(defmacro dolist-with-rest (formals list &body body)
  `(dolist-with-rest-fun ,list #'(lambda ,formals ,@body)))

(defun extremum (sequence predicate &key (key #'identity))
  (reduce #'(lambda (x y)
	      (if (funcall predicate (funcall key x) (funcall key y)) x y))
          sequence))

(defun length=-p (x y)
  ;(= (length x) (length y))
  (loop
    (let ((nx (atom x)) (ny (atom y)))
      (if nx (return ny)
	(if ny (return nil)
	  (setf x (cdr x) y (cdr y)))))))

(defun length=n-p (x n)
  ;(= (length x) n)
  (check-type n (integer 0 *))
  (loop
    :for l = x :then (cdr l)
    :for i :downfrom n :do
    (cond
      ((zerop i) (return (null l)))
      ((not (consp l)) (return nil)))))

(defun length<-p (x y)
  ;(= (length x) (length y))
  (loop
    (if (atom y) (return nil)
      (if (atom x) (return t)
	(setf x (cdr x) y (cdr y))))))
(defun length<n-p (x n)
  ;(< (length x) n)
  (loop
    (if (zerop n) (return nil)
      (if (atom x) (return t)
	(setf x (cdr x) n (1- n))))))
(defun length<=-p (x y)
  (not (length<-p y x)))
(defun length<=n-p (x n)
  (length<n-p x (1+ n)))
(defun length>-p (x y)
  (length<-p y x))
(defun length>n-p (x n)
  (not (length<=n-p x n)))
(defun length>=-p (x y)
  (length<=n-p y x))
(defun length>=n-p (x n)
  (not (length<n-p x n)))

;;; CONS-trees
(defun cons-tree-map (fun x)
  (cond
   ((null x)
    nil)
   ((consp x)
    (cons (cons-tree-map fun (car x))
	  (cons-tree-map fun (cdr x))))
   (t
    (funcall fun x))))

;;; a-list

(defmacro make-keys (&rest keys)
  "expand to code that creates a list suitable to pass to a function with &KEY argument,
from a specification for each argument of a ARG, (ARG), (ARG KEYWORD) or (ARG KEYWORD SUPPLIED-P)
where KEYWORD defaults to :ARG and SUPPLIED-P defaults to ARG."
  (if (null keys) nil
    (let* ((ks (first keys))
	   (key (if (consp ks) (if (consp (car ks)) (cadar ks) (car ks)) ks))
	   (kw (if (and (consp ks) (consp (car ks))) (caar ks)
		 (conc-keyword key)))
	   (suppliedp (if (and (listp ks) (<= 2 (length ks))) (second ks)
			(conc-symbol key :-supplied-p)))
	   (if-supplied (if (and (listp ks) (<= 3 (length ks)))
			    (third ks)
			  key)))
      (with-gensyms (foo)
      `(let ((,foo (make-keys ,@(rest keys))))
	 (if ,suppliedp (cons ,kw (cons ,if-supplied ,foo)) ,foo))))))

(defun default-behavior (&optional if-not-found default (error-msg "bad"))
  (cond
   ((null if-not-found) (values default nil))
   ((functionp if-not-found) (funcall if-not-found))
   ((eq if-not-found :error) (error error-msg))
   (t (error "default action not recognized"))))

(defun association (x alist &key if-not-found default
		      (key nil ksp) (test nil tsp) (test-not nil tnsp))
  (let* ((foo (apply #'assoc x alist
		     (make-keys (key ksp) (test tsp) (test-not tnsp)))))
    (if foo (values (cdr foo) t)
      (default-behavior if-not-found default "association not found"))))

(define-setf-expander association
  (x alist &key (key nil ksp) (test nil tsp) (test-not nil tnsp)
     &environment env)
  "Set the association of x in a alist to the given value.
May alter the cons cells that constitute the spine of the alist"
   (multiple-value-bind (al-dummies al-vals al-newvals al-setter al-getter)
       (get-setf-expansion alist env)
     (with-gensyms (x-val al-val assoc cons head store)
       (values `(,x-val ,@al-dummies ,al-val ,assoc ,cons ,head)
               `(,x ,@al-vals
		 ,al-getter
		 (assoc ,x-val ,al-val
			,@(make-keys (key ksp) (test tsp) (test-not tnsp)))
		 (if ,assoc ,assoc (cons ,x nil))
		 (if ,assoc nil (cons ,cons ,al-val)))
               `(,store)
               `(progn (rplacd ,cons ,store)
		       (unless ,assoc
			 (let ((,(car al-newvals) ,head))
			   ,al-setter))
		       ,store)
               `(cdr ,cons)))))

(defun make-collector ()
  "Create a collector closure, that when called with one argument,
   tucks the argument at the end of a list, when called with no argument,
   returns the list and resets it for further potential use"
  (let ((acc ()))
    #'(lambda (&optional (x nil addp))
        (cond
          (addp (push x acc) t)
          (t (prog1 (nreverse acc) (setf acc nil)))))))
