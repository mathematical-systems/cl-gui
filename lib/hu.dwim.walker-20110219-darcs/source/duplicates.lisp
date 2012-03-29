;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.walker)

(defmacro dolist* ((iterator list &optional return-value) &body body)
  "Like DOLIST but destructuring-binds the elements of LIST.

If ITERATOR is a symbol then dolist* is just like dolist EXCEPT
that it creates a fresh binding."
  (if (listp iterator)
      (let ((i (gensym "DOLIST*-I-")))
        `(dolist (,i ,list ,return-value)
           (destructuring-bind ,iterator ,i
             ,@body)))
      `(dolist (,iterator ,list ,return-value)
         (let ((,iterator ,iterator))
           ,@body))))

;; TODO drop these and use lambda list parsing from alexandria once it supports macro lambda list parsing
(defun %illegal-lambda-list (lambda-list)
  (error "Illegal lambda list: ~S" lambda-list))

(locally #+sbcl(declare (sb-ext:muffle-conditions style-warning sb-ext:compiler-note))
  (defun %parse-lambda-list (lambda-list visitor &key macro)
   ;; TODO finish macro lambda list parsing
   (declare (optimize (speed 3))
            (type list lambda-list)
            (type (or symbol function) visitor))
   (let ((args lambda-list))
     (labels
         ((fail ()
            (%illegal-lambda-list lambda-list))
          (process-&whole ()
            (assert (eq (first args) '&whole))
            (pop args)
            (unless macro
              (fail))
            (let ((whole (pop args)))
              (unless whole
                (fail))
              (funcall visitor '&whole whole whole))
            (case (first args)
              (&key          (entering-&key))
              (&rest         (process-&rest))
              (&optional     (entering-&optional))
              (&body         (process-&body))
              (&environment  (process-&environment))
              ((&whole &aux &allow-other-keys) (fail))
              (t             (process-required))))
          (process-&body ()
            (assert (eq (first args) '&body))
            (pop args)
            (unless macro
              (fail))
            (let ((body (pop args)))
              (unless (null args)
                (fail))
              (unless body
                (fail))
              (funcall visitor '&body body body)))
          (process-&environment ()
            (assert (eq (first args) '&environment))
            (pop args)
            (unless macro
              (fail))
            (let ((env (pop args)))
              (unless env
                (fail))
              (funcall visitor '&environment env env))
            (case (first args)
              (&key          (entering-&key))
              (&rest         (process-&rest))
              (&optional     (entering-&optional))
              (&body         (process-&body))
              (&aux          (process-&aux))
              ((&whole &environment &allow-other-keys) (fail))
              (t             (process-required))))
          (process-required ()
            (unless args
              (done))
            (case (first args)
              (&key          (entering-&key))
              (&rest         (process-&rest))
              (&optional     (entering-&optional))
              (&body         (process-&body))
              (&environment  (process-&environment))
              ((&whole &allow-other-keys) (fail))
              (&aux          (entering-&aux))
              (t
               (let ((arg (pop args)))
                 (funcall visitor nil arg arg))
               (process-required))))
          (process-&rest ()
            (assert (eq (first args) '&rest))
            (pop args)
            (let ((rest (pop args)))
              (unless rest
                (fail))
              (funcall visitor '&rest rest rest))
            (unless args
              (done))
            (case (first args)
              (&key               (entering-&key))
              (&environment       (process-&environment))
              ((&whole &optional &rest &body &allow-other-keys) (fail))
              (&aux               (entering-&aux))
              (t                  (fail))))
          (entering-&optional ()
            (assert (eq (first args) '&optional))
            (pop args)
            (process-&optional))
          (process-&optional ()
            (unless args
              (done))
            (case (first args)
              (&key               (entering-&key))
              (&rest              (process-&rest))
              (&body              (process-&body))
              ((&whole &optional &environment &allow-other-keys) (fail))
              (&aux               (entering-&aux))
              (t
               (let ((arg (ensure-list (pop args))))
                 (funcall visitor '&optional (first arg) arg))
               (process-&optional))))
          (entering-&key ()
            (assert (eq (first args) '&key))
            (pop args)
            (process-&key))
          (process-&key ()
            (unless args
              (done))
            (case (first args)
              (&allow-other-keys       (funcall visitor '&allow-other-keys nil nil))
              ((&key &optional &whole &environment &body) (fail))
              (&aux                    (entering-&aux))
              (t
               (let ((arg (ensure-list (pop args))))
                 (funcall visitor '&key (first arg) arg))
               (process-&key))))
          (entering-&aux ()
            (assert (eq (first args) '&aux))
            (pop args)
            (process-&aux))
          (process-&aux ()
            (unless args
              (done))
            (case (first args)
              ((&whole &optional &key &environment &allow-other-keys &aux &body) (fail))
              (t
               (let ((arg (ensure-list (pop args))))
                 (funcall visitor '&aux (first arg) arg))
               (process-&aux))))
          (done ()
            (return-from %parse-lambda-list (values))))
       (when args
         (case (first args)
           (&whole (process-&whole))
           (t      (process-required))))))))

(defun lambda-list-to-variable-name-list (args &key macro include-specials)
  (let ((result (list))
        (rest-variable-name nil)
        (whole-variable-name nil)
        (env-variable-name nil))
    (%parse-lambda-list args
                       (lambda (kind name entry)
                         (declare (ignore entry))
                         (case kind
                           (&allow-other-keys )
                           (&environment      (setf env-variable-name name)
                                              (when include-specials
                                                (push name result)))
                           (&whole            (setf whole-variable-name name)
                                              (when include-specials
                                                (push name result)))
                           ((&rest &body)     (setf rest-variable-name name)
                                              (when include-specials
                                                (push name result)))
                           (t                 (push name result))))
                       :macro macro)
    (values (nreverse result)
            rest-variable-name
            whole-variable-name
            env-variable-name)))

