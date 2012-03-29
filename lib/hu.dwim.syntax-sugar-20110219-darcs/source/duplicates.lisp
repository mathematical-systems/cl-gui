;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.syntax-sugar)

;;; Copied over from Stefil

(define-condition illegal-lambda-list (error)
  ((lambda-list :accessor lambda-list-of :initarg :lambda-list)))

(defun illegal-lambda-list (lambda-list)
  (error 'illegal-lambda-list :lambda-list lambda-list))

(locally #+sbcl(declare (sb-ext:muffle-conditions style-warning sb-ext:compiler-note))
  (defun parse-lambda-list (lambda-list visitor &key macro)
    ;; TODO finish macro lambda list parsing
    (declare (optimize (speed 3))
             (type list lambda-list)
             (type (or symbol function) visitor))
    (let ((args lambda-list))
      (labels
          ((fail ()
             (illegal-lambda-list lambda-list))
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
                (let* ((arg (ensure-list (pop args)))
                       (name (first arg))
                       (default (second arg)))
                  (funcall visitor '&optional name arg nil default))
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
                (let* ((arg (ensure-list (pop args)))
                       (name-part (first arg))
                       (default (second arg))
                       (external-name (if (consp name-part)
                                          (progn
                                            (unless (= (length name-part) 2)
                                              (illegal-lambda-list lambda-list))
                                            (first name-part))
                                          (intern (symbol-name name-part) #.(find-package "KEYWORD"))))
                       (local-name (if (consp name-part)
                                       (second name-part)
                                       name-part)))
                  (funcall visitor '&key local-name arg external-name default))
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
             (return-from parse-lambda-list (values))))
        (when args
          (case (first args)
            (&whole (process-&whole))
            (t      (process-required))))))))

(defun lambda-list-to-funcall-list (args)
  (let ((result (list))
        (rest-variable-name nil))
    (parse-lambda-list args
                       (lambda (kind name entry &optional external-name default)
                         (declare (ignore entry default))
                         (case kind
                           (&key
                            (push external-name result)
                            (push name result))
                           (&allow-other-keys)
                           (&rest (setf rest-variable-name name))
                           (t (push name result)))))
    (values (nreverse result)
            rest-variable-name)))

(defun lambda-list-to-lambda-list-with-quoted-defaults (args)
  (let ((primaries (list))
        (keywords (list))
        (optionals (list))
        (rest-variable-name nil)
        (allow-other-keys? nil))
    (parse-lambda-list args
                       (lambda (kind name entry &optional external-name default)
                         (declare (ignore entry))
                         (ecase kind
                           (&key
                            (push `((,external-name ,name) (quote ,default)) keywords))
                           (&optional
                            (push `(,name ,default) optionals))
                           (&allow-other-keys (setf allow-other-keys? t))
                           (&rest (setf rest-variable-name name))
                           ((nil) (push name primaries)))))
    (values `(,@(nreverse primaries)
              ,@(when optionals (cons '&optional (nreverse optionals)))
              ,@(when keywords (cons '&key (nreverse keywords)))
              ,@(when allow-other-keys? (list '&allow-other-keys)))
            rest-variable-name)))
