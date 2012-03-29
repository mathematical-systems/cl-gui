;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

#+xcvb (module (:depends-on ("package" "base/macros")))

(in-package :fare-utils)

;;; File replacement function
(defun clobber-file-with-vector (file vector &key (external-format :default))
  (with-open-file (s file
                     :direction :output
                     :if-exists :rename-and-delete
                     :if-does-not-exist :create
                     :element-type (array-element-type vector)
                     :external-format external-format)
    (write-sequence vector s)))

(defparameter $buffer-size 8192)

#-gcl ;;---*** Test this code, fix it, then find out if GCL still borks when compiling it
(defmacro with-buffered-file-contents ((vector file
                                        &key
                                        (buffer-size '$buffer-size)
                                        (element-type ''(unsigned-byte 8))
                                        (external-format :default))
                                       &body body)
  (with-gensyms (stream length)
    (evaluating-once (buffer-size element-type external-format)
      `(with-open-file (,stream ,file
                        :direction :input
                        :if-does-not-exist nil
                        :element-type ,element-type
                        :external-format ,external-format)
        (let ((,vector (make-array (list ,buffer-size)
                                   :element-type ,element-type)))
          (loop :for ,length = (read-sequence ,vector ,stream)
                :while (> ,length 0) :do
                (progn
                  ,@body)))))))

(defun get-file-contents (file &key
                                   (element-type '(unsigned-byte 8))
                                   (external-format :default))
  (with-open-file (s file
                     :direction :input
                     :if-does-not-exist nil
                     :element-type element-type
                     :external-format external-format)
    (when s
      (let* ((size (and (file-position s :end) (file-position s)))
             (vector (if size
                         (make-array (list size) :element-type element-type)
                         (error "could not determine file size"))))
        (file-position s :start)
        (read-sequence vector s)
        vector))))

(defun equal-array (a1 a2)
  (and (= (length a1) (length a2))
       (every #'eql a1 a2)))

(defun file-contents-equal-vector-p (file vector &key (external-format :default))
  ;; a buffering version will be needed for large files
  (let ((contents (get-file-contents file
                                     :element-type (array-element-type vector)
                                     :external-format external-format)))
    (and contents
         (typecase vector
           (string
            (equal vector contents))
           ((vector (unsigned-byte 8) *)
            (equal-array vector contents))
           (t
            (error "can't compare file contents to that"))))))

(defun clobber-file-if-different (file vector &key (external-format :default))
  (unless (file-contents-equal-vector-p file vector :external-format external-format)
    (clobber-file-with-vector file vector :external-format external-format)))
