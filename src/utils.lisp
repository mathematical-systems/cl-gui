(in-package :cl-gui)

;;;
(defun canonicalize-data (data)
  data)

(defun get-data-in-message (key message)
  (canonicalize-data (cdr (assoc key message :test 'equal))))

(defun canonicalize-value (value)
  value)

(defun get-value-in-data (key data)
  (canonicalize-value (cdr (assoc key data :test 'equal))))

(defun mapc-data (fn data)
  (loop for d in data
        for k = (car d)
        for v = (cdr d)
        do
           (unless (or (equal k :--id)
                       (equal k :--oid))
             (funcall fn k v))))

;;; debugging utils
(defvar *debug-output-p* t)
(defvar *debug-output* *trace-output*)
(defvar *debug-output-lock*
  (with-not-implemented-error
    #+sbcl (sb-thread:make-mutex)
    #+allegro (mp:make-process-lock)))

(defmacro with-lock ((lock) &body body)
  (with-not-implemented-error
    #+sbcl
    `(sb-thread:with-recursive-lock (,lock)
       (locally ,@body))
    #+allegro
    `(mp:with-process-lock (,lock)
       (locally ,@body))))

(defun debug-format (control-string &rest format-arguments)
  (with-lock (*debug-output-lock*)
    (when *debug-output-p*
      (format *debug-output* "~&")
      (with-standard-io-syntax
        (apply #'format *debug-output* control-string format-arguments))
      (format *debug-output* "~&")
      (finish-output *debug-output*)))
  nil)

;;; logging utils
(defvar *log-output* *error-output*)
(defvar *log-output-lock* *debug-output-lock*)

(defun log-format (control-string-or-category &rest args)
  ;; TODO: set restart
  (with-lock (*log-output-lock*)
    (let (control-string category)
      (etypecase control-string-or-category
        (string
         (setf control-string control-string-or-category
               category nil))
        (keyword
         (setf category control-string-or-category
               control-string (first args)
               args (rest args))))
      (format *log-output* "~&")
      (when category
        (format *log-output* "[~A]: " category))
      (apply #'format *log-output* control-string args)
      (format *log-output* "~&")
      (finish-output *log-output*)))
  nil)

