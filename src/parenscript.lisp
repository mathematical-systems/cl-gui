(in-package :cl-gui)

(defun eval-ps (ps-form)
  (let ((code (ps* ps-form)))
    (send-message :eval :code code)))

(defun load-ps-file (filename)
  (let ((code (ps-compile-file filename)))
    (send-message :eval :code code)))

(defun compile-ps-file (filename &optional output-file)
  (let* ((name (pathname-name filename))
         (ext (pathname-type filename))
         (target-output-file
           (or output-file
               (progn
                 (assert (not (equalp ext "js")))
                 (merge-pathnames (make-pathname :name name :type "js") filename)))))
    (with-open-file (f target-output-file
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
      (let ((*parenscript-stream* f))
        (format *terminal-io* "~&Compiling PS file ~a~%" filename)
        (ps-compile-file filename)))
    target-output-file))

(defmacro defun-ps (name args &body body)
  (eval-ps `(setf ,name (lambda ,args ,@body)))
  `',name)

(defmacro+ps $ (id &rest chains)
  `(chain (jquery ,id) ,@chains))

(defmacro+ps add-div (container id)
  (check-type id string)
  `(chain ($ ,container) (append ,(format nil "<div id=~s></div>" id))))

