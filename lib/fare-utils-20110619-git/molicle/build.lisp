(in-package :molicle)

(defvar *molicles-by-name* (make-hash-table :test 'equal)
  "table of all molicles, by name")
(defvar *molicles-by-truename* (make-hash-table :test 'equal)
  "table of all molicles, by truename")

(defgeneric find-molicle (designator))
(defgeneric compile-molicle (designator))
(defgeneric load-molicle (designator))
(defgeneric post-compile-molicle (designator))

(defmethod find-molicle ((pathname pathname))
  (let* ((merged (merge-pathnames* pathname))
         (truename (probe-file merged))
         (fasl (asdf:compile-file-pathname* truename)))
    (unless truename
      (error "Cannot find molicle at ~A" pathname))
    (or (gethash truename *molicles-by-truename*)
        (make-instance
         'molicle
         :pathname merged
         :truename truename
         :fasl fasl
         :name (list :unnamed "molicle at" merged)))))

(defmethod compile-molicle ((pathname pathname))
  (compile-molicle (find-molicle pathname)))

(defmethod compile-molicle ((molicle molicle))
  (let ((*molicle* molicle)
        (*package* (find-package :molicle-user))
        (*evaluation-time* +coffee-time+)
        (fasl (molicle-fasl molicle)))
    (unless (null (state *molicle*))
      (error "molicle already compiled"))
    (ensure-directories-exist fasl)
    (compile-file (molicle-pathname molicle) :output-file fasl)
    (post-compile-molicle molicle)
    fasl))

(defmethod load-molicle ((pathname pathname))
  (load-molicle (find-molicle pathname)))

(defmethod load-molicle :before ((molicle molicle))
  (unless (eq (state *molicle*) :post-compiled)
    (error "molicle not compiled yet")))

(defmethod load-molicle ((molicle molicle))
  (let ((*molicle* molicle)
        (*package* (find-package :molicle-user))
        (*evaluation-time* +load-time+))
    (load (molicle-fasl molicle))))

(defmethod post-compile-molicle ((molicle molicle))
  (dolist (hook (reverse (post-compile-hook molicle)))
    (funcall hook)))
