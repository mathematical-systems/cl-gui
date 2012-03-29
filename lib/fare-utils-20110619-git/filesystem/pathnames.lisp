;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

#+xcvb (module (:depends-on ("package" "base/utils" "base/streams")))

(in-package :fare-utils)

(exporting-definitions

;; This is only valid for Unix
(defvar +root-path+ (make-pathname :directory '(:absolute))
  "pathname for the file hierarchy root")

;; You should only use this with merge-pathnames*
(defvar +back-path+ (make-pathname :directory '(:relative :back))
  "logical parent path")

(defun pathname-directory-pathname (pathname)
  (make-pathname :type nil :name nil :version nil :defaults pathname))

(defun pathname-base-pathname (pathname)
  (make-pathname :directory nil :defaults pathname))

(defun merge-pathnames* (specified &optional (defaults *default-pathname-defaults*))
  "MERGE-PATHNAMES* is like MERGE-PATHNAMES except that if the SPECIFIED pathname
does not have an absolute directory, then the HOST and DEVICE come from the DEFAULTS.
Also, if either argument is NIL, then the other argument is returned unmodified."
  ;; Same as in ASDF 2.
  (when (null specified) (return-from merge-pathnames* defaults))
  (when (null defaults) (return-from merge-pathnames* specified))
  (let* ((specified (pathname specified))
         (defaults (pathname defaults))
         (directory (pathname-directory specified))
         (directory (if (stringp directory) `(:absolute ,directory) directory))
         (name (or (pathname-name specified) (pathname-name defaults)))
         (type (or (pathname-type specified) (pathname-type defaults)))
         (version (or (pathname-version specified) (pathname-version defaults))))
    (labels ((ununspecific (x)
               (if (eq x :unspecific) nil x))
             (unspecific-handler (p)
               (if (typep p 'logical-pathname) #'ununspecific #'identity)))
      (multiple-value-bind (host device directory unspecific-handler)
          (ecase (first directory)
            ((nil)
             (values (pathname-host defaults)
                     (pathname-device defaults)
                     (pathname-directory defaults)
                     (unspecific-handler defaults)))
            ((:absolute)
             (values (pathname-host specified)
                     (pathname-device specified)
                     directory
                     (unspecific-handler specified)))
            ((:relative)
             (values (pathname-host defaults)
                     (pathname-device defaults)
                     (append (pathname-directory defaults) (cdr directory))
                     (unspecific-handler defaults))))
        (make-pathname :host host :device device :directory directory
                       :name (funcall unspecific-handler name)
                       :type (funcall unspecific-handler type)
                       :version (funcall unspecific-handler version))))))

(defun pathname-parent (pathname)
  "Takes a pathname and returns the pathname of the parent directory
of the directory of the given pathname"
  (cond
    ;; no pathname, no parent
    ((null pathname)
     nil)
    ;; / is its own parent.
    ((equal (pathname-directory pathname) '(:absolute))
     +root-path+)
    (t
     (merge-pathnames* +back-path+
		      (pathname-directory-pathname pathname)))))

(defun top-level-name (name)
  "This function takes a name, and returns everything up to the first \"/\" in the name"
  (subseq name 0 (position #\/ (namestring name))))

(defun directory-name-p (name)
  (and (stringp name)
       (eql #\/ (last-char name))))

(defun portable-pathname-string-component-char-p (c)
  (declare (type character c))
  ;; Assumes ASCII
  (and (or (char<= #\a c #\z)
	   (char<= #\A c #\Z)
	   (char<= #\0 c #\9)
	   (member c '(#\" #\. #\, #\- #\+ #\_)))
       t))

(defun portable-pathname-string-component-p (x)
  (and (stringp x)
       (every #'portable-pathname-string-component-char-p x)
       (not (member x '("" "." "..") :test 'equal))))

(defun portable-pathname-type-component-p (x)
  (and (portable-pathname-string-component-p x)
       (not (find #\. x))))

(defun portable-pathname-directory-output
    (directory &key out (allow-absolute t) (allow-relative t))
  "DIRECTORY being the directory component of a pathname,
output to OUT a portable representation of it,
erroring out if some source of non-portability is found"
  (with-output (out)
    (labels ((d2s (x)
	       (dolist (c x)
		 (unless (portable-pathname-string-component-p c)
		   (error "Non-portable component ~S in directory ~S" c directory))
		 (write-string c out)
		 (write-char #\/ out))))
      (cond
        ((null directory) ;; accept the former representation, not the latter
         (setf directory '(:relative)))
        ((equal directory '(:relative))
         (error "Invalid directory (:relative)")))
      (cond
	((member directory '(:wild :unspecific nil))
	 (error "Cannot portably stringify directory ~S" directory))
	((stringp directory)
	 (error "xcvb doesn't support non-hierarchical filesystems"))
	((and (consp directory) (eq (car directory) :absolute))
	 (unless allow-absolute
	   (error "absolute directory ~S not allowed" directory))
	 (write-char #\/ out)
	 (d2s (cdr directory)))
	((and (consp directory) (eq (car directory) :relative))
	 (unless allow-relative
	   (error "relative directory ~S not allowed" directory))
         (d2s (cdr directory)))
	(t
	 (error "Invalid directory ~S" directory))))))

(defun portable-pathname-name-output (name &key out)
  (with-output (out)
    (unless (portable-pathname-string-component-p name)
      (error "Non-portable pathname name ~S" name))
    (write-string name out)))

(defun portable-pathname-type-output (type &key out)
  (with-output (out)
    (unless (portable-pathname-type-component-p type)
      (error "Non-portable pathname type ~S" type))
    (write-string type out)))

(defun portable-pathname-output (pathname &key out (allow-absolute t) (allow-relative t))
  (with-output (out)
    (let* ((p (pathname pathname))
	   (directory (pathname-directory p))
	   (name (pathname-name p))
	   (type (pathname-type p))
	   (version (pathname-version p)))
      (unless (member version '(nil :unspecific :newest))
	(error "Non-portable pathname version ~S in ~S" version pathname))
      (portable-pathname-directory-output
       directory
       :out out :allow-absolute allow-absolute :allow-relative allow-relative)
      (when name
	(portable-pathname-name-output name :out out)
	(cond
	  ((stringp type)
	   (write-char #\. out)
	   (portable-pathname-type-output type :out out))
	  ((member type '(nil :unspecific :newest))
	   (when (find #\. name)
	     (error "Non-portable pathname ~S with a dot in name but no type" pathname)))
	  (t
	   (error "Non-portable pathname type ~S" type)))))))

(defun portable-namestring (pathname)
  (portable-pathname-output pathname))

(defun portable-pathname-from-string (string &key
					     (start 0) (end (length string))
					     (allow-absolute t) (allow-relative t))
  (let (r name type)
    (unless (< start end)
      (error "cannot parse beyond the end of string ~S (start: ~S, end: ~S)" string start end))
    (cond
      ((eql (char string start) #\/)
       (unless allow-absolute
	 (error "unexpected absolute pathname ~S (start: ~S, end: ~S)" string start end))
       (setf r (list :absolute)) (incf start))
      (t
       (unless allow-relative
	 (error "unexpected relative pathname ~S (start: ~S, end: ~S)" string start end))
       (setf r (list :relative))))
    (loop :for p = (and (< start end) (position #\/ string :start start :end end))
	  :while p :do
	  (let ((dir (subseq string start p)))
 	    (unless (portable-pathname-string-component-p dir)
 	      (error "non-portable pathname directory ~S" dir))
	    (push dir r)
	    (setf start (1+ p))))
    (when (< start end)
      (let ((ldp (position #\. string :start start :end end :from-end t)))
	(setf name (subseq string start (or ldp end))
	      type (and ldp (subseq string (1+ ldp) end)))
	(unless (portable-pathname-string-component-p name)
	  (error "non-portable pathname name ~S" name))
	(when type
	  (unless (portable-pathname-type-component-p type)
	    (error "non-portable pathname type ~S" type)))))
    (make-pathname :directory (unless (equal r '(:relative)) (nreverse r))
                   :name name :type type)))

(defun subpathname (path string)
  (merge-pathnames*
   (portable-pathname-from-string string :allow-absolute nil)
   path))

(defun pathname-absolute-p (path)
  "Assuming PATH is a pathname, is it an absolute pathname?"
  (let ((directory (pathname-directory path)))
    (and (consp directory) (eq (car directory) :absolute))))

(defun absolute-pathname-p (path)
  "Return true iff pathname P is an absolute pathname"
  (and (pathnamep path)
       (pathname-absolute-p path)))

(defun portable-namestring-absolute-p (namestring)
  (eql (first-char namestring) #\/))

(defun portable-pathname-absolute-p (name)
  (etypecase name
    (pathname (pathname-absolute-p name))
    (string (portable-namestring-absolute-p name))))

(defun absolute-portable-namestring-p (namestring)
  (and (portable-namestring-p namestring)
       (portable-namestring-absolute-p namestring)))

(defun portable-namestring-p (x)
  (and (stringp x)
       (ignore-errors (portable-pathname-from-string x))
       t))

(defun ensure-absolute-pathname (x)
  (let ((path (pathname x)))
    (cond
      ((absolute-pathname-p path)
       path)
      ((absolute-pathname-p *default-pathname-defaults*)
       (merge-pathnames* path))
      (t
       (truename (merge-pathnames* path *default-pathname-defaults*))))))

(defun portable-namestring-prefix<= (x y)
  (and (string-prefix-p x y)
       (or (= (length x) (length y))
           (eql #\/ (char y (length x))))))

(defun ensure-pathname-is-directory (x)
  (etypecase x
    (string
     (cond
       ((equal x "")
	(error "empty namestring"))
       ((eql (last-char x) #\/)
	(pathname x))
       (t
	(pathname (strcat x "/")))))
    (pathname
     (if (or (pathname-name x)
             (pathname-type x)
             (not (member (pathname-version x) '(nil :unspecific :newest))))
       (error "pathname ~S isn't a directory" x)
       x))))

(defun unwilden (pathspec)
  (block :u
    (let ((p (pathname pathspec)))
      (unless (wild-pathname-p p)
        (return-from :u (values p (make-pathname))))
      (when (or (wild-pathname-p p :host) (wild-pathname-p p :device))
        (return-from :u (values (make-pathname) p)))
      (let ((host (pathname-host p))
            (device (pathname-device p))
            (directory (pathname-directory p)))
        (when (wild-pathname-p p :directory)
          (when (atom directory)
            (return-from :u (values (make-pathname) p)))
          (loop :with unwild = nil
            :for i :from 1 :to (length directory)
            :for dir = (subseq directory 0 i)
            :until (wild-pathname-p (make-pathname :directory dir) :directory)
            :do (setf unwild dir)
            :finally (return-from :u
                       (values
                        (make-pathname :host host :device device :directory unwild)
                        (make-pathname :host host :device device
                                       :directory (if unwild
                                                      `(:relative ,@(subseq directory (1- i)))
                                                      directory)
                                       :defaults p)))))
        (values
         (make-pathname :host host :device device :directory directory)
         (make-pathname :directory nil :defaults p))))))
)
