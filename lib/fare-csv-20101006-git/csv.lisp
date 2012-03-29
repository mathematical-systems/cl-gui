;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;; csv: reading files in Comma-Separated Values format.

#+xcvb (module (:depends-on ("package")))

#| "
HOME PAGE:
	http://www.cliki.net/fare-csv

LICENSE:
	http://www.geocities.com/SoHo/Cafe/5947/bugroff.html
	Also under no-restriction BSD license for those who insist.

DEPENDENCIES:
	apt-get install cl-asdf

USAGE:
	(asdf:load-system :fare-csv) ;; or (asdf:oos 'asdf:load-op :fare-csv) if using an old asdf
	(read-csv-line)
	(read-csv-stream s)
	(read-csv-file "foo.csv")

EXAMPLE USE:
	...

BUGS:
	I implemented just enough of CSV to import a specific file
	from a PC application that will remain unnamed.
	If you need more, you can cont(r)act me, and/or hack it yourself.

	CSV is intrinsically an underspecified lossy format,
	and the particular PC application I'm using loses heavily
	(i.e. no quoting convention at all, not even a pascal-like one)
	when text fields contain the quote character. Ouch.

SEE ALSO:
	This spec seems to explain popular usage, is refered by docs below.
	http://www.creativyst.com/Doc/Articles/CSV/CSV01.htm

	This one says about the same:
	http://edoceo.com/utilitas/csv-file-format

	There's now an RFC that tries to standardize CSV:
	http://www.rfc-editor.org/rfc/rfc4180.txt

	Here's what Perl hackers think CSV is:
	http://search.cpan.org/~hmbrand/Text-CSV_XS-0.59/CSV_XS.pm


Share and enjoy!
" |#

; -----------------------------------------------------------------------------
;;; Packaging stuff

(in-package :fare-csv)

; -----------------------------------------------------------------------------
;;; Optimization
(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (safety 1) (debug 3))))

; -----------------------------------------------------------------------------
;;; Thin compatibility layer
#| ;;; Not needed anymore
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'parse-number)
    (defun parse-number (s)
      (with-standard-io-syntax ()
	(let* ((*read-eval* nil)
	       (*read-default-float-format* 'double-float)
	       (n (read-from-string s)))
	  (if (numberp n) n)))))) |#

; -----------------------------------------------------------------------------
;;; Parameters

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +cr+ #.(format nil "~A" #\Return))
  (defparameter +lf+ #.(format nil "~A" #\Linefeed))
  (defparameter +crlf+ #.(format nil "~A~A" #\Return #\Linefeed))
  (defparameter *csv-variables* '())) ; list of (var rfc4180-value creativyst-value)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (macrolet
      ((def (var rfc4180 creativyst doc)
         `(progn
            (eval-when (:compile-toplevel :load-toplevel :execute)
              (pushnew `(,',var ,,rfc4180 ,,creativyst) *csv-variables* :key #'car))
            (defparameter ,var ,creativyst ,doc))))
    (def *separator*
	#\, #\,
      "Separator between CSV fields")
    (def *quote*
	#\" #\"
      "delimiter of string data; pascal-like quoted as double itself in a string.")
    (def *unquoted-quotequote*
	nil nil
      "does a pair of quotes represent a quote outside of quotes?
M$, RFC says NIL, csv.3tcl says T")
    (def *loose-quote*
	nil nil
      "can quotes appear anywhere in a field?")
    (def *allow-binary*
	t t
      "do we accept non-ascii data?")
    (def *keep-meta-info*
	nil nil
      "when parsing, include meta information?")
    (def *eol*
	+lf+ +crlf+
      "line ending when exporting CSV")
    (def *line-endings*
	(list +crlf+ +lf+) (list +cr+ +lf+ +crlf+)
      "acceptable line endings when importing CSV")
    (def *skip-whitespace*
	nil t
      "shall we skip unquoted whitespace around separators?")))

(defun char-ascii-text-p (c)
  (<= #x20 (char-code c) #x7E))

(defmacro with-creativyst-csv-syntax (() &body body)
  `(call-with-creativyst-csv-syntax (lambda () ,@body)))
(defun call-with-creativyst-csv-syntax (thunk)
  (progv (mapcar #'first *csv-variables*) (mapcar #'third *csv-variables*)
    (funcall thunk)))

(defmacro with-rfc4180-csv-syntax (() &body body)
  `(call-with-rfc4180-csv-syntax (lambda () ,@body)))
(defun call-with-rfc4180-csv-syntax (thunk)
  (progv (mapcar #'first *csv-variables*) (mapcar #'second *csv-variables*)
    (funcall thunk)))

(defmacro with-strict-rfc4180-csv-syntax (() &body body)
  `(call-with-strict-rfc4180-csv-syntax (lambda () ,@body)))

(defun call-with-strict-rfc4180-csv-syntax (thunk)
  (with-rfc4180-csv-syntax ()
    (setf *line-endings* (list +crlf+)
	  *allow-binary* nil)
    (funcall thunk)))

(defun valid-eol-p (x)
  (member x (list +cr+ +lf+ +crlf+) :test #'equal))

(defun validate-csv-parameters ()
  (assert (typep *separator* 'character) ())
  (assert (typep *quote* 'character) ())
  (assert (not (eql *separator* *quote*)) ())
  (assert (typep *unquoted-quotequote* 'boolean) ())
  (assert (typep *loose-quote* 'boolean) ())
  (assert (typep *keep-meta-info* 'boolean) ())
  (assert (valid-eol-p *eol*) ())
  (assert (not (member (aref *eol* 0) (list *separator* *quote*))) ())
  (assert (and *line-endings* (every #'valid-eol-p *line-endings*)) ())
  (assert (typep *skip-whitespace* 'boolean) ()))

;; For internal use only
(defvar *accept-cr* t "internal: do we accept cr?")
(defvar *accept-lf* t "internal: do we accept lf?")
(defvar *accept-crlf* t "internal: do we accept crlf?")

; -----------------------------------------------------------------------------
;;; The parser

(defmacro defsubst (name arglist &body body)
  "Declare an inline defun."
  `(progn (declaim (inline ,name))
	  (defun ,name ,arglist ,@body)))

(defsubst char-space-p (c)
  "Is character C some kind of white space?
BUG: this only handles a tiny subset of character sets,
even if restricted to ASCII. However, it's rather portable."
  (declare (type (or null character) c))
  (and c (member c '(#\Space #\Tab))))

;;#+DEBUG (defparameter *max* 2000)
;;#+DEBUG (defun maxbreak () (when (<= *max* 0) (setf *max* 2000) (break)) (decf *max*))

(defsubst accept-p (x s)
  (let ((c (peek-char nil s nil nil)))
    ;;#+DEBUG (format t "~&Current char: ~S~%" c)
    ;;#+DEBUG (maxbreak)
    (etypecase x
      (character (eql x c))
      ((or function symbol) (funcall x c))
      (integer (eql x (char-code c))))))

(defsubst accept (x s)
  (and (accept-p x s)
       (read-char s)))

(defsubst accept-eof (s)
  (not (peek-char nil s nil nil)))

(defsubst accept-eol (s)
  (block nil
    (when (and *accept-lf* (accept #\Linefeed s)) (return t))
    (when (or *accept-crlf* *accept-cr*)
      (when (accept #\Return s)
	(when *accept-crlf*
	  (if (accept #\Linefeed s)
	      (return t)
	      (unless *accept-cr*
		(error "Carriage-return without Linefeed!"))))
	(return t)))
    nil))

(defsubst accept-space (s)
  (accept #'char-space-p s))

(defsubst accept-spaces (s)
  (loop for x = (accept-space s)
	while x
	collect x))

(defsubst accept-quote (s)
  (accept *quote* s))

(defsubst accept-separator (s)
  (accept *separator* s))

(defun read-csv-line (s)
  (validate-csv-parameters)
  (let ((ss (make-string-output-stream))
	(fields '())
	(had-quotes nil)
	;;(had-spaces nil)
	;;(had-binary nil)
	(*accept-cr* (member +cr+ *line-endings* :test #'equal))
	(*accept-lf* (member +lf+ *line-endings* :test #'equal))
	(*accept-crlf* (member +crlf+ *line-endings* :test #'equal)))
    (labels
	((do-fields ()
	   ;;#+DEBUG (format t "~&do-field~%")
	   (setf had-quotes nil)
	   (when *skip-whitespace*
	     (accept-spaces s))
	   ;;#+DEBUG (format t "~&do-field, after spaces~%")
	   (cond
	     ((or (accept-eol s) (accept-eof s))
	      (done))
	     (t
	      (do-field-start))))
	 (do-field-start ()
	   ;;#+DEBUG (format t "~&do-field-start~%")
	   (cond
	     ((accept-separator s)
	      (add "") (do-fields))
	     ((accept-quote s)
	      (cond
		((and *unquoted-quotequote* (accept-quote s))
		 (add-char *quote*) (do-field-unquoted))
		(t
		 (do-field-quoted))))
	     (t
	      (do-field-unquoted))))
	 (do-field-quoted ()
	   ;;#+DEBUG (format t "~&do-field-quoted~%")
	   (setf had-quotes t)
           (cond
	     ((accept-eof s)
	      (error "unexpected end of stream in quotes"))
	     ((accept-quote s)
	      (cond
		((accept-quote s)
		 (quoted-field-char *quote*))
		(*loose-quote*
		 (do-field-unquoted))
		(t
		 (add (current-string))
		 (end-of-field))))
	     (t
	      (quoted-field-char (read-char s)))))
	 (quoted-field-char (c)
	   ;;#+DEBUG (format t "~&quoted-field-char~%")
	   (add-char c)
	   (do-field-quoted))
	 (do-field-unquoted ()
	   ;;#+DEBUG (format t "~&do-field-unquoted~%")
	   (if *skip-whitespace*
	       (let ((spaces (accept-spaces s)))
		 (cond
		   ((accept-separator s)
		    (add (current-string))
		    (do-fields))
		   ((or (accept-eol s) (accept-eof s))
		    (add (current-string))
		    (done))
		   (t
		    (loop for x in spaces do (add-char x))
		    (do-field-unquoted-no-skip))))
	       (do-field-unquoted-no-skip)))
	 (do-field-unquoted-no-skip ()
	   ;;#+DEBUG (format t "~&do-field-unquoted-no-skip~%")
	   (cond
	     ((accept-separator s)
	      (add (current-string))
	      (do-fields))
	     ((or (accept-eol s) (accept-eof s))
	      (add (current-string))
	      (done))
	     ((accept-quote s)
	      (cond
		((and *unquoted-quotequote* (accept-quote s))
		 (add-char *quote*) (do-field-unquoted))
		(*loose-quote*
		 (do-field-quoted))
		(t
		 (error "unexpected quote in middle of field"))))
	     (t
	      (add-char (read-char s))
	      (do-field-unquoted))))
	 (end-of-field ()
	   ;;#+DEBUG (format t "~&end-of-field~%")
	   (when *skip-whitespace*
	     (accept-spaces s))
	   (cond
	     ((or (accept-eol s) (accept-eof s))
	      (done))
	     ((accept-separator s)
	      (do-fields))
	     (t
	      (error "end of field expected"))))
	 (add (x)
	   ;;#+DEBUG (format t "~&add ~S~%" x)
	   (push
	    (if *keep-meta-info*
		(list x :quoted had-quotes)
		x)
	    fields))
	 (add-char (c)
	   ;;#+DEBUG (format t "~&add-char ~S~%" c)
	   (write-char c ss))
	 (current-string ()
	   (get-output-stream-string ss))
	 (done ()
	   ;;#+DEBUG (format t "~&done ~S~%" fields)
	   (nreverse fields)))
      (do-fields))))

(defun read-csv-stream (s)
  (loop until (accept-eof s)
    collect (read-csv-line s)))

(defun read-csv-file (pathname)
  (with-open-file (s pathname :direction :input :if-does-not-exist :error)
    (read-csv-stream s)))

(defun char-needs-quoting (x)
  (or (eql x *quote*)
      (eql x *separator*)
      (not (char-ascii-text-p x))))

(defun string-needs-quoting (x)
  (and (not (zerop (length x)))
       (or (char-space-p (char x 0))
	   (char-space-p (char x (1- (length x))))
	   (some #'char-needs-quoting x))
       t))

(defun write-csv-lines (lines stream)
  "Write many CSV line to STREAM."
  (dolist (x lines)
    (write-csv-line x stream)))

(defun write-csv-line (fields stream)
  "Write one CSV line to STREAM."
  (loop for x on fields
	while x
	do
	(write-csv-field (first x) stream)
	(when (cdr x)
	  (write-char *separator* stream)))
  (write-string *eol* stream))

(defun write-csv-field (field stream)
  (etypecase field
    (null t)
    (number (princ field stream))
    (string (write-csv-string-safely field stream))
    (symbol (write-csv-string-safely (symbol-name field) stream))))

(defun write-csv-string-safely (string stream)
  (if (string-needs-quoting string)
      (write-quoted-string string stream)
      (write-string string stream)))

(defun write-quoted-string (string stream)
  (write-char *quote* stream)
  (loop for c across string do
	(when (char= c *quote*)
	  (write-char c stream))
	(write-char c stream))
  (write-char *quote* stream))

;(trace read-csv-line read-csv-stream)

;;#+DEBUG (write (read-csv-file "test.csv"))
;;#+DEBUG (progn (setq *separator* #\;) (write (read-csv-file "/samba/ciev.csv")))
