;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

(def (function eo) sanitize-string (string lower-limit upper-limit &key (replacement #\?))
  "By default sanitize for the printable ASCII range. Limits are inclusive."
  (check-type string string)
  (check-type replacement character)
  (check-type lower-limit (or null non-negative-integer))
  (check-type upper-limit (or null non-negative-integer))
  (bind ((result (make-string (length string) :element-type (array-element-type string))))
    (iter (for char :in-vector string)
          (for char-code = (char-code char))
          (for index :upfrom 0)
          (setf (aref result index)
                (if (or (and lower-limit
                             (< char-code lower-limit))
                        (and upper-limit
                             (< upper-limit char-code)))
                    replacement
                    char)))
    result))

(def (function eo) sanitize-string/printable-ascii (string &key (replacement #\?))
  (sanitize-string string 32 126 :replacement replacement))


;;;;;;
;;; Symbols

(def (function e) find-symbol* (symbol-name &key packages (otherwise :error otherwise?))
  (check-type symbol-name string)
  (setf packages (ensure-list packages))
  (bind ((first-colon-position (position #\: symbol-name :test #'char=))
         (last-colon-position (position #\: symbol-name :test #'char= :from-end #t))
         (symbol-name/name (if last-colon-position
                               (subseq symbol-name (1+ last-colon-position))
                               symbol-name))
         (symbol-name/package (when first-colon-position
                                (if (zerop first-colon-position)
                                    "KEYWORD"
                                    (subseq symbol-name 0 first-colon-position))))
         (colon-count (if first-colon-position
                          (- (1+ last-colon-position) first-colon-position)
                          0))
         ;; (external? (= colon-count 1))
         )
    (unless (<= colon-count 2)
      (return-from find-symbol* (handle-otherwise (error "~S is not a legal symbol name" symbol-name))))
    (flet ((try-in-package (package signal-error?)
             (check-type package package)
             ;; TODO add support for external/internal handling
             (or (find-symbol symbol-name/name package)
                 (if signal-error?
                     (handle-otherwise (error "~S does not exist in package ~A" symbol-name/name package))
                     nil))))
      (if symbol-name/package
          (bind ((package (find-package symbol-name/package)))
            (if package
                (try-in-package package #t)
                (handle-otherwise (error "Package named ~S does not exist" symbol-name/package))))
          (or (some (lambda (package-name)
                      (try-in-package (find-package package-name) #f))
                    packages)
              (handle-otherwise (error "Could not find symbol named ~S in packages ~S" symbol-name packages)))))))

;;;;;;
;;; Whitespace

(def (constant e) +whitespace-characters+ (coerce
                                            '(#\Space #\Tab #\Linefeed #\Return #\Page)
                                            'simple-base-string))

(def (function eio) string-trim-whitespace (text)
  (check-type text string)
  (string-trim +whitespace-characters+ text))

(def (function eio) whitespace? (character)
  (find character +whitespace-characters+ :test #'char=))

(def (macro e) string/trim-whitespace-and-maybe-nil-it (variable)
  `(progn
     (when ,variable
       (setf ,variable (string-trim-whitespace ,variable))
       (when (zerop (length ,variable))
         (setf ,variable nil)))
     ,variable))

;;;;;;
;;; make-string-of-spaces

(def constant +string-of-spaces/cache-size+ 64)

(def (constant :test 'equalp) +indent-length->string-of-spaces+
  (bind ((result (make-array +string-of-spaces/cache-size+)))
    (iter (for index :from 0 :below +string-of-spaces/cache-size+)
          (setf (aref result index) (make-string index :initial-element #\Space)))
    result))

(def (function eo) make-string-of-spaces (count)
  (check-type count array-index)
  (if (< count +string-of-spaces/cache-size+)
      (aref +indent-length->string-of-spaces+ count)
      (make-string count :element-type 'base-char :initial-element #\Space)))

(def (function e) write-spaces (count &optional (stream *standard-output*))
  (write-string (make-string-of-spaces count) stream))

(def (function e) write-characters (character count &optional (stream *standard-output*))
  (iter (repeat count)
        (write-char character stream)))

;;;;;;
;;; String concatenation

(def (function eo) string+ (&rest args)
  ;; don't inline, otherwise the compiler macro is kicked
  (apply #'concatenate 'string args))

(def compiler-macro string+ (&rest args)
  `(concatenate 'string ,@args))

(def (function eo) join-strings (separator &rest strings)
  (with-output-to-string (string)
    (iter (for el :in-sequence strings)
          (unless (first-time-p)
            (princ separator string))
          (write-string el string))))

(def (function e) make-adjustable-string (initial-contents)
  (make-array (length initial-contents) :element-type 'character :initial-contents initial-contents :adjustable #t))

;;;;;;
;;; Roman numeral

(def (function e) roman-numeral-digit-character? (char)
  (check-type char character)
  (case char
    (#\I 1)
    (#\V 5)
    (#\X 10)
    (#\L 50)
    (#\C 100)
    (#\D 500)
    (#\M 1000)
    (t nil)))

(def (function e) parse-roman-numeral (str &key (start 0) end)
  (iter (for index from start below (or end (length str)))
        (for ch = (elt str index))
        (for digit = (roman-numeral-digit-character? ch))
        (while digit)
        (for prev-digit previous digit)
        (for result first 0 then (if (>= prev-digit digit)
                                         (+ result prev-digit)
                                         (- result prev-digit)))
        (finally (return-from parse-roman-numeral (values (+ result (or digit prev-digit 0)) index)))))

(def (function e) string-with-numeric< (str1 str2 digit-p parse-number &key (start1 0) (start2 0))
  (bind ((num-start1 (position-if digit-p str1 :start start1))
         (num-start2 (position-if digit-p str2 :start start2)))
    (if (and (numberp num-start1)
             (numberp num-start2)
             (= num-start1 num-start2))
        (cond
          ((string< str1 str2 :start1 start1 :end1 num-start1 :start2 start2 :end2 num-start2) #t)
          ((string> str1 str2 :start1 start1 :end1 num-start1 :start2 start2 :end2 num-start2) #f)
          (t (bind ((num-end1 (position-if-not digit-p str1 :start num-start1))
                    (num-end2 (position-if-not digit-p str2 :start num-start2))
                    (num1 (funcall parse-number str1 :start num-start1 :end num-end1))
                    (num2 (funcall parse-number str2 :start num-start2 :end num-end2)))
               (cond
                 ((< num1 num2) #t)
                 ((> num1 num2) #f)
                 (t (string-with-numeric< str1 str2 :start1 num-end1 :start2 num-end2))))))

        (string< str1 str2 :start1 start1 :start2 start2))))

(def (function e) string-with-integers< (str1 str2 &key (start1 0) (start2 0))
  (string-with-numeric< str1 str2 #'digit-char-p #'parse-integer :start1 start1 :start2 start2))

(def (function e) string-with-roman-numerals< (str1 str2 &key (start1 0) (start2 0))
  (string-with-numeric< str1 str2 #'roman-numeral-digit-character? #'parse-roman-numeral :start1 start1 :start2 start2))

;;;;;;
;;; Well known sets

(def (constant e) +lower-case-ascii-alphabet+ (coerce "abcdefghijklmnopqrstuvwxyz" 'simple-base-string))
(def (constant e) +upper-case-ascii-alphabet+ (coerce "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 'simple-base-string))
(def (constant e) +ascii-alphabet+ (coerce (concatenate 'string +upper-case-ascii-alphabet+ +lower-case-ascii-alphabet+) 'simple-base-string))
(def (constant e) +alphanumeric-ascii-alphabet+ (coerce (concatenate 'string +ascii-alphabet+ "0123456789") 'simple-base-string))
(def (constant e) +base64-alphabet+ (coerce (concatenate 'string +alphanumeric-ascii-alphabet+ "+/") 'simple-base-string))

;;;;;;
;;; Random string

(def (function eo :inline :possible) random-string (&optional length (alphabet +ascii-alphabet+) prefix)
  (unless length
    (setf length 32))
  (unless alphabet
    (setf alphabet +ascii-alphabet+))
  (check-type length (or null array-index))
  (check-type alphabet string)
  (assert (or (null prefix)
              (< (length prefix) length)))
  (macrolet ((with-string-dispatch (&body body)
               `(etypecase alphabet
                  (simple-base-string (symbol-macrolet ((-element-type- 'base-char))
                                        ,@body))
                  (simple-string      (symbol-macrolet ((-element-type- 'character))
                                        ,@body))
                  (string             (symbol-macrolet ((-element-type- 'character))
                                        ,@body)))))
    (with-string-dispatch
      (loop
        :with result = (make-string length :element-type -element-type-)
        :with alphabet-length = (length alphabet)
        :initially (when prefix
                     (replace result prefix))
        :for i :from (if prefix (length prefix) 0) :below length
        :do (setf (aref result i) (aref alphabet (random alphabet-length)))
        :finally (return result)))))

;;;;;;
;;; Levenshtein distance

(def (function e) levenshtein-distance (s1 s2)
  (let* ((width (1+ (length s1)))
	 (height (1+ (length s2)))
	 (d (make-array (list height width))))
    (dotimes (x width)
      (setf (aref d 0 x) x))
    (dotimes (y height)
      (setf (aref d y 0) y))
    (dotimes (x (length s1))
      (dotimes (y (length s2))
	(setf (aref d (1+ y) (1+ x))
	      (min (1+ (aref d y (1+ x)))
		   (1+ (aref d (1+ y) x))
		   (+ (aref d y x)
		      (if (char= (aref s1 x) (aref s2 y))
			  0
			  1))))))
    (aref d (1- height) (1- width))))

(def (function e) levenshtein-relative-distance (s1 s2)
  (bind ((max-distance (max (length s1) (length s2)))
         (distance (levenshtein-distance s1 s2)))
    (if (zerop max-distance)
        0
        (/ distance max-distance))))
