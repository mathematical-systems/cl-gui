;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Basic String Manipulation

#+xcvb (module (:depends-on ("package" "base/utils")))

(in-package :fare-utils)


;;; simplifying strings into base-strings
(eval-now
 (def*parameter +all-chars-base-feature+ (featurify (subtypep 'character 'base-char))))

(eval-now
(exporting-definitions

(defun base-char-p (c)
  (typep c 'base-char))

(defun null-string-p (x)
  (and (stringp x) (zerop (length x))))

(defun ->string (x)
  "transform some stuff into a string"
  (typecase x
    (character (make-string
                1 :initial-element x
                :element-type (if (base-char-p x) 'base-char 'character)))
    (string x)
    (null "")
    (symbol (symbol-name x))
    (t (format nil "~A" x))))

(defun conc-string (&rest rest)
  "make a string by concatenating stuff"
  (apply #'strcat (mapcar #'->string rest)))
))

(eval-now
#+#.fare-utils:+all-chars-base-feature+
(exporting-definitions

(defun string-all-base-char-p (s)
  (check-type s string)
  t)

(defun string-basic-p (s)
  (check-type s string)
  t)

(defun simplify-string (s)
  (check-type s string)
  s)

)


#-#.fare-utils:+all-chars-base-feature+
(exporting-definitions

(defun string-all-base-char-p (s)
  (check-type s string)
  (every #'base-char-p s))

(defun string-basic-p (s)
  (check-type s string)
  (or (typep s 'base-string)
      (string-all-base-char-p s)))

(defun simplify-string (s)
  (check-type s string)
  (if (and (not (typep s 'base-string))
           (string-all-base-char-p s))
      (coerce s 'simple-base-string)
      s))))

(eval-now
(exporting-definitions
(defun strcat (&rest strings)
  (apply #'concatenate 'string strings))
;; The below definition would save space, but makes debugging hell,
;; because SLIME borks at the first string when it tries to print it
;; readably yet without read-eval.
;;    (let ((basicp (every #'string-basic-p strings)))
;;    (apply #'concatenate (if basicp 'base-string 'string) strings))
))
