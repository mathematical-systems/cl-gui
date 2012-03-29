;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.syntax-sugar.test)

(defsuite* (test/string-quote :in test))

(deftest test/string-quote/return-type ()
  (enable-string-quote-syntax)
  (bind ((result (read-from-string "｢alma｣")))
    (is (typep result 'simple-base-string))
    (is (string= result "alma")))
  (bind ((result (read-from-string "｢körte｣")))
    (is (typep result 'string))
    (is (string= result "körte"))))

(deftest test/string-quote/simple ()
  (enable-string-quote-syntax)
  (is (string= "alma" (read-from-string "｢alma｣")))
  (is (string= "al\"ma" (read-from-string "｢al\"ma｣"))))
