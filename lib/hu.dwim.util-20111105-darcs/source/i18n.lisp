;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

;;;;;;
;;; trie  = null
;;;       | integer
;;;       | cons nil (alist char trie)
;;;       | cons integer (alist char trie) 

(def function make-trie (string value &key (start 0) end)
  (labels ((make-it (start end)
             (if (= start end)
                 value
                 (cons nil (list (cons (elt string start) (make-it (1+ start) end)))))))
    (make-it start (or end (length string)))))

(def function trie-union (trie-1 trie-2)
  (etypecase trie-1
    (null trie-2)
    (integer (etypecase trie-2
               (null trie-1)
               (integer (if (= trie-1 trie-2)
                            trie-1
                            (error "Failed to union: ~A and ~A" trie-1 trie-2)))
               (cons (if (car trie-2)
                         (if (= trie-1 (car trie-2))
                             trie-2
                             (error "Failed to union: ~A and ~A" trie-1 trie-2))
                         (cons trie-1 (cdr trie-2))))))
    (cons (etypecase trie-2
            (null trie-1)
            (integer (if (car trie-1)
                         (if (= trie-2 (car trie-1))
                             trie-1
                             (error "Failed to union: ~A and ~A" trie-1 trie-2))
                         (cons trie-2 (cdr trie-1))))
            (cons (cons (trie-union (car trie-1) (car trie-2))
                        (bind ((chars (union (mapcar #'car (cdr trie-1))
                                             (mapcar #'car (cdr trie-2)))))
                          (iter (for char in chars)
                                (for pair-1 = (assoc char (cdr trie-1)))
                                (for pair-2 = (assoc char (cdr trie-2)))
                                (collect (cons char (trie-union (cdr pair-1)
                                                                (cdr pair-2))))))))))))

(def function trie-prefix-search (string trie &key (start 0) (char-test #'char=))
  "Returns the value associated with the longest prefix of STRING in the TRIE and the length of the matching prefix."
  (labels ((find-it (index end trie)
             (etypecase trie
               (null (values nil 0))
               (integer (values trie 0))
               (cons (if (= index end)
                         (values (car trie) 0)
                         (aif (assoc (elt string index) (cdr trie) :test char-test)
                              (bind (((:values value len) (find-it (1+ index) end (cdr it))))
                                (if value
                                    (values value (1+ len))
                                    (values (car trie) 0)))
                              (values (car trie) 0)))))))
    (find-it start (length string) trie)))

;;;;;;
;;; TODO extend cl-l10n to process the collation info in the CLDR files and that for locale specific sorting
;;; Collate table is a mapping from character codes to (or integer cons) implemented by a vector.
;;; If the value is an integer the ordinal of the character is that integer.
;;; If the value is a cons it is a trie mapping valid collate suffixes to ordinals.
;;; (e.g. ""->2, "s"->3, "cs"->3 for suffixes of "c" in hungarian)
;;; The supported character range is 0..511.

(def function build-collate-table (list)
  (bind ((table (make-array '(512) :initial-element nil)))
    (labels ((add-entry (letters ordinal)
               (bind ((letters (map 'string #'char-downcase letters))
                      (index (char-code (elt letters 0)))
                      (length (length letters))
                      (trie (aref table index))
                      (new-trie (trie-union trie (make-trie letters ordinal :start 1 :end length))))
                 (setf (aref table (char-code (elt letters 0))) new-trie
                       (aref table (char-code (char-upcase (elt letters 0)))) new-trie))))
      (iter (for ordinal from 0)
            (for letters in list)
            (etypecase letters
              (list (mapc (lambda (letter) (add-entry letter ordinal)) letters))
              (string (add-entry letters ordinal))))
      table)))

(def special-variable *hungarian-collate-table*
  (build-collate-table
   '(("a" "á") "b" "c" ("cs" "ccs") "d" ("dz" "ddz") ("dzs" "ddzs") ("e" "é") "f" "g" ("gy" "ggy")
     "h" ("i" "í") "j" "k" "l" ("ly" "lly") "m" "n" ("ny" "nny") ("o" "ó") ("ö" "ő") "p" "q" "r" "s"
     ("sz" "ssz") "t" ("ty" "tty") ("u" "ú") ("ü" "ű") "v" "w" "x" "y" "z" ("zs" "zzs"))))

(def special-variable *english-collate-table*
  (build-collate-table
   '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z")))

(def function collate-table-for (locale)
  (bind ((language (when locale (cl-l10n::language-of locale))))
    (cond
      ((null language) nil)
      ((string= language "hu") *hungarian-collate-table*)
      ((string= language "en") *english-collate-table*)
      (t (error "Unsupported locale: ~S" locale)))))

;;;;;;
;;; dwim string compare

(def macro dwim-string<>=-body (less-p equal-p)
  (bind ((less-op (if less-p '< '>))
         (greater-op (if less-p '> '<))
         (compare (if less-p
                 (if equal-p '<= '<)
                 (if equal-p '>= '>)))
         (string-compare (if less-p
                         (if equal-p 'string<= 'string<)
                         (if equal-p 'string>= 'string>))))
    `(if (and (null locale) (not parse-roman-numerals-p) (not parse-decimals-p))
        (,string-compare str1 str2 :start1 start1 :end1 end1 :start2 start2 :end2 end2)
        (bind ((end1 (or end1 (length str1)))
               (end2 (or end2 (length str2)))
               (locale-table (collate-table-for locale)))
          (labels ((ordinal-for (str start end)
                     (bind ((char (elt str start))
                            (code (char-code (elt str start)))
                            (ordinal (when (and locale-table
                                                (< code (length locale-table)))
                                       (elt locale-table code)))
                            ((:values decimal-number decimal-end)
                             (when (and parse-decimals-p (digit-char-p char))
                               (parse-integer str :start start :end end :junk-allowed #t)))
                            ((:values roman-number roman-end)
                             (when (and parse-roman-numerals-p (roman-numeral-digit-character? char))
                               (parse-roman-numeral str :start start :end end))))
                       (cond
                         (decimal-number
                          (values (+ 1000 decimal-number) (- decimal-end start 1)))
                         ((and roman-number
                               (or (= start 0)
                                   (not (alpha-char-p (elt str (1- start)))))
                               (or (= end roman-end)
                                   (not (alpha-char-p (elt str roman-end)))))
                          (values (+ 1000 roman-number) (- roman-end start 1)))
                         ((integerp ordinal)
                          (values ordinal 0))
                         ((consp ordinal)
                          (trie-prefix-search str ordinal :start (1+ start)))
                         (t
                          (values code 0))))))
      
            (iter (with end1 = (or end1 (length str1)))
                  (with end2 = (or end2 (length str2)))
                  (for i1 from start1 below end1)
                  (for i2 from start2 below end2)
                  (for len1 from 0)
                  (for len2 from 0)
                  (for (values ordinal1 extra1) = (ordinal-for str1 i1 end1))
                  (for (values ordinal2 extra2) = (ordinal-for str2 i2 end2))
                  #+nil(format t "First: ~A[~A-~A]=~A, Second: ~A[~A-~A]=~A~%"
                               str1 i1 (+ i1 extra1) ordinal1 str2 i2 (+ i2 extra2) ordinal2)
                  (cond
                    ((,less-op ordinal1 ordinal2) (return #t))
                    ((,greater-op ordinal1 ordinal2) (return #f))
                    (t (incf i1 extra1)
                       (incf i2 extra2)))
                  (finally
                   (return (,compare len1 len2)))))))))

(def (function e) dwim-string< (str1 str2 &key (start1 0) end1 (start2 0) end2 (locale (first (cl-l10n:current-locale)))
                                               parse-decimals-p parse-roman-numerals-p)
  (dwim-string<>=-body #t #f))

(def (function e) dwim-string> (str1 str2 &key (start1 0) end1 (start2 0) end2 (locale (first (cl-l10n:current-locale)))
                                               parse-decimals-p parse-roman-numerals-p)
  (dwim-string<>=-body #f #f))

(def (function e) dwim-string<= (str1 str2 &key (start1 0) end1 (start2 0) end2 (locale (first (cl-l10n:current-locale)))
                                                parse-decimals-p parse-roman-numerals-p)
  (dwim-string<>=-body #t #t))

(def (function e) dwim-string>= (str1 str2 &key (start1 0) end1 (start2 0) end2 (locale (first (cl-l10n:current-locale)))
                                                parse-decimals-p parse-roman-numerals-p)
  (dwim-string<>=-body #f #t))
