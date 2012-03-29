;;; -*- encoding: utf-8 -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(provide 'hu.dwim.syntax-sugar)

(defgroup hu.dwim.syntax-sugar.faces nil
  "Faces installed by hu.dwim.syntax-sugar."
  :prefix "hu.dwim.syntax-sugar"
  :group 'applications)

(defface hu.dwim.syntax-sugar.lambda-face
   '((((class color) (background light)) (:foreground "Purple3" :weight bold)))
  "Face for the lambda character."
  :group 'hu.dwim.syntax-sugar.faces)

(defface hu.dwim.syntax-sugar.greek-face
   '((((class color) (background light)) (:foreground "Purple3" :weight normal)))
  "Face for greek characters."
  :group 'hu.dwim.syntax-sugar.faces)

(defun hu.dwim.syntax-sugar.greek-letter? (char)
  (or (and (<= #X03B1 char)
           (< char (+ #X03B1 25)))
      (and (<= #X0391 char)
           (< char (+ #X0391 25)))))

(defun hu.dwim.syntax-sugar.install-substitute-pattern (pattern replacement)
  (setf replacement (cond
                      ((characterp replacement)
                       (string replacement))
                      ((stringp replacement)
                       replacement)
                      (t (error "Don't know how to treat hu.dwim.syntax-sugar replacement %s" replacement))))
  (font-lock-add-keywords
   nil `((,pattern (0 (progn
                        ,(when (and (= 1 (length replacement))
                                    (hu.dwim.syntax-sugar.greek-letter? (elt replacement 0)))
                           `(add-text-properties (match-beginning 1) (match-end 1)
                                                 `(face (hu.dwim.syntax-sugar.greek-face
                                                         ,@',(when (eql (elt replacement 0) ?λ)
                                                               '(hu.dwim.syntax-sugar.lambda-face))))))
                        (compose-region (match-beginning 1) (match-end 1)
                                        ,replacement)
			nil))))))

(defun hu.dwim.syntax-sugar.install-greek-letter-rules ()
  "Installs some rules that display the written greek letter names using the single character of the greek letter."
  (let ((char-code #X03B1))
    (dolist (name '("alpha" "beta" "gamma" "delta" "epsilon" "zeta" "eta"
                    "theta" "iota" "kappa" "lambda" "mu" "nu" "xi" "omicron"
                    "pi" "rho" "sigma" "tau" "upsilon" "phi" "chi" "psi" "omega"))
      (when (> (length name) 3)
        (hu.dwim.syntax-sugar.install-substitute-pattern
         (concatenate 'string "[ 	\n()]\\(" name "\\)[ 	\n()]")
         (elt (string char-code) 0)))
      (incf char-code))))

(defun hu.dwim.syntax-sugar.install-square-bracket-lambda-rule ()
  "Installs some rules that displays [... !1 ...] as λ[... !1 ...] with the brackets colored."
  (font-lock-add-keywords
   nil `(("\\(\\[\\).*?[^{}]+?!.*?[^{}]+?\\(\\]\\)"
          (0 (progn
               (add-text-properties (match-beginning 1) (match-end 1) '(display "λ["))
               nil))
          (1 'hu.dwim.syntax-sugar.lambda-face)
          (2 'hu.dwim.syntax-sugar.lambda-face)))))

(add-hook 'lisp-mode-hook 'hu.dwim.syntax-sugar.install-greek-letter-rules)
(add-hook 'lisp-mode-hook 'hu.dwim.syntax-sugar.install-square-bracket-lambda-rule)
