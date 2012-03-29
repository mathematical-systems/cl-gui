;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.syntax-sugar.documentation)

(def project :hu.dwim.syntax-sugar)

(def book user-guide (:title "User guide")
  (chapter (:title "Introduction")
    (paragraph ()
      "TODO"))
  (chapter (:title "Supported Common Lisp Implementations")
    (paragraph ()
      "SBCL"))
  (chapter (:title "Supported Operating Systems")
    (paragraph ()
      "Linux"))
  (chapter (:title "Tutorial")
    (paragraph ()
      "TODO")))

;; TODO: emacs documentation
;; to use from your init.el:
;; (setq dwim-workspace (getenv "DWIM_WORKSPACE"))
;; (add-to-list 'load-path (expand-file-name (concat dwim-workspace "/hu.dwim.syntax-sugar/emacs/")))
;; (require 'hu.dwim.syntax-sugar)

;; some interesting unicode characters:
;; ← ↑ → ↓ ║ = ≠ ≡ ≢ < > ≤ ≥ ∧ ∨ ¬ ∅ … ‼ ′ ″ ∀ ∃ ∈ √ ² ³
;;
;; (loop for i upfrom #X03B1 repeat 25 collect (string i))
;; α β γ δ ε ζ η θ ι κ λ μ ν ξ ο π ρ ς σ τ υ φ χ ψ ω
;;
;; (loop for i upfrom #X0391 repeat 25 collect (string i))
;; Α Β Γ Δ Ε Ζ Η Θ Ι Κ Λ Μ Ν Ξ Ο Π Ρ ΢ Σ Τ Υ Φ Χ Ψ Ω

;; TODO: make this part of the documentation
#|
;;; Following the steps outlined below will make your project
;;; load using ASDF and Slime's C-c C-c will work fine, too.

;;; in your .asd:

;; TODO update after the (def package ...) refactor

(defsystem :foo
  :default-component-class cl-source-file-with-readtable
  :class system-with-readtable
  :components
  (...))


;;; in some of the early loaded files:

(in-package :my-package)


|#
