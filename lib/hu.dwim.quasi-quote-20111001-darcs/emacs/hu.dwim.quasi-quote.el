;;; -*- encoding: utf-8 -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(require 'paredit)
(provide 'hu.dwim.quasi-quote)

;; usage example in your init.el:
;;
;; (setq dwim-workspace (getenv "DWIM_WORKSPACE"))
;; (add-to-list 'load-path (expand-file-name (concat dwim-workspace "/hu.dwim.quasi-quote/emacs/")))
;;
;; (require 'hu.dwim.quasi-quote)
;;
;; (define-key slime-mode-map (kbd "C-w") (lambda (n)
;;                                          (interactive "P")
;;                                          (hu.dwim.quasi-quote:wrap-selection-or-sexp nil n)))
;; (define-key slime-mode-map (kbd "C-S-w") (lambda (n)
;;                                            (interactive "P")
;;                                            (hu.dwim.quasi-quote:wrap-selection-or-sexp t n)))

(defvar hu.dwim.quasi-quote:xml-syntax-table
  (let ((table (copy-syntax-table lisp-mode-syntax-table)))
    (modify-syntax-entry ?< "(>" table)
    (modify-syntax-entry ?> ")<" table)
    table))

(defgroup hu.dwim.quasi-quote.faces nil
  "Faces installed by hu.dwim.quasi-quote."
  :prefix "hu.dwim.quasi-quote"
  :group 'applications)

(defface hu.dwim.quasi-quote.quasi-quote-face
   '((((class color) (background light)) (:foreground "Purple3" :weight bold)))
  "Face for the start syntax of the hu.dwim.quasi-quote stuff."
  :group 'hu.dwim.quasi-quote.faces)

(defface hu.dwim.quasi-quote.xml-paren-face
   '((((class color) (background light)) (:foreground "#444")))
  "Face for the <> parens in the <element ()> syntax."
  :group 'hu.dwim.quasi-quote.faces)

(defface hu.dwim.quasi-quote.xml-element-face
   '((((class color) (background light)) (:foreground "#888")))
  "Face for the element name in the <element ()> syntax."
  :group 'hu.dwim.quasi-quote.faces)

(defun hu.dwim.quasi-quote:mark-text-as-xml-paren (start end)
  (add-text-properties start end
                       `(face hu.dwim.quasi-quote.xml-paren-face
                         syntax-table ,hu.dwim.quasi-quote:xml-syntax-table)))

(defun hu.dwim.quasi-quote:lisp-mode-hook ()
  (hu.dwim.quasi-quote:install-js-indentations)
  (mapcar (lambda (parens)
            (let ((open (elt parens 0))
                  (close (elt parens 1)))
              (modify-syntax-entry open (concat "\(" (string close)) lisp-mode-syntax-table)
              (modify-syntax-entry close (concat "\)" (string open)) lisp-mode-syntax-table)))
          ;; tell emacs that these should behave just like normal parens.
          ;; adding <> here would causes headaches for < and > when they are
          ;; used in their normal meaning, so don't. see below for special treatment.
          '("[]" "{}" "｢｣" "「」" "«»"))
  (make-local-variable 'parse-sexp-lookup-properties)
  (setf parse-sexp-lookup-properties t)
  (make-local-variable 'text-property-default-nonsticky)
  (let ((elem (assq 'syntax-table text-property-default-nonsticky)))
    (if elem
        (setcdr elem t)
        (setq text-property-default-nonsticky
              (cons '(syntax-table . t)
                    text-property-default-nonsticky))))
  ;; set up some prepended rules that apply the new syntax table on the regexp matched <> chars
  (font-lock-add-keywords
   nil `(("\\(`ui\\|`xml\\|`js-inline\\|`js-onload\\|`js-xml\\|`js-piece\\|`js\\|`str\\|`\\|,\\)" 1 'hu.dwim.quasi-quote.quasi-quote-face)
         ("[ 	\n`]\\(<\\)\\([0-9a-zA-Z,<]+\\)"
          (0 (progn
               (hu.dwim.quasi-quote:mark-text-as-xml-paren (match-beginning 1) (match-end 1))
               nil)
             prepend)
          (2 'hu.dwim.quasi-quote.xml-element-face))
         ("[^-'=/<>(]\\(>+\\)[^=]"
          (0 (progn
               (hu.dwim.quasi-quote:mark-text-as-xml-paren (match-beginning 1) (match-end 1))
               (hu.dwim.quasi-quote:%mark-upcoming-xml-parens)))))
   'append)
  ;; set up some appended rules that remove it
  (font-lock-add-keywords
   nil `(("\\w\\([<>]\\)[^>]"
          (0 (progn
               (remove-text-properties (match-beginning 1) (match-end 1)
                                       `(syntax-table nil))
               nil)))))
  (font-lock-add-keywords
   nil `(("\\([<>]\\)[=\"]"
          (0 (progn
               (remove-text-properties (match-beginning 1) (match-end 1)
                                       `(syntax-table nil))
               nil))))))

(add-hook 'lisp-mode-hook 'hu.dwim.quasi-quote:lisp-mode-hook)

;; TODO wtf?, it seems to be not getting triggered...
(defun hu.dwim.quasi-quote:%mark-upcoming-xml-parens ()
  ;; ok, and now let's go until the end of line and while we only see close parens
  ;; mark all >'s as an xml close paren
  (let ((index (match-end 1)))
    (while (or (member (char-after index) '(?\  ?\"))
               (find (char-after index) hu.dwim.quasi-quote:paren-pairs :key 'second))
      (when (equal (char-after index) ?> )
        (hu.dwim.quasi-quote:mark-text-as-xml-paren index (1+ index)))
      (incf index)))
  nil)

(defun hu.dwim.quasi-quote:install-js-indentations ()
  (let ((overrides '((try unwind-protect))))
    (dolist (el overrides)
      (put (first el) 'common-lisp-indent-function
           (if (symbolp (second el))
               (get (second el) 'common-lisp-indent-function)
               (second el))))))

(defvar hu.dwim.quasi-quote:paren-pairs
  (mapcar
   (lambda (el)
     (list (elt el 0) (elt el 1)))
   '("()" "[]" "<>" "{}" "‹›" "«»" "⋘⋙" "⟨⟩" "⟪⟫" "⟦⟧" "⁅⁆" "⊂⊃" "⊏⊐" "⊲⊳" "☾☽" "⁽⁾"
     "₍₎" "⦃⦄" "❨❩" "❪❫" "❬❭" "❮❯" "❰❱" "❲❳" "❴❵" "〈〉" "《》" "「」" "『』" "【】" "〔〕"
     "〖〗" "〘〙" "〚〛" "（）" "＜＞" "［］" "｛｝" "｢｣"
     )))

(defun hu.dwim.quasi-quote:paren-characters-for-context ()
  (save-excursion
    (catch 'return
      (let ((start (point))
            (distance 0)
            (direction 1)
            (reverse-parens (mapcar (lambda (el)
                                      (list (cadr el) (car el)))
                                    hu.dwim.quasi-quote:paren-pairs)))
        (while (and (< distance 50)
                    (not (eobp))
                    (not (bobp)))
          (setq distance (+ distance direction))
          (if (eql (char-after) ?\,)
              (when (eql direction 1)
                (goto-char start)
                (setq direction -1))
              (let ((parens (assoc (char-after) hu.dwim.quasi-quote:paren-pairs)))
                (unless parens
                  (setq parens (assoc (char-after) reverse-parens))
                  (when parens
                    (setq parens (list (cadr parens) (car parens)))))
                (when parens
                  (throw 'return parens))))
          (if (eql direction 1)
              (forward-char)
              (backward-char))))
      (list ?\( ?\)))))

(defun hu.dwim.quasi-quote:sexp-separator-p (char)
  (member char (list ?\n ?\, ?\' ?\` ?\t ?\  ?\( ?\) ?\[ ?\] ?\< ?\>)))

(defun hu.dwim.quasi-quote:before-sexp-separator-p ()
  (or (bolp)
      (hu.dwim.quasi-quote:sexp-separator-p (char-after))))

(defun hu.dwim.quasi-quote:after-sexp-separator-p ()
  (or (eolp)
      (hu.dwim.quasi-quote:sexp-separator-p (char-before))))

(defun hu.dwim.quasi-quote:wrap-selection-or-sexp (dwim-parens &optional n)
  "If selection is active, then wrap it with parens. If DWIM-PARENS is T, then chose the wrapping parens by looking around in the context."
  (if (or (hu.dwim.quasi-quote:after-sexp-separator-p)
          (hu.dwim.quasi-quote:before-sexp-separator-p))
      (if dwim-parens
          (hu.dwim.quasi-quote:wrap-sexp n)
          (paredit-wrap-sexp n))
      (save-excursion
        (backward-sexp)
        (if dwim-parens
            (hu.dwim.quasi-quote:wrap-sexp n)
            (paredit-wrap-sexp n)))))

(defun hu.dwim.quasi-quote:wrap-sexp (&optional n)
  "Wrap the following S-expression in parens dwim-ishly finding out which paren characters to use. Otherwise behaves just like paredit-wrap-sexp."
  (interactive "P")
  (let ((parens (hu.dwim.quasi-quote:paren-characters-for-context)))
    (paredit-handle-sexp-errors
        (paredit-insert-pair (or n
                                 (and (not (paredit-region-active-p))
                                      1))
                             (first parens) (second parens)
                             'goto-char)
      (insert (second parens))
      (backward-char)))
  (save-excursion
    ;; not needed, but why if it's there in paredit-wrap-sexp? (backward-up-list)
    (indent-sexp)))
