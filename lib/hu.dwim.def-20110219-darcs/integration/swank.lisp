;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(def (function e) register-readtable-for-swank (package-names readtable)
  (check-type readtable readtable)
  (dolist (package-name (ensure-list package-names))
    (setf package-name (string package-name))
    (setf (assoc-value swank:*readtable-alist* package-name :test #'string=)
          readtable)))

(def function definer-lookup-hook (form)
  (when (typep form 'definer-name)
    (awhen (find-definer form nil)
      (values it t))))

(when (boundp 'swank::*inspector-lookup-hooks*)
  (pushnew 'definer-lookup-hook swank::*inspector-lookup-hooks*))

(def function notify-swank-about-package-readtable (extended-package)
  (when (symbolp extended-package)
    (setf extended-package (find-extended-package (string extended-package))))
  (awhen (readtable-setup-form-of extended-package)
    (register-readtable-for-swank (name-of extended-package)
                                  (bind ((*readtable* (copy-readtable (with-standard-io-syntax
                                                                        *readtable*))))
                                    (eval it)
                                    *readtable*))))

(pushnew 'notify-swank-about-package-readtable *extended-package-definition-hooks*)

;; notify swank about the readtable of the already defined extended-packages
(do-namespace (extended-package _ extended-package)
  (notify-swank-about-package-readtable extended-package))
