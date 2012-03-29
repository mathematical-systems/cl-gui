;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

;; needed for bootstrapping namespace
(def global-variable *namespace-namespace* nil)

#+nil
(def (class* e) namespace ()
  ((name :type symbol)
   (lock)
   (entries)))

;; inline macroexpansion to lower dependencies
(PROGN
  (DEFCLASS NAMESPACE NIL
    ((NAME :ACCESSOR NAME-OF :INITARG :NAME :TYPE SYMBOL)
     (LOCK :ACCESSOR LOCK-OF :INITARG :LOCK)
     (ENTRIES :ACCESSOR ENTRIES-OF :INITARG :ENTRIES)))
  (EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
    (EXPORT '(NAMESPACE) "HU.DWIM.DEF")))

(def print-object namespace
  (write (name-of -self-)))

;;;;;;
;;; some indirections to be redefined later to lower dependencies

(def with-macro with-lock-held-on-namespace (namespace)
  (declare (ignore namespace))
  (-with-macro/body-))

(def function handle-otherwise/value (otherwise &key default-message)
  (setf default-message (ensure-list default-message))
  (case otherwise
    (:error  (apply #'error (or default-message (list "Otherwise assertion failed"))))
    (:cerror (apply #'cerror "Continue" (or default-message (list "Otherwise assertion failed"))))
    (:warn   (apply #'warn (or default-message (list "Otherwise assertion failed"))))
    (t otherwise)))

(def function make-namespace-lock (namespace-name)
  (declare (ignore namespace-name))
  nil)

(def function make-namespace-hash-table (test-function weakness)
  (declare (ignore weakness))
  (make-hash-table :test test-function))

;; namespace is only exported later when it's loaded through hu.dwim.def.namespace.asd and after it was made thread-safe.
(def (definer :available-flags "e") namespace (namespace-name &optional definer-args &body definer-forms)
  (bind ((finder-name   (getf -options- :finder-name (symbolicate '#:find- namespace-name)))
         (test-function (getf -options- :test '#'eq))
         (weakness      (getf -options- :weakness)))
    (remove-from-plistf -options- :test :weakness :finder-name)
    `(progn
       ,@(when (getf -options- :export)
           `((export '(,finder-name))))
       ,@(bind ((namespace-maker-form `(make-instance 'namespace
                                                      :name ',namespace-name
                                                      :lock (make-namespace-lock ',namespace-name)
                                                      :entries (make-namespace-hash-table ,test-function ,weakness))))
           (if (eq namespace-name 'namespace)
               `((setf *namespace-namespace* ,namespace-maker-form)
                 (setf (gethash 'namespace (entries-of *namespace-namespace*)) *namespace-namespace*))
               `((setf (find-namespace ',namespace-name) ,namespace-maker-form))))
       (def function ,finder-name (name &key (otherwise :error otherwise?))
         (%namespace-getter ,(if (eq namespace-name 'namespace)
                                 `(symbol-value '*namespace-namespace*)
                                 `(quote ,namespace-name))
                            name otherwise otherwise?))
       (def function (setf ,finder-name) (value name)
         (%namespace-setter ',namespace-name name value))
       ,@(unless (zerop (length definer-forms))
          `((def (definer ,@-options- :available-flags "e") ,namespace-name (-name- ,@definer-args)
              ;; locking here _might_ even be useful in some _weird_ situations, but that would prevent having a toplevel (effective) EVAL-ALWAYS inside DEFINER-FORMS
              ;; NOTE: the first argument supposed to be the name in the namespace for the new entry
              `(progn
                 ,@(when (getf -options- :export)
                    `((eval-when (:compile-toplevel :load-toplevel :execute)
                        (export ',-name-))))
                 (setf (,',finder-name ',-name-) ,,@definer-forms)))))
       ',namespace-name)))

(def function %namespace-getter (namespace-name name otherwise otherwise?)
  (bind ((namespace (ensure-namespace namespace-name)))
    (with-lock-held-on-namespace namespace
      (or (gethash name (entries-of namespace))
          (bind ((error-message (list "Cannot find ~S in namespace ~S" name namespace)))
            (if otherwise?
                (handle-otherwise/value otherwise :default-message error-message)
                (apply 'error error-message)))))))

(def function %namespace-setter (namespace-name name value)
  (bind ((namespace (ensure-namespace namespace-name)))
    (with-lock-held-on-namespace namespace
      (if value
          (setf (gethash name (entries-of namespace)) value)
          (remhash name (entries-of namespace)))))
  value)

(def (namespace e) namespace)


;;;;;;
;;; namespace accessors

(def function ensure-namespace (namespace)
  (etypecase namespace
    (namespace namespace)
    (symbol (find-namespace namespace))))

(def (function e) iterate-namespace (namespace visitor)
  (bind ((namespace (ensure-namespace namespace)))
    (with-lock-held-on-namespace namespace
      (maphash visitor (entries-of namespace)))))

(def (function e) collect-namespace-names (namespace)
  (bind ((namespace (ensure-namespace namespace)))
    (with-lock-held-on-namespace namespace
      (hash-table-keys (entries-of namespace)))))

(def (function e) collect-namespace-values (namespace)
  (bind ((namespace (ensure-namespace namespace)))
    (with-lock-held-on-namespace namespace
      (hash-table-values (entries-of namespace)))))

(def (macro e) do-namespace ((namespace key-var &optional value-var return-value) &body body)
  (check-type key-var symbol)
  (check-type value-var symbol)
  (bind ((namespace-designator namespace)
         (ignored-variables '()))
    (macrolet ((optional (var)
                 `(when (string= ,var "_")
                    (setf ,var (gensym))
                    (push ,var ignored-variables))))
      (optional key-var)
      (optional value-var)
      (with-unique-names (namespace iterator has-more?)
        `(bind ((,namespace (ensure-namespace ',namespace-designator)))
           (with-lock-held-on-namespace ,namespace
             (with-hash-table-iterator (,iterator (entries-of ,namespace))
               (loop
                 (bind (((:values ,has-more? ,key-var ,value-var) (,iterator)))
                   (declare (ignore ,@ignored-variables))
                   (unless ,has-more?
                     (return ,return-value))
                   ,@body)))))))))

(def (macro e) do-all-namespaces ((value-var &optional return-value) &body body)
  ;; this is only defined and exported to help understanding code that iterates all namespaces...
  `(do-namespace (namespace _ ,value-var ,return-value)
     ,@body))

#| ; TODO it's crap as it is...
(def (macro e) update-namespace-value ((namespace key value-var &optional exists?-var) &body body)
  (bind ((namespace-designator namespace))
    (with-unique-names (namespace)
      `(bind ((namespace (ensure-namespace ',namespace-designator)))
         (with-lock-held-on-namespace ,namespace
           (bind (((:values ,value-var ,exists?-var) (gethash ,key ,',hashtable-var)))
             ,@body))))))
|#
