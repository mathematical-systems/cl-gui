;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

;;; KLUDGE redefine a few things to make namespaces thread-safe now that we loaded a few more dependencies...
(hu.dwim.util:with-muffled-redefinition-warnings

  (def function make-namespace-lock (namespace-name)
    (bordeaux-threads:make-recursive-lock
     (bind ((*package* (find-package :keyword)))
       (format nil "lock for namespace ~S" namespace-name))))

  ;; ensure the lock of all the currently defined namespaces before we redefine WITH-LOCK-HELD-ON-NAMESPACE
  (do-all-namespaces (namespace)
    (unless (lock-of namespace)
      (setf (lock-of namespace) (make-namespace-lock (name-of namespace)))))

  (def with-macro with-lock-held-on-namespace (namespace)
    (bordeaux-threads:with-recursive-lock-held ((lock-of namespace))
      (-with-macro/body-)))

  (def function make-namespace-hash-table (test-function weakness)
    (trivial-garbage:make-weak-hash-table :test test-function :weakness weakness))

  (def function handle-otherwise/value (otherwise &key default-message)
    (hu.dwim.util:handle-otherwise/value otherwise :default-message default-message))

  (export 'namespace :hu.dwim.def))
