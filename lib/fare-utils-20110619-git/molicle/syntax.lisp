(in-package :molicle)

(defmacro begin (&rest keys &key &allow-other-keys)
  `(progn
     (initialize-evaluation-time)
     (initialize-molicle ,@keys)))

(defun register-final-form (form)
  "To be used in macros, but not in their expansion"
  (assert *molicle*)
  (assert (eq (state *molicle*) :compiling))
  (push form (final-forms *molicle*)))

(defun register-post-compile-hook (hook)
  "To be used in macros, but not in their expansion"
  (assert *molicle*)
  (assert (eq (state *molicle*) :compiling))
  (push hook (post-compile-hook *molicle*)))

(defmacro end-molicle ()
  `(progn
     (finalize-evaluation-time)
     (finalize-molicle)))

(define-symbol-macro end].# (end-molicle))
