(in-package :fare-utils)

(defun list-starts-with-p (start list)
  (and (consp list) (eq start (car list))))

(defun parse-body (body &key documentation whole)
  "Parses BODY into (values remaining-forms declarations doc-string).
Documentation strings are recognized only if DOCUMENTATION is true.
Syntax errors in body are signalled and WHOLE is used in the signal
arguments when given."
  (unless (or (null body) (null (cdr (last body))))
    (error "body should be a proper list, got ~S" body))
  (loop :with doc = nil
    :for body :on body
    :for (current . rest) = body
    :when (list-starts-with-p 'declare current)
      :collect current :into decls
    :else :do
    (cond
      ((and documentation (stringp current) rest)
       (when doc
         (error "Too many documentation strings in ~S." (or whole body)))
       (setf doc current))
      (t
       (return (values body decls doc))))))


;;; decoding a macro-lambda-list or destructuring-lambda-list
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *&keywords*
    '(&whole &environment &body &optional &rest &key &allow-other-keys &aux))
  (defun identifierp (x &optional env)
    (and (symbolp x) (not (or (constantp x env) (member x *&keywords*)))))
  (defun %accept-&kw? (keyword x)
    (when (and (consp x) (eq keyword (car x)))
      t))
  (defun %accept-&kw-id? (keyword x)
    (when (%accept-&kw? keyword x)
      (assert (consp (cdr x)))
      (assert (identifierp (cadr x)))
      t)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-macro-lambda-list (macro-lambda-list)
    (declare (optimize (speed 0) (safety 3) (debug 3)))
    (let ((whole nil)
          (front nil)
          (environment nil)
          (rest macro-lambda-list))
      (labels ((accept ()
                 (push (pop rest) front))
               (accept-&kw? (keyword)
                 (when (and (consp rest) (eq keyword (car rest)))
                   t))
               (accept-&kw-id? (keyword)
                 (when (accept-&kw? keyword)
                   (assert (consp (cdr rest)))
                   (assert (identifierp (cadr rest)))
                   t))
               (accept-env ()
                 (when (accept-&kw-id? '&environment)
                   (if environment
                       (error "only one ~S argument allowed in macro-lambda-list, got ~S"
                              '&environment macro-lambda-list)
                       (setf environment (progn (pop rest) (pop rest))))))
               (result ()
                 (values (nreconc front rest) ; should be validated as deconstructing-lambda-list
                         (or whole (gensym "WHOLE"))
                         (and whole t)
                         (or environment (gensym "ENV"))
                         (and environment t)))
               (accept-vars ()
                 (loop :while (and (consp rest) (or (consp (car rest))
                                                    (identifierp (car rest))))
                   :do (accept))))
        (when (accept-&kw-id? '&whole)
          (setf whole (progn (pop rest) (pop rest))))
        (accept-env)
        (accept-vars)
        (accept-env)
        (when (accept-&kw? '&optional)
          (accept)
          (accept-vars)
          (accept-env))
        (cond
          ((identifierp rest)
           (result))
          (t
           (when (or (accept-&kw? '&body) (accept-&kw? '&rest))
             (when (accept-&kw-id? (car rest))
               (accept) (accept))
             (accept-env))
           (when (accept-&kw? '&key)
             (accept)
             (accept-vars)
             (when (accept-&kw? '&allow-other-key)
               (accept))
             (accept-env))
           (when (accept-&kw? '&aux)
             (accept-vars)
             (accept-env))
           (assert (null rest))
           (result)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-defsetf-lambda-list (lambda-list)
    (when (find '&aux lambda-list)
      (error "~S not allowed in ~S lambda-list, got ~S"
             '&aux 'defsetf lambda-list))
    (let* ((len (length lambda-list))
           (last2 (when (<= 2 len) (nbutlast lambda-list 2)))
           (envp (eq '&environment (car last2)))
           (id (cadr last2)))
      (cond
        (envp
         (assert (identifierp id))
         (assert (null (cddr last2)))
         (values (subseq lambda-list 0 (- len 2)) id t))
        (t
         (assert (not (eq '&environment id)))
         (values lambda-list (gensym) nil))))))

#|
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-destructuring-lambda-list (destructuring-lambda-list whole)
    (let* ((vars '())
           (tmp-vars '())
           (action-queue (list 'progn))
           (structure-stack '()))
      (labels ((var! (id)
                 (push id vars))
               (tmpvar! ()
                 (let ((id (gensym)))
                   (push id tmp-vars)
                   id))
               (curvar! (cur)
                 (if (identifierp cur)
                     cur
                     (let ((id (tmpvar!)))
                       (act! `(setf ,id ,cur))
                       id)))
               (act! (action)
                 (let ((cell (list action)))
                   (setf (cdr action-queue) cell
                         action-queue cell)))
               (accept-id (id cur)
                 (var! id)
                 (act! `(setf ,id ,cur)))
               (accept (pat cur)
                 (cond
                   ((identifierp pat)
                    (var! pat)
                    (act! `(setf ,pat ,cur)))
                   ((%accept-&kw-id? '&whole pat)
                    (let ((id (cadr pat)))
                      (accept-id id cur)
                      (accept (cddr pat) id)))
                   ((%accept-&kw? '&optional pat)
                    (accept-&optional! (cdr pat) cur))
                   ((or (%accept-&kw-id? '&rest pat)
                        (%accept-&kw-id? '&body pat))
                    (accept-id (cadr pat) cur)
                    (accept-?&key (cddr pat) cur))
                   ((%accept-&kw? '&key pat)
                    (accept-&key! (cdr pat) cur))
                   ((%accept-&kw? '&aux pat)
                    (accept-&aux! (cdr pat) cur))
                   ((consp pat) ; no keyword yet
                    (act! `(assert (consp ,cur)))
                    (accept (car pat) `(car ,cur))
                    (accept (cdr pat) `(cdr ,cur)))
                   (t
                    (error "invalid destructuring bind pattern ~S in ~S"
                           pat destructuring-lambda-list))))
               (accept-&optional! (pat cur)
                 (cond
                   ((%accept-&kw? '&key pat)
                    (accept-&key! (cdr pat) cur))
                   ((%accept-&kw? '&aux pat)
                    (accept-&aux! (cdr pat) cur))
                   ((null pat)
                    t)
                   ((consp pat)
                    (let ((opat (car pat)))

                      (act! (assert (consp cur))
                      (push action-queue structure-stack)
                      (setf action-queue (when
                      (act!
                      ...
                      (cond
                        ((identifierp opat)

                    )
                   (t
                    (error "invalid &OPTIONAL bind pattern ~S in ~S"
                           pat destructuring-lambda-list))))




                    ...
                    '&allow-other-keys))

                   ((%accept-&kw? '&aux pat))
                    ...)

                 (push (pop rest) front))
               (accept-&kw? (keyword)
                 (%accept-&kw? keyword rest))
               (accept-&kw-id? (keyword)
                 (%accept-&kw-id? keyword rest))
               (accept-env ()
                 (when (accept-&kw-id? '&environment)
                   (if environment
                       (error "only one ~S argument allowed in macro-lambda-list, got ~S"
                              '&environment macro-lambda-list)
                       (setf environment (progn (pop rest) (pop rest))))))
               (result ()
                 (values (nreconc front rest) ; should be validated as deconstructing-lambda-list
                         (or whole (gensym "WHOLE"))
                         (and whole t)
                         (or environment (gensym "ENV"))
                         (and environment t)))
               (accept-vars ()
                 (loop :while (and (consp rest) (or (consp (car rest))
                                                    (identifierp (car rest))))
                   :do (accept))))
        (when (accept-&kw-id? '&whole)
          (set whole (progn (pop rest) (pop rest))))
        (accept-env)
        (accept-vars)
        (accept-env)
        (when (accept-&kw? '&optional)
          (accept-vars)
          (accept-env))
        (cond
          ((identifierp rest)
           (result))
          (t
           (when (or (accept-&kw? '&body) (accept-&kw? '&rest))
             (when (accept-&kw-id? (car rest))
               (accept) (accept))
             (accept-env))
           (when (accept-&kw? '&key)
             (accept)
             (accept-vars)
             (when (accept-&kw? '&allow-other-key)
               (accept))
             (accept-env))
           (when (accept-&kw? '&aux)
             (accept-vars)
             (accept-env))
           (assert (null rest))
           (result)))))))
|#

