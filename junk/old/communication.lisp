(in-package :cl-gui)

(defglobal .server-connection. nil)
(defglobal .communication-handler-thread. nil)

(defglobal .no-such-function-handler.
    (lambda (package function args)
      (warn "No such function: ~a:~a, args: ~a" package function args)))

;;;
(defun handle-funcall (package function args)
  (check-type package string)
  (check-type function string)
  (let* ((pkg (find-package package)))
    (if (null pkg)
        (warn "No such package: ~a, function: ~a, args: ~a" package function args)
        (let* ((fn-symbol (find-symbol function pkg))
               (fn (when (and fn-symbol (fboundp fn-symbol))
                     (symbol-function fn-symbol))))
          (if (null fn)
              (funcall .no-such-function-handler. package function args)
              (apply fn args))))))

(defun dispatch-on-message (message)
  (let ((type (get-data-in-message :type message)))
    (cond ((string= type "update")
           (let ((data (get-data-in-message :data message)))
             (update-js-object-slots data)))
          ((string= type "event")
           (let ((event (get-data-in-message :event message))
                 (args (get-data-in-message :args message)))
             (debug-format "~&Event: ~a, args: ~a" event args)
             (handle-event event args)))
          ;; TODO
          ((string= type "funcall")
           (let ((package (string-upcase (get-data-in-message :pkg message)))
                 (function (string-upcase (get-data-in-message :fn message)))
                 (args (get-data-in-message :args message)))
             (debug-format "~&Funcall: ~a:~a, args: ~s" package function args)
             (handle-funcall package function args)))
          ((string= type "eval")
           (let ((code (get-data-in-message :code message)))
             (if *enable-eval-in-cl*
                 (progn
                   (debug-format "~&Evaluating ~a" code)
                   (eval (read-from-string code nil nil)))
                 (log-format :error "Eval is disabled, code: ~a" code))))
          (t
           (log-format :error "~&Unknown message: ~a" message)))))

(defun canonicalize-message (message)
  message)

(defun communication-handler ()
  (unwind-protect
       (loop for message = (canonicalize-message (json:decode-json .server-connection.))
             do (dispatch-on-message message))
    (terminate-thread-and-connection)))

(defun start-communication-thread ()
  (with-not-implemented-error
    #+sbcl
    (setf .communication-handler-thread.
          (sb-thread:make-thread #'communication-handler :name "Communication thread"))
    #+allegro
    (setf .communication-handler-thread.
          (mp:process-run-function
           `(:name "Communication thread"
             :initial-bindings ((*package* . ',*package*)))
           #'communication-handler))))

(defun connect-to-server (&key (host "localhost") (port *web-server-port*))
  (with-not-implemented-error
    #+sbcl
    (let ((socket (sb-bsd-sockets:make-inet-socket :stream :tcp)))
      (sb-bsd-sockets:socket-connect socket
                                     (sb-bsd-sockets:host-ent-address (sb-bsd-sockets:get-host-by-name host))
                                     port)
      (setf (sb-bsd-sockets:sockopt-tcp-nodelay socket) t
            (sb-bsd-sockets:sockopt-keep-alive socket) t)
      (setf .server-connection.
            (sb-bsd-sockets:socket-make-stream socket
                                               :external-format :utf8
                                               :input t
                                               :output t)))
    #+allegro
    (let ((socket (socket:make-socket :remote-host host :remote-port port)))
      (socket:set-socket-options socket :nodelay t :keepalive t)
      (setf .server-connection. socket)))
  (update-full-data)
  (start-communication-thread))

(defun terminate-thread-and-connection ()
  (when .server-connection.
    (with-not-implemented-error
      #+sbcl
      (progn
        (when (and .communication-handler-thread.
                   (sb-thread:thread-alive-p .communication-handler-thread.))
          (sb-thread:terminate-thread .communication-handler-thread.))
        (close .server-connection.)
        (setf .communication-handler-thread. nil
              .server-connection. nil))
      #+allegro
      (progn
        (when (and .communication-handler-thread.
                   (mp:process-alive-p .communication-handler-thread.))
          (mp:process-kill .communication-handler-thread.))
        (close .server-connection.)
        (setf .communication-handler-thread. nil
              .server-connection. nil)))))

(defun send-message (type &key data name args code)
  (json:encode-json-alist
   (ecase type
     (:update `((:type . "update")
                (:data . ,data)))
     (:event `((:type . "event")
               (:event . ,name)
               (:args . ,args)))
     (:funcall `((:type . "funcall")
                 (:fn . ,name)
                 (:args . ,args)))
     (:eval `((:type . "eval")
              (:code . ,code)))
     (:full `((:type . "full")
              (:data . ,data))))
   .server-connection.)
  (terpri .server-connection.)
  (finish-output .server-connection.))

(defmacro emit (type &rest all-keys &key data name args code)
  (declare (ignorable data name args code))
  `(send-message ,type ,@all-keys))

(defun update-full-data ()
  (emit
   :full
   :data (or (hash-table-alist .js-objects.)
             '())))
