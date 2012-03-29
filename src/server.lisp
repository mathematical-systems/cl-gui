(in-package :cl-gui)

(defglobal .web-server. nil)

(defun enable-debugger ()
  (setf hunchentoot:*catch-errors-p* nil
        hunchentoot:*show-lisp-errors-p* t))

(defun disable-debugger ()
  (setf hunchentoot:*catch-errors-p* t
        hunchentoot:*show-lisp-errors-p* nil))

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
          #+nil
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
  (loop for message = (canonicalize-message (json:decode-json .server-connection.))
        do (dispatch-on-message message)))

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


;;; hunchentoot/hunchensock integration
(defun start-server (&key (project-pathname *default-pathname-defaults*)
                          (port *web-server-port*)
                          (upload-handler-url "/upload-handler"))
  (let* ((acceptor (make-instance 'hunchensocket:websocket-acceptor
                                  :name (gensym "cl-gui-http")
                                  :port port
                                  :document-root (merge-pathnames "www/" project-pathname)))
         (jslib-handler (hunchentoot:create-folder-dispatcher-and-handler
                         "/jslib/"
                         (merge-pathnames "jslib/" project-pathname)))
         )
    ;; setup handlers
    (setf hunchentoot:*dispatch-table*
          (list 'hunchentoot:dispatch-easy-handlers
                jslib-handler))
    (hunchen.io:define-socket.io-handler
        (lambda (message)
          (hunchentoot:log-message* :debug message)))
    (hunchen.io:socket.io-on "connection" (session)
      (declare (ignorable session))
      (hunchen.io:socket.io-on "my other event" (data)
        (hunchentoot:log-message* :debug data)))
    (setf .web-server. (hunchentoot:start acceptor))
    ;; it's a hack
    (setf hunchentoot:*acceptor* .web-server.)))

(defun stop-server ()
  (hunchentoot:stop .web-server.))

