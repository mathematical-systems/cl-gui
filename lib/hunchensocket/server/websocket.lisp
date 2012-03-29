;;;; Hunchensocket - websocket.lisp Hunchentoot-based WebSocket (draft) implementation
;;;; Copyright (C) 2011  Alexander Kahl <e-user@fsfe.org>
;;;; This file is part of Hunchensocket.
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions
;;;; are met:
;;;; 
;;;;   * Redistributions of source code must retain the above copyright
;;;;     notice, this list of conditions and the following disclaimer.
;;;; 
;;;;   * Redistributions in binary form must reproduce the above
;;;;     copyright notice, this list of conditions and the following
;;;;     disclaimer in the documentation and/or other materials
;;;;     provided with the distribution.
;;;; 
;;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :hunchensocket)


(defvar *websocket-stream* nil
  "The currently active WebSocket stream")
(defvar *websocket-stream-mutex* nil
  "Mutex lock for the currently active WebSocket stream")
(defvar *websocket-socket* nil
  "The currently active WebSocket socket")

(defparameter *websocket-handlers* nil
  "List of handler closures that will be queried for new connections")

(defclass websocket-request (request)
  ((handler :accessor websocket-request-handler
            :initform nil
            :documentation "Message handler of the current request"))
  (:documentation "Subclass of the regular Hunchentoot request"))

(defclass websocket-reply (reply) ()
  (:documentation "Subclass of the regular Hunchentoot reply (used only for dispatching)"))

(defmethod initialize-instance :after ((reply websocket-reply) &rest initargs &key &allow-other-keys)
  "initialize-instance :after websocket-reply &rest initargs &key &allow-other-keys => side effect

Set the reply's external format to Unix EOL / UTF-8 explicitly."
  (declare (ignore initargs))
  (setf (reply-external-format reply) (make-external-format :utf8 :eol-style :lf)))

(defclass websocket-acceptor (easy-acceptor)
  ((websocket-timeout :initarg :websocket-timeout
                      :accessor websocket-timeout
                      :initform 60
                      :documentation "Custom WebSocket timeout overriding the default usocket one"))
  (:default-initargs :request-class 'websocket-request :reply-class 'websocket-reply)
  (:documentation "Special WebSocket acceptor to be used in place of the regular Hunchentoot one"))

(defclass websocket-ssl-acceptor (websocket-acceptor ssl-acceptor) ()
  (:documentation "Special WebSocket SSL acceptor"))

(define-condition websocket-unsupported-version (condition)
  ((version :initarg :version :reader websocket-unsupported-version-of
            :initform (required-argument :version)
            :documentation "Original version argument that is unsupported"))
  (:documentation "Signal for unsupported / unrecognized WebSocket versions"))

(define-condition websocket-illegal-key (condition)
  ((key :initarg :key :reader websocket-illegal-key-of
        :initform (required-argument :key)
        :documentation "Original, spurious key argument"))
  (:documentation "Signal in case the client has sent an erroneous handshake key"))

(define-condition websocket-illegal-frame-type (condition)
  ((type :initarg :type :reader websocket-illegal-frame-type-of
         :initform (required-argument :type)
         :documentation "Spurious frame type received"))
  (:documentation "Signal in case the client has sent a spurious frame type"))


(defun control-opcode-p (opcode)
  (declare (type (unsigned-byte 4) opcode))
  (= 1 (ash opcode -3)))

(defun digest-key (key) 
  (digest-sequence :sha1 key))

(defun generate-accept-key (key) 
  (base64:usb8-array-to-base64-string
   (digest-key
    (string-to-utf-8-bytes
     (concatenate 'string key +websocket-accept-uuid+)))))

(defun read-bytes-array (stream number)
  "read-bytes-array stream number => unsigned byte array

Read NUMBER bytes from Chunga STREAM into array and return it."
  (let ((result (make-array number :element-type '(unsigned-byte 8))))
    (dotimes (index number result)
      (setf (aref result index)
            (char-int (read-char* stream t))))))

(defun get-opcode-and-fin (stream)
  (let* ((data (read-byte stream)))
    (declare (type (unsigned-byte 8) data))
    (let* ((fin (ldb (byte 1 0) data))
           (opcode (ldb (byte 4 4) data)))
      (values opcode fin))))

(defun get-payload-length-and-mask-flag (stream)
  (let* ((masked-payload (read-byte stream))
         (masked-p (ldb (byte 1 0) masked-payload))
         (payload-guess (ldb (byte 7 1) masked-payload)))
    (cond ((= payload-guess 126)
           ;; big endian
           (values (logior (ash (read-byte stream) 8) (read-byte stream))
                   masked-p))
          ((= payload-guess 127)
           (values (ironclad:octets-to-integer (read-bytes-array stream 8))
                   masked-p))
          (t
           (values payload-guess masked-p)))))

(defun get-masking-key (stream)
  (read-bytes-array stream 4))

(defun unmask-sequence (sequence mask)
  (declare (type (simple-array (unsigned-byte 8) (*)) sequence)
           (type (simple-array (unsigned-byte 8) (4)) mask))
  (loop for i of-type fixnum from 0 below (length sequence)
        for j of-type fixnum from 0
        with result = (make-array (length sequence) :element-type '(unsigned-byte 8))
        do (progn
             (when (= 4 j)
               (setf j 0))
             (setf (aref result i)
                   (logxor (aref sequence i) (aref mask j))))
        finally (return result)))

(defun get-payload-data (stream bytes masking-key)
  (let ((raw (read-bytes-array stream bytes)))
    (if masking-key
        (unmask-sequence raw masking-key)
        raw)))

(defun websocket-handle-handshake (request reply)
  "websocket-handle-handshake request reply => handshake

Try to determine the WebSocket handshake version by checking REQUEST headers and
handle accordingly, if possible."
  (handler-case
      (let ((versions (remove nil
                              (mapcar (lambda (s) (handler-case (parse-integer s) (parse-error () nil)))
                                      (split-sequence:split-sequence #\, (header-in :sec-websocket-version request)))))
            accept-key)
        (cond ((loop for v in versions
                     thereis (<= 6 v 17))
               (setf accept-key
                     (generate-accept-key (string-trim " " (header-in :sec-websocket-key request))))) 
              (t
               (error 'websocket-unsupported-version :unknown)))
        (setf (return-code* reply) +http-switching-protocols+
              (header-out :upgrade reply) "websocket"
              (header-out :connection reply) "Upgrade"
              (header-out :sec-websocket-accept reply) accept-key
              (content-type* reply) "application/octet-stream")
        accept-key)
    (websocket-illegal-key (condition)
      (hunchentoot-error "Illegal key ~a encountered" (websocket-illegal-key-of condition)))
    (websocket-unsupported-version ()
      (hunchentoot-error "WebSocket handshake failed because of unsupported protocol version"))))

(defun websocket-process-connection (stream message-handler)
  "websocket-process-connection stream message-handler &optional => REPL*

Implements the main WebSocket loop for supported protocol versions. Framing is
handled automatically, MESSAGE-HANDLER ought to handle the actual payloads.

*Not really a REPL because it doesn't print implicitly but it does what you'd
expect from the name."
  (let (masking-key payload)
    (iter (for (values opcode fin) = (get-opcode-and-fin stream))
          (for (values payload-length masked-p) = (get-payload-length-and-mask-flag stream))
          (when (= masked-p 1)
            (setf masking-key (get-masking-key stream)))
          (setf payload (get-payload-data stream payload-length masking-key))
          (cond ((= 1 opcode)
                 (funcall message-handler (utf-8-bytes-to-string payload)))
                ((control-opcode-p opcode)
                 ;; TODO:
                 nil)
                ((= 0 opcode)
                 (print 'continuation)
                 (error 'websocket-illegal-frame-type :type (list :opcode opcode)))
                (t
                 (error 'websocket-illegal-frame-type :type (list :opcode opcode))))))) 

;;;
(defmethod process-connection :around ((*acceptor* websocket-acceptor) (socket t))
  "process-connection :around websocket-acceptor t => context

Continue the process with *WEBSOCKET-SOCKET* bound to the original TCP socket
and *HUNCHENTOOT-VERSION* enhanced by the Hunchensocket version."
  (let ((*websocket-socket* socket)
        (hunchentoot-asd:*hunchentoot-version* (format nil "~a Hunchensocket 0" hunchentoot-asd:*hunchentoot-version*)))
    (call-next-method)))

;; (defmethod process-connection ((*acceptor* websocket-acceptor) (socket t))
;;   "process-connection websocket-acceptor t => context

;; Enclose the connection processing with a general error handler w/logging."
;;   (handler-case
;;       (call-next-method)
;;     (error (condition)
;;       (log-message* :error "WebSocket connection terminated unexpectedly: ~a" condition)
;;       (log-message* :debug "~@[~%~a~]"
;;                    (print-backtrace condition :output nil)))))

(defmethod process-request :around ((request websocket-request))
  "process-request :around websocket-request => context / side effect / stream

First, continue the process with HTTP 101 added to Hunchentoot's list of
*APPROVED-RETURN-CODES*. If that status code got issued in the response after
regular processing, hijack the connection and continue to process it as a
WebSocket one.

PS: I *do* know what I'm doing, Mister!"
  (let* ((stream (call-next-method)))
    (prog1 stream
      (when (= +http-switching-protocols+ (return-code*))
        (force-output stream)
        (let ((timeout (websocket-timeout (request-acceptor request))))
          (set-timeouts *websocket-socket* timeout timeout))
        (handler-case
            (let ((*websocket-stream* stream)
                  (*websocket-stream-mutex* (make-lock)))
              (websocket-process-connection
               stream
               (funcall (websocket-request-handler request)
                        stream
                        *websocket-stream-mutex*))) ; custom handshake
          (end-of-file ()
            (log-message* :debug "WebSocket connection terminated"))
          (websocket-illegal-frame-type (condition)
            (log-message* :error "WebSocket illegal frame type 0x~x encountered, terminating"
                          (websocket-illegal-frame-type-of condition))))))))

(defun dispatch-websocket-handlers (request)
  "dispatch-websocket-handlers request => handler

Try to dispatch REQUEST against the available *WEBSOCKET-HANDLERS*, evaluating
to the first match which should be a `λ stream mutex -> λ message' handshake /
message handler closure (please consult the README)."
  (some #'(lambda (handler)
            (funcall handler request))
        *websocket-handlers*))

(defmethod handle-request ((*acceptor* websocket-acceptor) (*request* websocket-request))
  "handle-request websocket-acceptor websocket-request => context

WebSocket junction. Try to continue as a WebSocket connection if the Upgrade and
WebSocket request headers were issued and a suitable WebSocket handler could be
found."
  (if (and (string= "upgrade" (string-downcase (header-in* :connection)))
           (string= "websocket" (string-downcase (header-in* :upgrade))))
      (if-let ((handler (dispatch-websocket-handlers *request*)))
        (prog1 (websocket-handle-handshake *request* *reply*)
          (setf (websocket-request-handler *request*) handler))
        (hunchentoot-error "WebSocket request rejected (no suitable handler found)"))
      (call-next-method)))

;; send
(defun generate-payload-length-bytes (payload-length)
  (cond ((<= payload-length 125)
         (ironclad:integer-to-octets payload-length :n-bits 8))
        ((< payload-length #.(expt 2 16))
         (ironclad:integer-to-octets (+ payload-length (ash 126 16)) :n-bits 16))
        ((< payload-length #.(expt 2 64))
         (ironclad:integer-to-octets (+ payload-length (ash 127 64)) :n-bits 64))
        (t
         (error "payload-length too large: ~a" payload-length))))

(defun websocket-send-message (message &optional (stream *websocket-stream*) (mutex *websocket-stream-mutex*))
  (let* ((payload (string-to-utf-8-bytes message)) 
         (payload-length (generate-payload-length-bytes (length payload)))
         (fin-opcode (ironclad:integer-to-octets 129 :n-bits 8)))
    (progn ;; with-lock-held (mutex)
      (write-sequence fin-opcode stream)
      (write-sequence payload-length stream)
      (write-sequence payload stream)
      (finish-output stream))
    message))
