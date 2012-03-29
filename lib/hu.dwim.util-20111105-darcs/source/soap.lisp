;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

;;;;;;
;;; SOAP

(def (hu.dwim.logger:logger e) soap ())

(def special-variable *soap-stream*)

(eval-always
  (def (function e) with-quasi-quoted-soap-xml-to-string-emitting-form-syntax ()
    (hu.dwim.quasi-quote.xml:with-quasi-quoted-xml-to-string-emitting-form-syntax '*soap-stream*)))

(def (macro e) emit-soap-request-to-string (&body forms)
  `(with-output-to-string (*soap-stream*)
     (hu.dwim.quasi-quote:emit ,@forms)))

(def (constant e) +xml-namespace-uri/soap+ "http://www.w3.org/2003/05/soap-envelope")

(def (function e) make-soap-envelope (body)
  {with-quasi-quoted-soap-xml-to-string-emitting-form-syntax
    (hu.dwim.quasi-quote::as-delayed-emitting
      (hu.dwim.quasi-quote.xml:emit-xml-prologue :encoding :utf-8 :stream *soap-stream* :version "1.0")
      (hu.dwim.quasi-quote:emit <soap:Envelope (xmlns:xsi  "http://www.w3.org/2001/XMLSchema-instance"
                                                xmlns:xsd  "http://www.w3.org/2001/XMLSchema"
                                                ;; NOTE: contrary to google results, it doesn't work with this soap namespace: "http://schemas.xmlsoap.org/soap/envelope/"
                                                xmlns:soap #.+xml-namespace-uri/soap+)
                                  <soap:Body ,body>>))})

(def (function e) send-soap-request (host service-url body &key proxy (timeout 15))
  (check-type host string)
  (check-type service-url string)
  (bind ((request (emit-soap-request-to-string (make-soap-envelope body)))
         (url (format nil "http://~A/~A" host service-url)))
    (soap.debug "Sending soap request to ~A" url)
    (soap.dribble "Sending soap request to ~A, request body is ~S" url request)
    (bind ((response (multiple-value-list
                      (with-deadline (timeout)
                        ;; TODO add a timeout for the socket stuff without sb-ext:with-timeout
                        (drakma:http-request url
                                             :proxy proxy
                                             :method :post
                                             :content-type "application/soap+xml"
                                             :content request)))))
      (soap.debug "Received soap response from ~A" url)
      (when (typep (first response) '(vector (unsigned-byte 8)))
        (setf (first response) (babel:octets-to-string (coerce (first response) '(vector (unsigned-byte 8))) :encoding :utf-8)))
      (soap.dribble "Received soap response from ~A. Response is ~S" url response)
      (values-list response))))

(def (function e) parse-soap-envelope/flexml (string)
  (cxml:parse string (hu.dwim.util.flexml:make-builder :default-package *package*
                                                       :default-node-class 'hu.dwim.util.flexml:node
                                                       :drop-whitespace #t)))

(def (function e) soap-envelope-body/flexml (envelope)
  (the-only-element (hu.dwim.util.flexml:children-of envelope)))

(def (function e) parse-soap-envelope/dom (string)
  (labels ((dummy-entity-resolver (public-id system-id)
             (declare (ignore public-id system-id))
             (babel-streams:make-in-memory-input-stream #())))
    (cxml:parse string (cxml-dom:make-dom-builder) :entity-resolver #'dummy-entity-resolver)))

