(in-package :hunchensocket)

;; Constants indicating WebSocket protocol version.
;; NOTE: only version 8 and 13 are supported
(defconstant +version-hixie75+ -1)
(defconstant +version-hybi00+ 0)
(defconstant +version-hybi01+ 1)
(defconstant +version-hybi02+ 2)
(defconstant +version-hybi03+ 2)
(defconstant +version-hybi04+ 4)
(defconstant +version-hybi05+ 5)
(defconstant +version-hybi06+ 6)
(defconstant +version-hybi07+ 7)
(defconstant +version-hybi08+ 8)
(defconstant +version-hybi09+ 8)
(defconstant +version-hybi10+ 8)
(defconstant +version-hybi11+ 8)
(defconstant +version-hybi12+ 8)
(defconstant +version-hybi13+ 13)
(defconstant +version-hybi14+ 13)
(defconstant +version-hybi15+ 13)
(defconstant +version-hybi16+ 13)
(defconstant +version-hybi17+ 13)

;; schemes
(define-constant +web-socket-scheme+ "ws" :test 'equal)
(define-constant +web-socket-secure-scheme+ "wss" :test 'equal)

;; frame opcodes defined in the spec.
(defconstant +opcode-continuation+ #x0)
(defconstant +opcode-text+ #x1)
(defconstant +opcode-binary+ #x2)
(defconstant +opcode-close+ #x8)
(defconstant +opcode-ping+ #x9)
(defconstant +opcode-pong+ #xa)

;; uuids used by hybi 04 and later opening handshake and frame masking.
(define-constant +websocket-accept-uuid+ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11" :test 'equal)

;; opening handshake header names and expected values.
(define-constant +upgrade-header+ "Upgrade" :test 'equal)
(define-constant +websocket-upgrade-type+ "websocket" :test 'equal)
(define-constant +websocket-upgrade-type-hixie75+ "WebSocket" :test 'equal)
(define-constant +connection-header+ "Connection" :test 'equal)
(define-constant +upgrade-connection-type+ "Upgrade" :test 'equal)
(define-constant +host-header+ "Host" :test 'equal)
(define-constant +origin-header+ "Origin" :test 'equal)
(define-constant +sec-websocket-origin-header+ "Sec-WebSocket-Origin" :test 'equal)
(define-constant +sec-websocket-key-header+ "Sec-WebSocket-Key" :test 'equal)
(define-constant +sec-websocket-accept-header+ "Sec-WebSocket-Accept" :test 'equal)
(define-constant +sec-websocket-version-header+ "Sec-WebSocket-Version" :test 'equal)
(define-constant +sec-websocket-protocol-header+ "Sec-WebSocket-Protocol" :test 'equal)
(define-constant +sec-websocket-extensions-header+ "Sec-WebSocket-Extensions" :test 'equal)
(define-constant +sec-websocket-draft-header+ "Sec-WebSocket-Draft" :test 'equal)
(define-constant +sec-websocket-key1-header+ "Sec-WebSocket-Key1" :test 'equal)
(define-constant +sec-websocket-key2-header+ "Sec-WebSocket-Key2" :test 'equal)
(define-constant +sec-websocket-location-header+ "Sec-WebSocket-Location" :test 'equal)

;; extensions
(define-constant +deflate-stream-extension+ "deflate-stream" :test 'equal)
(define-constant +deflate-frame-extension+ "deflate-frame" :test 'equal)

;; status codes
;; code status-code-not-available should not be used in actual frames. this code
;; is exposed to javascript api as pseudo status code which represent actual
;; frame does not have status code.
(defconstant +status-normal+ 1000)
(defconstant +status-going-away+ 1001)
(defconstant +status-protocol-error+ 1002)
(defconstant +status-unsupported+ 1003)
(defconstant +status-code-not-available+ 1005)
(defconstant +status-abnormal-close+ 1006)
(defconstant +status-invalid-frame-payload+ 1007)
(defconstant +status-policy-violation+ 1008)
(defconstant +status-message-too-big+ 1009)
(defconstant +status-mandatory-ext+ 1010)

;; http status codes
;; NOTE: use hunchentoot's status code
;; (defconstant +http-status-bad-request+ 400)
;; (defconstant +http-status-forbidden+ 403)
;; (defconstant +http-status-not-found+ 404)

