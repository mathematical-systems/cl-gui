;;;; cl-heredoc - ring-buffer.lisp
;;;; Copyright (C) 2010  Alexander Kahl <e-user@fsfe.org>
;;;; This file is part of cl-heredoc.
;;;; cl-heredoc is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; cl-heredoc is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :cl-heredoc)

;;; Operations on ring buffers, as described by Paul Graham in ANSI Common Lisp.
(defstruct ring-buffer
  "Structure defining ring buffers utilizing a simple VECTOR of fixed size and
four indices:
START: Index of first live value
END:   Index of last live value
USED:  Beginning of current match
NEW:   End of current match"
  vector (start -1) (used -1) (new -1) (end -1))

(defun new-ring-buffer (length)
  "new-ring-buffer length => ring-buffer

Create a new RING-BUFFER containing a simple character vector of fixed size
LENGTH."
  (make-ring-buffer :vector (make-array length :element-type 'character)))

(defun rbref (buffer index)
  "rbref buffer index => character or #\Nul

Return character stored at INDEX in ring BUFFER."
  (char (ring-buffer-vector buffer)
        (mod index (length (ring-buffer-vector buffer)))))

(defun (setf rbref) (value buffer index)
  "setf (rbref buffer index) value => value

SETF for RBREF. If INDEX > LENGTH of BUFFER, start over at the beginning."
  (setf (char (ring-buffer-vector buffer)
              (mod index (length (ring-buffer-vector buffer))))
        value))

(defun ring-buffer-insert (buffer value)
  "ring-buffer-insert buffer value => value

Increment END of BUFFER inserting VALUE at the new index."
  (setf (rbref buffer (incf (ring-buffer-end buffer)))
        value))

(defun ring-buffer-reset (buffer)
  "ring-buffer-reset buffer => end-index

Reset match beginning/end indices USED and NEW in BUFFER to START and END."
  (setf (ring-buffer-used buffer) (ring-buffer-start buffer)
        (ring-buffer-new buffer) (ring-buffer-end buffer)))

(defun ring-buffer-pop (buffer)
  "ring-buffer-pop buffer => character

Increment START of BUFFER returning VALUE at the new index. Additionally, reset
the BUFFER match indices."
  (prog1
      (rbref buffer (incf (ring-buffer-start buffer)))
    (ring-buffer-reset buffer)))

(defun ring-buffer-next (buffer)
  "ring-buffer-next buffer => character or nil

Return next match character incrementing USED in BUFFER or simply NIL if none
are left."
  (when (< (ring-buffer-used buffer) (ring-buffer-new buffer))
    (rbref buffer (incf (ring-buffer-used buffer)))))

(defun ring-buffer-clear (buffer)
  "ring-buffer-clear buffer => -1

Reset all indices of BUFFER to their initial state."
  (setf (ring-buffer-start buffer) -1
        (ring-buffer-used buffer)  -1
        (ring-buffer-new buffer)   -1
        (ring-buffer-end buffer)   -1))

(defun ring-buffer-flush (buffer)
  "ring-buffer-flush buffer => string

Flush all unused characters in BUFFER."
  (with-output-to-string (out)
    (do ((index (1+ (ring-buffer-used buffer)) (1+ index)))
        ((> index (ring-buffer-end buffer)))
      (write-char (rbref buffer index) out))))
