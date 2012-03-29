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

(shadowing-import
 '(new-ring-buffer ring-buffer ring-buffer-clear
   ring-buffer-end ring-buffer-flush ring-buffer-insert
   ring-buffer-new ring-buffer-next ring-buffer-p
   ring-buffer-pop ring-buffer-reset ring-buffer-start
   ring-buffer-used ring-buffer-vector rbref
   make-ring-buffer)
 :cl-heredoc-test)

(in-package :cl-heredoc-test)

(in-suite all)
(defsuite ring-buffer)
(in-suite ring-buffer)

(deftest test-new-ring-buffer (length)
  (let ((ring-buffer (new-ring-buffer length)))
    (prog1 ring-buffer
      (is (ring-buffer-p ring-buffer))
      (is (vectorp (ring-buffer-vector ring-buffer)))
      (is (= length (length (ring-buffer-vector ring-buffer)))))))

(deftest ring-buffer-creation ()
  (dotimes (length 10)
    (test-new-ring-buffer length)))

(deftest test-rbref ()
  (let ((ring-buffer (make-ring-buffer :vector (make-array 5 :element-type 'character))))
    (setf (rbref ring-buffer 5) #\f
          (rbref ring-buffer 6) #\n
          (rbref ring-buffer 7) #\o
          (rbref ring-buffer 8) #\r
          (rbref ring-buffer 9) #\d)
    (is (string= "fnord" (ring-buffer-vector ring-buffer)))
    (is (char= #\f (rbref ring-buffer 10)))
    (is (char= #\n (rbref ring-buffer 11)))
    (is (char= #\o (rbref ring-buffer 12)))
    (is (char= #\r (rbref ring-buffer 13)))
    (is (char= #\d (rbref ring-buffer 14)))))

;;; I'm too lazy to write more tests right now
