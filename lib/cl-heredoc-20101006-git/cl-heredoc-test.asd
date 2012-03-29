;;;; cl-heredoc - cl-heredoc-test.asd
;;;; Copyright (C) 2009, 2010  Alexander Kahl <e-user@fsfe.org>
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

(in-package :cl-user)

(defpackage :cl-heredoc-test-system
  (:use :cl :asdf))

(in-package :cl-heredoc-test-system)

(asdf:defsystem :cl-heredoc-test
                :description "cl-heredoc test package."
                :version "0.0.1"
                :author "Alexander Kahl <e-user@fsfe.org>"
                :license "GPLv3+"
                :depends-on (:cl-heredoc :stefil)
                :components
                ((:module "test"
                          :serial t
                          :components
                          ((:file "package")
                           (:file "ring-buffer")))))
