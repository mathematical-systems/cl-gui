;;;; hunchen.io - hunchen.io.asd hunchen.io ASDF definition file
;;;; Copyright (C) 2011  Alexander Kahl <e-user@fsfe.org>
;;;; This file is part of hunchen.io.
;;;; hunchen.io is free software; you can redistribute it and/or modify it
;;;; under the terms of the GNU Affero General Public License as
;;;; published by the Free Software Foundation; either version 3 of the
;;;; License, or (at your option) any later version.
;;;;
;;;; hunchen.io is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :cl-user)

(defpackage :hunchen.io-system
  (:use :cl :asdf))

(in-package :hunchen.io-system)

(defsystem :hunchen.io
  :description "socket.io for Hunchentoot"
  :author "Alexander Kahl <e-user@fsfe.org>"
  :license "AGPLv3+"
  :depends-on (:hunchentoot :hunchensocket :uuid :cl-json :cl-heredoc :alexandria)
  :components
  ((:module "server"
            :serial t
            :components
            ((:file "package")
             (:file "socket.io")))))
