;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(def definer cl-l10n::localization (locale &body resources)
  `(cl-l10n:defresources ,locale ,@resources))

(integrated-export 'cl-l10n::localization :hu.dwim.def)
