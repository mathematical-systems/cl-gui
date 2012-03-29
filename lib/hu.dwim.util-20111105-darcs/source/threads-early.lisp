;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

(def (structure e) (atomic-counter (:conc-name ac/))
  "A place holding a machine word sized integer in a way that parallel CPU's don't step on each other's toes when incrementing/decrementing it. These operation don't expand to bignums, they just overflow/underflow when the high/low bound of the word sized integer is reached."
  (counter 0 :type #*((:sbcl (unsigned-byte #.sb-vm:n-word-bits))
                      (t #.(warn "~S does not have a thread safe implementation on your platform!" 'atomic-counter)
                         fixnum))))

(def (function ioe) atomic-counter/increment (atomic-counter &optional (delta 1))
  "Increments a special place holding a machine word sized integer in a way that paralell CPU's don't step on each other's toes when incrementing/decrementing it. These operation don't expand to bignums, they just overflow/underflow when the high/low bound of the word sized integer is crossed."
  #*((:sbcl (sb-ext:atomic-incf (ac/counter atomic-counter) delta))
     (t (incf (ac/counter atomic-counter) delta))))

(def (function ioe) atomic-counter/decrement (atomic-counter &optional (delta 1))
  "A convenience version of ATOMIC-COUNTER/INCREMENT, mostly to keep the user code readable."
  (atomic-counter/increment atomic-counter (- delta)))

(def (function ioe) atomic-counter/value (atomic-counter)
  "See ATOMIC-COUNTER for details."
  (ac/counter atomic-counter))
