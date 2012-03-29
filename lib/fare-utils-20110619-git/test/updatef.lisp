(in-package :fare-utils-test)

(declaim (optimize (speed 1) (debug 3) (space 3)))

(defsuite* (test-updatef
            :in root-suite
            :documentation "Testing pure update"))

(deftest test-updatef ()
  (is (equal (updatef (car '(1 2)) 3) '(3 2)))
  (is (equal (updatef (cdr '(1 2)) 3) '(1 . 3)))
  (is (equal (updatef (car (cdr '(1 2))) 3) '(3))) ; and not (1 3) - Ahem. We need some way of composing updates...
  (values))

