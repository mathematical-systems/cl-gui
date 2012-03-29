(in-package :fare-utils-test)

(defsuite* (test-strings
            :in root-suite
            :documentation "Test string functions"))

(deftest test-strcat ()
  (is (equal (strcat "foo" "bar" "baz") "foobarbaz")))

(deftest test-join-strings ()
  (is (equal (join-strings ":" '("/bin" "/usr/bin" "/usr/local/bin"))
             "/bin:/usr/bin:/usr/local/bin")))
