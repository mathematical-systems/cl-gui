#+xcvb (module (:depends-on nil))

(cl:defpackage #:fare-csv
  (:use #:common-lisp)
  (:export
   #:read-csv-line #:read-csv-stream #:read-csv-file
   
   #:write-csv-line #:write-csv-lines

   #:+cr+ #:+lf+ #:+crlf+

   #:*separator* #:*quote*
   #:*unquoted-quotequote* #:*loose-quote*
   ;;#:*allow-binary*
   #:*keep-meta-info*
   #:*eol* #:*line-endings*
   #:*skip-whitespace*
))
