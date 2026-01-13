(defpackage :cl-readr
  (:use :cl)
  (:export #:read-csv
           #:read-tsv
           #:read-delim
           #:read-file
           #:read-lines
           
           ;; Column types
           #:col-guess
           #:col-character
           #:col-integer
           #:col-double
           #:col-logical
           #:col-date
           #:col-datetime
           #:col-skip))
