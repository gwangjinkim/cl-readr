(in-package :cl-readr/tests)

(in-suite :cl-readr)

(test tokenize-basic
  (is (equal '("a" "b" "c") (cl-readr:tokenize-line "a,b,c")))
  (is (equal '("a" "b" "c") (cl-readr:tokenize-line "a|b|c" :delim #\|)))
  (is (equal '("foo" "bar") (cl-readr:tokenize-line "foo,bar"))))

(test tokenize-quotes
  (is (equal '("a" "b,c" "d") (cl-readr:tokenize-line "a,\"b,c\",d")))
  (is (equal '("a" "b\"c" "d") (cl-readr:tokenize-line "a,\"b\"\"c\",d" :escape #\"))) ;; CSV style: " -> ""
  (is (equal '("a" "b\"c" "d") (cl-readr:tokenize-line "a,\"b\\\"c\",d" :escape #\\))) ;; Escape style: \" 
  )

(test tokenize-empty
  (is (equal '("" "" "") (cl-readr:tokenize-line ",,")))
  (is (equal '("a" "" "c") (cl-readr:tokenize-line "a,,c")))
  (is (equal '("") (cl-readr:tokenize-line "")))
  )

(test read-delim-integration
  ;; Basic integration test for read-delim calling tokenize
  (uiop:with-temporary-file (:stream s :pathname p :keep t :direction :output)
    (write-line "a,b" s)
    (write-line "1,2" s)
    (finish-output s)
    (close s)
    (unwind-protect
         (let ((result (cl-readr:read-delim p #\,)))
            ;; Now that M4 is integrated, read-delim returns a TIBBLE, not a list of lists.
            ;; We should check that it parsed correctly.
            ;; Headers: a, b. Row: 1, 2.
            (let ((names (cl-tibble:tbl-names result)))
              (is (equal '("a" "b") (coerce names 'list))))
            (is (= 1 (aref (cl-tibble:tbl-col result "a") 0)))
            (is (= 2 (aref (cl-tibble:tbl-col result "b") 0))))
      (uiop:delete-file-if-exists p))))
