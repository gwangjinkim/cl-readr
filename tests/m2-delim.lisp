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
            ;; For M2, we expect list of lists of strings if not yet tibble? 
            ;; M2 goal says "Basic tokenizer/splitter". 
            ;; Let's assume read-delim returns list of lists until M4.
            ;; OR, we can start returning tibbles early but with string cols independently.
            ;; For now, let's just inspect the result structure or assume list of lists.
            (is (equal '(("a" "b") ("1" "2")) result)))
      (uiop:delete-file-if-exists p))))
