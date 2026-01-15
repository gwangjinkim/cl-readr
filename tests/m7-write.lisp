(in-package :cl-readr/tests)

(in-suite :cl-readr)

(test write-csv-roundtrip
  (uiop:with-temporary-file (:stream s :pathname p :keep t :direction :output)
    (close s) ;; Just need the path name
    (unwind-protect
         (let ((df (cl-tibble:tibble :a #(1 2) :b #("x" "y,z"))))
           (cl-readr:write-csv df p)
           
           (let ((read-back (cl-readr:read-csv p)))
             (is (equal '("a" "b") (coerce (cl-tibble:tbl-names read-back) 'list)))
             (is (= 1 (aref (cl-tibble:tbl-col read-back "a") 0)))
             (is (string= "y,z" (aref (cl-tibble:tbl-col read-back "b") 1)))))
      (uiop:delete-file-if-exists p))))

(test write-tsv-roundtrip
  (uiop:with-temporary-file (:stream s :pathname p :keep t :direction :output)
    (close s)
    (unwind-protect
         (let ((df (cl-tibble:tibble :a #(1 2) :b #("x" "y\tz")))) ;; embedded tab
           (cl-readr:write-tsv df p)
           
           (let ((read-back (cl-readr:read-tsv p)))
             (is (equal '("a" "b") (coerce (cl-tibble:tbl-names read-back) 'list)))
             (is (= 1 (aref (cl-tibble:tbl-col read-back "a") 0)))
             (is (string= "y\tz" (aref (cl-tibble:tbl-col read-back "b") 1)))))
      (uiop:delete-file-if-exists p))))


