(in-package :cl-readr/tests)

(in-suite :cl-readr)

(test read-delim-tibble-integration
  (uiop:with-temporary-file (:stream s :pathname p :keep t :direction :output)
    (write-line "name,age,active" s)
    (write-line "alice,25,TRUE" s)
    (write-line "bob,30,FALSE" s)
    (write-line "charlie,,TRUE" s)
    (finish-output s)
    (close s)
    (unwind-protect
         (let ((df (cl-readr:read-delim p #\,)))
            ;; Check it is a tibble class (using string name check to avoid pkg dependency issues if possible, 
            ;; but we technically depend on cl-tibble so checks work).
            ;; Check it is a tibble class
            ;; Check it is a tibble class
            (format t "DEBUG: Class of df: ~S~%" (class-name (class-of df)))
            (format t "DEBUG: Names of df: ~S~%" (cl-tibble:tbl-names df))
            (is (eq 'cl-tibble:tbl (class-name (class-of df))))
            
            ;; Check columns
            ;; Convert vector to list for comparison if needed, and check content type
            (let ((names (cl-tibble:tbl-names df)))
               (is (equal '("name" "age" "active") (coerce names 'list))))
            
            ;; Check types
            ;; We need to access columns. cl-tibble doesn't seem to expose column access easily in my last check?
            ;; Scripts showed `tbl-col`.
            (let ((age-col (cl-tibble:tbl-col df "age")))
              (is (vectorp age-col))
              ;; Should be integers (or doubles?)
              (is (= 25 (aref age-col 0)))
              (is (null (aref age-col 2))))
            
            (let ((active-col (cl-tibble:tbl-col df "active")))
               (is (eq t (aref active-col 0)))))
      (uiop:delete-file-if-exists p))))
