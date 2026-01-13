(in-package :cl-readr/tests)

(in-suite :cl-readr)

(test read-opts-skip
  (uiop:with-temporary-file (:stream s :pathname p :keep t :direction :output)
    (write-line "meta header" s)
    (write-line "col1,col2" s)
    (write-line "1,2" s)
    (finish-output s)
    (close s)
    (unwind-protect
         ;; Skip 1 line
         (let ((df (cl-readr:read-delim p #\, :skip 1)))
            (is (equal '("col1" "col2") (coerce (cl-tibble:tbl-names df) 'list)))
            (is (= 1 (aref (cl-tibble:tbl-col df "col1") 0))))
      (uiop:delete-file-if-exists p))))

(test read-opts-comment
  (uiop:with-temporary-file (:stream s :pathname p :keep t :direction :output)
    (write-line "# This is a comment" s)
    (write-line "a,b" s)
    (write-line "# Another comment" s)
    (write-line "1,2" s)
    (finish-output s)
    (close s)
    (unwind-protect
         (let ((df (cl-readr:read-delim p #\, :comment "#")))
            (is (equal '("a" "b") (coerce (cl-tibble:tbl-names df) 'list)))
            (is (= 1 (aref (cl-tibble:tbl-col df "a") 0)))
            ;; Should only have 1 row
            (is (= 1 (cl-tibble:tbl-nrows df))))
      (uiop:delete-file-if-exists p))))

(test read-opts-n-max
  (uiop:with-temporary-file (:stream s :pathname p :keep t :direction :output)
    (write-line "a,b" s)
    (write-line "1,2" s)
    (write-line "3,4" s)
    (write-line "5,6" s)
    (finish-output s)
    (close s)
    (unwind-protect
         ;; Read only 1 data row
         (let ((df (cl-readr:read-delim p #\, :n-max 1)))
            (is (= 1 (cl-tibble:tbl-nrows df)))
            (is (= 1 (aref (cl-tibble:tbl-col df "a") 0))))
      (uiop:delete-file-if-exists p))))

(test read-opts-empty-rows
  (uiop:with-temporary-file (:stream s :pathname p :keep t :direction :output)
    (write-line "a,b" s)
    (write-line "" s)
    (write-line "1,2" s)
    (finish-output s)
    (close s)
    (unwind-protect
         ;; skip-empty-rows defaults to T usually
         (let ((df (cl-readr:read-delim p #\, :skip-empty-rows t)))
            (is (= 1 (cl-tibble:tbl-nrows df)))
            (is (= 1 (aref (cl-tibble:tbl-col df "a") 0))))
      (uiop:delete-file-if-exists p))))
