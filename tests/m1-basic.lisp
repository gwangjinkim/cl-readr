(in-package :cl-readr/tests)

(in-suite :cl-readr)

(test read-file-basic
  (uiop:with-temporary-file (:stream s :pathname p :keep t :direction :output)
    (write-string "Hello world" s)
    (finish-output s)
    (close s)
    (unwind-protect
         (is (string= "Hello world" (cl-readr:read-file p)))
      (uiop:delete-file-if-exists p))))

(test read-lines-basic
  (uiop:with-temporary-file (:stream s :pathname p :keep t :direction :output)
    (write-line "Line 1" s)
    (write-line "Line 2" s)
    (finish-output s)
    (close s)
    (unwind-protect
         (let ((lines (cl-readr:read-lines p)))
           (is (= 2 (length lines)))
           (is (string= "Line 1" (first lines)))
           (is (string= "Line 2" (second lines))))
      (uiop:delete-file-if-exists p))))
