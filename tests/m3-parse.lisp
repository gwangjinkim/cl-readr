(in-package :cl-readr/tests)

(in-suite :cl-readr)

(test parse-integer-test
  (is (equal 123 (cl-readr:parse-integer-safe "123")))
  (is (equal -45 (cl-readr:parse-integer-safe "-45")))
  (is (null (cl-readr:parse-integer-safe "NA")))
  (is (null (cl-readr:parse-integer-safe "")))
  (is (null (cl-readr:parse-integer-safe "12.3"))) ;; Should fail integer parse
  (is (null (cl-readr:parse-integer-safe "abc"))))

(test guess-type-test
  (is (eq :integer (cl-readr:guess-column-type '("1" "2" "3"))))
  (is (eq :double (cl-readr:guess-column-type '("1.1" "2.2" "3.3"))))
  (is (eq :double (cl-readr:guess-column-type '("1" "2.5" "3")))) ;; Mix int/double -> double
  (is (eq :logical (cl-readr:guess-column-type '("TRUE" "FALSE" "T"))))
  (is (eq :character (cl-readr:guess-column-type '("1" "a" "3"))))
  (is (eq :integer (cl-readr:guess-column-type '("1" "NA" "3")))) ;; NA handled
  )

(test parse-column-test
  (let ((col (cl-readr:parse-column '("1" "2" "NA") :integer)))
    (is (vectorp col))
    (is (= 1 (aref col 0)))
    (is (null (aref col 2))))
  
  (let ((col (cl-readr:parse-column '("1.5" "2") :double)))
    (is (= 1.5d0 (aref col 0)))
    (is (= 2.0d0 (aref col 1)))))
