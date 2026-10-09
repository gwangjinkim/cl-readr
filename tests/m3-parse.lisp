(in-package :cl-readr/tests)

(in-suite :cl-readr)

(test parse-integer-test
  (is (equal 123 (cl-readr:parse-integer-safe "123")))
  (is (equal -45 (cl-readr:parse-integer-safe "-45")))
  (is (null (cl-readr:parse-integer-safe "NA")))
  (is (null (cl-readr:parse-integer-safe "")))
  (is (null (cl-readr:parse-integer-safe "12.3"))) ;; Should fail integer parse
  (is (null (cl-readr:parse-integer-safe "abc")))
  (is (equal 7 (cl-readr:parse-integer-safe " 7 "))))

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

(test doubles-keep-double-precision
  (is (= 0.1d0 (cl-readr:parse-double-safe "0.1")))
  (is (= 1.5d10 (cl-readr:parse-double-safe "1.5e10")))
  (is (= -0.25d0 (cl-readr:parse-double-safe "-.25"))))

(test cells-are-never-evaluated
  "A #.(...) cell must not run code or intern symbols."
  (let ((cl-user::*readr-eval-probe* nil))
    (declare (special cl-user::*readr-eval-probe*))
    (is (eq :character
            (cl-readr:guess-column-type '("1" "#.(setf cl-user::*readr-eval-probe* t)"))))
    (is (null (cl-readr:parse-double-safe "#.(setf cl-user::*readr-eval-probe* t)")))
    (is (null cl-user::*readr-eval-probe*)))
  (is (eq :character (cl-readr:guess-column-type '("abc" "1"))))
  (is (null (cl-readr:parse-double-safe "abc"))))
