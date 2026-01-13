(defpackage :cl-readr/tests
  (:use :cl :fiveam :cl-readr))
(in-package :cl-readr/tests)

(def-suite :cl-readr
  :description "Tests for cl-readr")

(in-suite :cl-readr)

(test basic-sanity
  (is (equal 1 1)))
