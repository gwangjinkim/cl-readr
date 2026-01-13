(asdf:defsystem "cl-readr"
  :description "A Common Lisp port of R's readr package."
  :author "Antigravity"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-vctrs-lite
               :cl-tibble
               :cl-ppcre
               :local-time
               :alexandria
               :cl-excel)
  :components ((:module "src"
                :components
                ((:file "packages")
                 (:file "tokenize")
                 (:file "parse")
                 (:file "read")
                 (:file "write"))))
  :in-order-to ((asdf:test-op (asdf:test-op "cl-readr/tests"))))

(asdf:defsystem "cl-readr/tests"
  :depends-on (:cl-readr :fiveam)
  :components ((:module "tests"
                :components
                ((:file "main")
                 (:file "m1-basic")
                 (:file "m2-delim")
                 (:file "m3-parse")
                 (:file "m4-tibble")
                 (:file "m5-robust")
                 (:file "m7-write"))))
  :perform (asdf:test-op (o c) (symbol-call :fiveam :run! :cl-readr)))
