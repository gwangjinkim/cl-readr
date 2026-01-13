(require :asdf)
(let* ((root (uiop:getcwd))
       (asd  (merge-pathnames "cl-readr.asd" root)))
  (asdf:load-asd asd))
;; cl-excel is a dependency, so loading cl-readr should load it or make it available
(ql:quickload :cl-excel)
(format t "~&Exports of CL-EXCEL:~%")
(do-external-symbols (s (find-package :cl-excel))
  (format t "~a~%" s))
