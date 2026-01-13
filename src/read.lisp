(in-package :cl-readr)

(defun read-file (file)
  "Reads a file into a string."
  (uiop:read-file-string file))

(defun read-lines (file)
  "Reads a file into a list of lines."
  (uiop:read-file-lines file))
