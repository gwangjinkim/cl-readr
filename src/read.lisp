(in-package :cl-readr)

(defun read-file (file)
  "Reads a file into a string."
  (uiop:read-file-string file))

(defun read-lines (file)
  "Reads a file into a list of lines."
  (uiop:read-file-lines file))

(defun read-delim (file delim &key (quote #\") (escape #\\) (col-names t))
  "Reads a delimited file. Returns a list of rows (lists of strings) for now."
  ;; For M2, we read lines and tokenize them
  (let ((lines (read-lines file)))
    (if lines
        (let ((rows (mapcar (lambda (line) (tokenize-line line :delim delim :quote quote :escape escape)) lines)))
           ;; Handle col-names later? For now just return all rows 
           rows)
        '())))

(defun read-csv (file &rest args)
  (apply #'read-delim file #\, args))

(defun read-tsv (file &rest args)
  (apply #'read-delim file #\Tab args))
