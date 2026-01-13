(in-package :cl-readr)

(defun read-file (file)
  "Reads a file into a string."
  (uiop:read-file-string file))

(defun read-lines (file)
  "Reads a file into a list of lines."
  (uiop:read-file-lines file))

(defun transpose-rows (rows)
  "Transposes a list of rows (lists) into a list of columns (lists)."
  (if rows
      (apply #'mapcar #'list rows)
      '()))

(defun make-unique-names (names)
  "Ensures column names are unique and not empty."
  (let ((seen (make-hash-table :test #'equal))
        (result '()))
    (dolist (name names)
      (let ((base-name (if (string= name "") "X" name))
            (final-name nil))
         (if (gethash base-name seen)
             (let ((suffix 1))
               (loop
                 (setf final-name (format nil "~a~d" base-name suffix))
                 (unless (gethash final-name seen) (return))
                 (incf suffix)))
             (setf final-name base-name))
         (setf (gethash final-name seen) t)
         (push final-name result)))
    (nreverse result)))

(defun read-delim (file delim &key (quote #\") (escape #\\) (col-names t) (skip 0) (n-max nil) (comment nil) (skip-empty-rows t))
  "Reads a delimited file into a tibble."
  ;; 1. Read all lines (inefficient for now, M6 optimization: stream reading)
  (let ((all-lines (read-lines file)))
    
    ;; 2. Filter lines (Skip / Comment / Empty)
    (let ((processed-lines '())
          (lines-read 0))
      
      ;; Skip first N lines
      (setf all-lines (nthcdr skip all-lines))
      
      (loop for line in all-lines
            do
            (block continue-loop
              ;; Check n-max - REMOVED from here, applied to data-rows only later
              ;; (when (and n-max (>= lines-read n-max)) (return))
              
              ;; Check empty
              (when (and skip-empty-rows (string= line ""))
                (return-from continue-loop))
              
              ;; Check comment
              (when (and comment 
                         (> (length line) 0) 
                         (string= (subseq line 0 (length comment)) comment))
                (return-from continue-loop))
              
              ;; Collect
              (push line processed-lines)
              (incf lines-read)))
      
      (setf processed-lines (nreverse processed-lines))
      
      ;; Tokenize
      (let ((rows (mapcar (lambda (line) (tokenize-line line :delim delim :quote quote :escape escape)) processed-lines)))
      
        (unless rows
          (return-from read-delim (cl-tibble:tibble)))

        (let ((headers nil)
              (data-rows nil))
          
          ;; Logic cleanup: n-max usually applies to ROWS not lines (i.e. after header).
          ;; If we filtered above, we might have included header in the count.
          ;; Let's refine:
          
          (if (eq col-names t)
              (progn
                (setf headers (make-unique-names (first rows)))
                (setf data-rows (rest rows)))
              (progn
                (if (listp col-names)
                    (setf headers col-names)
                    (setf headers (loop for i from 1 to (length (first rows)) collect (format nil "X~d" i))))
                (setf data-rows rows)))
          
          ;; If n-max is set, truncate data-rows
          (when (and n-max (> (length data-rows) n-max))
             (setf data-rows (subseq data-rows 0 n-max)))
          
          ;; Transpose to columns
          (let* ((columns-raw (transpose-rows data-rows))
                 ;; Guess types and parse
                 (parsed-columns (loop for col-data in columns-raw
                                       for i from 0
                                       collect 
                                       (let ((type (guess-column-type col-data)))
                                         (parse-column col-data type)))))
            
            (let ((args '()))
              (loop for name in headers
                    for col in parsed-columns
                    do (push (alexandria:make-keyword (string-upcase name)) args)
                       (push col args))
              (apply #'cl-tibble:tibble (nreverse args)))))))))

(defun read-csv (file &rest args)
  (apply #'read-delim file #\, args))

(defun read-tsv (file &rest args)
  (apply #'read-delim file #\Tab args))
