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
      (let ((base-name (if (and (stringp name) (string= name "")) "X" (format nil "~a" name))) ;; Handle non-string headers from Excel?
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


;; Extract tibble construction logic for reuse
(defun data-to-tibble (rows col-names-arg n-max)
  "Converts a list of rows (list of lists) into a tibble, handling headers, transpose, and type guessing."
  (unless rows
    (return-from data-to-tibble (cl-tibble:tibble)))
  
  (let ((headers nil)
        (data-rows nil))
    
    (if (eq col-names-arg t)
        (progn
          (setf headers (make-unique-names (first rows)))
          (setf data-rows (rest rows)))
        (progn
          (if (listp col-names-arg)
              (setf headers col-names-arg)
              ;; Handle implicit names
              (setf headers (loop for i from 1 to (length (first rows)) collect (format nil "X~d" i))))
          (setf data-rows rows)))
    
    ;; Handle n-max
    (when (and n-max (> (length data-rows) n-max))
      (setf data-rows (subseq data-rows 0 n-max)))
    
    ;; Transpose
    (let* ((columns-raw (transpose-rows data-rows))
           (parsed-columns (loop for col-data in columns-raw
                                 collect 
                                 (let ((type (guess-column-type col-data)))
                                   (parse-column col-data type)))))
      
      (let ((args '()))
        (loop for name in headers
              for col in parsed-columns
              do (push (alexandria:make-keyword (string-upcase name)) args)
                 (push col args))
        (apply #'cl-tibble:tibble (nreverse args))))))

(defun read-delim (file delim &key (quote #\") (escape #\\) (col-names t) (skip 0) (n-max nil) (comment nil) (skip-empty-rows t))
  "Reads a delimited file into a tibble."
  (let ((all-lines (read-lines file)))
    
    (let ((processed-lines '())
          (lines-read 0))
      
      (setf all-lines (nthcdr skip all-lines))
      
      (loop for line in all-lines
            do
            (block continue-loop
              (when (and skip-empty-rows (string= line ""))
                (return-from continue-loop))
              
              (when (and comment 
                         (> (length line) 0) 
                         (string= (subseq line 0 (length comment)) comment))
                (return-from continue-loop))
              
              (push line processed-lines)
              (incf lines-read)))
      
      (setf processed-lines (nreverse processed-lines))
      
      (let ((rows (mapcar (lambda (line) (tokenize-line line :delim delim :quote quote :escape escape)) processed-lines)))
        (data-to-tibble rows col-names n-max)))))

(defun read-csv (file &rest args)
  (apply #'read-delim file #\, args))

(defun read-tsv (file &rest args)
  (apply #'read-delim file #\Tab args))



(defvar *default-readr-options* '())

(defmacro with-readr-options ((&key (skip 0) (n-max nil) (comment nil) (skip-empty-rows t)) &body body)
  "Binds default options for readr operations within BODY (Concept only, need to update functions to use globals if we want this to work strictly, or just use this wrapper to call functions with args injected.
   Actually, a better DSL might just be a wrapper that calls read calls with defaults.
   For now, this is a placeholder for future expansion."
  (declare (ignore skip n-max comment skip-empty-rows))
  `(progn ,@body))
