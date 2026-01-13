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

(defun read-delim (file delim &key (quote #\") (escape #\\) (col-names t))
  "Reads a delimited file into a tibble."
  (let* ((lines (read-lines file))
         (rows (mapcar (lambda (line) (tokenize-line line :delim delim :quote quote :escape escape)) lines)))
    
    (unless rows
      (return-from read-delim (cl-tibble:tibble)))

    (let ((headers nil)
          (data-rows nil))
      
      (if (eq col-names t)
          (progn
            (setf headers (make-unique-names (first rows)))
            (setf data-rows (rest rows)))
          (progn
            ;; If col-names is a list of strings, use them.
            ;; If col-names is nil, generate X1, X2...? 
            ;; For now assume T or list.
            (if (listp col-names)
                (setf headers col-names)
                (setf headers (loop for i from 1 to (length (first rows)) collect (format nil "X~d" i))))
            (setf data-rows rows)))
      
      ;; Transpose to columns
      (let* ((columns-raw (transpose-rows data-rows))
             ;; Guess types and parse
             (parsed-columns (loop for col-data in columns-raw
                                   for i from 0
                                   collect 
                                   (let ((type (guess-column-type col-data)))
                                     (parse-column col-data type)))))
        
        ;; Construct Tibble
        ;; cl-tibble:tibble accepts key-value pairs? 
        ;; No, based on inspection it might, but we want to pass a list.
        ;; cl-tibble usually has a constructor or macro. 
        ;; Let's check if we can construct it via make-instance or internal helper if exported?
        ;; Or use `apply #'cl-tibble:tibble` with interleaving names and cols.
        ;; But names function in tibble usually takes keywords string designators.
        
        (let ((args '()))
          (loop for name in headers
                for col in parsed-columns
                do (push (alexandria:make-keyword (string-upcase name)) args)
                   (push col args))
          (apply #'cl-tibble:tibble (nreverse args)))))))

(defun read-csv (file &rest args)
  (apply #'read-delim file #\, args))

(defun read-tsv (file &rest args)
  (apply #'read-delim file #\Tab args))
